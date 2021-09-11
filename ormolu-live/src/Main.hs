{-# OPTIONS_GHC -Wno-orphans #-}

module Main where

import Control.Lens
import Data.Generics.Labels ()
import GHC.Driver.Session (unsafeGlobalDynFlags)
import qualified GHC.Hs.Dump as Dump
import GHC.SyntaxHighlighter
import GHC.Utils.Outputable (showSDocDump)
import qualified Language.Javascript.JSaddle.Warp.Extra as JSaddleWarp
import Miso
import Miso.String (MisoString, fromMisoString, ms)
import qualified Ormolu as O
import qualified Ormolu.Config as O
import qualified Ormolu.Parser as O
import qualified Ormolu.Parser.Result as O
import qualified Ormolu.Utils as O
import System.IO.Unsafe (unsafePerformIO)
import UnliftIO.Exception

type Output = Either (Either Text O.OrmoluException) Text

type OrmoluConfig = O.Config O.RegionIndices

data Model = Model
  { input :: MisoString,
    output :: Output,
    config :: OrmoluConfig,
    showParseResult :: Bool
  }
  deriving stock (Show, Eq, Generic)

deriving stock instance Generic (O.Config region)

data Action
  = NoOp
  | SetInput MisoString
  | SetOutput Output
  | Format
  | UpdateConfig (OrmoluConfig -> OrmoluConfig)
  | SetShowParseResult Bool

main :: IO ()
main = JSaddleWarp.run 8080 "www" $ startApp App {..}
  where
    initialAction = NoOp
    model = Model {..}
      where
        input = ""
        output = Right ""
        config = O.defaultConfig {O.cfgCheckIdempotence = True}
        showParseResult = False
    update = fromTransition . updateModel
    view = viewModel
    events = defaultEvents
    subs = []
    mountPoint = Nothing
    logLevel = Off

updateModel :: Action -> Transition Action Model ()
updateModel = \case
  NoOp -> pass
  SetInput t -> do
    #input .= t
    format
  SetOutput o ->
    #output .= o
  Format -> do
    input <- fromMisoString <$> use #input
    config <- use #config
    scheduleIO do
      output <-
        tryAnyDeep (O.ormolu config "<input>" input)
          <&> _Left %~ extractOrmoluException
      pure $ SetOutput output
  UpdateConfig f -> do
    #config %= f
    format
  SetShowParseResult b ->
    #showParseResult .= b
  where
    format = scheduleIO $ pure Format

viewModel :: Model -> View Action
viewModel model@Model {..} =
  div_
    []
    [ link_ [rel_ "stylesheet", href_ "style.css"],
      section_ [class_ "section"] . pure . div_ [class_ "container is-fluid"] $
        [ h1_ [class_ "title"] [text $ "Ormolu " <> VERSION_ormolu],
          div_
            [class_ "content"]
            [ p_
                []
                [ a_
                    [class_ "button is-link is-light", href_ "https://github.com/tweag/ormolu"]
                    [text "Report issues etc. on GitHub"]
                ],
              div_ [] . intersperse (br_ []) $
                [ configCheckbox
                    #cfgCheckIdempotence
                    "Check idempotence (formatting twice is the same as formatting once)",
                  configCheckbox
                    #cfgUnsafe
                    "Unsafe mode (don't ensure that formatting preserves the AST)",
                  checkbox (^. #showParseResult) SetShowParseResult "Show internal parse result"
                ]
            ],
          div_
            [class_ "columns"]
            [ div_
                [class_ "column is-half is-flex"]
                [ textarea_
                    [class_ "textarea is-family-code", onInput SetInput, rows_ "20", autofocus_ True]
                    [text input]
                ],
              div_ [class_ "column is-half is-flex"] [out]
            ]
        ]
          <> [ div_
                 [class_ "columns"]
                 [ pre_
                     [class_ "column is-half is-family-code"]
                     [ text . ms . prettyAST . fromMisoString $ input
                     ],
                   pre_
                     [class_ "column is-half is-family-code"]
                     [ either (const (text "")) (text . ms . prettyAST) output
                     ]
                 ]
               | showParseResult
             ]
    ]
  where
    checkbox fromModel action desc =
      label_
        [class_ "checkbox"]
        [ input_
            [ type_ "checkbox",
              checked_ $ fromModel model,
              onChecked \(Checked c) -> action c
            ],
          text $ " " <> desc
        ]
    configCheckbox (cloneLens -> l) = checkbox (^. #config . l) \c -> UpdateConfig $ l .~ c

    out = case output of
      Right t ->
        pre_ [class_ "is-family-code is-flex-grow-1"]
          . maybe (pure . text . ms $ t) (tokenToHtml <$>)
          . tokenizeHaskell
          $ t
      Left e ->
        pre_
          [class_ "is-flex-grow-1 content has-background-danger-light has-text-danger-dark"]
          [text . ms . showOrmoluException $ e]
    tokenToHtml (token, t) = span_ (maybeToList $ class_ <$> tokenClass token) [text $ ms t]
    tokenClass = \case
      KeywordTok -> Just "has-text-link"
      PragmaTok -> Just "has-text-grey"
      ConstructorTok -> Just "has-text-primary-dark"
      CharTok -> Just "has-text-success"
      StringTok -> Just "has-text-success"
      CommentTok -> Just "has-text-grey"
      OperatorTok -> Just "has-text-warning-dark"
      SymbolTok -> Just "has-text-warning-dark"
      _ -> Nothing
    showOrmoluException = \case
      Right oe ->
        unlines . fmap toText $ case oe of
          O.OrmoluParsingFailed s m -> ["The GHC parser failed:", "", O.showOutputable s, "", m]
          O.OrmoluOutputParsingFailed s m -> ["Parsing of formatted code failed:", "", O.showOutputable s, "", m]
          O.OrmoluASTDiffers _ ss ->
            ["AST of input and AST of formatted code differ. Please, consider reporting the bug.", ""]
              <> do O.showOutputable <$> ss
          O.OrmoluNonIdempotentOutput _ -> ["Formatting is not idempotent. Please, consider reporting the bug."]
          O.OrmoluUnrecognizedOpts os ->
            ["The following GHC options were not recognized:", "", toString . unwords . fmap toText . toList $ os]
          O.OrmoluCabalFileParsingFailed _ -> error "unreachable"
          O.OrmoluMissingStdinInputFile -> error "unreachable"
      Left e -> e

    prettyAST t = case parseModule t of
      Left e -> showOrmoluException e
      Right (_, Left (srcSpan, msg)) ->
        show (srcSpan, msg)
      Right (_, Right O.ParseResult {..}) ->
        toText
          . showSDocDump unsafeGlobalDynFlags
          . Dump.showAstData Dump.NoBlankSrcSpan
          $ prParsedSource
      where
        parseModule =
          unsafePerformIO
            . do mapped . _Left %~ extractOrmoluException
            . tryAny
            . O.parseModule configWithDeltas "<input>"
            . toString
        configWithDeltas = fmap (O.regionIndicesToDeltas (length (lines t))) config

extractOrmoluException :: SomeException -> Either Text O.OrmoluException
extractOrmoluException = \case
  (fromException -> Just oe) -> Right oe
  e -> Left . toText . displayException $ e
