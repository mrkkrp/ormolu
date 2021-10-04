module Main where

import Prelude
import Control.Promise (Promise)
import Control.Promise as Promise
import Data.Array (intersperse)
import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple)
import Effect (Effect)
import Effect.Aff (Aff, runAff_)
import Effect.Class (liftEffect)
import Effect.Exception (throwException)
import Flame (Html, QuerySelector(..), Subscription, (:>))
import Flame.Application.EffectList as FAEL
import Flame.Html.Attribute as HA
import Flame.Html.Element as HE

main :: Effect Unit
main =
  runAff_ (either throwException (const (pure unit))) do
    asteriusInstance <- Promise.toAffE newAsteriusInstance
    liftEffect
      $ FAEL.mount_ (QuerySelector "body")
          { init
          , view
          , update: update asteriusInstance
          , subscribe
          }

type Output
  = Either String String

type Config
  = { a :: Boolean, b :: Boolean }

type Model
  = { input :: String
    , output :: Output
    , config :: Config
    }

data Message
  = SetInput String
  | SetOutput Output
  | Format
  | UpdateConfig (Config -> Config)

init :: Tuple Model (Array (Aff (Maybe Message)))
init = { input: "", output: Right "", config } :> []
  where
  config = { a: true, b: false }

update ::
  AsteriusInstance ->
  Model -> Message -> Tuple Model (Array (Aff (Maybe Message)))
update asteriusInstance model = case _ of
  SetInput t -> (model { input = t }) :> [ pure $ Just Format ]
  SetOutput o -> (model { output = o }) :> []
  Format -> model :> [ Just <<< SetOutput <$> format asteriusInstance model.input ]
  UpdateConfig f -> (model { config = f model.config }) :> []

view :: Model -> Html Message
view model =
  HE.section [ HA.class' "section" ]
    [ HE.div [ HA.class' "container is-fluid" ]
        [ HE.h1 [ HA.class' "title" ] "Ormolu Live"
        , HE.div [ HA.class' "content" ]
            [ HE.p_
                [ HE.a
                    [ HA.class' "button is-link is-light"
                    , HA.href "https://github.com/tweag/ormolu"
                    ]
                    "Report issues etc. on GitHub"
                ]
            , HE.div_ <<< intersperse HE.br
                $ [ checkbox _.config.a (\c -> UpdateConfig $ _ { a = c })
                      "Config option A"
                  , checkbox _.config.b (\c -> UpdateConfig $ _ { b = c })
                      "Config option B"
                  ]
            ]
        , HE.div [ HA.class' "columns" ]
            [ HE.div [ HA.class' "column is-half is-flex" ]
                [ HE.textarea
                    [ HA.class' "textarea is-family-code"
                    , HA.onInput SetInput
                    , HA.rows 20
                    , HA.autofocus true
                    ]
                    model.input
                ]
            , HE.div [ HA.class' "column is-half is-flex" ] [ out ]
            ]
        ]
    ]
  where
  out = case model.output of
    Right t -> HE.pre [ HA.class' "is-family-code is-flex-grow-1" ] t
    Left e -> HE.pre [ HA.class' "is-flex-grow-1 has-background-danger-light has-text-danger-dark" ] e

  checkbox fromModel event desc =
    HE.label [ HA.class' "checkbox" ]
      [ HE.input
          [ HA.type' "checkbox", HA.checked (fromModel model), HA.onCheck event
          ]
      , HE.text $ " " <> desc
      ]

subscribe :: Array (Subscription Message)
subscribe = []

format :: AsteriusInstance -> String -> Aff Output
format asteriusInstance = map Right <<< Promise.toAffE <<< webOrmolu asteriusInstance

foreign import data AsteriusInstance :: Type

foreign import newAsteriusInstance :: Effect (Promise AsteriusInstance)

foreign import webOrmolu :: AsteriusInstance -> String -> Effect (Promise String)
