{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Pretty-printer for Haskell AST.
module Ormolu.Printer
  ( printModule,
  )
where

import Data.Text (Text)
import qualified Data.Text as T
import Ormolu.Parser.Result
import Ormolu.Printer.Combinators
import Ormolu.Printer.Meat.Backpack
import Ormolu.Printer.Meat.Module
import Ormolu.Printer.SpanStream

-- | Render a module.
printModule ::
  -- | Result of parsing
  [SourceSnippet] ->
  -- | Resulting rendition
  Text
printModule = T.concat . fmap printSnippet
  where
    printSnippet = \case
      ParsedSnippet ParseResult {..} ->
        let runR' a p =
              runR
                (p a)
                (mkSpanStream a)
                prCommentStream
                prAnns
                prUseRecordDot
                prExtensions
         in case prParsedSource of
              ParsedModule hmod ->
                runR' hmod . located' $
                  p_hsModule prStackHeader prPragmas
              ParsedSig hsig -> runR' hsig $ p_hsSig prPragmas
      RawSnippet r -> r
