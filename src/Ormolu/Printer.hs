{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Pretty-printer for Haskell AST.
module Ormolu.Printer
  ( printSnippets,
  )
where

import Data.Text (Text)
import qualified Data.Text as T
import Ormolu.Parser.Result
import Ormolu.Printer.Combinators
import Ormolu.Printer.Meat.Backpack
import Ormolu.Printer.Meat.Module
import Ormolu.Printer.SpanStream
import Ormolu.Processing.Common (reindent)

-- | Render one or more source snippets.
printSnippets ::
  -- | Result of parsing
  [SourceSnippet] ->
  -- | Resulting rendition
  Text
printSnippets = T.concat . fmap printSnippet
  where
    printSnippet = \case
      ParsedSnippet ParseResult {..} ->
        let runR' source printer =
              runR
                (printer source)
                (mkSpanStream source)
                prCommentStream
                prAnns
                prUseRecordDot
                prExtensions
         in reindent prIndent $ case prParsedSource of
              ParsedModule hmod ->
                runR' hmod . located' $
                  p_hsModule prStackHeader prPragmas
              ParsedSig hsig -> runR' hsig $ p_hsSig prPragmas
      RawSnippet r -> r
