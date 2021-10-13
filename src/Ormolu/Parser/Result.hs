{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | A type for result of parsing.
module Ormolu.Parser.Result
  ( SourceSnippet (..),
    ParsedSource (..),
    ParseResult (..),
  )
where

import Data.Data
import Data.Text (Text)
import GHC.Data.EnumSet (EnumSet)
import GHC.Driver.Backpack.Syntax
import GHC.Driver.Types
import GHC.Hs
import GHC.LanguageExtensions.Type
import GHC.Types.SrcLoc
import GHC.Unit.Info
import Ormolu.Parser.Anns
import Ormolu.Parser.CommentStream
import Ormolu.Parser.Pragma (Pragma)

-- | Either a 'ParseResult', or a raw snippet.
data SourceSnippet = RawSnippet Text | ParsedSnippet ParseResult

data ParsedSource = ParsedModule (Located HsModule) | ParsedSig [LHsUnit PackageName]
  deriving stock (Data)

-- TODO where to put this?
deriving stock instance Data n => Data (HsUnit n)

deriving stock instance Data n => Data (HsUnitDecl n)

deriving stock instance Data n => Data (IncludeDecl n)

deriving stock instance Data n => Data (HsUnitId n)

deriving stock instance Data n => Data (HsModuleId n)

deriving stock instance Data Renaming

deriving stock instance Data HscSource

deriving stock instance Data PackageName

-- | A collection of data that represents a parsed module in Ormolu.
data ParseResult = ParseResult
  { -- | 'ParsedSource' from GHC
    prParsedSource :: ParsedSource,
    -- | Ormolu-specfic representation of annotations
    prAnns :: Anns,
    -- | Stack header
    prStackHeader :: Maybe (RealLocated Comment),
    -- | Pragmas and the associated comments
    prPragmas :: [([RealLocated Comment], Pragma)],
    -- | Comment stream
    prCommentStream :: CommentStream,
    -- | Whether or not record dot syntax is enabled
    prUseRecordDot :: Bool,
    -- | Enabled extensions
    prExtensions :: EnumSet Extension,
    -- | Indentation level, can be non-zero in case of region formatting
    prIndent :: Int
  }
