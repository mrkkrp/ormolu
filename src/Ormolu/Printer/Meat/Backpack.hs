{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Ormolu.Printer.Meat.Backpack where

import Control.Monad
import Data.Foldable
import qualified Data.Text as T
import GHC.Driver.Backpack.Syntax
import GHC.Driver.Types (HscSource (..))
import GHC.Hs (HsModule)
import GHC.Types.SrcLoc
import GHC.Unit.Info
import Ormolu.Parser.CommentStream
import Ormolu.Parser.Pragma
import Ormolu.Parser.Result ()
import Ormolu.Printer.Combinators
import Ormolu.Printer.Meat.Common
import Ormolu.Printer.Meat.Module
import Ormolu.Printer.Meat.Pragma

txtDebug :: Show a => a -> R ()
txtDebug = txt . T.pack . show

-- | Mutates all the module declarations of the parsed Backpack file using the
-- specified function
updateModules ::
  -- | Parsed Backpack file
  [LHsUnit PackageName] ->
  -- | Function to mutate each module
  (HsModule -> HsModule) ->
  [LHsUnit PackageName]
updateModules lunits mut = fmap updateModules' lunits
  where
    updateModules' (L l unit@HsUnit {..}) = L l unit {hsunitBody = fmap updateDecl hsunitBody}
    updateDecl (L l unitDecl) = L l $ case unitDecl of
      DeclD source name (Just (L l' hmod)) -> DeclD source name (Just (L l' $ mut hmod))
      _ -> unitDecl

p_hsSig :: [([RealLocated Comment], Pragma)] -> [LHsUnit PackageName] -> R ()
p_hsSig pragmas units = do
  p_pragmas pragmas
  newline
  for_ units $ \lunit -> located lunit $ \HsUnit {..} -> do
    p_hsmodLikeName "unit" hsunitName
    space
    txt "where"
    newline
    inci . for_ hsunitBody $ \ldecl -> located ldecl $ \case
      DeclD declType modName mHsModule -> do
        let namePrefix = case declType of
              HsSrcFile -> "module"
              HsigFile -> "signature"
              HsBootFile -> undefined
        case mHsModule of
          Just hsMod ->
            located hsMod $ p_hsModuleLike inci namePrefix Nothing []
          Nothing -> do
            located modName $ p_hsmodLikeName namePrefix
            newline *> newline
      IncludeD IncludeDecl {..} -> do
        txt "dependency"
        space
        when idSignatureInclude $ do
          txt "signature"
          space
        let p_hsUnitId (HsUnitId name substs) = do
              atom name
              let p_subst (lModName, lModId) = do
                    located lModName atom
                    equals
                    located lModId $ \case
                      HsModuleVar lModName' -> do
                        txt "<"
                        located lModName' atom
                        txt ">"
                      HsModuleId lUnitId lmodName' -> do
                        located lUnitId p_hsUnitId
                        txt ":"
                        located lmodName' atom
              unless (null substs) $
                brackets N $ sep commaDel (located' p_subst) substs
        located idUnitId p_hsUnitId
        for_ idModRenaming $ \renamings -> do
          space
          let p_renaming Renaming {..} = do
                located renameFrom atom
                for_ renameTo $ \lto -> do
                  space
                  txt "as"
                  space
                  located lto atom
          parens N $ sep commaDel (located' p_renaming) renamings
        newline
