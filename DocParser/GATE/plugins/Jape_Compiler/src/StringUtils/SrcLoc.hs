-----------------------------------------------------------------------------
-- |
-- Module      : StringUtils.SrcLoc
-- Copyright   : (C) Krasimir Angelov 2005
-- License     : BSD3
--
-- Maintainer  : Krasimir Angelov <kr.angelov@gmail.com>
-- Stability   : alpha
-- Portability : GHC only
-----------------------------------------------------------------------------

module StringUtils.SrcLoc
          ( SrcLoc			-- Abstract

          -- * Construction
          , mkSrcLoc, mkImportedSrcLoc, mkGeneralSrcLoc
          
          -- * Predicates
          , isGoodSrcLoc, isImportedSrcLoc, isGeneralSrcLoc
 
          -- * Operations         
          , advanceSrcLoc

          -- * Deconstruction
          , srcLocFile
          , srcLocLine
          , srcLocCol
          ) where

import StringUtils.PackedString
import StringUtils.InternalTypes

-- SrcLoc is an instance of Ord so that we can sort error messages easily
instance Eq SrcLoc where
  loc1 == loc2 = case loc1 `cmpSrcLoc` loc2 of
		   EQ    -> True
		   other -> False

instance Ord SrcLoc where
  compare = cmpSrcLoc

cmpSrcLoc (GeneralLoc s1) (GeneralLoc s2) = s1 `compare` s2
cmpSrcLoc (GeneralLoc _)  other      	      = LT

cmpSrcLoc (ImportedLoc _)  (GeneralLoc _)  = GT
cmpSrcLoc (ImportedLoc m1) (ImportedLoc m2)  = m1 `compare` m2
cmpSrcLoc (ImportedLoc _)  other	     = LT

cmpSrcLoc (SrcLoc s1 l1 c1) (SrcLoc s2 l2 c2)      
  = (s1 `compare` s2) `thenCmp` (l1 `cmpline` l2) `thenCmp` (c1 `cmpline` c2)
  where
    l1 `cmpline` l2 | l1 <  l2  = LT
                    | l1 == l2  = EQ
                    | otherwise = GT 
			
    thenCmp :: Ordering -> Ordering -> Ordering
    thenCmp EQ    any = any
    thenCmp other any = other
cmpSrcLoc (SrcLoc _ _ _) other = GT

-------------------------------------------------------------------------------
-- Construction

mkSrcLoc :: PackedString -> Int -> Int -> SrcLoc
mkSrcLoc fname line col = SrcLoc fname line col

mkGeneralSrcLoc :: PackedString -> SrcLoc
mkGeneralSrcLoc = GeneralLoc

mkImportedSrcLoc :: PackedString -> SrcLoc
mkImportedSrcLoc = ImportedLoc

-------------------------------------------------------------------------------
-- Predicates

isGoodSrcLoc :: SrcLoc -> Bool
isGoodSrcLoc (SrcLoc _ _ _) = True
isGoodSrcLoc other          = False

isImportedSrcLoc :: SrcLoc -> Bool
isImportedSrcLoc (ImportedLoc _) = True
isImportedSrcLoc other           = False

isGeneralSrcLoc :: SrcLoc -> Bool
isGeneralSrcLoc (GeneralLoc _) = True
isGeneralSrcLoc other          = False

-------------------------------------------------------------------------------
-- Operations

advanceSrcLoc :: Char -> SrcLoc -> SrcLoc
advanceSrcLoc '\t' (SrcLoc f l c) = SrcLoc f  l (tab c)
advanceSrcLoc '\n' (SrcLoc f l c) = SrcLoc f  (l + 1) 0
advanceSrcLoc _    (SrcLoc f l c) = SrcLoc f  l (c + 1)
advanceSrcLoc _    loc	          = loc	-- Better than nothing

-- Advance to the next tab stop.  Tabs are at column positions 0, 8, 16, etc.
tab :: Int -> Int
tab c = (c `quot` 8 + 1) * 8

-------------------------------------------------------------------------------
-- Deconstruction

-- | return the file name part
srcLocFile :: SrcLoc -> PackedString
srcLocFile (SrcLoc fname _ _) = fname
srcLocFile _                  = pack "<unknown file>"

-- | return the line part
srcLocLine :: SrcLoc -> Int
srcLocLine (SrcLoc _ l c) = l
srcLocLine _              = 1

-- | return the column part
srcLocCol :: SrcLoc -> Int
srcLocCol (SrcLoc _ l c) = c
srcLocCol _              = 1
