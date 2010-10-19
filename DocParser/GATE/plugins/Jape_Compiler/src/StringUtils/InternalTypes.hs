-- #hide
-----------------------------------------------------------------------------------------
{-| Module      : StringUtils.PackedString.Internals
    Copyright   : (C) Krasimir Angelov 2005
    License     : BSD3

    Maintainer  : kr.angelov@gmail.com
    Stability   : 
    Portability : 
-}
-----------------------------------------------------------------------------------------

module StringUtils.InternalTypes (
        PackedString(..),
        KeyString(..),
        StringBuffer(..),
        SrcLoc(..)
      ) where

import GHC.Base

-- -----------------------------------------------------------------------------
-- The PackedString type

data PackedString = PS ByteArray# Int# Int#


-- -----------------------------------------------------------------------------
-- The KeyString type

data KeyString = KS Int# ByteArray# Int#


-- -----------------------------------------------------------------------------
-- The StringBuffer type

data StringBuffer
 = StringBuffer
     ByteArray#
     Int#         -- length
     Int#         -- current pos
     Int#         -- lexeme  pos
     !SrcLoc      -- current SrcLoc


-- -----------------------------------------------------------------------------
-- The SrcLoc type

data SrcLoc
  = SrcLoc PackedString	        -- A precise location (file name)
           {-# UNBOXED #-} !Int -- line number, begins at 1
           {-# UNBOXED #-} !Int -- column number, begins at 1

  | ImportedLoc	PackedString  -- Module name
  | GeneralLoc  PackedString  -- Just a general indication
