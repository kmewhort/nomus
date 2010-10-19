-----------------------------------------------------------------------------
-- |
-- Module      :  StringUtils.StringBuffer
-- Copyright   :  Krasimir Angelov 2005
-- 
-- Maintainer  :  Krasimir Angelov <kr.angelov@gmail.com>
-- Stability   :  alpha
-- Portability :  GHC only
--
-- Buffers for scanning string input stored in external arrays.
--
-----------------------------------------------------------------------------

module StringUtils.StringBuffer
         ( StringBuffer

         -- * Construction
         , readFile               -- :: FilePath     -> IO StringBuffer
         , stringToStringBuffer   -- :: String       -> IO StringBuffer

         -- * Lookup
         , currentChar            -- :: StringBuffer -> Char
         , prevChar               -- :: StringBuffer -> Char -> Char
         , lookAhead              -- :: StringBuffer -> Int  -> Char

         -- * Moving
         , stepOn                 -- :: StringBuffer -> StringBuffer
         , atEnd		  -- :: StringBuffer -> Bool
         , getSrcLoc              -- :: StringBuffer -> SrcLoc
         , setSrcLoc              -- :: SrcLoc -> StringBuffer -> StringBuffer

         -- * Conversion
         , startLexeme            -- :: StringBuffer -> StringBuffer
         , lexemeToString         -- :: StringBuffer -> String
         , lexemeToPackedString   -- :: StringBuffer -> PackedString
         , lexemeToKeyString      -- :: KeyStringScope -> StringBuffer -> KeyString
         ) where

import Prelude hiding (readFile)
import StringUtils.PackedString as PS
import StringUtils.KeyString as KS
import StringUtils.InternalTypes
import StringUtils.SrcLoc
import Foreign
import GHC.Base
import GHC.IOBase
import GHC.IO		( slurpFile )
import System.IO		( openBinaryFile, openFile, hFileSize, IOMode(ReadMode), hClose )
import Data.Array.IArray	( listArray )
import Data.Array.MArray 	( unsafeFreeze, newArray_ )
import Data.Array.Base		( UArray(..)  )
import Data.Array.IO		( IOArray, hGetArray )
import Data.Char		( ord )
import Data.List as List        ( map, length )

instance Show StringBuffer where
  showsPrec d sbuf = showString "<string buffer>"
-- -----------------------------------------------------------------------------
-- Construction

readFile :: FilePath -> IO StringBuffer
readFile fname = do
   h <- openFile fname ReadMode
   size <- hFileSize h
   let size_i = fromIntegral size
   arr <- newArray_ (0,size_i-1)
   r@(I# sz#) <- if size_i == 0 then return 0 else hGetArray h arr size_i
   hClose h
   frozen <- unsafeFreeze arr
   case frozen of
      UArray _ _ bytearr# -> return (StringBuffer bytearr# sz# 0# 0# (mkSrcLoc (PS.pack fname) 1 1))

stringToStringBuffer str = do
  let size@(I# sz#) = List.length str
      arr = listArray (0,size-1) (List.map (fromIntegral.ord) str)
		 :: UArray Int Word8
  case arr of
	UArray _ _ bytearr# -> return (StringBuffer bytearr# sz# 0# 0# (mkGeneralSrcLoc (PS.pack "<string>")))

-- -----------------------------------------------------------------------------
-- Lookup

currentChar  :: StringBuffer -> Char
currentChar (StringBuffer arr# l# current# _ _) = C# (indexCharArray# arr# current#)

prevChar :: StringBuffer -> Char -> Char
prevChar (StringBuffer _ _ 0# _ _) deflt = deflt
prevChar s deflt = lookAhead s (-1)

lookAhead :: StringBuffer -> Int  -> Char
lookAhead (StringBuffer arr# l# c# _ _) (I# i#) = C# (indexCharArray# arr# off)
  where 
    off = c# +# i#

-- -----------------------------------------------------------------------------
-- Moving

stepOn :: StringBuffer -> StringBuffer
stepOn (StringBuffer arr# len# c# l# srcLoc) = StringBuffer arr# len# (c# +# 1#) l# srcLoc'
  where
    srcLoc' = advanceSrcLoc (C# (indexCharArray# arr# c#)) srcLoc

atEnd :: StringBuffer -> Bool
atEnd (StringBuffer _ l# c# _ _) = l# ==# c#

getSrcLoc :: StringBuffer -> SrcLoc
getSrcLoc (StringBuffer _ _ _ _ srcLoc) = srcLoc

setSrcLoc :: SrcLoc -> StringBuffer -> StringBuffer
setSrcLoc srcLoc (StringBuffer arr# len# c# l# _) = StringBuffer arr# len# c# l# srcLoc

-- -----------------------------------------------------------------------------
-- Conversion

startLexeme :: StringBuffer -> StringBuffer
startLexeme (StringBuffer arr# len# c# _ srcLoc) = StringBuffer arr# len# c# c# srcLoc

lexemeToString :: StringBuffer -> String
lexemeToString (StringBuffer arr# _ c# l# _) = unpack l#
 where
    unpack nh
      | nh >=# c# = []
      | otherwise = C# ch : unpack (nh +# 1#)
      where
        ch = indexCharArray# arr# nh

lexemeToPackedString :: StringBuffer -> PackedString
lexemeToPackedString (StringBuffer ba# _ c# l# _) = PS ba# l# (c# -# l#)

lexemeToKeyString :: KeyStringScope -> StringBuffer -> KeyString
lexemeToKeyString kss buffer = keyString kss (lexemeToPackedString buffer)
