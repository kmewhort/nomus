-----------------------------------------------------------------------------
-- |
-- Module      : StringUtils.KeyString
-- Copyright   : (C) Krasimir Angelov 2005
-- License     : BSD3
--
-- Maintainer  : Krasimir Angelov <kr.angelov@gmail.com>
-- Stability   : alpha
-- Portability : GHC only
--
-- Each @KeyString@ is a compact representation of character string which in
-- addition has an unique key in the context of some @KeyStringScope@. 
-- The uniqueness of the keys is not guaraneed if the strings were created 
-- with different scopes. The comparison of the @KeyString@s is based 
-- on these keys, so they aren't lexicographicaly ordered. The comparison is
-- always O(1).
--
-----------------------------------------------------------------------------

module StringUtils.KeyString (
        -- * The @KeyString@ type
        KeyString,
        KeyStringScope,
        
        -- * Introducing @KeyString@s
        newScope,  -- :: IO KeyStringScope
        keyString, -- :: KeyStringScope -> PackedString -> KeyString 
        
        -- * @KeyString@ properties
        keyStringID,       -- :: KeyString -> Int
        keyStringPS,       -- :: KeyString -> PackedString
        
        -- * Packing and unpacking @KeyString@ from\/to string
        pack,              -- :: KeyStringScope -> String -> KeyString
        unpack,            -- :: KeyString -> String
      ) where

import qualified StringUtils.PackedString as PS
import StringUtils.InternalTypes
import GHC.Base
import GHC.IOBase
import Foreign

data KeyStringScope
  = KSScope
      (MutableByteArray# RealWorld)           -- used as IORef Int
      (MutableArray# RealWorld [KeyString])

instance Eq KeyString where
  (KS uid1# _ _) == (KS uid2# _ _) = uid1# ==# uid2#

instance Ord KeyString where
  (KS uid1# _ _) >  (KS uid2# _ _) = uid1# >#  uid2#
  (KS uid1# _ _) >= (KS uid2# _ _) = uid1# >=# uid2#
  (KS uid1# _ _) <  (KS uid2# _ _) = uid1# <#  uid2#
  (KS uid1# _ _) <= (KS uid2# _ _) = uid1# <=# uid2#

instance Show KeyString where
  showsPrec p ps r = showsPrec p (unpack ps) r

hASH_TBL_SIZE = 4091

-- | Create a new @KeyStringScope@
newScope :: IO KeyStringScope
newScope = IO $ \s1# ->
  case newByteArray# n# s1# of { (# s2#, uidRef# #) ->
    case writeIntArray# uidRef# 0# 0# s2# of { s3# ->
      case newArray# hASH_TBL_SIZE# [] s3# of  { (# s4#, arr# #) ->
        (# s4#, KSScope uidRef# arr# #)
      }
    }
  }
  where
    (I# hASH_TBL_SIZE#) = hASH_TBL_SIZE
    (I# n#) = sizeOf hASH_TBL_SIZE

-- | Convert 'PackedString' to @KeyString@. The function
-- is pure so it will return one and the same @KeyString@
-- if it applied multiple times to equal packed strings.
keyString :: KeyStringScope -> PackedString -> KeyString
keyString (KSScope uidRef# tbl#) (PS barr# start# len#) =
  case readArray# tbl# h realWorld# of
    (# s1#, [] #) ->
       -- no match, add it to table by copying out the
       -- the string into a ByteArray
       case newByteArray# len# s1# of { (# s2#, arr_in# #) ->
         case fill_in arr_in# 0# s2# of { s3# ->
           case unsafeFreezeByteArray# arr_in# s3# of { (# s4#, frozen# #) ->
             case readIntArray# uidRef# 0# s4# of { (# s5#, uid# #) ->
               case writeIntArray# uidRef# 0# (uid# +# 1#) s5# of { s6# ->
                 let f_str = KS uid# frozen# len# in
                 case writeArray# tbl# h [f_str] s6# of { s7# ->
                   f_str
                 }
               }
             }
           }
	     }
       }
    (# s1#, ls #) ->
       -- non-empty `bucket', scan the list looking
       -- entry with same length and compare byte by byte. 
       case bucket_match ls start# len# barr# of
         Nothing -> 
           case newByteArray# len# s1# of { (# s2#, arr_in# #) ->
             case fill_in arr_in# 0# s2# of { s3# ->
               case unsafeFreezeByteArray# arr_in# s3# of { (# s4#, frozen# #) ->
                 case readIntArray# uidRef# 0# s4# of { (# s5#, uid# #) ->
                   case writeIntArray# uidRef# 0# (uid# +# 1#) s5# of { s6# ->
                     let f_str = KS uid# frozen# len# in
                     case writeArray# tbl# h (f_str:ls) s6# of { s7# ->
                       f_str
	                 }
	               }
	             }
	           }
             }
           }
         (Just v) -> v
 where
   h = hashSubStrBA barr# start# len#
   
   unpack arr# nh c#
      | nh >=# c# = []
      | otherwise = C# ch : unpack arr# (nh +# 1#) c#
      where
        ch = indexCharArray# arr# nh

   bucket_match :: [KeyString] -> Int# -> Int# -> ByteArray# -> Maybe KeyString
   bucket_match []     _      _    _   = Nothing
   bucket_match (v:ls) start# len# ba# =
     case v of
       KS _ barr# l#
         | len# ==# l# && memcmp_baoff_ba ba# start# barr# len#
             -> Just v
       _     -> bucket_match ls start# len# ba#

   memcmp_baoff_ba ba1# idx# ba2# len#
     | len# ==# 0# = True
     | eqChar# (indexCharArray# ba1# (idx# +# len# -# 1#)) 
               (indexCharArray# ba2# (len# -# 1#)) 
                   = memcmp_baoff_ba ba1# idx# ba2# (len# -# 1#)
     | otherwise   = False

   fill_in arr_in# idx# s1#
      | idx# ==# len# = s1#
      | otherwise     = 
           case writeCharArray# arr_in# idx# (indexCharArray# barr# (start# +# idx#)) s1# of { s2# -> 
              fill_in arr_in# (idx# +# 1#) s2# }

hashSubStrBA  :: ByteArray# -> Int# -> Int# -> Int#
 -- use the byte array to produce a hash value between 0 & m (inclusive)
hashSubStrBA ba# start# len# = loop 0# 0#
   where 
    loop h n | n ==# len# = h
	     | otherwise  = loop h2 (n +# 1#)
	  where c = ord# (indexCharArray# ba# (start# +# n))
		h2 = (c +# (h *# 128#)) `remInt#` hASH_TBL_SIZE#

    (I# hASH_TBL_SIZE#) = hASH_TBL_SIZE

-- | \/O(1)\/ The unique string
keyStringID :: KeyString -> Int
keyStringID (KS id# _ _) = I# id#

-- | \/O(1)\/ Convert a 'KeyString' into a 'PackedString'
keyStringPS :: KeyString -> PackedString
keyStringPS (KS _ ba# len#) = PS ba# 0# len#

-- | The same as 'keyString' but uses plain 'String'
pack :: KeyStringScope -> String -> KeyString
pack kss s = keyString kss (PS.pack s)

-- | \/O(n)\/ Convert a 'KeyString' into a 'String'
unpack :: KeyString -> String
unpack ks = PS.unpack (keyStringPS ks)
