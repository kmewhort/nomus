-----------------------------------------------------------------------------
-- |
-- Module      :  StringUtils.PackedString
-- Copyright   :  Krasimir Angelov 2005
-- 
-- Maintainer  :  Krasimir Angelov <kr.angelov@gmail.com>
-- Stability   :  alpha
-- Portability :  GHC only
--
-- A time and space-efficient implementation of strings as packed
-- byte arrays.
--
-- This module is intended to be imported @qualified@, to avoid name
-- clashes with Prelude functions.  eg.
--
-- >  import qualified StringUtils.PackedString as PS
--
-----------------------------------------------------------------------------

module StringUtils.PackedString (
        -- * The @PackedString@ type
        PackedString,  -- abstract, instances: Eq, Ord, Show

        -- * Introducing and eliminating 'PackedString's
        empty,       -- :: PackedString
        pack,        -- :: String -> PackedString
	unpack,      -- :: PackedString -> String

        -- * Basic list-like interface
        cons,        -- :: Char -> PackedString -> PackedString
        snoc,        -- :: PackedString -> Char -> PackedString
        append,      -- :: PackedString -> PackedString -> PackedString
        head,        -- :: PackedString -> Char
        tail,        -- :: PackedString -> PackedString
        last,        -- :: PackedString -> Char
        init,        -- :: PackedString -> PackedString
        null,        -- :: PackedString -> Bool
        length,      -- :: PackedString -> Int

        -- * List transformations
        map,         -- :: (Char -> Char) -> PackedString -> PackedString
        reverse,     -- :: PackedString -> PackedString

        -- * Reducing 'PackedString's
        foldl,       -- :: (a -> Char -> a) -> a -> PackedString -> a
        foldr,       -- :: (Char -> a -> a) -> a -> PackedString -> a
        foldl1,      -- :: (Char -> Char -> Char) -> PackedString -> Char
        foldr1,      -- :: (Char -> Char -> Char) -> PackedString -> Char

        -- ** Special folds
        concat,      -- :: [PackedString] -> PackedString
        any,         -- :: (Char -> Bool) -> PackedString -> Bool
        all,         -- :: (Char -> Bool) -> PackedString -> Bool
        maximum,     -- :: FastString -> Char
        minimum,     -- :: FastString -> Char
        
        -- * Substrings
        take,        -- :: Int -> PackedString -> PackedString
        drop,        -- :: Int -> PackedString -> PackedString
        splitAt,     -- :: Int -> PackedString -> (PackedString, PackedString)
	
        takeWhile,   -- :: (Char -> Bool) -> PackedString -> PackedString
        dropWhile,   -- :: (Char -> Bool) -> PackedString -> PackedString
        span,        -- :: (Char -> Bool) -> PackedString -> (PackedString, PackedString)
        break,       -- :: (Char -> Bool) -> PackedString -> (PackedString, PackedString)

        -- * Searching 'PackedString's

        -- ** Searching by equality
        elem,         -- :: Char -> PackedString -> Bool

        -- ** Searching with a predicate
        filter,      -- :: (Char -> Bool) -> PackedString -> PackedString
        find,        -- :: (Char -> Bool) -> PackedString -> Maybe Char
        
        -- * Indexing 'PackedString's
	index,       -- :: PackedString -> Int -> Char

	elemIndex,   -- :: Char -> PackedString -> Maybe Int
	elemIndices, -- :: Char -> PackedString -> [Int]
	
	findIndex,   -- :: (Char -> Bool) -> PackedString -> Maybe Int
        findIndices, -- :: (Char -> Bool) -> PackedString -> [Int]
        
        -- * I\/O with @PackedString@s
	hGet,        -- :: Handle -> Int -> IO PackedString
        hPut         -- :: Handle -> PackedString -> IO ()

      ) where

import StringUtils.InternalTypes
import Prelude hiding (reverse,head,tail,last,init,null,
                       length,map,lines,foldl,foldr,unlines,
                       concat,any,take,drop,splitAt,takeWhile,
                       dropWhile,span,break,elem,filter,unwords,
                       words,maximum,minimum,all,concatMap,
                       foldl1,foldr1,readFile,writeFile)
import GHC.Base hiding (map, foldr)
import GHC.Word
import GHC.IOBase
import GHC.Handle
import Data.Maybe(listToMaybe)
import Data.Array.Base
import Data.Array.IO hiding (index)
import qualified Data.List as List

instance Eq PackedString where
  a == b = case compare a b of
             EQ -> True
             _  -> False

instance Ord PackedString where
  compare (PS ba1# start1# len1#) (PS ba2# start2# len2#) = cmp ba1# start1# len1# ba2# start2# len2#
    where
      cmp ba1# start1# len1# ba2# start2# len2#
        | len1# ==# 0# && len2# ==# 0# = EQ
        | len1# ==# 0#                 = LT
        |                 len2# ==# 0# = GT
        | c1# `ltChar#` c2#            = LT
        | c1# `gtChar#` c2#            = GT
        | otherwise                    = cmp ba1# (start1# +# 1#) (len1# -# 1#) ba2# (start2# +# 1#) (len2# -# 1#)
        where
          c1# = indexCharArray# ba1# start1#
          c2# = indexCharArray# ba2# start2#

instance Show PackedString where
  showsPrec p ps r = showsPrec p (unpack ps) r

-- -----------------------------------------------------------------------------
-- Constructing and destructing packed strings

-- | \/O(1)\/ The empty 'PackedString'
empty :: PackedString
empty =
  case newByteArray# 0# realWorld# of { (# s1#, barr# #) ->
    case unsafeFreezeByteArray# barr# s1# of { (# s2#, frozen# #) ->
      PS frozen# 0# 0# }}
{-# NOINLINE empty #-}

-- | \/O(n)\/ Convert a 'String' into a 'PackedString'
pack :: String -> PackedString
pack str = fs
 where
  (I# length#) = List.length str

  fs =
    case newByteArray# length# realWorld# of { (# s2#, barr# #) ->
      case fill_in barr# 0# str s2# of { s3# ->
        case unsafeFreezeByteArray# barr# s3# of { (# s4#, frozen# #) ->
          PS frozen# 0# length#
        }}}  

  fill_in barr# idx# []           s1# = s1#
  fill_in barr# idx# (C# c# : cs) s1# =
    case writeCharArray# barr# idx# c# s1# of { s2#   ->
      fill_in barr# (idx# +# 1#) cs s2#
    }

-- | \/O(n)\/ Convert a 'PackedString' into a 'String'
unpack :: PackedString -> String
unpack (PS ba# s# l#) = unpack 0#
  where
    unpack n#
      | n# >=# l# = []
      | otherwise = C# ch : unpack (n# +# 1#)
      where
	    ch = indexCharArray# ba# (s# +# n#)

-- -----------------------------------------------------------------------------
-- List-like functions for FastStrings

-- | \/O(n)\/ 'cons' is analogous to (:) for lists.
cons :: Char -> PackedString -> PackedString
cons (C# c#) (PS ba# start# len#) = fs
 where
  new_len# = len# +# 1#

  fs =
    case newByteArray# new_len# realWorld# of { (# s2#, new_ba# #) ->
      case writeCharArray# new_ba# 0# c# s2# of { s3#   ->
        case fill_in new_ba# ba# 0# s3# of { s4# ->
          case unsafeFreezeByteArray# new_ba# s4# of { (# s5#, frozen# #) ->
            PS frozen# 0# new_len#
        }}}}

  fill_in new_ba# ba# idx# s1#
    | idx# >=# len# = s1#
    | otherwise     = 
        case writeCharArray# new_ba# (idx# +# 1#) (indexCharArray# ba# (start# +# idx#)) s1# of { s2#   ->
          fill_in new_ba# ba# (idx# +# 1#) s2# }

-- | \/O(n)\/ Append a character to the end of a 'PackedString'
snoc :: PackedString -> Char -> PackedString
snoc (PS ba# start# len#) (C# c#) = fs
 where
  new_len# = len# +# 1#

  fs =
    case newByteArray# new_len# realWorld# of { (# s2#, new_ba# #) ->
      case fill_in new_ba# ba# 0# s2# of { s3# ->
        case unsafeFreezeByteArray# new_ba# s3# of { (# s4#, frozen# #) ->
          PS frozen# 0# new_len#
        }}}

  fill_in new_ba# ba# idx# s1#
    | idx# >=# len# = writeCharArray# new_ba# idx# c# s1#
    | otherwise     = 
        case writeCharArray# new_ba# idx# (indexCharArray# ba# (start# +# idx#)) s1# of { s2#   ->
          fill_in new_ba# ba# (idx# +# 1#) s2# }

-- | \/O(n)\/ Append two packed strings
append :: PackedString -> PackedString -> PackedString
append (PS ba1# start1# len1#) (PS ba2# start2# len2#) = fs
 where
  new_len# = len1# +# len2#

  fs =
    case newByteArray# new_len# realWorld# of { (# s2#, new_ba# #) ->
      case fill_in new_ba# ba1# 0# start1# 0# len1# s2# of { s3# ->
        case fill_in new_ba# ba2# len1# start2# 0# len2# s3# of { s4# ->
          case unsafeFreezeByteArray# new_ba# s4# of { (# s5#, frozen# #) ->
            PS frozen# 0# new_len#
          }}}}

  fill_in new_ba# ba# new_start# start# idx# len# s1#
    | idx# >=# len# = s1#
    | otherwise     = 
        case writeCharArray# new_ba# (new_start# +# idx#) (indexCharArray# ba# (start# +# idx#)) s1# of { s2#   ->
          fill_in new_ba# ba# new_start# start# (idx# +# 1#) len# s2# }

-- | \/O(1)\/ Extract the first element of a packed string, which must be non-empty.
head :: PackedString -> Char
head (PS ba# s# len#)
  | len# ==# 0# = errorEmptyPackedString "head"
  | otherwise  = C# (indexCharArray# ba# s#)
{-# INLINE head #-}

-- | \/O(1)\/ Extract the elements after the head of a packed string, which must be non-empty.
tail :: PackedString -> PackedString
tail (PS ba# s# len#) 
  | len# ==# 0# = errorEmptyPackedString "tail"
  | len# ==# 1# = empty
  | otherwise  = PS ba# (s# +# 1#) (len# -# 1#)
{-# INLINE tail #-}

-- | \/O(1)\/ Extract the last element of a packed string, which must be finite and non-empty.
last :: PackedString -> Char
last ps@(PS ba# s# len#)        -- ps ! 0 is inlined manually to eliminate a (+0)
  | len# ==# 0# = errorEmptyPackedString "last"
  | otherwise  = C# (indexCharArray# ba# (s# +# len# -# 1#))
{-# INLINE last #-}

-- | \/O(1)\/ Return all the elements of a 'PackedString' except the last one.
init :: PackedString -> PackedString
init (PS ba# s# len#) 
    | len# ==# 0# = errorEmptyPackedString "init"
    | len# ==# 1# = empty
    | otherwise = PS ba# s# (len# -# 1#)
{-# INLINE init #-}

-- | \/O(1)\/ Test whether a packed string is empty.
null :: PackedString -> Bool
null (PS _ _ l#) = l# ==# 0#
{-# INLINE null #-}

-- | \/O(1)\/ 'length' returns the length of a packed string as an 'Int'.
length :: PackedString -> Int
length (PS _ _ l#) = I# l#
{-# INLINE length #-}

-- | \/O(n)\/ 'map' @f xs@ is the packed string obtained by applying @f@ to each
-- element of @xs@, i.e.,
map :: (Char -> Char) -> PackedString -> PackedString
map f (PS ba# start# len#) = fs
 where
  fs =
    case newByteArray# len# realWorld# of { (# s2#, new_ba# #) ->
      case fill_in new_ba# ba# 0# s2# of { s3# ->
        case unsafeFreezeByteArray# new_ba# s3# of { (# s4#, frozen# #) ->
          PS frozen# 0# len#
        }}}

  fill_in new_ba# ba# idx# s1#
    | idx# >=# len# = s1#
    | otherwise     =
        case f (C# (indexCharArray# ba# (start# +# idx#))) of { C# c# -> 
          case writeCharArray# new_ba# idx# c# s1# of { s2#   ->
            fill_in new_ba# ba# (idx# +# 1#) s2#
          }}

-- | \/O(n)\/ 'reverse' @xs@ efficiently returns the elements of @xs@ in reverse order.
reverse :: PackedString -> PackedString
reverse (PS ba# start# len#) = fs
 where
  fs =
    case newByteArray# len# realWorld# of { (# s2#, new_ba# #) ->
      case fill_in new_ba# ba# 0# s2# of { s3# ->
        case unsafeFreezeByteArray# new_ba# s3# of { (# s4#, frozen# #) ->
          PS frozen# 0# len#
        }}}

  fill_in new_ba# ba# idx# s1#
    | idx# >=# len# = s1#
    | otherwise     =
        case writeCharArray# new_ba# (len# -# idx# -# 1#) (indexCharArray# ba# (start# +# idx#)) s1# of { s2#   ->
          fill_in new_ba# ba# (idx# +# 1#) s2#
        }

-- | 'foldl', applied to a binary operator, a starting value (typically
-- the left-identity of the operator), and a packed string, reduces the
-- packed string using the binary operator, from left to right.
foldl :: (a -> Char -> a) -> a -> PackedString -> a
foldl f v (PS ba# start# len#) = foldl 0# v
  where
    foldl idx# v
      | idx# >=# len# = v
      | otherwise     = foldl (idx# +# 1#) (f v c)
      where
        c = C# (indexCharArray# ba# (start# +# idx#))

-- | 'foldr', applied to a binary operator, a starting value
-- (typically the right-identity of the operator), and a packed string,
-- reduces the packed string using the binary operator, from right to left.
foldr :: (Char -> a -> a) -> a -> PackedString -> a
foldr f v (PS ba# start# len#) = foldr 0#
  where
    foldr idx#
      | idx# >=# len# = v
      | otherwise     = f c (foldr (idx# +# 1#))
      where
        c = C# (indexCharArray# ba# (start# +# idx#))

-- | 'foldl1' is a variant of 'foldl' that has no starting value
-- argument, and thus must be applied to non-empty 'PackedString's.
foldl1 :: (Char -> Char -> Char) -> PackedString -> Char
foldl1 f ps
    | null ps   = errorEmptyPackedString "foldl1"
    | otherwise = foldl f (head ps) (tail ps)

-- | 'foldr1' is a variant of 'foldr' that has no starting value argument,
-- and thus must be applied to non-empty 'PackedString's
foldr1 :: (Char -> Char -> Char) -> PackedString -> Char
foldr1 f ps
    | null ps        = errorEmptyPackedString "foldr1"
    | length ps == 1 = head ps
    | otherwise      = f (head ps) (foldr1 f (tail ps))

-- | Concatenate a list of packed strings.
concat :: [PackedString] -> PackedString
concat = List.foldr append empty

-- | Applied to a predicate and a packed string, 'any' determines if
-- any character in the 'PackedString' satisfies the predicate.
any :: (Char -> Bool) -> PackedString -> Bool
any pred (PS ba# start# len#) = any 0#
  where
    any idx# | idx# >=# len# = False
             | pred c        = True
             | otherwise     = any (idx# +# 1#)
             where
                c = C# (indexCharArray# ba# (start# +# idx#))

-- | Applied to a predicate and a 'PackedString', 'all' determines if
-- all characters in the 'PackedString' satisfy the predicate.
all :: (Char -> Bool) -> PackedString -> Bool
all pred (PS ba# start# len#) = all 0#
  where
    all idx# | idx# >=# len# = True
             | not (pred c)  = False
             | otherwise     = all (idx# +# 1#)
             where
                c = C# (indexCharArray# ba# (start# +# idx#))

-- | 'minimum' returns the minimum character from a 'PackedString'
minimum :: PackedString -> Char
minimum (PS ba# start# len#)
  | len# ==# 0# = errorEmptyPackedString "minimum"
  | otherwise   = minimum (indexCharArray# ba# start#) 1#
  where
    minimum c1# idx#
      | idx# >=# len#     = C# c1#
      | c1# `gtChar#` c2# = minimum c2# (idx# +# 1#)
      | otherwise         = minimum c1# (idx# +# 1#)
      where
        c2# = indexCharArray# ba# (start# +# idx#)

-- | 'maximum' returns the maximum character from a 'PackedString'
maximum :: PackedString -> Char
maximum (PS ba# start# len#)
  | len# ==# 0# = errorEmptyPackedString "maximum"
  | otherwise   = maximum (indexCharArray# ba# start#) 1#
  where
    maximum c1# idx#
      | idx# >=# len#     = C# c1#
      | c1# `ltChar#` c2# = maximum c2# (idx# +# 1#)
      | otherwise         = maximum c1# (idx# +# 1#)
      where
        c2# = indexCharArray# ba# (start# +# idx#)

-- | 'take' @n@, applied to a packed string @xs@, returns the prefix
-- of @xs@ of length @n@, or @xs@ itself if @n > 'length' xs@.
take :: Int -> PackedString -> PackedString
take (I# n#) ps@(PS ba# start# len#)
    | n# <=# 0#   = empty
    | n# >=# len# = ps
    | otherwise   = PS ba# start# n#
{-# INLINE take #-}

-- | 'drop' @n xs@ returns the suffix of @xs@ after the first @n@
-- elements, or @[]@ if @n > 'length' xs@.
drop  :: Int -> PackedString -> PackedString
drop (I# n#) ps@(PS ba# start# len#)
    | n# <=# 0#   = ps
    | n# >=# len# = empty
    | otherwise   = PS ba# (start# +# n#) (len# -# n#)
{-# INLINE drop #-}

-- | 'splitAt' @n xs@ is equivalent to @('take' n xs, 'drop' n xs)@.
splitAt :: Int -> PackedString -> (PackedString, PackedString)
splitAt (I# n#) ps@(PS ba# start# len#)
    | n# <=# 0#   = (empty,ps)
    | n# >=# len# = (ps,empty)
    | otherwise   = (PS ba# start# n#, PS ba# (start# +# n#) (len# -# n#))
{-# INLINE splitAt #-}

-- | 'takeWhile', applied to a predicate @p@ and a packed string @xs@,
-- returns the longest prefix (possibly empty) of @xs@ of elements that
-- satisfy @p@.
takeWhile :: (Char -> Bool) -> PackedString -> PackedString
takeWhile pred ps = fst (span pred ps)

-- | 'dropWhile' @p xs@ returns the suffix remaining after 'takeWhile' @p xs@.
dropWhile :: (Char -> Bool) -> PackedString -> PackedString
dropWhile pred ps = snd (span pred ps)

-- | 'span' @p xs@ breaks the packed string into two segments. It is
-- equivalent to @('takeWhile' p xs, 'dropWhile' p xs)@
span :: (Char -> Bool) -> PackedString -> (PackedString, PackedString)
span p ps = break (not . p) ps

-- | 'break' @p@ is equivalent to @'span' ('not' . p)@.
break :: (Char -> Bool) -> PackedString -> (PackedString, PackedString)
break pred ps@(PS ba# start# len#)
  | pred c0   = (empty,ps)
  | otherwise = find 1#
  where
    c0 = C# (indexCharArray# ba# start#)
    
    find idx# | idx# >=# len# = (ps,empty)
              | pred c        = (PS ba# start# idx#, PS ba# (start# +# idx#) (len# -# idx#))
              | otherwise     = find (idx# +# 1#)
              where
                c = C# (indexCharArray# ba# (start# +# idx#))

-- | 'elem' is the 'PackedString' membership predicate.
elem :: Char -> PackedString -> Bool
elem c ps = case elemIndex c ps of
    Nothing -> False
    Just _  -> True

-- | \/O(n)\/ 'filter', applied to a predicate and a packed string,
-- returns a packed string containing those characters that satisfy the
-- predicate.
filter :: (Char -> Bool) -> PackedString -> PackedString
filter pred (PS ba# start# len#) = fs
 where
  fs =
    case newByteArray# len# realWorld# of { (# s2#, new_ba# #) ->
      case fill_in new_ba# ba# 0# 0# s2# of { s3# ->
        case unsafeFreezeByteArray# new_ba# s3# of { (# s4#, frozen# #) ->
          PS frozen# 0# len#
        }}}

  fill_in new_ba# ba# new_idx# idx# s1#
    | idx# >=# len# = s1#
    | pred (C# c#)  =
        case writeCharArray# new_ba# new_idx# c# s1# of { s2#   ->
          fill_in new_ba# ba# (new_idx# +# 1#) (idx# +# 1#) s2# 
        }
    | otherwise     =
        fill_in new_ba# ba# new_idx# (idx# +# 1#) s1# 
    where
      c# = indexCharArray# ba# (start# +# idx#)

-- | \/O(n)\/ The 'find' function takes a predicate and a packed string
-- and returns the first element in matching the predicate, or 'Nothing'
-- if there is no such element.
find :: (Char -> Bool) -> PackedString -> Maybe Char
find pred (PS ba# start# len#) = find 0#
  where
    find idx# | idx# >=# len# = Nothing
              | pred c        = Just c
              | otherwise     = find (idx# +# 1#)
              where
                c = C# (indexCharArray# ba# (start# +# idx#))

-- | 'PackedString' index (subscript) operator, starting from 0.
index :: PackedString -> Int -> Char
index (PS ba# start# len#) (I# n#) 
    | n# <# 0#    = error "PackedString.index: negative index"
    | n# >=# len# = error "PackedString.index: index too large"
    | otherwise   = C# (indexCharArray# ba# (start# +# n#))
{-# INLINE index #-}

-- | \/O(n)\/ The 'elemIndex' function returns the index of the first element
-- in the given 'PackedString' which is equal (by memchr) to the query
-- element, or 'Nothing' if there is no such element.
elemIndex :: Char -> PackedString -> Maybe Int
elemIndex c ps = listToMaybe (elemIndices c ps)
{-# INLINE elemIndex #-}

-- | The 'elemIndices' function extends 'elemIndex', by returning the
-- indices of all elements equal to the query element, in ascending order.
elemIndices :: Char -> PackedString -> [Int]
elemIndices (C# c1#) (PS ba# start# len#) = find 0#
  where
    find idx# | idx# >=# len#     = []
              | c1# `eqChar#` c2# = (I# idx#) : find (idx# +# 1#)
              | otherwise         = find (idx# +# 1#)
              where
                c2# = indexCharArray# ba# (start# +# idx#)

-- | The 'findIndex' function takes a predicate and a 'PackedString'
-- and returns the index of the first element in the packed string
-- satisfying the predicate.
findIndex :: (Char -> Bool) -> PackedString -> Maybe Int
findIndex f ps = listToMaybe (findIndices f ps)

-- | The 'findIndices' function extends 'findIndex', by returning the
-- indices of all elements satisfying the predicate, in ascending order.
findIndices :: (Char -> Bool) -> PackedString -> [Int]
findIndices pred (PS ba# start# len#) = find 0#
  where
    find idx# | idx# >=# len# = []
              | pred c        = (I# idx#) : find (idx# +# 1#)
              | otherwise     = find (idx# +# 1#)
              where
                c = C# (indexCharArray# ba# (start# +# idx#))


-- -----------------------------------------------------------------------------
-- Input/Output

-- | Outputs a 'PackedString' to the specified 'Handle'.
hPut handle (PS ba# start# len#)
  | len# ==# 0# = return ()
  | otherwise   = do
      wantWritableHandle "hPut" handle $ 
        \ handle_@Handle__{ haFD=fd, haBuffer=ref, haIsStream=stream } -> do

          old_buf@Buffer{ bufBuf=old_raw, bufRPtr=r, bufWPtr=w, bufSize=size }
	    <- readIORef ref

	  let len   = I# len#
	      I# w# = w

          -- enough room in handle buffer?
          if (size - w > len)
		-- There's enough room in the buffer:
		-- just copy the data in and update bufWPtr.
	    then do IO (copy_to old_raw ba# w# start# 0# len#)
		    writeIORef ref old_buf{ bufWPtr = w + len }
		    return ()

		-- else, we have to flush
	    else do flushed_buf <- flushWriteBuffer fd stream old_buf
		    writeIORef ref flushed_buf
		    let this_buf = 
			    Buffer{ bufBuf=unsafeCoerce# ba#, bufState=WriteBuffer, 
				    bufRPtr=0, bufWPtr=len, bufSize=len }
		    flushWriteBuffer fd stream this_buf
		    return ()
  where
    copy_to new_ba# ba# new_start# start# idx# len# s1#
      | idx# >=# len# = (# s1#, () #)
      | otherwise     = 
          case writeCharArray# new_ba# (new_start# +# idx#) (indexCharArray# ba# (start# +# idx#)) s1# of { s2#   ->
            copy_to new_ba# ba# new_start# start# (idx# +# 1#) len# s2# }

hGet :: Handle -> Int -> IO PackedString
hGet _      0         = return empty
hGet handle n@(I# n#) = do
   arr <- newArray_ (0,n-1)
   r <- hGetArray handle arr n
   if r /= n
     then ioError (userError "short read of file")
     else do
       frozen <- unsafeFreeze arr
       case frozen of
         UArray _ _ ba# -> return (PS ba# 0# n#)

-- -----------------------------------------------------------------------------

-- Common up near identical calls to `error' to reduce the number
-- constant strings created when compiled:
errorEmptyPackedString :: String -> a
errorEmptyPackedString fun = error ("StringUtils.PackedString." ++ fun ++ ": empty PackedString")
