-----------------------------------------------------------------------------
-- |
-- Module      :  StringUtils.KeyStringSet
-- Copyright   :  Krasimir Angelov 2005
-- 
-- Maintainer  :  Krasimir Angelov <kr.angelov@gmail.com>
-- Stability   :  alpha
-- Portability :  GHC only
--
-- An efficient implementation of maps from 'KeyString' to values 
-- (dictionaries). 
-----------------------------------------------------------------------------

module StringUtils.KeyStringSet
          ( KeyStringSet
          , empty, singleton, member, null, insert, fromList, delete
          ) where

import Prelude hiding (null, lookup)
import StringUtils.KeyString
import qualified Data.IntSet as ISet

newtype KeyStringSet = KeyStringSet ISet.IntSet

empty :: KeyStringSet
empty = KeyStringSet (ISet.empty)

singleton :: KeyString -> KeyStringSet
singleton ks = KeyStringSet (ISet.singleton (keyStringID ks))

member :: KeyString -> KeyStringSet -> Bool
member ks (KeyStringSet set) = ISet.member (keyStringID ks) set

null :: KeyStringSet -> Bool
null (KeyStringSet set) = ISet.null set

insert :: KeyString -> KeyStringSet -> KeyStringSet
insert fs (KeyStringSet set) = KeyStringSet (ISet.insert (keyStringID fs) set)

fromList :: [KeyString] -> KeyStringSet
fromList vs = KeyStringSet (mkSet ISet.empty vs)
  where
    mkSet set []          = set
    mkSet set (ks:kss) = 
      let set' = ISet.insert (keyStringID ks) set 
      in set' `seq` mkSet set' kss

delete :: KeyString -> KeyStringSet -> KeyStringSet
delete fs (KeyStringSet set) = KeyStringSet (ISet.delete (keyStringID fs) set)
