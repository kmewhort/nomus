-----------------------------------------------------------------------------
-- |
-- Module      :  StringUtils.KeyStringMap
-- Copyright   :  Krasimir Angelov 2005
-- 
-- Maintainer  :  Krasimir Angelov <kr.angelov@gmail.com>
-- Stability   :  alpha
-- Portability :  GHC only
--
-- An efficient implementation of maps from 'KeyString' to values 
-- (dictionaries). 
-----------------------------------------------------------------------------

module StringUtils.KeyStringMap 
          ( KeyStringMap
          , empty, singleton, lookup, null, insert, fromList, delete
          ) where

import Prelude hiding (null, lookup)
import StringUtils.KeyString
import qualified Data.IntMap as IMap

newtype KeyStringMap a = KeyStringMap (IMap.IntMap a)

instance Show (KeyStringMap a) where
  showsPrec d _ = showString "<KeyStringMap>"

empty :: KeyStringMap a
empty = KeyStringMap (IMap.empty)

singleton :: KeyString -> a -> KeyStringMap a
singleton ks x = KeyStringMap (IMap.singleton (keyStringID ks) x)

lookup :: KeyString -> KeyStringMap a -> Maybe a
lookup ks (KeyStringMap map) = IMap.lookup (keyStringID ks) map

null :: KeyStringMap a -> Bool
null (KeyStringMap map) = IMap.null map

insert :: KeyString -> a -> KeyStringMap a -> KeyStringMap a
insert ks val (KeyStringMap map) = KeyStringMap (IMap.insert (keyStringID ks) val map)

fromList :: [(KeyString, a)] -> KeyStringMap a
fromList vs = KeyStringMap (mkMap IMap.empty vs)
  where
    mkMap map []          = map
    mkMap map ((fs,v):vs) = 
      let map' = IMap.insert (keyStringID fs) v map 
      in map' `seq` mkMap map' vs

delete :: KeyString -> KeyStringMap a -> KeyStringMap a
delete fs (KeyStringMap map) = KeyStringMap (IMap.delete (keyStringID fs) map)
