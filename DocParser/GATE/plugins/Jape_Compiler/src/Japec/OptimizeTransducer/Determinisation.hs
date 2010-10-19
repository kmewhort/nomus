{-# OPTIONS -fglasgow-exts #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Japec.OptimizeTransducer.Determinisation
-- Copyright   : (C) Ontotext 2005
-- License     : LGPL
--
-- Maintainer  : Krasimir Angelov <kr.angelov@sirma.bg>
-- Stability   : beta
-- Portability : portable
--
-- The module is an implementation of the automata determinisation.
-- The automata processes annotations instead of characters and
-- the annotations can subsume. For this reason after the determinisation
-- the automata is still nondeterministic but the number of the required
-- constraint checks is reduced significantly.
--
-----------------------------------------------------------------------------

module Japec.OptimizeTransducer.Determinisation(determinisation) where

import Japec.TSyntax
import Data.Array
import Data.Map as Map hiding ((!))

-- | The entry point to the determinisation.
determinisation :: TransitionGraph -> TransitionGraph
determinisation tgraph =
  let (env, start', vis) = genId [start] tgraph emptyDTEnv []
      lst = loop vis env
  in array (getBounds lst) lst
  where
    getBounds xs = (0,maximum [id | (id,_) <- xs])

    (start,end) = bounds tgraph

    loop []                         env = []
    loop ((vi',closure,action):vis) env =
      let (env',links,vis') = group [link | v1 <- closure, link <- fst (tgraph ! v1)] env [] vis
      in (vi',(links, action)) : loop vis' env'

    group []         env links vis = (env,links,vis)
    group ((v,c):xs) env links vis =
      let (vs,c',xs')  = filter c xs
          (env',v',vis') = genId (insertSorted v vs) tgraph env vis
      in group xs' env' ((v',c'):links) vis'
      where
        filter c []            = ([],c,[])
        filter c (x@(v,c1):xs)
          | (EQ,c) <- mergeEdges c c1
                      = let (vs,c',xs') = filter c xs
                        in (insertSorted v vs,c',xs')
          | otherwise = let (vs,c',xs') = filter c xs
                        in (vs,c',x:xs')

        insertSorted :: Vertex -> [Vertex] -> [Vertex]
        insertSorted n [] = [n]
        insertSorted n all@(n1:ns)
          | n == n1   = all
          | n >  n1   = n1 : insertSorted n ns
    	  | otherwise = n  : all


-- | In the determinisation for each vertex is computed the set of
-- all neighbouring vertices accessible through edges annotated with 
-- one and the same constraint. For each such set is generated a new 
-- vertex in the new graph. The @DTEnv@ is used to keep the 
-- correspondence. The set is represented as a sorted list
-- of vertices.
data DTEnv = DTEnv !Vertex !(Map [Vertex] Vertex)

-- | An empty 'DTEnv'
emptyDTEnv = DTEnv 0 Map.empty

-- | The type is used internally to keep the pair of
--    * The vertex in the new graph
--    * The set from which this vertex has been generated
--    * The action which will be associated to this vertex
type VertexClosure = (Vertex,[Vertex],[TransducerAction])

-- | New vertexes generation
genId :: [Vertex] -> TransitionGraph -> DTEnv -> [VertexClosure] -> (DTEnv, Vertex, [VertexClosure])
genId closure tgraph env@(DTEnv id new_states) vis =
  case Map.lookup closure new_states of
    Just v' -> (env,                                             v',                     vis)
    Nothing -> (DTEnv (id+1) (Map.insert closure id new_states), id, (id,closure,action):vis)
  where
    action = foldr (\v -> mergeActions (snd (tgraph ! v))) [] closure
