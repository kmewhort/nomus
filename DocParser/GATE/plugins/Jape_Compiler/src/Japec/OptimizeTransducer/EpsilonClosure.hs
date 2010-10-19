{-# OPTIONS -fglasgow-exts #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Japec.OptimizeTransducer.EpsilonClosure
-- Copyright   : (C) Ontotext 2005
-- License     : LGPL
--
-- Maintainer  : Krasimir Angelov <kr.angelov@sirma.bg>
-- Stability   : beta
-- Portability : portable
--
-- The module is an implementation of the epsilon closure.
-- This is the first step in the optimiser and it is required
-- in order to perform the next steps.
--
-----------------------------------------------------------------------------

module Japec.OptimizeTransducer.EpsilonClosure(epsilonClosure) where

import Japec.TSyntax
import Data.Array
import Data.Map as Map hiding ((!))

-- | The entry point to the epsilon closure.
epsilonClosure :: TransitionGraph -> TransitionGraph
epsilonClosure tgraph =
  let r@(env, _, vis) = genId start tgraph emptyECEnv []
      lst = loop vis env
  in array (getBounds lst) lst
  where
    getBounds xs = (0,maximum [id | (id,_) <- xs])

    (start,end) = bounds tgraph
          
    loop []                         env = []
    loop ((vi',closure,action):vis) env =
      let trans_links = [link | v1 <- closure, link@(_,c) <- fst (tgraph ! v1), not (isNullConstraint c)]
          (env', trans_links',vis') =
             foldr (\(v,c) (env,links,vis) ->
                         let (env',v',vis')  = genId v tgraph env vis
                         in (env',(v',c):links,vis')) (env,[],vis) trans_links
      in (vi',(trans_links', action)) : loop vis' env'


-- | In the epsilon closure for each vertex is computed the set of
-- all vertices accessible using only epsilon annotated 
-- edges (with 'NullConstraint' annotation). For each such
-- set is generated a new vertex in the new graph. The @ECEnv@ is used 
-- to keep the correspondence. The set is represented as a sorted list
-- of vertices.
data ECEnv = ECEnv !Vertex !(Map [Vertex] Vertex)

-- | An empty 'ECEnv'
emptyECEnv = ECEnv 0 Map.empty

-- | The type is used internally to keep the pair of
--    * The vertex in the new graph
--    * The set from which this vertex has been generated
--    * The action which will be associated to this vertex
type VertexClosure = (Vertex,[Vertex],[TransducerAction])

-- | New vertexes generation
genId :: Vertex -> TransitionGraph -> ECEnv -> [VertexClosure] -> (ECEnv, Vertex, [VertexClosure])
genId v tgraph env@(ECEnv id new_states) vis =
  case Map.lookup closure new_states of
    Just v' -> (env,                                             v',                     vis)
    Nothing -> (ECEnv (id+1) (Map.insert closure id new_states), id, (id,closure,action):vis)
  where
    (closure,action) = getClosure [v] [] []

    getClosure []     closure actions           = (closure, actions)
    getClosure (v:vs) closure actions
      | Just closure' <- insertSorted v closure = getClosure (epsilon_links++vs) closure' (mergeActions actions1 actions)
      | otherwise                               = getClosure                 vs  closure               actions
      where
        (links,actions1) = tgraph ! v
        epsilon_links = [v | (v,c) <- links, isNullConstraint c]
            
    	insertSorted :: Vertex -> [Vertex] -> Maybe [Vertex]
    	insertSorted n [] = Just [n]
    	insertSorted n all@(n1:ns)
    	  | n == n1   = Nothing
    	  | n >  n1   = case insertSorted n ns of
    			  Just ns1 -> Just (n1:ns1)
    			  Nothing  -> Nothing
    	  | otherwise = Just (n : all)


isNullConstraint NullConstraint = True
isNullConstraint _              = False
