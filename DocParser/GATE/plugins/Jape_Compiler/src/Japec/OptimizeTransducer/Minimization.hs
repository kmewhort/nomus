{-# OPTIONS -fglasgow-exts #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Japec.OptimizeTransducer.Minimization
-- Copyright   : (C) Ontotext 2005
-- License     : LGPL
--
-- Maintainer  : Krasimir Angelov <kr.angelov@sirma.bg>
-- Stability   : beta
-- Portability : portable
--
-- The module is an implementation of the automata minimization.
--
-----------------------------------------------------------------------------

module Japec.OptimizeTransducer.Minimization(minimization) where

import Control.Monad(foldM)
import Control.Monad.ST
import Data.Map as Map hiding ((!),map)
import Data.Array
import Data.Array.ST hiding (bounds)
import StringUtils.KeyString as KS
import Japec.TSyntax

-- | The entry point to the determinisation.
minimization :: TransitionGraph -> TransitionGraph
minimization tgraph = runST mkMinimalGraph
  where
    (start,end) = bounds tgraph
    
    mkMinimalGraph :: forall s . ST s TransitionGraph
    mkMinimalGraph = do
       (mapping :: STArray s Vertex Vertex) <- newArray_ (start,end)
       classes <- mkInitClasses mapping 0 [] start Map.empty
       classes <- iterate mapping classes
       lst <- mapM (foldM (construct mapping) ([],[])) classes
       return (listArray (0,length lst-1) lst)
       where
         iterate mapping classes = do
           tables <- mapM (clasifyVertices mapping []) classes
           new_classes <- splitVertices mapping 0 [] tables
           if length new_classes == length classes
             then return classes
             else iterate mapping new_classes
         
         construct mapping (links',actions') v = do
	    links <- foldM (toClassLink mapping) links' links
	    return (links, mergeActions actions' actions)
	    where
	      (links,actions) = tgraph ! v

    mkInitClasses mapping classId classes v actionEnv
      | v > end   = return (reverse classes)
      | otherwise =
          case Map.lookup action_names actionEnv of
            Nothing     -> do writeArray mapping v classId
	      	              mkInitClasses mapping (classId+1) ([v]:classes) (v+1) (Map.insert action_names classId actionEnv)
            Just vclass -> do writeArray mapping v vclass
	      	              mkInitClasses mapping classId (addVertexAt (classId-vclass-1) v classes) (v+1) actionEnv
	  where
	    (links,actions) = tgraph ! v
	    action_names    = map getActionName actions

            addVertexAt 0      v (vs:classes) = (v:vs) : classes
            addVertexAt vclass v (vs:classes) =    vs  : addVertexAt (vclass-1) v classes

    splitVertices mapping classId new_classes [] = return (reverse new_classes)
    splitVertices mapping classId new_classes (vs:classes) = do
      splitAll classId new_classes vs
      where
        splitAll classId new_classes []                      = splitVertices mapping classId new_classes classes
        splitAll classId new_classes ((v,class_links):table) = do
          writeArray mapping v classId
          (table',vs) <- splitByVertex v classId class_links [v] [] table
          splitAll (classId+1) (vs:new_classes) table'
          where
            splitByVertex v vclass class_links vs new_table [] = return (new_table,vs)
            splitByVertex v vclass class_links vs new_table (r@(v1,class_links1):table)
              | compareClassLinks class_links class_links1 = do
                  writeArray mapping v1 vclass
                  splitByVertex v vclass class_links (v1:vs) new_table table
              | otherwise = do
                  splitByVertex v vclass class_links vs (r:new_table) table

    clasifyVertices mapping table []     = return table
    clasifyVertices mapping table (v:vs) = do
      class_links <- foldM (toClassLink mapping) [] links
      clasifyVertices mapping ((v,class_links):table) vs
      where
        (links,action) = tgraph ! v

    toClassLink mapping class_links (v,constr) = do
      vclass <- readArray mapping v
      return (insertSorted vclass constr class_links)
      where
        insertSorted vclass c [] = [(vclass,c)]
        insertSorted vclass c all@(vc1@(vclass1,c1):vcs) =
	  case mergeEdges c c1 of
	    (EQ,c) -> (vclass,c) : vcs
	    (GT,c) -> vc1        : insertSorted vclass c vcs
	    (LT,c) -> (vclass,c) : all

    compareClassLinks []                     []                     = True
    compareClassLinks ((v1,c1):class_links1) ((v2,c2):class_links2)
      | v1 == v2  = case mergeEdges c1 c2 of
                      (EQ,_) -> compareClassLinks class_links1 class_links2
                      _      -> False
    compareClassLinks _                      _                      = False
