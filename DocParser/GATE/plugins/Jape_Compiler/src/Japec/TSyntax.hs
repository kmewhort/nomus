-----------------------------------------------------------------------------
-- |
-- Module      : Japec.TSyntax
-- Copyright   : (C) Ontotext 2005
-- License     : LGPL
--
-- Maintainer  : Krasimir Angelov <kr.angelov@sirma.bg>
-- Stability   : beta
-- Portability : portable
--
-- The module contains the representation of the intermediate JAPE syntax or 
-- the @Transducer@ syntax. 
--
-- In the transducer syntax the list of rules is converted to a graph that 
-- represents the structure of FSM like automata. Each edge in the graph is
-- annotated with either annotation type or feature contraint. So if in rule
-- there is a basic pattern which has both annotation type and feature contraint
-- then it is converted to multiple vertices connected with edges.
--
-- If there is a multiphase transducer which has in its phases another multiphase 
-- transducer then the phases of the internal transducer are expanded. 
-- In this way we can have multi-phase trasducer only at the top level and it can 
-- contain only single-phase sub transducers. For this reason in the TSyntax we 
-- have separated types for single and multi phase transducers.
--
-- All macro references in rule LHS are expanded too.
--
-----------------------------------------------------------------------------

module Japec.TSyntax
         ( Transducer
         , TransducerPhase(..)
         , Priority, FileLine
         , TransducerAction(..)
         , JapeValue(..)
         , JapeAction(..)
         , JapeControlType(..)
         , Vertex, TransitionGraph, ComparisonType(..), EdgeConstraint(..), Binding(..)
         , mergeEdges, mergeActions
         , getActionName, getPhaseName
         , IncomingEdges, getIncomingEdges
         , ppTransitionGraph
         ) where

import Japec.AbsSyntax
import StringUtils.KeyString as KS
import Text.PrettyPrint
import Control.Monad
import Control.Monad.ST
import Data.Array
import Data.Array.ST
import qualified Data.IntSet as ISet
import Data.List(nub)

-- | General transducer type. It is 'Either' single or multi phase transducer.
type Transducer = [TransducerPhase]

-- | Single-phase transducer. It is represented as FSM like automata but in addition it has
-- some other properties derived from the 'JapeGrammar'
data TransducerPhase
  = TransducerPhase String [KS.KeyString] JapeControlType Bool Bool TransitionGraph

-- | All final states in the transducer are annotated with a list of actions that should be 
-- executed at this state. The @TransducerAction@ also keeps the rule name and the priority.
data TransducerAction 
  = TransducerAction KS.KeyString Priority FileLine [JapeAction]

-- | The @Vertex@ represents a vertex in the transition graph and\/or the FSM state.
type Vertex = Int

-- | The @TransitionGraph@ graph represents the structure of FSM like automata.
-- The transition from one state to another happens only when the current annotation
-- satisfies the 'EdgeConstraint' associated the linking edge. If given state is
-- intermediate then the associated 'TransducerAction' is 'NoAction'.
type TransitionGraph = Array Vertex ([(Vertex,EdgeConstraint)], [TransducerAction])

type IncomingEdges = Array Vertex [Vertex]

-- | In the 'JapeConstraint' there are positive and negative constraints.
-- In the TSyntax this is reflected with the @ComparisonType@ field in the 'EdgeConstraint'.
data ComparisonType
  = Equal | NotEqual
  deriving (Eq,Ord)

-- | Each edge in the transition graph is annotated with 'EdgeConstraint'.
-- It represents the constraint which the current annotation should satisfy
-- in order to move in this direction.
data EdgeConstraint
  = TypeConstraint    [Binding] ComparisonType KS.KeyString
  | FeatureConstraint [Binding] ComparisonType KS.KeyString JapeValue
  | NullConstraint

data Binding 
  = Binding KS.KeyString KS.KeyString
  deriving Eq

mergeEdges :: EdgeConstraint -> EdgeConstraint -> (Ordering, EdgeConstraint)
mergeEdges c@(NullConstraint                  ) (NullConstraint                  ) = (EQ, c)
mergeEdges c@(NullConstraint                  ) (TypeConstraint    _   _   _     ) = (LT, c)
mergeEdges c@(NullConstraint                  ) (FeatureConstraint _   _   _   _ ) = (LT, c)
mergeEdges c@(TypeConstraint    _   _   _     ) (NullConstraint                  ) = (GT, c)
mergeEdges c@(TypeConstraint    bs1 op1 name1 ) (TypeConstraint    bs2 op2 name2 ) = 
  case compare op1 op2 of
    EQ -> case compare name1 name2 of
            EQ -> (EQ, TypeConstraint (nub (bs1++bs2)) op1 name1)
            x  -> (x,  c)
    x  -> (x, c)
mergeEdges c@(TypeConstraint    _   _     _   ) (FeatureConstraint _   _     _ _ ) = (LT, c)
mergeEdges c@(FeatureConstraint _   _     _ _ ) (NullConstraint                  ) = (GT, c)
mergeEdges c@(FeatureConstraint _   _     _ _ ) (TypeConstraint    _   _     _   ) = (GT, c)
mergeEdges c@(FeatureConstraint bs1 op1 name1 val1) (FeatureConstraint bs2 op2 name2 val2) =
  case compare op1 op2 of
    EQ -> case compare name1 name2 of
            EQ -> case compare val1 val2 of
                    EQ -> (EQ, FeatureConstraint (nub (bs1++bs2)) op1 name1 val1)
                    x  -> (x,  c)
            x  -> (x, c)
    x  -> (x, c)

mergeActions :: [TransducerAction] -> [TransducerAction] -> [TransducerAction]
mergeActions actions1 actions2 = 
  foldr insertSortedAction actions2 actions1
  where
    insertSortedAction action1 []                = [action1]
    insertSortedAction action1 all@(action2:actions)
      | name1 == name2 = all
      | name1 >  name2 = action2 : insertSortedAction action1 actions
      | otherwise      = action1 : all
      where
    	name1 = getActionName action1
    	name2 = getActionName action2

getActionName :: TransducerAction -> KeyString
getActionName (TransducerAction name _ _ _) = name

getPhaseName :: TransducerPhase -> String
getPhaseName (TransducerPhase name _ _ _ _ _) = name

getIncomingEdges :: TransitionGraph -> IncomingEdges
getIncomingEdges tgraph = runSTArray (check tgraph)
  where
    check tgraph = do
      let graph_bounds@(start,_) = Data.Array.bounds tgraph
      inArr <- newArray graph_bounds []
      compute start ISet.empty inArr
      return inArr
      where
        compute :: Vertex -> ISet.IntSet -> STArray s Vertex [Vertex] -> ST s ISet.IntSet
        compute v processed inEdges
          | v `ISet.member` processed
                      = return processed
          | otherwise = foldM (\processed (v',_) -> do
                                   vs <- readArray inEdges v'
                                   writeArray inEdges v' (v:vs)
                                   compute v' (ISet.insert v processed) inEdges) processed links
          where
            (links,_) = tgraph ! v

-- | The @ppTransitionGraph@ function is used to dump the transition graph
-- in GraphViz format (See "http:\/\/www.graphviz.org").
ppTransitionGraph :: TransducerPhase -> Doc
ppTransitionGraph (TransducerPhase name input control debug match tgraph) =
  text "digraph" <+> text name $$
  braces (vcat (map ppVertex (assocs tgraph)))
  where
    incomingEdges = getIncomingEdges tgraph

    ppVertex (v,(links,actions)) =
      (if incomingCount > 1 || not (null actions)
         then int v <+> brackets (
                 (if incomingCount > 1
                    then text "fillcolor =" <+> doubleQuotes(text "bisque3") <+> text "style=\"filled\""
                    else empty) <+>
                 (if not (null actions)
		    then text "label" <+> equals <+> doubleQuotes(int v <> colon <> hsep (map ppAction actions)) <+> 
                         text "shape=\"diamond\""
                    else empty))
         else empty) $$
      vcat (map (ppLink v) links)
      where
        incomingCount = length (incomingEdges ! v)

    ppLink v1 (v2,constr) =
      let attributes = text "label" <+> char '=' <+> doubleQuotes(ppConstr constr)
      in int v1 <+> text "->" <+> int v2 <+> brackets attributes
      
    ppAction = text . unpack . getActionName

    ppConstr (NullConstraint                 )  = empty
    ppConstr (TypeConstraint    _ Equal    tp)  =             text (unpack tp)
    ppConstr (TypeConstraint    _ NotEqual tp)  = char '!' <> text (unpack tp)
    ppConstr (FeatureConstraint _ Equal    f v) = text (unpack f) <+> char '='  <+> ppValue v
    ppConstr (FeatureConstraint _ NotEqual f v) = text (unpack f) <+> text "!=" <+> ppValue v

    ppValue (IndentifierValue name) = text (unpack name)
    ppValue (IntegerValue val) = int val
    ppValue (StringValue val)  = text (init (tail (show val)))
