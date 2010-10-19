-----------------------------------------------------------------------------
-- |
-- Module      : Japec.TranslateGrammar
-- Copyright   : (C) Ontotext 2005
-- License     : LGPL
--
-- Maintainer  : Krasimir Angelov <kr.angelov@sirma.bg>
-- Stability   : beta
-- Portability : portable
--
-- In this module is implemented the AbsSyntax -> TSyntax translation.
-- The tasks that are performed here are:
-- 
--    * Macro reference elimination
--
--    * Multi-phase transducer expansion. If there is a multi-phase
-- transducer that is embeded in another multi-phase transducer then it 
-- is removed and its phases are added to the upper level.
--
--    * 'JapeRule's to 'TransitionGraph' translation. All rules in a 
-- single-phase transducer are merged in a common transition graph that 
-- represents FSM like automata.
--
-----------------------------------------------------------------------------

module Japec.TranslateGrammar(translateGrammar, MissingMacros) where

import StringUtils.KeyString as KS
import StringUtils.KeyStringMap as KSM
import StringUtils.KeyStringSet as KSS
import Data.List as LST
import Data.Array.ST
import Data.Maybe(maybe)
import Japec.AbsSyntax
import Japec.TSyntax

-- | The 'MacrosEnv' is used internally to collect the
-- set of all defined macros and their content.
type MacrosEnv = KSM.KeyStringMap (Either [[JapePattern]] JapeAction)

-- | The translator collects all missing macroses
-- in list of macro name and rule name pairs.
-- The rule name is the name of the rule where the
-- macro reference has been found.
type MissingMacros = [(KS.KeyString,KS.KeyString)]

-- | 'translateGrammar' performs the main translations.
-- If there are missing macroses then the 'MissingMacros'
-- list contains their names. If the list is empty then
-- the translation has been performed successfully.
translateGrammar :: JapeGrammar -> (MissingMacros, Transducer)
translateGrammar grammar =
  let (_, missing, grammar') = translateGrammar KSM.empty [] grammar
  in (missing, grammar')
  where
    translateGrammar :: MacrosEnv -> MissingMacros -> JapeGrammar -> (MacrosEnv, MissingMacros, Transducer)
    translateGrammar menv missing (MultiPhase name phases) = translateGrammars menv missing phases
      where
        translateGrammars menv missing []             = (menv, missing, [])
        translateGrammars menv missing (phase:phases) =
          let (menv', missing', phases1) = translateGrammar  menv  missing  phase
              (menv'',missing'',phases2) = translateGrammars menv' missing' phases
          in (menv'',missing'',phases1 ++ phases2)
    translateGrammar menv missing (SinglePhase name input ctrl debug match rules) = 
      let (graph, (menv', missing')) = {-# SCC "my_scc" #-}
               runGraphBuilder $ do
                 initState <- getNextId
                 translateRules menv missing initState rules
      in (menv', missing', [TransducerPhase name input ctrl debug match graph])
      where
        translateRules menv missing initState []                                = return (menv, missing)
        translateRules menv missing initState (JapePatternMacro name patterns : rules) =
          case expandConstraintGroup menv patterns of
            Right patterns -> translateRules (KSM.insert name (Left patterns) menv) missing initState rules
            Left  macro    -> translateRules menv ((name,macro):missing) initState rules
        translateRules menv missing initState (JapeActionMacro name action : rules) =
	  translateRules (KSM.insert name (Right action) menv) missing initState rules
        translateRules menv missing initState (JapeRule name priority fileLine cgroups actions : rules) =
          case expandConstraintGroup menv cgroups of
            Right cgroups -> do
              insulator <- getNextId
              addTransition initState insulator NullConstraint
              let (missing',actions') = traslateActions menv missing actions
              finalState <- getFinalId (TransducerAction name priority fileLine actions')
	      buildCGroup insulator finalState cgroups name []
              translateRules menv missing' initState rules
            Left  macro    -> translateRules menv ((name,macro):missing) initState rules
          where
            traslateActions menv missing []                              = (missing, [])
            traslateActions menv missing (MacroRefAction macro : actions) =
              case KSM.lookup macro menv of
                Just (Right action') -> (missing',             action':actions')
                _                    -> ((name,macro):missing',        actions')
              where
                (missing',actions') = traslateActions menv missing actions
            traslateActions menv missing (action:actions) =
              let (missing',actions') = traslateActions menv missing actions
              in (missing',action:actions')

        expandConstraintGroup mset []             = Right []
        expandConstraintGroup mset (group:groups) =
          case expandPatterns mset group of
            Right group -> case expandConstraintGroup mset groups of
                             Right groups -> Right (group:groups)
                             Left  macro  -> Left macro
            Left  macro -> Left macro

        expandPatterns mset []                 = Right []
        expandPatterns mset (pattern:patterns) = 
            case expandPattern mset pattern of
              Right pattern -> case expandPatterns mset patterns of
                                 Right patterns -> Right (pattern:patterns)
                                 Left  macro    -> Left macro
              Left macro    -> Left macro
            
        expandPattern mset (MacroReference name) =
          case KSM.lookup name mset of
            Just (Left value) -> Right (ComplexPattern value KleeneNoop Nothing)
            _                 -> Left  name
        expandPattern mset (ComplexPattern value op binding) = 
          case expandConstraintGroup mset value of
            Right value -> Right (ComplexPattern value op binding)
            Left macro  -> Left macro
        expandPattern _ pattern = Right pattern


---------------------------------------------------------------------------------------
-- GraphBuilder monad
---------------------------------------------------------------------------------------

type Edge = (Vertex,Either (Vertex,EdgeConstraint) TransducerAction)


-- | The @GraphBuilder@ monad is used to keep the intermediate
-- structures required to build the 'TrasitionGraph'.
-- The required structures are the list of all generated 
-- edges and the value of the last 'Vertex' generated.
-- The later is used to build fresh 'Vertex' values.
newtype GraphBuilder a = GraphBuilder (Vertex -> [Edge] -> (Vertex, [Edge], a))

instance Monad GraphBuilder where
  return x = GraphBuilder (\id trans -> (id, trans, x))
  (GraphBuilder f) >>= g = GraphBuilder (\id trans -> 
     case f id trans of
       (id, trans, x) -> case g x of
                           GraphBuilder g -> g id trans)

-- | @runGraphBuilder@ executes the given 'GraphBuilder' action
-- and returns the generated 'TransitionGraph'.
runGraphBuilder :: GraphBuilder r -> (TransitionGraph, r)
runGraphBuilder (GraphBuilder builder) =
  case builder 0 [] of
    (id, edges, r) -> (runSTArray (do
                         arr <- newArray (0,id-1) ([],[])
                         collect arr edges
                         return arr), r)
  where
    collect arr []  = return ()
    collect arr ((v,Left link):edges) = do
      (links,actions) <- readArray arr v
      writeArray arr v (link:links,actions)
      collect arr edges
    collect arr ((v,Right action):edges) = do
      (links,actions) <- readArray arr v
      writeArray arr v (links,action:actions)
      collect arr edges

-------------------------------------------------
-- Some helpfull 'GraphBuilder' specific actions

getNextId :: GraphBuilder Int
getNextId = GraphBuilder (\id trans -> let next_id = id+1
                                       in next_id `seq` (next_id,trans,id))

getFinalId :: TransducerAction -> GraphBuilder Int
getFinalId action = GraphBuilder (\id trans -> let next_id = id+1
                                               in next_id `seq` (next_id,(id,Right action):trans,id))

addTransition :: Vertex -> Vertex -> EdgeConstraint -> GraphBuilder ()
addTransition v1 v2 x = GraphBuilder (\id trans -> (id,(v1,Left (v2,x)):trans,()))



---------------------------------------------------------------------------------------
-- TransitionGraph building
---------------------------------------------------------------------------------------

buildCGroup initState finalState []             ruleName bindings = return ()
buildCGroup initState finalState (group:groups) ruleName bindings = do
  state <- buildPatterns initState finalState group ruleName bindings
  buildCGroup initState finalState groups ruleName bindings

buildPatterns initState finalState []        ruleName bindings = return ()
buildPatterns initState finalState [pattern] ruleName bindings = do
  buildPattern initState finalState pattern ruleName bindings
buildPatterns initState finalState (pattern:patterns) ruleName bindings = do
  nextState <- getNextId
  buildPattern initState nextState pattern ruleName bindings
  buildPatterns nextState finalState patterns ruleName bindings

buildPattern initState finalState (BasicPattern constraints) ruleName bindings = do
  buildConstraints initState finalState constraints ruleName bindings
  return ()
buildPattern initState finalState (ComplexPattern cgroups op binding) ruleName bindings = do
  initInsulator  <- getNextId
  addTransition initState initInsulator NullConstraint
  finalInsulator <- getNextId
  addTransition finalInsulator finalState NullConstraint
  buildCGroup initInsulator finalInsulator cgroups ruleName (maybe bindings (\binding -> (Binding ruleName binding):bindings) binding)
  case op of
    KleeneQuery  -> addTransition initInsulator finalInsulator NullConstraint
    KleeneStar   -> do addTransition initInsulator finalInsulator NullConstraint
                       addTransition finalInsulator initInsulator NullConstraint
    KleenePlus   -> addTransition finalInsulator initInsulator NullConstraint
    KleeneNoop   -> return ()
buildPattern initState finalState (MacroReference name) ruleName bindings = do
  error ("FSM builder can't accept grammars with macro references: (macro = "++KS.unpack name++")")

buildConstraints initState finalState []                                       ruleName bindings = return []
buildConstraints initState finalState constrs@(PositiveConstraint tp attr : _) ruleName bindings = do
  let (fvs,constrs') = collectFeatures tp [] [] constrs
  case fvs of
    []          -> addTransition initState finalState (TypeConstraint bindings Equal tp)
    ((f,v):fvs) -> do nextState <- getNextId
                      addTransition initState nextState (TypeConstraint [] Equal tp)
                      nextState <- addFeatureTransitions fvs nextState
                      addTransition nextState finalState (FeatureConstraint bindings Equal f v)
  buildConstraints initState finalState constrs' ruleName bindings
buildConstraints initState finalState (NegativeConstraint tp attr : constrs) ruleName bindings = do
  let (fvs,constrs') = collectFeatures tp [] [] constrs
  addTransition initState finalState (TypeConstraint bindings NotEqual tp)
  case fvs of
    [] -> addTransition initState finalState (TypeConstraint bindings Equal tp)
    _  -> do nextState <- getNextId
             addTransition initState finalState (TypeConstraint [] Equal tp)
             mapM_ (\(f,v) -> addTransition nextState finalState (FeatureConstraint bindings NotEqual f v)) fvs
  buildConstraints initState finalState constrs' ruleName bindings

collectFeatures tp fvs cs' []       = (fvs,cs')
collectFeatures tp fvs cs' (c : cs) =
  case c of
    PositiveConstraint tp1 attr | tp == tp1 -> 
      case attr of
        AttributeConstraint f v -> collectFeatures tp ((f,v):fvs) cs'  cs
        _                       -> collectFeatures tp        fvs  cs'  cs
    NegativeConstraint tp1 attr | tp == tp1 ->
      case attr of
        AttributeConstraint f v -> collectFeatures tp ((f,v):fvs) cs'  cs
        _                       -> collectFeatures tp        fvs  cs'  cs
    _                           -> collectFeatures tp fvs      (c:cs') cs

addFeatureTransitions []          state = return state
addFeatureTransitions ((f,v):fvs) state = do
  nextState <- getNextId
  addTransition state nextState (FeatureConstraint [] Equal f v)
  addFeatureTransitions fvs nextState
