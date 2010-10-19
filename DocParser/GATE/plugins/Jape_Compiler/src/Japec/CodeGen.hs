-----------------------------------------------------------------------------
-- |
-- Module      : Japec.CodeGen
-- Copyright   : (C) Ontotext 2005
-- License     : LGPL
--
-- Maintainer  : Krasimir Angelov <kr.angelov@sirma.bg>
-- Stability   : beta
-- Portability : portable
--
-----------------------------------------------------------------------------

module Japec.CodeGen
         ( Statement(..), SwitchCase(..), StateCase(..), JapeValue(..)
         , codeGen,  ppStatement, ppStateSwitch
         ) where

import Japec.TSyntax
import StringUtils.KeyString as KS
import Data.Array
import Data.Map as Map hiding ((!), map)
import Data.List(mapAccumR)
import Text.PrettyPrint as Doc
import Debug.Trace

data Statement
  = AnnotationSwitch           [SwitchCase]
  | FeatureSwitch KS.KeyString [SwitchCase]
  | AnnotationNESwitch           JapeValue Statement
  | FeatureNESwitch KS.KeyString JapeValue Statement
  | Fire   KS.KeyString Priority FileLine [JapeAction]
  | PushState Vertex
  | StepOn    Vertex Statement
  | AddBinding KS.KeyString KS.KeyString
  | Empty
  | Seq Statement Statement

data SwitchCase
  = SwitchCase JapeValue Statement

data StateCase
  = StateCase Vertex Statement

codeGen :: TransducerPhase -> [StateCase]
codeGen (TransducerPhase name input control debug match tgraph) = generateAll start
  where
    incomingEdges = getIncomingEdges tgraph
    
    generateAll v
      | v > end      = []
      | v == start || length (incomingEdges ! v) > 1 
                     = StateCase v (generate v) : generateAll (v+1)
      | otherwise    = generateAll (v+1)

    (start, end) = bounds tgraph

    generate v = mkFireStmts actions statement
      where
        statement = foldr mkFeatureNESwitch
          (case abranches of
             []       -> foldr mkFeatureSwitch Empty features
             branches ->
	       let aswitch = AnnotationSwitch (map mkSwitchCase branches)
	           switch  = foldr mkAnnotationNESwitch aswitch eqnots
	       in foldr mkFeatureSwitch (StepOn v switch) features) eqnots

        mkSwitchCase (v,val,bindings) = (SwitchCase val (foldr mkAddBinding (generateNext v) bindings))
          where
            mkAddBinding (Binding ruleName binding) stmt = Seq (AddBinding ruleName binding) stmt

	mkFeatureSwitch (feature,branches) stmt =
	  Seq (FeatureSwitch feature (map mkSwitchCase branches)) stmt

	mkFeatureNESwitch (v,(Just name),val) stmt = Seq (FeatureNESwitch name val (generateNext v)) stmt
	mkFeatureNESwitch _                   stmt = stmt

	mkAnnotationNESwitch (v,Nothing,val) stmt = Seq (AnnotationNESwitch val (generateNext v)) stmt
	mkAnnotationNESwitch _               stmt = stmt

	generateNext v
	  | length (incomingEdges ! v) > 1 = PushState v
          | otherwise                      = generate v

	mkFireStmts []                                                     stmt = stmt
	mkFireStmts (TransducerAction name priority fileLine actions : as) stmt = 
	  Seq (Fire name priority fileLine actions) (mkFireStmts as stmt)

        (links,actions) = tgraph ! v

        (abranches,features,eqnots) = collect links [] Map.empty []

        collect []         annots features eqnots = (annots,Map.toList features,eqnots)
        collect ((v,c):vs) annots features eqnots = collect vs annots' features' eqnots'
          where
            (annots',features',eqnots') = case c of
               TypeConstraint    b Equal         tp  -> ((v,IndentifierValue tp,b):annots,features,eqnots)
               FeatureConstraint b Equal    name val -> (annots,Map.insertWith (++) name [(v,val,b)] features,eqnots)
               TypeConstraint    b NotEqual      tp  -> (annots,features,(v,Nothing,  IndentifierValue tp):eqnots)
               FeatureConstraint b NotEqual name val -> (annots,features,(v,Just name,                val):eqnots)

ppStateSwitch :: [StateCase] -> Doc
ppStateSwitch cases =
  text "stateSwitch" $$
  text "{" $$
  vcat (map ppStateCase cases) $$
  text "}"
  where
    ppStateCase (StateCase val stmt) =
      text "case" <+> int val <> colon $$
      nest 4 (ppStatement stmt)

ppStatement :: Statement -> Doc
ppStatement (AnnotationSwitch cases) =
  text "annotationSwitch" $$
  text "{" $$
  vcat (map ppSwitchCase cases) $$
  text "}"
ppStatement (FeatureSwitch name cases) =
  text "featureSwitch" <+> parens (text (KS.unpack name)) $$
  text "{" $$
  vcat (map ppSwitchCase cases) $$
  text "}"
ppStatement (AnnotationNESwitch val stmt) =
  text "if" <+> parens (char '!' <> ppValue val) $$
  text "{" $$
  nest 4 (ppStatement stmt) $$
  text "}"
ppStatement (FeatureNESwitch name val stmt) =
  text "if" <+> parens (text (KS.unpack name) <+> text "!=" <+> ppValue val) $$
  text "{" $$
  nest 4 (ppStatement stmt) $$
  text "}"
ppStatement (PushState label) =
  text "pushState" <+> int label
ppStatement (Fire name _ _ _) =
  text "fire" <+> text (KS.unpack name)
ppStatement Empty = Doc.empty
ppStatement (StepOn _ stmt) =
  text "stepOn"  $$
  text "{" $$
  nest 4 (ppStatement stmt) $$
  text "}"
ppStatement (AddBinding ruleName binding) =
  text "addBinding" <+> text (show ruleName) <+> text (show binding)
ppStatement (Seq stmt1 stmt2) =
  ppStatement stmt1 $$
  ppStatement stmt2

ppSwitchCase (SwitchCase val stmt) =
  text "case" <+> ppValue val <> colon $$
  nest 4 (ppStatement stmt)

ppValue (IndentifierValue val) = text (KS.unpack val)
ppValue (IntegerValue     val) = int val
ppValue (StringValue      val) = text val
