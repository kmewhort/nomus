-----------------------------------------------------------------------------
-- |
-- Module      : Japec.CodeGen.Java
-- Copyright   : (C) Ontotext 2005
-- License     : LGPL
--
-- Maintainer  : Krasimir Angelov <kr.angelov@sirma.bg>
-- Stability   : beta
-- Portability : portable
--
-----------------------------------------------------------------------------

module Japec.CodeGen.Java
         ( ppTransducerPhaseJavaCode
         ) where

import Japec.CodeGen
import Japec.TSyntax
import Japec.AbsSyntax
import Japec.AbsSyntax.Lexer(keyStringScope)
import Japec.CmdFlags
import StringUtils.KeyString as KS
import qualified StringUtils.PackedString as PS
import qualified StringUtils.KeyStringSet as KSS
import Text.PrettyPrint as Doc
import Data.Maybe(maybe)
import Data.Array

ppTransducerPhaseJavaCode :: CmdFlags -> TransducerPhase -> Doc
ppTransducerPhaseJavaCode flags t@(TransducerPhase name input control debug match tgraph) =
  (if null (package flags)
     then empty
     else text "package" <+> text (package flags) <> semi $$ space) $$
  text "import java.io.*;" $$
  text "import java.util.*;" $$
  text "import gate.*;" $$
  text "import gate.jape.*;" $$
  text "import gate.creole.ontology.*;" $$
  text "import gate.annotation.*;" $$
  text "import gate.util.*;" $$
  text "import com.ontotext.gate.japec.*;" $$
  space $$
  text "public class" <+> text name <+> text "extends com.ontotext.gate.japec.SinglePhaseTransducer implements JapeConstants" <+> char '{' $$  
  nest 4 (space $$
          text "public" <+> text name <> text "()" <+> char '{' $$
          nest 4 (text "super" <> parens(text "new String[]" <+> braces (cat (punctuate comma (map (text . show) input))) <> comma <+> 
                                         ppControl control <> comma <+> 
                                         ppBoolean debug <> comma <+>
                                         ppBoolean match) <> semi) $$
          char '}' $$
          vcat (map ppMatchingProcedure state_cases) $$
          space $$
          text "protected void runMatching(int offsetIndex) throws JapeException" <+> char '{' $$
          nest 4 (text "State stack = newInitState(offsetIndex);" $$
                  space $$
                  text "do {" $$
                  nest 4 (text "State state = stack;" $$
                          text "stack = stack.pop();" $$
                          space $$
                          text "switch (state.getCode())" $$
			  text "{" $$
			  vcat (map ppStateCase state_cases) $$
			  text "default:" $$
			  nest 4 (text "throw new JapeException(\"Invalid current state code\");") $$
			  text "}" $$
                          text "state.close();") $$
                  text "} while (stack != null);") $$
          char '}' $$
          actionsDoc KSS.empty start) $$
  char '}'  
  where
    state_cases = codeGen t

    (start,end) = bounds tgraph

    actionsDoc processed v
      | v > end  = empty
      | otherwise = 
          let (processed',doc) = foldr actionDoc (processed,empty) actions
          in doc $$ actionsDoc processed' (v+1)
      where
        (links,actions) = tgraph ! v
        
        actionDoc (TransducerAction name priority fileLine actions) (processed,doc)
	  | not (KSS.member name processed) = (KSS.insert name processed, ppActionDoc name priority fileLine actions $$ doc)
          | otherwise                       = (processed,doc)

    ppActionDoc name priority fileLine jape_actions =
      space $$
      text "class" <+> text (KS.unpack name++"RuleAction") <+> text "extends com.ontotext.gate.japec.RuleAction" <+> char '{' $$
      nest 4 (space $$
              text "public" <+> text (KS.unpack name++"RuleAction") <> text "(com.ontotext.gate.japec.SinglePhaseTransducer.State state)" <+> char '{' $$
              nest 4 (text "super" <> parens(text (show name) <> comma <+> int priority <> comma <+> int fileLine <> comma <+> text "state") <> semi) $$
              char '}' $$
              space $$
              text "public void doit" <> parens (text "Document doc," $$
                                                 text "AnnotationSet annotations, " $$
                                                 text "AnnotationSet inputAS, AnnotationSet outputAS, " $$
                                                 text "Ontology ontology") <+> char '{' $$
              nest 4 (ppJapeActions jape_actions KSS.empty) $$
              char '}') $$
      char '}'

ppStateCase (StateCase val stmt) =
  text "case" <+> int val <> text ": stack = match" <> int val <> text "(state, stack); break;"

ppMatchingProcedure (StateCase val stmt) =
  space $$
  text "private State match" <> int val <> text "(State state, State stack)" <+> char '{' $$
  nest 4 (text "Annotation annotation = state.currentAnnotation();" $$
          text "String annType = (annotation == null) ? null : annotation.getType();" $$
          text "Object feature;" $$
          space $$
          ppJavaStatement (text "state") stmt $$
          space $$
          text "return stack;") $$
  char '}'

ppJavaStatement state (AnnotationSwitch cases) =
  vcat (punctuate (text "else") (map ppCase cases))
  where
    ppCase (SwitchCase val stmt) =
      text "if" <+> parens (text "annType.equals" <> parens (ppValue val)) <+> char '{' $$
      nest 4 (ppJavaStatement state stmt) $$
      char '}'
ppJavaStatement state (FeatureSwitch name cases) =
  text "feature =" <+> state <> text ".currentAnnotation().getFeatures().get" <> parens (text (show name)) <> semi $$
  text "if (feature != null) {" $$
  nest 4 (vcat (punctuate (text "else") (map ppCase cases))) $$
  char '}'
  where
    ppCase | name == classFeatureName = ppIsSubClass
           | otherwise                = ppEquals
           
    ppEquals (SwitchCase val stmt) =
      text "if" <+> parens (text "feature.equals" <> parens (ppValue val)) <+> char '{' $$
      nest 4 (ppJavaStatement state stmt) $$
      char '}'
    ppIsSubClass (SwitchCase val stmt) =
      text "if (ontology.isSubClassOf" <> parens (ppValue val <> comma <+> text "(String) feature") <> text ") {" $$
      nest 4 (ppJavaStatement state stmt) $$
      char '}'
ppJavaStatement state (AnnotationNESwitch val stmt) =
  text "if" <+> parens (text "!annType.equal" <> parens (ppValue val)) <+> char '{' $$
  nest 4 (ppJavaStatement state stmt) $$
  char '}'
ppJavaStatement state (FeatureNESwitch name val stmt) =
  text "feature = annotation.getFeatures().get" <> parens (text (show name)) <> semi $$
  text "if" <+> parens (text "feature == null ||" <+> (if name == classFeatureName
                                                         then text "!ontology.isSubClassOf((String) feature," <> ppValue val
                                                         else text "!feature.equals" <> parens (ppValue val))) <+> char '{' $$
  nest 4 (ppJavaStatement state stmt) $$
  char '}'
ppJavaStatement state (PushState label) =
  text "stack =" <+> state <> text ".pushCode" <> parens(text "stack" <> comma <+> int label) <> semi
ppJavaStatement state (Fire name _ _ _) =
  text "if (addMatchingRule" <> parens (text "new" <+> text (KS.unpack name++"RuleAction") <> parens state) <> text ") return null;"
ppJavaStatement state Empty = empty
ppJavaStatement state (StepOn v stmt) =
  text "State" <+> state' <+> equals <+> state <> text ".getNext();" $$
  text "while" <+> parens (state' <> text ".hasNextAnnotation()") <+> char '{' $$
  nest 4 (text "annotation" <+> equals <+> state' <> text ".nextAnnotation" <> parens state <> semi $$
          text "annType = annotation.getType();" $$
          ppJavaStatement state' stmt) $$
  char '}' $$
  state' <> text ".close();"
  where
    state' = text "state" <> int v
ppJavaStatement state (AddBinding ruleName binding) =
  state <> text ".addBinding" <> parens(text (show ruleName) <> comma <+> text (show binding)) <> semi
ppJavaStatement state (Seq stmt1 stmt2) =
  ppJavaStatement state stmt1 $$
  ppJavaStatement state stmt2

ppValue (IndentifierValue val) = text (show val)
ppValue (IntegerValue     val) = doubleQuotes (int val)
ppValue (StringValue      val) = text (show val)
ppValue (BooleanValue     val) = text (if val then "true" else "false")
ppValue (FloatValue       val) = text (show val)

ppAssigment (JapeAssigment name value) =
  text "features.put" <> parens
  (text (show name) <> comma <+>
   case value of
     IndentifierValue val -> text (show val)
     IntegerValue     val -> text "new Long" <> parens (int val)
     StringValue      val -> text (show val)
     BooleanValue     val -> text (if val then "Boolean.TRUE" else "Boolean.FALSE")
     FloatValue       val -> text "new Double" <> parens (double val)) <> semi
ppAssigment (JapeRefAssigment name binding annType feature) =
  text "{ // need a block for the existing annot set" $$
  text "  AnnotationSet" <+> existingAnnotSetName <+> text "= (AnnotationSet) bindings.get" <> parens (text (show binding)) <> semi $$
  text "  if" <> parens (existingAnnotSetName <+> text "!= null") <+> char '{' $$
  text "    AnnotationSet existingAnnots =" <+> existingAnnotSetName <> text ".get" <> parens (text (show annType)) <> semi $$
  text "    if(existingAnnots != null) {" $$
  text "      Iterator iter = existingAnnots.iterator();" $$
  text "      while (iter.hasNext()) {" $$
  text "        Annotation existingA = (Annotation) iter.next();" $$
  text "        Object existingFeatureValue = existingA.getFeatures().get" <> parens(text (show feature)) <> semi $$
  text "        if (existingFeatureValue != null) {" $$
  text "          features.put" <> parens(text (show name) <> text ", existingFeatureValue") <> semi $$
  text "          break;" $$
  text "        }" $$
  text "      } // while" $$
  text "    } // if existingAnnots != null" $$
  text "  } // if" <+> existingAnnotSetName <+> text "!= null" $$
  text "} // block for existing annots"
  where
    existingAnnotSetName = text (KS.unpack binding ++ "ExistingAnnots")

ppJapeActions []                                     bindings = empty
ppJapeActions (JavaAction mb_name content : actions) bindings =
  let (bindings',doc) = 
        case mb_name of
          Nothing   -> (bindings,
              char '{' $$
              nest 4 (text (PS.unpack content)) $$
              char '}')
          Just name -> ppBindingsGet name bindings (text (PS.unpack content))
  in doc $$ ppJapeActions actions bindings'
ppJapeActions (AssigmentAction name annType assigments : actions) bindings =
  let (bindings',doc) = 
        ppBindingsGet name bindings
          (text "// RHS assignment block" $$
           text "FeatureMap features = Factory.newFeatureMap();" $$
           vcat (map ppAssigment assigments) $$
           text "annotations.add" <> parens (annotSetName <> text ".firstNode()" <> comma $$
                                             annotSetName <> text ".lastNode()"  <> comma $$
                                             text (show annType) <> comma <+> text "features") <> semi $$
           text "// end of RHS assignment block")
  in doc $$ ppJapeActions actions bindings'
  where
    annotSetName = text (KS.unpack name ++ "Annots")
ppJapeActions (MacroRefAction name : actions) bindings =
  text "// MACRO:" <+> text (KS.unpack name) $$
  ppJapeActions actions bindings

ppControl :: JapeControlType -> Doc
ppControl Appelt = text "APPELT_STYLE"
ppControl First  = text "FIRST_STYLE"
ppControl Brill  = text "BRILL_STYLE"
ppControl Once   = text "ONCE_STYLE"
ppControl All    = text "ALL_STYLE"

ppBoolean :: Bool -> Doc
ppBoolean True  = text "true"
ppBoolean False = text "false"

ppBindingsGet name bindings doc = (bindings',doc')
  where
    (getter,bindings')
       | name `KSS.member` bindings 
                   = ( empty
                     , bindings
                     )
       | otherwise = ( text "AnnotationSet" <+> annotSetName <+> text "= (AnnotationSet) bindings.get" <> parens(text (show name)) <> semi
                     , KSS.insert name bindings
                     )
    doc' =
      getter $$ 
      text "if" <> parens (annotSetName <+> text "!=" <+> text "null" <+> text "&&" <+> annotSetName <> text ".size()" <+> text "!= 0") <+> char '{' $$
      nest 4 doc $$
      char '}'

    annotSetName = text (KS.unpack name ++ "Annots")

classFeatureName = KS.pack keyStringScope "class"
