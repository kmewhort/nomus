/*
* AnnotationsByOffset.java
* Copyright:    Copyright (c) 2005, Ontotext Lab.
* Company:      Ontotext Lab.
* Krasimir Angelov 12/2005 */

package com.ontotext.gate.japec;

import java.util.*;
import gate.*;
import gate.util.*;
import gate.creole.*;
import gate.annotation.*;
import gate.jape.*;

public abstract class SinglePhaseTransducer extends AbstractLanguageAnalyser implements JapeConstants {
  public SinglePhaseTransducer(String[] inputs, int ruleApplicationStyle, boolean debugMode, boolean matchGroupMode) {
    this.inputASName  = null;
    this.outputASName = null;
    this.ontology     = null;

    this.inputs               = inputs;
    this.ruleApplicationStyle = ruleApplicationStyle;
    this.debugMode            = debugMode;
    this.matchGroupMode       = matchGroupMode;
    this.annotationsByOffset  = null;
    this.annotationsByOffsetCount= 0;
    this.currentMatchingRules = null;
  }

  public void execute() throws ExecutionException {
    if (document == null)
      throw new ExecutionException("No document provided!");

    currentMatchingRules     = new ArrayList();
    annotationsByOffset      = new AnnotationsByOffset[256];
    annotationsByOffsetCount = 0;

    AnnotationSet inputAS;
    if (inputASName == null || inputASName.equals(""))
      inputAS = document.getAnnotations();
    else
      inputAS = document.getAnnotations(inputASName);

    AnnotationSet outputAS;
    if (outputASName == null || outputASName.equals(""))
      outputAS = document.getAnnotations();
    else
      outputAS = document.getAnnotations(outputASName);

    HashMapLong offsetsMap  = new HashMapLong();
    if (inputs == null || inputs.length == 0)
      addAnnotationsByOffset(offsetsMap, inputAS);
    else {
      for (int i = 0; i < inputs.length; i++) {
        AnnotationSet ofOneType = inputAS.get(inputs[i]);
        if (ofOneType != null) {
          addAnnotationsByOffset(offsetsMap, ofOneType);
        }
      }
    }
    offsetsMap = null;

    if (annotationsByOffsetCount == 0) {
      fireProcessFinished();
      return;
    }

    java.util.Arrays.sort(annotationsByOffset,0,annotationsByOffsetCount);

    try {
      int offsetIndex = -1;
      do {
        currentMatchingRules.clear();

        runMatching(offsetIndex);

        if (currentMatchingRules.isEmpty()) {
          //no rule to fire, advance to the next input offset
          offsetIndex = offsetIndex + 1;
        } else if (ruleApplicationStyle == BRILL_STYLE ||
                   ruleApplicationStyle == ALL_STYLE) {
            // fire the rules corresponding to all accepting FSM instances
            Iterator accRuleIter = currentMatchingRules.iterator();

            long lastOffset = -1;
            while (accRuleIter.hasNext()) {
              RuleAction currentRule = (RuleAction) accRuleIter.next();

              currentRule.transduce(document, inputAS, outputAS, ontology);

              //find the maximal next position
              long currentLastOffset = currentRule.lastOffset;
              if (currentLastOffset > lastOffset)
                lastOffset = currentLastOffset;
            }

            if (ruleApplicationStyle == ALL_STYLE) {
              //simply advance to next offset
              offsetIndex = offsetIndex + 1;
            }
            else
            {
              while (offsetIndex+1 < annotationsByOffsetCount &&
                     annotationsByOffset[offsetIndex+1].offset < lastOffset)
                offsetIndex++;
            }
        } else if(ruleApplicationStyle == APPELT_STYLE ||
                  ruleApplicationStyle == FIRST_STYLE ||
                  ruleApplicationStyle == ONCE_STYLE) {

            // AcceptingFSMInstances is an ordered structure:
            // just execute the longest (last) rule
            Collections.sort(currentMatchingRules, Collections.reverseOrder());
            Iterator accRuleIter = currentMatchingRules.iterator();
            RuleAction currentRule = (RuleAction) accRuleIter.next();

            if (debugMode){
                //see if we have any conflicts
                Iterator accIter = currentMatchingRules.iterator();
                RuleAction aRule;
                List conflicts = new ArrayList();
                while (accIter.hasNext()){
                  aRule = (RuleAction)accIter.next();
                  if(aRule.equals(currentRule)){
                    conflicts.add(aRule);
                  }else{
                    break;
                  }
                }

                if (conflicts.size() > 1) {
                  Out.prln("\nConflicts found during matching:" +
                           "\n================================");
                  accIter = conflicts.iterator();
                  int i = 0;
                  while(accIter.hasNext()){
                    Out.prln(i++ + ") " + accIter.next().toString());
                  }
                }
            }

            currentRule.transduce(document, inputAS, outputAS, ontology);

            //if in matchGroup mode check other possible patterns in this span
            if (matchGroupMode) {
                // ~bp:  check for other matching fsm instances with same length,
                // priority and rule index : if such execute them also.
                String currentAcceptorString = null;
                multiModeWhile: while(accRuleIter.hasNext()) {
                  RuleAction rivalRule = (RuleAction) accRuleIter.next();
                  //get rivals that match the same document segment
                  //makes use of the semantic difference between the compareTo and
                  //equals methods on FSMInstance
                  if (rivalRule.compareTo(currentRule)==0) {
                    // gets the rivals that are NOT COMPLETELY IDENTICAL with the
                    // current acceptor.
                    if (!rivalRule.equals(currentRule)) {
                      if (debugMode) {
                          /*depends on the debug option in the transducer */
                        if (currentAcceptorString == null) {
                          // first rival
                          currentAcceptorString = currentRule.toString();
                          Out.prln("~Jape Grammar Transducer : " +
                                   "\nConcurrent Patterns by length,priority and index (all transduced):");
                          Out.prln(currentAcceptorString);
                          Out.prln("bindings : " + currentRule.getBindings());
                          Out.prln("Rivals Follow: ");
                        }
                        Out.prln(rivalRule);
                        Out.prln("bindings : " + rivalRule.getBindings());
                      } // DEBUG

                      rivalRule.transduce(document, inputAS, outputAS, ontology);
                    }
                    else {
                      //if rival is not equal this means that there are no further
                      // equal rivals (since the list is sorted)
                      break multiModeWhile;
                    }
                  }
                } // while there are fsm instances
            } // matchGroupMode

            //if in ONCE mode stop after first match
            if (ruleApplicationStyle == ONCE_STYLE) {
              return;
            }

            //advance in AG
            while (offsetIndex+1 < annotationsByOffsetCount &&
                   annotationsByOffset[offsetIndex+1].offset < currentRule.lastOffset)
              offsetIndex++;
          } else
            throw new RuntimeException("Unknown rule application style!");
      }
      while (offsetIndex < annotationsByOffsetCount);
    }
    catch (JapeException ex) {
      throw new ExecutionException("JapeException was thrown in SinglePhaseTransducer: "+ex.getMessage());
    }
    finally
    {
      currentMatchingRules     = null;
      annotationsByOffset      = null;
      annotationsByOffsetCount = 0;
    }
  }

  private String inputASName;
  private String outputASName;
  protected gate.creole.ontology.Ontology ontology;

  public void setInputASName(String newInputASName) {
    inputASName = newInputASName;
  }

  public String getInputASName() {
    return inputASName;
  }

  public void setOutputASName(String newOutputASName) {
    outputASName = newOutputASName;
  }

  public String getOutputASName() {
    return outputASName;
  }

  public gate.creole.ontology.Ontology getOntology() {
    return ontology;
  }

  public void setOntology(gate.creole.ontology.Ontology ontology) {
    this.ontology = ontology;
  }

  private void addAnnotationsByOffset(HashMapLong offsetsMap, Set annotations) {
    Iterator annIter = annotations.iterator();
    while(annIter.hasNext()) {
      Annotation ann = (Annotation)annIter.next();

      long offset = ann.getStartNode().getOffset().longValue();
      if (offset == ann.getEndNode().getOffset().longValue())
        continue;

      List f = (ArrayList) offsetsMap.get(offset);
      if (f == null)
      {
        AnnotationsByOffset aoff = new AnnotationsByOffset(offset);
        f = aoff.annotations;
        offsetsMap.put(offset, f);

        if (annotationsByOffset.length == annotationsByOffsetCount)
        {
          AnnotationsByOffset[] temp = new AnnotationsByOffset[annotationsByOffset.length*2];
          System.arraycopy(annotationsByOffset, 0, temp, 0, annotationsByOffset.length);
          annotationsByOffset = temp;
        }

        annotationsByOffset[annotationsByOffsetCount++] = aoff;
      }
      f.add(ann);
    }
  }

  protected abstract void runMatching(int offsetIndex) throws JapeException;

  public boolean addMatchingRule(RuleAction action)
  {
    currentMatchingRules.add(action);
    return (ruleApplicationStyle == FIRST_STYLE);
  }

  private String inputs[];
  private int ruleApplicationStyle;
  private boolean debugMode;
  private boolean matchGroupMode;

  private AnnotationsByOffset annotationsByOffset[];
  private int annotationsByOffsetCount;

  private State statesPool = null;
  private Binding bindingsPool = null;

  ArrayList currentMatchingRules = new ArrayList();

  protected State newInitState(int offsetIndex)
  {
    State new_state = newState();
    new_state.offsetIndex = offsetIndex;
    return new_state;
  }

  private State newState()
  {
    State new_state;
    if (statesPool == null)
      new_state = new State();
    else
    {
      new_state  = statesPool;
      statesPool = new_state.next;
    }

    new_state.code            = 0;
    new_state.offsetIndex     = -1;
    new_state.annotationIndex = -1;
    new_state.bindings        = null;
    new_state.next            = null;

    return new_state;
  }

  protected class Binding
  {
    String ruleName;
    String label;
    Annotation annotation;
    int refCount;
    Binding next;
  }

  protected class State
  {
    int code;
    int offsetIndex;
    int annotationIndex;
    Binding bindings;
    State next;

    public State getNext()
    {
      State new_state = newState();
      new_state.code            = code;
      new_state.offsetIndex     = offsetIndex+1;
      new_state.annotationIndex = -1;
      new_state.bindings        = bindings;
      new_state.next            = null;

      if (new_state.bindings != null)
        new_state.bindings.refCount++;

      Annotation annotation = currentAnnotation();
      if (annotation != null)
      {
        long lastOffset = annotation.getEndNode().getOffset().longValue();
        while (new_state.offsetIndex < annotationsByOffsetCount &&
               annotationsByOffset[new_state.offsetIndex].offset < lastOffset)
          new_state.offsetIndex++;
      }

      return new_state;
    }

    public int getCode()
    {
      return code;
    }

    public State pushCode(State stack, int new_code)
    {
      State new_state = newState();
      new_state.code            = new_code;
      new_state.offsetIndex     = offsetIndex;
      new_state.annotationIndex = annotationIndex;
      new_state.bindings        = bindings;
      new_state.next            = stack;

      if (new_state.bindings != null)
        new_state.bindings.refCount++;

      return new_state;
    }

    public State pop()
    {
      return next;
    }

    public boolean hasNextAnnotation()
    {
      if (offsetIndex < 0 || offsetIndex >= annotationsByOffsetCount)
        return false;

      List annotations = annotationsByOffset[offsetIndex].annotations;
      return (annotationIndex+1 < annotations.size());
    }

    public Annotation nextAnnotation(State state)
    {
      if (offsetIndex < 0 || offsetIndex >= annotationsByOffsetCount)
        return null;

      List annotations = annotationsByOffset[offsetIndex].annotations;
      if (annotationIndex+1 >= annotations.size())
        return null;

      while (bindings != null)
      {
        bindings.refCount--;

        if (bindings.refCount > 0)
          break;

        Binding temp  = bindings.next;
        bindings.next = bindingsPool;
        bindingsPool  = bindings;

        bindings = temp;
      }
      bindings = null;

      annotationIndex++;
      bindings        = state.bindings;
      next            = null;

      if (bindings != null)
        bindings.refCount++;

      return (Annotation) annotations.get(annotationIndex);
    }

    public Annotation currentAnnotation()
    {
      if (offsetIndex < 0 || offsetIndex >= annotationsByOffsetCount)
        return null;

      List annotations = annotationsByOffset[offsetIndex].annotations;
      if (annotationIndex < 0 || annotationIndex >= annotations.size())
        return null;

      return (Annotation) annotations.get(annotationIndex);
    }

    public void addBinding(String ruleName, String label)
    {
      Binding new_binding;
      if (bindingsPool == null)
        new_binding = new Binding();
      else
      {
        new_binding = bindingsPool;
        bindingsPool = new_binding.next;
      }

      new_binding.ruleName   = ruleName;
      new_binding.label      = label;
      new_binding.annotation = currentAnnotation();
      new_binding.refCount   = 1;
      new_binding.next       = bindings;

      bindings = new_binding;
    }

    public Map getBindings(String ruleName)
    {
      Map result = new HashMap();

      Binding binding = bindings;
      while (binding != null)
      {
        if (binding.ruleName.equals(ruleName))
        {
          AnnotationSet annots = (AnnotationSet) result.get(binding.label);
          if (annots == null)
          {
            annots = new AnnotationSetImpl(document);
            result.put(binding.label, annots);
          }
          annots.add(binding.annotation);
        }

        binding = binding.next;
      }

      return result;
    }

    public void close()
    {
      while (bindings != null)
      {
        bindings.refCount--;

        if (bindings.refCount > 0)
          break;

        Binding temp  = bindings.next;
        bindings.next = bindingsPool;
        bindingsPool  = bindings;

        bindings = temp;
      }
      bindings = null;

      next = statesPool;
      statesPool = this;
    }
  }
}
