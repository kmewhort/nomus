/*
* AnnotationsByOffset.java
* Copyright:    Copyright (c) 2005, Ontotext Lab.
* Company:      Ontotext Lab.
* Krasimir Angelov 12/2005 */

package com.ontotext.gate.japec;

import java.io.*;
import java.util.*;

import gate.*;
import gate.jape.*;
import gate.creole.ontology.Ontology;
import gate.util.Strings;

public abstract class RuleAction implements Comparable {
  int priority;
  int fileLine;
  long lastOffset;

  public Map bindings;

  public RuleAction(String ruleName, int priority, int fileLine, SinglePhaseTransducer.State state) {
    Annotation annotation = state.currentAnnotation();

    this.bindings   = state.getBindings(ruleName);
    this.priority   = priority;
    this.fileLine   = fileLine;
    this.lastOffset = annotation.getEndNode().getOffset().longValue();
  }

  public void transduce(Document doc, AnnotationSet inputAS, AnnotationSet outputAS, Ontology ontology)
      throws JapeException {
    try {
      doit(doc, outputAS, inputAS, outputAS, ontology);
    } catch (Exception e) {
      StringWriter stackTraceWriter = new StringWriter();
      e.printStackTrace(new PrintWriter(stackTraceWriter));
      throw new JapeException(
          "Couldn't run RHS action: " + Strings.getNl() +
          stackTraceWriter.getBuffer().toString());
    }
  }

  public abstract void doit(Document doc, AnnotationSet annotations, AnnotationSet inputAS, AnnotationSet outputAS, Ontology ontology);

  Map getBindings()
  {
    return bindings;
  }

  public int compareTo(Object obj) {
   if (obj instanceof RuleAction) {
     if(obj == this) return 0;
     RuleAction other = (RuleAction) obj;
     if (lastOffset < other.lastOffset) return -1;
     else if(lastOffset > other.lastOffset) return 1;
     //equal length
     else if(priority < other.priority) return -1;
     else if(priority > other.priority) return 1;
     //equal priority
     else if(other.fileLine < fileLine) return -1;
     else if(other.fileLine > fileLine) return 1;
     //equal fileLine
     else return 0;
   } else throw new ClassCastException(
                   "Attempt to compare a KIMRhsAction object to an object " +
                   "of type " + obj.getClass()+"!");
 }
}
