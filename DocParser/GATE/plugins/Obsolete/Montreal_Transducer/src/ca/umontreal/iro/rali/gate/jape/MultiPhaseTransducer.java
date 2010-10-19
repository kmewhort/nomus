/*
 *  MultiPhaseTransducer.java - transducer class
 *
 *  Copyright (c) 1995-2010, The University of Sheffield. See the file
 *  COPYRIGHT.txt in the software or at http://gate.ac.uk/gate/COPYRIGHT.txt
 *
 *  This file is part of GATE (see http://gate.ac.uk/), and is free
 *  software, licenced under the GNU Library General Public License,
 *  Version 2, June 1991 (in the distribution as file licence.html,
 *  and also available at http://gate.ac.uk/gate/licence.html).
 *
 *  Hamish Cunningham, 24/07/98
 *
 *  Minor modifications made by Luc Plamondon, Université de Montréal, 27/11/03:
 *  - migrated original file to the ca.umontreal.iro.rali.gate.jape package.
 *
 *  $Id$
 */


package ca.umontreal.iro.rali.gate.jape;

import java.util.*;


import gate.annotation.*;
import gate.event.*;
import gate.util.*;
import gate.creole.*;
import gate.creole.ontology.Ontology;
import gate.*;


/**
  * Represents a complete CPSL grammar, with a phase name, options and
  * rule set (accessible by name and by sequence).
  * Implements a transduce method taking a Document as input.
  * Constructs from String or File.
  */
public class MultiPhaseTransducer extends Transducer
implements JapeConstants, java.io.Serializable
{
  /** Debug flag */
  private static final boolean DEBUG = false;

  /** Construction from name. */
  public MultiPhaseTransducer(String name) {
    this();
    setName(name);
  } // constr from name

  /**
   * Notifies this PR that it should stop its execution as soon as possible.
   */
  public synchronized void interrupt(){
    interrupted = true;
    Iterator phasesIter = phases.iterator();
    while(phasesIter.hasNext()){
      ((Transducer)phasesIter.next()).interrupt();
    }
  }


  /** Anonymous construction */
  public MultiPhaseTransducer() {
    phases = new ArrayList();
  } // anon construction

  /** Set the name. */
  public void setName(String name) { this.name = name; }

  /** The SinglePhaseTransducers that make up this one.
    * Keyed by their phase names.
    */
  private ArrayList phases;


  /**
   * Sets the ontology used by this transducer;
   * @param ontology an {@link gate.creole.ontology.Ontology} value;
   */
  public void setOntology(Ontology ontology) {
    super.setOntology(ontology);
    Iterator phasesIter = phases.iterator();
    while(phasesIter.hasNext()){
      ((Transducer)phasesIter.next()).setOntology(ontology);
    }
  }

  /** Add phase. */
  public void addPhase(String name, Transducer phase) {
    //Debug.pr(this, "MPT: adding " + name + Debug.getNl());
    phases.add(phase);
  } // addPhase

  /** Change the phase order to the one specified in a list of names. */
  public void orderPhases(String[] phaseNames) {
    Err.println("oops: MPT.orderPhases not done yet :-(");
    /*
    // for each phaseName
    //   destructively get the phase and add to new array map
    // errors: any phaseName not in phases,
    HashMap newPhaseMap = new HashMap();
    for(int i=0; i<phaseNames.length; i++) {
      Transducer t = (Transducer) phases.remove(phaseNames[i]);
      if(t == null) {
        // ERROR
      }
      else {
        newPhaseMap.add(t);
      }
    }
    phases = newPhaseMap;
    */
  } // orderPhases


  /** Finish: replace dynamic data structures with Java arrays; called
    * after parsing.
    */
  public void finish(){
    for(Iterator i = phases.iterator(); i.hasNext(); )
      ((Transducer) i.next()).finish();
  } // finish


  /** Transduce the document by running each phase in turn. */
  public void transduce(Document doc, AnnotationSet input,
                        AnnotationSet output) throws JapeException,
                                                     ExecutionException {

    interrupted = false;
    ProgressListener pListener = null;
    StatusListener sListener = null;
    pListener = new ProgressListener(){
      public void processFinished(){
        donePhases ++;
        if(donePhases == phasesCnt) fireProcessFinished();
      }

      public void progressChanged(int i){
        int value = (donePhases * 100 + i)/phasesCnt;
        fireProgressChanged(value);
      }

      int phasesCnt = phases.size();
      int donePhases = 0;
    };

    sListener = new StatusListener(){
      public void statusChanged(String text){
        fireStatusChanged(text);
      }
    };

    for(Iterator i = phases.iterator(); i.hasNext(); ) {
      Transducer t = (Transducer) i.next();

      if(isInterrupted()) throw new ExecutionInterruptedException(
        "The execution of the \"" + getName() +
        "\" Jape transducer has been abruptly interrupted!");

      try {
        fireStatusChanged("Transducing " + doc.getName() +
                             " (Phase: " + t.getName() + ")...");
        t.addProgressListener(pListener);
        t.addStatusListener(sListener);

        t.transduce(doc, input, output);
        t.removeProgressListener(pListener);
        t.removeStatusListener(sListener);
        fireStatusChanged("");
      } catch(JapeException e) {
        String errorMessage = new String(
          "Error transducing document " + doc.getName() +
          ", phase " + t.getName() + Strings.getNl() + e.getMessage()
        );
        throw(new JapeException(errorMessage));
      }
    }

    cleanUp();
  } // transduce

  /** Ask each phase to clean up (delete action class files, for e.g.). */
  public void cleanUp() {

    for(Iterator i = phases.iterator(); i.hasNext(); )
      ((Transducer) i.next()).cleanUp();

  } // cleanUp

  /** Create a string representation of the object. */
  public String toString() { return toString(""); }

  /** Create a string representation of the object. */
  public String toString(String pad) {
    String newline = Strings.getNl();

    StringBuffer buf = new StringBuffer(
      pad + "MPT: name(" + name + "); phases(" + newline + pad
    );

    for(Iterator i = phases.iterator(); i.hasNext(); )
      buf.append(
        ((Transducer) i.next()).toString(
            Strings.addPadding(pad, INDENT_PADDING)
        ) + " "
      );

    buf.append(newline + pad + ")." + newline);

    return buf.toString();
  } // toString

  //needed by FSM
  public ArrayList getPhases(){ return phases; }

} // class MultiPhaseTransducer



