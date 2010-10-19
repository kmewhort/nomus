/*
 *  SinglePhaseTransducer.java - transducer class
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
 *  Modifications by Luc Plamondon, Université de Montréal, 20/11/03:
 *  - Migrated original file from gate.jape to ca.umontreal.iro.rali.gate.jape
 *  - When testing for equality between a constraint and a path (an annotation),
 *    check whether the constraint is negated.  If yes, succeed if annotation
 *    type is NOT equal to constraint annotation type.  Also compare attributes
 *    when the negated constraint has ones.
 *  - When testing for equality between the attributes and values, call a
 *    "subsume" method that is aware that the comparison operator for each 
 *    attribute may be different from equality.
 *  - Allows constraints on different types of annotation
 *
 *  $Id$
 */


package ca.umontreal.iro.rali.gate.jape;

import java.io.*;

import gate.annotation.*;
import gate.util.*;
import gate.*;
import gate.gui.*;
import gate.creole.*;
import gate.event.*;

import java.util.*;

import ca.umontreal.iro.rali.gate.fsm.*;

/**
  * Represents a complete CPSL grammar, with a phase name, options and
  * rule set (accessible by name and by sequence).
  * Implements a transduce method taking a Document as input.
  * Constructs from String or File.
  */
public class SinglePhaseTransducer
extends Transducer implements JapeConstants, java.io.Serializable
{
  /** Debug flag */
  private static final boolean DEBUG = false;

  /** Construction from name. */
  public SinglePhaseTransducer(String name) {
    this.name = name;
    rules = new PrioritisedRuleList();
    finishedAlready = false;
  } // Construction from name

  /** Type of rule application (constants defined in JapeConstants). */
  private int ruleApplicationStyle = BRILL_STYLE;

  /** Set the type of rule application (types defined in JapeConstants). */
  public void setRuleApplicationStyle(int style) {
    ruleApplicationStyle = style;
  }

  /** The list of rules in this transducer. Ordered by priority and
    * addition sequence (which will be file position if they come from
    * a file).
    */
  private PrioritisedRuleList rules;

  FSM fsm;

  public FSM getFSM(){
    return fsm;
  }

  /** Add a rule. */
  public void addRule(Rule rule) {
    rules.add(rule);
  } // addRule

  /** The values of any option settings given. */
  private java.util.HashMap optionSettings = new java.util.HashMap();

  /** Add an option setting. If this option is set already, the new
    * value overwrites the previous one.
    */
  public void setOption(String name, String setting) {
    optionSettings.put(name, setting);
  } // setOption

  /** Get the value for a particular option. */
  public String getOption(String name) {
    return (String) optionSettings.get(name);
  } // getOption

  /** Whether the finish method has been called or not. */
  private boolean finishedAlready;

  /** Finish: replace dynamic data structures with Java arrays; called
    * after parsing.
    */
  public void finish(){
    // both MPT and SPT have finish called on them by the parser...
    if(finishedAlready) return;
    else finishedAlready = true;

    //each rule has a RHS which has a string for java code
    //those strings need to be compiled now
    Map actionClasses = new HashMap(rules.size());
    for(Iterator i = rules.iterator(); i.hasNext(); ){
      Rule rule = (Rule)i.next();
      rule.finish();
      actionClasses.put(rule.getRHS().getActionClassName(),
                        rule.getRHS().getActionClassString());
    }
    try{
      Javac.loadClasses(actionClasses);
    }catch(Exception e){
      Err.prln("Compile error:\n" + e.getMessage());
//e.printStackTrace();
    }

    //build the finite state machine transition graph
    fsm = new FSM(this);
    //clear the old style data structures
    rules.clear();
    rules = null;
  } // finish

  private void addAnnotationsByOffset(SimpleSortedSet keys, Set annotations){
    Iterator annIter = annotations.iterator();
    while(annIter.hasNext()){
      Annotation ann = (Annotation)annIter.next();
      //ignore empty annotations
      long offset = ann.getStartNode().getOffset().longValue();
      if(offset == ann.getEndNode().getOffset().longValue())
        continue;
//dam: was
/*
//      Long offset = ann.getStartNode().getOffset();

      List annsAtThisOffset = null;
      if(keys.add(offset)){
        annsAtThisOffset = new LinkedList();
        map.put(offset, annsAtThisOffset);
      }else{
        annsAtThisOffset = (List)map.get(offset);
      }
      annsAtThisOffset.add(ann);
*/
//dam: end
      keys.add(offset, ann);
    }
  }//private void addAnnotationsByOffset()


  /**
    * Transduce a document using the annotation set provided and the current
    * rule application style.
    */
  public void transduce(Document doc, AnnotationSet inputAS,
                        AnnotationSet outputAS) throws JapeException,
                                                       ExecutionException {
    interrupted = false;
    fireProgressChanged(0);

    //the input annotations will be read from this map
    //maps offset to list of annotations

//dam was
/*
    Map annotationsByOffset = new HashMap();

    SortedSet offsets = new TreeSet();
*/
//dam: now
    SimpleSortedSet offsets = new SimpleSortedSet();
    SimpleSortedSet annotationsByOffset = offsets;
//dam: end

    //select only the annotations of types specified in the input list
//    Out.println("Input:" + input);
    if(input.isEmpty())
    {
//dam: was
//        addAnnotationsByOffset(annotationsByOffset, offsets, inputAS);
//dam: now
        addAnnotationsByOffset(offsets, inputAS);
//dam: end
    } else {
      Iterator typesIter = input.iterator();
      AnnotationSet ofOneType = null;
      while(typesIter.hasNext()){
        ofOneType = inputAS.get((String)typesIter.next());
        if(ofOneType != null){
//dam: was
//        addAnnotationsByOffset(annotationsByOffset, offsets, ofOneType);
//dam: now
          addAnnotationsByOffset(offsets, ofOneType);
//dam: end
        }
      }
    }

    if(annotationsByOffset.isEmpty()){
      fireProcessFinished();
      return;
    }

    annotationsByOffset.sort();
    //define data structures
    //FSM instances that haven't blocked yet
    java.util.ArrayList activeFSMInstances = new java.util.ArrayList();

    // FSM instances that have reached a final state
    // This set will be sorted later by the length
    // of the document content covered by the matched annotations
    java.util.ArrayList acceptingFSMInstances = new ArrayList();
    FSMInstance currentFSM;


    //find the first node of the document
    Node startNode = ((Annotation)
                      ((ArrayList)annotationsByOffset.
                             get(offsets.first())).get(0)).
                      getStartNode();

    //used to calculate the percentage of processing done
    long lastNodeOff = doc.getContent().size().longValue();

    //the offset of the node where the matching currently starts
    //the value -1 marks no more annotations to parse
    long startNodeOff = startNode.getOffset().longValue();

    //used to decide when to fire progress events
    long oldStartNodeOff = 0;

    //the big while for the actual parsing
    while(startNodeOff != -1){
//Out.prln();
//Out.pr("Start: " + startNodeOff);
      //while there are more annotations to parse
      //create initial active FSM instance starting parsing from new startNode
      //currentFSM = FSMInstance.getNewInstance(
      currentFSM = new FSMInstance(
                  fsm,
                  fsm.getInitialState(),//fresh start
                  startNode,//the matching starts form the current startNode
                  startNode,//current position in AG is the start position
                  new java.util.HashMap(),//no bindings yet!
                  doc
                  );

      // at this point ActiveFSMInstances should always be empty!
      activeFSMInstances.clear();
      acceptingFSMInstances.clear();
//dam: was used LinkedList
//      activeFSMInstances.addLast(currentFSM);
//dam: now used ArrayList
      activeFSMInstances.add(currentFSM);
//dam: end

      //far each active FSM Instance, try to advance
      whileloop2:
      while(!activeFSMInstances.isEmpty()){
        if(interrupted) throw new ExecutionInterruptedException(
          "The execution of the \"" + getName() +
          "\" Jape transducer has been abruptly interrupted!");

//Out.pr(" <" + acceptingFSMInstances.size() + "/" +
//              activeFSMInstances.size() +">");
        // take the first active FSM instance
        currentFSM = (FSMInstance)activeFSMInstances.remove(0);

        // process the current FSM instance
        if(currentFSM.getFSMPosition().isFinal()){
          //the current FSM is in a final state
//dam: was LinkedList
//          acceptingFSMInstances.addLast(currentFSM.clone());
//dam: now
          acceptingFSMInstances.add(currentFSM.clone());
//dam: end
//          //if we are in APPELT mode clear all the accepting instances
//          //apart from the longest one
//          if(ruleApplicationStyle == APPELT_STYLE &&
//             acceptingFSMInstances.size() > 1){
//            Object longestAcceptor = acceptingFSMInstances.last();
//            acceptingFSMInstances.clear();
//            acceptingFSMInstances.add(longestAcceptor);
//          }
          //if we're only looking for the shortest stop here
          if(ruleApplicationStyle == FIRST_STYLE) break whileloop2;
        }

        // get all the annotations that start where the current FSM finishes
        SimpleSortedSet offsetsTailSet = offsets.tailSet(
			currentFSM.getAGPosition().getOffset().longValue());

        ArrayList paths; 
        long theFirst = offsetsTailSet.first();
        if(theFirst <0)
          continue;
	paths = (ArrayList)annotationsByOffset.get(theFirst);
	//System.out.println("Paths: " + paths + "\n^localInputIndex: " + localInputIndex);
        if(paths.isEmpty()) continue;

	// get the transitions for the current state of the FSM
	State currentState = currentFSM.getFSMPosition();	
	Iterator transitionsIter = currentState.getTransitions().iterator();

	// for each transition, keep the set of annotations starting at current
	// node (the "paths") that match each constraint of the transition
	transitionsWhile:
	while(transitionsIter.hasNext()){
	  Transition currentTransition = (Transition)transitionsIter.next();
	  Constraint[] currentConstraints = 
	               currentTransition.getConstraints().getConstraints();

	  // separate negated from "positive" constraints
	  ArrayList negatedConstraints = new ArrayList(3);
	  ArrayList positiveConstraints = new ArrayList(3);
	  for (int i=0; i<currentConstraints.length; i++) {
	    if (currentConstraints[i].isNegated())
	      negatedConstraints.add(currentConstraints[i]);
	    else
	      positiveConstraints.add(currentConstraints[i]);
	  }

	  // First test against negative constraints; skip this transition
	  // as soon as there is an unwanted match
	  for (Iterator pathsIter=paths.iterator(); pathsIter.hasNext(); ) {
            Annotation onePath = (Annotation)pathsIter.next();
	    for (int i=0, size=negatedConstraints.size(); i<size; i++) {
	      Constraint theConstraint = (Constraint) negatedConstraints.get(i);
	      if (theConstraint.getAnnotType().equals(onePath.getType()))
		// if type is the same, also check attributes
		if (theConstraint.subsumesOne((SimpleFeatureMapImpl) onePath.getFeatures())) {
		  continue transitionsWhile;
		}
	    }
	  }

	  // Then test against positive constraints and keep track of the
	  // annots that match each constraint.
	  
	  // First create empty lists for each constraint...
	  ArrayList matchingAnnot = new ArrayList(positiveConstraints.size());
	  for (int i=0, size=positiveConstraints.size(); i<size; i++) {
	    matchingAnnot.add(new LinkedList());
	  }

	  // for each annot. at the current node in the annot graph...
	  for (Iterator pathsIter=paths.iterator(); pathsIter.hasNext(); ) {
	    Annotation onePath = (Annotation)pathsIter.next();
	    
	    // for each positive constraint...
	    for (int i=0, size=positiveConstraints.size(); i<size; i++) {
	      Constraint theConstraint = (Constraint) positiveConstraints.get(i);
	      if (theConstraint.getAnnotType().equals(onePath.getType())) {
		// if type is the same, also check attributes
		if (theConstraint.subsumes((SimpleFeatureMapImpl) onePath.getFeatures())) {
		  ((LinkedList) matchingAnnot.get(i)).add(onePath);
		}
	      }
	    } // for each positive constraint
	  } // for each annot.

	  // If there are only negative constraints and no positive constraints,
	  // let the first list of matchingAnnot contain all annots at this node
	  if (negatedConstraints.size() != 0 && matchingAnnot.size() == 0) {
	    matchingAnnot.add(new LinkedList(paths));
	  }

	  // We have a match if every constraint is met by at least one annot.
	  // Given the sets Sx of the annotations that match constraint x, 
          // compute all tuples (A1, A2, ..., An) where Ax comes from the 
          // set Sx and n is the number of constraints
	  LinkedList combinations = combine(
		     matchingAnnot, matchingAnnot.size(), new LinkedList());
	    
	  // Create a new FSM for every tuple of annot
	  for (ListIterator tuplesIter = combinations.listIterator(); 
	       tuplesIter.hasNext(); ) {

	    LinkedList tuple = (LinkedList) tuplesIter.next();
	    //we have a match
	    //System.out.println("Match!");
	    //create a new FSMInstance, advance it over the current annotation
	    //take care of the bindings  and add it to ActiveFSM
	    FSMInstance newFSMI = (FSMInstance)currentFSM.clone();

	    // Find rightmost node offset
	    Node endNode = null;
	    for (ListIterator annotsInTupleIter = tuple.listIterator();
		 annotsInTupleIter.hasNext(); ) {

	      Annotation annot = (Annotation) annotsInTupleIter.next();
	      if (endNode == null) {
		// first loop: initialise
		endNode = annot.getEndNode();
	      }
	      else {
		// check for a rightmost offset
		if (annot.getEndNode().getOffset().compareTo(endNode.getOffset()) > 0) {
		  endNode = annot.getEndNode();
		}
	      } // else
	    } // ListIterator annotsInTupleIter

	    newFSMI.setAGPosition(endNode);
	    newFSMI.setFSMPosition(currentTransition.getTarget());

	    //bindings
	    java.util.Map binds = newFSMI.getBindings();
	    java.util.Iterator labelsIter =
                                   currentTransition.getBindings().iterator();
	    String oneLabel;
	    AnnotationSet boundAnnots, newSet;
	    while(labelsIter.hasNext()){
	      oneLabel = (String)labelsIter.next();
	      boundAnnots = (AnnotationSet)binds.get(oneLabel);
	      if(boundAnnots != null)
		newSet = new AnnotationSetImpl((AnnotationSet)boundAnnots);
	      else
		newSet = new AnnotationSetImpl(doc);

	      // newSet.addAll(tuple);  Ok with Gate 2.1, but buggy with 2.2!!!
	      // So call newSet.add one at a time so that annots keep their IDs
	      for (ListIterator annotsInTupleIter = tuple.listIterator();
		   annotsInTupleIter.hasNext(); ) {
		newSet.add((Annotation) annotsInTupleIter.next());
	      }

	      binds.put(oneLabel, newSet);

	    }//while(labelsIter.hasNext())
	    activeFSMInstances.add(newFSMI);
	    //Out.pr("^(" + newFSMI.getStartAGPosition().getOffset() +
	    //                               "->" + newFSMI.getAGPosition().getOffset() + ")");
	  } //while combinationsIter.hasNext()

	}//while(transitionsIter.hasNext())
      }//while(!activeFSMInstances.isEmpty())


      //FIRE THE RULE
//dam: use long
//      Long lastAGPosition = null;
//dam: now
      long lastAGPosition = -1;
//dam: end
      if(acceptingFSMInstances.isEmpty()){
        //no rule to fire, advance to the next input offset
        lastAGPosition = startNodeOff + 1;
      } else if(ruleApplicationStyle == BRILL_STYLE) {
      //System.out.println("Brill acceptor");
        // fire the rules corresponding to all accepting FSM instances
        java.util.Iterator accFSMs = acceptingFSMInstances.iterator();
        FSMInstance currentAcceptor;
        RightHandSide currentRHS;
        lastAGPosition = startNode.getOffset().longValue();

        while(accFSMs.hasNext()){
          currentAcceptor = (FSMInstance) accFSMs.next();

          currentRHS = currentAcceptor.getFSMPosition().getAction();
          currentRHS.transduce(doc, currentAcceptor.getBindings(),
                               inputAS, outputAS, ontology);
//dam: use long
//          Long currentAGPosition = currentAcceptor.getAGPosition().getOffset();
//dam: now
          long currentAGPosition = 
	       currentAcceptor.getAGPosition().getOffset().longValue();
//dam: end
          if(currentAGPosition > lastAGPosition)
            lastAGPosition = currentAGPosition;
        }

      } else if(ruleApplicationStyle == APPELT_STYLE ||
                ruleApplicationStyle == FIRST_STYLE ||
                ruleApplicationStyle == ONCE_STYLE) {

//System.out.println("Appelt acceptor");
	
	// We want the rule that matches the longest document region, so use
	// a reverse comparator based on the length to the region and rule
	// priority (implemented in the compareTo method of FSMInstance).  
	// However, the FSM that are equal (same length and priority) must
	// stay in the same order, because the transitions were added in a
	// a specific order by FSM.convertComplexPE to implement the greediness
	// of "*" and "+" Kleene operators (loop transitions must be explored
	// before the others)
        Collections.sort(acceptingFSMInstances, Collections.reverseOrder());

        FSMInstance currentAcceptor =(FSMInstance)acceptingFSMInstances.get(0);
        if(isDebugMode()){
          //see if we have any conflicts
          Iterator accIter = acceptingFSMInstances.iterator();
          FSMInstance anAcceptor;
          List conflicts = new ArrayList();
          while(accIter.hasNext()){
            anAcceptor = (FSMInstance)accIter.next();
            if(anAcceptor.equals(currentAcceptor)){
              conflicts.add(anAcceptor);
            }else{
              break;
            }
          }
          if(conflicts.size() > 1){
            Out.prln("\nConflicts found during matching:" +
                     "\n================================");
            accIter = conflicts.iterator();
            int i = 0;
            while(accIter.hasNext()){
              Out.prln(i++ + ") " + accIter.next().toString());
            }
          }
        }

        RightHandSide currentRHS = currentAcceptor.getFSMPosition().getAction();
        currentRHS.transduce(doc, currentAcceptor.getBindings(),
                             inputAS, outputAS, ontology);

        //if in ONCE mode stop after first match
        if(ruleApplicationStyle == ONCE_STYLE) return;

        //advance in AG
        lastAGPosition = currentAcceptor.getAGPosition().getOffset().longValue();
      }
//      else if(ruleApplicationStyle == FIRST_STYLE) {
//        // AcceptingFSMInstances is an ordered structure:
//        // just execute the shortest (first) rule
//
//        FSMInstance currentAcceptor =(FSMInstance)acceptingFSMInstances.first();
//        RightHandSide currentRHS = currentAcceptor.getFSMPosition().getAction();
//        currentRHS.transduce(doc, outputAS, currentAcceptor.getBindings());
//        //advance in AG
//        long lastAGPosition = currentAcceptor.getAGPosition().
//                              getOffset().longValue();
//        //advance the index on input
//        while(inputIndex < annotations.size() &&
//              ((Annotation)annotations.get(inputIndex)).
//              getStartNode().getOffset().longValue() < lastAGPosition){
//          inputIndex++;
//        }
//      }
      else throw new RuntimeException("Unknown rule application style!");


      //advance on input
//      SortedSet OffsetsTailSet = offsets.tailSet(lastAGPosition);
      SimpleSortedSet OffsetsTailSet = offsets.tailSet(lastAGPosition);
//<<< DAM: isEmpty speedup
/*
      if(OffsetsTailSet.isEmpty()){
*/
//=== DAM: now
        long theFirst = OffsetsTailSet.first();
      if( theFirst < 0){
//>>> DAM: end
        //no more input, phew! :)
        startNodeOff = -1;
        fireProcessFinished();
      }else{
//<<< DAM: use long
/*
        Long nextKey = (Long)OffsetsTailSet.first();
*/
//=== DAM: now
        long nextKey = theFirst;
//>>> DAM: end
        startNode = ((Annotation)
                      ((ArrayList)annotationsByOffset.get(nextKey)).get(0)). //nextKey
                    getStartNode();
        startNodeOff = startNode.getOffset().longValue();

        //eliminate the possibility for infinite looping
        if(oldStartNodeOff == startNodeOff){
//Out.prln("");
//Out.pr("SKIP " + startNodeOff);
          //we are about to step twice in the same place, ...skip ahead
          lastAGPosition = startNodeOff + 1;
          OffsetsTailSet = offsets.tailSet(lastAGPosition);
//<<< DAM: isEmpty speedup
/*
          if(OffsetsTailSet.isEmpty()){
*/
//=== DAM: now
          theFirst = OffsetsTailSet.first();
          if(theFirst < 0){
//>>> DAM: end
            //no more input, phew! :)
            startNodeOff = -1;
            fireProcessFinished();
          }else{
//<<< DAM: use long
//            nextKey = (Long)OffsetsTailSet.first();
//=== DAM: now
            nextKey = theFirst;
//>>> DAM: end
            startNode = ((Annotation)
                          ((List)annotationsByOffset.get(theFirst)).get(0)).
                        getStartNode();
            startNodeOff =startNode.getOffset().longValue();
          }
//Out.prln(" ->" + startNodeOff);
        }//if(oldStartNodeOff == startNodeOff)


        //fire the progress event
        if(startNodeOff - oldStartNodeOff > 256){
          if(isInterrupted()) throw new ExecutionInterruptedException(
            "The execution of the \"" + getName() +
            "\" Jape transducer has been abruptly interrupted!");

          fireProgressChanged((int)(100 * startNodeOff / lastNodeOff));
          oldStartNodeOff = startNodeOff;
        }
      }
    }//while(startNodeOff != -1)
    fireProcessFinished();
  } // transduce


  /** Clean up (delete action class files, for e.g.). */
  public void cleanUp() {
//    for(DListIterator i = rules.begin(); ! i.atEnd(); i.advance())
//      ((Rule) i.get()).cleanUp();
  } // cleanUp

  /** A string representation of this object. */
  public String toString() {
    return toString("");
  } // toString()

  /** A string representation of this object. */
  public String toString(String pad) {
    String newline = Strings.getNl();
    String newPad = Strings.addPadding(pad, INDENT_PADDING);

    StringBuffer buf =
      new StringBuffer(pad + "SPT: name(" + name + "); ruleApplicationStyle(");

    switch(ruleApplicationStyle) {
      case APPELT_STYLE: buf.append("APPELT_STYLE); "); break;
      case BRILL_STYLE:  buf.append("BRILL_STYLE); ");  break;
      default: break;
    }

    buf.append("rules(" + newline);
    Iterator rulesIterator = rules.iterator();
    while(rulesIterator.hasNext())
      buf.append(((Rule) rulesIterator.next()).toString(newPad) + " ");

    buf.append(newline + pad + ")." + newline);

    return buf.toString();
  } // toString(pad)

  //needed by fsm
  public PrioritisedRuleList getRules() {
    return rules;
  }

  /**
    * Adds a new type of input annotations used by this transducer.
    * If the list of input types is empty this transducer will parse all the
    * annotations in the document otherwise the types not found in the input
    * list will be completely ignored! To be used with caution!
    */
  public void addInput(String ident) {
    input.add(ident);
  }
  public synchronized void removeProgressListener(ProgressListener l) {
    if (progressListeners != null && progressListeners.contains(l)) {
      Vector v = (Vector) progressListeners.clone();
      v.removeElement(l);
      progressListeners = v;
    }
  }
  public synchronized void addProgressListener(ProgressListener l) {
    Vector v = progressListeners == null ? new Vector(2) : (Vector) progressListeners.clone();
    if (!v.contains(l)) {
      v.addElement(l);
      progressListeners = v;
    }
  }

  /**
    * Defines the types of input annotations that this transducer reads. If this
    * set is empty the transducer will read all the annotations otherwise it
    * will only "see" the annotations of types found in this list ignoring all
    * other types of annotations.
    */
  java.util.Set input = new java.util.HashSet();
  private transient Vector progressListeners;

  protected void fireProgressChanged(int e) {
    if (progressListeners != null) {
      Vector listeners = progressListeners;
      int count = listeners.size();
      for (int i = 0; i < count; i++) {
        ((ProgressListener) listeners.elementAt(i)).progressChanged(e);
      }
    }
  }
  protected void fireProcessFinished() {
    if (progressListeners != null) {
      Vector listeners = progressListeners;
      int count = listeners.size();
      for (int i = 0; i < count; i++) {
        ((ProgressListener) listeners.elementAt(i)).processFinished();
      }
    }
  }
  public int getRuleApplicationStyle() {
    return ruleApplicationStyle;
  }

  /*
  private void writeObject(ObjectOutputStream oos) throws IOException {
    Out.prln("writing spt");
    oos.defaultWriteObject();
    Out.prln("finished writing spt");
  } // writeObject
  */

  /**
     Computes all tuples (x1, x2, ..., xn) resulting from the linear
     combination of the elements of n lists, where x1 comes from the 1st list,
     x2 comes from the second, etc.  This method works recursively.  The first
     call should have those parameters:

     @param sourceLists an array of n lists whose elements will be combined
     @param maxTupleSize the number of elements per tuple
     @param incompleteTuple an empty list
  */

  private static LinkedList combine(ArrayList sourceLists, 
			    int maxTupleSize, 
			    LinkedList incompleteTuple) {

    LinkedList newTupleList = new LinkedList();

    if (incompleteTuple.size() == maxTupleSize) {
      newTupleList.add(incompleteTuple);
    }
    else {
      LinkedList currentSourceList = (LinkedList) sourceLists.get(incompleteTuple.size());
      // use for loop instead of ListIterator to increase speed (critical here)
      for (int i=0; i<currentSourceList.size(); i++) {
	LinkedList augmentedTuple = (LinkedList) incompleteTuple.clone();
	augmentedTuple.add(currentSourceList.get(i));
	newTupleList.addAll(combine(sourceLists, maxTupleSize, augmentedTuple));
      }
    }

    return newTupleList;
  }




} // class SinglePhaseTransducer

/*
class SimpleSortedSet {

    static final int INCREMENT = 1023;
    int[] theArray = new int[INCREMENT];
    Object[] theObject = new Object[INCREMENT];
    int tsindex = 0;
    int size = 0;
    public static int avesize = 0;
    public static int maxsize = 0;
    public static int avecount = 0;
    public SimpleSortedSet()
    {
        avecount++;
        java.util.Arrays.fill(theArray, Integer.MAX_VALUE);
    }

    public Object get(int elValue)
    {
        int index = java.util.Arrays.binarySearch(theArray, elValue);
        if (index >=0)
            return theObject[index];
        return null;
    }

    public boolean add(int elValue, Object o)
    {
        int index = java.util.Arrays.binarySearch(theArray, elValue);
        if (index >=0)
        {
            ((ArrayList)theObject[index]).add(o);
            return false;
        }
        if (size == theArray.length)
        {
            int[] temp = new int[theArray.length + INCREMENT];
            Object[] tempO = new Object[theArray.length + INCREMENT];
            System.arraycopy(theArray, 0, temp, 0, theArray.length);
            System.arraycopy(theObject, 0, tempO, 0, theArray.length);
            java.util.Arrays.fill(temp, theArray.length, temp.length , Integer.MAX_VALUE);
            theArray = temp;
            theObject = tempO;
        }
        index = ~index;
        System.arraycopy(theArray, index, theArray, index+1, size - index );
        System.arraycopy(theObject, index, theObject, index+1, size - index );
        theArray[index] = elValue;
        theObject[index] = new ArrayList();
        ((ArrayList)theObject[index]).add(o);
        size++;
        return true;
    }
    public int first()
    {
        if (tsindex >= size) return -1;
        return theArray[tsindex];
    }

    public Object getFirst()
    {
        if (tsindex >= size) return null;
        return theObject[tsindex];
    }

    public SimpleSortedSet tailSet(int elValue)
    {
        if (tsindex < theArray.length && elValue != theArray[tsindex])
        {
            if (tsindex<(size-1) && elValue > theArray[tsindex] &&
                elValue <= theArray[tsindex+1])
                {
                    tsindex++;
                   return this;
                }
            int index = java.util.Arrays.binarySearch(theArray, elValue);
            if (index < 0)
                index = ~index;
            tsindex = index;
        }
        return this;
    }

    public boolean isEmpty()
    {
        return size ==0;
    }
};
*/
