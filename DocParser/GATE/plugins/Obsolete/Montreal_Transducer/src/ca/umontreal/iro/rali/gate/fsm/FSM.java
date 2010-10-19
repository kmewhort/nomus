/*
 *  FSM.java
 *
 *  Copyright (c) 1995-2010, The University of Sheffield. See the file
 *  COPYRIGHT.txt in the software or at http://gate.ac.uk/gate/COPYRIGHT.txt
 *
 *  This file is part of GATE (see http://gate.ac.uk/), and is free
 *  software, licenced under the GNU Library General Public License,
 *  Version 2, June 1991 (in the distribution as file licence.html,
 *  and also available at http://gate.ac.uk/gate/licence.html).
 *
 *  Valentin Tablan, 29/Mar/2000
 *
 *  Minor modifications made by Luc Plamondon, Université de Montréal, 27/11/03:
 *  - Migrated original file from gate.fsm to 
 *    ca.umontreal.iro.rali.gate.fsm package.
 *  - Changed processing of Kleene operators in convertComplexPE.
 *
 *  $Id$
 */

package ca.umontreal.iro.rali.gate.fsm;

import java.util.*;

import ca.umontreal.iro.rali.gate.jape.*;
import gate.util.*;

/**
  * This class implements a standard Finite State Machine.
  * It is used for both deterministic and non-deterministic machines.
  */
public class FSM implements JapeConstants {

  /** Debug flag */
  private static final boolean DEBUG = false;

  /**
    * Builds a standalone FSM starting from a single phase transducer.
    * @param spt the single phase transducer to be used for building this FSM.
    */
  public FSM(SinglePhaseTransducer spt){
    initialState = new State();
    transducerName = spt.getName();
    Iterator rulesEnum = spt.getRules().iterator();
    Rule currentRule;

    while(rulesEnum.hasNext()){
      currentRule = (Rule) rulesEnum.next();
      FSM ruleFSM = new FSM(currentRule);
      initialState.addTransition(new Transition(null,
                                                ruleFSM.getInitialState()));
    }

    eliminateVoidTransitions();
//Out.prln("Transducer " + spt.getName() + " converted to " + allStates.size() + " states");

  }

  /**
    * Builds a FSM starting from a rule. This FSM is actually a part of a larger
    * one (usually the one that is built based on the single phase transducer
    * that contains the rule).
    * built by this constructor.
    * @param rule the rule to be used for the building process.
    */
  public FSM(Rule rule) {

    initialState = new State();
    LeftHandSide lhs = rule.getLHS();
    PatternElement[][] constraints =
                       lhs.getConstraintGroup().getPatternElementDisjunction();
    // the rectangular array constraints is a disjunction of sequences of
    // constraints = [[PE]:[PE]...[PE] ||
    //                [PE]:[PE]...[PE] ||
    //                ...
    //                [PE]:[PE]...[PE] ]

    //The current and the next state for the current ROW.
    State currentRowState, nextRowState;
    State finalState = new State();
    PatternElement currentPattern;

    for(int i = 0; i < constraints.length; i++){
      // for each row we have to create a sequence of states that will accept
      // the sequence of annotations described by the restrictions on that row.
      // The final state of such a sequence will always be a final state which
      // will have associated the right hand side of the rule used for this
      // constructor.

      // For each row we will start from the initial state.
      currentRowState = initialState;
      for(int j=0; j < constraints[i].length; j++) {

        // parse the sequence of constraints:
        // For each basic pattern element add a new state and link it to the
        // currentRowState.
        // The case of kleene operators has to be considered!
        currentPattern = constraints[i][j];
        State insulator = new State();
        currentRowState.addTransition(new Transition(null,insulator));
        currentRowState = insulator;
        if(currentPattern instanceof BasicPatternElement) {
          //the easy case
          nextRowState = new State();
          currentRowState.addTransition(
            new Transition((BasicPatternElement)currentPattern, nextRowState));
          currentRowState = nextRowState;
        } else if(currentPattern instanceof ComplexPatternElement) {

          // the current pattern is a complex pattern element
          // ..it will probaly be converted into a sequence of states itself.
          currentRowState =  convertComplexPE(
                              currentRowState,
                              (ComplexPatternElement)currentPattern,
                              new LinkedList());
        } else {
          // we got an unknown kind of pattern
          throw new RuntimeException("Strange looking pattern: " +
                                     currentPattern);
        }

      } // for j

      //link the end of the current row to the final state using
      //an empty transition.
      currentRowState.addTransition(new Transition(null,finalState));
      finalState.setAction(rule.getRHS());
      finalState.setFileIndex(rule.getPosition());
      finalState.setPriority(rule.getPriority());
    } // for i
  }

  /**
    * Gets the initial state of this FSM
    * @return an object of type gate.fsm.State representing the initial state.
    */
  public State getInitialState() {
    return initialState;
  } // getInitialState

  /**
    * Receives a state to start from and a complex pattern element.
    * Parses the complex pattern element and creates all the necessary states
    * and transitions for accepting annotations described by the given PE.
    * @param state the state to start from
    * @param cpe the pattern to be recognized
    * @param label the bindings name for all the annotation accepted along
    * the way this is actually a listy of Strings. It is necessary to use
    * a list becuase of the reccursive definition of ComplexPatternElement.
    * @return the final state reached after accepting a sequence of annotations
    * as described in the pattern
    */
  private State convertComplexPE(State startState,
                                ComplexPatternElement cpe, LinkedList labels){
    //create a copy
    LinkedList newBindings = (LinkedList)labels.clone();
    String localLabel = cpe.getBindingName ();

    if(localLabel != null)newBindings.add(localLabel);

    PatternElement[][] constraints =
                       cpe.getConstraintGroup().getPatternElementDisjunction();

    // the rectangular array constraints is a disjunction of sequences of
    // constraints = [[PE]:[PE]...[PE] ||
    //                [PE]:[PE]...[PE] ||
    //                ...
    //                [PE]:[PE]...[PE] ]

    //The current and the next state for the current ROW.
    State currentRowState, nextRowState, endState = new State();
    PatternElement currentPattern;

    // For the "+" and "*" Kleene operator to be greedy, the loop transition
    // must appear before the other transitions of the current state.
    // A more complete processing of the Kleene operators will be done at 
    // the end.
    int kleeneOp = cpe.getKleeneOp();
    switch (kleeneOp){
      case KLEENE_PLUS:{

        // allow to return to startState
        endState.addTransition(new Transition(null,startState));
        break;
      }
      case KLEENE_STAR:{

        // allow to return to startState
        endState.addTransition(new Transition(null,startState));
        break;
      }
    } // switch (cpe.getKleeneOp())


    for(int i = 0; i < constraints.length; i++) {
      // for each row we have to create a sequence of states that will accept
      // the sequence of annotations described by the restrictions on that row.
      // The final state of such a sequence will always be a finale state which
      // will have associated the right hand side of the rule used for this
      // constructor.

      //For each row we will start from the initial state.
      currentRowState = startState;
      for(int j=0; j < (constraints[i]).length; j++) {

        //parse the sequence of constraints:
        //For each basic pattern element add a new state and link it to the
        //currentRowState.
        //The case of kleene operators has to be considered!
        State insulator = new State();
        currentRowState.addTransition(new Transition(null,insulator));
        currentRowState = insulator;
        currentPattern = constraints[i][j];
        if(currentPattern instanceof BasicPatternElement) {

          //the easy case
          nextRowState = new State();
          currentRowState.addTransition(
            new Transition((BasicPatternElement)currentPattern,
                            nextRowState,newBindings));
          currentRowState = nextRowState;
        } else if(currentPattern instanceof ComplexPatternElement) {

          // the current pattern is a complex pattern element
          // ..it will probaly be converted into a sequence of states itself.
          currentRowState =  convertComplexPE(
                              currentRowState,
                              (ComplexPatternElement)currentPattern,
                              newBindings);
        } else {

          //we got an unknown kind of pattern
          throw new RuntimeException("Strange looking pattern:"+currentPattern);
        }

      } // for j
        // link the end of the current row to the general end state using
        // an empty transition.
        currentRowState.addTransition(new Transition(null,endState));
    } // for i

    // let's take care of the kleene operator
    switch (kleeneOp){
      case NO_KLEENE_OP:{
        break;
      }
      case KLEENE_QUERY:{
        //allow to skip everything via a null transition
        startState.addTransition(new Transition(null,endState));
        break;
      }
      case KLEENE_PLUS:{

        // allow to return to startState: this has been done above!
        // endState.addTransition(new Transition(null,startState));
        break;
      }
      case KLEENE_STAR:{

        // allow to skip everything via a null transition
        startState.addTransition(new Transition(null,endState));

        // allow to return to startState: this has been done above!
        // endState.addTransition(new Transition(null,startState));
        break;
      }
      default:{
        throw new RuntimeException("Unknown Kleene operator"+kleeneOp);
      }
    } // switch (cpe.getKleeneOp())
    return endState;
  } // convertComplexPE

  /**
    * Converts this FSM from a non-deterministic to a deterministic one by
    * eliminating all the unrestricted transitions.
    */
  public void eliminateVoidTransitions() {

    dStates.clear(); //kalina: replaced from new HashSet()
    LinkedList unmarkedDStates = new LinkedList();
    AbstractSet currentDState = new HashSet();
    //kalina: prefer clear coz faster than init()
    newStates.clear();

    currentDState.add(initialState);
    currentDState = lambdaClosure(currentDState);
    dStates.add(currentDState);
    unmarkedDStates.add(currentDState);

    // create a new state that will take the place the set of states
    // in currentDState
    initialState = new State();
    newStates.put(currentDState, initialState);

    // find out if the new state is a final one
    Iterator innerStatesIter = currentDState.iterator();
    RightHandSide action = null;

    while(innerStatesIter.hasNext()){
      State currentInnerState = (State)innerStatesIter.next();
      if(currentInnerState.isFinal()){
        action = (RightHandSide)currentInnerState.getAction();
        initialState.setAction(action);
        initialState.setFileIndex(currentInnerState.getFileIndex());
        initialState.setPriority(currentInnerState.getPriority());
        break;
      }
    }

    while(!unmarkedDStates.isEmpty()) {
      currentDState = (AbstractSet)unmarkedDStates.removeFirst();
      Iterator insideStatesIter = currentDState.iterator();

      while(insideStatesIter.hasNext()) {
        State innerState = (State)insideStatesIter.next();
        Iterator transIter = innerState.getTransitions().iterator();

        while(transIter.hasNext()) {
          Transition currentTrans = (Transition)transIter.next();

          if(currentTrans.getConstraints() !=null) {
            State target = currentTrans.getTarget();
            AbstractSet newDState = new HashSet();
            newDState.add(target);
            newDState = lambdaClosure(newDState);

            if(!dStates.contains(newDState)) {
              dStates.add(newDState);
              unmarkedDStates.add(newDState);
              State newState = new State();
              newStates.put(newDState, newState);

              //find out if the new state is a final one
              innerStatesIter = newDState.iterator();
              while(innerStatesIter.hasNext()) {
                State currentInnerState = (State)innerStatesIter.next();

                if(currentInnerState.isFinal()) {
                  newState.setAction(
                          (RightHandSide)currentInnerState.getAction());
                  newState.setFileIndex(currentInnerState.getFileIndex());
                  newState.setPriority(currentInnerState.getPriority());
                  break;
                }
              }
            }// if(!dStates.contains(newDState))

            State currentState = (State)newStates.get(currentDState);
            State newState = (State)newStates.get(newDState);
            currentState.addTransition(new Transition(
                                        currentTrans.getConstraints(),
                                        newState,
                                        currentTrans.getBindings()));
          }// if(currentTrans.getConstraints() !=null)

        }// while(transIter.hasNext())

      }// while(insideStatesIter.hasNext())

    }// while(!unmarkedDstates.isEmpty())

    /*
    //find final states
    Iterator allDStatesIter = dStates.iterator();
    while(allDStatesIter.hasNext()){
      currentDState = (AbstractSet) allDStatesIter.next();
      Iterator innerStatesIter = currentDState.iterator();
      while(innerStatesIter.hasNext()){
        State currentInnerState = (State) innerStatesIter.next();
        if(currentInnerState.isFinal()){
          State newState = (State)newStates.get(currentDState);

          newState.setAction(currentInnerState.getAction());
          break;
        }
      }

    }
    */
    allStates = newStates.values();
  }//eliminateVoidTransitions

  /*
    * Computes the lambda-closure (aka epsilon closure) of the given set of
    * states, that is the set of states that are accessible from any of the
    * states in the given set using only unrestricted transitions.
    * @return a set containing all the states accessible from this state via
    * transitions that bear no restrictions.
    */
  private AbstractSet lambdaClosure(AbstractSet s) {
    // the stack/queue used by the algorithm
    LinkedList list = new LinkedList(s);

    // the set to be returned
    AbstractSet lambdaClosure = new HashSet(s);
    State top;
    Iterator transIter;
    Transition currentTransition;
    State currentState;
    while(!list.isEmpty()){
      top = (State)list.removeFirst();
      transIter = top.getTransitions().iterator();

      while(transIter.hasNext()){
        currentTransition = (Transition)transIter.next();

        if(currentTransition.getConstraints() == null){
          currentState = currentTransition.getTarget();
          if(!lambdaClosure.contains(currentState)){
            lambdaClosure.add(currentState);
            list.addFirst(currentState);
          }// if(!lambdaClosure.contains(currentState))

        }// if(currentTransition.getConstraints() == null)

      }
    }
    return lambdaClosure;
  } // lambdaClosure

  /**
    * Returns a GML (Graph Modelling Language) representation of the transition
    * graph of this FSM.
    */
  public String getGML() {

    String res = "graph[ \ndirected 1\n";
///    String nodes = "", edges = "";
    StringBuffer nodes = new StringBuffer(gate.Gate.STRINGBUFFER_SIZE),
                 edges = new StringBuffer(gate.Gate.STRINGBUFFER_SIZE);

    Iterator stateIter = allStates.iterator();
    while (stateIter.hasNext()){
      State currentState = (State)stateIter.next();
      int stateIndex = currentState.getIndex();
/*      nodes += "node[ id " + stateIndex +
               " label \"" + stateIndex;
*/
        nodes.append("node[ id ");
        nodes.append(stateIndex);
        nodes.append(" label \"");
        nodes.append(stateIndex);

             if(currentState.isFinal()){
/*              nodes += ",F";
              nodes += "\\n" + currentState.getAction().shortDesc();
*/
              nodes.append(",F\\n" + currentState.getAction().shortDesc());
             }
///             nodes +=  "\"  ]\n";
             nodes.append("\"  ]\n");
///      edges += currentState.getEdgesGML();
      edges.append(currentState.getEdgesGML());
    }
    res += nodes.toString() + edges.toString() + "]\n";
    return res;
  } // getGML

  /**
    * Returns a textual description of this FSM.
    */
  public String toString(){
    String res = "Starting from:" + initialState.getIndex() + "\n";
    Iterator stateIter = allStates.iterator();
    while (stateIter.hasNext()){
      res += "\n\n" + stateIter.next();
    }
    return res;
  } // toString

  /**
    * The initial state of this FSM.
    */
  private State initialState;

  /**
    * The set of states for this FSM
    */
  private transient Collection allStates =  new HashSet();

  //kalina: added this member here to minimise HashMap allocation
  private transient Map newStates = new HashMap();
  private transient Set dStates = new HashSet();

  private String transducerName;

} // FSM
