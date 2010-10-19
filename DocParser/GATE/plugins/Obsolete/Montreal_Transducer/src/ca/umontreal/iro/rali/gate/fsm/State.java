/*
 *  State.java
 *
 *  Copyright (c) 1995-2010, The University of Sheffield. See the file
 *  COPYRIGHT.txt in the software or at http://gate.ac.uk/gate/COPYRIGHT.txt
 *
 *  This file is part of GATE (see http://gate.ac.uk/), and is free
 *  software, licenced under the GNU Library General Public License,
 *  Version 2, June 1991 (in the distribution as file licence.html,
 *  and also available at http://gate.ac.uk/gate/licence.html).
 *
 *  Valentin Tablan, 11/Apr/2000
 *
 *  Minor modifications made by Luc Plamondon, Université de Montréal, 27/11/03:
 *  - migrated original file from gate.fsm to 
 *    ca.umontreal.iro.rali.gate.fsm package.
 *
 *  $Id$
 */

package ca.umontreal.iro.rali.gate.fsm;

import java.util.*;
import ca.umontreal.iro.rali.gate.jape.*;
import gate.util.SimpleArraySet;

/**
 * This class implements a Finite State Machine state.
 *
 */
public class State implements JapeConstants {

  /** Debug flag
   */
  private static final boolean DEBUG = false;

  /**
   * Build a new state.
   *
   * @param owner the FSM that owns this state.
   */
  public State() {
    myIndex = State.index++;
    isFinal = false;
  }

  /**
   * Reports if this state is a final one.
   * Note: A state has an associated action if and only if it is final.
   */
  public boolean isFinal() {
    return isFinal;
  }

  /**
   * Gets the set of transitions for this state.
   *
   * @return a Set contining objects of type gate.fsm.Transition
   */
// >>> DAM, was Set
/*
  public Set getTransitions() {
    return transitions;
  }
*/
// >>> DAM, TransArray optimization
  public SimpleArraySet getTransitions() {
    return transitions;
  }
// >>> DAM, end
  /** Sets the action associated to this FINAL state. An action is actually
   * a gate.jape.RightHandSide object.
   * NOTE: only a final state has an associated action so after a call to this
   * method this state will be a final one.
   */
  protected void setAction(RightHandSide rhs) {
    action = rhs;
    isFinal = (action != null);
  }

  /** Sets the value for fileIndex. File index is the index in the jape
   * definition file of the rule that contains as right hand side the action
   * associated to this state. This value is only intended for final states.
   */
  protected void setFileIndex(int i) { fileIndex = i; }

  /** Sets the value for priority. Priority is the priority in the jape
   * definition file of the rule that contains as right hand side the action
   * associated to this state. This value is only intended for final states.
   */
  protected void setPriority(int i) { priority = i; }

  /**
   * Gets the action associated to this state.
   *
   * @return a RightHandSide object
   */
  public RightHandSide getAction() {
    return action;
  }

  /**
   * Returns the index in the definition file of the rule that generated this
   * state.
   * The value for fileIndex is correct only on final states!
   */
  int getFileIndex() { return fileIndex; }

  /**
   * Returns the priority in the definition file of the rule that generated
   * this state.
   * This value is correct only on final states!
   */
  int getPriority() { return priority; }

  /**
   * Adds a new transition to the list of outgoing transitions for this state.
   *
   * @param transition the transition to be added
   */
  public void addTransition(Transition transition) {
    transitions.add(transition);
  } // addTransition

  /**
   * Gets the index of this state. Each state has a unique index (a int value).
   * This value is not actually used by any of the algorithms. It is useful only
   * as a way of refering to states in string representations so it is used by
   * toString and GML related methods.
   *
   * @return the index associated to this state
   */
  protected int getIndex() {
    return myIndex;
  }// getIndex

  /**
   * Returns a GML (graph modelling language) representation for the edges
   * corresponding to transitions departing from this state in the
   * transition graph of the FSM to which this state belongs
   *
   * @return a string value contining the GML text
   */
  public String getEdgesGML() {
///    String res = "";
    StringBuffer res = new StringBuffer(gate.Gate.STRINGBUFFER_SIZE);

    Iterator transIter = transitions.iterator();
    BasicPatternElement bpe;

    while(transIter.hasNext()) {
      Transition currentTrans = (Transition)transIter.next();
/*      res += "edge [ source " + myIndex +
             " target " + currentTrans.getTarget().getIndex() +
             " label \"" + currentTrans.shortDesc() + ":";
*/
        res.append("edge [ source ");
        res.append(myIndex);
        res.append(" target ");
        res.append(currentTrans.getTarget().getIndex());
        res.append(" label \"");
        res.append(currentTrans.shortDesc());
        res.append(":");

             bpe = currentTrans.getConstraints();
             if(bpe == null) ///res += "null";
                res.append("null");
             else ///res += bpe.shortDesc();
                res.append(bpe.shortDesc());
///             res += " :" + currentTrans.getBindings() +              "\" ]\n";
             res.append(" :");
             res.append(currentTrans.getBindings());
             res.append("\" ]\n");
    }
    return res.toString();
  } // getEdgesGML

  /**
   * Returns a textual description of this state
   *
   * @return a String value.
   */
  public String toString() {
///    String res = "State " + myIndex;
    StringBuffer res = new StringBuffer(gate.Gate.STRINGBUFFER_SIZE);

    if(isFinal()) ///res += "\nFinal!";
        res.append("\nFinal!");

    ///res += "\nTransitions:\n";
    res.append("\nTransitions:\n");

    Iterator transIter = transitions.iterator();
    while(transIter.hasNext()){
      ///res += transIter.next().toString();
      res.append(transIter.next().toString());
    }
    return res.toString();
  }


  /**
   * A set of objects of type gata.fsm.Transition representing the outgoing
   * transitions.
   */
// >>> DAM was
/*
  private Set transitions = new HashSet();
*/
// >>> DAM, TransArray optimization
  private SimpleArraySet transitions = new SimpleArraySet();
// >>> DAM, end

  /**
   * Is this state a final one?
   */
  protected boolean isFinal = false;

  /**
   * The right hand side associated to the rule for which this state recognizes
   * the lhs.
   */
  protected RightHandSide action = null;

  /**
   * The unique index of this state.
   */
  protected int myIndex;

  /**
   * The class data member used for generating unique indices for State
   * instances.
   */
  protected static int index = 0;

  /**
   * The index in the definition file of the rule that was used for creating
   * this state.
   * NOTE: this member is consistent only for FINAL STATES!
   */
  protected int fileIndex = 0;

  /**
   * The priority of the rule from which this state derived.
   *
   */
  protected int priority = -1;

} // State
