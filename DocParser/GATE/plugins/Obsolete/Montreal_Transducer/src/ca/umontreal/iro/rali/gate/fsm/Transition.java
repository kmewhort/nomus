/*
 *  Transition.java
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

import ca.umontreal.iro.rali.gate.jape.*;

import java.util.*;
import java.io.*;

/**
  * This class implements a Finite State Machine transition.
  * A transition is owned by a gate.fsm.State object and contains set of
  * restrictions and a reference to the next state that will be accessed after
  * consuming a set of input symbols according to the restrictions.
  * A transition can also hold information about the label that should be bound
  * to the symbols (annotations) consumed during the state transition.
  */
// >>> DAM
/*
public class Transition implements Serializable {
*/
// >>> DAM, TransArray optimzation, now implements the Comparable interface
public class Transition implements Serializable, Comparable {
// >>> DAM, end

  /** Debug flag */
  private static final boolean DEBUG = false;

  /**
    * Default constructor. Creates a new transition with a new unique index.
    * This constructor should be called by all other constructors.
    */
  public Transition() {
    myIndex = Transition.index++;
  }

  /**
    * Creates a new transition using the given set of constraints and target
    * state.
    * @param constraints the set on constraints associated to this transition
    * @param state the target state of this transition
    */
  public Transition(BasicPatternElement constraints, State state) {
    this();
    this.constraints = constraints;
    target = state;
    bindings = new LinkedList();
  }

  /**
    * Ctreates a new transition from a set of constraints, a target state and a
    * list of labels to be bound with the recognized input symbols
    * (aka annotations).
    */
  public Transition(BasicPatternElement constraints, State state,
                    LinkedList bindings) {
    this();
    this.constraints = constraints;
    target = state;
    this.bindings = bindings;
  }

  /**
    * Gets the target state of this transition
    * @return an object of type gate.fsm.State
    */
  public State getTarget(){ return target; }

  /**
    * Gets the constraints associated to this transition
    */
  public BasicPatternElement getConstraints(){ return constraints; }

  /**
    * Returns a textual desciption of this transition.
    * @return a String
    */
  public String toString(){
    String res = "If: " + constraints + " then ->: " + target.getIndex();
    return res;
  }

  /**
    * Returns a shorter description that toSting().
    * Actually, it returns the unique index in String form.
    */
  public String shortDesc(){
    String res = "" + myIndex;
    return res;
  }

  /**
    *  Returns the list of bindings associated to this transition
    */
  public LinkedList getBindings(){ return bindings; }

  /**
    * The constraints on this transition.
    */
  private BasicPatternElement constraints;

  /**
    * The state this transition leads to
    */
  private State target;

  /**
    * A list with all the labels associated to the annotations recognized by
    * this transition.
    * We need to use the actual object and not the interface (java.util.List)
    * because we need this object to be cloneable
    */
  private LinkedList bindings;

  /** The unique index of this transition. This value is not used by any of
    * the algorithms. It is only provided as a convenient method of identifying
    * the transitions in textual representations (toString() and GML related
    * methods)
    */
  private int myIndex;

  /** Static member used for generating unique IDs for the objects of type
    * Transition*/
  private static int index = 0;

// >>> DAM, TransArray optimzation, now implements the Comparable interface
  public int compareTo(Object o)
  throws ClassCastException
  {
    if (!(o instanceof Transition)) throw new ClassCastException("gate.frm.Transition(compareTo)");
    return myIndex - ((Transition)o).myIndex;
  }
// >>> DAM, end
} // Transition
