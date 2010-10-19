/* 
 *  PatternElement.java - transducer class
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
import gate.util.*;
import gate.*;


/**
  * Superclass of the various types of pattern element, and of
  * ConstraintGroup. Inherits from Matcher, providing matches and reset.
  * Provides access to the annotations that are cached by subclasses, and
  * multilevel rollback of those caches. Stores the match history.
  */
abstract public class PatternElement implements Cloneable, Matcher,
		      JapeConstants, java.io.Serializable
{
  /** Debug flag */
  private static final boolean DEBUG = false;

  /** Match history stack, for use in rollback. In BasicPatternElements
    * the objects on the stack are Integers giving the number of annots that
    * were cached at that point in the history. In ComplexPatternElements
    * the objects are Integers giving the number of times the component
    * ConstraintGroup was successfully matched. In ConstraintGroups the
    * elements are arrays representing conjunctions of PatternElement that
    * succeeded at that point in the history.
    */
  protected Stack matchHistory;

  /** Anonymous construction. */
  public PatternElement() {
    matchHistory = new Stack();
  } // Anonymous constructor.

  /** Cloning for processing of macro references. Note that it doesn't
    * really clone the match history, just set it to a new Stack. This is
    * because a) JGL doesn't have real clone methods and b) we don't
    * actually need it anywhere but during parsing the .jape, where there
    * is no match history yet.
    */
  public Object clone() {
    try {
      PatternElement newPE = (PatternElement) super.clone();
      newPE.matchHistory = new Stack();
      return newPE;
    } catch(CloneNotSupportedException e) {
      throw(new InternalError(e.toString()));
    }
  } // clone

  /** Access to the annotations that have been matched. */
  abstract public AnnotationSet getMatchedAnnots();

  /** Multilevel rollback of annotation caches. */
  abstract public void rollback(int arity);

  /** Reset: clear annotation caches etc. Most of the behaviour of
    * this method is the responsibility of subclasses.
    */
  public void reset() {
    matchHistory = new Stack();
  } // reset

  /** Create a string representation of the object with padding. */
  abstract public String toString(String pad);

} // class PatternElement
