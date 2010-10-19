/*
 *  ComplexPatternElement.java - transducer class
 *
 *  Copyright (c) 1995-2010, The University of Sheffield. See the file
 *  COPYRIGHT.txt in the software or at http://gate.ac.uk/gate/COPYRIGHT.txt
 *
 *  This file is part of GATE (see http://gate.ac.uk/), and is free
 *  software, licenced under the GNU Library General Public License,
 *  Version 2, June 1991 (in the distribution as file licence.html,
 *  and also available at http://gate.ac.uk/gate/licence.html).
 *
 *  Minor modifications made by Luc Plamondon, Université de Montréal, 27/11/03:
 *  - migrated original file to the ca.umontreal.iro.rali.gate.jape package.
 *
 *  Hamish Cunningham, 24/07/98
 *
 *  $Id$
 */


package ca.umontreal.iro.rali.gate.jape;

import java.util.*;
import gate.annotation.*;
import gate.util.*;
import gate.*;


/**
  * A pattern element enclosed in round brackets. Has a
  * ConstraintGroups, Kleene operator and binding name.
  */
public class ComplexPatternElement extends PatternElement
implements JapeConstants, java.io.Serializable
{
  /** Debug flag */
  private static final boolean DEBUG = false;

  /** Kleene operator (defaults to none). Other values: KLEENE_STAR (*);
    * KLEENE_PLUS (+); KLEENE_QUERY (?) */
  private int kleeneOp = NO_KLEENE_OP;

  /** Binding name (may be null). */
  private String bindingName = null;

  /** Get binding name. */
  public String getBindingName() { return bindingName; }

  /** Get a list of CPEs that we contain. */
  protected Iterator getCPEs() {
    return constraintGroup.getCPEs();
  } // getCPEs

  /** The recursive definition of what pattern elements make up this one. */
  private ConstraintGroup constraintGroup;

  /** Construction from ConstraintGroup, Kleene operator type and binding
    * name. Kleene types are defined in JapeConstants.
    */
  public ComplexPatternElement(
    ConstraintGroup constraintGroup,
    int kleeneOp,
    String bindingName
  ) {
    this.constraintGroup = constraintGroup;
    this.kleeneOp = kleeneOp;
    this.bindingName = bindingName;
  }

  /** Need cloning for processing of macro references. See comments on
    * <CODE>PatternElement.clone()</CODE>
    */
  public Object clone() {
    ComplexPatternElement newPE = (ComplexPatternElement) super.clone();
    newPE.constraintGroup = (ConstraintGroup) constraintGroup.clone();
    return newPE;
  } // clone

  /** Finish: replace dynamic data structures with Java arrays; called
    * after parsing.
    */
  public void finish() {
    constraintGroup.finish();
  } // finish

  /** Access to the annotations that have been matched. */
  public AnnotationSet getMatchedAnnots() {
    return constraintGroup.getMatchedAnnots();
  }

  /** Reset: clear caches of annotations matched. */
  public void reset() {
    constraintGroup.reset();
    super.reset();
  } // reset

  /** Multilevel rollback of annotation caches. */
  public void rollback(int arity) {
    /*Debug.pr(
      this, "CPE rollback(" + arity + "), mH.size = " +
      matchHistory.size() + Debug.getNl()
    );*/

    // for arity times, pop the arity history stack and
    // ask the CG to rollback how ever many times it succeeded then
    for(int i=0; i<arity; i++) {
      int matchArity = ((Integer) matchHistory.pop()).intValue();
      constraintGroup.rollback(matchArity);
    }
  } // rollback

  /** Does this element match the document at this position? */
  public boolean matches(
    Document doc, int position, MutableInteger newPosition
  ) {
    /*Debug.pr(
      this, "CPE.matches: trying at position " + position + Debug.getNl()
    );*/
    int matchArity = 0; // number of successful applications in this match
    boolean firstTry = constraintGroup.matches(doc, position, newPosition);
    if(firstTry) {
      matchArity++;
      /*Debug.pr(this,
        "CPE.matches: first try succeeded, newPosition = " + newPosition.value
        + Debug.getNl()
      );*/
    }
    int theEndOfTheDocument = doc.getContent().size().intValue();

    if(kleeneOp == NO_KLEENE_OP) {
      if(firstTry) matchHistory.push(new Integer(matchArity));
      return firstTry;
    }
    else if(kleeneOp == KLEENE_QUERY) {
      if(firstTry) matchHistory.push(new Integer(matchArity));
      /* Debug.pr(this, "CPE.matches: true, QUERY rule"); */
      return true;
    }
    else if(kleeneOp == KLEENE_PLUS) {
      if(! firstTry)
        return false; // no cache purge: maybe we're under another * etc.
    }
    else if(kleeneOp == KLEENE_STAR && !firstTry) {
      /*Debug.pr(this,
        "CPE.matches: true, STAR rule, newPos("+newPosition.value+")");*/
      matchHistory.push(new Integer(matchArity));
      return true;
    }

    // we get here if we have either Kleene *, or Kleene +, and a
    // successful first move. now we try it again as many times as it
    // succeeds, store the final match arity and then return true
    while(constraintGroup.matches(doc, newPosition.value, newPosition)) {
      /*Debug.pr(this,
        "CPE.matches: trying while loop, matchArity = " + matchArity);*/
      matchArity++;

      // if we've negated failing constraints, we may match for ever
      if(newPosition.value >= theEndOfTheDocument) // stop at the end!
        break;
    } // while
    matchHistory.push(new Integer(matchArity));
    //Debug.pr(this,
    //         "CPE.matches: true, matchArity(" + matchArity + ") pushed");
    return true;
  } // matches


  /** Create a string representation of the object. */
  public String toString() { return toString(""); }

  /** Create a string representation of the object. */
  public String toString(String pad) {
    String newline = Strings.getNl();

    StringBuffer buf = new StringBuffer(
      pad + "CPE: bindingName(" + bindingName + "); kleeneOp("
    );

    switch(kleeneOp) {
      case NO_KLEENE_OP: buf.append("NO_KLEENE_OP"); break;
      case KLEENE_STAR:  buf.append("KLEENE_STAR");  break;
      case KLEENE_QUERY: buf.append("KLEENE_QUERY"); break;
      case KLEENE_PLUS:  buf.append("KLEENE_PLUS");  break;
      default: break;
    }

    buf.append(
      "); constraintGroup(" + newline +
      constraintGroup.toString(Strings.addPadding(pad, INDENT_PADDING)) +
      newline + pad + ") CPE." + newline
    );

    return buf.toString();
  } // toString
  //needed by FSM

  public int getKleeneOp(){ return kleeneOp; };

  public ConstraintGroup getConstraintGroup(){ return constraintGroup; };

} // class ComplexPatternElement


