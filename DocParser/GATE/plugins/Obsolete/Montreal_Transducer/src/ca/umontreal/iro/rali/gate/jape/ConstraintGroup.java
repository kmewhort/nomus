/*
 *  ConstraintGroup.java - transducer class
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
  * A sequence of conjunctions of PatternElement that form a
  * disjunction.
  */
public class ConstraintGroup
extends PatternElement implements JapeConstants, java.io.Serializable
{
  /** Debug flag */
  private static final boolean DEBUG = false;

  /** Anonymous constructor. */
  public ConstraintGroup() {
    patternElementDisjunction1 = new ArrayList();
    currentConjunction = new ArrayList();
    patternElementDisjunction1.add(currentConjunction);
  } // Anonymous constructor

  /** Need cloning for processing of macro references. See comments on
    * <CODE>PatternElement.clone()</CODE>
    */
  public Object clone() {
    ConstraintGroup newPE = (ConstraintGroup) super.clone();

    // created by createDisjunction
    newPE.currentConjunction = null;

    newPE.patternElementDisjunction1 = new ArrayList();
    // for each (conjunction) member of the pattern element discjunction
    for(
      Iterator disjunction = patternElementDisjunction1.iterator();
      disjunction.hasNext();

    ) {

      newPE.createDisjunction();
      // for each pattern element making up this conjunction
      for(
        Iterator conjunction = ((ArrayList) disjunction.next()).iterator();
        conjunction.hasNext();

      ) {
        PatternElement pat = (PatternElement) conjunction.next();

        newPE.addPatternElement((PatternElement) pat.clone());
      } // for each element of the conjunction
    } // for each conjunction (element of the disjunction)

    return newPE;
  } // clone

  /** An array of arrays that represent PatternElement conjunctions
    * during parsing of the .jape. Each conjunction is
    * considered as being disjunct with the next. (I.e. they are
    * or'd, in the same way as expressions around "||" in C and
    * Java.) Set during parsing; replaced by finish().
    */
  private ArrayList patternElementDisjunction1;

  /** The pattern element disjunction for transduction - Java arrays. */
  private PatternElement[][] patternElementDisjunction2;

  /** An array of PatternElements making up a conjunction. It is a member of
    * patternElementDisjunction. This is the one we're adding to
    * at present. Used during parsing, not matching.
    */
  private ArrayList currentConjunction;

  /** Make a new disjunction at this point. */
  public void createDisjunction() {
    currentConjunction = new ArrayList();
    patternElementDisjunction1.add(currentConjunction);
  } // createDisjunction

  /** Add an element to the current conjunction. */
  public void addPatternElement(PatternElement pe) {
    currentConjunction.add(pe);
  } // addPatternElement

  /** Get an list of CPEs that we contain. */
  protected Iterator getCPEs() {
    ArrayList cpes = new ArrayList();

    // for each (conjunction) member of the pattern element discjunction
    for(
      Iterator disjunction = patternElementDisjunction1.iterator();
      disjunction.hasNext();
    ) {
      // for each pattern element making up this conjunction
      for(
        Iterator conjunction = ((ArrayList) disjunction.next()).iterator();
        conjunction.hasNext();
      ) {
        PatternElement pat = (PatternElement) conjunction.next();

        Iterator i = null;
        if(pat instanceof ComplexPatternElement) {
          cpes.add(pat);
          i = ((ComplexPatternElement) pat).getCPEs();
        }
        else if(pat instanceof ConstraintGroup)
          i = ((ConstraintGroup) pat).getCPEs();

        if(i != null)
          for( ; i.hasNext(); )
            cpes.add(i.next());
      } // for each element of the conjunction
    } // for each conjunction (element of the disjunction)

    return cpes.iterator();
  } // getCPEs

  /** Finish: replace dynamic data structures with Java arrays; called
    * after parsing.
    */
  public void finish() {

    // index into patternElementDisjunction2
    int i = 0;

    // index into the conjunctions (second dimension of pED2)
    int j = 0;

    patternElementDisjunction2 =
      new PatternElement[patternElementDisjunction1.size()][];

    // for each (conjunction) member of the pattern element discjunction
    for(
      Iterator disjuncIter = patternElementDisjunction1.iterator();
      disjuncIter.hasNext();
      i++
    ) {
      ArrayList conjunction = (ArrayList) disjuncIter.next();
      patternElementDisjunction2[i] = new PatternElement[conjunction.size()];
      j = 0;

      // for each pattern element making up this conjunction
      for(
        Iterator conjIter = conjunction.iterator();
        conjIter.hasNext();
        j++
      ) {
        patternElementDisjunction2[i][j] = (PatternElement) conjIter.next();
        patternElementDisjunction2[i][j].finish();
      } // loop on conjunction

    } // loop on patternElementDisjunction1

    patternElementDisjunction1 = null;
  } // finish

  /** Access to the annotations that have been matched by this group. */
  public AnnotationSet getMatchedAnnots() {
    AnnotationSet matchedAnnots = new AnnotationSetImpl((Document) null);
    int pEDLen = patternElementDisjunction2.length;

    // for each (conjunction) member of the pattern element disjunction
    for(int i = 0; i < pEDLen; i++) {
      int conjLen = patternElementDisjunction2[i].length;

      // for each pattern element making up this conjunction
      for(int j = 0; j < conjLen; j++) {
        PatternElement pat = patternElementDisjunction2[i][j];
        AnnotationSet patMatchedAnnots = pat.getMatchedAnnots();
        if(patMatchedAnnots != null)
          matchedAnnots.addAll(pat.getMatchedAnnots());
      } // for each element of the conjunction

    } // for each conjunction (element of the disjunction)

    return matchedAnnots;
  } // getMatchedAnnots


  /** Clear all the annotations that have been matched by this group. */
  public void reset() {
    // Debug.pr(this, "CG reset, matchHistory.size() = " + matchHistory.size());
    int pEDLen = patternElementDisjunction2.length;

    // for each (conjunction) member of the pattern element disjunction
    for(int i = 0; i < pEDLen; i++) {
      int conjLen = patternElementDisjunction2[i].length;

      // for each pattern element making up this conjunction
      for(int j = 0; j < conjLen; j++)
        patternElementDisjunction2[i][j].reset();
    }

    super.reset(); // should be redundant: there for if PE.reset changes
  } // reset

  /** Multilevel rollback of annot caches etc. */
  public void rollback(int arity) {
    // Debug.pr(this, "CG rollback(" + arity + "), matchHistory.size() = " +
    //                   matchHistory.size());
    for(int i=0; i<arity; i++) {
      PatternElement[] conjunction = (PatternElement[]) matchHistory.pop();
      int conjLen = conjunction.length;
      for(int j = 0; j < conjLen; j++)
        conjunction[j].rollback(1);
    }
  } // rollback


  /** Does this element match the document at this position? */
  public boolean matches(
    Document doc, int position, MutableInteger newPosition
  ) {
    // if a whole conjunction matches, we set newPosition to the max of
    // rightmost advance of all the composite elements that matched, and
    // position.
    int rightmostAdvance = position;

    // when we fail the whole disjunction, we set newPosition to the max of
    // leftmost failure point, and position
    int leftmostFailurePoint = Integer.MAX_VALUE;

    // outerLoop:
    // for each conjunction
    //   for each element in the conjunction
    //     if it fails continue outerLoop;
    //   return true;
    // return false;

    // for each member of the disjunctions array
    int savedPosition = position;
    int pEDLen = patternElementDisjunction2.length;
    outerLoop:
    for(int i = 0; i < pEDLen; i++) {
      int conjLen = patternElementDisjunction2[i].length;
      position = savedPosition;
      rightmostAdvance = position;

      // for each pattern element making up this conjunction
      for(int j = 0; j < conjLen; j++) {
        PatternElement pat = patternElementDisjunction2[i][j];

        if(! pat.matches(doc, position, newPosition)) {
          // reset the last failure point to the furthest we got so far
          leftmostFailurePoint =
            Math.min(leftmostFailurePoint, newPosition.value);

          // rollback matches done in the previous elements of this conjunction
          for(int k = j - 1; k >= 0; k--)
            patternElementDisjunction2[i][k].rollback(1);

          // try the next conjunction
          continue outerLoop;
        }

        // reset our advance point to the furthest so far
        position = rightmostAdvance =
          Math.max(rightmostAdvance, newPosition.value);

      } // for each element of the conjunction

      // a whole conjunction matched: record advance and which conj succeeded
      newPosition.value = rightmostAdvance;
      matchHistory.push(patternElementDisjunction2[i]);
      //Debug.pr(this, "CG matches: pushing");
      return true;

    } // for each conjunction (element of the disjunction)

    // we reached the end of the disjunction without matching a
    // whole conjunction
    if(leftmostFailurePoint == Integer.MAX_VALUE)
      leftmostFailurePoint = position + 1;
    newPosition.value = Math.max(position + 1, leftmostFailurePoint);
    return false; // annot caches have been rolled back already in inner loop
  } // matches


  /** Create a string representation of the object. */
  public String toString() { return toString(""); }

  /** Create a string representation of the object. */
  public String toString(String pad) {
    String newline = Strings.getNl();

    StringBuffer buf =
      new StringBuffer(pad + "CG: disjunction(" + newline);
    String newPad = Strings.addPadding(pad, INDENT_PADDING);

    boolean firstTime = true;

    if(patternElementDisjunction1 != null) { // before finish()
      // for each (conjunction) member of the pattern element discjunction
      for(
        Iterator disjunction = patternElementDisjunction1.iterator();
        disjunction.hasNext();
      ) {
        if(firstTime) firstTime = false;
        else buf.append(newline + pad + "|" + newline);

        // for each pattern element making up this conjunction
        for(
          Iterator conjunction = ((ArrayList) disjunction.next()).iterator();
          conjunction.hasNext();
        ) {
          buf.append(
            ((PatternElement) conjunction.next()).toString(newPad) + newline
          );
        } // for each element of the conjunction
      } // for each conjunction (element of the disjunction)

    } else { // after finish
      int pEDLen = patternElementDisjunction2.length;
      if(firstTime) firstTime = false;
      else buf.append(newline + pad + "|" + newline);

      for(int i = 0; i < pEDLen; i++) {
        int conjLen = patternElementDisjunction2[i].length;
        // for each pattern element making up this conjunction
        for(int j = 0; j < conjLen; j++)
          buf.append(
            patternElementDisjunction2[i][j].toString(newPad) + newline
          );
      }
    }

    buf.append(pad + ") CG." + newline);

    return buf.toString();
  } // toString


  //needed by FSM
  public PatternElement[][] getPatternElementDisjunction(){
    return patternElementDisjunction2;
  }

} // class ConstraintGroup


