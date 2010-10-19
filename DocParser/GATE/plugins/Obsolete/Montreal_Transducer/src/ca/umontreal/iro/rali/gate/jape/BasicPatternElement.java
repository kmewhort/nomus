/*
 *  BasicPatternElement.java - transducer class
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
 *  Minor modifications made by Luc Plamondon, Université de Montréal, 20/11/03:
 *  - migrated original file to the ca.umontreal.iro.rali.gate.jape package.
 *  - addConstraint: the list of attributes returned by the Constraint object is
 *    a LinkedList, not a FeatureMap.
 *
 *  $Id$
 */


package ca.umontreal.iro.rali.gate.jape;

import java.util.*;
import gate.annotation.*;
import gate.util.*;
import gate.*;


/**
  * A pattern element within curly braces. Has a set of Constraint,
  * which all must be satisfied at whatever position the element is being
  * matched at.
  */
public class BasicPatternElement
extends PatternElement implements JapeConstants, java.io.Serializable
{
  /** Debug flag */
  private static final boolean DEBUG = false;

  /** A set of Constraint. Used during parsing. */
  private ArrayList constraints1;

  /** A set of Constraint. Used during matching. */
  private Constraint[] constraints2;

  /** A map of constraint annot type to constraint. Used during parsing. */
  private HashMap constraintsMap;

  /** Cache of the last position we failed at (-1 when none). */
  private int lastFailurePoint = -1;

  /** The position of the next available annotation of the type required
    * by the first constraint.
    */
  //private MutableInteger nextAvailable = new MutableInteger();

  /** The set of annotations we have matched. */
  private AnnotationSet matchedAnnots;

  /** Access to the annotations that have been matched. */
  public AnnotationSet getMatchedAnnots() { return matchedAnnots; }

  /** Construction. */
  public BasicPatternElement() {
    constraintsMap = new HashMap();
    constraints1 = new ArrayList();
    lastFailurePoint = -1;
    //nextAvailable = new MutableInteger();
    matchedAnnots = new AnnotationSetImpl((Document) null);
  } // construction

  /** Need cloning for processing of macro references. See comments on
    * <CODE>PatternElement.clone()</CODE>
    */
  public Object clone() {
    BasicPatternElement newPE = (BasicPatternElement) super.clone();
    newPE.constraintsMap = (HashMap) constraintsMap.clone();
    newPE.constraints1 = new ArrayList();
    int consLen = constraints1.size();
    for(int i = 0; i < consLen; i++)
      newPE.constraints1.add(
        ((Constraint) constraints1.get(i)).clone()
      );
//    newPE.matchedAnnots = new AnnotationSetImpl((Document) null);
//    newPE.matchedAnnots.addAll(matchedAnnots);
    return newPE;
  } // clone

  /** Add a constraint. Ensures that only one constraint of any given
    * annotation type and negation state exists.
    */
  public void addConstraint(Constraint newConstraint) {
    /* if the constraint is already mapped, put it's attributes on the
     * existing constraint, else add it
     */
    String annotType = newConstraint.getAnnotType();
    Boolean negState = new Boolean(newConstraint.isNegated());
    Pair typeNegKey = new Pair((Object) annotType, (Object) negState);

    Constraint existingConstraint = (Constraint) constraintsMap.get(typeNegKey);
    if(existingConstraint == null) {
      constraintsMap.put(typeNegKey, newConstraint);
      constraints1.add(newConstraint);
    }
    else {
      LinkedList newAttrs = newConstraint.getAttributeSeq();
      LinkedList existingAttrs =
        existingConstraint.getAttributeSeq();
        existingAttrs.addAll(newAttrs);
	//if(newConstraint.isNegated())
        //  existingConstraint.negate();
    }
  } // addConstraint


  /** Finish: replace dynamic data structures with Java arrays; called
    * after parsing.
    */
  public void finish() {
    int j=0;
    constraints2 = new Constraint[constraints1.size()];
    for(Iterator i=constraints1.iterator(); i.hasNext(); ) {
      constraints2[j] = (Constraint) i.next();
      constraints2[j++].finish();
    }
    constraints1 = null;
  } // finish

  /** Reset: clear last failure point and matched annotations list. */
  public void reset() {
    super.reset();
    lastFailurePoint = -1;
    //nextAvailable.value = -1;
    matchedAnnots = new AnnotationSetImpl((Document) null);
  } // reset

  /** Multilevel rollback of the annotation cache. */
  public void rollback(int arity) {
    //Debug.pr(this, "BPE rollback(" + arity + "), matchHistory.size() = " +
    //          matchHistory.size());
    //Debug.nl(this);

    for(int i=0; i<arity; i++) {
      matchedAnnots.removeAll((AnnotationSet) matchHistory.pop());
    }
  } // rollback

  /** Does this element match the document at this position? */
  public boolean matches (
    Document doc, int position, MutableInteger newPosition
  ) {
    final int startingCacheSize = matchedAnnots.size();
    AnnotationSet addedAnnots = new AnnotationSetImpl((Document) null);

    //Debug.pr(this, "BPE.matches: trying at position " + position);
    //Debug.nl(this);
    int rightmostEnd = -1;
    int end = doc.getContent().size().intValue();
    MutableInteger nextAvailable = new MutableInteger();
    int nextAvailOfFirstConstraint = -1;

    for(int len = constraints2.length, i = 0; i < len; i++) {
      Constraint constraint = constraints2[i];
      String annotType = constraint.getAnnotType();
      JdmAttribute[] constraintAttrs = constraint.getAttributeArray();
      MutableBoolean moreToTry = new MutableBoolean();

      if(DEBUG) {
        Out.println(
          "BPE.matches: selectAnn on lFP = " + lastFailurePoint +
          "; max(pos,lfp) = " + Math.max(position, lastFailurePoint) +
          "; annotType = " + annotType + "; attrs = " +
          constraintAttrs.toString() + Strings.getNl()
        );
        for(int j=0; j<constraintAttrs.length; j++)
          Out.println(
            "BPE.matches attr: " + constraintAttrs[j].toString()
          );
      }
      FeatureMap features = Factory.newFeatureMap();
      for(int j = constraintAttrs.length - 1; j >= 0; j--)
        features.put(constraintAttrs[j].getName(), constraintAttrs[j].getValue());
      AnnotationSet match = doc.getAnnotations().get(
        // this loses "April 2" on the frozen tests:
        // Math.max(nextAvailable.value, Math.max(position, lastFailurePoint)),
        annotType,
        features,
        new Long(Math.max(position, lastFailurePoint))  /*,
        nextAvailable,
        moreToTry */
      );
      if(DEBUG) Out.println(
        "BPE.matches: selectAnn returned " + match + ".... moreToTry = " +
        moreToTry.value + "    nextAvailable = " + nextAvailable.value
      );

      // store first constraint's next available
      if(nextAvailOfFirstConstraint == -1)
        nextAvailOfFirstConstraint = nextAvailable.value;

      // if there are no more annotations of this type, then we can
      // say that we failed this BPE and that we tried the whole document
      if(! moreToTry.value) {
        if(match != null)
          throw(new RuntimeException("BPE: no more annots but found one!"));
        lastFailurePoint = end;
        newPosition.value = end;
      }

      // selectNextAnnotation ensures that annotations matched will
      // all start >= position. we also need to ensure that second and
      // subsequent matches start <= to the rightmost end. otherwise
      // BPEs can match non-contiguous annotations, which is not the
      // intent. so we record the rightmostEnd, and reject annotations
      // whose leftmostStart is > this value.
      int matchEnd = -1;
      if(match != null) {
        matchEnd = match.lastNode().getOffset().intValue();
        if(rightmostEnd == -1) { // first time through
          rightmostEnd = matchEnd;
        }
        else if(match.firstNode().getOffset().intValue() >= rightmostEnd) {
          // reject
          lastFailurePoint = matchEnd;
          match = null;
        }
        else { // this one is ok; reset rightmostEnd
          if(rightmostEnd < matchEnd)
            rightmostEnd = matchEnd;
        }
      } // match != null

      // negation
      if(constraint.isNegated()) {
        if(match == null) {
          //Debug.pr(
          //  this, "BPE.matches: negating failed constraint" + Debug.getNl()
          //);
          continue;
        }
        else {
          // Debug.pr(
          //  this, "BPE.matches: negating successful constraint, match = " +
          //  match.toString() + Debug.getNl()
          //);
          lastFailurePoint = matchEnd;
          match = null;
        }
      } // constraint is negated

      if(match == null) { // clean up
        //Debug.pr(this, "BPE.matches: selectNextAnnotation returned null");
        //Debug.nl(this);

        newPosition.value = Math.max(position + 1, nextAvailOfFirstConstraint);
        lastFailurePoint = nextAvailable.value;

        // we clear cached annots added this time, not all: maybe we were
        // applied under *, for example, and failure doesn't mean we should
        // purge the whole cache
        //for(int j = matchedAnnots.size() - 1; j >= startingCacheSize; j--)
        //  matchedAnnots.removeNth(j);
        matchedAnnots.removeAll(addedAnnots);

        //Debug.pr(
        //  this, "BPE.matches: false, newPosition.value(" +
        //  newPosition.value + ")" + Debug.getNl()
        //);
        return false;
      } else {

        //Debug.pr(this,"BPE.matches: match= "+match.toString()+Debug.getNl());
        matchedAnnots.addAll(match);
        addedAnnots.addAll(match);
        newPosition.value = Math.max(newPosition.value, matchEnd);
      }

    } // for each constraint

    // success: store the annots added this time
    matchHistory.push(addedAnnots);

    //Debug.pr(this, "BPE.matches: returning true" + Debug.getNl());
    // under negation we may not have advanced...
    if(newPosition.value == position)
      newPosition.value++;

    return true;
  } // matches

  /** Create a string representation of the object. */
  public String toString() {
    StringBuffer result = new StringBuffer("{");
    Constraint[] constraints = getConstraints();
    for(int i = 0; i<constraints.length; i++){
      result.append(constraints[i].shortDesc() + ",");
    }
    result.setCharAt(result.length() -1, '}');
    return result.toString();
  }

  /** Create a string representation of the object. */
  public String toString(String pad) {
    String newline = Strings.getNl();
    String newPad = Strings.addPadding(pad, INDENT_PADDING);

    StringBuffer buf = new StringBuffer(pad +
      "BPE: lastFailurePoint(" + lastFailurePoint + "); constraints("
    );

    // constraints
    if(constraints1 != null) {
      for(int len = constraints1.size(), i = 0; i < len; i++)
        buf.append(
          newline + ((Constraint) constraints1.get(i)).toString(newPad)
        );
    } else {
      for(int len = constraints2.length, i = 0; i < len; i++)
        buf.append(newline + constraints2[i].toString(newPad));
    }

    // matched annots
    buf.append(
      newline + pad + "matchedAnnots: " + matchedAnnots +
      newline + pad + ") BPE."
    );

    return buf.toString();
  } // toString

  /**
    * Returns a short description.
    */
  public String shortDesc() {
    String res = "";
    if(constraints1 != null) {
      for(int len = constraints1.size(), i = 0; i < len; i++)
        res += ((Constraint) constraints1.get(i)).toString();
    } else {
      for(int len = constraints2.length, i = 0; i < len; i++)
        res += constraints2[i].shortDesc();
    }
    return res;
  }

  public Constraint[] getConstraints(){
    return constraints2;
  }

} // class BasicPatternElement

