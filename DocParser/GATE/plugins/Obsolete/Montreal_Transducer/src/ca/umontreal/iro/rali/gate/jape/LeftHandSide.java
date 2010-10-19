/*
 *  LeftHandSide.java - transducer class
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

import java.util.Enumeration;
import java.io.Serializable;
import java.util.*;
import gate.annotation.*;
import gate.util.*;
import gate.*;


/**
  * The LHS of a CPSL rule. The pattern part. Has a ConstraintGroup and
  * binding information that associates labels with ComplexPatternElements.
  * Provides the Matcher interface.
  */
public class LeftHandSide implements Matcher, JapeConstants, Serializable
{

  /** Debug flag */
  private static final boolean DEBUG = false;

  /** The constraint group making up this LHS. */
  private ConstraintGroup constraintGroup;

  /** Mapping of binding names to ComplexPatternElements */
  private HashMap bindingTable;

  /** Flag for whether our last match was successful or not. */
  private boolean hasMatched = false;

  /** Construction from a ConstraintGroup */
  public LeftHandSide(ConstraintGroup constraintGroup) {
    this.constraintGroup = constraintGroup;
    bindingTable = new HashMap();
    hasMatched = false;
  } // construction from ConstraintGroup

  /** Add a binding record. */
  public void addBinding(
    String bindingName,
    ComplexPatternElement binding,
    HashSet bindingNameSet,
    boolean macroRef
  ) throws JapeException {
    if(bindingTable.get(bindingName) != null)
      throw new JapeException(
        "LeftHandSide.addBinding: " + bindingName + " already bound"
      );
    bindingTable.put(bindingName, binding);
    bindingNameSet.add(bindingName);

    // if it was a macro ref, we need to recursively set up bindings
    // in any CPEs that this one contains
    if(macroRef) {
      for(Iterator i = binding.getCPEs(); i.hasNext(); ) {
        binding = (ComplexPatternElement) i.next();
        bindingName = binding.getBindingName();
        if(bindingName == null) // not all CPEs have binding names
          continue;
        if(bindingTable.get(bindingName) != null)
          throw new JapeException(
            "LeftHandSide.addBinding: " + bindingName + " already bound"
          );
        bindingTable.put(bindingName, binding);
        bindingNameSet.add(bindingName);
      } // for each binding
    } // macroRef

  } // addBinding

  /** Finish: replace dynamic data structures with Java arrays; called
    * after parsing.
    */
  public void finish() {
    constraintGroup.finish();
  } // finish

  /** Get annotations via a binding name. */
  public AnnotationSet getBoundAnnots(String bindingName) {
    ComplexPatternElement pat =
      (ComplexPatternElement) bindingTable.get(bindingName);
    if(pat == null) return null;
    return pat.getMatchedAnnots();
  } // getBoundAnnots

  /** For debugging only.
    * Return a set of all annotations matched by the LHS during the
    * last call to matches. (May be null.)
    */
  AnnotationSet getMatchedAnnots() {
    return constraintGroup.getMatchedAnnots();
  } // getMatchedAnnots

  /** Clear the matched annotations cached in pattern elements. */
  public void reset() {
    constraintGroup.reset();
    hasMatched = false;
  } // reset

  /** Was the last match successful? */
  public boolean hasMatched() { return hasMatched; }

  /** Does the LHS match the document at this position? */
  public boolean matches(
    Document doc, int position, MutableInteger newPosition
  ) {
     boolean status = constraintGroup.matches(doc, position, newPosition);
     //Debug.pr(this, "LHS: status(" + status + "); this: " + this.toString());

     if(! status) { // purge caches of matched annotations
       constraintGroup.reset();
       hasMatched = false;
     } else {
       hasMatched = true;
     }
     return status;
  }  // matches

  /** Create a string representation of the object. */
  public String toString() { return toString(""); }

  /** Create a string representation of the object. */
  public String toString(String pad) {
    String newline = Strings.getNl();
    String newPad = Strings.addPadding(pad, INDENT_PADDING);

    StringBuffer buf = new StringBuffer(pad +
      "LHS: hasMatched(" + hasMatched + "); constraintGroup(" + newline +
      constraintGroup.toString(newPad) + newline + pad +
      "); bindingTable(" + newline + pad
    );

    for(Iterator i = bindingTable.keySet().iterator(); i.hasNext(); ) {
      String bName = ((String) i.next());
      ComplexPatternElement cpe = ((ComplexPatternElement)
                                        bindingTable.get(bName));
      buf.append(
        pad + "bT.bn(" + bName + "), cpe.bn(" + cpe.getBindingName() + ")"
      );
    }

    buf.append(newline + pad + ") LHS." + newline);

    return buf.toString();
  } // toString

  /** Get the constraint group */
  public ConstraintGroup getConstraintGroup(){
    return constraintGroup;
  }

} // class LeftHandSide


