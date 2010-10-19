/*
 *  Rule.java - transducer class
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
 *  - migrated original file from gate.jape to 
 *    ca.umontreal.iro.rali.gate.jape package.
 *
 *  $Id$
 */


package ca.umontreal.iro.rali.gate.jape;

import java.util.*;
import gate.annotation.*;
import gate.event.*;
import gate.util.*;
import gate.*;

/**
  * A CPSL rule. Has an LHS, RHS and a name, and a priority.
  */
public class Rule extends Transducer
implements JapeConstants, java.io.Serializable
{
  /** Debug flag */
  private static final boolean DEBUG = false;

  /** Construction */
  public Rule(
    String name, int position, int priority,
    LeftHandSide lhs, RightHandSide rhs
  ) {
    this.name = name;
    this.position = position;
    this.priority = priority;
    this.lhs = lhs;
    this.rhs = rhs;
  } // Construction

  /** The LHS or pattern of the rule. */
  private LeftHandSide lhs;

  /** The RHS or action of the rule. */
  private RightHandSide rhs;

  /** The priority of the rule. */
  private int priority;

  /** Get the rule priority. */
  public int getPriority() { return priority; }

  /** The rule's position in sequence (e.g. order in file). */
  private int position;

  /** Get the rule's position in sequence (e.g. order in file). */
  public int getPosition() { return position; }

  /** If we're pending (have matched), get the position we want to fire in,
    * else -1.
    */
  public int pending() {
    return pendingPosition;
  } // pending

  /** If we matched but didn't fire yet, this is our pending position. */
  private int pendingPosition = -1;

  /** Flag for end of document during getNextMatch. */
  private boolean weFinished = false;

  /** Have we hit the end of the document without matching? */
  public boolean finished() {
    return weFinished;
  } // finished

  /** Finish: replace dynamic data structures with Java arrays; called
    * after parsing. WARNING:
    * bad choice of names: this is not related to the weFinished
    * member or the finished method!
    */
  public void finish() {
    lhs.finish();
  } // finish

  /** If another match at or beyond <CODE>position</CODE> is possible return
    * the position we want to fire in, else -1.
    */
  public int getNextMatch(Document doc, int position, int end) {
    MutableInteger newPosition = new MutableInteger();
    newPosition.value = position;
    while(position < end) {

      if(matches(doc, position, newPosition)) {
        pendingPosition = getStartPosition();
        return pendingPosition;
      }
      position = Math.max(position + 1, newPosition.value);

    } // while position not final

    weFinished = true;
    return -1;
  } // getNextMatch

  /** Return the ending position of a match. This is the rightmost span
    * end of the matched annotations.
    */
  public int getEndPosition() {
    return lhs.getMatchedAnnots().lastNode().getOffset().intValue();
  }

  /** Return the starting position of a match. This is the leftmost span
    * start of the matched annotations.
    */
  public int getStartPosition() {
    return lhs.getMatchedAnnots().firstNode().getOffset().intValue();
  }

  /** Does this element match the document at this position? */
  public boolean matches(
    Document doc, int position, MutableInteger newPosition
  ) {
    if(DEBUG) Out.println("trying rule " + name + " at " + position);
    return lhs.matches(doc, position, newPosition);
  } // matches

  /** Apply the RHS of this rule (LHS must have been matched first). */
  public void transduce(Document doc, AnnotationSet inputAS,
                        AnnotationSet outputAS) throws JapeException {
    // the righthand side does the transduction, using bindings from lhs */
    if(DEBUG) Out.println("applying rule " + name);
//    rhs.transduce(doc);
    /*Debug.pr(
      this, "Rule.transduce: annotations after transduction: " +
      doc.selectAnnotations("Name", new FeatureMap()).toString() +
      Debug.getNl()
    );*/

    // clear the caches of matched annotations in the LHS
    reset();
    //Debug.pr(this, "LHS after reset: " + lhs.toString());

  } // transduce

  /** Clear away the results of a match. */
  public void reset() {
     if(weFinished) // no annotations cached
       weFinished = false;
     else
       lhs.reset();
     pendingPosition = -1;
  }

  /** For debugging. */
  // public String getName() { return name; }

  /** Clean up (delete action class files, for e.g.). */
  public void cleanUp() {
    rhs.cleanUp();
  } // cleanUp


  /** Create a string representation of the object. */
  public String toString() { return toString(""); }

  /** Create a string representation of the object. */
  public String toString(String pad) {
    String newline = Strings.getNl();
    String newPad = Strings.addPadding(pad, INDENT_PADDING);

    StringBuffer buf = new StringBuffer(
      pad + "Rule: name(" + name + "); position(" + position + "); priority(" +
      priority + "); pendingPosition(" + pendingPosition + "); " +
      "weFinished(" + weFinished + "); lhs(" + newline +
      lhs.toString(newPad) + newline + pad + "); rhs(" + newline +
      rhs.toString(newPad) + newline + pad + ");"
    );

    buf.append(newline + pad + ") Rule." + newline);

    return buf.toString();
  } // toString

  //needed by FSM
  public LeftHandSide getLHS(){
    return lhs;
  }
  public RightHandSide getRHS(){
    return rhs;
  }

  //StatusReporter VOID Implementation
  public void addStatusListener(StatusListener listener){}
  public void removeStatusListener(StatusListener listener){}

  //ProcessProgressReporter VOID implementation
  public void addProcessProgressListener(ProgressListener listener){}
  public void removeProcessProgressListener(ProgressListener listener){}
  //ProcessProgressReporter implementation ends here

} // class Rule


