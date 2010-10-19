/*
 *  Matcher.java - transducer class
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
  * Interface to be implemented by classes providing matching on documents,
  * e.g. PatternElement and LeftHandSide.
  */
public interface Matcher extends java.io.Serializable
{
  /** Does this element match the document at this position? */
  abstract public boolean matches(
    Document doc, int position, MutableInteger newPosition
  );

  /** Reset: clear annotation caches etc. */
  abstract public void reset();

  /** Finish: replace dynamic data structures with Java arrays; called
    * after parsing.
    */
  abstract public void finish();

} // class Matcher

