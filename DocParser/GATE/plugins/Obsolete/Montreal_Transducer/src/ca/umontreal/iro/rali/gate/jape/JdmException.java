/*
 *  JdmException.java
 *
 *  Copyright (c) 1995-2010, The University of Sheffield. See the file
 *  COPYRIGHT.txt in the software or at http://gate.ac.uk/gate/COPYRIGHT.txt
 *
 *  This file is part of GATE (see http://gate.ac.uk/), and is free
 *  software, licenced under the GNU Library General Public License,
 *  Version 2, June 1991 (in the distribution as file licence.html,
 *  and also available at http://gate.ac.uk/gate/licence.html).
 * 
 *  Kalina Bontcheva, 23/02/2000
 *
 *  Minor modifications made by Luc Plamondon, Université de Montréal, 27/11/03:
 *  - migrated original file to the ca.umontreal.iro.rali.gate.jape package.
 *
 *  $Id$
 *
 *  Description:  This is JDM aimed at repeating the functionality of GDM
 */

package ca.umontreal.iro.rali.gate.jape;

/**
  * THIS CLASS SHOULDN'T BE HERE. Please let's all ignore it, and maybe
  * it will go away.
  */
public class JdmException extends gate.util.GateException {

  /** Debug flag */
  private static final boolean DEBUG = false;

	public JdmException() {
  	super();
  }

  public JdmException(String s) {
  	super(s);
  }
}
