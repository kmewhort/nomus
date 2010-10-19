/*
 *  JapeConstants.java
 *
 *  Copyright (c) 1995-2010, The University of Sheffield. See the file
 *  COPYRIGHT.txt in the software or at http://gate.ac.uk/gate/COPYRIGHT.txt
 *
 *  This file is part of GATE (see http://gate.ac.uk/), and is free
 *  software, licenced under the GNU Library General Public License,
 *  Version 2, June 1991 (in the distribution as file licence.html,
 *  and also available at http://gate.ac.uk/gate/licence.html).
 *
 *  Hamish Cunningham, 09/07/98
 *
 *  Minor modifications by Luc Plamondon, Université de Montréal, 20/11/03:
 *  - Added operators to compare attribute values.
 *
 *  $Id$
 */


package ca.umontreal.iro.rali.gate.jape;

import gate.*;
import gate.annotation.*;
import gate.util.*;
import java.util.*;
import java.io.*;

/**
  * Constants interface for the JAPE package.
  */
public interface JapeConstants extends Serializable
{

  /** no Kleene operator */
  public int NO_KLEENE_OP		=  0;

  /** Kleene star (*) */
  public int KLEENE_STAR		=  1;

  /** Kleene plus (+) */
  public int KLEENE_PLUS		=  2;

  /** Kleene query (?) */
  public int KLEENE_QUERY		=  3;


  /** Comparison operator for equality (==) */
  public int EQUAL		        =  0;

  /** Comparison operator for difference (!=) */
  public int NOT_EQUAL 		        =  1;

  /** Comparison operator for greater than (>) */
  public int GREATER		        =  2;

  /** Comparison operator for lesser than (<) */
  public int LESSER 		        =  3;

  /** Comparison operator for greater or equal (>=) */
  public int GREATER_OR_EQUAL		=  4;

  /** Comparison operator for lesser or equal (<=) */
  public int LESSER_OR_EQUAL	        =  5;

  /** Comparison operator for matching a regular expression (=~) */
  public int REGEXP	                =  6;

  /** Comparison operator for not matching a regular expr. (!~) */
  public int NOT_REGEXP	                =  7;


  /** No binding on this element */
  public int NO_BINDING			=  1;

  public int MULTI_SPAN_BINDING		=  2;

  public int SINGLE_SPAN_BINDING	=  3;

  /** Brill-style rule application */
  public int BRILL_STYLE = 1;
  /** Appelt-style rule application */
  public int APPELT_STYLE = 2;
  /** Appelt-shortest-style rule application */
  public int FIRST_STYLE = 3;
  /** The phase finishes on the first match */
  public int ONCE_STYLE = 4;


  /** The default priority of a rule. */
  public int DEFAULT_PRIORITY = -1;

  /** How far to increase indent when padding toString invocations. */
  public int INDENT_PADDING = 4;

} // JapeConstants

