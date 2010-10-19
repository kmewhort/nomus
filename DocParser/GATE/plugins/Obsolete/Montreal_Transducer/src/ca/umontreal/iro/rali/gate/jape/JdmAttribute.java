/*
 *  JdmAttribute.java
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
 *  Modifications by Luc Plamondon, Université de Montréal, 20/11/03:
 *  -Added a third field to hold a equality/difference/etc. operator read
 *   from the grammar.
 *
 *  $Id$
 *
 *  Description:  This is JDM aimed at repeating the functionality of GDM
 */

package ca.umontreal.iro.rali.gate.jape;

import gate.*;
import gate.util.*;
import java.io.Serializable;

/**
  * THIS CLASS SHOULDN'T BE HERE. Please let's all ignore it, and maybe
  * it will go away.
  * <P>
  * Implements the TIPSTER and GDM API for attributes.
  * Test code in <code>testAttributes</code> class. <P>
  * The JdmAttribute class would accept all java serialisable classes, all
  * jdm classes and also all user-defined classes provided they implement
  * the Serializable interface. This restriction is necessary  since Jdm
  * uses Java serialisation to ensure object persistency. However, making
  * classes serialisable is usually quite straightforward. <P>
  * @author Kalina Bontcheva
*/
public class JdmAttribute implements Serializable {

  /** Debug flag */
  private static final boolean DEBUG = false;

  private String name;
  private Object value;
  private int operator = JapeConstants.EQUAL;

  protected JdmAttribute() {
  }

  /** Create a tuple for an attribute: (name, value, operator). 
    * Operator is JapeConstants.EQUAL by default.
    * <p> This constructor should be called when the attribute is part of an annotation in the
    * document.  When the attribute is part of a constraint in a grammar, use
    * JdmAttribute(name, value, operator).
    * @param name name of the attribute
    * @param value value of the attribute
    */
  public JdmAttribute(String name, Object value) {
    this.name = name; this.value = value;
  }

  /** Create a tuple that describes an attribute: (name, value, operator).
    * <p> This constructor should be called when the attribute is part of a constraint in a grammar.
    * When the attribute is part of an annotation in the document, use JdmAttribute(name, value);
    * @param name name of the attribute
    * @param value value of the attribute
    * @param operator see JapeConstants (EQUAL, NOT_EQUAL...)
    */
  public JdmAttribute(String name, Object value, int operator) {
    this.name = name; this.value = value; this.operator = operator;
  }


  /** throws JdmException when the value isn't one of the types we know
    * how to store, i.e., a serialisable or Jdm class.
    */
  public JdmAttribute(JdmAttribute jdmAttr) {
    String name = jdmAttr.getName();
    Object value = jdmAttr.getValue();
    int operator = jdmAttr.getOperator();
  }

  public String getName() {
    return name;
  }

  public Object getValue() {
    return value;
  }

  public String getValueType() {
    return value.getClass().getName();
  }

  public int getOperator() {
    return operator;
  }

  public boolean equals(Object obj) {
    JdmAttribute a = (JdmAttribute) obj;
    return a.getName().equals(name) && a.getValue().equals(value) && a.getOperator() == operator;
  }

  public String toString() {
     return "JdmAttr: name=" + name + "; operator=" + operator + "; value=" + value.toString();

  }

} // class JdmAttribute
