/*
 *  Constraint.java - transducer class
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
 *  Modifications by Luc Plamondon, Université de Montréal, 20/11/03:
 *  - Migrated original file to the ca.umontreal.iro.rali.gate.jape package
 *  - The attributes/values are stored in a list of JdmAttribute objects,
 *    instead of a FeatureMap.  This allows us to store a comparison operator
 *    other than "equal" in addition to a feature/value pair in each 
 *    JdmAttribute object.
 *  - Removed a few constructors that were not used elsewhere.
 *  - Added a series of subsume methods (initially in gate.FeatureMap, but due 
 *    to cross-package restrictions on attributes/methods and due to speed 
 *    considerations, I did not extend a new class from SimpleFeatureMapImpl 
 *    and did not overload the subsume method).
 *
 *  $Id$
 */


package ca.umontreal.iro.rali.gate.jape;

import java.net.*;
import java.util.*;
import java.util.regex.*;

import gate.annotation.*;
import gate.creole.ontology.*;
import gate.util.*;
import gate.*;

/**
  * An individual annotation/attributes/values/operator expression. 
  * It doesn't extend PatternElement, even though it has to "match", because a 
  * set of Constraint must be applied together in order to avoid doing separate
  * selectAnnotations calls for each one.
  */
public class Constraint
implements JapeConstants, gate.creole.ANNIEConstants, java.io.Serializable, Cloneable
{
  /** Debug flag */
  private static final boolean DEBUG = false;

  /** Construction from annot type string */
  public Constraint(String annotType) {
    this.annotType = annotType;
    attributeList = new LinkedList();
  } // Construction from annot type

  /** The type of annnotation we're looking for. */
  private String annotType;

  /** Are we negated? */
  private boolean negated = false;

  /** Set negation. */
  public void negate() { negated = true; }

  /** Access to negation flag. */
  public boolean isNegated() { return negated; }

  /** Change the sign of the negation flag. */
  public void changeSign() { negated = !negated; }

  /** Get the type of annnotation we're looking for. */
  public String getAnnotType() { return annotType; }


  /** The list of attributes that must match the annotation features.  
      Attributes are JdmAttribute objects. 
  */
  private LinkedList attributeList;

  /** Get the list of attributes that must match the annotation features. 
      Attributes are JdmAttribute objects. 
  */
  public LinkedList getAttributeSeq() { return attributeList; }

  /** Get the attributes that must be present on the matched annotation. */
  public JdmAttribute[] getAttributeArray() { 
    return (JdmAttribute[]) attributeList.toArray(); 
  }

  /** Add an attribute. */
  public void addAttribute(JdmAttribute attr) {
    attributeList.add(attr);
  } // addAttribute


  /** Need cloning for processing of macro references. See comments on
    * <CODE>PatternElement.clone()</CODE>
    */
  public Object clone() {
    Constraint newC = null;
    try {
      newC = (Constraint) super.clone();
    } catch(CloneNotSupportedException e) {
      throw(new InternalError(e.toString()));
    }
    newC.annotType = annotType;
    // Shallow clone.  Is it ok?
    newC.attributeList = (LinkedList) attributeList.clone();

     return newC;
  } // clone


 /** Finish: replace dynamic data structures with Java arrays; called
    * after parsing.
    */
  public void finish() {
    /*
    if(attrs1 == null || attrs1.size() == 0) {
      attrs2 = new JdmAttribute[0];
      attrs1 = null;
      return;
    }
    int attrsLen = attrs1.size();
    attrs2 = new JdmAttribute[attrsLen];

    int i = 0;
    //for(Enumeration e = attrs1.getElements(); e.hasMoreElements(); i++) {
    //  attrs2[i] = (JdmAttribute) e.nextElement();
    //}
    Iterator iter = attrs1.keySet().iterator();
    while(iter.hasNext()) {
      String name = (String) iter.next();
      Object value = attrs1.get(name);
      attrs2[i++] = new JdmAttribute(name, value);
    }
    attrs1 = null;
    */
  } // finish

  /** Create a string representation of the object. */
  public String toString() { return toString(""); }

  /** Create a string representation of the object. */
  public String toString(String pad) {
    String newline = Strings.getNl();

    StringBuffer buf = new StringBuffer(
      pad + "Constraint: annotType(" + annotType + "); attrs(" + newline + pad
    );

    buf.append(attributeList.toString());
    buf.append(newline + pad + ") Constraint." + newline);

    return buf.toString();
  } // toString

  public String shortDesc() {
    String res = annotType + "(";
    res += attributeList.toString();
    res += ")";
    return res;
  } // shortDesc


  /** Test if an annotation complies with at least one feature and comparison 
      operators of this constraint's attributes.
      @param annot an annotation
      @return <code>true</code> if the annotation tests successfully with the 
              values and operators of at least one attribute of this constraint  and 
              <code>false</code> if not.
  */
  public boolean subsumesOne(SimpleFeatureMapImpl annot) {
    return subsumesOneOrAll(annot, false);
  }

  /** Test if an annotation complies with all features and comparison 
      operators of this constraint's attributes.
      @param annot an annotation
      @return <code>true</code> if the annotation tests successfully with the 
              values and operators from every attribute of this constraint  and 
              <code>false</code> if not.
  */
  public boolean subsumes(SimpleFeatureMapImpl annot) {
    return subsumesOneOrAll(annot, true);
  }

  /** Test if an annotation complies with the features and comparison 
      operators of this constraint's attributes.  Each feature is tested and
      then a big AND can be done if <code>anded=true</code>, or a big
      OR if <code>anded=false</code>.
      @param annot an annotation
      @param anded <code>true</code>: all features must match. 
                   <code>false</code>: at least one feature must match.
      @return <code>true</code> if the annotation tests successfully with the 
              values and operators from the attribute of this constraint  and 
              <code>false</code> if not.
  */
  protected boolean subsumesOneOrAll(SimpleFeatureMapImpl annot, boolean anded){

    boolean ored = !anded;

    // null is included in everything
    if (attributeList == null) 
      return true;

    /* General algorithm: we will now iterate over all the features to test.
       If we are ANDing them, return false as soon as a test fails.
       If we are ORing them, return true as soon as a test succeeds.
    */

    JdmAttribute attribute;

    for (ListIterator i = attributeList.listIterator(); i.hasNext(); ) {
      attribute = (JdmAttribute) i.next();
      
      String attributeName = attribute.getName();
      Object attributeValue = attribute.getValue();
      int attributeOp = attribute.getOperator();
      Object annotValue = annot.get(attributeName);

      if (attributeName == null || attributeValue == null) 
	continue;

      /* if an attribute pertains to a class from the ontology, 
	 perform ontology-aware subsume */     
      if (attributeName.equals(LOOKUP_CLASS_FEATURE_NAME) && annotValue != null) {
	
	//find the ontology url amongst the other attributes of the constraint
	Object constraintOntoObj = null;
	JdmAttribute currentAttribute = null;
	for (ListIterator j = attributeList.listIterator(); j.hasNext(); ) {
	  currentAttribute = (JdmAttribute) j.next();
	  if (currentAttribute.getName().equals(LOOKUP_ONTOLOGY_FEATURE_NAME)) {
	    constraintOntoObj = currentAttribute.getValue();
	    continue;
	  }
	}
      
	/* continue only if ont. url from constraint and annot. are the same,
	   otherwise ignore this attribute (default with true) */
	Object thisOntoObj = annot.get(LOOKUP_ONTOLOGY_FEATURE_NAME);
	if (constraintOntoObj != null && thisOntoObj != null) {
	  if (constraintOntoObj.equals(thisOntoObj)) {
	    boolean doSubsume = ontologySubsume(
						constraintOntoObj.toString(),
						attributeValue.toString(),
						annotValue.toString());
	      
	    if (attributeOp == JapeConstants.EQUAL) {
	      if (!doSubsume && anded) return false;
	      if (doSubsume && ored) return true;
	    }
	    if (attributeOp == JapeConstants.NOT_EQUAL) {
	      if (doSubsume && anded) return false;
	      if (!doSubsume && ored) return true;
	    }
	    // else: accept (ignore)
	  
	  } // if ontologies are with the same url
	} //if not null objects
      }
  
      /* process without ontology awareness */

      try {

	// EQUAL
	if (attributeOp == JapeConstants.EQUAL) {
	  boolean successful = equalSubsume(attributeName, attributeValue, annotValue);
	  if (!successful && anded) return false;
	  if (successful && ored) return true;
	} //if (attributeOp == JapeConstants.EQUAL)
	
	// NOT_EQUAL
	if (attributeOp == JapeConstants.NOT_EQUAL) {
	  boolean successful = equalSubsume(attributeName, attributeValue, annotValue);
	  if (successful && anded) return false;
	  if (!successful && ored) return true;
	} //if (attributeOp == JapeConstants.NOT_EQUAL)

	// REGEXP
	if (attributeOp == JapeConstants.REGEXP) {
	  // create a default value if annotation value is null
	  if (annotValue == null)
	    annotValue = new String();
	  boolean successful = regexpSubsume(attributeName, attributeValue, annotValue);
	  if (!successful && anded) return false;
	  if (successful && ored) return true;
	} //if (attributeOp == JapeConstants.REGEXP)
	
	// NOT_REGEXP
	if (attributeOp == JapeConstants.NOT_REGEXP) {
	  // create a default value if annotation value is null
	  if (annotValue == null)
	    annotValue = new String();
	  boolean successful = regexpSubsume(attributeName, attributeValue, annotValue);
	  if (successful && anded) return false;
	  if (!successful && ored) return true;
	} //if (attributeOp == JapeConstants.NOT_REGEXP)
	
	// GREATER
	if (attributeOp == JapeConstants.GREATER) {
	  boolean successful = greaterSubsume(attributeName, attributeValue, annotValue);
	  if (!successful && anded) return false;
	  if (successful && ored) return true;
	} //if (attributeOp == JapeConstants.GREATER)
	
	// LESSER_OR_EQUAL
	if (attributeOp == JapeConstants.LESSER_OR_EQUAL) {
	  boolean successful = greaterSubsume(attributeName, attributeValue, annotValue);
	  if (successful && anded) return false;
	  if (!successful && ored) return true;
	} //if (attributeOp == JapeConstants.GREATER)     
	
	// LESSER
	if (attributeOp == JapeConstants.LESSER) {
	  boolean successful = lesserSubsume(attributeName, attributeValue, annotValue);
	  if (!successful && anded) return false;
	  if (successful && ored) return true;
	} //if (attributeOp == JapeConstants.GREATER)
	
	// GREATER_OR_EQUAL
	if (attributeOp == JapeConstants.GREATER_OR_EQUAL) {
	  boolean successful = lesserSubsume(attributeName, attributeValue, annotValue); 
	  if (successful && anded) return false;
	  if (!successful && ored) return true;
	} //if (attributeOp == JapeConstants.GREATER)           
      }
      
      catch (JapeException je) {
	gate.util.Err.println(je.getMessage());
	if (anded) return false;
      }
      
    } // checked all attributes

    /* If all features had to match (ANDed), then we can return true because
       no test has failed so far.  If at least one feature had to match (ORed), 
       then we have to return false because no test has succeeded so far.
    */
    if (anded) 
      return true;
    else  // if (ored) 
      return false;
  
  } //subsumes()
    

  /** For a given attribute, test whether the values from the annotation and 
      from the constraint are equal.
      They must have the same type.  The method use a.equals(b) to test for 
      equality.
      @param attributeName the name of the attribute, in this constraint
      @param attributeValue the value of the attribute, in this constraint
      @param annotValue the value of the attribute, in the annotation
      
      @return true if both values are equal, false otherwise or if the values 
              have different type or if annotValue is null.
	      
      @throws JapeException if the attribute in the constraint has an unexpected
              type (other than String/Long/Double/Boolean).  This probably means
	      that the grammar parser (ParseCpsl.jj) has been changed to allow
	      that.

   */
  
  private static boolean equalSubsume(Object attributeName, Object attributeValue, 
				      Object annotValue)
    throws JapeException {

    if (annotValue == null) return false;
 
    if (attributeValue.equals(annotValue)) return true;
      
    /* The constraint's attribute can be a String/Long/Double/Boolean.
       The annotation's attribute must be of the same type,
       otherwise equals will return false.
       In that case, let's suppose the annot's attrib. is a String and
       let's try to convert it to the same type as the constraint. */
    
    if (annotValue instanceof String) {
      String annotValueString = (String) annotValue;
	
      if (attributeValue instanceof String)
	return false; // the comparison has already been made

      try {
	// the constraint's attribute is a Long
	if (attributeValue instanceof Long) {
	  if (attributeValue.equals(Long.valueOf(annotValueString)))
	    return true;
	  else 
	    return false;
	}
	// the constraint's attribute is a Double
	if (attributeValue instanceof Double){
	  if (attributeValue.equals(Double.valueOf(annotValueString)))
	    return true;
	  else
	    return false;
	}
	// the constraint's attribute is a Boolean
	if (attributeValue instanceof Boolean) {
	  if (attributeValue.equals(Boolean.valueOf(annotValueString)))
	    return true;
	  else
	    return false;
	}
   
	// if we reach that point, it means constraint has an unexpected type!
	throw new JapeException("Warning! Attribute \""+attributeName+"\" from a grammar rule has a value that is not a String/Long/Double/Boolean.  This should not be allowed."); 
      }
      catch (NumberFormatException otherType) {
	// annot is a String and cannot be converted to Long/Double/Boolean,
	// cannot be equal
	return false;
      }
    } //if String
    
    return false;
  }


  /** For a given attribute, test whether the value from the annotation matches
      the regular expression stated in the constraint.
   
      @param attributeName the name of the attribute, in this constraint
      @param attributeValue the value of the attribute, in this constraint 
             (that is, the regular expression)
      @param annotValue the value of the attribute, in the annotation

      @return true if annotValue matches the attributeValue regexp, 
              false otherwise

      @throws JapeException if the attribute in the annotation is not a String
              (pattern matching is irrelevant in that case)
	      
   */

  private static boolean regexpSubsume(Object attributeName, Object attributeValue, 
				Object annotValue)
    throws JapeException {

    String annotValueString;
    Pattern constraintPattern;

    if (annotValue instanceof String) {
      annotValueString = (String) annotValue;
      if (annotValueString == null) 
	annotValueString = new String();
      constraintPattern = (Pattern) attributeValue;
      if (constraintPattern.matcher(annotValueString).matches()) {
	return true;
      }
      else {
	return false;
      }
    }
    else {
      throw new JapeException("Cannot do pattern matching on attribute \""+attributeName.toString()+"\".  Are you sure the value is a string?");
    }
  }


  /** For a given attribute, test whether the value from the annotation is 
      greater than the value from this constraint.  This method uses the compareTo
      method from the 
      java.lang.Comparable interface to perform the test.  If any of the values
      cannot be cast to Comparable (Boolean, for example, are not Comparable),
      this method returns false.
      <p> Because sometimes numerical values are stored in annotations as
      strings (Token.length for example), this method tries to convert the
      annotation's value to a Double or a Long when the compareTo test fails,
      and then performs the compareTo test again.  

      @param attributeName the name of the attribute, in this constraint
      @param attributeValue the value of the attribute, in this constraint
      @param annotValue the value of the attribute, in the annotation

      @return true if annotValue is greater than attributeValue, 
              false otherwise or if annotValue cannot be cast to a Long/Double,
	      or if annotValue cannot be parsed to a Long/Double.

      @throws JapeException if the attribute in the annotation cannot be
              compared or parsed to an object of the same type as the attribute
              in the constraint, or if the attribute in the annotation is null.
  */

  private static boolean greaterSubsume(Object attributeName, Object attributeValue, 
				 Object annotValue)
    throws JapeException {


    if (annotValue == null)
      throw new JapeException("Cannot compare attribute \"" +
			      attributeName.toString() +
			      "\" with <, >, <= or >= because annotation value is null.");

    /* attributeValue can be a String/Long/Double/Boolean.
       First assume that annotValue is of the same type as
       attributeValue and call compareTo, except for Boolean.
       If it fails, assume that annotValue is a string then
       convert it to the same type as attributeValue and test. */

    // first check if we can call the compareTo method
    if (attributeValue instanceof Comparable) {
      Comparable attributeComparable = (Comparable) attributeValue;
      
      // then assume attribute and annot are of the same type and compare
      try {
	if (attributeComparable.compareTo(annotValue) < 0) 
	  return true;
	else
	  return false;
      }
      // attribute and annot do not have same type
      catch (ClassCastException notSameType) {

	// if annotValue is a String, there is still hope to parse it
	if (annotValue instanceof String) {
	  String annotValueString = (String) annotValue;

	  if (attributeValue instanceof String)
	    return false; // the comparison has already been made
	
	  try {
	    // if attribute is a Long, parse annot as a Long
	    if (attributeComparable instanceof Long) {
	      if (attributeComparable.compareTo(
		  Long.valueOf(annotValueString)) < 0) 
		return true;
	      else
		return false;
	    }
	    // if attribute is a Double
	    if (attributeComparable instanceof Double) {
	      if (attributeComparable.compareTo(
		  Double.valueOf(annotValueString)) < 0) 
		return true;
	      else
		return false;
	    }
	    
	    // annotValue is of another type, so cannot compare
	    return false; 
	  } //try
	  catch (NumberFormatException nfe) {
	    // attribute is a Long/Double, but annot is not: cannot compare
	    throw new JapeException("Cannot compare values for attribute \"" +
				    attributeName.toString() +
				    "\" because \"" +
				    attributeValue.toString() +
				    "\" and/or \"" +
				    annotValue.toString() +
				    "\" do not have comparable types.");
	  }
	} // if instanceof String
      } //catch notSameType
    } //if instanceof Comparable
    else {
      // cannot compare this type, so throw an exception
  
      throw new JapeException("Cannot compare values for attribute \"" +
			      attributeName.toString() +
			      "\" because \"" +
			      attributeValue.toString() +
			      "\" and/or \"" +
			      annotValue.toString() +
			      "\" do not have comparable types.");
    }
    
    return true;
  }


  /** For a given attribute, test whether the value from the annotation is 
      lesser than the value from this constraint.  This method uses the compareTo
       method from the 
      java.lang.Comparable interface to perform the test.  If any of the values
      cannot be cast to Comparable (Boolean, for example, are not Comparable), 
      this method returns false.
      <p> Because sometimes numerical values are stored in annotations as
      strings (Token.length for example), this method tries to convert the 
      annotation's value to a Double or a Long when the compareTo test fails, 
      and then performs the compareTo test again.  

      @param constraintName the name of the attribute, in the constraint
      @param attributeValue the value of the attribute, in the constraint
      @param annotValue the value of the attribute, in the annotation
      @return true if annotValue is lesser than attributeValue, 
              false otherwise or if annotValue cannot be cast to a Long/Double,
	      or if annotValue cannot be parsed to a Long/Double.

      @throws JapeException if the attribute in the annotation cannot be
              compared or parsed to an object of the same type as the attribute
              in the constraint, or if the attribute in the annotation is null.
  */

  private static boolean lesserSubsume(Object attributeName, Object attributeValue, 
				Object annotValue)
    throws JapeException {

    if (annotValue == null)
      throw new JapeException("Cannot compare attribute \"" +
			      attributeName.toString() +
			      "\" with <, >, <= or >= because annotation value is null.");

    /* attributeValue can be a String/Long/Double/Boolean.
       First assume that annotValue is of the same type as
       attributeValue and call compareTo, except for Boolean.
       If it fails, assume that annotValue is a string then
       convert it to the same type as attributeValue and test. */

    // first check if we can call the compareTo method
    if (attributeValue instanceof Comparable) {
      Comparable attributeComparable = (Comparable) attributeValue;
      
      // then assume attribute and annot are of the same type and compare
      try {
	if (attributeComparable.compareTo(annotValue) > 0) 
	  return true;
	else
	  return false;
      }
      // attribute and annot do not have same type
      catch (ClassCastException notSameType) {

	// if annotValue is a String, there is still hope to parse it
	if (annotValue instanceof String) {
	  String annotValueString = (String) annotValue;

	  if (attributeValue instanceof String)
	    return false; // the comparison has already been made
	
	  try {
	    // if attribute is a Long, parse annot as a Long
	    if (attributeComparable instanceof Long) {
	      if (attributeComparable.compareTo(
		  Long.valueOf(annotValueString)) > 0) 
		return true;
	      else
		return false;
	    }
	    // if attribute is a Double
	    if (attributeComparable instanceof Double) {
	      if (attributeComparable.compareTo(
		  Double.valueOf(annotValueString)) > 0) 
		return true;
	      else
		return false;
	    }
	    
	    // annotValue is of another type, so cannot compare
	    return false; 
	  } //try
	  catch (NumberFormatException nfe) {
	    // attribute is a Long/Double, but annot is not: cannot compare
	    throw new JapeException("Cannot compare values for attribute \"" +
				    attributeName.toString() +
				    "\" because \"" +
				    attributeValue.toString() +
				    "\" and/or \"" +
				    annotValue.toString() +
				    "\" do not have comparable types.");
	  }
	} // if instanceof String
      } //catch notSameType
    } //if instanceof Comparable
    else {
      // cannot compare this type, so throw an exception
  
      throw new JapeException("Cannot compare values for attribute \"" +
			      attributeName.toString() +
			      "\" because \"" +
			      attributeValue.toString() +
			      "\" and/or \"" +
			      annotValue.toString() +
			      "\" do not have comparable types.");
    }
    
    return true;
  }


  /**ontology enhanced subsume
   * @param ontoUrl the url of the ontology to be used
   * @return true if value1 subsumes value2 in the specified ontology */
  private static boolean ontologySubsume(String ontoUrl,String value1,String value2) {

    // this method is a copy of gate.util.SimpleFeatureMapImpl.ontologySubsume

    boolean result = false;
    try {
      URL url;
      try {
        url = new URL(ontoUrl);
      } catch (MalformedURLException e){
        throw new RuntimeException(
        "\nin SimpleFeatureMapImpl on ontologySubsume()\n"
        +e.getMessage()+"\n");
      }

      /* GET ONTOLOGY BY URL : a bit tricky reference
      since the behaviour behind the getOntology method is
      certainly static.
      : should be temporary */
      Ontology o = OntologyUtilities.getOntology(url);

      OClass c1 = (OClass) o.getOResourceByName(value1);
      OClass c2 = (OClass) o.getOResourceByName(value2);

      if (null!= c1 && null!= c2) {
        if (c1.equals(c2)) {
          result = true;
        } else {
          Set subs1;
          subs1 = c1.getSubClasses(OConstants.TRANSITIVE_CLOSURE);
          if (subs1.contains(c2))
            result = true;
        } // else
      } // if not null classes
    } catch  (gate.creole.ResourceInstantiationException x) {
      x.printStackTrace(Err.getPrintWriter());
    }
    return result;
  } // ontologySubsume

} // class Constraint

