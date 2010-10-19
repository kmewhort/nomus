/*
 *  MtlTransducer.java
 *
 *  Copyright (c) 1995-2010, The University of Sheffield. See the file
 *  COPYRIGHT.txt in the software or at http://gate.ac.uk/gate/COPYRIGHT.txt
 *
 *  This file is part of GATE (see http://gate.ac.uk/), and is free
 *  software, licenced under the GNU Library General Public License,
 *  Version 2, June 1991 (in the distribution as file licence.html,
 *  and also available at http://gate.ac.uk/gate/licence.html).
 *
 *  Valentin Tablan, 01 Feb 2000
 *
 *  Minor modifications by Luc Plamondon, Université de Montréal, 26/11/03:
 *  - Renamed gate.creole.Transducer.java into MtlTransducer.java and put 
 *    in package ca.umontreal.iro.rali.gate.creole
 *  - Added javadoc comment to describe the new features of the transducer
 *    of the ca.umontreal.iro.rali.gate package
 *  - Added code of gate.creole.OntologyAwareTransducer to add an
 *    ontology aware behaviour. 
 *  - The classpath is automatically updated on initialisation.
 *
 *  $Id$
 */

package ca.umontreal.iro.rali.gate.creole;

import gate.creole.*;
import gate.*;
import gate.util.*;
import gate.creole.ontology.Ontology;
import gate.Resource;
import gate.event.*;

import ca.umontreal.iro.rali.gate.jape.*;
import ca.umontreal.iro.rali.gate.annotation.*;

import java.net.*;
import java.util.*;
import java.io.*;
import java.net.*;

/**
   Montreal Transducer: A cascaded multi-phase ontology-aware transducer using the Jape language which is a
   variant of the CPSL language.  Requires java 1.4 or higher.

   <p>
   The Montreal Transducer is based on the Transducer from the ANNIE suite but with the following added features:

   <ul>
   <li>
   It provides more <a href="#morecomparison">comparison</a> operators in left hand side constraints
   </li>
   <li>
   It allows <a href="#conjunctions">conjunctions</a> of constraints on different types of annotation
   </li>
   <li>
   It guarantees that the "*" and "+" Kleene operators are <a href="#greedy">greedy</a>
   </li>
   </ul>

   <p>
   To use this resource, the repository (or directory) containing the <tt>creole.xml</tt> and resource jar file must be loaded by the user.  The repository must be accessible via the <tt>file://</tt> protocol.  Unlike most resources, the repository cannot be a web URL (<tt>http://www...</tt>).  This is because the transducer compiles java code (the grammar rules) every time it is loaded and the resource jar file must be part of the classpath when compiling, but only regular file URLs are allowed in the classpath.  The resource will try to add the jar file to the classpath automatically; if problems arise when loading the transducer, add the jar file to the classpath manually prior to running the application.  

   <p>
   <strong><a name="morecomparison">More comparison operators</a></strong>

   <p> 
   The Montreal Transducer offers more comparison operators to put in left hand side constraints of a JAPE grammar.  The standard ANNIE transducer allows constraints only like these:
   
   <ul>
   <li> 
   <code>{MyAnnot}</code> // true if the current annotation is a MyAnnot annotation
   </li>
   <li>
   <code>{MyAnnot.attrib == "3"}</code> // true if <code>attrib</code> attribute has a value that is equal to 3
   </li>
   </ul>

    <p> 
    The Montreal Transducer allows the following constraints:

    <ul>
    <li>
    <code>{!MyAnnot}</code> // true if NO annotation at current point is a MyAnnot
    </li>
    <li>
    <code>{!MyAnnot.attrib == 3}</code> // true if <code>attrib</code> is not equal to 3
    </li>
    <li>
    <code>{MyAnnot.attrib != 3}</code> // true if <code>attrib</code> is not equal to 3
    </li>
    <li>
    <code>{MyAnnot.attrib &gt; 3}</code> // true if <code>attrib</code> &gt; 3
    </li>
    <li>
    <code>{MyAnnot.attrib &gt;= 3}</code> // true if <code>attrib</code> &ge; 3
    </li>
    <li>
    <code>{MyAnnot.attrib &lt; 3}</code> // true if <code>attrib</code> &lt; 3
    </li>
    <li>
    <code>{MyAnnot.attrib &lt;= 3}</code> // true if <code>attrib</code> &le; 3
    </li>
    <li>
    <code>{MyAnnot.attrib =~ "[Dd]ogs?"}</code> // true if regular expression matches <code>attrib</code> entirely
    </li>
    <li>
    <code>{MyAnnot.attrib !~ "[Dd]ogs?"}</code> // true if regular expression does not match <code>attrib</code>
    </li>
    </ul>

    <p> 
    See the notes on the <a href="#equality">equality</a> operators, <a href="#comparison">comparison</a> operators, <a href="#pattern">pattern matching</a> operators and <a href="#negation">negation</a> operator. 


    <p>
    <strong><a name="equality">Notes on equality operators: "==" and "!="</a></strong>

    <p>
    The "!=" operator is the negation of the "==" operator, that is to say: <code>{Annot.attribute != value}</code> is equivalent to <code>{!Annot.attribute == value}</code>.

    <p>
    When a constraint on an attribute cannot be evaluated because an annotation does not have a value for the attribute, the equality operator returns false (and the difference operator returns true). 

    <p>
    If the constraint's attribute is a string, then the String.equals method is called with the annotation's attribute as a parameter.  If the constraint's attribute is an integer, then the Long.equals method is called.  If the constraint's attribute is a float, then the Double.equals method is called.  And if the constraint's attribute is a boolean, then the Boolean.equals method is called.  The grammar parser does not allow other types of constraints.

    <p>
    Normally, when the types of the constraint's and the annotation's attribute differ, they cannot be equal.  However, because some ANNIE processing resources (namely the tokeniser) set all attribute values as strings even when they are numbers (<code>Token.length</code> is set to a string value, for example), the Montreal Transducer can convert the string to a Long/Double/Boolean before testing for equality.  In other words, for the token "dog":

    <ul>
    <li>
    <code>{Token.attrib == "3"}</code> is true using either the ANNIE transducer or the Montreal Transducer
    </li>
    <li
    <code>{Token.attrib == 3}</code> is false using the ANNIE transducer, but true using the Montreal Transducer
    </li>
    </ul>


    <p>
    <strong><a name="comparison">Notes on comparison operators: "&gt;", "&lt;", "&gt;=" and "&lt;="</a></strong>

    <p>
    If the constraint's attribute is a string, then the String.compareTo method is called with the annotation's attribute as a parameter (strings can be compared alphabetically).  If the constraint's attribute is an integer, then the Long.compareTo method is called.  If the constraint's attribute is a float, then the Double.compareTo method is called.  The transducer issues a warning if an attempt is made to compare two Boolean because this type does not extend the Comparable interface and thus has no compareTo method.
  
    <p> 
    The transducer issues a warning when it encounters an annotation's attribute that cannot be compared to the constraint's attribute because the value types are different, or because one value is null.  For example, given a constraint <code>{MyAnnot.attrib &gt; 2}</code>, a warning is issued for any MyAnnot in the document for which <code>attrib</code> is not an integer, such as <code>attrib = "dog"</code> because we cannot evaluate <code>"dog" &gt; 2</code>.  Similarly, <code>{MyAnnot.attrib &gt; 2}</code> cannot be compared to <code>attrib = 2.5</code> because 2.5 is a float.  In this case, force 2 as a float with <code>{MyAnnot.attrib &gt; 2.0}</code>.

    <p>
    The transducer does not issue a warning when the constraint's attribute is an integer/float and the annotation's attribute is a string but can be parsed as an integer/float.  Some ANNIE processing resources (namely the tokeniser) set all attribute values as strings even when they are numbers (<code>Token.length</code> is set to a string value, for example), and because <code>{Token.length &lt; "10"}</code> would lead to an alphabetical comparison, a workaround was needed so we could write <code>{Token.length &lt; 10}</code>.   

    <p>
    <strong><a name="pattern">Notes on pattern matching operators: "=~" and "!~"</a></strong>

    <p>
    The "!~" operator is the negation of the "=~" operator, that is to say: <code>{Annot.attribute !~ "value"}</code> is equivalent to <code>{!Annot.attribute =~ "value"}</code>.

    <p>
    When a constraint on an attribute cannot be evaluated because an annotation does not have a value for the attribute, the value defaults to an empty string (""). 

    <p>
    The regular expression must be enclosed in double quotes, otherwise the transducer issues a warning:

    <ul>
    <li>
    <code>{MyAnnot.attrib =~ "[Dd]ogs?"}</code> is correct
    </li>
    <li>
    <code>{MyAnnot.attrib =~ 2}</code> is incorrect
    </li>
    </ul>

    <p>
    The regular expression must be a valid java.util.regex.Pattern, otherwise a warning is issued.

    <p>
    To have a match, the regular expression must cover the entire attribute string, not only a part of it.  For example:

    <ul>
    <li>
    <code>{MyAnnot.attrib =~ "do"}</code> does not match "does"
    </li>
    <li>
    <code>{MyAnnot.attrib =~ "do.*"}</code> matches "does"
    </li>
    </ul>

    <p>
    <strong><a name="negation">Notes on the negation operator: "!"</a></strong>

    <p>
    Bindings: when a constraint contains both negated and regular elements, the negated elements do not affect the bindings of the regular elements.  Thus, <code>{Person, !Organization}</code> binds to the same annotations (amongst those that starts at current node in the annotation graph) as <code>{Person}</code>; the difference between the two is that the first will simply not match if one of the annotations starting at current node is an Organization.  On the other hand, when a constraint contains only negated elements such as <code>{!Organization}</code>, it binds to all annotations starting at current node.  It is important to keep that in mind especially when a rule ends with a constraint with negated elements only: the longest annotation at current node will be preferred.

    
    <p>
    <strong><a name="conjunctions">Conjunctions of constraints on different types of annotation</a></strong>

    <p>
    The Montreal Transducer allows constraints on different types of annotation.  Though the JAPE implementation exposed in the GATE 2.1 User Guide details an algorithm that would allow such constraints, the ANNIE transducer does not implement it.  This transducer does.  Those examples do not work as expected with the ANNIE transducer but do with this transducer:

    <ul>
    <li>
    <code>{Person, Organization}</code>
    </li>
    <li>
    <code>{Person, Organization, Token.length == "10"}</code>
    </li>
    <li>
    <code>{Person, !Organization}</code>
    </li>
    </ul>
    
    As described in the algorithm, the first example above matches points in the document (or nodes in the annotation graph) where both a Person and an Organization annotations begin, even if they do not end at the same point in the document and even if other annotations begin at the same point.  When a negation is involved, such as in the third example above, no annotation of that kind must begin at a given point for a match to occur (see the note on the negation operator below). 
  
    <p>
    <strong><a name="greedy">Greedy Kleene operators: "*" and "+"</a></strong>

    <p>
    The ANNIE transducer does not behave consistently regarding the "*" and "+" Kleene operators.  Suppose we have the following rule with 2 bindings:

    <ul>
    <li>
    <code>({Lookup.majorType == title})+:titles ({Token.orth == upperInitial})+:names</code>
    </li>
    </ul>

    Given the sentence "<code>the Honourable Mr. John Atkinson</code>", we expect the following bindings:

    <ul>
    <li>
    titles: "<code>Honourable Mr.</code>"
    </li>
    <li>
    names: "<code>John Atkinson</code>"
    </li>
    </ul>

    But the ANNIE transducer could give something like:

    <ul>
    <li>
    titles: "<code>Honourable</code>"
    </li>
    <li>
    names: "<code>Mr. John Atkinson</code>"
    </li>
    </ul>

    This is not incorrect, but according to convention, "*" and "+" operators match as many tokens as possible before moving on to the next constraint.  The Montreal Transducer guarantees that "*" and "+" are greedy. 


    



 */
public class MtlTransducer extends AbstractLanguageAnalyser {

  public static final String
    TRANSD_DOCUMENT_PARAMETER_NAME = "document";

  public static final String
    TRANSD_INPUT_AS_PARAMETER_NAME = "inputASName";

  public static final String
    TRANSD_OUTPUT_AS_PARAMETER_NAME = "outputASName";

  public static final String
    TRANSD_ENCODING_PARAMETER_NAME = "encoding";

  public static final String
    TRANSD_GRAMMAR_URL_PARAMETER_NAME = "grammarURL";

  public static final String
    TRANSD_AUTHORISE_DUPLICATES_PARAMETER_NAME = "authoriseDuplicates";

  /**
   * Default constructor. Does nothing apart from calling the default
   * constructor from the super class. The actual object initialisation is done
   * via the {@link #init} method.
   */
  public MtlTransducer() {
  }

  /*
  private void writeObject(ObjectOutputStream oos) throws IOException {
    Out.prln("writing transducer");
    oos.defaultWriteObject();
    Out.prln("finished writing transducer");
  } // writeObject
  */

  /**
   * This method is the one responsible for initialising the transducer. It
   * assumes that all the needed parameters have been already set using the
   * appropiate setXXX() methods.
   *@return a reference to <b>this</b>
   */
  public Resource init() throws ResourceInstantiationException {
    if(grammarURL != null && encoding != null){

      // get the absolute path of the jar file that contains this class
      String className = this.getClass().getName();
      ResourceData resData = (ResourceData) Gate.getCreoleRegister().get(className);
      File jarFile = new File(resData.getJarFileUrl().getPath());
      String jarPath = jarFile.getAbsolutePath();

      // split the classpath into its elements
      //String pathSep = System.getProperty("path.separator");
      String pathSep = File.pathSeparator;
      String classPath = System.getProperty("java.class.path");
      StringTokenizer pathTokenizer = new StringTokenizer(classPath, pathSep);

      // check whether the jar is already in the classpath
      boolean jarNotFound = true;
      while (pathTokenizer.hasMoreTokens() && jarNotFound) {
	File thisPath = new File(pathTokenizer.nextToken());
	if (jarPath.equals(thisPath.getAbsolutePath())) {
	  jarNotFound = false;
	}
      }

      // add the jar of the current class to the classpath
      if (jarNotFound) {
	String newClassPath = classPath + pathSep + jarPath;
	System.setProperty("java.class.path", newClassPath);
	addedClassPath = jarPath;
      }

    
      // now launch the module
      try{
        fireProgressChanged(0);
        // batch = new Batch(grammarURL, encoding, 
	//                   new InternalStatusListener());
	batch = new Batch(grammarURL, encoding);
        fireProcessFinished();
      }catch(JapeException je){
        throw new ResourceInstantiationException(je);
      }
    } 
    else {
      throw new ResourceInstantiationException (
        "Both the URL (was " + grammarURL + ") and the encoding (was " +
        encoding + ") are needed to create a JapeTransducer!"
      );
    }

    batch.addProgressListener(new IntervalProgressListener(0, 100));
    batch.setOntology(ontology);
     
    return this;
  }

  /**
   * Implementation of the run() method from {@link java.lang.Runnable}.
   * This method is responsible for doing all the processing of the input
   * document.
   */
  public void execute() throws ExecutionException{
    interrupted = false;
    if(document == null) throw new ExecutionException("No document provided!");
    if(inputASName != null && inputASName.equals("")) inputASName = null;
    if(outputASName != null && outputASName.equals("")) outputASName = null;

    AnnotationSet inputAS = (inputASName == null) ?
                             document.getAnnotations() :
                             document.getAnnotations(inputASName);
    AnnotationSet outputAS = (outputASName == null) ?
                              document.getAnnotations() :
                              document.getAnnotations(outputASName);

    NoDupAnnotationSetImpl tempInputAS = new NoDupAnnotationSetImpl(inputAS);
    NoDupAnnotationSetImpl tempOutputAS;

    if (inputAS == outputAS) {
      tempOutputAS = tempInputAS;
    }
    else {
      tempOutputAS = new NoDupAnnotationSetImpl(outputAS);
    }

    if (getAuthoriseDuplicates().booleanValue() == true) {
      tempInputAS.setAuthoriseDuplicates(Boolean.TRUE);
      tempOutputAS.setAuthoriseDuplicates(Boolean.TRUE);
    }

    try{
      batch.transduce(document, tempInputAS, tempOutputAS);
    }catch(JapeException je){
      throw new ExecutionException(je);
    }

    // Update the original sets of the document
    inputAS.clear();
    inputAS.addAll(tempInputAS);
    if (inputAS != outputAS) {
      outputAS.clear();
      outputAS.addAll(tempOutputAS);
    }

  }


  /**
   * Notifies all the PRs in this controller that they should stop their
   * execution as soon as possible.
   */
  public synchronized void interrupt(){
    interrupted = true;
    batch.interrupt();
  }
  /**
   * Sets the grammar to be used for building this transducer.
   * @param newGrammarURL an URL to a file containing a Jape grammar.
   */
  public void setGrammarURL(java.net.URL newGrammarURL) {
    grammarURL = newGrammarURL;
  }

  /**
   * Gets the URL to the grammar used to build this transducer.
   * @return a {@link java.net.URL} pointing to the grammar file.
   */
  public java.net.URL getGrammarURL() {
    return grammarURL;
  }

  /**
   *
   * Sets the encoding to be used for reding the input file(s) forming the Jape
   * grammar. Note that if the input grammar is a multi-file one than the same
   * encoding will be used for reding all the files. Multi file grammars with
   * different encoding across the composing files are not supported!
   * @param newEncoding a {link String} representing the encoding.
   */
  public void setEncoding(String newEncoding) {
    encoding = newEncoding;
  }

  /**
   * Gets the encoding used for reding the grammar file(s).
   */
  public String getEncoding() {
    return encoding;
  }

  /**
   * Sets the {@link gate.AnnotationSet} to be used as input for the transducer.
   * @param newInputAS a {@link gate.AnnotationSet}
   */
  public void setInputASName(String newInputASName) {
    inputASName = newInputASName;
  }

  /**
   * Gets the {@link gate.AnnotationSet} used as input by this transducer.
   * @return a {@link gate.AnnotationSet}
   */
  public String getInputASName() {
    return inputASName;
  }

  /**
   * Sets the {@link gate.AnnotationSet} to be used as output by the transducer.
   * @param newOutputAS a {@link gate.AnnotationSet}
   */
  public void setOutputASName(String newOutputASName) {
    outputASName = newOutputASName;
  }

  /**
   * Gets the {@link gate.AnnotationSet} used as output by this transducer.
   * @return a {@link gate.AnnotationSet}
   */
  public String getOutputASName() {
    return outputASName;
  }

  /**
   * Sets the authoriseDuplicates flag that allow/prevent the transducer
   * from creating annotations that already exist at some point in the doc.
   * This is particularly useful when the transducer is called more than
   * once in a pipeline (as when the gazetteer is updated by a first pass
   * of the transducer and we want the transducer to do a second pass)

   * @param newAuthoriseDuplicates if set to false, the transducer performs
   * righthandside actions as usual but does not add annotations to the
   * output annotation set when an identical annotation exists at the
   * same point in the document.
   */
  public void setAuthoriseDuplicates(Boolean newAuthoriseDuplicates) {
    authoriseDuplicates = newAuthoriseDuplicates;
  }

  /**
   * Gets the authoriseDuplicates flag that allow/prevent the transducer
   * from creating annotations that already exist at some point in the doc.
   *
   * @return true/false
   */
  public Boolean getAuthoriseDuplicates() {
    return authoriseDuplicates;
  }


  /**
   * The URL to the jape file used as grammar by this transducer.
   */
  private java.net.URL grammarURL;


  /**
   * The actual JapeTransducer used for processing the document(s).
   */
  protected Batch batch;

  /**
   * The encoding used for reding the grammar file(s).
   */
  private String encoding;

  /**
   * The {@link gate.AnnotationSet} used as input for the transducer.
   */
  private String inputASName;

  /**
   * The {@link gate.AnnotationSet} used as output by the transducer.
   */
  private String outputASName;

  /** The path added by this module to the system classpath */     
  private String addedClassPath;

  /** A flag to prevent or not the creation by the transducer of
      annotations that already exist at the same point in the doc */
  private Boolean authoriseDuplicates;

  /**
   * The ontology that will be available on the RHS of JAPE rules.
   */
  private gate.creole.ontology.Ontology ontology;

  /**
   * Gets the ontology used by this transducer.
   * @return an {@link gate.creole.ontology.Ontology} value.
   */
  public gate.creole.ontology.Ontology getOntology() {
    return ontology;
  }

  /**
   * Sets the ontology used by this transducer.
   * @param ontology an {@link gate.creole.ontology.Ontology} value.
   */
  public void setOntology(gate.creole.ontology.Ontology ontology) {
    this.ontology = ontology;
  }


  /** Remove this class' jar file from the system classpath so that the system
      state is the same as when the init method was called (and before this
      class' jar file was added to the classpath, if missing).
  */
  public void cleanup() {
    if (addedClassPath != null) {

      // split the classpath into its elements
      String pathSep = File.pathSeparator;
      String classPath = System.getProperty("java.class.path");
      StringTokenizer pathTokenizer = new StringTokenizer(classPath, pathSep);

      // copy the StringTokenizer enumeration to a list
      ArrayList pathList = new ArrayList();
      while (pathTokenizer.hasMoreTokens()) {
	pathList.add(pathTokenizer.nextToken());
      }

      // remove the jar file from the classpath (remove last occurence)
      int index = pathList.lastIndexOf(addedClassPath);
      if (index >= 0) {
	pathList.remove(index);
      }

      // rebuild classpath
      StringBuffer newClassPath = new StringBuffer(255);      
      for (ListIterator i = pathList.listIterator(); i.hasNext(); ) {
	newClassPath.append(i.next());
	if (i.hasNext()) {
	  newClassPath.append(pathSep);
	}
      }
      
      System.setProperty("java.class.path", newClassPath.toString());
    }
  }
}
