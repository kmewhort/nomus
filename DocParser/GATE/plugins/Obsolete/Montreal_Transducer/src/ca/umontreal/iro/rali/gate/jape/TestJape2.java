/*
 *  TestJape2.java (Java Annotation Patterns Engine)
 *
 *  Copyright (c) 1995-2010, The University of Sheffield. See the file
 *  COPYRIGHT.txt in the software or at http://gate.ac.uk/gate/COPYRIGHT.txt
 *
 *  This file is part of GATE (see http://gate.ac.uk/), and is free
 *  software, licenced under the GNU Library General Public License,
 *  Version 2, June 1991 (in the distribution as file licence.html,
 *  and also available at http://gate.ac.uk/gate/licence.html).
 *
 *  Hamish Cunningham, 23/02/2000
 *
 *  Minor modifications made by Luc Plamondon, Université de Montréal, 27/11/03:
 *  - migrated original file from gate.jape to 
 *    ca.umontreal.iro.rali.gate.jape package.
 *
 *  $Id$
 *
 *  Description: Test class for JAPE.
 */

package ca.umontreal.iro.rali.gate.jape;

import java.util.*;
import java.io.*;


import ca.umontreal.iro.rali.gate.jape.parser.*;
import gate.*;
import gate.annotation.*;
import gate.util.*;
import gate.creole.*;

/**
  * Second test harness for JAPE.
  * Uses the Sheffield Tokeniser and Gazetteer, and must be run
  * from the gate directory.
  * @author Hamish Cunningham
  */
public class TestJape2 {

  /** Debug flag */
  private static final boolean DEBUG = false;

  /** How much noise to make. */
  static private boolean verbose = false;


  /** Take a list of text files and a collection name, and
    * call tokeniser/gazetteer/jape on them, creating the
    * collection.
    */
  static public void main(String[] args) {

    // turn debug output on/off
    //Debug.setDebug(true);
    //Debug.setDebug(AnnotationSet.class, true);
    //Debug.setDebug(BasicPatternElement.class, true);
    //Debug.setDebug(ComplexPatternElement.class, true);
    //Debug.setDebug(ConstraintGroup.class, true);
    //Debug.setDebug(SinglePhaseTransducer.class, true);

    // variables to parse the command line options into
    String collName = null;
    String japeName = null;
    ArrayList fileNames = null;

    // process options
    for(int i=0; i<args.length; i++) {
      if(args[i].equals("-c") && ++i < args.length) // -c = coll name
        collName = args[i];
      else if(args[i].equals("-j") && ++i < args.length) // -j: .jape name
        japeName = args[i];
      else if(args[i].equals("-v")) // -v = verbose
        verbose = true;
      else { // a list of files
        fileNames = new ArrayList();
        do {
          fileNames.add(args[i++]);
        } while(i < args.length);
      }
    } // for each arg

    // did they give valid options?
    message("checking options");
    if(collName == null || japeName == null || fileNames == null)
      usage("you must supply collection, transducer and file names");

    // create a collection and run the tokeniser
    message("creating coll, tokenising and gazetteering");
    Corpus coll = null;
    try {
      coll = tokAndGaz(collName, fileNames);
    } catch(ResourceInstantiationException e) {
      usage("couldn't open collection: " + e);
    }
/*
    // run the parser test
    message("parsing the .jape file (or deserialising the .ser file)");
    Batch batch = null;
    try { batch = new Batch(japeName);
    } catch(JapeException e) {
      usage("can't create transducer " + e.getMessage());
    }
*/
    /*Transducer transducer = parseJape(japeName);
    //Out.println(transducer);
    if(transducer == null)
      System.exit(1);*/

    // test the transducers from the parser
/*
    message("running the transducer");
    try { batch.transduce(coll); } catch(JapeException e) {
      usage("couldn't run transducer " + e.getMessage());
    }
    //runTransducer(transducer, coll);
    //Out.println(transducer);

    message("done\n\r");
    //System.exit(0);
*/
  } // main


  /**
    * Create a collection and put tokenised and gazetteered docs in it.
    */
  static public Corpus tokAndGaz(String collName, ArrayList fileNames)
  throws ResourceInstantiationException {

    // create or overwrite the collection
    Corpus collection = null;
    File collDir = new File(collName);
    collection = Factory.newCorpus(
      collDir.getAbsolutePath()
    );

    // add all the documents
    for(Iterator i = fileNames.iterator(); i.hasNext(); ) {
      String fname = (String) i.next();

      File f = new File(fname);
      FeatureMap attrs = Factory.newFeatureMap();
      Document doc = null;

      try {
        AnnotationSet annots = new AnnotationSetImpl(doc);
        collection.add(
          Factory.newDocument(f.getAbsolutePath())
        );
      } catch(ResourceInstantiationException e) {
        e.printStackTrace();
      }

      /*
      // Tokenise the document
      Tokeniser tokeniser = new Tokeniser(doc, Tokeniser.HMM);
      try { tokeniser.hmmTokenSequence(); }
      catch(sheffield.creole.tokeniser.ParseException ex) {
        ex.printStackTrace();
        return null;
      } catch (CreoleException ex) {
        ex.printStackTrace();
        return null;
      }

      // Gazetteer the document
      gate.creole.Annotator gazetteer = new GazetteerAnnotator();
      gazetteer.annotate(doc, null);
      */
    } // for each doc name

    // return the annotated collection
    return collection;

  } //tokAndGaz


  /**
    * Must be run from the gate directory.
    * Parse the .jape file.
    */
    /*
    static public Transducer parseJape(String japeName) {
    Transducer transducer = null;

    if(japeName.endsWith(".ser")) { // it's compiled already
      message("deserialising " + japeName);
      File f = new File(japeName);
      if(! f.exists())
        Out.println(japeName + " not found");

      try {
        FileInputStream fis = new FileInputStream(f.getPath());
        ObjectInputStream ois = new ObjectInputStream(fis);
        transducer = (Transducer) ois.readObject();
        ois.close();
      } catch (Exception ex) {
        Err.println(
          "Can't read from " + f.getName() + ": " + ex.toString()
        );
      }
    } else { // parse it
      message("parsing " + japeName);
      try {
        ParseCpsl cpslParser = new ParseCpsl(japeName);
        transducer = cpslParser.MultiPhaseTransducer();
      } catch(IOException e) {
        e.printStackTrace();
      } catch(ca.umontreal.iro.rali.gate.jape.parser.ParseException ee) {
        Err.println("Error parsing transducer: " + ee.getMessage());
      }
    }

    return transducer;
  } // parseJape


  static public void runTransducer(
    Transducer transducer, Corpus coll
  ) {

    try {
      Document doc = coll.firstDocument();
      do {
        message("doing document " + doc.getId());
        transducer.transduce(doc);
        // Out.println(transducer.toString());
      } while( (doc = coll.nextDocument()) != null );
    } catch(JdmException e) {
      e.printStackTrace();
    } catch(JapeException e) {
      e.printStackTrace();
    }
  } // runTransducer
  */

  /** You got something wrong, dumbo. */
  public static void usage(String errorMessage) {
    String usageMessage =
      "usage: java ca.umontreal.iro.rali.gate.jape.TestJape2.main [-v] " +
        "-j JapePatternFile -c CollectionName FileName(s)";

    Err.println(errorMessage);
    Err.println(usageMessage);
    //System.exit(1);

  } // usage


  /** Hello? Anybody there?? */
  public static void message(String mess) {
    if(verbose) Out.println("TestJape2: " + mess);
  } // message

} // class TestJape2


