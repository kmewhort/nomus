/*
 *  TestJape.java
 *
 *  Copyright (c) 1995-2010, The University of Sheffield. See the file
 *  COPYRIGHT.txt in the software or at http://gate.ac.uk/gate/COPYRIGHT.txt
 *
 *  This file is part of GATE (see http://gate.ac.uk/), and is free
 *  software, licenced under the GNU Library General Public License,
 *  Version 2, June 1991 (in the distribution as file licence.html,
 *  and also available at http://gate.ac.uk/gate/licence.html).
 *
 *  Hamish Cunningham, 23/Feb/00
 *
 *  Minor modifications made by Luc Plamondon, Université de Montréal, 27/11/03:
 *  - migrated original file from gate.jape to 
 *    ca.umontreal.iro.rali.gate.jape package.
 *
 *  $Id$
 */

package ca.umontreal.iro.rali.gate.jape;

import java.util.*;
import java.io.*;
import java.text.*;
import junit.framework.*;

import gate.*;
import gate.util.*;
import gate.annotation.*;
import gate.creole.tokeniser.*;
import gate.creole.gazetteer.*;
import gate.creole.*;



/** Tests for the Corpus classes
  */
public class TestJape extends TestCase
{
  /** Debug flag */
  private static final boolean DEBUG = false;

  /** Construction */
  public TestJape(String name) { super(name); }

  /** Fixture set up */
  public void setUp() {
    //Out.println("TestJape.setUp()");
  } // setUp

  /** Test using the large "combined" grammar from the gate/resources
    * tree.
    */
  public void _testCombined() throws IOException, GateException, Exception {
    DoTestBigGrammar("AveShort");

    /*
    Corpus c = Factory.newCorpus("TestJape corpus");
    c.add(
      Factory.newDocument(Files.getResourceAsString("texts/doc0.html"))
    );

    //add some annotations on the first (only) document in corpus c
    Document doc = (Document) c.first();
    AnnotationSet defaultAS = doc.getAnnotations();
    FeatureMap feat = Factory.newFeatureMap();
    defaultAS.add(new Long( 2), new Long( 4), "A",feat);
    defaultAS.add(new Long( 4), new Long(6), "B",feat);
    defaultAS.add(new Long(6), new Long(8), "C",feat);
    defaultAS.add(new Long(8), new Long(10), "C",feat);

    // run the parser test
    Gate.init();
    Batch batch = null;
    batch = new Batch("jape/combined/", "main.jape");

    // test the transducers
    batch.transduce(c);
    //Out.println(batch.getTransducer());

    // check the results
    doc = (Document)c.first();
    */
  } // testCombined()

  /** Batch run */
  public void testBatch() throws Exception{
    Corpus c = Factory.newCorpus("TestJape corpus");
    c.add(
      Factory.newDocument(Files.getGateResourceAsString("texts/doc0.html"))
    );
    //add some annotations on the first (only) document in corpus c
    Document doc = (Document)c.get(0);
    AnnotationSet defaultAS = doc.getAnnotations();

    try {
      FeatureMap feat = Factory.newFeatureMap();
      // defaultAS.add(new Long( 0), new Long( 2), "A",feat);
      defaultAS.add(new Long( 2), new Long( 4), "A",feat);
      // defaultAS.add(new Long( 4), new Long( 6), "A",feat);
      // defaultAS.add(new Long( 6), new Long( 8), "A",feat);
      defaultAS.add(new Long( 4), new Long(6), "B",feat);
      // defaultAS.add(new Long(10), new Long(12), "B",feat);
      // defaultAS.add(new Long(12), new Long(14), "B",feat);
      // defaultAS.add(new Long(14), new Long(16), "B",feat);
      // defaultAS.add(new Long(16), new Long(18), "B",feat);
      defaultAS.add(new Long(6), new Long(8), "C",feat);
      defaultAS.add(new Long(8), new Long(10), "C",feat);
      // defaultAS.add(new Long(22), new Long(24), "C",feat);
      // defaultAS.add(new Long(24), new Long(26), "C",feat);
    } catch(gate.util.InvalidOffsetException ioe) {
      ioe.printStackTrace(Err.getPrintWriter());
    }
/*
    // run the parser test
    Batch batch = null;
    // String japeFileName = "/gate/jape/Test11.jape";
    String japeFileName = Files.getResourcePath() + "/jape/TestABC.jape";
    // String japeFileName = "/gate/jape/Country.jape";
    InputStream japeFileStream = Files.getResourceAsStream(japeFileName);
    if(japeFileStream == null)
      throw new JapeException("couldn't open " + japeFileName);
*/
    Batch batch = new Batch(
            Files.getGateResource("/jape/TestABC.jape"), "UTF-8");
    // test code: print the first line of the jape stream
    // Out.println(
    //   new BufferedReader(new InputStreamReader(japeFileStream)).readLine()
    // );

    // test the transducers
    batch.transduce(c);
    // check the results
    doc = (Document)c.get(0);
    // defaultAS = doc.getAnnotations();
    // Out.println(defaultAS);
  } // testBatch()

  public void DoTestBigGrammar(String textName) throws GateException, Exception{
    long startCorpusLoad = 0, startCorpusTokenization = 0,
         startGazeteerLoad = 0, startLookup = 0,
         startJapeFileOpen = 0, startCorpusTransduce = 0,
         endProcess = 0;
    Out.print("Procesing " + textName + "...\n" +
                     "Started at: " + (new Date()) + "\n");
    startCorpusLoad = System.currentTimeMillis();
    Out.print("Loading corpus... ");
    Corpus corpus = Factory.newCorpus("Jape Corpus");
    try {
    corpus.add(Factory.newDocument(
        Files.getGateResourceAsString("jape/InputTexts/" + textName)));
    } catch(IOException ioe) {
      ioe.printStackTrace(Err.getPrintWriter());
    }

    if(corpus.isEmpty()) {
      Err.println("Missing corpus !");
      return;
    }

    //tokenize all documents
    gate.creole.tokeniser.DefaultTokeniser tokeniser = null;
    try {
      //create a default tokeniser
      FeatureMap params = Factory.newFeatureMap();
      tokeniser = (DefaultTokeniser) Factory.createResource(
                            "gate.creole.tokeniser.DefaultTokeniser", params);
      /*Files.getResourceAsStream("creole/tokeniser/DefaultTokeniser.rules"));*/
    } catch(ResourceInstantiationException re) {
      re.printStackTrace(Err.getPrintWriter());
    }
    startCorpusTokenization = System.currentTimeMillis();
    Out.print(": " +
                       (startCorpusTokenization - startCorpusLoad) +
                       "ms\n");

    Out.print("Tokenizing the corpus... ");
    int progress = 0;
    int docCnt = corpus.size();
    Iterator docIter = corpus.iterator();
    Document currentDoc;
    while(docIter.hasNext()){
      currentDoc = (Document)docIter.next();
      tokeniser.setDocument(currentDoc);
      //use the default anotation set
      tokeniser.setAnnotationSetName(null);
      tokeniser.execute();
      // Verfy if all annotations from the default annotation set are consistent
      gate.corpora.TestDocument.verifyNodeIdConsistency(currentDoc);
    }

    startJapeFileOpen = System.currentTimeMillis();
    Out.print(": " + (startJapeFileOpen - startCorpusTokenization) +
                     "ms\n");

    //Do gazeteer lookup
    gate.creole.gazetteer.DefaultGazetteer gazeteer = null;
    startGazeteerLoad = startLookup = System.currentTimeMillis();
    Out.print("Loading gazeteer lists...");
    try {
      //create a default gazetteer
      FeatureMap params = Factory.newFeatureMap();
      gazeteer = (DefaultGazetteer) Factory.createResource(
                            "gate.creole.gazetteer.DefaultGazetteer", params);
      gazeteer.init();
      startLookup = System.currentTimeMillis();
      Out.print(": " +
                         (startLookup - startGazeteerLoad) +
                         "ms\n");

      Out.print("Doing gazeteer lookup... ");
      docIter = corpus.iterator();
      while(docIter.hasNext()){
        currentDoc = (Document)docIter.next();
        gazeteer.setDocument(currentDoc);
        gazeteer.execute();
        // Verfy if all annotations from the default annotation set are consistent
        gate.corpora.TestDocument.verifyNodeIdConsistency(currentDoc);
      }
    } catch(ResourceInstantiationException re) {
      Err.println("Cannot read the gazeteer lists!" +
                         "\nAre the Gate resources in place?\n" + re);
    }

    startJapeFileOpen = System.currentTimeMillis();
    Out.print(": " + (startJapeFileOpen - startLookup) +
                     "ms\n");


    //do the jape stuff
    Gate.init();


    try {
      Out.print("Opening Jape grammar... ");
      Batch batch = new Batch(
              Files.getGateResource("/jape/combined/main.jape"), "UTF-8");
      /*
      Batch batch = new Batch("jape/combined/", "brian-soc-loc1.jape");
      Batch batch =
        new Batch("z:/gate/src/gate/resources/jape/combined/main.jape");
      Batch batch = new Batch("jape/", "Country.jape");
      */
      startCorpusTransduce = (new Date()).getTime();
      Out.print(": " + (startCorpusTransduce - startJapeFileOpen) +
                       "ms\n");
      Out.print("Transducing the corpus... ");
      batch.transduce(corpus);
      endProcess = System.currentTimeMillis();
      Out.print(": " + (endProcess - startCorpusTransduce) + "ms\n");
    } catch(JapeException je) {
      je.printStackTrace(Err.getPrintWriter());
    }
  } // DoBugTestGrammar

  /**
   * This test sets up a JAPE transducer based on a grammar
   * (RhsError.jape) that will throw a null pointer exception.
   * The test succeeds so long as we get that exception.
   */
  public void testRhsErrorMessages() {
    boolean gotException = false;

    try {
      if(DEBUG) {
        Out.print(
          "Opening Jape grammar... " + Gate.getUrl("tests/RhsError.jape")
        );
      }
      // a JAPE batcher
      Batch batch = new Batch(Gate.getUrl("tests/RhsError.jape"), "UTF-8");

      // a document with an annotation
      Document doc = Factory.newDocument("This is a Small Document.");
      FeatureMap features = Factory.newFeatureMap();
      features.put("orth", "upperInitial");
      doc.getAnnotations().add(new Long(0), new Long(8), "Token", features);

      // run jape on the document
      batch.transduce(doc);
    } catch(Exception e) {
      if(DEBUG) Out.prln(e);
      gotException = true;
    }

    assertTrue("Bad JAPE grammar didn't throw an exception", gotException);

  }  // testRhsErrorMessages

//  /**
//   * This test sets up a JAPE transducer based on a grammar
//   * (RhsError2.jape) that will throw a compiler error.
//   * The test succeeds so long as we get that exception.
//   */
//  public void testRhsErrorMessages2() {
//    boolean gotException = false;
//
//    // disable System.out so that the compiler can't splash its error on screen
//    if(DEBUG) System.out.println("hello 1");
//    PrintStream sysout = System.out;
//    System.setOut(new PrintStream(new ByteArrayOutputStream()));
//    if(DEBUG) System.out.println("hello 2");
//
//    // run a JAPE batch on the faulty grammar
//    try {
//      if(DEBUG) {
//        Out.print(
//          "Opening Jape grammar... " + Gate.getUrl("tests/RhsError2.jape")
//        );
//      }
//      // a JAPE batcher
//      Batch batch = new Batch(Gate.getUrl("tests/RhsError2.jape"), "UTF-8");
//    } catch(Exception e) {
//      if(DEBUG) Out.prln(e);
//      gotException = true;
//    } finally {
//
//      // re-enable System.out
//      System.setOut(sysout);
//      if(DEBUG) System.out.println("hello 3");
//    }
//
//    assertTrue("Bad JAPE grammar (2) didn't throw an exception", gotException);
//
//  }  // testRhsErrorMessages2
//

  /** Test suite routine for the test runner */
  public static Test suite() {
    return new TestSuite(TestJape.class);
  } // suite

  //main method for running this test as a standalone test
  public static void main(String[] args) {
    for(int i = 0; i < 6; i++){
    System.gc();
    Out.println("Run " + i + "   ==============");
      try{
        TestJape testJape = new TestJape("Test Jape");
        testJape.setUp();
        if(args.length < 1) testJape.DoTestBigGrammar("AveShort");
       else testJape.DoTestBigGrammar(args[0]);
      } catch(Exception e) {
        e.printStackTrace(Err.getPrintWriter());
      }
    }
  }
} // class TestJape
