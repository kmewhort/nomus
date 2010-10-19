/*
 *  Batch.java - transducer class
 *
 *  Copyright (c) 1995-2010, The University of Sheffield. See the file
 *  COPYRIGHT.txt in the software or at http://gate.ac.uk/gate/COPYRIGHT.txt
 *
 *  This file is part of GATE (see http://gate.ac.uk/), and is free
 *  software, licenced under the GNU Library General Public License,
 *  Version 2, June 1991 (in the distribution as file licence.html,
 *  and also available at http://gate.ac.uk/gate/licence.html).
 *
 *  Hamish Cunningham, 10/08/98
 *
 *  Minor modifications by Luc Plamondon, Université de Montréal, 20/11/03:
 *  - migrated original file to the ca.umontreal.iro.rali.gate.jape package.
 *
 *  $Id$
 *
 *  DEVELOPER NOTES:
 *
 *  This is one that got away; the relation between constructors,
 *  initTransducer and parseTransducer are totally screwy and get worse
 *  every time I add something (e.g. support for resource loading).
 *  We should probably junk this whole thing and start again....
 */

package ca.umontreal.iro.rali.gate.jape;

import java.util.*;
import java.util.jar.*;
import java.io.*;
import java.net.*;

import gate.annotation.*;
import gate.util.*;
import gate.*;
import gate.event.*;
import gate.creole.*;

/** Batch processing of JAPE transducers against documents or collections.
  * Construction will parse or deserialise a transducer as required.
  */
public class Batch implements JapeConstants {
  /** Debug flag */
  private static final boolean DEBUG = false;

  /** The name of the transducer file, a .jape or .ser. */
//  private String japeFileName;

  /** The URL that points to a .jape file */
  private URL japeURL;

  /**The encoding used for reading the grammar file(s)*/
  private String encoding;

  /** The JAPE transducer. */
  private Transducer transducer;

  /** A stream connected to the JAPE file (often null). */
//  private InputStream japeStream = null;

  /** Create non-initialised instance (private, used in main). */
  private Batch() { }

  /** Create a fully initialised instance.
    * <P><CODE>japeFileName</CODE>: the name of a .jape or .ser transducer
    * file. This may be an absolute path, or may a .jar
    * that lives somewhere on the classpath.
    */
  public Batch(URL url, String encoding) throws JapeException {
    this.japeURL = url;
    this.encoding =  encoding;
    parseJape();
    if(transducer != null){
      transducer.addStatusListener(new StatusListener(){
        public void statusChanged(String text){
          fireStatusChanged(text);
        }
      });

      transducer.addProgressListener(new ProgressListener(){
        public void progressChanged(int value){
          fireProgressChanged(value);
        }

        public void processFinished(){
          fireProcessFinished();
        }
      });
    }

  } // full init constructor

  public Batch(URL url, String encoding, StatusListener sListener)
         throws JapeException {

    this.addStatusListener(sListener);
    this.japeURL = url;
    this.encoding =  encoding;
    parseJape();
    if(transducer != null){
      transducer.addStatusListener(new StatusListener(){
        public void statusChanged(String text){
          fireStatusChanged(text);
        }
      });

      transducer.addProgressListener(new ProgressListener(){
        public void progressChanged(int value){
          fireProgressChanged(value);
        }

        public void processFinished(){
          fireProcessFinished();
        }
      });
    }
  } // full init constructor

  /**
   * Notifies this PR that it should stop its execution as soon as possible.
   */
  public synchronized void interrupt(){
    transducer.interrupt();
  }
  /** Create a fully initialised instance.
    * <P><CODE>japeFileName</CODE>: the name of a .jape or .ser transducer
    * file. This may be an absolute path, or may a .jar
    * that lives somewhere on the classpath.
    */
/*
  public Batch(String japeFileName) throws JapeException {
    this.japeFileName = japeFileName;
    initTransducer();
  } // full init constructor
*/
/*
  public Batch(String japeFileName, StatusListener sListener)
                                                        throws JapeException {
    this.japeFileName = japeFileName;
    this.addStatusListener(sListener);
    initTransducer();
  } // full init constructor
*/

  /** Create a fully initialised instance from an InputStream connected
    * to the JAPE file.
    */
/*
  public Batch(InputStream japeStream) throws JapeException {
    if(japeStream == null)
      throw new JapeException(
        "attempt to create a batch parser with null input stream"
      );
    this.japeFileName = "stream";
    this.japeStream = japeStream;
    initTransducer();
  } // full init constructor
*/
  /** Create a fully initialised instance from a resource path and resource
    * name.
    */
/*
  public Batch(String resPath, String resName) throws JapeException {
    fromResource = true;
    this.japeFileName = resName;
    this.resPath = resPath;
    initTransducer();
  } // full init constructor
*/

  /** Get the transducer. */
  public Transducer getTransducer() { return transducer; }

  /** Instantiate transducer member as necessary. */
/*
  private void initTransducer()
  throws JapeException {
    if(fromResource) {
      parseJape(resPath, japeFileName);
    } else if(japeFileName.endsWith(".ser") || japeFileName.endsWith(".SER"))
      deserialiseJape(new File(japeFileName));
    else if(japeFileName.endsWith(".jape") || japeFileName.endsWith(".JAPE"))
      parseJape();
    else if(japeFileName.endsWith(".jar") || japeFileName.endsWith(".JAR"))
      deserialiseJape();
    else if(japeFileName.equals("stream"))
      parseJape(japeStream);
    else
      throw new JapeException(
        "unknown file type (not .jape, .ser or .jar):" + japeFileName
      );
    if(transducer != null) transducer.addStatusListener(new StatusListener() {
      public void statusChanged(String text){
        fireStatusChangedEvent(text);
      }
    });
  }
*/
  /** Parse a jape file from {@link #japeURL} and store the transducer. */
  private void parseJape() throws JapeException {
    try {
      ca.umontreal.iro.rali.gate.jape.parser.ParseCpsl parser =
        new ca.umontreal.iro.rali.gate.jape.parser.ParseCpsl(japeURL, encoding);

      StatusListener listener = null;
      listener = new StatusListener(){
        public void statusChanged(String text){
          fireStatusChanged(text);
        }
      };
      parser.addStatusListener(listener);
      transducer = parser.MultiPhaseTransducer();
      parser.removeStatusListener(listener);
    } catch (ca.umontreal.iro.rali.gate.jape.parser.ParseException e) {
      throw new
        JapeException("Batch: error parsing transducer: " + e.getMessage());
    } catch (java.io.IOException e) {
      throw new
        JapeException("Batch: couldn't open JAPE file: " + e.getMessage());
    }
  } // parseJape

  /** Parse a jape file from an InputStream and store the transducer. */
/*
  private void parseJape(InputStream japeStream) throws JapeException {
    try {
      ca.umontreal.iro.rali.gate.jape.parser.ParseCpsl parser =
        new ca.umontreal.iro.rali.gate.jape.parser.ParseCpsl(japeFileName, japeStream);
      transducer = parser.MultiPhaseTransducer();
    } catch (ca.umontreal.iro.rali.gate.jape.parser.ParseException e) {
      throw new
        JapeException("Batch: error parsing transducer: " + e.getMessage());
    } catch (java.io.IOException e) {
      throw new
        JapeException("Batch: couldn't read JAPE stream: " + e.getMessage());
    }
  } // parseJape(InputStream)
*/
  /** Parse a jape file from a resource and store the transducer. */
/*
  private void parseJape(String resPath, String resName) throws JapeException {
    try {
      ca.umontreal.iro.rali.gate.jape.parser.ParseCpsl parser =
        new ca.umontreal.iro.rali.gate.jape.parser.ParseCpsl(resPath, resName);
      transducer = parser.MultiPhaseTransducer();
    } catch (ca.umontreal.iro.rali.gate.jape.parser.ParseException e) {
      throw new
        JapeException("Batch: error parsing transducer: " + e.getMessage());
    } catch (java.io.IOException e) {
      throw new
        JapeException("Batch: couldn't read JAPE resource: " + e.getMessage());
    }
  } // parseJape(resPath, resName)
*/

  /** Deserialise from a .ser file. */
/*
  private void deserialiseJape(File japeFile) throws JapeException {

    // set up a file input stream
    FileInputStream japeInputStream = null;
    try {
      japeInputStream = new FileInputStream(japeFile.getPath());
    } catch (IOException e) {
      throw new JapeException(
        "Can't read from " + japeFile.getPath() + ": " + e.getMessage()
      );
    }

    // call the input stream deserialise method
    deserialiseJape(japeInputStream);
  } // deserialiseJape(File)
*/
  /** Deserialise from a JAR file. */
/*
  private void deserialiseJape() throws JapeException {
    // find the jar from CLASSPATH
    //SearchPath classPath =
    //  new SearchPath(System.getProperty("java.class.path"), ".");
    File jarFile = new File(japeFileName); //classPath.getFile(japeFileName);
    if(jarFile == null)
      throw new JapeException("Batch: can't find " + japeFileName);

    // get a byte array input stream with the .ser in out of the jar file
    JarFile jar = null;
    BufferedInputStream japeInputStream = null;
    try {
      jar = new JarFile(jarFile.getPath());
      japeInputStream = new BufferedInputStream(
        jar.getInputStream(jar.getJarEntry(jarNameToSerName(japeFileName)))
      );
    } catch(IOException e) {
      throw new JapeException("couldn't read jar file " + japeFileName);
    }


    // call the input stream deserialise method
    deserialiseJape(japeInputStream);
  } // deserialiseJape()
*/
  /** Create a transducer from an object input stream (deserialisation). */
/*
  private void deserialiseJape(InputStream japeInputStream)
  throws JapeException {
    try {
      ObjectInputStream ois = new ObjectInputStream(japeInputStream);
      transducer = (Transducer) ois.readObject();
      ois.close();
      japeInputStream.close(); // redundant?
    } catch (IOException e) {
      throw new JapeException(
        "Batch: can't deserialise InputStream (1): " + e.getMessage()
      );
    } catch (ClassNotFoundException e) {
      throw new JapeException(
        "Batch: can't deserialise InputStream (2): " + e.getMessage()
      );
    }
  } // deserialise(OIS)
*/
  /** Create a .ser name from a .jar name. */
/*
  private String jarNameToSerName(String jarName) {
    return jarName.substring(0, jarName.length() - 4) + ".ser";
  } // jarNameToSerName
*/

  /** Process the given collection. */
  public void transduce(Corpus coll) throws JapeException, ExecutionException {
    // for each doc run the transducer
    Iterator iter = coll.iterator();
    while(iter.hasNext()) {
      Document doc = (Document) iter.next();
      // transducer.transduce(doc);
      transduce(doc, doc.getAnnotations(), doc.getAnnotations());
    }
  } // transduce(coll)

  /** Process a single document. */
  public void transduce(Document doc) throws JapeException, ExecutionException {
    transducer.transduce(doc, doc.getAnnotations(), doc.getAnnotations());
  } // transduce(doc)

  /** Process a single document. */
  public void transduce(Document doc, AnnotationSet inputAS,
                        AnnotationSet outputAS) throws JapeException,
                                                       ExecutionException {
    //no need to transduce empty document
    if (inputAS == null || inputAS.isEmpty())
      return;
    transducer.transduce(doc, inputAS, outputAS);

  } // transduce(doc)

  /** Process a single text. */
/*
  public Document transduce(String text) throws JapeException {
    Document doc = null;
    try {
      doc = Factory.newDocument(text);
    } catch (ResourceInstantiationException e) {
      throw new JapeException(e.toString());
    }
    transducer.transduce(doc, doc.getAnnotations());
    return doc;
  } // transduce(text)
*/
  /** Process a single file. */
/*
  public Document transduce(File textFile) throws JapeException {
    String text = null;
    try {
      text = gate.util.Files.getString(textFile);
    } catch(IOException e) { throw new JapeException(e.toString()); }
    return transduce(text);
  } // transduce(textFile)
*/
  /** Process a set of files. */
/*
  public Corpus transduce(String[] textFileNames) throws JapeException {
    Corpus coll = null;
    try {
      coll = Factory.newCorpus("JAPE batch corpus");
      Document doc = null;
      for(int i = 0; i < textFileNames.length; i++) {
          doc = Factory.newDocument(textFileNames[i]);
          doc.setFeatures(Factory.newFeatureMap());
          /*coll.createDocument(
            textFileNames[i],
            null, // the text - should get read from disk
            new AnnotationSetImpl(doc),
            Factory.newFeatureMap(),
            Document.COPIED
          );*/
/*
        transducer.transduce(doc, doc.getAnnotations());
      }
    } catch(ResourceInstantiationException e) {
      throw new JapeException(e.toString());
    }
    return coll;
  } // transduce(textFileNames)
*/
  /** This is where it all happens. This is <I>the</I> place to be. Take
    * your summer holidays here. Visit on Saturday nights. Buy a season
    * ticket from <CODE>www.programmer.gone.insane.com</CODE>.
    * <P>
    * Takes a .jape/.jar/.ser
    *  file name (-j option) which is assumed to hold a pattern
    * grammar for a multi-phase transducer, and a collection
    * name (-c option) or a list of files. As needed it then parses and
    * compiles the transducer, then transduces all the documents in the
    * collection and saves it to disk.
    */
  public static void main(String args[]) {
/*
    // oh great bug in the sky give us this day our daily fuckup
    //gate.util.Debug.setDebug(true);
    //gate.util.Debug.setDebug(Rule.class, true);
    //gate.util.Debug.setDebug(LeftHandSide.class, true);
    //gate.util.Debug.setDebug(BasicPatternElement.class, true);
    //gate.util.Debug.setDebug(AnnotationSet.class, true);

    // The persistent name of the collection.
    String persCollName = null;;

    // The collection to process.
    Corpus collection = null;

    // create one of us
    Batch batch = new Batch();

    // process the options
    int i = 0;
    for( ; i<args.length; i++) {
      if(args[i].equals("-c") && ++i < args.length) // -c = coll name
        persCollName = args[i];
      else if(args[i].equals("-j") && ++i < args.length)// -j = transducer name
        batch.japeFileName = args[i];
      else if(args[i].equals("-v")) // -v = verbose
        batch.setVerbose(true);
      else if(args[i].startsWith("-"))
        batch.usage("unknown option " + args[i]);
      else
        break;
    } // for each arg

    // file name list
    String[] fileNames = null;
    if(args.length > i) {
      fileNames = new String[args.length - i];
      for(int j = 0; i<args.length; j++, i++)
        fileNames[j] = args[i];
    }

    // did they give valid options?
    if(batch.japeFileName == null)
      batch.usage("you must supply a transducer name");
    if(fileNames != null && persCollName != null)
      batch.usage("can't read a collection AND process a file list");

    // parse the transducer or bomb
    batch.message("parsing the transducer");
    try { batch.initTransducer(); }
    catch(JapeException e) {
      batch.usage("oops: " + e.toString());
    }

    Corpus coll = null;
    if(persCollName != null) { // we got a collection name, not a list of files

      // open the collection or bomb
      coll = null;
      batch.message("opening the collection");
      try {
        coll = Factory.newCorpus(persCollName);
      } catch(ResourceInstantiationException e) {
        batch.usage("oops (x): " + e);
      }

      // transduce
      batch.message("calling transducer");
      try { batch.transduce(coll); }
      catch(JapeException e) {
        batch.usage("oops (1): " + e.toString());
      }

      // save to disk
      batch.message("saving the collection");
      batch.usage("couldn't sync coll ");

    // we got a list of files, not a collection
    } else {
      batch.message("transducing transient collection");
      try {
        coll = batch.transduce(fileNames);
      } catch(JapeException e) {
        batch.usage("oops (2): " + e.toString());
      }
    }

    // we won! we won! we can smash up all the computers now!
    batch.message("done");
    //System.exit(0);
*/
  } // main


  /** Whether to print progress messages or not. */
  private boolean verbose = false;

  /** Set verbosity. */
  public void setVerbose(boolean turtleSoup) { verbose = turtleSoup; }

  /** You got something wrong, dumbo. */
  public void usage(String errorMessage) {
    String usageMessage =
      "usage: java ca.umontreal.iro.rali.gate.jape.Batch.main [-v] " +
        "-j japefile(.ser|.jape|.jar) " +
        "(-c CollectionName | filenames)";

    Err.println(errorMessage);
    Err.println(usageMessage);
    // System.exit(1);

  } // usage

  /** Hello? Anybody there?? */
  public void message(String mess) {
    if(verbose) Out.println("Batch: " + mess);
  } // message

  public void setFeatures(gate.FeatureMap newFeatures) {
    features = newFeatures;
  }
  public gate.FeatureMap getFeatures() {
    return features;
  }
  public synchronized void removeProgressListener(ProgressListener l) {
    if (progressListeners != null && progressListeners.contains(l)) {
      Vector v = (Vector) progressListeners.clone();
      v.removeElement(l);
      progressListeners = v;
    }
  }
  public synchronized void addProgressListener(ProgressListener l) {
    Vector v = progressListeners == null ? new Vector(2) : (Vector) progressListeners.clone();
    if (!v.contains(l)) {
      v.addElement(l);
      progressListeners = v;
    }
  }

  //ProcessProgressReporter implementation ends here

  /** Are we initialising from a resource? */
//  private boolean fromResource = false;

  /** Path to the resources tree */
//  private String resPath = null;


  private gate.FeatureMap features;
  private transient Vector progressListeners;
  private transient Vector statusListeners;

  protected void fireProgressChanged(int e) {
    if (progressListeners != null) {
      Vector listeners = progressListeners;
      int count = listeners.size();
      for (int i = 0; i < count; i++) {
        ((ProgressListener) listeners.elementAt(i)).progressChanged(e);
      }
    }
  }
  protected void fireProcessFinished() {
    if (progressListeners != null) {
      Vector listeners = progressListeners;
      int count = listeners.size();
      for (int i = 0; i < count; i++) {
        ((ProgressListener) listeners.elementAt(i)).processFinished();
      }
    }
  }
  public synchronized void removeStatusListener(StatusListener l) {
    if (statusListeners != null && statusListeners.contains(l)) {
      Vector v = (Vector) statusListeners.clone();
      v.removeElement(l);
      statusListeners = v;
    }
  }
  public synchronized void addStatusListener(StatusListener l) {
    Vector v = statusListeners == null ? new Vector(2) : (Vector) statusListeners.clone();
    if (!v.contains(l)) {
      v.addElement(l);
      statusListeners = v;
    }
  }
  protected void fireStatusChanged(String e) {
    if (statusListeners != null) {
      Vector listeners = statusListeners;
      int count = listeners.size();
      for (int i = 0; i < count; i++) {
        ((StatusListener) listeners.elementAt(i)).statusChanged(e);
      }
    }
  }

  /**
   * Sets the ontology to be used by the transducers
   * @param ontology
   */
  public void setOntology(gate.creole.ontology.Ontology ontology) {
    transducer.setOntology(ontology);
  }


  /*
  private void writeObject(ObjectOutputStream oos) throws IOException {
    Out.prln("writing batch");
    oos.defaultWriteObject();
    Out.prln("finished writing batch");
  } // writeObject
  */

} // class Batch

