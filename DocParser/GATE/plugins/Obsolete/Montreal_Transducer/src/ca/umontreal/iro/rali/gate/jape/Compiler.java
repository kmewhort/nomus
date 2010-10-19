/*
 *  Compiler.java - compile .jape files
 *
 *  Copyright (c) 1995-2010, The University of Sheffield. See the file
 *  COPYRIGHT.txt in the software or at http://gate.ac.uk/gate/COPYRIGHT.txt
 *
 *  This file is part of GATE (see http://gate.ac.uk/), and is free
 *  software, licenced under the GNU Library General Public License,
 *  Version 2, June 1991 (in the distribution as file licence.html,
 *  and also available at http://gate.ac.uk/gate/licence.html).
 *
 *  Minor modifications made by Luc Plamondon, Université de Montréal, 27/11/03:
 *  - migrated original file to the ca.umontreal.iro.rali.gate.jape package.
 *
 *  Hamish Cunningham, 23/02/2000
 *
 *  $Id$
 */

package ca.umontreal.iro.rali.gate.jape;

import java.io.*;
import java.util.*;

import gate.util.*;
import gate.annotation.*;
import ca.umontreal.iro.rali.gate.jape.parser.*;

/**
  * Compiler for JAPE files.
  */
public class Compiler {

  /** Debug flag */
  private static final boolean DEBUG = false;

  /** How much noise to make. */
  static private boolean verbose = false;

  static String defaultEncoding = "UTF-8";

  /** Take a list of .jape files names and compile them to .ser.
    * Also recognises a -v option which makes it chatty.
    */
  static public void main(String[] args) {

    // process options
    int argsIndex = 0;
    while(args[argsIndex].toCharArray()[0] == '-')
      if(args[argsIndex++].equals("-v"))
        verbose = true;

    // construct list of the files
    ArrayList fileNames = new ArrayList();
    for( ; argsIndex<args.length; argsIndex++)
      fileNames.add(args[argsIndex]);

    // compile the files
    compile(fileNames);

    message("done");
  } // main

  /** The main compile method, taking a file name. */
  static public void compile(String japeFileName, String encoding) {
    // parse
    message("parsing " + japeFileName);
    Transducer transducer = null;
    try {
      transducer = parseJape(japeFileName, encoding);
    } catch(JapeException e) {
      emessage("couldn't compile " + japeFileName + ": " + e);
      return;
    }

    // save
    message("saving " + japeFileName);
    try {
      saveJape(japeFileName, transducer);
    } catch (JapeException e) {
      emessage("couldn't save " + japeFileName + ": " + e);
    }

    message("finished " + japeFileName);
  } // compile(String japeFileName)

  /** The main compile method, taking a list of file names. */
  static public void compile(ArrayList fileNames) {
    // for each file, compile and save
    for(Iterator i = fileNames.iterator(); i.hasNext(); )
      compile((String) i.next(), defaultEncoding);
  } // compile

  /** Parse a .jape and return a transducer, or throw exception. */
  static public Transducer parseJape(String japeFileName, String encoding)
  throws JapeException {
    Transducer transducer = null;

    try {
      ParseCpsl cpslParser = new ParseCpsl(new File(japeFileName).toURL(),
                                           encoding);
      transducer = cpslParser.MultiPhaseTransducer();
    } catch(ParseException e) {
      throw(new JapeException(e.toString()));
    } catch(IOException e) {
      throw(new JapeException(e.toString()));
    }

    return transducer;
  } // parseJape

  /** Save a .jape, or throw exception. */
  static public void saveJape(String japeFileName, Transducer transducer)
  throws JapeException {
    String saveName = japeNameToSaveName(japeFileName);

    try {
      FileOutputStream fos = new FileOutputStream(saveName);
      ObjectOutputStream oos = new ObjectOutputStream (fos);
      oos.writeObject(transducer);
      oos.close();
    } catch (IOException e) {
      throw(new JapeException(e.toString()));
    }
  } // saveJape

  /** Convert a .jape file name to a .ser file name. */
  static String japeNameToSaveName(String japeFileName) {
    String base = japeFileName;
    if(japeFileName.endsWith(".jape") || japeFileName.endsWith(".JAPE"))
      base = japeFileName.substring(0, japeFileName.length() - 5);
    return base + ".ser";
  } // japeNameToSaveName

  /** Hello? Anybody there?? */
  public static void message(String mess) {
    if(verbose) Out.println("JAPE compiler: " + mess);
  } // message

  /** Ooops. */
  public static void emessage(String mess) {
    Err.println("JAPE compiler error: " + mess);
  } // emessage

} // class Compiler


