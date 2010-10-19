/*
* AnnotationsByOffset.java
* Copyright:    Copyright (c) 2005, Ontotext Lab.
* Company:      Ontotext Lab.
* Krasimir Angelov 12/2005 */

package com.ontotext.gate.japec;

import java.io.*;
import java.net.*;
import java.util.*;
import gate.*;
import gate.util.*;
import gate.creole.*;

public class JapecTransducer extends AbstractLanguageAnalyser {
  private static final boolean DEBUG = false;

  private File grammarFile;
  private URL grammarURL;
  private File workDir;
  private String packageName;
  private String encoding;
  private String inputASName;
  private String outputASName;
  private gate.creole.ontology.Ontology ontology;
  private List phases;

  public JapecTransducer() {
    grammarURL   = null;
    workDir      = null;
    packageName  = null;
    encoding     = null;
    inputASName  = null;
    outputASName = null;
    ontology     = null;
    phases       = new ArrayList();
  }

  public Resource init() throws ResourceInstantiationException
  {
    //sanity checks
    if(grammarURL == null) throw new ResourceInstantiationException(
            "No URL provided for the grammar!");
    //Japec can only read file: URLs
    if(!grammarURL.getProtocol().equals("file"))
      throw new ResourceInstantiationException(
              "Japec JAPE Compiler only supports file URLs for input!");

    loadGrammar();
    return this;
  }

  public void reInit() throws ResourceInstantiationException
  {
    loadGrammar();
  }

  protected void loadGrammar() throws ResourceInstantiationException
  {
    try{
      String gateSym = Gate.genSym();

      //create the temporary work directory
      workDir = File.createTempFile("japec-workdir", gateSym);
      workDir.delete();
      workDir.mkdir();
      workDir.deleteOnExit();

      grammarFile = Files.fileFromURL(grammarURL);
      if(DEBUG) {
        System.err.println("Loading grammar from file " + grammarFile);
      }

      packageName = "com.ontotext.gate.japec.grammar" + gateSym;

      if(DEBUG) {
        System.err.println("Generating classes into package " + packageName);
      }

      compileGrammar();

      Files.rmdir(workDir);
    } catch(ResourceInstantiationException rie) {
      throw rie;
    }catch(Exception e){
      throw new ResourceInstantiationException(e);
    }
  }

  public void execute() throws ExecutionException {
    for (int i = 0; i < phases.size(); i++)
    {
      SinglePhaseTransducer phase = (SinglePhaseTransducer) phases.get(i);
      phase.setDocument(document);
      phase.setInputASName(inputASName);
      phase.setOutputASName(outputASName);
      phase.execute();
    }
  }

  protected void compileGrammar() throws IOException,
      ResourceInstantiationException, ClassNotFoundException, GateException,
      InstantiationException, IllegalAccessException
  {
    //compile the JAPE file(s) to Java

    String osname = System.getProperty("os.name").toLowerCase();

    // Find Jape_Compiler plugin directory from plugin ResourceData
    ResourceData japecRD =
      (ResourceData)Gate.getCreoleRegister().get(this.getClass().getName());
    URL japecCreoleXML = japecRD.getXmlFileUrl();
    if(!"file".equals(japecCreoleXML.getProtocol())) {
      throw new ResourceInstantiationException(
          "Jape_Compiler plugin must be loaded from a file: URL");
    }
    File japecCreoleXMLFile = Files.fileFromURL(japecCreoleXML);

    // search for Japec compiler binary.  We look first for japec (.exe on
    // Windows) in the Jape_Compiler plugin directory.  If that does not exist,
    // we try the OS-specific name (japec-windows.exe, japec-mac, etc).  If
    // neither of these exist, throw an exception.  Searching this way round
    // means a locally compiled japec will be found before the default
    // japec-platform version.
    File japecDir = japecCreoleXMLFile.getParentFile();
    String japecBinary = "japec";
    if(osname.indexOf("windows") >= 0) japecBinary += ".exe";

    File plainJapecPath = new File(japecDir, japecBinary);
    File japecPath;
    if(plainJapecPath.exists()) {
      japecPath = plainJapecPath;
    }
    else {
      String suffix = null;
      if (osname.indexOf("windows") >= 0) suffix = "-windows.exe";
      else if (osname.indexOf("linux") >= 0) suffix = "-linux";
      else if (osname.indexOf("mac") >= 0) suffix = "-mac";
      else throw new ResourceInstantiationException(
          "Could not find Japec compiler - should be at " + plainJapecPath);

      japecPath = new File(japecDir, "japec" + suffix);

      if(!japecPath.exists()) {
        throw new ResourceInstantiationException(
            "Could not find Japec compiler - should be at either "
            + plainJapecPath + " or " + japecPath);
      }
    }

    if(DEBUG) {
      Out.prln("Using compiler at " + japecPath);
    }

    Process p = Runtime.getRuntime().exec(new String[] {japecPath.getPath(),
                                                        grammarFile.getPath(),
                                                        "--odir", workDir.getPath(),
                                                        "--package", packageName
                                                       });

    BufferedReader error =
      new BufferedReader(new InputStreamReader(p.getErrorStream()));
    String line;
    while ((line = error.readLine()) != null) {
      Err.prln(line);
    }
    error.close();

    BufferedReader input =
      new BufferedReader(new InputStreamReader(p.getInputStream()));
    while ((line = input.readLine()) != null) {
      Err.prln(line);
    }
    input.close();



    //compile the generated classes and load the classes into the classloader
    Map sources = new LinkedHashMap();

    // phase class names are stored in a List to allow the same phase to be
    // used more than once in a multiphase transducer.
    List allPhaseClassNames = new ArrayList();

    //read the phases file
    File phasesFile = new File(workDir, "phases");
    phasesFile.deleteOnExit();
    BufferedReader phasesReader = new BufferedReader(
            new FileReader(phasesFile));
    for(line = phasesReader.readLine();
        line != null;
        line = phasesReader.readLine()){
      String phaseName = line.trim();
      String phaseClassName = packageName + "." + phaseName;
      File phaseFile = new File(workDir, phaseName + ".java");
      phaseFile.deleteOnExit();
      BufferedReader sourceReader = new BufferedReader(new FileReader(
              phaseFile));
      StringBuffer source = new StringBuffer();
      for(String sourceLine = sourceReader.readLine();
          sourceLine != null;
          sourceLine = sourceReader.readLine()){
        source.append(sourceLine);
        source.append(Strings.getNl());
      }

      sources.put(phaseClassName, source.toString());
      allPhaseClassNames.add(phaseClassName);
    }
    //call the Java compiler
    Javac.loadClasses(sources);

    // create the phases
    phases.clear();
    for(Iterator phaseClassNameIter = allPhaseClassNames.iterator();
        phaseClassNameIter.hasNext();){
      String phaseClassName = (String)phaseClassNameIter.next();
      Class phaseClass = Gate.getClassLoader().loadClass(phaseClassName);
      SinglePhaseTransducer phase = (SinglePhaseTransducer) phaseClass.newInstance();
      phase.setOntology(ontology);
      phases.add(phase);
    }
  }

  public URL getGrammarURL()
  {
    return grammarURL;
  }

  public void setGrammarURL(URL url)
  {
    grammarURL = url;
  }

  /**
   *
   * Sets the encoding to be used for reding the input file(s) forming the Jape
   * grammar. Note that if the input grammar is a multi-file one than the same
   * encoding will be used for reding all the files. Multi file grammars with
   * different encoding across the composing files are not supported!
   *
   * @param newEncoding
   *          a {link String} representing the encoding.
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

  public void setInputASName(String newInputASName) {
    inputASName = newInputASName;
  }

  public String getInputASName() {
    return inputASName;
  }

  public void setOutputASName(String newOutputASName) {
    outputASName = newOutputASName;
  }

  public String getOutputASName() {
    return outputASName;
  }

  public gate.creole.ontology.Ontology getOntology() {
    return ontology;
  }

  public void setOntology(gate.creole.ontology.Ontology newOntology) {
    this.ontology = newOntology;
  }
}
