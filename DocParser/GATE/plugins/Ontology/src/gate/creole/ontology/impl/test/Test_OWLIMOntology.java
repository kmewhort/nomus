/*
 *  TestOntologyAPI.java
 *
 *  Niraj Aswani, 09/March/07
 *
 *  $Id: Test_OWLIMOntology.java 12602 2010-05-06 14:28:51Z ian_roberts $
 */
package gate.creole.ontology.impl.test;

import gate.creole.ontology.*;
import gate.util.GateException;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.Set;

import gate.Factory;
import gate.FeatureMap;
import gate.Gate;
import gate.creole.ResourceInstantiationException;
import gate.creole.ontology.OConstants.Closure;
import java.io.FileOutputStream;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;
import org.apache.log4j.Logger;

/**
 * Run all the essential regression tests for the OWLIMOntology LR.s
 */
public class Test_OWLIMOntology extends TestCase {
  public static void main(String[] args) throws GateException, MalformedURLException {
    System.out.println("Running main");
    junit.textui.TestRunner.run(Test_OWLIMOntology.class);
  }

  public Test_OWLIMOntology(String arg0) throws GateException, MalformedURLException {
    super(arg0);
  }

  File ontologiesDir = null;
  File configDir = null;
  File tmpDir = null;
  // TODO: it seems we cannot use a static as intended here: the
  // init code still gets run for each fixture?
  static boolean isInitialized = false;
  Logger log =  Logger.getLogger(this.getClass());
  OConstants.Closure DIRECT_CLOSURE = Closure.DIRECT_CLOSURE;
  OConstants.Closure TRANSITIVE_CLOSURE = Closure.TRANSITIVE_CLOSURE;

  // global preparation stuff - check if stuff already initialized, if
  // yes, do nothing
  protected void init() throws GateException, MalformedURLException {
    if(!isInitialized) {
    System.out.println("Inititalizing ...");
    Gate.init();
    File pluginHome =
        new File(new File(Gate.getGateHome(), "plugins"),
                 "Ontology");
    System.out.println("Plugin home directory is "+pluginHome.getAbsolutePath());
    Gate.getCreoleRegister().registerDirectories(
            pluginHome.toURI().toURL());
    File testingDir = new File(pluginHome,"test");
    assertTrue(testingDir.exists());
    ontologiesDir = new File(testingDir, "ontologies");
    assertTrue(ontologiesDir.exists());
    tmpDir = getUniqueTmpDir();
    assertTrue(tmpDir.canWrite());
    System.out.println("Init complete");
    } else {
      isInitialized = true;
    }
  }


  /**
   * per-test set up stuff
   * @throws Exception
   */
  protected void setUp() throws Exception {
    super.setUp();
    init();
  }

  protected void tearDown() throws Exception {
    super.tearDown();
  }

  // testLoadImports: test the loading of an existing ontology, finding the ontology
  // URI, import mappings, imports etc.
  public void testLoadImports() throws MalformedURLException,
          ResourceInstantiationException {
    FeatureMap fm = Factory.newFeatureMap();
    File demoFile = new File(ontologiesDir,"protonu.owl");
    URL rdfXmlURL = demoFile.toURI().toURL();
    fm.put("rdfXmlURL", rdfXmlURL);
    fm.put("baseURI", "");
    File mappingsFile = new File(ontologiesDir,"mappingsfile1.txt");
    URL mappingsFileURL = mappingsFile.toURI().toURL();
    fm.put("mappingsURL",mappingsFileURL);
    fm.put("persistent","false");
    fm.put("loadImports", "true");
    Ontology ontology = (Ontology)Factory.createResource(
            "gate.creole.ontology.impl.sesame.OWLIMOntology", fm);
    // we should now have the protou ontology loaded and the protons ontology
    // imported from the local directory. The protont import should have
    // been ignored, as specified in the mappings file.
    Set<OClass> allClasses = ontology.getOClasses(false);
    //for(OClass aClass : allClasses ) {
    //  System.out.println("Class: "+aClass+" / "+aClass.getONodeID().toTurtle());
    //}
    // Check if the Base URI was set from the loaded ontology
    System.out.println("Found default nameSpace: "+ontology.getDefaultNameSpace());
    assertEquals("Default Name Space",
        "http://proton.semanticweb.org/2005/04/protonu#",
        ontology.getDefaultNameSpace());
    //System.out.println("Found ontology URIs: "+ontology.getOntologyURIs());
    // make sure we find classes from protonu and protont, but not protons
    OClass c1 = ontology.getOClass(ontology.createOURIForName("SportEvent"));
    assertNotNull(c1);
    ontology.cleanup();
  }


  // testAccessOldNew: test ontology methods that were already present in the old
  // implementation - output measurements and do some benchmarking so
  // we can compare.
  public void testAccessOldNew() throws MalformedURLException,
          ResourceInstantiationException {
    FeatureMap fm = Factory.newFeatureMap();
    File demoFile = new File(ontologiesDir,"protonust.owl");
    URL rdfXmlURL = demoFile.toURI().toURL();
    fm.put("rdfXmlURL", rdfXmlURL);
    Ontology ontology = (Ontology)Factory.createResource(
            "gate.creole.ontology.impl.sesame.OWLIMOntology", fm);

    long beginTime;
    long elapsed1;

    beginTime = System.nanoTime();
    Set<OClass> classes = ontology.getOClasses(false);
    elapsed1 = System.nanoTime() - beginTime;
    System.out.println("##### all classes: "+classes.size());
    System.out.println("@@@@@ all classes: "+elapsed1);
    assertEquals(252, classes.size());
    //System.out.println("Classes: "+classes);


    beginTime = System.nanoTime();
    classes = ontology.getOClasses(true);
    elapsed1 = System.nanoTime() - beginTime;
    System.out.println("##### top classes: "+classes.size());
    System.out.println("@@@@@ top classes: "+elapsed1);
    assertEquals(15, classes.size());

    beginTime = System.nanoTime();
    Set<AnnotationProperty> anns = ontology.getAnnotationProperties();
    elapsed1 = System.nanoTime() - beginTime;
    System.out.println("List  ann props: "+anns);
    System.out.println("##### ann props: "+anns.size());
    System.out.println("@@@@@ ann props: "+elapsed1);
    assertEquals(6, anns.size());

    beginTime = System.nanoTime();
    Set<DatatypeProperty> dats = ontology.getDatatypeProperties();
    elapsed1 = System.nanoTime() - beginTime;
    System.out.println("##### dat props: "+dats.size());
    System.out.println("@@@@@ dat props: "+elapsed1);
    assertEquals(11, dats.size());

    beginTime = System.nanoTime();
    Set<ObjectProperty> obs = ontology.getObjectProperties();
    elapsed1 = System.nanoTime() - beginTime;
    System.out.println("##### obj props: "+obs.size());
    System.out.println("@@@@@ obj props: "+elapsed1);
    assertEquals(43, obs.size());

    beginTime = System.nanoTime();
    @SuppressWarnings("deprecation")
    Set<OInstance> insts = ontology.getOInstances();
    elapsed1 = System.nanoTime() - beginTime;
    System.out.println("##### insts: "+insts.size());
    System.out.println("@@@@@ insts: "+elapsed1);
    assertEquals(0, insts.size());


    beginTime = System.nanoTime();
    Set<RDFProperty> props =  ontology.getPropertyDefinitions();
    elapsed1 = System.nanoTime() - beginTime;
    System.out.println("##### propdefs: "+props.size());
    System.out.println("@@@@@ propdefs: "+elapsed1);
    // assertEquals(0, insts.size());


    beginTime = System.nanoTime();
    Set<SymmetricProperty> symprops = ontology.getSymmetricProperties();
    elapsed1 = System.nanoTime() - beginTime;
    System.out.println("##### symprops: "+symprops.size());
    System.out.println("@@@@@ symprops: "+elapsed1);
    // assertEquals(0, insts.size());

    beginTime = System.nanoTime();
    Set<TransitiveProperty> transprops = ontology.getTransitiveProperties();
    elapsed1 = System.nanoTime() - beginTime;
    System.out.println("##### symprops: "+transprops.size());
    System.out.println("@@@@@ symprops: "+elapsed1);
    // assertEquals(0, insts.size());

    OURI cBAURI = ontology.createOURIForName("BusinessAbstraction");
    //System.out.println("OUIR: "+cBAURI+" / "+cBAURI.toTurtle());
    OClass cBA = ontology.getOClass(cBAURI);
    assertNotNull(cBA);

    beginTime = System.nanoTime();
    classes = cBA.getSubClasses(TRANSITIVE_CLOSURE);
    elapsed1 = System.nanoTime() - beginTime;
    System.out.println("##### BA subclasses all: "+classes.size());
    System.out.println("@@@@@ BA subclasses all: "+elapsed1);
    // assertEquals(0, insts.size());

    beginTime = System.nanoTime();
    classes = cBA.getSubClasses(DIRECT_CLOSURE);
    elapsed1 = System.nanoTime() - beginTime;
    System.out.println("##### BA subclasses direct: "+classes.size());
    System.out.println("@@@@@ BA subclasses direct: "+elapsed1);
    // assertEquals(0, insts.size());



    //ontology.getOResourcesWith(null, null);
    //ontology.getOResourcesWith(null, null);
    //ontology.getDistance(null, null);

    // TODO:
    // for a couple of classes, get all subclasses, direct subclasses
    //   get all superclasses ...

    ontology.cleanup();
  }

  public void testWineOntology() throws MalformedURLException,
          ResourceInstantiationException {
    FeatureMap fm = Factory.newFeatureMap();
    // For local wine:
    File demoFile = new File(ontologiesDir,"wine.rdfxml.owl");
    URL rdfXmlURL = demoFile.toURI().toURL();
    fm.put("rdfXmlURL", rdfXmlURL);
    Ontology ontology = (Ontology)Factory.createResource(
           "gate.creole.ontology.impl.sesame.OWLIMOntology", fm);

    // For remote wine:
    //fm.put("repositoryID","owlim-wine");
    //fm.put("repositoryLocation","http://localhost:8080/openrdf-sesame");
    //Ontology ontology = (Ontology)Factory.createResource(
    //        "gate.creole.ontology.impl.sesame.ConnectSesameOntology", fm);

    long beginTime;
    long elapsed1;

    beginTime = System.nanoTime();
    Set<OClass> classes = ontology.getOClasses(false);
    elapsed1 = System.nanoTime() - beginTime;
    System.out.println("##### all WINE classes: "+classes.size());
    System.out.println("@@@@@ all WINE classes: "+elapsed1);

    beginTime = System.nanoTime();
    classes = ontology.getOClasses(true);
    elapsed1 = System.nanoTime() - beginTime;
    System.out.println("##### top WINE classes: "+classes.size());
    System.out.println("@@@@@ top WINE classes: "+elapsed1);
    //System.out.println("Top wine classes: "+classes);

    beginTime = System.nanoTime();
    System.out.println("Starting to expand all classes ... ");
    List<ONodeID> allIDs = new ArrayList<ONodeID>();
    expandAllClasses(classes, allIDs, 0);
    System.out.println("Exapnding classes complete");
    System.out.println("##### expanded WINE classes: "+allIDs.size());
    System.out.println("@@@@@ expanded WINE classes: "+elapsed1);
    //List<ONodeID> uriList = new ArrayList<ONodeID>(allIDs);
    //Collections.sort(uriList);
    //for(ONodeID u : uriList) {
    //  System.out.println("=== "+u);
    //}

    // TODO: try to find the WineColor class ...
    OClass c1 = ontology.getOClass(getURI4Name(ontology,"WineColor"));
    assertNotNull("Find WineColor",c1);

    // TODO: what is the atual correct number of total and top wine classes to
    // expect, what is the number of instances and properties?
    //assertEquals("Total number of wine classes and restrictions",542,allIDs.size());
    ontology.cleanup();
  }

  public void test1Ontology() throws ResourceInstantiationException, MalformedURLException {
    FeatureMap fm = Factory.newFeatureMap();
    File demoFile = new File(ontologiesDir,"test1.rdfxml.owl");
    URL rdfXmlURL = demoFile.toURI().toURL();
    fm.put("rdfXmlURL", rdfXmlURL);
    Ontology ontology = (Ontology)Factory.createResource(
            "gate.creole.ontology.impl.sesame.OWLIMOntology", fm);

    ontology.setDefaultNameSpace("http://dummyurl.com/20090825/test1.rdfxml.owl#");
    long beginTime;
    long elapsed1;

    beginTime = System.nanoTime();
    Set<OClass> classes = ontology.getOClasses(false);
    elapsed1 = System.nanoTime() - beginTime;
    System.out.println("##### t1 all classes: "+classes.size());
    System.out.println("@@@@@ t1 all classes: "+elapsed1);
    System.out.println("T1 All classes: "+classes);
    //assertEquals(252, classes.size());


    beginTime = System.nanoTime();
    classes = ontology.getOClasses(true);
    elapsed1 = System.nanoTime() - beginTime;
    System.out.println("##### t1 top classes: "+classes.size());
    System.out.println("@@@@@ t1 top classes: "+elapsed1);
    System.out.println("T1 Top classes: "+classes);
    //assertEquals(15, classes.size());

    OClass c02a = ontology.getOClass(getURI4Name(ontology,"Class02a"));
    assertNotNull(c02a);

    OInstance i1 = ontology.addOInstance(getURI4Name(ontology,"Ic02a01"), c02a);
    assertNotNull(i1);
    Set<OClass> iclasses = i1.getOClasses(DIRECT_CLOSURE);
    System.out.println("Direct classes for Ic02a01: "+iclasses);
    assertEquals(iclasses.size(), 2);
    iclasses = i1.getOClasses(TRANSITIVE_CLOSURE);
    System.out.println("All classes for Ic02a01: "+iclasses);
    assertEquals(iclasses.size(), 2);

    OClass c02b = ontology.getOClass(getURI4Name(ontology,"Class02b"));
    assertNotNull(c02b);

    OClass newClass = ontology.addOClass(getURI4Name(ontology,"newClass01"));
    i1.addOClass(newClass);
    iclasses = i1.getOClasses(DIRECT_CLOSURE);
    System.out.println("Direct classes for Ic02a01 after addOClass: "+iclasses);
    assertEquals(iclasses.size(), 3);
    iclasses = i1.getOClasses(TRANSITIVE_CLOSURE);
    System.out.println("All classes for Ic02a01 after addOClass: "+iclasses);
    assertEquals(iclasses.size(), 3);


    // Class02b is asserted to be an equivalent class of Class02a so the
    // newly added instance should also be an instance of c02b
    Set<OInstance> is = ontology.getOInstances(c02b,TRANSITIVE_CLOSURE);
    System.out.println("Instances of class02b: "+is);
    System.out.println("Instances of c02b: "+is);
    assertEquals(3, is.size());

    i1 = is.iterator().next();

    is = ontology.getOInstances();
    System.out.println("All instances in ontology: "+is);
    System.out.println("Number of all instances: "+is.size());
    // the 30 from the stored ontology plus the one we created
    assertEquals(31,is.size());

    OInstance i2 = ontology.getOInstance(ontology.createOURIForName("IClass01_10"));
    assertNotNull(i2);
    iclasses = i2.getOClasses(DIRECT_CLOSURE);
    System.out.println("Direct classes for IClass01_10: "+iclasses);
    assertEquals(2,iclasses.size());
    iclasses = i2.getOClasses(TRANSITIVE_CLOSURE);
    System.out.println("Transitive classes for IClass01_10: "+iclasses);

    OInstance i3 = ontology.getOInstance(ontology.createOURIForName("IPerson_01"));
    iclasses = i3.getOClasses(DIRECT_CLOSURE);
    System.out.println("Direct classes for IPerson_01: "+iclasses);
    iclasses = i3.getOClasses(TRANSITIVE_CLOSURE);
    System.out.println("Transitive classes for IPerson_01: "+iclasses);

    //Set<OResource> x = new HashSet<OClass>();

    // test deprecated getting resource by name
    List<OResource> rs = ontology.getOResourcesByName("Animal");
    for (OResource r : rs) {
      System.out.println("Found resource: "+r+"/"+r.getClass().getName());
    }
    rs = ontology.getOResourcesByName("IPerson_02");
    for (OResource r : rs) {
      System.out.println("Found resource: "+r+"/"+r.getClass().getName());
    }
    rs = ontology.getOResourcesByName("iperson_02");
    for (OResource r : rs) {
      System.out.println("Found resource: "+r+"/"+r.getClass().getName());
    }

    String theQuery = "SELECT DISTINCT x,y " + " from {x} rdfs:subClassOf {y}";
    String queryResult = ontology.executeQuery(theQuery);
    //System.out.println("Query Result is: >"+queryResult+"<");
    ontology.cleanup();
  }

   public void testCreateModify() throws MalformedURLException,
          ResourceInstantiationException,
          FileNotFoundException,
          IOException {
    FeatureMap fm = Factory.newFeatureMap();
    Ontology ontology = (Ontology)Factory.createResource(
            "gate.creole.ontology.impl.sesame.OWLIMOntology", fm);

    long beginTime;
    long elapsed1;

    beginTime = System.nanoTime();
    Set<OClass> classes = ontology.getOClasses(false);
    elapsed1 = System.nanoTime() - beginTime;
    System.out.println("##### all classes: "+classes.size());
    System.out.println("@@@@@ all classes: "+elapsed1);
    assertEquals(0, classes.size());

    // set the Base URI
    ontology.setDefaultNameSpace("http://gate.ac.uk/Ontology/testCreateModify/#");
    // create some classes
    ontology.addOClass(getURI4Name(ontology,"Class01"));
    OClass c02 = ontology.addOClass(getURI4Name(ontology,"Class02"));
    assertNotNull(c02);
    OClass c0201 = ontology.addOClass(getURI4Name(ontology,"Class0201"));
    assertNotNull(c0201);
    c02.addSubClass(c0201);
    OClass c020101 = ontology.addOClass(getURI4Name(ontology,"Class020101"));
    assertNotNull(c020101);
    c0201.addSubClass(c020101);


    beginTime = System.nanoTime();
    classes = ontology.getOClasses(false);
    elapsed1 = System.nanoTime() - beginTime;
    System.out.println("##### all classes: "+classes.size());
    System.out.println("@@@@@ all classes: "+elapsed1);
    assertEquals(4, classes.size());

    OClass c02b = ontology.getOClass(getURI4Name(ontology,"Class02"));
    assertNotNull("Find Class02 again",c02b);

    beginTime = System.nanoTime();
    classes = c02b.getSubClasses(DIRECT_CLOSURE);
    elapsed1 = System.nanoTime() - beginTime;
    System.out.println("##### c02 sub classes: "+classes.size());
    System.out.println("@@@@@ c02 sub classes: "+elapsed1);
    assertEquals(1, classes.size());

    beginTime = System.nanoTime();
    classes = c02b.getSubClasses(TRANSITIVE_CLOSURE);
    elapsed1 = System.nanoTime() - beginTime;
    System.out.println("##### c02 all classes: "+classes.size());
    System.out.println("@@@@@ c02 all classes: "+elapsed1);
    assertEquals(2, classes.size());

    // add annotation, datatype, object properties, subproperties,
    // transitive properties
    // instances
    // restrictions

    // delete some of the stuff added and see if it is gone



    // save to the tmp dir
    File savedOntology = new File(tmpDir,"testCreateModify.rdfxml.owl");
    FileOutputStream os = new FileOutputStream(savedOntology);
    ontology.writeOntologyData(os, OConstants.OntologyFormat.RDFXML, false);
    os.close();
    File savedOntologyT = new File(tmpDir,"testCreateModify.turtle.owl");
    os = new FileOutputStream(savedOntologyT);
    ontology.writeOntologyData(os, OConstants.OntologyFormat.TURTLE, false);
    os.close();
    savedOntologyT = new File(tmpDir,"testCreateModifyWithImports.turtle.owl");
    os = new FileOutputStream(savedOntologyT);
    ontology.writeOntologyData(os, OConstants.OntologyFormat.TURTLE, true);
    os.close();

    ontology.cleanup();

    //////////////////////////////////////////////////
    URL savedOntoURL = savedOntology.toURI().toURL();
    fm.put("rdfXmlURL", savedOntoURL);
    ontology = (Ontology)Factory.createResource(
            "gate.creole.ontology.impl.sesame.OWLIMOntology", fm);
    ontology.setDefaultNameSpace("http://gate.ac.uk/Ontology/testCreateModify/#");

    beginTime = System.nanoTime();
    classes = ontology.getOClasses(false);
    elapsed1 = System.nanoTime() - beginTime;
    System.out.println("##### 2 all classes: "+classes.size());
    System.out.println("@@@@@ 2 all classes: "+elapsed1);
    assertEquals(4, classes.size());

    c02b = ontology.getOClass(getURI4Name(ontology,"Class02"));
    assertNotNull("Find Class02 again",c02b);

    beginTime = System.nanoTime();
    classes = c02b.getSubClasses(DIRECT_CLOSURE);
    elapsed1 = System.nanoTime() - beginTime;
    System.out.println("##### c02 sub classes: "+classes.size());
    System.out.println("@@@@@ c02 sub classes: "+elapsed1);
    assertEquals(1, classes.size());

    beginTime = System.nanoTime();
    classes = c02b.getSubClasses(TRANSITIVE_CLOSURE);
    elapsed1 = System.nanoTime() - beginTime;
    System.out.println("##### c02 all classes: "+classes.size());
    System.out.println("@@@@@ c02 all classes: "+elapsed1);
    assertEquals(2, classes.size());


    ontology.cleanup();


   }



  // ***************************************************************
  // ************************** HELPER METHODS 
  // ***************************************************************

  // this is for testing if we can recursively expand all classes  and
  // to see how many distinct IDs we find that way
   // for each of the classes in the parameter, do a depth first exapansion
   // of direct subclasses until we do not get any more.
   // For each path down the subclass hierarchy, remember the IDs and
   // check if we have already seen such an ID
  public void expandAllClasses(Set<OClass> classes, List<ONodeID> allIDs, int depth) {
    depth = depth + 1;
    if(depth > 10) {
      // TODO: print everything about the node that gets us into a loop here!
      assertTrue("Depth must not exceed 10",false);
    }
    if(classes.size() == 0) {
      return;
    }
    //System.out.println("Expanding set: "+classes);
    for (OClass c : classes) {

      //System.out.println("Processing class: "+c);
      ONodeID n = c.getONodeID();
      if(allIDs.contains(n)) {

        System.err.println("Node ID already found: "+n);
        System.err.println("Class is "+c.getClass().getName());
        System.err.println("Equivalent classes: "+c.getEquivalentClasses());
        System.err.println("SubClasses: "+c.getSubClasses(DIRECT_CLOSURE));
        System.err.println("Expanding this set: "+classes);
        System.err.println("Seen Nodes: "+allIDs);

      }
      // get the set of direct subclasses of this class
      // only expand if it is not an anonymous class
      //if(! (c instanceof AnonymousClass)) {
        Set<OClass> subclasses = c.getSubClasses(OConstants.Closure.DIRECT_CLOSURE);
        // check the subclasses ...
        if(subclasses.contains(c)) {
          System.out.println("Subclass of itself: "+c);
          assertTrue(false);
        }
        //System.out.println("Subclasses for "+c+": "+subclasses);
        List<ONodeID> newlist = new ArrayList<ONodeID>(allIDs);
        newlist.add(n);
        expandAllClasses(subclasses,newlist,depth);
      //}
    }
  }
  public void expandAllClassesOLD(Set<OClass> classes, Set<ONodeID> allIDs, int depth) {
    depth = depth + 1;
    if(depth > 10) {
      // TODO: print everything about the node that gets us into a loop here!
      assertTrue("Depth must not exceed 10",false);
    }
    if(classes.size() == 0) {
      return;
    }
    //System.out.println("Expanding set: "+classes);
    for (OClass c : classes) {
      //System.out.println("Processing class: "+c);
      ONodeID n = c.getONodeID();
      if(allIDs.contains(n)) {

        System.err.println("Node ID already found: "+n);
        System.err.println("Class is "+c.getClass().getName());
        System.err.println("Equivalent classes: "+c.getEquivalentClasses());
        System.err.println("SubClasses: "+c.getSubClasses(DIRECT_CLOSURE));
        System.err.println("Expanding this set: "+classes);
        System.err.println("Seen Nodes: "+allIDs);

      }
      allIDs.add(n);
      // get the set of direct subclasses of this class
      // only expand if it is not an anonymous class
      //if(! (c instanceof AnonymousClass)) {
        Set<OClass> subclasses = c.getSubClasses(OConstants.Closure.DIRECT_CLOSURE);
        // check the subclasses ...
        if(subclasses.contains(c)) {
          System.out.println("Subclass of itself: "+c);
          assertTrue(false);
        }
        //System.out.println("Subclasses for "+c+": "+subclasses);
        expandAllClassesOLD(subclasses,allIDs,depth);
      //}
    }
  }

  protected static File getUniqueTmpDir() {
    String tmplocation = System.getProperty("run.java.io.tmpdir");
    if(tmplocation == null) {
      tmplocation = System.getProperty("java.io.tmpdir");
    }
    if(tmplocation == null) {
      tmplocation = "/tmp";
    }
    if(tmplocation == null) {
       System.err.println("Not temp-directory found, cannot continue");
       System.exit(1);
    }
    File tmpdir = new File(tmplocation);
    if(!tmpdir.exists()) {
       System.err.println("Temp dir does not exist: "+tmpdir.getAbsolutePath());
       System.exit(1);
    }
    String tmpString = Long.toString(System.currentTimeMillis(),36);
    File uniqTmpDir = new File(tmpdir,"gate-towlim-"+tmpString);
    uniqTmpDir.mkdir();
    return uniqTmpDir;
  }

  protected OURI getURI(Ontology o, String uri) {
    return o.createOURI(uri);
  }

  protected OURI getURI4Name(Ontology o, String uri) {
    return o.createOURIForName(uri);
  }

  /** Test suite routine for the test runner */
  public static Test suite() {
    System.out.println("Running suite");
    return new TestSuite(Test_OWLIMOntology.class);
  } // suite
}
