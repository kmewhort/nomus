/*
 *  TestOntologyAPI.java
 *
 *  Niraj Aswani, 09/March/07
 *
 *  $Id: TestOntologyAPI.java 12602 2010-05-06 14:28:51Z ian_roberts $
 */
package gate.creole.ontology.owlim.test;

import gate.Executable;
import gate.creole.ExecutionException;
import java.io.File;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.Set;

import gate.Factory;
import gate.FeatureMap;
import gate.Gate;
import gate.creole.ResourceInstantiationException;
import gate.creole.ontology.*;
import gate.util.Benchmark;
import java.util.HashSet;
import java.util.List;
import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;
import org.apache.commons.io.FileUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;
import org.apache.log4j.BasicConfigurator;


/**
 * Simple test class that load an ontology available online and accesses
 * its content via the ontology API
 */
public class TestOntologyAPI extends TestCase {
  public static void main(String[] args) {
    junit.textui.TestRunner.run(TestOntologyAPI.class);
  }

  File ontologiesDir = null;
  File tmpDir = null;
  Logger log =  Logger.getLogger(this.getClass());

  byte DIRECT_CLOSURE = OConstants.DIRECT_CLOSURE;
  byte TRANSITIVE_CLOSURE = OConstants.TRANSITIVE_CLOSURE;

  public TestOntologyAPI(String arg0) {
    super(arg0);
  }

  protected void setUp() throws Exception {
    super.setUp();
    System.out.println("Starting up");
    Gate.init();
    File pluginHome =
        new File(new File(Gate.getGateHome(), "plugins"),
                 "Ontology_OWLIM2");
    System.out.println("Plugins Home is "+pluginHome.getAbsolutePath());
    Gate.getCreoleRegister().registerDirectories(
            pluginHome.toURI().toURL());
    File testingDir = new File(pluginHome,"test");
    assertTrue(testingDir.exists());
    ontologiesDir = new File(testingDir, "ontologies");
    tmpDir = getUniqueTmpDir();
    assertTrue(tmpDir.canWrite());
    System.out.println("Start up complete");
    log.setLevel(Level.ALL);
    BasicConfigurator.configure();
    //log.info("This is for the logger!");
  }

  protected void tearDown() throws Exception {
    super.tearDown();
    // tmpDir.deleteOnExit();
    FileUtils.deleteDirectory(tmpDir);
  }

  public void testAccessOldNew() throws MalformedURLException,
          ResourceInstantiationException {
    FeatureMap fm = Factory.newFeatureMap();
    File demoFile = new File(ontologiesDir,"protonust.owl");
    URL rdfXmlURL = demoFile.toURI().toURL();
    fm.put("rdfXmlURL", rdfXmlURL);
    Ontology ontology = (Ontology)Factory.createResource(
            "gate.creole.ontology.owlim.OWLIMOntologyLR", fm);

    long beginTime;
    long elapsed1, elapsed2, elapsed3;

    beginTime = System.nanoTime();
    Set<OClass> classes = ontology.getOClasses(false);
    elapsed1 = System.nanoTime() - beginTime;
    System.out.println("##### all classes: "+classes.size());
    System.out.println("@@@@@ all classes: "+elapsed1);
    assertEquals(252, classes.size());
    
    beginTime = System.nanoTime();
    classes = ontology.getOClasses(true);
    elapsed1 = System.nanoTime() - beginTime;
    System.out.println("##### top classes: "+classes.size());
    System.out.println("@@@@@ top classes: "+elapsed1);
    assertEquals(15, classes.size());
    
    beginTime = System.nanoTime();
    Set<AnnotationProperty> anns = ontology.getAnnotationProperties();
    elapsed1 = System.nanoTime() - beginTime;
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

    /* this fails because of:
     * gate.creole.ontology.owlim.ObjectPropertyImpl cannot be cast to gate.creole.ontology.TransitiveProperty
java.lang.ClassCastException: gate.creole.ontology.owlim.ObjectPropertyImpl cannot be cast to gate.creole.ontology.TransitiveProperty
        at gate.creole.ontology.owlim.AbstractOWLIMOntologyImpl.getTransitiveProperties(AbstractOWLIMOntologyImpl.java:958)
        at gate.creole.ontology.owlim.test.TestOntologyAPI.run_tests1(TestOntologyAPI.java:225)
        at gate.creole.ontology.owlim.test.TestOntologyAPI.testWineOntology(TestOntologyAPI.java:164)

    beginTime = System.nanoTime();
    Set<TransitiveProperty> transprops = ontology.getTransitiveProperties();
    elapsed1 = System.nanoTime() - beginTime;
    System.out.println("##### symprops: "+transprops.size());
    System.out.println("@@@@@ symprops: "+elapsed1);
    // assertEquals(0, insts.size());
     * */

    OClass cBA = ontology.getOClass(
        new URI("http://proton.semanticweb.org/2005/04/protonu#BusinessAbstraction",false));
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
    ontology.cleanup();
  }

   



  // testWineOntology: test ontology methods that were already present in the old
  // implementation - output measurements and do some benchmarking so
  // we can compare.
  public void testWineOntology() throws MalformedURLException,
          ResourceInstantiationException {
    FeatureMap fm = Factory.newFeatureMap();
    File demoFile = new File(ontologiesDir,"wine.rdfxml.owl");
    URL rdfXmlURL = demoFile.toURI().toURL();
    fm.put("rdfXmlURL", rdfXmlURL);
    Ontology ontology = (Ontology)Factory.createResource(
            "gate.creole.ontology.owlim.OWLIMOntologyLR", fm);

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
    Set<URI> allIDs = new HashSet<URI>();
    expandAllClasses(classes, allIDs, 0);
    System.out.println("Exapnding classes complete");
    System.out.println("##### expanded WINE classes: "+allIDs.size());
    System.out.println("@@@@@ expanded WINE classes: "+elapsed1);

    //List<URI> uriList = new ArrayList<URI>(allIDs);
    //Collections.sort(uriList);
    //for(URI u : uriList) {
    //  System.out.println("=== "+u);
    //}
    //assertEquals("Total number of wine classes and restrictions",542,allIDs.size());
    ontology.cleanup();
  }

  public void test1Ontology() throws ResourceInstantiationException, MalformedURLException {
    FeatureMap fm = Factory.newFeatureMap();
    File demoFile = new File(ontologiesDir,"test1.rdfxml.owl");
    URL rdfXmlURL = demoFile.toURI().toURL();
    fm.put("rdfXmlURL", rdfXmlURL);
    Ontology ontology = (Ontology)Factory.createResource(
            "gate.creole.ontology.owlim.OWLIMOntologyLR", fm);

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

    OClass c02b = ontology.getOClass(getURI4Name(ontology,"Class02b"));
    assertNotNull(c02b);

    // Class02b is asserted to be an equivalent class of Class02a so the
    // newly added instance should also be an instance of c02b
    Set<OInstance> is = ontology.getOInstances(c02b,TRANSITIVE_CLOSURE);
    System.out.println("Instances of class02b: "+is);
    System.out.println("Instances of c02b: "+is);
    assertEquals(2, is.size());

    i1 = is.iterator().next();
    //String name = i1.getOU

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
    if(rs != null) {
    for (OResource r : rs) {
      System.out.println("Found resource: "+r+"/"+r.getClass().getName());
    }
    }


    ontology.cleanup();
  }


  public void testMisc() throws ResourceInstantiationException, MalformedURLException {
    FeatureMap fm = Factory.newFeatureMap();
    //File demoFile = new File(ontologiesDir,"test1.rdfxml.owl");
    //URL rdfXmlURL = demoFile.toURI().toURL();
    //fm.put("rdfXmlURL", rdfXmlURL);
    Ontology ontology = (Ontology)Factory.createResource(
            "gate.creole.ontology.owlim.OWLIMOntologyLR", fm);

    ontology.setDefaultNameSpace("http://dummyurl.com/20090825/testmisc.owl#");
    AnnotationProperty anp1 = (AnnotationProperty)
        ontology.getProperty(new URI("http://some.uri.com/yes#doesntexist", false));
    System.out.println("Got the nonexistent property: "+anp1);

    ontology.cleanup();
  }

  public void expandAllClasses(Set<OClass> classes, Set<URI> allIDs, int depth) {
    depth = depth + 1;
    if(depth > 9) {
      // TODO: print everything about the node that gets us into a loop here!
      assertTrue("Depth must not exceed 9",false);
    }
    if(classes.size() == 0) {
      return;
    }
    //System.out.println("Expanding set: "+classes);
    for (OClass c : classes) {
      //System.out.println("Processing class: "+c);
      URI n = c.getURI();
      if(allIDs.contains(n)) {

        //System.err.println("Node ID already found: "+n.toTurtle());
      }
      allIDs.add(n);
      // get the set of direct subclasses of this class
      Set<OClass> subclasses = c.getSubClasses(OConstants.DIRECT_CLOSURE);
      //System.out.println("Subclasses for "+c+": "+subclasses);
      expandAllClasses(subclasses,allIDs,depth);
    }
  }

  protected File getUniqueTmpDir() {
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
    return new URI(uri, false);
  }

  protected OURI getURI4Name(Ontology o, String name) {
    return new URI(o.getDefaultNameSpace()+name,false);
  }


  /** Test suite routine for the test runner */
  public static Test suite() {
    return new TestSuite(TestOntologyAPI.class);
  } // suite
}
