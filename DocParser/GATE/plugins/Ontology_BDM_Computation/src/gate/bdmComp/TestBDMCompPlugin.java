package gate.bdmComp;

import gate.Corpus;
import gate.Document;
import gate.Factory;
import gate.FeatureMap;
import gate.Gate;
import gate.GateConstants;
import gate.util.ExtensionFileFilter;
import gate.util.GateException;

import java.io.File;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.Arrays;
import java.util.Comparator;

import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;

public class TestBDMCompPlugin extends TestCase {
  /** Use it to do initialisation only once. */
  private static boolean initialized = false;
  /** Learning home for reading the data and configuration file. */
  private static File bdmPluginHome;
  
  /** Constructor, setting the home directory. */
  public TestBDMCompPlugin(String arg0) throws GateException,
    MalformedURLException {
    super(arg0);
    if(!initialized) {
      Gate.init();
      File owlimPluginHome = new File(new File(Gate.getGateHome(), "plugins"),
        "Ontology_OWLIM2");
      Gate.getCreoleRegister().addDirectory(owlimPluginHome.toURI().toURL());
      bdmPluginHome = new File(new File(Gate.getGateHome(), "plugins"),
        "Ontology_BDM_Computation");
      Gate.getCreoleRegister().addDirectory(bdmPluginHome.toURI().toURL());
      initialized = true;
    }
  }
  
  /** Fixture set up */
  public void setUp() {
  } // setUp

  /**
   * Put things back as they should be after running tests.
   */
  public void tearDown() throws Exception {
  } // tearDown

  /** Test suite routine for the test runner */
  public static Test suite() {
    return new TestSuite(TestBDMCompPlugin.class);
  } // suite
  
  /** The test the IAA. */
  public void testBDMCompPlugin() throws Exception {

    Boolean savedSpaceSetting = Gate.getUserConfig().getBoolean(
            GateConstants.DOCUMENT_ADD_SPACE_ON_UNPACK_FEATURE_NAME);
    Gate.getUserConfig().put(
            GateConstants.DOCUMENT_ADD_SPACE_ON_UNPACK_FEATURE_NAME,
            Boolean.FALSE);
    try {
      
      //Gate.setGateHome(new File("C:\\svn\\gate"));
      //Gate.setUserConfigFile(new File("C:\\svn\\gate.xml"));
      //Gate.init();
      //ExtensionFileFilter fileFilter = new ExtensionFileFilter();
      //fileFilter.addExtension("xml");
      //iaaPluginHome = new File(new File(Gate.getGateHome(), "plugins"),
      //"iaaPlugin");
      //Gate.getCreoleRegister().addDirectory(iaaPluginHome.toURL());

      // Load the documents into a corpus
      Corpus data = Factory.newCorpus("data");
      
      String corpusDirName; 
      corpusDirName = new File(bdmPluginHome, "test/ontology").getAbsolutePath();
      
      //corpusDirName = "C:\\svn\\gate\\plugins\\bdmComputation\\test\\ontology";
      
      
      //String testDir = "plugins/iaaPlugin/test/";
      
      //System.out.println("testDir00=*"+(new File(testDir,"beijing-opera.xml")).getAbsolutePath().toString()+"*");
      /*Document doc = Factory.newDocument(new File("C:\\svn\\gate\\plugins\\iaaPlugin\\test\\beijing-opera.xml").toURL(), "UTF-8");
      Document doc1 = Factory.newDocument(new File("C:\\svn\\gate\\plugins\\iaaPlugin\\test\\beijing-opera-2.xml").toURL(), "UTF-8");
      data.add(doc);
      data.add(doc1);*/
      
      BDMCompMain bdmM;

      FeatureMap parameters = Factory.newFeatureMap();
      
      bdmM = (BDMCompMain)Factory.createResource(
        "gate.bdmComp.BDMCompMain", parameters);
      
      File testOnto = new File(corpusDirName, "protont.owl");
      File bdmFile = new File(corpusDirName, "protont-bdm.txt");
     
      bdmM.setOntologyURL(testOnto.toURI().toURL()); //("ann1;ann2;ann3");
      bdmM.setOutputBDMFile(bdmFile.toURI().toURL());
      //bdmM.setAnnTypesAndFeats("Os;sent->Op");
      //bdmM.setVerbosity("0");
      /** The controller include the ML Api as one PR. */
      gate.creole.SerialController
      controller = (gate.creole.SerialController)Factory
      .createResource("gate.creole.SerialController");
      //controller.setCorpus(data);
      controller.add(bdmM);
      
      System.out.println("starting executing...");
      
      controller.execute();
      
      /** four BDM scores for testing purpose. */
      float [] bdmTS = new float[4];
      
      for(BDMOne oneb: bdmM.bdmScores) {
        String key = oneb.con11.getName();
        String res = oneb.con22.getName();
        if((key.equals("ContactInformation") && res.equals("Topic")) ||
          (res.equals("ContactInformation") && key.equals("Topic")))
          bdmTS[0] = oneb.bdmScore;
        if((key.equals("InformationResource") && res.equals("GeneralTerm")) ||
          (res.equals("InformationResource") && key.equals("GeneralTerm")))
          bdmTS[1] = oneb.bdmScore;
        if((key.equals("Recognized") && res.equals("Entity")) ||
          (res.equals("Recognized") && key.equals("Entity")))
          bdmTS[2] = oneb.bdmScore;
        if((key.equals("Role") && res.equals("Event")) ||
          (res.equals("Role") && key.equals("Event")))
          bdmTS[3] = oneb.bdmScore;
      }
      
      int[] nPwF = new int[4];
      nPwF[0] = (int)Math.ceil((double)bdmTS[0]*1000);
      nPwF[1] = (int)Math.ceil((double)bdmTS[1]*1000);
      nPwF[2] = (int)Math.ceil((double)bdmTS[2]*1000);
      nPwF[3] = (int)Math.ceil((double)bdmTS[3]*1000);
      //System.out.println("1="+nPwF[0]+", 2="+nPwF[1]+", 3="+nPwF[2]+", 4="+nPwF[3]+".");
      assertEquals("Wrong value for correct: ", 495, nPwF[0]);
      assertEquals("Wrong value for correct: ", 0, nPwF[1]);
      assertEquals("Wrong value for correct: ", 0, nPwF[2]);
      assertEquals("Wrong value for correct: ", 296, nPwF[3]);
      
      
      
    }
    finally {
      Gate.getUserConfig().put(
              GateConstants.DOCUMENT_ADD_SPACE_ON_UNPACK_FEATURE_NAME,
              savedSpaceSetting);
    }

  }

}
