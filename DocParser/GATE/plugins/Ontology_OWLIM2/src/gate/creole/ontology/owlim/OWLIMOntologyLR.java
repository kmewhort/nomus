/*
 *  OWLIMOntologyLR.java
 *
 *  Niraj Aswani, 09/March/07
 *
 *  $Id: OWLIMOntologyLR.java 12602 2010-05-06 14:28:51Z ian_roberts $
 */
package gate.creole.ontology.owlim;

import gate.Gate;
import gate.Resource;
import gate.creole.ResourceData;
import gate.creole.ResourceInstantiationException;
import gate.creole.ontology.OConstants;
import gate.event.CreoleEvent;
import gate.event.CreoleListener;
import gate.gui.ActionsPublisher;
import gate.gui.MainFrame;
import gate.util.Files;
import gate.util.GateRuntimeException;
import gate.util.Out;

import java.awt.event.ActionEvent;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.OutputStream;
import java.io.Reader;
import java.io.StringReader;
import java.io.Writer;
import java.net.URL;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import javax.swing.AbstractAction;
import javax.swing.JFileChooser;

import org.openrdf.sesame.config.SystemConfig;
import org.openrdf.sesame.config.handlers.SystemConfigFileHandler;
import org.openrdf.sesame.repository.SesameRepository;
import org.openrdf.sesame.repository.local.LocalService;

/**
 * This LR provides an implementation of Ontology interface and uses
 * OWLIM as a SAIL to store and interact with SESAME. All data is stored
 * in Memory, unless asked to persist in a file. If so, it stores
 * everything in ntripples format. This implementation supports four
 * different formats including RDF/XML, NTRIPPLES, N3 and TURTLE.
 * 
 * @author niraj
 * 
 */
public class OWLIMOntologyLR extends AbstractOWLIMOntologyImpl implements
                                                              ActionsPublisher,
                                                              CreoleListener {
  private static final long serialVersionUID = 3761129327521051696L;

  /**
   * If true, the debug information is printed.
   */
  private final static boolean DEBUG = false;

  /**
   * URL for the RDF/XML ontology.
   */
  protected URL rdfXmlURL;

  /**
   * URL for the NTripples ontology.
   */
  protected URL ntriplesURL;

  /**
   * URL for the N3 ontology.
   */
  protected URL n3URL;

  /**
   * URL for the Turtle ontology.
   */
  protected URL turtleURL;

  /**
   * Location where the repository should be persisted
   */
  protected URL persistLocation;

  /**
   * Actions for the remote ontology LR.
   */
  protected List<AbstractAction> actionsList;

  /**
   * Constructor
   */
  public OWLIMOntologyLR() throws ResourceInstantiationException {
    super();
    actionsList = new ArrayList<AbstractAction>();
    actionsList.add(new SaveAsNTRIPLESAction("Save in NTRIPLES format"));
    actionsList.add(new SaveAsRDFXMLAction("Save in RDFXML format"));
    //actionsList.add(new SaveAsN3Action("Save in N3 format"));
    actionsList.add(new SaveAsTURTLEAction("Save in TURTLE format"));
    actionsList.add(new SaveOntologyEventLogAction("Save Ontology Event Log"));
    actionsList.add(new LoadOntologyEventLogAction("Load Ontology Event Log"));
    
  }

  /** Initialises this resource, and returns it. */
  public Resource init() throws ResourceInstantiationException {
    load();
    Gate.getCreoleRegister().addCreoleListener(this);
    // just a quick hack to see if this works at all ...
    return this;
  }

  /**
   * Loads this ontology.
   */
  public void load() throws ResourceInstantiationException {
    try {
      if(defaultNameSpace == null || defaultNameSpace.trim().length() == 0)
        defaultNameSpace = "http://gate.ac.uk/owlim#";
      byte type = OConstants.ONTOLOGY_FORMAT_RDFXML;
      if(rdfXmlURL != null && rdfXmlURL.toString().trim().length() > 0) {
        ontologyURL = rdfXmlURL;
      }
      else if(ntriplesURL != null && ntriplesURL.toString().trim().length() > 0) {
        ontologyURL = ntriplesURL;
        type = OConstants.ONTOLOGY_FORMAT_NTRIPLES;
      }
      else if(n3URL != null && n3URL.toString().trim().length() > 0) {
        ontologyURL = n3URL;
        type = OConstants.ONTOLOGY_FORMAT_N3;
      }
      else if(turtleURL != null && turtleURL.toString().trim().length() > 0) {
        ontologyURL = turtleURL;
        type = OConstants.ONTOLOGY_FORMAT_TURTLE;
      }
      else {
        ontologyURL = null;
        type = OConstants.ONTOLOGY_FORMAT_RDFXML;
      }
      if(DEBUG) System.out.println("Creating repository");
      String ontoURLString = ontologyURL == null ? "" : ontologyURL
              .toExternalForm();

      // determine the URL to the Ontology_Tools plugin directory
      ResourceData myResourceData = (ResourceData)Gate.getCreoleRegister().get(
              this.getClass().getName());
      URL creoleXml = myResourceData.getXmlFileUrl();
      URL gosHomeURL = new URL(creoleXml, ".");

      // create a standalone OWLIMServiceImpl
      OWLIMServiceImpl owlimService = new OWLIMServiceImpl();
      owlimService.setGosHomeURL(gosHomeURL);
      SystemConfig sesameConfig = createStandaloneConfiguration();
      LocalService sesameService = new LocalService(sesameConfig);
      owlimService.setSesameLocalService(sesameService);
      owlimService.setSystemConfLocation(null);
      owlimService.setShutDownOnLogout(true);
      owlimService.init();
      owlimService.login("admin", "admin");
      
      owlim = owlimService;

      String persistLocationPath = null;
      if(persistLocation != null
              && persistLocation.toString().trim().length() != 0) {
        persistLocationPath = Files.fileFromURL(persistLocation).getAbsolutePath();
      }
      else if(persistRepository.booleanValue()) {
        throw new ResourceInstantiationException(
                "As you've set the parameter 'persistRepository' to 'true', you must provide a valid path for where you want to preserve your data!");
      }
      else {
        persistLocationPath = File.createTempFile("abc", "abc").getParentFile()
                .getAbsolutePath();
      }

      if(ontoURLString.length() == 0) {
        sesameRepositoryID = owlim.createRepository("owlim" + this.hashCode(),
                "admin", "admin", "", defaultNameSpace, type,
                persistLocationPath, persistRepository.booleanValue(), false);
      }
      else {
        sesameRepositoryID = owlim.createRepositoryFromUrl("owlim"
                + this.hashCode(), "admin", "admin", ontoURLString,
                defaultNameSpace, type, persistLocationPath, persistRepository
                        .booleanValue(), false);

      }
      if(sesameRepositoryID == null) {
        throw new ResourceInstantiationException(
                "repository cannot be created!");
      }

      // we need to populate all our maps with the resources available
      // in repository
      // the following methods should do so
      getOClasses(false);
      getOInstances();
      getPropertyDefinitions();
    } catch(IOException ioe) {
      throw new ResourceInstantiationException(ioe);
    }
  }

  /**
   * Create a fixed sesame system configuration for the standalone OWLIM service.
   */
  private SystemConfig createStandaloneConfiguration() throws ResourceInstantiationException {
    String s = "<?xml version='1.0'?>"
      + "<system-conf>"
      + "<admin password=''/>"
      + "<log dir='plugins/Ontology_Tools/logs' level='3'/>"
      + "<tmp dir='plugins/Ontology_Tools/tmp'/>"
      + "<rmi-factory enabled='false' class='com.ontotext.util.rmi.CustomRMIFactory' port='1099'/>"
      + "<userlist>"
      + "<user id='1' login='admin'><fullname>Admin</fullname><password>admin</password></user>"
      + "<user id='3' login='guest'><fullname>Guest</fullname><password>guest</password></user>"
      + "</userlist>"
      + "<repositorylist></repositorylist></system-conf>";
    try {
      Reader reader = new StringReader(s);
      SystemConfig config = SystemConfigFileHandler.readConfiguration(reader);
      reader.close();
      return config;
    }
    catch(IOException e) {
      throw new ResourceInstantiationException("Error loading sesame configuration", e);
    }
  }

  /**
   * This method is invoked at the time of resource unloading
   */
  public void cleanup() {
    super.cleanup();
  }

  /*
   * (non-Javadoc)
   * 
   * @see gate.gui.ActionsPublisher#getActions()
   */
  public List getActions() {
    return actionsList;
  }

  // ************ Creole Listener Methods ************
  public void resourceLoaded(CreoleEvent ce) {
    // do nothing
  }

  public void datastoreClosed(CreoleEvent ce) {
  }

  public void resourceRenamed(Resource resource, String oldname, String newname) {
  }

  public void datastoreOpened(CreoleEvent ce) {
  }

  public void datastoreCreated(CreoleEvent ce) {
  }

  /**
   * This method removes the repository, provided that user has decided
   * not to persist this, when this resource is unloaded
   */
  public void resourceUnloaded(CreoleEvent ce) {
    if(ce.getResource() == this) {
      unload();
      Gate.getCreoleRegister().removeCreoleListener(this);
    }
  }

  /**
   * This method deletes the repository from memory and releases all
   * resources occupied by the ontology. Please note that after calling
   * this method, any call to any method of the ontology will throw an
   * exception complaining that the repository does not exist. So this
   * should be the last call to this ontology when you decide to discard
   * this instance of ontology.
   */
  public void unload() {
      if(!getPersistRepository().booleanValue()) {
        owlim.removeRepository(getSesameRepositoryID(), getPersistRepository()
                .booleanValue());
        owlim.logout(sesameRepositoryID);
        owlim = null;
      }
  }
  /**
   * Export Results in NTRIPLE Format
   * 
   * @author niraj
   */
  public class SaveAsNTRIPLESAction extends AbstractAction {
    private static final long serialVersionUID = 3257002159693444913L;

    public SaveAsNTRIPLESAction(String label) {
      super(label);
    }

    public void actionPerformed(ActionEvent ae) {
      Runnable runableAction = new Runnable() {
        public void run() {
          File selectedFile = askForFile();
          if(selectedFile == null) return;
          try {
            MainFrame.lockGUI("Saving...");
            BufferedWriter writer = new BufferedWriter(new FileWriter(
                    selectedFile));
            String output = owlim.getOntologyData(sesameRepositoryID,
                    OConstants.ONTOLOGY_FORMAT_NTRIPLES);
            writer.write(output);
            writer.flush();
            writer.close();
          }
          catch(Exception ex) {
            ex.printStackTrace(Out.getPrintWriter());
          }
          finally {
            MainFrame.unlockGUI();
          }
        }
      };
      Thread thread = new Thread(runableAction, "");
      thread.setPriority(Thread.MIN_PRIORITY);
      thread.start();
    }
  }

  /**
   * Export Results in RDFXML Format
   * 
   * @author niraj
   */
  public class SaveAsRDFXMLAction extends AbstractAction {
    private static final long serialVersionUID = 3834305146426570290L;

    public SaveAsRDFXMLAction(String label) {
      super(label);
    }

    public void actionPerformed(ActionEvent ae) {
      Runnable runableAction = new Runnable() {
        public void run() {
          File selectedFile = askForFile();
          if(selectedFile == null) return;
          try {
            MainFrame.lockGUI("Saving...");
            String output = owlim.getOntologyData(sesameRepositoryID,
                    OConstants.ONTOLOGY_FORMAT_RDFXML);
            BufferedWriter writer = new BufferedWriter(new FileWriter(
                    selectedFile));
            writer.write(output);
            writer.flush();
            writer.close();
          }
          catch(Exception ex) {
            ex.printStackTrace(Out.getPrintWriter());
          }
          finally {
            MainFrame.unlockGUI();
          }
        }
      };
      Thread thread = new Thread(runableAction, "");
      thread.setPriority(Thread.MIN_PRIORITY);
      thread.start();
    }
  }

  /**
   * Export Results in TURTLE Format
   * 
   * @author niraj
   */
  public class SaveAsTURTLEAction extends AbstractAction {
    private static final long serialVersionUID = 3905801980944136247L;

    public SaveAsTURTLEAction(String label) {
      super(label);
    }

    public void actionPerformed(ActionEvent ae) {
      Runnable runableAction = new Runnable() {
        public void run() {
          File selectedFile = askForFile();
          if(selectedFile == null) return;
          try {
            MainFrame.lockGUI("Saving...");
            String output = owlim.getOntologyData(sesameRepositoryID,
                    OConstants.ONTOLOGY_FORMAT_TURTLE);
            BufferedWriter writer = new BufferedWriter(new FileWriter(
                    selectedFile));
            writer.write(output);
            writer.flush();
            writer.close();
          }
          catch(Exception ex) {
            ex.printStackTrace(Out.getPrintWriter());
          }
          finally {
            MainFrame.unlockGUI();
          }
        }
      };
      Thread thread = new Thread(runableAction, "");
      thread.setPriority(Thread.MIN_PRIORITY);
      thread.start();
    }
  }

  /**
   * Export Results in N3 Format
   * 
   * @author niraj
   */
  public class SaveAsN3Action extends AbstractAction {
    private static final long serialVersionUID = 4051328924770382905L;

    public SaveAsN3Action(String label) {
      super(label);
    }

    public void actionPerformed(ActionEvent ae) {
      Runnable runableAction = new Runnable() {
        public void run() {
          File selectedFile = askForFile();
          if(selectedFile == null) return;
          try {
            MainFrame.lockGUI("Saving...");
            String output = owlim.getOntologyData(sesameRepositoryID,
                    OConstants.ONTOLOGY_FORMAT_N3);
            BufferedWriter writer = new BufferedWriter(new FileWriter(
                    selectedFile));
            writer.write(output);
            writer.flush();
            writer.close();
          }
          catch(Exception ex) {
            ex.printStackTrace(Out.getPrintWriter());
          }
          finally {
            MainFrame.unlockGUI();
          }
        }
      };
      Thread thread = new Thread(runableAction, "");
      thread.setPriority(Thread.MIN_PRIORITY);
      thread.start();
    }
  }

  /**
   * Opens a JFileChooser to ask user for an output file
   * 
   * @return
   */
  private File askForFile() {
    JFileChooser fileChooser = MainFrame.getFileChooser();
    fileChooser.setMultiSelectionEnabled(false);
    fileChooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
    fileChooser.setDialogTitle("Select document to save ...");
    fileChooser.setSelectedFiles(null);
    int res = fileChooser.showDialog(null, "Save");
    if(res == JFileChooser.APPROVE_OPTION) {
      return fileChooser.getSelectedFile();
    }
    return null;
  }

  /*
   * (non-Javadoc)
   * 
   * @see gate.creole.ontology.Ontology#startTransaction()
   */
  public void startTransaction() {
    try {
      ((OWLIMServiceImpl)owlim).startTransaction(this.sesameRepositoryID);
    }
    catch(Exception re) {
      throw new GateRuntimeException(re);
    }
  }

  /*
   * (non-Javadoc)
   * 
   * @see gate.creole.ontology.Ontology#commitTransaction()
   */
  public void commitTransaction() {
    try {
      ((OWLIMServiceImpl)owlim).commitTransaction(this.sesameRepositoryID);
    }
    catch(Exception re) {
      throw new GateRuntimeException(re);
    }
  }

  /*
   * (non-Javadoc)
   * 
   * @see gate.creole.ontology.Ontology#transationStarted()
   */
  public boolean transationStarted() {
    try {
      return ((OWLIMServiceImpl)owlim)
              .transactionStarted(this.sesameRepositoryID);
    }
    catch(Exception re) {
      throw new GateRuntimeException(re);
    }
  }

  /*
   * (non-Javadoc)
   * 
   * @see gate.creole.ontology.Ontology#writeOntologyData(java.io.OutputStream,
   *      byte)
   */
  public void writeOntologyData(OutputStream out, byte format) {
    try {
      ((OWLIMServiceImpl)owlim).writeOntologyData(this.sesameRepositoryID, out,
              format);
    }
    catch(Exception re) {
      throw new GateRuntimeException(re);
    }
  }

  /*
   * (non-Javadoc)
   * 
   * @see gate.creole.ontology.Ontology#writeOntologyData(java.io.Writer,
   *      byte)
   */
  public void writeOntologyData(Writer out, byte format) {
    try {
      ((OWLIMServiceImpl)owlim).writeOntologyData(this.sesameRepositoryID, out,
              format);
    }
    catch(Exception re) {
      throw new GateRuntimeException(re);
    }
  }

  /*
   * (non-Javadoc)
   * 
   * @see gate.creole.ontology.Ontology#getSesameRepository()
   */
  public SesameRepository getSesameRepository() {
      return ((OWLIMServiceImpl)owlim)
              .getSesameRepository(this.sesameRepositoryID);
  }

  /**
   * URL for the NTripples File
   * 
   * @return
   */
  public URL getNtriplesURL() {
    return ntriplesURL;
  }

  /**
   * Sets the URL for NTripples File
   * 
   * @return
   */
  public void setNtriplesURL(URL ntriplesURL) {
    this.ntriplesURL = ntriplesURL;
  }

  /**
   * URL for the RDFXML File
   * 
   * @return
   */
  public URL getRdfXmlURL() {
    return rdfXmlURL;
  }

  /**
   * Sets the URL for RDFXML File
   * 
   * @param rdfXmlURL
   */
  public void setRdfXmlURL(URL rdfXmlURL) {
    this.rdfXmlURL = rdfXmlURL;
  }

  /**
   * URL for the N3 File
   * 
   * @return
   */
  public URL getN3URL() {
    return n3URL;
  }

  /**
   * Sets the URL for the N3 File
   * 
   * @param n3url
   */
  public void setN3URL(URL n3url) {
    n3URL = n3url;
  }

  /**
   * URL for the Turtle File
   * 
   * @return
   */
  public URL getTurtleURL() {
    return turtleURL;
  }

  /**
   * Sets the URL for the Turtle file
   * 
   * @param turtleURL
   */
  public void setTurtleURL(URL turtleURL) {
    this.turtleURL = turtleURL;
  }

  /**
   * Gets the location, where user has decided to store the data. If
   * user didn't provide anything, this method returns the location that
   * system has automatically decided to use to store the temporary
   * data.
   * 
   * @return
   */
  public URL getPersistLocation() {
    return persistLocation;
  }

  /**
   * Allows users to specify where to store the repository data.
   * 
   * @param persistLocation
   */
  public void setPersistLocation(URL persistLocation) {
    this.persistLocation = persistLocation;
  }

  /**
   * This method returns the ontology output in ntripples format.
   */
  public String toString() {
      String output = owlim.getOntologyData(sesameRepositoryID,
              OConstants.ONTOLOGY_FORMAT_NTRIPLES);
      List<String> outputList = Arrays.asList(output.split(System
              .getProperty("line.separator")));
      Collections.sort(outputList);
      output = null;
      StringBuffer toReturn = new StringBuffer();
      for(String line : outputList) {
        toReturn = toReturn.append(line).append(
                System.getProperty("line.separator"));
      }
      return toReturn.toString();
  }
  
  /**
   * Export Results in N3 Format
   * 
   * @author niraj
   */
  public class SaveOntologyEventLogAction extends AbstractAction {
    private static final long serialVersionUID = 4051328924770382905L;

    public SaveOntologyEventLogAction(String label) {
      super(label);
    }

    public void actionPerformed(ActionEvent ae) {
      Runnable runableAction = new Runnable() {
        public void run() {
          File selectedFile = askForFile();
          if (selectedFile == null)
            return;
          try {
            MainFrame.lockGUI("Saving...");
            OntologyEventsLog.exportLog(owlim.getEventsLog(getSesameRepositoryID()),
                selectedFile);
          } catch (Exception ex) {
            ex.printStackTrace(Out.getPrintWriter());
          } finally {
            MainFrame.unlockGUI();
          }
        }
      };
      Thread thread = new Thread(runableAction, "");
      thread.setPriority(Thread.MIN_PRIORITY);
      thread.start();
    }
  }

  /**
   * Export Results in N3 Format
   * 
   * @author niraj
   */
  public class LoadOntologyEventLogAction extends AbstractAction {
    private static final long serialVersionUID = 4051328924770382905L;

    public LoadOntologyEventLogAction(String label) {
      super(label);
    }

    public void actionPerformed(ActionEvent ae) {
      Runnable runableAction = new Runnable() {
        public void run() {
          JFileChooser fileChooser = MainFrame.getFileChooser();
          fileChooser.setMultiSelectionEnabled(false);
          fileChooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
          fileChooser.setDialogTitle("Select event log to load...");
          fileChooser.setSelectedFiles(null);
          int res = fileChooser.showDialog(null, "Load");
          File selectedFile = null;
          if (res == JFileChooser.APPROVE_OPTION) {
            selectedFile = fileChooser.getSelectedFile();
          }
          if (selectedFile == null)
            return;
          try {
            MainFrame.lockGUI("Loading...");
            List<OEvent> events = OntologyEventsLog
                .importLog(selectedFile);
            MainFrame.lockGUI("Applying changes...");
            for (OEvent e : events) {
              if (e.getToAdd()) {
                if (e.getDatatype() == null) {
                  // this is the UUU statement
                  owlim.addStatement(getSesameRepositoryID(),
                      e.getSubject(), e.getPredicate(), e
                          .getObject());
                } else {
                  owlim.addStatement(getSesameRepositoryID(),
                      e.getSubject(), e.getPredicate(), e
                          .getObject(), e
                          .getDatatype());
                }
              } else {
                String subject = e.getSubject();
                String predicate = e.getPredicate();
                String object = e.getObject();
                subject = subject.equals("*") ? null : subject;
                object = object.equals("*") ? null : object;
                predicate = predicate.equals("*") ? null
                    : predicate;
                if (e.getDatatype() == null) {
                  owlim.removeStatement(
                      getSesameRepositoryID(), subject,
                      predicate, object);
                } else {
                  owlim.removeStatement(
                      getSesameRepositoryID(), subject,
                      predicate, object, e.getDatatype());
                }
              }
            }
          } catch (Exception ex) {
            ex.printStackTrace();
          } finally {
            MainFrame.unlockGUI();
            fireOntologyReset();
          }
        }
      };
      Thread thread = new Thread(runableAction, "");
      thread.setPriority(Thread.MIN_PRIORITY);
      thread.start();
    }
  }


}