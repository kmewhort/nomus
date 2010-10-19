/*
 *  Copyright (c) 1995-2010, The University of Sheffield. See the file
 *  COPYRIGHT.txt in the software or at http://gate.ac.uk/gate/COPYRIGHT.txt
 *
 *  This file is part of GATE (see http://gate.ac.uk/), and is free
 *  software, licenced under the GNU Library General Public License,
 *  Version 2, June 1991 (in the distribution as file licence.html,
 *  and also available at http://gate.ac.uk/gate/licence.html).
 *
 *  Johann Petrak 2009-08-13
 *
 *  $Id: ConnectSesameOntology.java 12602 2010-05-06 14:28:51Z ian_roberts $
 */
package gate.creole.ontology.impl.sesame;

import gate.Gate;
import gate.Resource;
import gate.creole.ResourceInstantiationException;
import gate.creole.metadata.CreoleParameter;
import gate.creole.metadata.CreoleResource;

import gate.creole.ontology.OConstants;
import gate.creole.ontology.OURI;
import java.net.MalformedURLException;

import java.util.Set;
import org.apache.log4j.Logger;


/**
 * An ontology LR that allows to connect to a repository containing an
 * ontology. The repository may be located in a local directory or 
 * on a Sesame server. Note that the ontology API will only work as expected
 * and documented if the repository uses the same configuration as used
 * by the OWLIMOntology language resource (OWLIM with owl-max reasoning and
 * partialRDFS optimizations turned off). Other configurations may
 * produce incomplete or wrong results.
 *
 * @author Johann Petrak
 * 
 */
@CreoleResource(
    name = "ConnectSesameOntology",
    interfaceName = "gate.creole.ontology.Ontology",
    comment = "Connect to a repository containing and ontology",
    icon = "ontology",
    helpURL = "http://gate.ac.uk/userguide/sec:ontologies:ontoplugin:connect")
public class ConnectSesameOntology  extends OntologyLR
{
  private static final long serialVersionUID = 1L;

  /**
   *
   * @param theURL
   */
  @CreoleParameter(
     comment="The location of the repository, either a direcotry "+
       "or a Sesame server URL")
  public void setRepositoryLocation(java.net.URL theURL) {
    repositoryLocation = theURL;
  }
  public java.net.URL getRepositoryLocation() {
    return repositoryLocation;
  }
  protected java.net.URL repositoryLocation;

  /**
   *
   * @param id
   */
  @CreoleParameter(comment="The name of the repository to connect to")
  public void setRepositoryID(String id) {
    repositoryID = id;
  }
  public String getRepositoryID() {
    return repositoryID;
  }
  protected String repositoryID;

  private Logger logger;

  /**
   * Constructor
   */
  public ConnectSesameOntology() {
    super();
    logger = Logger.getLogger(this.getClass().getName());
  }

  /** Initialises this resource, and returns it. */
  @Override
  public Resource init() throws ResourceInstantiationException {
    if(getRepositoryLocation() == null) {
      throw new ResourceInstantiationException("repositoryURL must be set");
    }
    if(getRepositoryID() == null) {
      throw new ResourceInstantiationException("repositoryID must be set");
    }

    load();
    Gate.getCreoleRegister().addCreoleListener(this);
    return this;
  }

  /**
   * Loads this ontology.
   * @throws ResourceInstantiationException
   */
  public void load() throws ResourceInstantiationException {
    logger.debug("Initializing");
    OntologyServiceImplSesame owlimService = new OntologyServiceImplSesame(this);
    owlimService.connectToRepository(repositoryLocation,repositoryID);
    ontologyService = owlimService;
    Set<OURI> ouris = ontologyService.getOntologyURIs();
    if(ouris.size() == 1) {
      String uri = ouris.iterator().next().toString();
      if(!uri.endsWith("/") && !uri.endsWith("#")) {
        uri = uri + "#";
      }
      setDefaultNameSpace(uri);
      System.out.println("Default name space set to "+uri);
    } else if(ouris.size() == 0) {
      setDefaultNameSpace(OConstants.ONTOLOGY_DEFAULT_BASE_URI);
      System.err.println("Warning: no ontology URI found, setting default name space to: "+OConstants.ONTOLOGY_DEFAULT_BASE_URI);
    } else if(ouris.size() == 0) {
      System.err.println("Error: several ontology URIs found, could not set default name space");
    }

    this.setURL(getSourceURL());
    logger.debug("Initializing complete");
  }


  public java.net.URL getSourceURL() {
    try {
      return new java.net.URL(getRepositoryLocation() + "/repositories/" + getRepositoryID());
    } catch (MalformedURLException ex) {
      return null;
    }
  }

}