/*
 *  Copyright (c) 1995-2010, The University of Sheffield. See the file
 *  COPYRIGHT.txt in the software or at http://gate.ac.uk/gate/COPYRIGHT.txt
 *
 *  This file is part of GATE (see http://gate.ac.uk/), and is free
 *  software, licenced under the GNU Library General Public License,
 *  Version 2, June 1991 (in the distribution as file licence.html,
 *  and also available at http://gate.ac.uk/gate/licence.html).
 *
 *  $Id: OntologyServiceImplSesame.java 12602 2010-05-06 14:28:51Z ian_roberts $
 */
package gate.creole.ontology.impl.sesame;

// TODO: we still get two different kinds of bnodeids: the old one without
//  _: and the new one with _:
// Figure out where the version without the prefix comes from and make
// all methods use the same convention!
// TODO: !!!!
// - replace generic Exception with something better
// - make replacement for repositoryConnection.isClass and repositoryConnection.isProperty
// - handle "system namespaces/URIs" etc better: have one array of these
// URIS declared somewhere and derive all the constants and tests from there.
// Have that array defined in the ontology namespace!
//
// !!!! Change all the return types used by GOS to either OResource objects
// or ONodeID or NodeIDorLiteral
// Create a class NodeIDorLiteral that *contains* either a NodeID or a
// Literal value. Use that class to pass back search results. Since this
// could be useful in the API, define that interface in the API!
// (For now and during testing, define in package impl)
//
// oneOf restrictions are simply returned as some anonymous class for now!
// check if it makes sense to actually use transactions somewhere? at the
// moment, by default the repositoryconnection is in autocommit mode and
// each modification is automatically commited.


import gate.creole.ontology.GateOntologyException;
import gate.creole.ontology.LiteralOrONodeID;
import gate.creole.ontology.OClass;
import gate.creole.ontology.OConstants;
import gate.creole.ontology.OConstants.Closure;
import gate.creole.ontology.OConstants.OntologyFormat;
import gate.creole.ontology.OURI;
import gate.creole.ontology.OBNodeID;
import gate.creole.ontology.OConstants.QueryLanguage;
import gate.creole.ontology.OInstance;
import gate.creole.ontology.ONodeID;
import gate.creole.ontology.RDFProperty;
import gate.creole.ontology.impl.*;
import gate.util.ClosableIterator;

import java.io.File;
import java.util.Vector;
import java.util.Collection;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.Reader;
import java.io.Writer;
import java.net.URL;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Set;


import org.apache.commons.io.FileUtils;
import org.apache.commons.io.IOUtils;
import org.apache.log4j.Logger;
import org.openrdf.model.BNode;
import org.openrdf.model.Literal;
import org.openrdf.model.Statement;
import org.openrdf.model.URI;
import org.openrdf.query.MalformedQueryException;
import org.openrdf.query.QueryEvaluationException;
import org.openrdf.rio.RDFWriter;
import org.openrdf.rio.n3.N3Writer;
import org.openrdf.rio.ntriples.NTriplesWriter;
import org.openrdf.rio.rdfxml.RDFXMLWriter;
import org.openrdf.rio.turtle.TurtleWriter;
import org.openrdf.model.vocabulary.RDFS;
import org.openrdf.model.vocabulary.OWL;
import org.openrdf.rio.RDFFormat;
import org.openrdf.model.Resource;
import org.openrdf.model.vocabulary.RDF;
import org.openrdf.repository.RepositoryConnection;
import org.openrdf.repository.RepositoryException;
import org.openrdf.repository.RepositoryResult;
import org.openrdf.model.Value;
import org.openrdf.model.impl.BNodeImpl;
import org.openrdf.model.impl.URIImpl;
import org.openrdf.query.BindingSet;
import org.openrdf.query.TupleQueryResult;

/**
 * Implementation of the GATE Ontology Services. This class provides an
 * implementation of each and every service defined under the OntologyService interface.
 * 
 * @author Niraj Aswani
 * @author Johann Petrak
 */
public class OntologyServiceImplSesame implements OntologyService {

  // ***************************************************************************
  // **** CONSTANTS ************************************************************
  // ***************************************************************************

  private static final org.openrdf.model.URI IMPORT_CONTEXT_URI =
      new org.openrdf.model.impl.URIImpl(
      "http://gate.ac.uk/dummyuri/OWLIM3OntologyPlugin/#ImportContext");
  private static final org.openrdf.model.URI DATA_CONTEXT_URI =
      new org.openrdf.model.impl.URIImpl(
      "http://gate.ac.uk/dummyuri/OWLIM3OntologyPlugin/#DataContext");
  private static final org.openrdf.model.URI SYSTEM_IMPORT_CONTEXT_URI =
      new org.openrdf.model.impl.URIImpl(
      "http://gate.ac.uk/dummyuri/OWLIM3OntologyPlugin/#SystemImportContext");


  // ***************************************************************************
  // **** CONSTANTS for prepared queries and the assiciated query variables
  // ***************************************************************************


  private Logger logger;

  protected SesameManager sesameManager;

  private RepositoryConnection repositoryConnection;

  public final AbstractOntologyImplSesame ontology;
  private String ontologyUrl;



  /**
   * Constructor
   */
  public OntologyServiceImplSesame(AbstractOntologyImplSesame o) {
    super();
    ontology = o;
    logger = Logger.getLogger(this.getClass().getName());
  }

  // ***************************************************************************
  // *** METHODS RELATED TO THE ONTOLOGY AS A WHOLE
  // ***************************************************************************

  // ********** CREATION, INITIALIZATION, SHUTDOWN

  /**
   * Create an unmanaged repository in the given data directory from the
   * given configuration data string.
   * 
   * @param dataDir
   * @param configData
   */
  public void createRepository(File dataDir, String configData) {
    sesameManager = new SesameManager();
    sesameManager.createUnmanagedRepository(dataDir, configData);
    repositoryConnection = sesameManager.getRepositoryConnection();
    try {
      repositoryConnection.setAutoCommit(true);
    } catch (RepositoryException ex) {
      sesameManager.disconnect();
      throw new GateOntologyException("Could not set autocommit");
    }
    init();
  }

  /**
   * Create a managed repository at the given repository location, which could
   * either be a directory or a sesame server, with the given repository ID
   * from theconfiguration file at the given URL. The configuration fiel should
   * be a configuration file template with the template variable "id" which
   * will be replaced with the repository ID.
   * 
   * @param repoLoc
   * @param repositoryID
   * @param configFileURL
   */
  void createManagedRepository(URL repoLoc, String repositoryID, URL configFileURL) {
    HashMap<String, String> map = new HashMap<String, String>();
    map.put("id", repositoryID);
    sesameManager = new SesameManager();
    String configData;
    try {
      InputStream is = configFileURL.openStream();
      configData = IOUtils.toString(is);
      IOUtils.closeQuietly(is);
    } catch (IOException ex) {
      throw new GateOntologyException("Cannot read config file " + configFileURL, ex);
    }
    sesameManager.connectToLocation(repoLoc);
    Set<String> repositories = sesameManager.getRepositories();
    if (repositories.contains(repositoryID)) {
      sesameManager.disconnect();
      throw new GateOntologyException("Repository with this ID already exists: " + repositoryID);
    }
    configData = SesameManager.substituteConfigTemplate(configData, map);
    //logger.debug("Config file is: \n" + configData);
    sesameManager.createRepository(configData);
    repositoryConnection = sesameManager.getRepositoryConnection();
    try {
      repositoryConnection.setAutoCommit(true);
    } catch (RepositoryException ex) {
      sesameManager.disconnect();
      throw new GateOntologyException("Could not set autocommit");
    }
    init();
  }

  /**
   * Conect to the repository with the given repository ID
   * at the given repository URL location, which could
   * be either a directory or a sesame server.
   *
   * @param repositoryURL
   * @param repositoryID
   */
  void connectToRepository(URL repositoryURL, String repositoryID) {
    sesameManager = new SesameManager();
    logger.debug("Service: calling connectToRepository for id " + repositoryID);
    sesameManager.connectToRepository(repositoryURL, repositoryID);
    repositoryConnection = sesameManager.getRepositoryConnection();
    try {
      repositoryConnection.setAutoCommit(true);
    } catch (RepositoryException ex) {
      sesameManager.disconnect();
      throw new GateOntologyException("Could not set autocommit");
    }
    init();
  }

  /**
   * Initialize the ontology service. This methods prepares the ontology service
   * for use. It must be called right after the repository has been created
   * or has been connected to.
   */
  private void init() {
    // once we have created a repository or connected to a repository,
    // do all the init stuff for which we need the repository
    // connection: mainly prepare queries.
    initQueries("default");
  }

  /**
   * Shutdown the repository. This must be done before the the object of this
   * class is destroyed!
   */
  public void shutdown() {
    sesameManager.disconnect();
  }

  // *************** IMPORT / EXPORT *******************************************

  public void readOntologyData(File selectedFile, String baseURI,
      OntologyFormat ontologyFormat, boolean asImport) {
    org.openrdf.model.URI contextURI;
    if (!asImport) {
      contextURI = DATA_CONTEXT_URI;
    } else {
      contextURI = IMPORT_CONTEXT_URI;
    }
    try {
      repositoryConnection.add(selectedFile, baseURI,
          ontologyFormat2RDFFormat(ontologyFormat), contextURI);
    } catch (Exception ex) {
      throw new GateOntologyException(
          "Could not load/import ontology data from file " +
          selectedFile.getAbsolutePath(), ex);
    }
  }
  public void readOntologyData(InputStream is, String baseURI,
      OntologyFormat ontologyFormat, boolean asImport) {
    org.openrdf.model.URI contextURI;
    if (!asImport) {
      contextURI = DATA_CONTEXT_URI;
    } else {
      contextURI = IMPORT_CONTEXT_URI;
    }
    try {
      repositoryConnection.add(is, baseURI,
          ontologyFormat2RDFFormat(ontologyFormat), contextURI);
    } catch (Exception ex) {
      throw new GateOntologyException(
          "Could not load/import ontology data from input stream ", ex);
    }
  }
  public void readOntologyData(Reader ir, String baseURI,
      OntologyFormat ontologyFormat, boolean asImport) {
    org.openrdf.model.URI contextURI;
    if (!asImport) {
      contextURI = DATA_CONTEXT_URI;
    } else {
      contextURI = IMPORT_CONTEXT_URI;
    }
    try {
      repositoryConnection.add(ir, baseURI,
          ontologyFormat2RDFFormat(ontologyFormat), contextURI);
    } catch (Exception ex) {
      throw new GateOntologyException(
          "Could not load/import ontology data from reader ", ex);
    }
  }

  public void writeOntologyData(Writer out, OntologyFormat ontologyFormat,
      boolean includeImports) {
    RDFWriter writer = getRDFWriter4Format(out, ontologyFormat);
    try {
      if (includeImports) {
        repositoryConnection.export(writer, DATA_CONTEXT_URI, IMPORT_CONTEXT_URI);
      } else {
        repositoryConnection.export(writer, DATA_CONTEXT_URI);
      }
    } catch (Exception ex) {
      throw new GateOntologyException("Could not write ontology data",ex);
    }
  }
  public void writeOntologyData(OutputStream out, OntologyFormat ontologyFormat,
      boolean includeImports) {
    RDFWriter writer = getRDFWriter4Format(out, ontologyFormat);
    try {
      if (includeImports) {
        repositoryConnection.export(writer, DATA_CONTEXT_URI, IMPORT_CONTEXT_URI);
      } else {
        repositoryConnection.export(writer, DATA_CONTEXT_URI);
      }
    } catch (Exception ex) {
      throw new GateOntologyException("Could not write ontology data",ex);
    }
  }

  public void loadSystemImport(File selectedFile,
      String baseURI, OntologyFormat ontologyFormat) {
    org.openrdf.model.URI contextURI = SYSTEM_IMPORT_CONTEXT_URI;
    try {
      repositoryConnection.add(selectedFile,
          baseURI, ontologyFormat2RDFFormat(ontologyFormat),
          contextURI);
    } catch (Exception ex) {
      throw new GateOntologyException("Could not import system file " +
          selectedFile.getAbsolutePath(), ex);
    }
  }


  // *************** OTHER METHODS RELATED TO THE ONTOLOGY AS A WHOLE **********



  /**
   * The method removes all data from the ontology, including imports and
   * the system imports.
   */
  public void cleanOntology() throws GateOntologyException {
    try {
      repositoryConnection.clear();
    } catch (Exception sue) {
      throw new GateOntologyException("error while cleaning repository:",
          sue);
    }
  }

  /**
   * From all the data and imports so far loaded, gather the set of
   * all ontology import URIs.
   *
   * @return
   */
  public Set<String> getImportURIStrings() {
    Set<String> uris = new HashSet<String>();
    RepositoryResult<Statement> result;
    try {
      // TODO: should we remove "system" import URIs here?
      // I tend to not do this here, instead ignore them in the resolveImports
      // method ...
      result = repositoryConnection.getStatements(null, OWL.IMPORTS, null, false);
      while(result.hasNext()) {
        String v = result.next().getObject().stringValue();
        uris.add(v); 
      }
    } catch (RepositoryException ex) {
      throw new GateOntologyException("Problem getting the import statements",ex);
    }
    return uris;
  }

  public Set<OURI> getOntologyURIs() {
    // apparently, this can return several ontology URIs, of which only
    // the one that is not object of an owl:priorVersion property is the
    // the one we want?

    // TODO: this just checks if the URI found is equal to one of the
    // import uri strings as present in the ontology, but does not check
    // against the actual import URI as it will be created from those
    // improt URI strings by replacing relative URI references.
    // Not sure how to really deal with this for now .... JP
    Set<OURI> theURIs =
        new HashSet<OURI>();

    Set<String> importURIs = getImportURIStrings();

    qp_getOntologyURIs.evaluate();
    while(qp_getOntologyURIs.hasNext()) {
      String someURI = qp_getOntologyURIs.nextFirstAsString();
      OURI u = new OURIImpl(someURI);
      if(!importURIs.contains(u.toString())) {
        theURIs.add(u);
      }
    }
    return theURIs;
  }

  /**
   * The method allows adding version information to the repository.
   *
   * @param versionInfo
   */
  public void setVersion(String versionInfo) {
    addUULStatement(this.ontologyUrl, OWL.VERSIONINFO.toString(), versionInfo, null);
  }

  /**
   * The method returns the version information of the repository.
   *
   * @return
   */
  public String getVersion() throws GateOntologyException {
    try {
      RepositoryResult<Statement> iter =
          repositoryConnection.getStatements(getResource(this.ontologyUrl),
          makeSesameURI(OWL.VERSIONINFO.toString()), null, true);
      while (iter.hasNext()) {
        Statement stmt = iter.next();
        return stmt.getObject().toString();
      }
    } catch (Exception e) {
      throw new GateOntologyException("Problem getting the ontology version: ", e);
    }
    return null;
  }


  public void setOntologyURI(OURI theURI) {
    addUUUStatement(theURI.toString(), RDF.TYPE.toString(), OWL.ONTOLOGY.toString());
  }



  // ***************************************************************************
  // **** CLASS RELATED METHODS
  // ***************************************************************************
  /**
   * The method allows adding a class to repository.
   *
   * @param classURI
   */
  public void addClass(String classURI, byte classType) {
    switch (classType) {
      case OConstants.OWL_CLASS:
        addUUUStatement(classURI, RDF.TYPE.toString(), OWL.CLASS.toString());
        return;
      default:
        addUUUStatement(classURI, RDF.TYPE.toString(), OWL.RESTRICTION.toString());
        return;
    }
  }

  /**
   * Given a class to delete, it removes it from the repository.
   *
   * @param classURI
   * @param removeSubTree
   *          - if set to true, removes all its subclasses and instances as
   *          well, otherwise shifts all subclasses and instances to its parent
   *          node
   * @return a list of other resources, which got removed as a result of this
   *         deletion
   */
  // TODO: !!! "its instances:" what if instances also belong to other classes
  // that do not get removed?
  // TODO: subclasses: what if a subclass also is a subclass of some other
  // class that does not get removed?
  // test/clarify in API!
  public String[] removeClass(String classURI,
      boolean removeSubTree) throws GateOntologyException {
    logger.debug("removeClass");
    //System.out.println("removeClass: "+classURI);
    ResourceInfo[] superClasses =
        getSuperClasses(classURI, OConstants.Closure.DIRECT_CLOSURE);

    List<String> deletedResources = new ArrayList<String>();
    // TODO: JP !!!! need a different way to check for explicit !!
    //if(removeUUUStatement(classURI, RDF.TYPE.toString(), null) == 0) {
    //  throw new GateOntologyException(classURI + " is not an explicit resource");
    //}
    //else {
    //  currentEventsLog.addEvent(new OEvent(classURI, RDF.TYPE.toString(), null, false));
    //  deletedResources.add(classURI);
    //}
    removeUUUStatement(classURI, RDF.TYPE.toString(), null);
    deletedResources.add(classURI);

    try {
      startTransaction(null);
      repositoryConnection.remove(getResource(classURI), makeSesameURI(RDFS.SUBCLASSOF.toString()),
          null);
      endTransaction(null);
    } catch (Exception sue) {
      throw new GateOntologyException("error while removing a class:" + classURI, sue);
    }

    // this should happen only if removeSubTree is set to true
    if (removeSubTree) {
      //ResourceInfo[] subClasses =
      //    getSubClassesOld(classURI, OConstants.Closure.DIRECT_CLOSURE);
      Set<OClass> subClasses = getSubClasses(
           string2ONodeID(classURI),  OConstants.Closure.DIRECT_CLOSURE);
      //for (int i = 0; i < subClasses.length; i++) {
      for (OClass subclass : subClasses) {
        String[] removedResources =
            //removeClass(subClasses[i].getUri(), true);
            removeClass(subclass.getONodeID().toString(), true);
        deletedResources.addAll(Arrays.asList(removedResources));
      }
      ClosableIterator<OInstance> instIt =
          getInstancesIterator(string2ONodeID(classURI), Closure.DIRECT_CLOSURE);
      while (instIt.hasNext()) {
        String[] removedResources = removeIndividual(instIt.next().toString());
        deletedResources.addAll(Arrays.asList(removedResources));
      }
    } else {
      //ResourceInfo[] subClasses =
      //    getSubClassesOld(classURI, OConstants.Closure.DIRECT_CLOSURE);
      Set<OClass> subClasses = getSubClasses(
           string2ONodeID(classURI),  OConstants.Closure.DIRECT_CLOSURE);
      //for (int i = 0; i < subClasses.length; i++) {
      for (OClass subclass : subClasses) {
        //removeSubClass(classURI, subClasses[i].getUri());
        removeSubClass(classURI,subclass.getONodeID().toString());
        for (int j = 0; j < superClasses.length; j++) {
          //addSubClass(superClasses[j].getUri(), subClasses[i].getUri());
          addSubClass(superClasses[j].getUri(), subclass.getONodeID().toString());
        }
      }
      ClosableIterator<OInstance> instIt =
          getInstancesIterator(string2ONodeID(classURI), Closure.DIRECT_CLOSURE);
      //for (int i = 0; i < individuals.length; i++) {
      while(instIt.hasNext()) {
        removeUUUStatement(instIt.next().toString(), RDF.TYPE.toString(), classURI);
        for (int j = 0; j < superClasses.length; j++) {
          addUUUStatement(instIt.next().toString(), RDF.TYPE.toString(), superClasses[j].getUri());
        }
      }
    }
    try {
      startTransaction(null);
      RepositoryResult<Statement> iter =
          repositoryConnection.getStatements(null, makeSesameURI(RDFS.DOMAIN.toString()), getResource(classURI), true);
      endTransaction(null);
      while (iter.hasNext()) {
        Statement stmt = iter.next();
        Resource resource = stmt.getSubject();
        String[] removedResources =
            removePropertyFromOntology(resource.toString(), removeSubTree);
        deletedResources.addAll(Arrays.asList(removedResources));
      }
      startTransaction(null);
      iter = repositoryConnection.getStatements(null, makeSesameURI(RDFS.RANGE.toString()), getResource(classURI), true);
      endTransaction(null);
      while (iter.hasNext()) {
        Statement stmt = iter.next();
        Resource resource = stmt.getSubject();
        String[] removedResources =
            removePropertyFromOntology(resource.toString(), removeSubTree);
        deletedResources.addAll(Arrays.asList(removedResources));
      }
    } catch (Exception e) {
      throw new GateOntologyException(e);
    }

    // finaly remove all statements concerning this resource
    try {
      startTransaction(null);
      repositoryConnection.remove(getResource(classURI), null, null);
      if (!(getResource(classURI) instanceof BNode)) {
        repositoryConnection.remove((Resource) null, makeSesameURI(classURI), null);
      }
      repositoryConnection.remove((Resource) null, null, getResource(classURI));

      endTransaction(null);
    } catch (Exception sue) {
      throw new GateOntologyException("error while removing a class:" + classURI, sue);
    }

    Collections.reverse(deletedResources);
    //System.out.println("deletedResources is: "+deletedResources);
    return listToArray(deletedResources);
  }

  /**
   * The method returns if the current repository has a class with URI that
   * matches with the class parameter.
   *
   * @return
   */
  public boolean hasClass(String classURI)
  {
    //System.out.println("Checking for class: "+classURI);
    try {
      // TODO: !!!!make two versions: one taking ONodeID and one String
      boolean hasOWLClass = 
          repositoryConnection.hasStatement(string2SesameResource(classURI),
          RDF.TYPE, OWL.CLASS, true);
      if (!hasOWLClass) {
        boolean hasRDFSClass = 
            repositoryConnection.hasStatement(string2SesameResource(classURI),
            RDF.TYPE, RDFS.CLASS, true);
        //System.out.println("OWL class not found, RDFS class "+hasRDFSClass);
        return hasRDFSClass;
      } else {
        //System.out.println("OWL class found");
        return true;
      }
    } catch (RepositoryException ex) {
      throw new GateOntologyException("Could not do hasStatement for class "+classURI);
    }
  }

  public boolean containsURI(OURI theURI) {
    try {
      return
          repositoryConnection.hasStatement(oNodeID2SesameResource(theURI), null, null, true) ||
          repositoryConnection.hasStatement(null, (URI) oNodeID2SesameResource(theURI), null, true) ||
          repositoryConnection.hasStatement(null, null, oNodeID2SesameResource(theURI), true);
    } catch (RepositoryException ex) {
      throw new GateOntologyException("Problem when looking up URI "+theURI,ex);
    }
  }


  public Set<OClass> getClasses(boolean topOnly) {
    Set<OClass> theClasses = new HashSet<OClass>();
    ClosableIterator<OClass> ci = getClassesIterator(topOnly);
    while(ci.hasNext()) {
      theClasses.add(ci.next());
    }
    return theClasses;
  }


  public ClosableIterator<OClass> getClassesIterator(boolean top) {
    if(top) {
      return new UtilResourceQueryIterator<OClass>(this, qp_getClassesTopAll, OClass.class);
    } else {
      return new UtilResourceQueryIterator<OClass>(this, qp_getClassesAllAll, OClass.class);
    }
  }

  public Set<OClass> getClassesByName(String name) {
    String query;
    Set<OClass> classes = new HashSet<OClass>();

    UtilTupleQueryIterator q;
    String qs = qs_getClassesByNameNoW3.replaceAll("yyy1", "\""+name+"\"");
    q = new UtilTupleQueryIterator(
            repositoryConnection, qs, ql_getClassesByNameNoW3);
      while (q.hasNext()) {
        Vector<Value> tuple = q.nextAsValue();
        Value t1 = tuple.get(0);
        String uristring = ((org.openrdf.model.URI) t1).toString();
        OURI ourURI = ontology.createOURI(uristring);
        classes.add(new OClassImpl(ourURI, ontology, this));
      }
      q.close();
    return classes;
  }

  private OClass createRestrictionFromURI(String uriString, String bnodeID) {
    OBNodeID uri = new OBNodeIDImpl(bnodeID);
    return createRestrictionFromURI(uriString, uri);
  }
  private OClass createRestrictionFromURI(String uriString, ONodeID uri) {
    if (uriString.equals("http://www.w3.org/2002/07/owl#cardinality")) {
      return new CardinalityRestrictionImpl(uri, ontology, this);
    } else if (uriString.equals("http://www.w3.org/2002/07/owl#allValuesFrom")) {
      return new AllValuesFromRestrictionImpl(uri, ontology, this);
    } else if (uriString.equals("http://www.w3.org/2002/07/owl#minCardinality")) {
      return new MinCardinalityRestrictionImpl(uri, ontology, this);
    } else if (uriString.equals("http://www.w3.org/2002/07/owl#maxCardinality")) {
      return new MaxCardinalityRestrictionImpl(uri, ontology, this);
    } else if (uriString.equals("http://www.w3.org/2002/07/owl#someValuesFrom")) {
      return new SomeValuesFromRestrictionImpl(uri, ontology, this);
    } else if (uriString.equals("http://www.w3.org/2002/07/owl#hasValue")) {
      return new HasValueRestrictionImpl(uri, ontology, this);
    } else if (uriString.equals("http://www.w3.org/2002/07/owl#oneOf")) {
      System.out.println("Warning: restriction oneOf not yet implemented in createRestrictionFromURI"+uri);
      return new AnonymousClassImpl(uri, ontology, this);
    }
    throw new GateOntologyException("createRestrictionFromURI not for this yet: " + uriString);
  }

  /**
   * Returns if the given class is a topOnly class.
   *
   * @param classURI
   * @return
   */
  public boolean isTopClass(String classURI) {
    // TODO: maybe this can be done more efficiently?
    return getSuperClasses(classURI, OConstants.Closure.DIRECT_CLOSURE).length == 0;
  }



  /**
   * Returns whether the theSuperClass is indeed a super class of the
   * theSubClassURI.
   * 
   * @param theSuperClassURI
   * @param theSubClassURI
   * @param direct
   * @return
   */
  public boolean isSuperClassOf(String theSuperClassURI,
      String theSubClassURI, Closure direct)
  {
    Resource r = getResource(theSubClassURI);
    String queryRep = string2Turtle(theSubClassURI);

    String query = "";
    query =
        "Select distinct SUPER FROM {" + queryRep + "} rdfs:subClassOf {SUPER}" + " WHERE SUPER!=" + queryRep + " AND SUPER != ALL ( " + " select distinct B FROM {B} owl:equivalentClass {" + queryRep + "} )";

    List<String> list = new ArrayList<String>();
    addSerqlQueryResultToCollection(query, list);

    if (direct == OConstants.Closure.DIRECT_CLOSURE) {
      Set<String> toDelete = new HashSet<String>();
      for (int i = 0; i < list.size(); i++) {
        String string = list.get(i);
        if (toDelete.contains(string)) {
          continue;
        }
        queryRep = string2Turtle(string);

        query =
            "Select distinct SUPER FROM {" + queryRep + "} rdfs:subClassOf {SUPER}" +
            " WHERE SUPER!=" + queryRep + " AND SUPER != ALL ( " +
            " select distinct B FROM {B} owl:equivalentClass {" + queryRep + "})";

        addSerqlQueryResultToCollection(query, toDelete);


      }
      list.removeAll(toDelete);
    }

    // here if the list contains the uri of the super class
    // we return true;
    return list.contains(theSuperClassURI);
  }

  protected void addSerqlQueryResultToCollection(String query, Collection<String> coll) {
    addSerqlQueryResultToCollection(query, coll, false);
  }

  protected void addSerqlQueryResultToCollection(String query, Collection<String> coll,
      boolean internIt) {
    UtilTupleQueryIterator result = 
        new UtilTupleQueryIterator(repositoryConnection,query,QueryLanguage.SERQL);
    //UtilTupleQueryIterator result = performSerqlQuery(query);
    while (result.hasNext()) {
      if (internIt) {
        coll.add(result.nextFirstAsString().intern());
      } else {
        String tmp = result.nextFirstAsString();
        //System.out.println("Adding to collection: "+tmp);
        coll.add(tmp);
      }
    }
    result.close();
  }

  protected void addSerqlQueryResultValuesToCollection(String query, Collection<Value> coll) {
    UtilTupleQueryIterator result = 
        new UtilTupleQueryIterator(repositoryConnection,query,QueryLanguage.SERQL);
    //UtilTupleQueryIterator result = performSerqlQuery(query);
    while (result.hasNext()) {
      coll.add(result.nextFirstAsValue());
    }
    result.close();
  }

  protected boolean hasSerqlQueryResultRows(String query) {
    //UtilTupleQueryIterator result = performSerqlQuery(query);
    UtilTupleQueryIterator result =
        new UtilTupleQueryIterator(repositoryConnection,query,QueryLanguage.SERQL);
    boolean hasResult = result.hasNext();
    result.close();
    return hasResult;
  }

  protected UtilTupleQueryIterator performSerqlQuery(String serqlQuery) {
    return 
        new UtilTupleQueryIterator(repositoryConnection,serqlQuery,QueryLanguage.SERQL);
  }


  /**
   * Returns whether the theSubClass is indeed a sub class of the
   * theSuperClassURI.
   * 
   * @param theSuperClassURI
   * @param theSubClassURI
   * @param direct
   * @return
   */
  public boolean isSubClassOf(String theSuperClassURI,
      String theSubClassURI, Closure direct) {
    return isSuperClassOf(theSuperClassURI, theSubClassURI,
        direct);
  }

  /**
   * Given a property URI, this method returns an object of Property
   * 
   * @param repositoryID
   * @param thePropertyURI
   * @return
   * @throws GateOntologyException
   */
  public Property getPropertyFromOntology(String thePropertyURI)
  {
    // here we need to check which type of property it is
    return createPropertyObject(thePropertyURI);
  }

  /**
   * Checks whether the two classes defined as same in the ontology.
   * 
   * @param theClassURI1
   * @param theClassURI2
   * @return
   * @throws Exception
   */
  public boolean isEquivalentClassAs(String theClassURI1,
      String theClassURI2)
  {

    String queryRep1 =  string2Turtle(theClassURI1);
    String queryRep2 =  string2Turtle(theClassURI2);

    String query =
        "SELECT * FROM {" + queryRep1 + "} owl:equivalentClass {" + queryRep2 + "}";
    return hasSerqlQueryResultRows(query);
  }

  // *******************************************************************
  // property methods
  // *******************************************************************
  // **************
  // Annotation Property
  // ************
  /**
   * Creates a new AnnotationProperty.
   * 
   * @param aPropertyURI
   *          URI of the property to be added into the ontology. Done
   */
  public void addAnnotationProperty(String aPropertyURI)
      throws GateOntologyException {
    addUUUStatement(aPropertyURI, RDF.TYPE.toString(), OWL.ANNOTATIONPROPERTY.toString());
  }

  /**
   * This method returns a set of all properties where the current resource has
   * been specified as one of the domain resources. Please note that this method
   * is different from the getAllSetProperties() method which returns a set of
   * properties set on the resource. For each property in the ontology, this
   * method checks if the current resource is valid domain. If so, the property
   * is said to be applicable, and otherwise not..
   * 
   * @param theResourceURI 
   * @return
   */
  public Property[] getPropertiesWithResourceAsDomain(
      String theResourceURI)
  {

    List<Property> list = new ArrayList<Property>();

    HashSet<String> toCheck = new HashSet<String>();
    try {
      if (repositoryConnection.hasStatement(
          string2SesameResource(theResourceURI),
          RDF.TYPE,
          OWL.CLASS,
          true // TODO: include inferred: true or false here?
          )) {

        String queryRep1 = string2Turtle(theResourceURI);
        String queryRep  = "{"+queryRep1+"}";

        String query =
            "Select distinct SUPER FROM " + queryRep + " rdfs:subClassOf {SUPER}" + " WHERE SUPER!=" + queryRep1 + " AND SUPER != ALL ( " + " select distinct B FROM {B} owl:equivalentClass " + queryRep + ")";

        addSerqlQueryResultToCollection(query, toCheck);

        toCheck.add(theResourceURI);
      } else if (repositoryConnection.hasStatement(
          string2SesameResource(theResourceURI),
          RDF.TYPE,
          RDF.PROPERTY, true)) {

        String queryRep1 = string2Turtle(theResourceURI);
        String queryRep = "{"+queryRep1+"}";


        String query =
            "Select distinct SUPER FROM " + queryRep + " rdfs:subPropertyOf {SUPER}" + " WHERE SUPER!=" + queryRep1 + " AND SUPER != ALL ( " + " select distinct B FROM {B} owl:equivalentProperty " + queryRep + ")";

        addSerqlQueryResultToCollection(query, toCheck);

        toCheck.add(theResourceURI);
      } else {
        // it is an instance
        String queryRep1 = string2Turtle(theResourceURI);
        String query =
            "Select DISTINCT B from {X} rdf:type {B} WHERE X=" + queryRep1 ;

        addSerqlQueryResultToCollection(query, toCheck, true);

      }
    } catch (Exception e) {
      throw new GateOntologyException("Error getting statements", e);
    }

    String query =
        "Select distinct X FROM {X} rdf:type {<" + OWL.ANNOTATIONPROPERTY + ">}";

    UtilTupleQueryIterator result = performSerqlQuery(query);
    while (result.hasNext()) {
      String anAnnProp = result.nextFirstAsString();
      list.add(new Property(OConstants.ANNOTATION_PROPERTY, anAnnProp));
    }
    result.close();

    boolean allowSystemStatements = this.returnSystemStatements;
    this.returnSystemStatements = true;
    Property[] props = listToPropertyArray(list);
    this.returnSystemStatements = allowSystemStatements;

    // now we obtain all datatype properties
    list = new ArrayList<Property>();
    query = "Select X FROM {X} rdf:type {<" + OWL.DATATYPEPROPERTY + ">}";

    //iter = performQuery(query);
    result = performSerqlQuery(query);
    while (result.hasNext()) {
      String anAnnProp = result.nextFirstAsString();
      // for each property we obtain its domain and search for the
      // resourceURI in it
      query =
          "select distinct Y from {<" + anAnnProp + ">} rdfs:domain {Y}";
      Set<String> set = new HashSet<String>();
      addSerqlQueryResultToCollection(query, set, true);

      if (set.isEmpty()) {
        list.add(new Property(OConstants.DATATYPE_PROPERTY, anAnnProp.toString()));
      }

      set = new HashSet<String>(reduceToMostSpecificClasses(set));

      set.retainAll(toCheck);
      if (!set.isEmpty()) {
        list.add(new Property(OConstants.DATATYPE_PROPERTY, anAnnProp.toString()));
      }
    }
    result.close();

    query = "Select X FROM {X} rdf:type {<" + OWL.OBJECTPROPERTY + ">}";

    result = performSerqlQuery(query);
    while (result.hasNext()) {
      String anAnnProp = result.nextFirstAsString();
      // for each property we obtain its domain and search for the
      // resourceURI in it
      query =
          "select distinct Y from {<" + anAnnProp + ">} rdfs:domain {Y}";
      Set<String> set = new HashSet<String>();
      addSerqlQueryResultToCollection(query, set, true);

      set = new HashSet<String>(reduceToMostSpecificClasses(set));

      byte type = OConstants.OBJECT_PROPERTY;

      if (set.isEmpty()) {
        if (isSymmetricProperty(anAnnProp.toString().intern())) {
          type = OConstants.SYMMETRIC_PROPERTY;
        } else if (isTransitiveProperty(anAnnProp.toString().intern())) {
          type = OConstants.TRANSITIVE_PROPERTY;
        }

        list.add(new Property(type, anAnnProp.toString()));
        continue;
      }
      set.retainAll(toCheck);
      if (!set.isEmpty()) {
        if (isSymmetricProperty(anAnnProp.toString().intern())) {
          type = OConstants.SYMMETRIC_PROPERTY;
        } else if (isTransitiveProperty(anAnnProp.toString().intern())) {
          type = OConstants.TRANSITIVE_PROPERTY;
        }

        list.add(new Property(type, anAnnProp.toString()));
      }
    }
    result.close();
    Property[] props1 = listToPropertyArray(list);
    Property[] toProps = new Property[props.length + props1.length];
    for (int i = 0; i < props.length; i++) {
      toProps[i] = props[i];
    }

    for (int i = 0; i < props1.length; i++) {
      toProps[props.length + i] = props1[i];
    }

    return toProps;

  }

  /**
   * This method returns a set of all properties where the current resource has
   * been specified as one of the range resources. Please note that this method
   * is different from the getAllSetProperties() method which returns a set of
   * properties set on the resource. For each property in the ontology, this
   * method checks if the current resource is valid range. If so, the property
   * is said to be applicable, and otherwise not.
   * 
   * @return
   */
  public Property[] getPropertiesWithResourceAsRange(String theResourceURI)
  {
    List<Property> list = new ArrayList<Property>();

    HashSet<String> toCheck = new HashSet<String>();
    try {
      if (repositoryConnection.hasStatement(getResource(theResourceURI), makeSesameURI(RDF.TYPE.toString()),
          getResource(OWL.CLASS.toString()), true)) {

        String queryRep = string2Turtle(theResourceURI);
        String query =
            "Select distinct SUPER FROM {" + queryRep + "} rdfs:subClassOf {SUPER}" + " WHERE SUPER!=" + queryRep + " AND SUPER != ALL ( " + " select distinct B FROM {B} owl:equivalentClass {" + queryRep + "} )";

        addSerqlQueryResultToCollection(query, toCheck);

        toCheck.add(theResourceURI);
      } else if (repositoryConnection.hasStatement(getResource(theResourceURI), makeSesameURI(RDF.TYPE.toString()),
          getResource(RDF.PROPERTY.toString()), true)) {

        String queryRep = string2Turtle(theResourceURI);
        String query =
          "Select distinct SUPER FROM {"+queryRep+"} rdfs:subPropertyOf {SUPER}" +
          " WHERE SUPER!=" + queryRep +
          " AND SUPER != ALL ( " +
          " select distinct B FROM {B} owl:equivalentProperty {" + queryRep + "})";

        addSerqlQueryResultToCollection(query, toCheck);

        toCheck.add(theResourceURI);
      } else {
        // it is an instance
        String query =
            "Select DISTINCT B from {X} rdf:type {B} WHERE X=<" + theResourceURI + ">";

        addSerqlQueryResultToCollection(query, toCheck, true);
      }
    } catch (Exception e) {
      throw new GateOntologyException("Could not get statements", e);
    }

    String query =
        "Select distinct X FROM {X} rdf:type {<" + OWL.ANNOTATIONPROPERTY + ">}";

    UtilTupleQueryIterator result = performSerqlQuery(query);
    while (result.hasNext()) {
      String anAnnProp = result.nextFirstAsString();
      list.add(new Property(OConstants.ANNOTATION_PROPERTY, anAnnProp));
    }
    result.close();

    boolean allowSystemStatements = this.returnSystemStatements;
    this.returnSystemStatements = true;
    Property[] props = listToPropertyArray(list);
    this.returnSystemStatements = allowSystemStatements;

    // now we obtain all datatype properties
    list = new ArrayList<Property>();
    query = "Select X FROM {X} rdf:type {<" + OWL.OBJECTPROPERTY + ">}";

    result = performSerqlQuery(query);
    while (result.hasNext()) {
      String anAnnProp = result.nextFirstAsString();
      // for each property we obtain its domain and search for the
      // resourceURI in it
      query =
          "select distinct Y from {<" + anAnnProp + ">} rdfs:range {Y}";
      Set<String> set = new HashSet<String>();
      addSerqlQueryResultToCollection(query, set, true);

      set = new HashSet<String>(reduceToMostSpecificClasses(set));

      byte type = OConstants.OBJECT_PROPERTY;

      if (set.isEmpty()) {
        if (isSymmetricProperty(anAnnProp.toString().intern())) {
          type = OConstants.SYMMETRIC_PROPERTY;
        } else if (isTransitiveProperty(anAnnProp.toString().intern())) {
          type = OConstants.TRANSITIVE_PROPERTY;
        }

        list.add(new Property(type, anAnnProp.toString()));
      }

      set.retainAll(toCheck);
      if (!set.isEmpty()) {
        if (isSymmetricProperty(anAnnProp.toString().intern())) {
          type = OConstants.SYMMETRIC_PROPERTY;
        } else if (isTransitiveProperty(anAnnProp.toString().intern())) {
          type = OConstants.TRANSITIVE_PROPERTY;
        }

        list.add(new Property(type, anAnnProp.toString()));
      }
    }
    result.close();
    Property[] props1 = listToPropertyArray(list);
    Property[] toProps = new Property[props.length + props1.length];
    for (int i = 0; i < props.length; i++) {
      toProps[i] = props[i];
    }

    for (int i = 0; i < props1.length; i++) {
      toProps[props.length + i] = props1[i];
    }

    return toProps;
  }

  /**
   * Gets the annotation properties set on the specified resource
   * 
   * @param theResourceURI
   * @return
   */
  public Property[] getAnnotationProperties(String theResourceURI)
  {
    List<Property> list = new ArrayList<Property>();

    String queryRep = string2Turtle(theResourceURI);

    String query =
        "Select DISTINCT X FROM {X} rdf:type {<" + OWL.ANNOTATIONPROPERTY + ">} WHERE EXISTS (SELECT * FROM {" + queryRep + "} X {Z})";

    UtilTupleQueryIterator result = performSerqlQuery(query);
    while (result.hasNext()) {
      list.add(new Property(OConstants.ANNOTATION_PROPERTY, result.nextFirstAsString()));
    }
    result.close();
    boolean allowSystemStatements = this.returnSystemStatements;
    this.returnSystemStatements = true;
    Property[] props = listToPropertyArray(list);
    this.returnSystemStatements = allowSystemStatements;
    return props;
  }

  /**
   * Gets the RDF properties set on the specified resource
   * 
   * @param theResourceURI
   * @return
   */
  public Property[] getRDFProperties(String theResourceURI)
  {
    List<Property> list = new ArrayList<Property>();

    String queryRep = string2Turtle(theResourceURI);
    String query =
        "Select distinct X FROM {X} rdf:type {<" + RDF.PROPERTY.toString() + ">} WHERE EXISTS (SELECT * FROM {" + queryRep + "} X {Z})";

    UtilTupleQueryIterator result = performSerqlQuery(query);
    while (result.hasNext()) {
      String propString = result.nextFirstAsString();
      if (isAnnotationProperty(propString) || isDatatypeProperty(propString) || isObjectProperty(propString) || isTransitiveProperty(propString) || isSymmetricProperty(propString)) {
        continue;
      }
      list.add(new Property(OConstants.RDF_PROPERTY, propString));
    }
    result.close();
    return listToPropertyArray(list);
  }

  /**
   * Gets the datatype properties set on the specified resource
   * 
   * @param theResourceURI
   * @return
   */
  public Property[] getDatatypeProperties(String theResourceURI)
  {
    List<Property> list = new ArrayList<Property>();
    String queryRep = string2Turtle(theResourceURI);
    String query =
        "Select distinct X FROM {X} rdf:type {<" + OWL.DATATYPEPROPERTY + ">} WHERE EXISTS (SELECT * FROM {" + queryRep + "} X {Z})";

    UtilTupleQueryIterator result = performSerqlQuery(query);
    while (result.hasNext()) {
      String anAnnProp = result.nextFirstAsString();
      list.add(new Property(OConstants.DATATYPE_PROPERTY, anAnnProp));
    }
    result.close();
    return listToPropertyArray(list);
  }

  /**
   * Gets the object properties set on the specified resource
   * 
   * @param theResourceURI
   * @return
   */
  public Property[] getObjectProperties(String theResourceURI) {
    List<Property> list = new ArrayList<Property>();
    String queryRep = string2Turtle(theResourceURI);
    String query =
        "Select distinct X FROM {X} rdf:type {<" + OWL.OBJECTPROPERTY + ">} WHERE EXISTS (SELECT * FROM {" + queryRep + "} X {Z})";

    UtilTupleQueryIterator result = performSerqlQuery(query);
    while (result.hasNext()) {
      String anAnnProp = result.nextFirstAsString();
      list.add(new Property(OConstants.OBJECT_PROPERTY, anAnnProp));
    }
    result.close();
    return listToPropertyArray(list);
  }

  /**
   * Gets the transitive properties set on the specified resource
   * 
   * @param theResourceURI
   * @return
   */
  public Property[] getTransitiveProperties(String theResourceURI)
  {
    List<Property> list = new ArrayList<Property>();
    String queryRep = string2Turtle(theResourceURI);
    String query =
        "Select distinct X FROM {X} rdf:type {<" + OWL.TRANSITIVEPROPERTY + ">} WHERE EXISTS (SELECT * FROM {" + queryRep + "} X {Z})";

    UtilTupleQueryIterator result = performSerqlQuery(query);
    while (result.hasNext()) {
      String anAnnProp = result.nextFirstAsString();
      list.add(new Property(OConstants.TRANSITIVE_PROPERTY, anAnnProp));
    }
    result.close();
    return listToPropertyArray(list);
  }

  /**
   * Gets the symmetric properties set on the specified resource
   * 
   * @param theResourceURI
   * @return
   */
  public Property[] getSymmetricProperties(String theResourceURI)
  {
    List<Property> list = new ArrayList<Property>();
    String queryRep = string2Turtle(theResourceURI);
    String query =
        "Select distinct X FROM {X} rdf:type {<" + OWL.SYMMETRICPROPERTY + ">} WHERE EXISTS (SELECT * FROM {" + queryRep + "} X {Z})";

    UtilTupleQueryIterator result = performSerqlQuery(query);
    while (result.hasNext()) {
      String anAnnProp = result.nextFirstAsString();
      list.add(new Property(OConstants.SYMMETRIC_PROPERTY, anAnnProp));
    }
    result.close();
    return listToPropertyArray(list);
  }

  /**
   * returns if the given property is an Annotation property
   * 
   * @param aPropertyURI
   * @return Done
   */
  public boolean isAnnotationProperty(String aPropertyURI)
  {
    String query =
        "Select * FROM {X} rdf:type {<" + OWL.ANNOTATIONPROPERTY + ">} WHERE X=<" + aPropertyURI + ">";
    return hasSerqlQueryResultRows(query);
  }

  /**
   * Adds a new annotation property value and specifies the language.
   * 
   * @param theAnnotationProperty
   *          the annotation property
   * @param value
   *          the value containing some value
   */
  public void addAnnotationPropertyValue(String theResourceURI, String theAnnotationPropertyURI, String value,
      String language)
  {
    // isAnnotationProperty also checks for the correct repository so no
    // need to give a call to it
    if (!isAnnotationProperty(theAnnotationPropertyURI)) {
      throw new GateOntologyException(
          "No annotation property found with the URI :" + theAnnotationPropertyURI);
    }
    addUULStatement(theResourceURI, theAnnotationPropertyURI, value, language);
  }

  /**
   * Gets the list of annotation property values
   * 
   * @param theResourceURI
   * @param theAnnotationPropertyURI
   * @return
   */
  public PropertyValue[] getAnnotationPropertyValues(String theResourceURI, String theAnnotationPropertyURI)
  {
    // isAnnotationProperty also checks for the correct repository so no
    // need to give a call to it
    if (!isAnnotationProperty(theAnnotationPropertyURI)) {
      throw new GateOntologyException(
          "No annotation property found with the URI :" + theAnnotationPropertyURI);
    }

    Resource r2 = getResource(theResourceURI);
    String queryRep21 = "<" + theResourceURI + ">";
    if (r2 instanceof BNode) {
      queryRep21 = "_:" + theResourceURI;
    }

    List<PropertyValue> list = new ArrayList<PropertyValue>();
    String query =
        "Select DISTINCT Y from {X} <" + theAnnotationPropertyURI + "> {Y} WHERE X=" + queryRep21 + " AND isLiteral(Y)";

    UtilTupleQueryIterator result = performSerqlQuery(query);
    while (result.hasNext()) {
      // TODO: how to process a literal here? Is the string representation
      // we get identical with literal.getLabel()?
      PropertyValue pv;
      Literal literal = (Literal) result.nextFirstAsValue();
      //Literal literal = (Literal)iter.getValue(i, 0);
      //pv = new PropertyValue(literal.getLanguage(), literal.getLabel());
      pv = new PropertyValue(literal.getLanguage(), literal.getLabel());
      list.add(pv);
    }
    return listToPropertyValueArray(list);
  }

  /**
   * Gets the annotation property for the given resource uri.
   * 
   * @param repositoryID
   * @param theResourceURI
   * @param theAnnotationPropertyURI
   * @param language
   * @return
   */
  public String getAnnotationPropertyValue(String theResourceURI, String theAnnotationPropertyURI, String language)
  {
    // isAnnotationProperty also checks for the correct repository so no
    // need to give a call to it
    if (!isAnnotationProperty(theAnnotationPropertyURI)) {
      throw new GateOntologyException(
          "No annotation property found with the URI :" + theAnnotationPropertyURI);
    }
    // TODO: !!! use toTurtle
    Resource r2 = getResource(theResourceURI);
    String queryRep21 = "<" + theResourceURI + ">";
    if (r2 instanceof BNode) {
      queryRep21 = "_:" + theResourceURI;
    }

    String query =
        "Select Y from {X} <" + theAnnotationPropertyURI + "> {Y} WHERE X=" + queryRep21 + " AND isLiteral(Y) AND lang(Y) LIKE \"" + language + "\"";

    UtilTupleQueryIterator result = performSerqlQuery(query);
    Literal literal = null;
    if (result.hasNext()) {
      literal = (Literal) result.nextFirstAsValue();
    }
    result.close();
    if (literal != null) {
      return literal.getLabel();
    } else {
      return null;
    }
  }

  /**
   * For the current resource, the method removes the given literal for the
   * given property.
   * 
   * @param theAnnotationProperty
   * @param literal
   */
  public void removeAnnotationPropertyValue(String theResourceURI, String theAnnotationPropertyURI, String value,
      String language)
  {
    // isAnnotationProperty also checks for the correct repository so no
    // need to give a call to it
    if (!isAnnotationProperty(theAnnotationPropertyURI)) {
      throw new GateOntologyException(
          "No annotation property found with the URI :" + theAnnotationPropertyURI);
    }
    startTransaction(null);
    removeUULStatement(theResourceURI, theAnnotationPropertyURI, value,
        language);
    endTransaction(null);
  }

  /**
   * Removes all values for a named property.
   * 
   * @param theProperty
   *          the property
   */
  public void removeAnnotationPropertyValues(String theResourceURI, String theAnnotationPropertyURI)
  {
    try {
      // isAnnotationProperty also checks for the correct repository so
      // no
      // need to give a call to it
      if (!isAnnotationProperty(theAnnotationPropertyURI)) {
        throw new GateOntologyException(
            "No annotation property found with the URI :" + theAnnotationPropertyURI);
      }
      startTransaction(null);
      repositoryConnection.remove(
          getResource(theResourceURI),
          makeSesameURI(theAnnotationPropertyURI), null);
      endTransaction(null);
    } catch (Exception e) {
      throw new GateOntologyException(
          "Error while removing annotation property values " + e.getMessage(), e);
    }
  }

  // **************
  // RDFProperties
  // *************
  /**
   * The method adds a generic property specifiying domain and range for the
   * same. All classes specified in domain and range must exist.
   * 
   * @param aPropertyURI
   * @param domainClassesURIs
   * @param rangeClassesTypes
   *          Done
   */
  public void addRDFProperty(String aPropertyURI,
      String[] domainClassesURIs, String[] rangeClassesTypes)
      throws GateOntologyException {
    addUUUStatement(aPropertyURI, RDF.TYPE.toString(), RDF.PROPERTY.toString());

    if (domainClassesURIs != null) {
      for (int i = 0; i < domainClassesURIs.length; i++) {
        addUUUStatement(aPropertyURI, RDFS.DOMAIN.toString(), domainClassesURIs[i]);
      }
    }
    if (rangeClassesTypes != null) {
      for (int i = 0; i < rangeClassesTypes.length; i++) {
        addUUUStatement(aPropertyURI, RDFS.RANGE.toString(), rangeClassesTypes[i]);
      }
    }
  }

  /**
   * returns if the given property is an RDF property
   * 
   * @param aPropertyURI
   * @return Done
   */
  public boolean isRDFProperty(String aPropertyURI)
  {
    boolean found =
        isAnnotationProperty(aPropertyURI) || isDatatypeProperty(aPropertyURI) || isObjectProperty(aPropertyURI) || isTransitiveProperty(aPropertyURI) || isSymmetricProperty(aPropertyURI);
    if (!found) {
      String query =
          "Select * FROM {X} rdf:type {<" + RDF.PROPERTY.toString() + ">} WHERE X=<" + aPropertyURI + ">";
      return hasSerqlQueryResultRows(query);
    } else {
      return false;
    }
  }

  // **************
  // Datatype Properties
  // *************
  /**
   * The method adds a data type property specifiying domain and range for the
   * same. All classes specified in domain and range must exist.
   * 
   * @param aPropertyURI
   * @param domainClassesURIs
   * @param dataTypeURI
   *          Done
   */
  public void addDataTypeProperty(String aPropertyURI,
      String[] domainClassesURIs, String dataTypeURI)
 {
    addUUUStatement(aPropertyURI, RDF.TYPE.toString(), OWL.DATATYPEPROPERTY.toString());
    addUUUStatement(aPropertyURI, RDFS.RANGE.toString(), dataTypeURI);

    if (domainClassesURIs != null) {
      for (int i = 0; i < domainClassesURIs.length; i++) {
        addUUUStatement(aPropertyURI, RDFS.DOMAIN.toString(), domainClassesURIs[i]);
      }
    }
  }

  /**
   * Returns the datatype uri specified for the given datatype property.
   * 
   * @param theDatatypePropertyURI
   * @return
   * @throws GateOntologyException
   */
  public String getDatatype(String theDatatypePropertyURI)
  {
    // isAnnotationProperty also checks for the correct repository so no
    // need to give a call to it
    if (!isDatatypeProperty(theDatatypePropertyURI)) {
      throw new GateOntologyException(
          "Invalid DatatypeProperty :" + theDatatypePropertyURI);
    }

    String query =
        "Select Z from {<" + theDatatypePropertyURI + ">} rdfs:range" + " {Z}";
    UtilTupleQueryIterator result = performSerqlQuery(query);

    String toReturn = null;
    if (result.hasNext()) {
      toReturn = result.nextFirstAsString();
    }
    result.close();
    if (OntologyUtilities.getDataType(toReturn) != null) {
      return toReturn;
    }
    return "http://www.w3.org/2001/XMLSchema#string";
  }

  // **************
  // Symmetric Properties
  // *************
  /**
   * The method adds a symmetric property specifying domain and range for the
   * same. All classes specified in domain and range must exist.
   * 
   * @param aPropertyURI
   * @param domainAndRangeClassesURIs
   *          Done
   */
  public void addSymmetricProperty(String aPropertyURI,
      String[] domainAndRangeClassesURIs)
  {
    if (debug) {
      logger.debug("addSymmetricProperty");
    }
    addUUUStatement(aPropertyURI, RDF.TYPE.toString(), OWL.SYMMETRICPROPERTY.toString());
    if (domainAndRangeClassesURIs != null) {
      for (int i = 0; i < domainAndRangeClassesURIs.length; i++) {
        addUUUStatement(aPropertyURI, RDFS.DOMAIN.toString(), domainAndRangeClassesURIs[i]);
        addUUUStatement(aPropertyURI, RDFS.RANGE.toString(), domainAndRangeClassesURIs[i]);
      }
    }
  }

  /**
   * Checkes whether the two properties are Equivalent.
   * 
   * @param aPropertyURI
   * @return
   * @throws GateOntologyException
   */
  public boolean isEquivalentPropertyAs(String aPropertyURI1, String aPropertyURI2)
  {
    String query =
        "Select * FROM {<" + aPropertyURI1 + ">} " + OWL.EQUIVALENTPROPERTY + " {<" + aPropertyURI2 + ">}";
    return hasSerqlQueryResultRows(query);
  }

  /**
   * for the given property, the method returns all its super properties
   * 
   * @param aPropertyURI
   * @param direct
   * @return
   */
  public Property[] getSuperProperties(String aPropertyURI, 
      Closure direct)
  {
    String queryRep = string2Turtle(aPropertyURI);

    String query = "";
    query =
        "Select distinct SUPER FROM {" + queryRep + "} rdfs:subPropertyOf {SUPER}" +
        " WHERE SUPER!=" + queryRep +
        " AND SUPER != ALL ( " +
        " select distinct B FROM {B} owl:equivalentProperty {" + queryRep + "} )";

    List<String> list = new ArrayList<String>();
    addSerqlQueryResultToCollection(query, list);

    if (direct == OConstants.Closure.DIRECT_CLOSURE) {
      Set<String> toDelete = new HashSet<String>();
      for (int i = 0; i < list.size(); i++) {
        String string = list.get(i);
        if (toDelete.contains(string)) {
          continue;
        }
        queryRep = string2Turtle(string);
        query =
            "Select distinct SUPER FROM {" + queryRep + "} rdfs:subPropertyOf {SUPER}" +
            " WHERE SUPER!=" + queryRep +
            " AND SUPER != ALL ( " +
            " select distinct B FROM {B} owl:equivalentProperty {" + queryRep + "} )";

        addSerqlQueryResultToCollection(query, toDelete);
      }
      list.removeAll(toDelete);
    }

    ArrayList<Property> properties = new ArrayList<Property>();
    for (int i = 0; i < list.size(); i++) {
      byte type = getPropertyType(list.get(i));
      properties.add(new Property(type, list.get(i)));
    }

    return listToPropertyArray(properties);
  }

  /**
   * for the given property, the method returns all its sub properties
   * 
   * @param aPropertyURI
   * @param direct
   * @return
   */
  public Property[] getSubProperties(String aPropertyURI,
      Closure direct)
  {

    String queryRep = string2Turtle(aPropertyURI);
    String query = "";
    query =
        // OLD "Select distinct SUB FROM {SUB} rdfs:subPropertyOf " + queryRep + " WHERE SUB!=" + queryRep1 + " MINUS " + " select distinct B FROM {B} owl:equivalentProperty " + queryRep;
        "Select distinct SUB FROM {SUB} rdfs:subPropertyOf {" + queryRep + "} WHERE SUB!=" +
        queryRep +
        " AND SUB !=ALL( select distinct B FROM {B} owl:equivalentProperty {" + queryRep + "} )";


    List<String> list = new ArrayList<String>();
    addSerqlQueryResultToCollection(query, list);

    if (direct == OConstants.Closure.DIRECT_CLOSURE) {
      Set<String> toDelete = new HashSet<String>();
      for (int i = 0; i < list.size(); i++) {
        String string = list.get(i);
        if (toDelete.contains(string)) {
          continue;
        }
        queryRep = string2Turtle(string);
        query =
            "Select distinct SUB FROM {SUB} rdfs:subPropertyOf {" + queryRep +
            "} WHERE SUB!=" + queryRep + " AND SUB != ALL ( " +
            " select distinct B FROM {B} owl:equivalentProperty {" + queryRep + "} )";

        addSerqlQueryResultToCollection(query, toDelete);
      }
      list.removeAll(toDelete);
    }

    ArrayList<Property> properties = new ArrayList<Property>();
    for (int i = 0; i < list.size(); i++) {
      byte type = getPropertyType(list.get(i));
      properties.add(new Property(type, list.get(i)));
    }

    return listToPropertyArray(properties);
  }

  /**
   * Checkes whether the two properties have a super-sub relation.
   * 
   * @param aSuperPropertyURI
   * @param aSubPropertyURI
   * @param direct
   * @return
   */
  public boolean isSuperPropertyOf(String aSuperPropertyURI, 
      String aSubPropertyURI, Closure direct)
  {
    String queryRep = string2Turtle(aSuperPropertyURI);
    String query = "";
    query =
        "Select distinct SUPER FROM {" + queryRep +
        "} rdfs:subPropertyOf {SUPER}" +
        " WHERE SUPER!=" + queryRep +
        " AND SUPER != ALL ( " +
        " select distinct B FROM {B} owl:equivalentProperty {" + queryRep + "} )";

    List<String> list = new ArrayList<String>();
    addSerqlQueryResultToCollection(query, list);

    if (direct == OConstants.Closure.DIRECT_CLOSURE) {
      Set<String> toDelete = new HashSet<String>();
      for (int i = 0; i < list.size(); i++) {
        String string = list.get(i);
        if (toDelete.contains(string)) {
          continue;
        }
        queryRep = string2Turtle(string);
        query =
            "Select distinct SUPER FROM {" + queryRep +
            "} rdfs:subPropertyOf {SUPER}" +
            " WHERE SUPER!=" + queryRep + " AND SUPER != ALL ( " +
            " select distinct B FROM {B} owl:equivalentProperty {" + queryRep + "} )";

        addSerqlQueryResultToCollection(query, toDelete);
      }
      list.removeAll(toDelete);
    }

    return list.contains(aSuperPropertyURI);
  }

  /**
   * Checkes whether the two properties have a super-sub relation.
   * 
   * @param aSuperPropertyURI
   * @param aSubPropertyURI
   * @param direct
   * @return
   */
  public boolean isSubPropertyOf(String aSuperPropertyURI,
      String aSubPropertyURI, Closure direct)
  {
    return isSuperPropertyOf(aSuperPropertyURI, aSubPropertyURI,
        direct);
  }

  /**
   * Returns whether the individual1 is different from the individual2.
   * 
   * @param theInstanceURI1
   * @param theInstanceURI2
   * @return
   */
  public boolean isDifferentIndividualFrom(String theInstanceURI1, String theInstanceURI2)
  {
    String query =
        "Select * from {<" + theInstanceURI1 + ">} owl:differentFrom {<" + theInstanceURI2 + ">}";
    return hasSerqlQueryResultRows(query);
  }

  /**
   * Checkes whether the two individuals are same.
   * 
   * @param individualURI1
   * @param invidualURI2
   * @return
   */
  public boolean isSameIndividualAs(String theInstanceURI1, String theInstanceURI2)
  {
    String query =
        "Select * from {<" + theInstanceURI1 + ">} owl:sameAs {<" + theInstanceURI2 + ">}";
    return hasSerqlQueryResultRows(query);
  }

  // *************
  // Instances and properties
  // **************
  /**
   * adds the RDF Property value on the specified instance
   * 
   * @param anInstanceURI
   * @param anRDFPropertyURI
   * @param aResourceURI
   */
  public void addRDFPropertyValue(String anInstanceURI,
      String anRDFPropertyURI, String aResourceURI)
  {
    addUUUStatement(anInstanceURI, anRDFPropertyURI, aResourceURI);
  }

  /**
   * Removes the specified RDF Property Value
   * 
   * @param anInstanceURI
   * @param anRDFPropertyURI
   * @param aResourceURI
   */
  public void removeRDFPropertyValue(String anInstanceURI,
      String anRDFPropertyURI, String aResourceURI)
  {
    startTransaction(null);
    removeUUUStatement(anInstanceURI, anRDFPropertyURI, aResourceURI);
    endTransaction(null);
  }

  /**
   * gets the rdf property values for the specified instance.
   * 
   * @param anInstanceURI
   * @param anRDFPropertyURI
   * @return resource URIs
   */
  public ResourceInfo[] getRDFPropertyValues(String anInstanceURI, String anRDFPropertyURI)
  {
    Resource r = getResource(anInstanceURI);
    // TODO: !!! turtle
    String queryRep2 = "<" + anInstanceURI + ">";
    if (r instanceof BNode) {
      queryRep2 = "_:" + anInstanceURI;
    }

    List<String> list = new ArrayList<String>();
    String query =
        "Select DISTINCT Y from {X} <" + anRDFPropertyURI + "> {Y} WHERE X=" + queryRep2;

    addSerqlQueryResultToCollection(query, list);
    return listToResourceInfoArray(list);
  }

  public List<LiteralOrONodeID> getRDFPropertyLiteralOrONodeIDs(
      ONodeID anInstanceURI, OURI anRDFPropertyURI)
  {

    List<LiteralOrONodeID> list = new LinkedList<LiteralOrONodeID>();
    String query =
        "Select DISTINCT Y from {" + anInstanceURI.toTurtle() +
        "} " + anRDFPropertyURI.toTurtle() +
        " {Y}";

    UtilTupleQueryIterator qit =
        new UtilTupleQueryIterator(
          repositoryConnection, query, OConstants.QueryLanguage.SERQL);
    while(qit.hasNext()) {
      list.add(qit.nextFirst());
    }
    return list;
  }



  /**
   * Removes all the RDF Property values from the given instance.
   * 
   * @param anInstanceURI
   * @param anRDFPropertyURI
   */
  public void removeRDFPropertyValues(String anInstanceURI, String anRDFPropertyURI)
  {
    try {
      repositoryConnection.remove(getResource(anInstanceURI),
          makeSesameURI(anRDFPropertyURI), null);
    } catch (Exception sue) {
      throw new GateOntologyException(
          "Error while removing RDF Property Values " + sue.getMessage(), sue);
    }
  }

  // ******************
  // DataType Properties
  // *****************
  /**
   * Adds the value for the given Property.
   * 
   * @param anInstanceURI
   * @param aDatatypePropertyURI
   * @param datatypeURI
   * @param value
   */
  public void addDatatypePropertyValue(String anInstanceURI, String aDatatypePropertyURI, String datatypeURI,
      String value)
  {
    if (!isDatatypeProperty(aDatatypePropertyURI)) {
      throw new GateOntologyException(
          "No datatype property exists with URI :" + aDatatypePropertyURI);
    }
    addUUDStatement(anInstanceURI, aDatatypePropertyURI, value,
        datatypeURI);
  }

  /**
   * Removes the provided value for the given instance.
   * 
   * @param anInstanceURI
   * @param aDatatypePropertyURI
   * @param datatypeURI
   * @param value
   */
  public void removeDatatypePropertyValue(String anInstanceURI, String aDatatypePropertyURI, String datatypeURI,
      String value)
  {
    if (!isDatatypeProperty(aDatatypePropertyURI)) {
      throw new GateOntologyException(
          "No datatype property exists with URI :" + aDatatypePropertyURI);
    }
    startTransaction(null);
    removeUUDStatement(anInstanceURI, aDatatypePropertyURI,
        value, datatypeURI);
    endTransaction(null);
  }

  /**
   * Gets a list of values for the given Property.
   * 
   * @param anInstanceURI
   * @param aDatatypePropertyURI
   * @return
   */
  public PropertyValue[] getDatatypePropertyValues(String anInstanceURI, String aDatatypePropertyURI)
  {
    if (!isDatatypeProperty(aDatatypePropertyURI)) {
      throw new GateOntologyException(
          "No datatype property exists with URI :" + aDatatypePropertyURI);
    }

    Resource r2 = getResource(anInstanceURI);
    // TODO: !!!!
    String queryRep21 = "<" + anInstanceURI + ">";
    if (r2 instanceof BNode) {
      queryRep21 = "_:" + anInstanceURI;
    }

    List<PropertyValue> list = new ArrayList<PropertyValue>();
    String query =
        "Select DISTINCT Y from {X} <" + aDatatypePropertyURI + "> {Y} WHERE X=" + queryRep21 + " AND isLiteral(Y)";

    UtilTupleQueryIterator result = performSerqlQuery(query);
    while (result.hasNext()) {
      PropertyValue pv;
      Literal literal = (Literal) result.nextFirstAsValue();
      String datatype = "http://www.w3.org/2001/XMLSchema#string";
      if (literal.getDatatype() != null) {
        datatype = literal.getDatatype().toString();
      }
      pv = new PropertyValue(datatype, literal.getLabel());
      list.add(pv);
    }
    return listToPropertyValueArray(list);
  }

  /**
   * Removes all property values set on the provided instance for the current
   * property.
   * 
   * @param anInstanceURI
   * @param aDatatypePropertyURI
   */
  public void removeDatatypePropertyValues(String anInstanceURI, String aDatatypePropertyURI)
  {
    if (!isDatatypeProperty(aDatatypePropertyURI)) {
      throw new GateOntologyException(
          "No datatype property exists with URI :" + aDatatypePropertyURI);
    }
    startTransaction(null);
    removeUUUStatement(anInstanceURI, aDatatypePropertyURI, null);
    endTransaction(null);
  }

  // ******************
  // Object, Symmetric and Transitive Properties
  // *****************
  /**
   * Adds the value for the given property (Object, Symmetric and Transitive).
   * 
   * @param sourceInstanceURI
   * @param anObjectPropertyURI
   * @param theValueInstanceURI
   */
  public void addObjectPropertyValue(String sourceInstanceURI, String anObjectPropertyURI,
      String theValueInstanceURI) {
    try {
      if (!repositoryConnection.hasStatement(
          getResource(anObjectPropertyURI),
          makeSesameURI(RDF.TYPE.toString()),
          getResource(OWL.OBJECTPROPERTY.toString()),
          true)) {
        throw new GateOntologyException(
            "No object property exists with URI :" + anObjectPropertyURI);
      }
    } catch (Exception e) {
      throw new GateOntologyException("Could not check for statement", e);
    }
    addUUUStatement(sourceInstanceURI, anObjectPropertyURI, theValueInstanceURI);
  }

  /**
   * Remove the provided value for the given property (Object, Symmetric and
   * Transitive).
   * 
   * @param sourceInstanceURI
   * @param anObjectPropertyURI
   * @param theValueInstanceURI
   */
  public void removeObjectPropertyValue(String sourceInstanceURI, String anObjectPropertyURI,
      String theValueInstanceURI)
  {
    if (!isObjectProperty(anObjectPropertyURI)) {
      throw new GateOntologyException(
          "No object property exists with URI :" + anObjectPropertyURI);
    }

    startTransaction(null);
    removeUUUStatement(sourceInstanceURI, anObjectPropertyURI,
        theValueInstanceURI);
    endTransaction(null);
  }

  /**
   * Gets a list of values for the given Property (Object, Symmetric and
   * Transitive).
   * 
   * @param sourceInstanceURI
   * @param anObjectPropertyURI
   * @return
   */
  public String[] getObjectPropertyValues(String sourceInstanceURI, String anObjectPropertyURI)
  {
    if (!isObjectProperty(anObjectPropertyURI) && !isTransitiveProperty(anObjectPropertyURI) && !isSymmetricProperty(anObjectPropertyURI)) {
      throw new GateOntologyException(
          "No object/transitive/symmetric property exists with URI :" + anObjectPropertyURI);
    }

    Resource r = getResource(sourceInstanceURI);
    String queryRep2 = "<" + sourceInstanceURI + ">";
    if (r instanceof BNode) {
      queryRep2 = "_:" + sourceInstanceURI;
    }

    List<String> list = new ArrayList<String>();
    String query =
        "Select DISTINCT Y from {X} <" + anObjectPropertyURI + "> {Y} WHERE X=" + queryRep2;

    addSerqlQueryResultToCollection(query, list);
    return listToArray(list);
  }

  /**
   * Removes all property values set for the current property (Object, Symmetric
   * and Transitive).
   * 
   * @param sourceInstanceURI
   * @param anObjectPropertyURI
   */
  public void removeObjectPropertyValues(String sourceInstanceURI, String anObjectPropertyURI)
  {
    if (!isObjectProperty(anObjectPropertyURI)) {
      throw new GateOntologyException(
          "No object property exists with URI :" + anObjectPropertyURI);
    }
    startTransaction(null);
    removeUUUStatement(sourceInstanceURI, anObjectPropertyURI, null);
    endTransaction(null);
  }


  /**
   * Returns if the given property is a topOnly property.
   * 
   * @param classURI
   * @return
   */
  public boolean isTopProperty(String aPropertyURI)
  {
    return getSuperProperties(aPropertyURI,
        OConstants.Closure.DIRECT_CLOSURE).length == 0;
  }

  //****************************************************************************
  // relations among classes
  //****************************************************************************
  /**
   * The method creates a new class with the URI as specified in className and
   * adds it as a subClassOf the parentClass. It also adds the provided comment
   * on the subClass.
   * 
   * @param superClassURI
   * @param subClassURI
   */
  public void addSubClass(String superClassURI,
      String subClassURI)
  {
    addUUUStatement(subClassURI, RDFS.SUBCLASSOF.toString(), superClassURI);
  }

  /**
   * The method creates a new class with the URI as specified in className and
   * adds it as a superClassOf the parentClass. It also adds the provided
   * comment on the subClass.
   * 
   * @param superClassURI
   * @param subClassURI
   */
  public void addSuperClass(String superClassURI,
      String subClassURI)
  {
    addUUUStatement(subClassURI, RDFS.SUBCLASSOF.toString(), superClassURI);
  }

  /**
   * Removes the subclass relationship
   * 
   * @param superClassURI
   * @param subClassURI
   */
  public void removeSubClass(String superClassURI,
      String subClassURI) {
    removeUUUStatement(subClassURI, RDFS.SUBCLASSOF.toString(), superClassURI);
  }

  /**
   * Removes the superclass relationship
   * 
   * @param superClassURI
   * @param subClassURI
   */
  public void removeSuperClass(String superClassURI,
      String subClassURI) {
    removeUUUStatement(subClassURI, RDFS.SUBCLASSOF.toString(), superClassURI);
  }

  public ClosableIterator<OClass> getSubClassesIterator(
      ONodeID forClass, Closure closure) {
    if(closure== Closure.DIRECT_CLOSURE) {
      // better would be to just do a setbinding for a prepared query,
      // but there is a bug in OWLIM that prevents this to work with this query
      String query = qs_getSubClassesDirectFor;
      query = query.replaceAll("yyy1", forClass.toTurtle());
      UtilTupleQueryIterator qp_getSubClassesDirectFor =
          new UtilTupleQueryIterator(
          repositoryConnection, query, ql_getSubClassesDirectFor);
      return new UtilResourceQueryIterator<OClass>(
          this, qp_getSubClassesDirectFor, OClass.class);
    } else {
      String query = qs_getSubClassesAllFor;
      query = query.replaceAll("yyy1", forClass.toTurtle());
      UtilTupleQueryIterator qp_getSubClassesAllFor =
          new UtilTupleQueryIterator(
          repositoryConnection, query, ql_getSubClassesAllFor);
      return new UtilResourceQueryIterator<OClass>(
          this, qp_getSubClassesAllFor, OClass.class);

    }
  }
  
  public Set<OClass> getSubClasses(ONodeID superClassURI, Closure direct)
  {
    Set<OClass> ret = new HashSet<OClass>();
    ClosableIterator<OClass> it = getSubClassesIterator(superClassURI, direct);
    while(it.hasNext()) {
      ret.add(it.next());
    }
    return ret;
  }


  public OClass getRestrictionForONodeID(ONodeID node) {
    OClass theClass = null;
    if(node.isAnonymousResource()) {
      qp_getRestrictionTypeFor.setBinding("yyy1", new LiteralOrONodeIDImpl(node));
      String nodeID = node.toTurtle();
      LiteralOrONodeID r = null;
      int i = 0;
      while(qp_getRestrictionTypeFor.hasNext()) {
        i++;
        r = qp_getRestrictionTypeFor.nextFirst();
      }
      if(i==0) {
        // no restriction property found, must be some other anonymous class
        theClass = Utils.createOClass(ontology, this, node.toString(), OConstants.ANNONYMOUS_CLASS);
      } else if(i == 1) {
          // make the apropriate restriction 
          theClass = createRestrictionFromURI(r.toString(), nodeID.toString());
      } else {
        // oddd
        System.out.println("getRestrictionForONodeIDs: Got more than one restriction type for: "+nodeID);
        theClass = Utils.createOClass(ontology, this, node.toString(), OConstants.ANNONYMOUS_CLASS);
      }
      return theClass;
    } else {
      throw new GateOntologyException("getRestrictionForONodeIDs: called for non-anonymous class: "+node.toTurtle());
    }
  }



  /**
   * This method returns all super classes of the given class
   * 
   * @param subClassURI
   * @param direct
   * @return
   */
  public ResourceInfo[] getSuperClasses(String subClassURI, Closure direct) throws GateOntologyException {

    String queryRep1 = string2Turtle(subClassURI);
    String queryRep = "{" + queryRep1 + "}";

    String query = "";
    query =
        "Select distinct SUPER FROM " + queryRep + " rdfs:subClassOf {SUPER}" + " WHERE SUPER!=" + queryRep1 + " AND SUPER != ALL ( " + " select distinct B FROM {B} owl:equivalentClass " + queryRep + ")";

    List<String> list = new ArrayList<String>();
    addSerqlQueryResultToCollection(query, list);

    if (direct == OConstants.Closure.DIRECT_CLOSURE) {
      Set<String> toDelete = new HashSet<String>();
      for (int i = 0; i < list.size(); i++) {
        String string = list.get(i);
        if (toDelete.contains(string)) {
          continue;
        }
        queryRep1 = string2Turtle(string);
        queryRep = "{"+queryRep1+"}";

        query =
            "Select distinct SUPER FROM " + queryRep + " rdfs:subClassOf {SUPER}" + " WHERE SUPER!=" + queryRep1 + " AND SUPER != ALL ( " + " select distinct B FROM {B} owl:equivalentClass " + queryRep + ")";

        addSerqlQueryResultToCollection(query, toDelete);
      }
      list.removeAll(toDelete);
    }

    return listToResourceInfoArray(list);
  }

  /**
   * Sets the classes as disjoint
   * 
   * @param class1URI
   * @param class2URI
   */
  public void setDisjointClassWith(String class1URI,
      String class2URI)
  {
    addUUUStatement(class1URI, OWL.DISJOINTWITH.toString(), class2URI);
  }

  /**
   * Sets the classes as same classes
   * 
   * @param class1URI
   * @param class2URI
   */
  public void setEquivalentClassAs(String class1URI,
      String class2URI) {
    addUUUStatement(class1URI, OWL.EQUIVALENTCLASS.toString(), class2URI);
  }

  /**
   * returns an array of classes which are marked as disjoint for the given
   * class
   * 
   * @param classURI
   * @return
   */
  public String[] getDisjointClasses(String aClassURI)
  {
    Resource r1 = getResource(aClassURI);
    // TODO: !!!! turtle
    String queryRep1 = "<" + aClassURI + ">";
    if (r1 instanceof BNode) {
      queryRep1 = "_:" + aClassURI;
    }

    String query =
        "Select distinct B FROM {A}" + " owl:disjointWith {B} WHERE A!=B AND A=" + queryRep1;

    List<String> list = new ArrayList<String>();
    addSerqlQueryResultToCollection(query, list);
    return listToArray(list);
  }

  /**
   * returns an array of classes which are equivalent as the given class
   * 
   * @param aClassURI
   * @return
   */
  public ResourceInfo[] getEquivalentClasses(String aClassURI)
  {
    String queryRep1 = string2Turtle(aClassURI);

    String query =
        "Select distinct B FROM {A}" + " owl:equivalentClass {B} WHERE A!=B AND A=" + queryRep1;

    List<String> list = new ArrayList<String>();
    addSerqlQueryResultToCollection(query, list);
    return listToResourceInfoArray(list);
  }

  /**
   * Removes the given property
   * 
   * @param aPropertyURI
   * @param removeSubTree
   *          - if set to true, removes all its subproperties, otherwise shifts
   *          subproperties to its parent property
   * @return a list of URIs of resources deleted as a result of deleting this
   *         property.
   */
  public String[] removePropertyFromOntology(String aPropertyURI, boolean removeSubTree)
  {
    List<String> deletedResources = new ArrayList<String>();
    // TODO: have to check for explicit property in some other way, this
    // does not work anymore!
    //if(removeUUUStatement(aPropertyURI, RDF.TYPE.toString(), null) == 0) {
    //  throw new GateOntologyException(aPropertyURI
    //    + " is not an explicit Property");
    // }
    // else {
    //   currentEventsLog
    //     .addEvent(new OEvent(aPropertyURI, RDF.TYPE.toString(), null, false));
    //   deletedResources.add(aPropertyURI);
    // }
    removeUUUStatement(aPropertyURI, RDF.TYPE.toString(), null);
    deletedResources.add(aPropertyURI);
    try {
      startTransaction(null);
      // removing all values set for the current property
      repositoryConnection.remove((Resource) null, makeSesameURI(aPropertyURI), null);
      repositoryConnection.remove(getResource(aPropertyURI),
          makeSesameURI(RDFS.SUBPROPERTYOF.toString()), null);
      endTransaction(null);
    } catch (Exception sue) {
      throw new GateOntologyException("error while removing a property:" + aPropertyURI, sue);
    }

    // this should happen only if removeSubTree is set to true
    if (removeSubTree) {
      Property[] subProps =
          getSubProperties(aPropertyURI, OConstants.Closure.DIRECT_CLOSURE);
      for (int i = 0; i < subProps.length; i++) {
        try {
          if (repositoryConnection.hasStatement(getResource(subProps[i].getUri()),
              makeSesameURI(RDF.TYPE.toString()), null, false)) {
            continue;
          }
          String[] removedResources =
              removePropertyFromOntology(subProps[i].getUri(), true);
          deletedResources.addAll(Arrays.asList(removedResources));
        } catch (RepositoryException e) {
          throw new GateOntologyException("Error finding statement ", e);
        }
      }
    }
    removeUUUStatement(aPropertyURI, null, null);
    removeUUUStatement(null, aPropertyURI, null);
    removeUUUStatement(null, null, aPropertyURI);
    return listToArray(deletedResources);
  }

  /**
   * The method adds an object property specifiying domain and range for the
   * same. All classes specified in domain and range must exist.
   * 
   * @param aPropertyURI
   * @param domainClassesURIs
   * @param rangeClassesTypes
   */
  public void addObjectProperty(String aPropertyURI,
      String[] domainClassesURIs, String[] rangeClassesTypes)
  {
    addUUUStatement(aPropertyURI, RDF.TYPE.toString(), OWL.OBJECTPROPERTY.toString());
    if (domainClassesURIs != null) {
      for (int i = 0; i < domainClassesURIs.length; i++) {
        addUUUStatement(aPropertyURI, RDFS.DOMAIN.toString(), domainClassesURIs[i]);
      }
    }
    if (rangeClassesTypes != null) {
      for (int i = 0; i < rangeClassesTypes.length; i++) {
        addUUUStatement(aPropertyURI, RDFS.RANGE.toString(), rangeClassesTypes[i]);
      }
    }
  }

  /**
   * The method adds a transitive property specifiying domain and range for the
   * same. All classes specified in domain and range must exist.
   * 
   * @param aPropertyURI
   * @param domainClassesURIs
   * @param rangeClassesTypes
   */
  public void addTransitiveProperty(String aPropertyURI,
      String[] domainClassesURIs, String[] rangeClassesTypes)
  {
    addUUUStatement(aPropertyURI, RDF.TYPE.toString(), OWL.TRANSITIVEPROPERTY.toString());
    if (domainClassesURIs != null) {
      for (int i = 0; i < domainClassesURIs.length; i++) {
        addUUUStatement(aPropertyURI, RDFS.DOMAIN.toString(), domainClassesURIs[i]);
      }
    }
    if (rangeClassesTypes != null) {
      for (int i = 0; i < rangeClassesTypes.length; i++) {
        addUUUStatement(aPropertyURI, RDFS.RANGE.toString(), rangeClassesTypes[i]);
      }
    }
  }

  /**
   * The method returns an array of properties. Property is a complex structure,
   * which contains name, comment, information about its domain and range.
   * 
   * @return
   */
  public Property[] getRDFProperties()
  {
    List<Property> list = new ArrayList<Property>();
    String query =
        "Select distinct X FROM {X} rdf:type {<" + RDF.PROPERTY + ">}";

    UtilTupleQueryIterator result = performSerqlQuery(query);
    while (result.hasNext()) {
      String propString = result.nextFirstAsString();
      if (isAnnotationProperty(propString) || isDatatypeProperty(propString) || isObjectProperty(propString) || isTransitiveProperty(propString) || isSymmetricProperty(propString)) {
        continue;
      }
      list.add(new Property(OConstants.RDF_PROPERTY, propString));
    }
    result.close();
    return listToPropertyArray(list);
  }

  /**
   * The method returns an array of properties. Property is a complex structure,
   * which contains name, comment, information about its domain and range.
   * 
   * @return
   */
  public Property[] getObjectProperties()
  {
    List<Property> list = new ArrayList<Property>();
    String query =
        "Select distinct X FROM {X} rdf:type {<" + OWL.OBJECTPROPERTY + ">}";

    logger.debug("=== searching for object properties");
    UtilTupleQueryIterator result = performSerqlQuery(query);
    while (result.hasNext()) {
      Value anAnnProp = result.nextFirstAsValue();
      logger.debug("Got an object property: "+anAnnProp);
      list.add(new Property(OConstants.OBJECT_PROPERTY, anAnnProp.toString()));
    }
    result.close();

    query =
        "Select distinct X FROM {X} rdf:type {<" + OWL.SYMMETRICPROPERTY + ">}";

    logger.debug("=== searching for symmetric properties");
    result = performSerqlQuery(query);
    while (result.hasNext()) {
      Value anAnnProp = result.nextFirstAsValue();
      logger.debug("Got a symmetric property: "+anAnnProp);
      list.add(new Property(OConstants.SYMMETRIC_PROPERTY, anAnnProp.toString()));
    }

    query =
        "Select distinct X FROM {X} rdf:type {<" + OWL.TRANSITIVEPROPERTY + ">}";

    result = performSerqlQuery(query);
    while (result.hasNext()) {
      Value anAnnProp = result.nextFirstAsValue();
      logger.debug("Got a transitive property: "+anAnnProp);
      list.add(new Property(OConstants.TRANSITIVE_PROPERTY, anAnnProp.toString()));
    }
    logger.debug("Returning from  OSI getObjectProperties");
    return listToPropertyArray(list);
  }

  /**
   * The method returns an array of properties. Property is a complex structure,
   * which contains name, comment, information about its domain and range.
   * 
   * @return
   */
  public Property[] getSymmetricProperties()
  {
    List<Property> list = new ArrayList<Property>();
    String query =
        "Select distinct X FROM {X} rdf:type {<" + OWL.SYMMETRICPROPERTY + ">}";

    UtilTupleQueryIterator result = performSerqlQuery(query);
    while (result.hasNext()) {
      Value anAnnProp = result.nextFirstAsValue();
      list.add(new Property(OConstants.SYMMETRIC_PROPERTY, anAnnProp.toString()));
    }
    return listToPropertyArray(list);
  }

  /**
   * The method returns an array of properties. Property is a complex structure,
   * which contains name, comment, information about its domain and range.
   * 
   * @return
   */
  public Property[] getTransitiveProperties()
  {
    List<Property> list = new ArrayList<Property>();
    String query =
        "Select distinct X FROM {X} rdf:type {<" + OWL.TRANSITIVEPROPERTY + ">}";

    UtilTupleQueryIterator result = performSerqlQuery(query);
    while (result.hasNext()) {
      Value anAnnProp = result.nextFirstAsValue();
      list.add(new Property(OConstants.TRANSITIVE_PROPERTY, anAnnProp.toString()));
    }
    return listToPropertyArray(list);
  }

  /**
   * The method returns an array of properties. Property is a complex structure,
   * which contains name, comment, information about its domain and range.
   * 
   * @return
   */
  public Property[] getDatatypeProperties()
  {
    List<Property> list = new ArrayList<Property>();
    String query =
        "Select distinct X FROM {X} rdf:type {<" + OWL.DATATYPEPROPERTY + ">}";

    UtilTupleQueryIterator result = performSerqlQuery(query);
    while (result.hasNext()) {
      Value anAnnProp = result.nextFirstAsValue();
      list.add(new Property(OConstants.DATATYPE_PROPERTY, anAnnProp.toString()));
    }
    return listToPropertyArray(list);
  }

  /**
   * The method returns an array of properties. Property is a complex structure,
   * which contains name, comment, information about its domain and range.
   * 
   * @return
   */
  public Property[] getAnnotationProperties()
  {
    List<Property> list = new ArrayList<Property>();
    String query =
        "Select distinct X FROM {X} rdf:type {<" + OWL.ANNOTATIONPROPERTY + ">}";

    UtilTupleQueryIterator result = performSerqlQuery(query);
    while (result.hasNext()) {
      Value anAnnProp = result.nextFirstAsValue();
      list.add(new Property(OConstants.ANNOTATION_PROPERTY, anAnnProp.toString()));
    }

    boolean allowSystemStatements = this.returnSystemStatements;
    this.returnSystemStatements = true;
    Property[] props = listToPropertyArray(list);
    this.returnSystemStatements = allowSystemStatements;
    return props;
  }

  public Set<RDFProperty> getPropertiesByName(String name) {
    Set<RDFProperty> properties = new HashSet<RDFProperty>();
    // TODOD: !!!!!!
    // TODO: get all rdf properties, object properties, symmetric properties
    // etc where the URI fragment identifier matches the name
    return properties;
  }

  /**
   * Given a property, this method returns its domain
   * 
   * @param aPropertyURI
   * @return
   */
  // TODO: return ONodeIDs or Classes, make reducing to most
  // specific classes optional
  public ResourceInfo[] getDomain(String aPropertyURI)
  {
    if (isAnnotationProperty(aPropertyURI)) {
      throw new GateOntologyException(
          "AnnotationProperties do no specify any domain or range");
    }

    String query =
        "select distinct Y from {<" + aPropertyURI + ">} rdfs:domain {Y}";
    UtilTupleQueryIterator result = performSerqlQuery(query);
    List<ResourceInfo> list = new ArrayList<ResourceInfo>();
    while (result.hasNext()) {
      String classString = result.nextFirstAsString();
      byte classType = getClassType(classString);
      if (classType == OConstants.ANNONYMOUS_CLASS) {
        continue;
      }
      list.add(new ResourceInfo(classString, classType));
    }
    result.close();
    return reduceToMostSpecificClasses(list);
  }

  /**
   * Given a property, this method returns its range
   * 
   * @param aPropertyURI
   * @return
   */
  public ResourceInfo[] getRange(String aPropertyURI)
  {
    if (isAnnotationProperty(aPropertyURI)) {
      throw new GateOntologyException(
          "AnnotationProperties do no specify any domain or range");
    }
    if (isDatatypeProperty(aPropertyURI)) {
      throw new GateOntologyException(
          "Please use getDatatype(String theDatatypeProerptyURI) method instead");
    }

    String query =
        "Select distinct Y from {<" + aPropertyURI + ">} rdfs:range {Y}";
    UtilTupleQueryIterator result = performSerqlQuery(query);
    List<ResourceInfo> list = new ArrayList<ResourceInfo>();
    while (result.hasNext()) {
      String classString = result.nextFirstAsString();
      byte classType = getClassType(classString);
      if (classType == OConstants.ANNONYMOUS_CLASS) {
        continue;
      }
      list.add(new ResourceInfo(classString, classType));
    }
    result.close();
    return reduceToMostSpecificClasses(list);
  }

  /**
   * Returns if the provided property is functional
   * 
   * @param aPropertyURI
   * @return
   */
  public boolean isFunctional(String aPropertyURI)
  {
    String query =
        "Select * FROM {X} rdf:type {<" + OWL.FUNCTIONALPROPERTY + ">} WHERE X=<" + aPropertyURI + ">";
    return hasSerqlQueryResultRows(query);
  }

  /**
   * sets the current property as functional
   * 
   * @param aPropertyURI
   * @param isFunctional
   */
  public void setFunctional(String aPropertyURI,
      boolean isFunctional)
  {
    if (isFunctional) {
      addUUUStatement(aPropertyURI, RDF.TYPE.toString(), OWL.FUNCTIONALPROPERTY.toString());
    } else {
      removeUUUStatement(aPropertyURI, RDF.TYPE.toString(), OWL.FUNCTIONALPROPERTY.toString());
    }
  }

  /**
   * returns if the given property is inverse functional property
   * 
   * @param aPropertyURI
   * @return
   */
  public boolean isInverseFunctional(String aPropertyURI)
  {
    String query =
        "Select * FROM {X} rdf:type {<" + OWL.INVERSEFUNCTIONALPROPERTY + ">} WHERE X=<" + aPropertyURI + ">";
    return hasSerqlQueryResultRows(query);
  }

  /**
   * Sets the current property as inverse functional property
   * 
   * @param aPropertyURI
   * @param isInverseFunctional
   */
  public void setInverseFunctional(String aPropertyURI,
      boolean isInverseFunctional)
  {
    if (isInverseFunctional) {
      addUUUStatement(aPropertyURI, RDF.TYPE.toString(), OWL.INVERSEFUNCTIONALPROPERTY.toString());
    } else {
      removeUUUStatement(aPropertyURI, RDF.TYPE.toString(), OWL.INVERSEFUNCTIONALPROPERTY.toString());
    }
  }

  /**
   * returns if the given property is a symmetric property
   * 
   * @param aPropertyURI
   * @return
   */
  public boolean isSymmetricProperty(String aPropertyURI)
  {
    String query =
        "Select * FROM {X} rdf:type {<" + OWL.SYMMETRICPROPERTY + ">} WHERE X=<" + aPropertyURI + ">";
    return hasSerqlQueryResultRows(query);
  }

  /**
   * returns if the given property is a transitive property
   * 
   * @param aPropertyURI
   * @return
   */
  public boolean isTransitiveProperty(String aPropertyURI)
  {
    String query =
        "Select * FROM {X} rdf:type {<" + OWL.TRANSITIVEPROPERTY + ">} WHERE X=<" + aPropertyURI + ">";
    return hasSerqlQueryResultRows(query);
  }

  /**
   * returns if the given property is a datatype property
   * 
   * @param aPropertyURI
   * @return
   */
  public boolean isDatatypeProperty(String aPropertyURI)
  {
    String query =
        "Select * FROM {X} rdf:type {<" + OWL.DATATYPEPROPERTY + ">} WHERE X=<" + aPropertyURI + ">";
    return hasSerqlQueryResultRows(query);
  }

  /**
   * returns if the given property is an object property
   * 
   * @param aPropertyURI
   * @return
   */
  public boolean isObjectProperty(String aPropertyURI)
  {
    String query =
        "Select * FROM {X} rdf:type {<" + OWL.OBJECTPROPERTY + ">} WHERE X=<" + aPropertyURI + ">";
    return hasSerqlQueryResultRows(query);
  }

  // *************************************
  // Relations among properties
  // *************************************
  /**
   * Sets two properties as same
   * 
   * @param property1URI
   * @param property2URI
   */
  public void setEquivalentPropertyAs(String property1URI,
      String property2URI)
  {
    addUUUStatement(property1URI, OWL.EQUIVALENTPROPERTY.toString(), property2URI);
  }

  /**
   * For the given property, this method returns all properties marked as
   * Equivalent as it
   * 
   * @param aPropertyURI
   * @return
   */
  public Property[] getEquivalentPropertyAs(String aPropertyURI)
  {
    String query =
        "Select DISTINCT Y FROM {X} owl:equivalentProperty {Y} WHERE X=<" + aPropertyURI + ">";
    UtilTupleQueryIterator result = performSerqlQuery(query);
    List<Property> list = new ArrayList<Property>();
    while (result.hasNext()) {
      list.add(createPropertyObject(result.nextFirstAsString()));
    }
    return listToPropertyArray(list);
  }

  /**
   * For the given properties, this method registers the super, sub relation
   * 
   * @param superPropertyURI
   * @param subPropertyURI
   */
  public void addSuperProperty(String superPropertyURI,
      String subPropertyURI)
  {
    addUUUStatement(subPropertyURI, RDFS.SUBPROPERTYOF.toString(), superPropertyURI);
  }

  /**
   * For the given properties, this method removes the super, sub relation
   * 
   * @param superPropertyURI
   * @param subPropertyURI
   */
  public void removeSuperProperty(String superPropertyURI,
      String subPropertyURI)
  {
    removeUUUStatement(subPropertyURI, RDFS.SUBPROPERTYOF.toString(), superPropertyURI);
  }

  /**
   * For the given properties, this method registers the super, sub relation
   * 
   * @param superPropertyURI
   * @param subPropertyURI
   */
  public void addSubProperty(String superPropertyURI,
      String subPropertyURI)
  {
    addUUUStatement(subPropertyURI, RDFS.SUBPROPERTYOF.toString(), superPropertyURI);
  }

  /**
   * For the given properties, this method removes the super, sub relation
   * 
   * @param superPropertyURI
   * @param subPropertyURI
   */
  public void removeSubProperty(String superPropertyURI,
      String subPropertyURI)
  {
    removeUUUStatement(subPropertyURI, RDFS.SUBPROPERTYOF.toString(), superPropertyURI);
  }

  /**
   * for the given property, the method returns all its super properties
   * 
   * @param aPropertyURI
   * @param direct
   * @return
   */
  public Property[] getSuperProperties(String aPropertyURI, boolean direct)
  {
    return this.getSuperProperties(aPropertyURI, direct
        ? OConstants.Closure.DIRECT_CLOSURE
        : OConstants.Closure.TRANSITIVE_CLOSURE);
  }

  /**
   * for the given property, the method returns all its sub properties
   * 
   * @param aPropertyURI
   * @param direct
   * @return
   */
  public Property[] getSubProperties(String aPropertyURI,
      boolean direct)
  {
    return this.getSubProperties(aPropertyURI, direct
        ? OConstants.Closure.DIRECT_CLOSURE
        : OConstants.Closure.TRANSITIVE_CLOSURE);
  }

  /**
   * for the given property, the method returns all its inverse properties
   * 
   * @param aPropertyURI
   * @return
   */
  public Property[] getInverseProperties(String aPropertyURI)
  {
    String query =
        "Select DISTINCT Y FROM {X} owl:inverseOf {Y} WHERE X=<" + aPropertyURI + ">";
    UtilTupleQueryIterator result = performSerqlQuery(query);
    List<Property> list = new ArrayList<Property>();
    while (result.hasNext()) {
      list.add(createPropertyObject(result.nextFirstAsString()));
    }
    result.close();
    return listToPropertyArray(list);
  }

  /**
   * property1 is set as inverse of property 2
   * 
   * @param propertyURI1
   * @param propertyURI2
   */
  public void setInverseOf(String propertyURI1, String propertyURI2)
  {
    addUUUStatement(propertyURI1, OWL.INVERSEOF.toString(), propertyURI2);
  }

  // *******************************************************************
  // *************************** Instance Methods **********************
  // *******************************************************************
  /**
   * The method adds a new instance (literal) into the repository. It then
   * creates a statement indicating membership relation with the provided class.
   * 
   * @param superClassURI
   * @param individualURI
   */
  public void addIndividual(String superClassURI,
      String individualURI)
  {
    addUUUStatement(individualURI, RDF.TYPE.toString(), superClassURI);
  }

  /**
   * The method removes the provided instance from the repository.
   * 
   * @param individual
   * @return
   */
  public String[] removeIndividual(String individualURI)
  {
    // TODO: JP we have to check for explicit individual in some other way
    // here since removeUUUStatement cannot return the number of removed
    // eny more
    // Do a query to assert this?

    //int no = removeUUUStatement(individualURI, RDF.TYPE.toString(), null);
    //if(no == 0)
    //  throw new GateOntologyException(individualURI
    //    + " is not an explicit Individual");
    removeUUUStatement(individualURI, RDF.TYPE.toString(), null);


    // we need to go though all ontology resources of the ontology
    // check if they have property with value the current resource
    // we need to delete it

    // TODO: !!!! is this not redundand with respect to what we do below?
    // (which is remove any triples that contain this URI anywhere?)
    // NOTE that removing all triples with this URI will fail for OWL-DL
    // if we just want to remove the occurrences of this individual as
    // individual but it is also a class!
    List<Property> properties = new ArrayList<Property>();
    properties.addAll(Arrays.asList(getObjectProperties()));
    try {
      startTransaction(null);
      for (int i = 0; i < properties.size(); i++) {
        repositoryConnection.remove((Resource) null, makeSesameURI(properties.get(i).getUri()),
            getResource(individualURI));
      }
      endTransaction(null);
    } catch (Exception sue) {
      throw new GateOntologyException("error while removing individual:" + individualURI, sue);
    }
    removeUUUStatement(individualURI, null, null);
    removeUUUStatement(null, null, individualURI);
    removeUUUStatement(null, individualURI, null);
    return new String[]{individualURI};
  }

  /**
   * The method returns all member instances of the provided class. It returns
   * only the direct instances if the boolean parameter direct is set to true.
   * 
   * @param superClassURI
   * @param direct
   */
  public String[] getIndividuals(String superClassURI,
      Closure direct) {
    String queryRep = string2Turtle(superClassURI);

    // A -> B -> I1

    String query = "";
    if (direct == OConstants.Closure.DIRECT_CLOSURE) {
      query = "Select distinct X from {X} serql:directType {" + queryRep +"}";
    } else {
      query = "Select distinct X from {X} rdf:type {" + queryRep +"}";
    }
    //System.out.println("getIndividuals query: "+query);
    List<String> list = new ArrayList<String>();
    addSerqlQueryResultToCollection(query, list);
    return listToArray(list);
  }


  public ClosableIterator<OInstance> getInstancesIterator(
      ONodeID aClass, OConstants.Closure closure) {
    // if aClass is null: all instances
    // if aClass is not null: instances of that class either 
    //   direct or transitive
    // once we have that, remove  unneeded classes and re-implement
    // getOInstance and has OInstance in abstractOntologyimpl
    UtilResourceQueryIterator ret = null;
    if(aClass == null) {
       ret = new UtilResourceQueryIterator<OInstance>(
        this, qp_getInstancesAll, OInstance.class);
    } else if(closure == OConstants.Closure.DIRECT_CLOSURE) {
        ret = new UtilResourceQueryIterator<OInstance>(
        this, qp_getInstancesDirectFor, OInstance.class);
        ret.setBinding("yyy1", new LiteralOrONodeIDImpl(aClass));

    } else {
       ret = new UtilResourceQueryIterator<OInstance>(
        this, qp_getInstancesAllFor, OInstance.class);
        ret.setBinding("yyy1", new LiteralOrONodeIDImpl(aClass));
   }
    return ret;
  }

  public Set<OInstance> getInstances(ONodeID aClass, OConstants.Closure closure) {
    //System.out.println("Running service.getInstances()");
    Set<OInstance> theInstances = new HashSet<OInstance>();
    ClosableIterator<OInstance> ii = getInstancesIterator(aClass, closure);
    while(ii.hasNext()) {
      OInstance i = ii.next();
      //System.out.println("Adding to result: "+i);
      theInstances.add(i);
    }
    return theInstances;
  }


 // public ClosableIterator<OInstance> getInstancesIterator() {
 //   //System.out.print("Creating ResourceQueryIt fir "+qp_getInstancesAll);
 //   return new UtilResourceQueryIterator<OInstance>(
 //       this, qp_getInstancesAll, OInstance.class);
 // }





  public Set<OInstance> getInstancesByName(String name) {
    Set<OInstance> instances = new HashSet<OInstance>();
    String query =
        "Select distinct X from {X} rdf:type {Y} rdf:type {<http://www.w3.org/2002/07/owl#Class>} WHERE Y != <http://www.w3.org/2002/07/owl#Thing> AND X LIKE yyy1 ";
    query = query.replaceAll("yyy1", "\"*"+name+"\"");
    //System.out.println("Query for instances: "+query);
    UtilTupleQueryIterator q = new UtilTupleQueryIterator(repositoryConnection,
        query,OConstants.QueryLanguage.SERQL);
    while(q.hasNext()) {
      Value v = q.nextFirstAsValue();
      OInstance i = Utils.createOInstance(ontology, this, v.toString());
      instances.add(i);
    }
    return instances;
  }


  // OURI must be not null, ONodeID can be null in that cas closure can be null too
  public boolean hasInstance(OURI theURI, ONodeID theClass, OConstants.Closure closure) {
    boolean ret = false;
    if(theClass == null) {
      qp_hasInstance.setBinding("yyy1", new LiteralOrONodeIDImpl(theURI));
      if(qp_hasInstance.hasNext()) {
        ret = true;
        qp_hasInstance.close();
      }
    } else if(closure == OConstants.Closure.DIRECT_CLOSURE) {
//      qp_hasInstanceDirectFor.setBinding("yyy1", new LiteralOrONodeIDImpl(theURI));
//      qp_hasInstanceDirectFor.setBinding("yyy2", new LiteralOrONodeIDImpl(theClass));
      String query = qs_hasInstanceDirectFor;
      query = query.replaceAll("yyy1", theURI.toTurtle());
      query = query.replaceAll("yyy2", theClass.toTurtle());
      UtilTupleQueryIterator qp_hasInstanceDirectFor =
          new UtilTupleQueryIterator(repositoryConnection, query, ql_hasInstanceDirectFor);
      if(qp_hasInstanceDirectFor.hasNext()) {
        ret = true;
        qp_hasInstanceDirectFor.close();
      }
    } else {
//      qp_hasInstanceAllFor.setBinding("yyy1", new LiteralOrONodeIDImpl(theURI));
//      qp_hasInstanceAllFor.setBinding("yyy2", new LiteralOrONodeIDImpl(theClass));
      String query = qs_hasInstanceAllFor;
      query = query.replaceAll("yyy1", theURI.toTurtle());
      query = query.replaceAll("yyy2", theClass.toTurtle());
      UtilTupleQueryIterator qp_hasInstanceAllFor =
          new UtilTupleQueryIterator(repositoryConnection, query, ql_hasInstanceAllFor);
      if(qp_hasInstanceAllFor.hasNext()) {
        ret = true;
        qp_hasInstanceAllFor.close();
      }
    }
    return ret;
  }


  /**
   * For the given individual, the method returns a set of classes for which the
   * individual is registered as instance of
   * 
   * @param individualURI
   */
  public ResourceInfo[] getClassesOfIndividual(String individualURI, Closure direct) throws GateOntologyException {
    if (debug) {
      logger.debug("getClassesOfIndividual");
    }
    String query = "";
    if (direct == OConstants.Closure.DIRECT_CLOSURE) {
      query =
          "Select DISTINCT B from {X} serql:directType {B} WHERE X=<" + individualURI + ">";
    } else {
      query =
          "Select DISTINCT B from {X} rdf:type {B} WHERE X=<" + individualURI + ">";
    }

    List<String> list = new ArrayList<String>();
    addSerqlQueryResultToCollection(query, list);
    return listToResourceInfoArray(list);
  }

  // *******************************************************************
  // relations among individuals
  // *******************************************************************
  /**
   * individual1 is sets as different individual from individual2
   * 
   * @param individual1URI
   * @param individual2URI
   */
  public void setDifferentIndividualFrom(String individual1URI, String individual2URI) throws GateOntologyException {
    if (debug) {
      logger.debug("setDifferentIndividualFrom");
    }
    addUUUStatement(individual1URI, OWL.DIFFERENTFROM.toString(), individual2URI);
  }

  /**
   * for the given individual, the method returns all individuals registered as
   * different from the given individual
   * 
   * @param individualURI
   * @return
   */
  public String[] getDifferentIndividualFrom(String individualURI) throws GateOntologyException {
    String query =
        "Select distinct B from {X} owl:differentFrom {B} WHERE X=<" + individualURI + ">";
    List<String> list = new ArrayList<String>();
    addSerqlQueryResultToCollection(query, list);
    return listToArray(list);
  }

  /**
   * individual1 is set as same as the individual2
   * 
   * @param individual1URI
   * @param individual2URI
   */
  public void setSameIndividualAs(String individual1URI,
      String individual2URI) throws GateOntologyException {
    if (debug) {
      logger.debug("setSameIndividualAs");
    }
    addUUUStatement(individual1URI, OWL.SAMEAS.toString(), individual2URI);
  }

  /**
   * for the given individual, the method returns all individuals which are
   * registered as same as the provided individual
   * 
   * @param inidividualURI
   * @return
   */
  public String[] getSameIndividualAs(String individualURI)
  {
    String query =
        "select distinct B from {X} owl:sameAs {B} WHERE X=<" + individualURI + "> AND X!=B";
    List<String> list = new ArrayList<String>();
    addSerqlQueryResultToCollection(query, list);
    return listToArray(list);
  }

  // ***********************************************
  // ********* Restrictions ***********************
  // ***********************************************
  /**
   * This method given a restriction uri returns the value for the onProperty
   * element.
   * 
   * @param restrictionURI
   * @return
   * @throws GateOntologyException
   */
  public Property getOnPropertyValue(String restrictionURI)
  {
    String queryRep = string2Turtle(restrictionURI);

    String query =
        "Select distinct B from {X} owl:onProperty {B} WHERE X=" + queryRep;
    //System.out.println("Query="+query);
    UtilTupleQueryIterator result = performSerqlQuery(query);
    String val = null;
    if (result.hasNext()) {
      // here we need to check which type of property it is
      val = result.nextFirstAsString();
    }
    result.close();
    if (val != null) {
      return createPropertyObject(val);
    } else {
      return null;
    }
  }

  /**
   * This method sets the value for onProperty element on the given restriction.
   * 
   * @param repositoryId
   * @param restrictionURI
   * @param propertyURI
   * @throws GateOntologyException
   */
  public void setOnPropertyValue(String restrictionURI,
      String propertyURI)
  {
    addUUUStatement(restrictionURI, OWL.ONPROPERTY.toString(), propertyURI);
  }

  /**
   * Gets the datatype uri specified on the given restriction uri.
   * 
   * @param repositoryID
   * @param restrictionURI
   * @return
   * @throws GateOntologyException
   */
  public PropertyValue getPropertyValue(
      String restrictionURI, byte restrictionType)
  {
    org.openrdf.model.URI whatValueURI = null;
    switch (restrictionType) {
      case OConstants.CARDINALITY_RESTRICTION:
        whatValueURI = OWL.CARDINALITY;
        break;
      case OConstants.MAX_CARDINALITY_RESTRICTION:
        whatValueURI = OWL.MAXCARDINALITY;
        break;
      case OConstants.MIN_CARDINALITY_RESTRICTION:
        whatValueURI = OWL.MINCARDINALITY;
        break;
      default:
        throw new GateOntologyException("Invalid restriction type :" + restrictionType + " for the " + restrictionURI);
    }

    try {
      RepositoryResult<Statement> iter =
          repositoryConnection.getStatements(getResource(restrictionURI), whatValueURI,
          null, true);
      if (iter.hasNext()) {
        Value v = iter.next().getObject();
        if (v instanceof Literal) {
          return new PropertyValue(((Literal) v).getDatatype().toString(), ((Literal) v).getLabel());
        }
      } else {
        throw new GateOntologyException("Could not find restriction value for "+restrictionURI+"/"+whatValueURI);
      }
    } catch (Exception e) {
      throw new GateOntologyException(e);
    }
    return null;
  }

  /**
   * Sets the datatype uri for the given restriction uri.
   * 
   * @param restrictionURI
   * @param datatypeURI
   */
  public void setPropertyValue(String restrictionURI,
      byte restrictionType, String value, String datatypeURI)
 {
    String whatValueURI = null;
    switch (restrictionType) {
      case OConstants.CARDINALITY_RESTRICTION:
        whatValueURI = OWL.CARDINALITY.toString();
        break;
      case OConstants.MAX_CARDINALITY_RESTRICTION:
        whatValueURI = OWL.MAXCARDINALITY.toString();
        break;
      case OConstants.MIN_CARDINALITY_RESTRICTION:
        whatValueURI = OWL.MINCARDINALITY.toString();
        break;
      default:
        throw new GateOntologyException("Invalid restriction type :" + restrictionType + " for the restriction " + restrictionURI);
    }
    Statement toDelete = null;
    try {
      RepositoryResult<Statement> iter =
          repositoryConnection.getStatements(getResource(restrictionURI), makeSesameURI(whatValueURI),
          null, true);
      if (iter.hasNext()) {
        Statement stmt = iter.next();
        Value v = stmt.getObject();
        if (v instanceof Literal) {
          if (((Literal) v).getDatatype().toString().intern() == datatypeURI.intern()) {
            toDelete = stmt;
          }
        }
      }
    } catch (Exception e) {
      throw new GateOntologyException(e);
    }

    if (toDelete != null) {
      Literal l = (Literal) toDelete.getObject();
      removeUUUStatement(whatValueURI, l.getLabel(), l.getDatatype().toString());
    }
    addUUDStatement(restrictionURI, whatValueURI, value,
        datatypeURI);
  }

  /**
   * Gets the cardinality value specified on the given restriction uri.
   * 
   * @param restrictionURI
   * @param restrictionType
   *          - either of the following constants from the OConstants -
   *          ALL_VALUES_FROM_RESTRICTION, SOME_VALUES_FROM_RESTRICTION, and
   *          HAS_VALUE_RESTRICTION
   * @return
   */
  public ResourceInfo getRestrictionValue(
      String restrictionURI, byte restrictionType)
  {
    System.out.println("getRestrictionValue for "+restrictionURI);
    URI whatValueURI = null;
    switch (restrictionType) {
      case OConstants.ALL_VALUES_FROM_RESTRICTION:
        whatValueURI = OWL.ALLVALUESFROM;
        break;
      case OConstants.HAS_VALUE_RESTRICTION:
        whatValueURI = OWL.HASVALUE;
        break;
      case OConstants.SOME_VALUES_FROM_RESTRICTION:
        whatValueURI = OWL.SOMEVALUESFROM;
        break;
      default:
        throw new GateOntologyException("Invalid restriction type:" + restrictionType + " for the restriction " + restrictionURI);
    }
    String resourceURI;
    try {
      RepositoryResult<Statement> iter =
          repositoryConnection.getStatements(string2SesameResource(restrictionURI), whatValueURI,
          null, true);
      if (iter.hasNext()) {
        resourceURI = iter.next().getObject().toString();
        Resource res = getResource(resourceURI);
        boolean isRestriction =
            repositoryConnection.hasStatement(
            res,
            RDF.TYPE,
            OWL.RESTRICTION,
            true);
        byte classType = OConstants.OWL_CLASS;
        if (isRestriction) {
          if (repositoryConnection.hasStatement(res, OWL.HASVALUE, null, true)) {
            classType = OConstants.HAS_VALUE_RESTRICTION;
          } else if (repositoryConnection.hasStatement(res, OWL.SOMEVALUESFROM, null, true)) {
            classType = OConstants.SOME_VALUES_FROM_RESTRICTION;
          } else if (repositoryConnection.hasStatement(res, OWL.ALLVALUESFROM, null, true)) {
            classType = OConstants.ALL_VALUES_FROM_RESTRICTION;
          } else if (repositoryConnection.hasStatement(res, OWL.CARDINALITY, null, true)) {
            classType = OConstants.CARDINALITY_RESTRICTION;
          } else if (repositoryConnection.hasStatement(res, OWL.MINCARDINALITY, null, true)) {
            classType = OConstants.MIN_CARDINALITY_RESTRICTION;
          } else if (repositoryConnection.hasStatement(res, OWL.MAXCARDINALITY, null, true)) {
            classType = OConstants.MAX_CARDINALITY_RESTRICTION;
          }
        }

        if (classType == OConstants.OWL_CLASS) {
          if (res instanceof BNode) {
            classType = OConstants.ANNONYMOUS_CLASS;
          } else {
            // check if it is an instance
            if (isIndividual(resourceURI)) {
              classType = OConstants.INSTANCE;
            }
          }
        }

        return new ResourceInfo(resourceURI, classType);
      }
    } catch (Exception e) {
      throw new GateOntologyException("Problem getting restriction value for "+restrictionURI,e);
    }
    return null;
  }

  /**
   * tells if the given URI is registered as an individual
   * @param individualURI
   * @return
   */
  public boolean isIndividual(String individualURI)
  {

    String query = "Select X from {<" + individualURI + ">} rdf:type {X} rdf:type {<http://www.w3.org/2002/07/owl#Class>}";
    return hasSerqlQueryResultRows(query);

  }

  /**
   * Sets the cardinality value for the given restriction uri.
   * 
   * @param restrictionURI
   * @param restrictionType
   *          - either of the following constants from the OConstants -
   *          ALL_VALUES_FROM_RESTRICTION, SOME_VALUES_FROM_RESTRICTION, and
   *          HAS_VALUE_RESTRICTION
   * @param value
   * @return
   */
  public void setRestrictionValue(String restrictionURI,
      byte restrictionType, String value)
  {
    String whatValueURI = null;
    switch (restrictionType) {
      case OConstants.ALL_VALUES_FROM_RESTRICTION:
        whatValueURI = OWL.ALLVALUESFROM.toString();
        break;
      case OConstants.HAS_VALUE_RESTRICTION:
        whatValueURI = OWL.HASVALUE.toString();
        break;
      case OConstants.SOME_VALUES_FROM_RESTRICTION:
        whatValueURI = OWL.SOMEVALUESFROM.toString();
        break;
      default:
        throw new GateOntologyException("Invalid restriction type:" + restrictionType + " for the restriction " + restrictionURI);

    }
    Statement toDelete = null;
    try {
      RepositoryResult<Statement> iter =
          repositoryConnection.getStatements(getResource(restrictionURI), makeSesameURI(whatValueURI),
          null, true);
      if (iter.hasNext()) {
        Statement stmt = iter.next();
        Value v = stmt.getObject();
        toDelete = stmt;
      }
    } catch (Exception e) {
      throw new GateOntologyException(e);
    }

    if (toDelete != null) {
      String objectString = toDelete.getObject().toString();
      removeUUUStatement(restrictionURI, whatValueURI, objectString);
    }
    addUUUStatement(restrictionURI, whatValueURI, value);
  }

  /**
   * This method tells what type of restriction the given uri refers to. If the
   * given URI is not a restriction, the method returns -1. Otherwise one of the
   * following values from the OConstants class. OWL_CLASS,
   * CARDINALITY_RESTRICTION, MIN_CARDINALITY_RESTRICTION,
   * MAX_CARDINALITY_RESTRICTION, HAS_VALUE_RESTRICTION,
   * ALL_VALUES_FROM_RESTRICTION.
   * 
   * @param restrictionURI
   * @return
   */
  // TODO: !!! how does this relate to getRestrictionForONodeID
  public byte getClassType(String restrictionURI)
  {

    Resource res = getResource(restrictionURI);
    logger.debug("Converted to resource: " + res);
    String rep1 = "<" + restrictionURI + ">";
    if (res instanceof BNode) {
      logger.debug("is an instance of Bnode: " + res);
      rep1 = restrictionURI;
    }

    if (res instanceof BNode) {
      logger.debug("is an instance of Bnode: " + res);
      String query = "select * from {" + rep1 + "} owl:hasValue {B}";
      if (hasSerqlQueryResultRows(query)) {
        return OConstants.HAS_VALUE_RESTRICTION;
      }

      query = "select * from {" + rep1 + "} owl:someValuesFrom {B}";
      if (hasSerqlQueryResultRows(query)) {
        return OConstants.SOME_VALUES_FROM_RESTRICTION;
      }

      query = "select * from {" + rep1 + "} owl:allValuesFrom {B}";
      if (hasSerqlQueryResultRows(query)) {
        return OConstants.ALL_VALUES_FROM_RESTRICTION;
      }

      query = "select * from {" + rep1 + "} owl:cardinality {B}";
      if (hasSerqlQueryResultRows(query)) {
        return OConstants.CARDINALITY_RESTRICTION;
      }

      query = "select * from {" + rep1 + "} owl:minCardinality {B}";
      if (hasSerqlQueryResultRows(query)) {
        return OConstants.MIN_CARDINALITY_RESTRICTION;
      }

      query = "select * from {" + rep1 + "} owl:maxCardinality {B}";
      if (hasSerqlQueryResultRows(query)) {
        return OConstants.MAX_CARDINALITY_RESTRICTION;
      }
    }

    if (res instanceof BNode) {
      logger.debug("is an instance of Bnode: " + res);
      return OConstants.ANNONYMOUS_CLASS;
    } else {
      logger.debug("is an ordinary class: " + res);
      return OConstants.OWL_CLASS;
    }
  }


  /**
   * The method is useful for adding statements into the graph. All three values
   * must exist in repository. These values are cast in Resources and then added
   * into the graph of repository.
   * 
   * @param subjectURI
   * @param predicateURI
   * @param objectURI
   */
  public void addStatement(String subjectURI,
      String predicateURI, String objectURI)
  {
    try {
      startTransaction(null);
      Resource s = subjectURI != null ? getResource(subjectURI) : null;
      URI p =
          predicateURI != null
          ? repositoryConnection.getValueFactory().createURI(predicateURI)
          : null;
      Resource o = objectURI != null ? getResource(objectURI) : null;
      repositoryConnection.add(s, p, o);
      endTransaction(null);
    } catch (Exception e) {
      throw new GateOntologyException(
          "error while adding statement into the repository where subject:" + subjectURI + " predicate:" + predicateURI + " objectURI:" + objectURI, e);
    }
  }

  /**
   * The method is useful for removing statements from the graph of repository.
   * All three values must exist in repository. these values are cast in
   * Resources and then removed from teh graph of repository.
   * 
   * @param subjectURI
   * @param predicateURI
   * @param objectURI
   */
  public void removeStatement(String subjectURI,
      String predicateURI, String objectURI)
  {
    try {
      startTransaction(null);
      Resource s = subjectURI != null ? getResource(subjectURI) : null;
      URI p =
          predicateURI != null
          ? repositoryConnection.getValueFactory().createURI(predicateURI)
          : null;
      Resource o = objectURI != null ? getResource(objectURI) : null;
      repositoryConnection.remove(s, p, o);
      endTransaction(null);
    } catch (Exception e) {
      throw new GateOntologyException(
          "error while removing statement from the repository where subject:" + subjectURI + " predicate:" + predicateURI + " objectURI:" + objectURI, e);
    }

  }

  // ***************************************************************************
  // *********************** Other Utility Methods
  // **************************************************************************
  private void addUUUStatement(String subject, String predicate, String object)
  {
    try {
      startTransaction(null);
      Resource s = subject != null ? getResource(subject) : null;
      URI p =
          predicate != null ? repositoryConnection.getValueFactory().createURI(predicate) : null;
      Resource o = object != null ? getResource(object) : null;
      repositoryConnection.add(s, p, o, DATA_CONTEXT_URI);
      endTransaction(null);
    } catch (Exception e) {
      throw new GateOntologyException(
          "error while adding statement into the repository where subject:" + subject + " predicate:" + predicate + " objectURI:" + object, e);
    }
  }

  private void addUULStatement(String subject, String predicate, String object,
      String language) throws GateOntologyException {
    if (debug) {
      logger.debug("addUULStatement for " + subject + " / " + predicate + " / " + object);
    }
    try {
      startTransaction(null);
      Resource s = subject != null ? getResource(subject) : null;
      URI p =
          predicate != null ? repositoryConnection.getValueFactory().createURI(predicate) : null;
      Literal o = null;
      if (language == null) {
        o =
            object != null ? repositoryConnection.getValueFactory().createLiteral(object) : null;
      } else {
        o =
            object != null
            ? repositoryConnection.getValueFactory().createLiteral(object, language)
            : null;
      }
      repositoryConnection.add(s, p, o, DATA_CONTEXT_URI);
      endTransaction(null);
    } catch (Exception e) {
      throw new GateOntologyException(
          "error while adding statement into the repository where subject:" + subject + " predicate:" + predicate + " objectURI:" + object, e);
    }
  }

  public void addStatement(String subject,
      String predicate, String object, String datatype)
  {
    try {
      startTransaction(null);
      Resource s = subject != null ? getResource(subject) : null;
      URI p =
          predicate != null ? repositoryConnection.getValueFactory().createURI(predicate) : null;
      URI d = repositoryConnection.getValueFactory().createURI(datatype);
      Literal l =
          object != null ? repositoryConnection.getValueFactory().createLiteral(object, d) : null;
      repositoryConnection.add(s, p, l, DATA_CONTEXT_URI);
      endTransaction(null);
    } catch (Exception e) {
      throw new GateOntologyException(
          "error while adding statement into the repository where subject:" + subject + " predicate:" + predicate + " objectURI:" + object, e);
    }
  }

  public void addUUDStatement(String subject,
      String predicate, String object, String datatype)
  {
    try {
      startTransaction(null);
      Resource s = subject != null ? getResource(subject) : null;
      URI p =
          predicate != null ? repositoryConnection.getValueFactory().createURI(predicate) : null;
      URI d = repositoryConnection.getValueFactory().createURI(datatype);
      Literal l =
          object != null ? repositoryConnection.getValueFactory().createLiteral(object, d) : null;
      repositoryConnection.add(s, p, l, DATA_CONTEXT_URI);
      endTransaction(null);
    } catch (Exception e) {
      throw new GateOntologyException(
          "error while adding statement into the repository where subject:" + subject + " predicate:" + predicate + " objectURI:" + object, e);
    }
  }

  // NOTE: this originally returned the number of removed statements, but
  // this does not work any more with Sesame2
  private void removeUUUStatement(String subject, String predicate, String object)
  {
    try {
      startTransaction(null);
      Resource s = subject != null ? getResource(subject) : null;
      URI p =
          predicate != null ? repositoryConnection.getValueFactory().createURI(predicate) : null;
      Resource o = object != null ? getResource(object) : null;
      //int no = repositoryConnection.remove(s, p, o);
      // TODO: should we restrict removal to the DATA context?
      repositoryConnection.remove(s, p, o);
      endTransaction(null);
      //return no;
    } catch (Exception e) {
      throw new GateOntologyException(
          "error while removing statement from the repository where subject:" + subject + " predicate:" + predicate + " objectURI:" + object, e);
    }
  }

  private void removeUULStatement(String subject, String predicate,
      String object, String language)
  {
    try {
      startTransaction(null);
      Resource s = subject != null ? getResource(subject) : null;
      URI p =
          predicate != null ? repositoryConnection.getValueFactory().createURI(predicate) : null;
      Literal l = null;
      if (language == null) {
        l =
            object != null ? repositoryConnection.getValueFactory().createLiteral(object) : null;
      } else {
        l =
            object != null ? repositoryConnection.getValueFactory().createLiteral(object,
            language) : null;
      }
      repositoryConnection.remove(s, p, l);
      endTransaction(null);
    } catch (Exception e) {
      throw new GateOntologyException(
          "error while removing statement from the repository where subject:" + subject + " predicate:" + predicate + " objectURI:" + object, e);
    }
  }

  public void removeUUDStatement(String subject,
      String predicate, String object, String datatype)
  {
    try {
      startTransaction(null);
      Resource s = subject != null ? getResource(subject) : null;
      URI p =
          predicate != null ? repositoryConnection.getValueFactory().createURI(predicate) : null;
      URI d = repositoryConnection.getValueFactory().createURI(datatype);
      Literal l =
          object != null ? repositoryConnection.getValueFactory().createLiteral(object) : null;

      repositoryConnection.remove(s, p, l);

      l =
          object != null ? repositoryConnection.getValueFactory().createLiteral(object, d) : null;
      repositoryConnection.remove(s, p, l);
      endTransaction(null);
    } catch (Exception e) {
      throw new GateOntologyException(
          "error while removing statement from the repository where subject:" + subject + " predicate:" + predicate + " objectURI:" + object, e);
    }
  }

  public void removeStatement(String subject,
      String predicate, String object, String datatype)
  {
    try {
      startTransaction(null);
      Resource s = subject != null ? getResource(subject) : null;
      URI p =
          predicate != null ? repositoryConnection.getValueFactory().createURI(predicate) : null;
      URI d = repositoryConnection.getValueFactory().createURI(datatype);
      Literal l =
          object != null ? repositoryConnection.getValueFactory().createLiteral(object) : null;

      repositoryConnection.remove(s, p, l);

      l =
          object != null ? repositoryConnection.getValueFactory().createLiteral(object, d) : null;
      repositoryConnection.remove(s, p, l);
      endTransaction(null);
    } catch (Exception e) {
      throw new GateOntologyException(
          "error while removing statement from the repository where subject:" + subject + " predicate:" + predicate + " objectURI:" + object, e);
    }
  }

  public void startTransaction(String repositoryID)
  {
  }

  public void endTransaction(String repositoryID) throws GateOntologyException {
  }


  private Property[] listToPropertyArray(List<Property> list) {
    if (list == null) {
      return null;
    }
    ArrayList<Property> subList = new ArrayList<Property>();
    for (int i = 0; i < list.size(); i++) {
      if (hasSystemNameSpace(list.get(i).getUri())) {
        continue;
      }
      subList.add(list.get(i));
    }
    Property[] props = new Property[subList.size()];
    for (int i = 0; i < subList.size(); i++) {
      props[i] = subList.get(i);
    }
    return props;
  }

  private PropertyValue[] listToPropertyValueArray(List<PropertyValue> subList) {
    if (subList == null) {
      return null;
    }
    PropertyValue[] props = new PropertyValue[subList.size()];
    for (int i = 0; i < subList.size(); i++) {
      props[i] = subList.get(i);
    }
    return props;
  }

  private ResourceInfo[] listToResourceInfoArray(List<String> list) {
    if (list == null) {
      return null;
    }
    ArrayList<ResourceInfo> subList = new ArrayList<ResourceInfo>();
    for (int i = 0; i < list.size(); i++) {
      String resourceURI = list.get(i);
      if (hasSystemNameSpace(resourceURI)) {
        continue;
      }
      byte classType = getClassType(resourceURI);
      if (classType == OConstants.ANNONYMOUS_CLASS) {
        continue;
      }
      subList.add(new ResourceInfo(list.get(i).toString(), classType));
    }

    ResourceInfo[] strings = new ResourceInfo[subList.size()];
    for (int i = 0; i < subList.size(); i++) {
      strings[i] = subList.get(i);
    }
    return strings;
  }

  /**
   * This method tells whether the resource is imported or added as an explicit
   * statement.
   * 
   * @param resourceURI
   * @return
   */

  // JP: seems what is meant here is "implicit" not "imported"
  public boolean isImplicitResource(String resourceURI)
  {
    try {
      return !repositoryConnection.hasStatement(getResource(resourceURI),
          makeSesameURI(RDF.TYPE.toString()), null, false);
    } catch (Exception e) {
      throw new GateOntologyException(e);
    }
  }

  private String[] listToArray(List<String> list) {
    if (list == null) {
      return null;
    }
    ArrayList<String> subList = new ArrayList<String>();
    for (int i = 0; i < list.size(); i++) {
      if (hasSystemNameSpace(list.get(i))) {
        continue;
      }
      subList.add(list.get(i));
    }
    String[] strings = new String[subList.size()];
    for (int i = 0; i < subList.size(); i++) {
      strings[i] = subList.get(i);
    }
    return strings;
  }

  private org.openrdf.model.URI makeSesameURI(String string) {
    Resource rs = repositoryConnection.getValueFactory().createURI(string);
    return (URI) rs;
  }

  private Resource getResource(String string) {
    Resource rs = null; // resourcesMap.get(string);
    // TODO: !!!!!
    if(string.startsWith("_:") ||
        (!string.startsWith("_:") && !string.contains(":"))) {
      if(string.startsWith("_:")) {
        string = string.substring(2);
      }
      rs = repositoryConnection.getValueFactory().createBNode(string);
      logger.debug("Created BNode resource for " + rs);
      //resourcesMap.put(string, rs);
    } else {
      logger.debug("Creating resource for " + string);
      rs = repositoryConnection.getValueFactory().createURI(string);
      logger.debug("Created URI resource for " + string);
      //resourcesMap.put(string, rs);
    }
    logger.debug("Created resource " + rs);
    return rs;
  }




  // TODO: get rid of this and use ontology objects directly!
  private Property createPropertyObject(String uri)
      throws GateOntologyException {
    byte type = OConstants.ANNOTATION_PROPERTY;
    if (isAnnotationProperty(uri)) {
      type = OConstants.ANNOTATION_PROPERTY;
    } else if (isObjectProperty(uri)) {
      type = OConstants.OBJECT_PROPERTY;
    } else if (isDatatypeProperty(uri)) {
      type = OConstants.DATATYPE_PROPERTY;
    } else if (isTransitiveProperty(uri)) {
      type = OConstants.TRANSITIVE_PROPERTY;
    } else if (isSymmetricProperty(uri)) {
      type = OConstants.SYMMETRIC_PROPERTY;
    } else if (isRDFProperty(uri)) {
      type = OConstants.RDF_PROPERTY;
    } else {
      return null;
    }
    return new Property(type, uri);
  }


  /**
   * This method is used to obtain the most specific classes
   * 
   * @param values
   * @return
   */
  private ResourceInfo[] reduceToMostSpecificClasses(List<ResourceInfo> values)
  {
    if (values == null || values.isEmpty()) {
      return new ResourceInfo[0];
    }
    List<String> classes = new ArrayList<String>();
    for (int i = 0; i < values.size(); i++) {
      classes.add(values.get(i).getUri());
    }
    outer:
    for (int i = 0; i < classes.size(); i++) {
      String c = classes.get(i);
      // if the class's children appear in list, it is not the most
      // specific class

      String queryRep = string2Turtle(c);
      String query =
          "select distinct A FROM {A} rdfs:subClassOf {" + queryRep +
          "} WHERE A!=" + queryRep +
          " AND A != ALL ( " +
          " select distinct B FROM {B} owl:equivalentClass {" + queryRep + "} )";

      List<String> list = new ArrayList<String>();
      addSerqlQueryResultToCollection(query, list);

      for (int j = 0; j < list.size(); j++) {
        if (classes.contains(list.get(j))) {
          classes.remove(i);
          values.remove(i);
          i--;
          continue outer;
        }
      }
    }
    return values.toArray(new ResourceInfo[0]);
  }

  /**
   * This method is used to obtain the most specific classes
   * 
   * @param values
   * @return
   */
  private List<String> reduceToMostSpecificClasses(Set<String> values)
  {
    if (values == null || values.isEmpty()) {
      return new ArrayList<String>();
    }
    List<String> classes = new ArrayList<String>(values);
    outer:
    for (int i = 0; i < classes.size(); i++) {
      String c = classes.get(i);
      // if the class's children appear in list, it is not the most
      // specific class

      String queryRep = string2Turtle(c);
      String query =
          "select distinct A FROM {A} rdfs:subClassOf {" + queryRep +
          "} WHERE A!=" + queryRep + " AND A!= ALL ( " +
          " select distinct B FROM {B} owl:equivalentClass {" + queryRep + "} )";

      List<String> list = new ArrayList<String>();
      addSerqlQueryResultToCollection(query, list);

      for (int j = 0; j < list.size(); j++) {
        if (classes.contains(list.get(j))) {
          classes.remove(i);
          i--;
          continue outer;
        }
      }
    }
    return classes;
  }

  private byte getPropertyType(String aPropertyURI)
      throws GateOntologyException {
    if (isDatatypeProperty(aPropertyURI)) {
      return OConstants.DATATYPE_PROPERTY;
    } else if (isTransitiveProperty(aPropertyURI)) {
      return OConstants.TRANSITIVE_PROPERTY;
    } else if (isSymmetricProperty(aPropertyURI)) {
      return OConstants.SYMMETRIC_PROPERTY;
    } else if (isObjectProperty(aPropertyURI)) {
      return OConstants.OBJECT_PROPERTY;
    } else if (isAnnotationProperty(aPropertyURI)) {
      return OConstants.ANNOTATION_PROPERTY;
    } else {
      return OConstants.RDF_PROPERTY;
    }
  }

  private PropertyValue[] getPropertyValues(String aResourceURI, String aPropertyURI) throws GateOntologyException {
    Resource r = getResource(aResourceURI);
    String rep1 = "<" + aResourceURI + ">";
    String rep2 = "{" + rep1 + "}";
    if (r instanceof BNode) {
      rep1 = "_:" + aResourceURI;
      rep2 = "{" + rep1 + "}";
    }
    String query =
        "Select DISTINCT Y from " + rep2 + " <" + aPropertyURI + "> {Y}";
    UtilTupleQueryIterator result = performSerqlQuery(query);
    List<PropertyValue> list = new ArrayList<PropertyValue>();
    while (result.hasNext()) {
      list.add(new PropertyValue(String.class.getName(), result.nextFirstAsString()));
    }
    result.close();
    return listToPropertyValueArray(list);
  }


  String executeQuery(String serqlQuery)  {
    //logger.info("executeQuery: "+serqlQuery);
    TupleQueryResult res = null;
    String ret = "";
    String msg = "Error executing query: "+serqlQuery;
    try {
      res = repositoryConnection.prepareTupleQuery(org.openrdf.query.QueryLanguage.SERQL, serqlQuery).evaluate();
      // TODO: convert to string that is compatible what the old Sesame1 to string
      // method did!
      // code taken from Sesame1    org.openrdf.sesame.query.QueryResultsTable.toString()
      StringBuffer buf = new StringBuffer();
      List<String> bindings = res.getBindingNames();
      //System.out.println("Found bindings: "+bindings);
      String[] _columnNames = bindings.toArray(new String[0]);
      //System.out.println("Found columns names: "+_columnNames);
  		if (_columnNames != null) {
			for (int i = 0; i < _columnNames.length; i++) {
				if (i > 0) {
					buf.append("\t| ");
				}
				buf.append(_columnNames[i]);
			}
			buf.append('\n');

			int dashCount = buf.length() + 7*(_columnNames.length-1);
			for (int i = 0; i < dashCount; i++) {
				buf.append('-');
			}
			buf.append('\n');
		}

    Vector<Value> columns = new Vector<Value>(bindings.size());
    for(int j = 0; j < bindings.size(); j++) {
      columns.add(null);
    }
		//for (int i = 0; i < _rowList.size(); i++) {
    while(res.hasNext()) {
      BindingSet bs = res.next();
      int i = 0;
      for(String name : bindings) {
        columns.set(i++, bs.getValue(name));
        //System.out.println("Found columns: "+columns);
      }
			//List columns = (List)_rowList.get(i);

			for (int j = 0; j < columns.size(); j++) {
				if (j > 0) {
					buf.append("\t| ");
				}
				buf.append( columns.get(j).stringValue() );
			}
			buf.append('\n');
		}

    ret = buf.toString();

    } catch (QueryEvaluationException ex) {
        throw new GateOntologyException(msg,ex);
    } catch (RepositoryException ex) {
        throw new GateOntologyException(msg,ex);
    } catch (MalformedQueryException ex) {
        throw new GateOntologyException(msg,ex);
    } finally {
      if(res != null) {
        try {
          res.close();
        } catch (QueryEvaluationException ex) {
          throw new GateOntologyException(msg,ex);
        }
      }
    }
    //logger.info("executeQuery returns:\n"+ret+"\n");
    return ret;
  }

  // ***************************************************************************
  // *** UTILITY FUNCTIONS
  // ***************************************************************************

  public RepositoryConnection getRepositoryConnection() {
    return repositoryConnection;
  }

  // TODO: is returnSystemStatements still relevant?
  // if yes, check how often and where actually used
  // This should probably become part of the query anyways.
  // Try to get rid and move entirely to UtilConvert
  private boolean hasSystemNameSpace(String uri) {
    if (returnSystemStatements) {
      return false;
    }
    Boolean  val = new Boolean(Utils.hasSystemNameSpace(uri));
    return val.booleanValue();
  }


  private RDFWriter getRDFWriter4Format(
      OutputStream out, OntologyFormat ontologyFormat) {
    RDFWriter writer = null;
    switch (ontologyFormat) {
      case N3:
        writer = new N3Writer(out);
        break;
      case NTRIPLES:
        writer = new NTriplesWriter(out);
        break;
      case TURTLE:
        writer = new TurtleWriter(out);
        break;
      case RDFXML:
        writer = new RDFXMLWriter(out);
        break;
      default:
        throw new GateOntologyException("Unsupported ontology format: " + ontologyFormat);
    }
    return writer;
  }

  private RDFWriter getRDFWriter4Format(
      Writer out, OntologyFormat ontologyFormat) {
    RDFWriter writer = null;
    switch (ontologyFormat) {
      case N3:
        writer = new N3Writer(out);
        break;
      case NTRIPLES:
        writer = new NTriplesWriter(out);
        break;
      case TURTLE:
        writer = new TurtleWriter(out);
        break;
      case RDFXML:
        writer = new RDFXMLWriter(out);
        break;
      default:
        throw new GateOntologyException("Unsupported ontology format: " + ontologyFormat);
    }
    return writer;
  }

  private RDFFormat ontologyFormat2RDFFormat(OntologyFormat format) {
    switch(format) {
      case RDFXML:
        return RDFFormat.RDFXML;
      case N3:
        return RDFFormat.N3;
      case NTRIPLES:
        return RDFFormat.NTRIPLES;
      case TURTLE:
        return RDFFormat.TURTLE;
      default:
        throw new GateOntologyException("Unsupported ontology format: "+format);
    }
  }

  private String string2Turtle(String queryRep1) {
    if(!queryRep1.startsWith("_:") && !queryRep1.startsWith("<")) {
      if(queryRep1.contains(":")) {
        queryRep1 = "<"+queryRep1+">";
      } else {
        queryRep1 = "_:"+queryRep1;
      }
    }
    return queryRep1;
  }

  private Resource string2SesameResource(String uriString) {
    if(uriString.startsWith("_:")) {
      return new BNodeImpl(uriString.substring(2));
    } else {
      // we can still get bnodeids from old methods where the initial _: is missing
      // we assume that if the string contains a colon it must be a proper
      // URI, otherwise it must be a Bnodeid
      if(uriString.contains(":")) {
        return new URIImpl(uriString);
      } else {
        return new BNodeImpl(uriString);
      }
    }
  }
  private Resource oNodeID2SesameResource(ONodeID id) {
    if(id.isAnonymousResource()) {
      // TODO: does this and should this include the _: part?
      return new BNodeImpl(id.getResourceName());
    } else {
      return new URIImpl(id.toString());
    }
  }

  private ONodeID string2ONodeID(String uri) {
    if(uri.startsWith("_:")) {
      return new OBNodeIDImpl(uri);
    } else if(uri.contains(":")) {
      return new OURIImpl(uri);
    } else {
      return new OBNodeIDImpl(uri);
    }
  }



  // The query language of the query is determined automatically: if the
  // query string contains "USING NAMESPACE" it is SERQL, if it contains
  // "PREFIX" at the beginning of a line it is SPARQL. If neither applies,
  // an exception is thrown.
  UtilTupleQueryIterator qp_getClassesTopAll;
  UtilTupleQueryIterator qp_getClassesAllAll;
  UtilTupleQueryIterator qp_getOntologyURIs;
  UtilTupleQueryIterator qp_getInstancesAll;
  UtilTupleQueryIterator qp_getInstancesAllFor;
  UtilTupleQueryIterator qp_getInstancesDirectFor;
  UtilTupleQueryIterator qp_hasInstance;
  // Unfortunately, the prepared queries do not work properly with
  // setBinding, we need to do String substitution!
  //UtilTupleQueryIterator qp_hasInstanceAllFor;
  //UtilTupleQueryIterator qp_hasInstanceDirectFor;
  String                 qs_hasInstanceAllFor;
  QueryLanguage          ql_hasInstanceAllFor;
  String                 qs_hasInstanceDirectFor;
  QueryLanguage          ql_hasInstanceDirectFor;

  UtilTupleQueryIterator qp_getRestrictionTypeFor;

  // Unfortunately, the prepared queries do not work properly with
  // setBinding, we need to do String substitution!
  //UtilTupleQueryIterator qp_getSubClassesAllFor;
  //UtilTupleQueryIterator qp_getSubClassesDirectFor;
  String                 qs_getSubClassesAllFor;
  QueryLanguage          ql_getSubClassesAllFor;
  String                 qs_getSubClassesDirectFor;
  QueryLanguage          ql_getSubClassesDirectFor;

  String                 qs_getClassesByNameNoW3;
  QueryLanguage          ql_getClassesByNameNoW3;
  //UtilTupleQueryIterator qp_getClassesByNameNoW3;
  File queriesDir;

  private void initQueries(String querySet) {
    queriesDir =
        new File(((AbstractOntologyImpl)ontology).getPluginDir(),"queries");
    queriesDir = new File(queriesDir,querySet);
    if(!queriesDir.exists()) {
      throw new GateOntologyException("Queries directory not found: "+queriesDir.getAbsolutePath());
    }

    qp_getClassesTopAll       = getPreparedTupleQueryFromFile("getClassesTopAll");
    qp_getClassesAllAll       = getPreparedTupleQueryFromFile("getClassesAllAll");
    qp_getOntologyURIs        = getPreparedTupleQueryFromFile("getOntologyURIs");
    qp_getInstancesAll        = getPreparedTupleQueryFromFile("getInstancesAll");
    qp_getInstancesDirectFor  = getPreparedTupleQueryFromFile("getInstancesDirectFor");
    qp_getInstancesAllFor     = getPreparedTupleQueryFromFile("getInstancesAllFor");
    qp_hasInstance            = getPreparedTupleQueryFromFile("hasInstance");
    //qp_hasInstanceAllFor      = getPreparedTupleQueryFromFile("hasInstanceAllFor");
    //qp_hasInstanceDirectFor   = getPreparedTupleQueryFromFile("hasInstanceDirectFor");
    qs_hasInstanceAllFor      = getQueryStringFromFile("hasInstanceAllFor");
    ql_hasInstanceAllFor      = determineQueryLanguage(qs_hasInstanceAllFor);
    qs_hasInstanceDirectFor   = getQueryStringFromFile("hasInstanceDirectFor");
    ql_hasInstanceDirectFor   = determineQueryLanguage(qs_hasInstanceDirectFor);
    qp_getRestrictionTypeFor  = getPreparedTupleQueryFromFile("getRestrictionTypeFor");

    qs_getSubClassesAllFor    = getQueryStringFromFile("getSubClassesAllFor");
    ql_getSubClassesAllFor    = determineQueryLanguage(qs_getSubClassesAllFor);
    qs_getSubClassesDirectFor = getQueryStringFromFile("getSubClassesDirectFor");
    ql_getSubClassesDirectFor = determineQueryLanguage(qs_getSubClassesDirectFor);

    qs_getClassesByNameNoW3   = getQueryStringFromFile("getClassesByNameNoW3");
    ql_getClassesByNameNoW3   = determineQueryLanguage(qs_getClassesByNameNoW3);

  }


  private UtilTupleQueryIterator getPreparedTupleQueryFromFile(String filename) {
    String queryString = null;
    try {
      queryString =
          FileUtils.readFileToString(new File(queriesDir, filename));
    } catch (IOException ex) {
      throw new GateOntologyException("Could not read query file: "+filename,ex);
    }
    QueryLanguage queryLanguage = determineQueryLanguage(queryString);
    return new UtilTupleQueryIterator(repositoryConnection,
          queryString,queryLanguage);
  }

  private String getQueryStringFromFile(String filename) {
    String queryString = null;
    try {
      queryString =
          FileUtils.readFileToString(new File(queriesDir, filename));
    } catch (IOException ex) {
      throw new GateOntologyException("Could not read query file: "+filename,ex);
    }
    return queryString;
  }

  private UtilBooleanQuery getPreparedBooleanQueryFromFile(String filename) {
    String queryString = null;
    try {
      queryString =
          FileUtils.readFileToString(new File(queriesDir, filename));
    } catch (IOException ex) {
      throw new GateOntologyException("Could not read query file: "+filename,ex);
    }
    QueryLanguage queryLanguage = determineQueryLanguage(queryString);
    return new UtilBooleanQuery(repositoryConnection,
          queryString,queryLanguage);

  }


  private QueryLanguage determineQueryLanguage(String query) {
    if(query.contains("USING NAMESPACE")) {
      return QueryLanguage.SERQL;
    } else if(query.contains("PREFIX ")) {
      return QueryLanguage.SPARQL;
    } else {
      throw new GateOntologyException("Could not determine query language for: "+query);
    }
  }



  // ***************************************************************************
  // **** STUFF TO GET RID OF EVENTUALLY
  // ***************************************************************************


  /**
   * Debug parameter, if set to true, shows various messages when different
   * methods are invoked
   */
  private boolean debug = true;

  public void setDebug(boolean debug) {
    this.debug = debug;
  }

  public boolean getDebug() {
    return debug;
  }

  // TODO: STUFF TO GET RID OF
  // TODO: get rid of this or make semantics available to API?
  // If it is just internal, make this local to whatever method and
  // context where it is relevant
  private boolean returnSystemStatements = false;



}
