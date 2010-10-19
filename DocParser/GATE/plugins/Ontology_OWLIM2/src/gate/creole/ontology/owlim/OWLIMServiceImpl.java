package gate.creole.ontology.owlim;

import gate.creole.ontology.GateOntologyException;
import gate.creole.ontology.InvalidValueException;
import gate.creole.ontology.OConstants;

import java.io.BufferedOutputStream;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.Writer;
import java.net.MalformedURLException;
import java.net.URISyntaxException;
import java.net.URL;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import javax.jws.WebService;

import org.openrdf.model.BNode;
import org.openrdf.model.Literal;
import org.openrdf.model.Resource;
import org.openrdf.model.Statement;
import org.openrdf.model.URI;
import org.openrdf.model.Value;
import org.openrdf.rio.RdfDocumentWriter;
import org.openrdf.rio.n3.N3Writer;
import org.openrdf.rio.ntriples.NTriplesWriter;
import org.openrdf.rio.rdfxml.RdfXmlWriter;
import org.openrdf.rio.turtle.TurtleWriter;
import org.openrdf.sesame.admin.AdminListener;
import org.openrdf.sesame.config.AccessDeniedException;
import org.openrdf.sesame.config.ConfigurationException;
import org.openrdf.sesame.config.RepositoryConfig;
import org.openrdf.sesame.config.RepositoryInfo;
import org.openrdf.sesame.config.SailConfig;
import org.openrdf.sesame.config.UnknownRepositoryException;
import org.openrdf.sesame.config.UserInfo;
import org.openrdf.sesame.config.handlers.SystemConfigFileHandler;
import org.openrdf.sesame.constants.QueryLanguage;
import org.openrdf.sesame.constants.RDFFormat;
import org.openrdf.sesame.query.MalformedQueryException;
import org.openrdf.sesame.query.QueryEvaluationException;
import org.openrdf.sesame.query.QueryResultsTable;
import org.openrdf.sesame.repository.RepositoryList;
import org.openrdf.sesame.repository.SesameRepository;
import org.openrdf.sesame.repository.local.LocalRepository;
import org.openrdf.sesame.repository.local.LocalService;
import org.openrdf.sesame.sail.NamespaceIterator;
import org.openrdf.sesame.sail.SailUpdateException;
import org.openrdf.sesame.sail.StatementIterator;
import org.openrdf.sesame.sailimpl.OWLIMSchemaRepository;
import org.openrdf.vocabulary.OWL;
import org.openrdf.vocabulary.RDF;
import org.openrdf.vocabulary.RDFS;

/**
 * Implementation of the GATE Ontology Services. This class provides an
 * implementation of each and every service defined under the OWLIM interface.
 * 
 * @author niraj
 */
@WebService(endpointInterface = "gate.creole.ontology.owlim.OWLIM", targetNamespace = "http://gate.ac.uk/ns/ontology/owlim")
public class OWLIMServiceImpl implements OWLIM, AdminListener {
  private static int nextIndex = 0;
  private int myIndex = nextIndex++;

  private HashMap<String, RepositoryDetails> mapToRepositoryDetails =
    new HashMap<String, RepositoryDetails>();

  private HashMap<String, Resource> resourcesMap =
    new HashMap<String, Resource>();

  private HashMap<String, Boolean> hasSystemNameSpace =
    new HashMap<String, Boolean>();

  /**
   * Debug parameter, if set to true, shows various messages when different
   * methods are invoked
   */
  private boolean debug = false;

  public void setDebug(boolean debug) {
    this.debug = debug;
  }

  public boolean getDebug() {
    return debug;
  }

  /**
   * OWLIMSchemaRepository is used as an interaction layer on top of Sesame
   * server. The class provides various methods of manipulating ontology data.
   */
  private OWLIMSchemaRepository sail;

  /**
   * The reference of currently selected repository is stored in this variable
   */
  private SesameRepository currentRepository;

  /**
   * Current Events Log
   */
  private OntologyEventsLog currentEventsLog;

  /**
   * The class that provides an implementation of the OWLIM_SCHEMA_REPOSITORY
   */
  public static final String OWLIM_SCHEMA_REPOSITORY_CLASS =
    "org.openrdf.sesame.sailimpl.OWLIMSchemaRepository";

  /**
   * The file that stores the various configuration parameters
   */
  private File systemConf = null;

  public void setSystemConfLocation(File systemConf) {
    this.systemConf = systemConf;
  }

  /**
   * The file that stores the RDFS schema defining OWL.
   */
  private URL owlRDFS = null;

  public void setOwlRDFSLocation(URL owlRDFS) {
    this.owlRDFS = owlRDFS;
  }

  /**
   * The file that stores the RDF schema defining RDFS.
   */
  private URL rdfSchema = null;

  public void setRdfSchemaLocation(URL rdfSchema) {
    this.rdfSchema = rdfSchema;
  }

  /**
   * Ontology URL
   */
  private String ontologyUrl;

  /**
   * Whether the repository should return system statements
   */
  private boolean returnSystemStatements;

  /**
   * GOS Home
   */
  private URL gosHomeURL;

  public void setGosHomeURL(URL gosHomeURL) {
    this.gosHomeURL = gosHomeURL;
  }

  /**
   * LocalService used to talk to Sesame.
   */
  private LocalService service;

  public void setSesameLocalService(LocalService service) {
    this.service = service;
  }

  /**
   * Should the sesame service be shut down on a logout? <code>true</code> for a
   * standalone service, <code>false</code> for a shared service.
   */
  private boolean shutDownOnLogout = true;

  public void setShutDownOnLogout(boolean shutDownOnLogout) {
    this.shutDownOnLogout = shutDownOnLogout;
  }

  /**
   * Constructor
   */
  public OWLIMServiceImpl() {
    super();
  }

  // *************************************
  // Listener methods
  // *************************************

  /**
   * An error message with optionally line and column number and the statement
   * to indicate the source of the error.
   */
  public void error(String msg, int lineNo, int columnNo, Statement statement) {
    System.err.println("ERROR :" + msg + "\n at line number :" + lineNo
      + " column number :" + columnNo);
    if(statement != null) {
      System.err.println("With Statement <");
      if(statement.getSubject() != null) {
        System.err.println("Subject :" + statement.getSubject().toString());
      }
      else {
        System.err.println("Subject : not available");
      }
      if(statement.getPredicate() != null) {
        System.err.println("Predicate :" + statement.getPredicate().toString());
      }
      else {
        System.err.println("Predicate : not available");
      }
      if(statement.getObject() != null) {
        System.err.println("Object :" + statement.getObject().toString());
      }
      else {
        System.err.println("Object : not available");
      }
    }
    throw new RuntimeException("ERROR :" + msg + "\n at line number :" + lineNo
      + " column number :" + columnNo);
  }

  /**
   * A notification message (not an error) with optionally line and column
   * number and the statement to indicate the source of the notification.
   */
  public void notification(String msg, int lineNo, int columnNo,
    Statement statement) {
  }

  /**
   * A status message with optional line and column number to indicate progress.
   */
  public void status(String msg, int lineNo, int columnNo) {

  }

  /**
   * Indicates the end of a transaction.
   */
  public void transactionEnd() {
    // don't do anything
  }

  /**
   * Indicates the start of a transaction.
   */
  public void transactionStart() {
    // don't do anything here
  }

  /**
   * warning messages
   */
  public void warning(String msg, int lineNo, int columnNo, Statement statement) {
    System.err.println("WARNING :" + msg + "\n at line number :" + lineNo
      + " column number :" + columnNo);
    if(statement != null) {
      System.err.println("With Statement <");
      if(statement.getSubject() != null) {
        System.err.println("Subject :" + statement.getSubject().toString());
      }
      else {
        System.err.println("Subject : not available");
      }
      if(statement.getPredicate() != null) {
        System.err.println("Predicate :" + statement.getPredicate().toString());
      }
      else {
        System.err.println("Predicate : not available");
      }
      if(statement.getObject() != null) {
        System.err.println("Object :" + statement.getObject().toString());
      }
      else {
        System.err.println("Object : not available");
      }
    }
  }

  /**
   * Initialises the OWLIM service. {@link #setSesameLocalService} must have
   * been called before calling this method. You may also wish to set the
   * gosHomeURL, owl.rdfs and rdf-schema.xml locations explicitly, though
   * reasonable defaults will be used if these are not set. Also, if you wish to
   * persist the system configuration, {@link #setSystemConfLocation} must have
   * been called with a valid non-null path.
   */
  public void init() throws GateOntologyException {
    // sanity checks first
    if(service == null) { throw new GateOntologyException(
      "Sesame Local Service must not be null"); }
    if(debug) {
      System.out.println("Initialising OWLIMServiceImpl: index = " + myIndex);
    }
    try {
      if(gosHomeURL == null) {
        URL classURL =
          this.getClass().getResource(
            "/" + this.getClass().getName().replace('.', '/') + ".class");
        if(classURL.getProtocol().equals("jar")) {
          // running from annic.jar
          String classURLStr = classURL.getFile();
          URL gosJarURL =
            new URL(classURLStr.substring(0, classURLStr.indexOf('!')));
          // gosURLJar is "file:/path/to/gos/lib/file.jar"
          gosHomeURL = new URL(gosJarURL, "..");
          // gosHomeURL is "file:/path/to/gos/"
        }
        else if(classURL.getProtocol().equals("file")) {
          // running from classes directory (e.g.inside Eclipse)
          // classURL is
          // "file:/path/to/gos/classes/gate/creole/ontology/owlim/OWLIMServiceImpl.class"
          gosHomeURL = new URL(classURL, "../../../../..");
          // gosHomeURL is "file:/path/to/gos/"
        }
      }

      if(!gosHomeURL.getPath().endsWith("/")) {
        // add a slash to the end - should we throw an exception instead?
        gosHomeURL = new URL(gosHomeURL.toExternalForm() + "/");
      }
    }
    catch(MalformedURLException e) {
      throw new GateOntologyException("Could not determine gosHomeURL", e);
    }

    if(debug) {
      System.out.println("GOS Home: " + gosHomeURL);
    }

    hasSystemNameSpace.put("http://www.w3.org/2002/07/owl#Thing", new Boolean(
      true));

    try {
      if(owlRDFS == null) {
        owlRDFS = new URL(gosHomeURL, "owl.rdfs");
      }
      if(rdfSchema == null) {
        rdfSchema = new URL(gosHomeURL, "rdf-schema.xml");
      }
    }
    catch(MalformedURLException e) {
      throw new GateOntologyException(
        "Could not construct owl.rdfs and rdf-schema.xml URLs.  "
          + "Please set them explicitly.", e);
    }

    login("admin", "admin");
    String[] repositories = getRepositoryList();
    if(repositories != null) {
      for(String rep : repositories) {
        setCurrentRepositoryID(rep);
      }
    }
  }

  /**
   * This method reports the events observed. Each event is a tupple consists of
   * the following: + or - that indicates addition or removal of the tripple.
   * subject - URI or * if all predicate - URI or * if all object - URI or * if
   * all datatype - URI or * if all
   * 
   * @param repositoryID
   * @return
   * @throws GateOntologyException
   */
  public String[] getEventsLog(String repositoryID)
    throws GateOntologyException {
    loadRepositoryDetails(repositoryID);
    String[] events = new String[currentEventsLog.getEvents().size()];
    for(int i = 0; i < currentEventsLog.getEvents().size(); i++) {
      events[i] = currentEventsLog.getEvents().get(i).toString();
    }
    return events;
  }

  /**
   * Gets the default name space for this ontology. The defaultNameSpace is (by
   * default) used for the newly created resources.
   * 
   * @return a String value.
   */
  public String getDefaultNameSpace(String repositoryID)
    throws GateOntologyException {
    loadRepositoryDetails(repositoryID);
    NamespaceIterator iter = sail.getNamespaces();
    while(iter.hasNext()) {
      iter.next();
      if(iter.getPrefix() == null || iter.getPrefix().trim().length() == 0)
        return iter.getName();
    }
    return null;
  }

  /**
   * Adds the ontology data
   * 
   * @param repositoryID
   * @param data
   * @param baseURI
   * @param format
   * @throws GateOntologyException
   */
  public void addOntologyData(String repositoryID, String data, String baseURI,
    byte format) throws GateOntologyException {
    loadRepositoryDetails(repositoryID);
    try {
      startTransaction(repositoryID);
      currentRepository
        .addData(data, baseURI, getRDFFormat(format), true, this);
      endTransaction(repositoryID);
    }
    catch(AccessDeniedException ioe) {
      throw new GateOntologyException(
        "Could not add data into the repository because " + ioe.getMessage(),
        ioe);
    }
    catch(IOException ioe) {
      throw new GateOntologyException(
        "Could not add data into the repository because " + ioe.getMessage(),
        ioe);
    }
  }

  /**
   * Exports the ontology data into the provided format to the provided output
   * stream.
   * 
   * @param out
   * @param format
   */
  public void writeOntologyData(String repositoryID, OutputStream out,
    byte format) throws Exception {
    loadRepositoryDetails(repositoryID);
    if(sail.transactionStarted()) sail.commitTransaction();

    RdfDocumentWriter writer = null;
    switch(format){
      case OConstants.ONTOLOGY_FORMAT_N3:
        writer = new N3Writer(out);
        break;
      case OConstants.ONTOLOGY_FORMAT_NTRIPLES:
        writer = new NTriplesWriter(out);
        break;
      case OConstants.ONTOLOGY_FORMAT_TURTLE:
        writer = new TurtleWriter(out);
        break;
      default:
        writer = new RdfXmlWriter(out);
        break;
    }
    writeData(writer);
    switch(format){
      case OConstants.ONTOLOGY_FORMAT_N3:
        ((N3Writer)writer).endDocument();
        break;
      case OConstants.ONTOLOGY_FORMAT_NTRIPLES:
        ((NTriplesWriter)writer).endDocument();
        break;
      case OConstants.ONTOLOGY_FORMAT_TURTLE:
        ((TurtleWriter)writer).endDocument();
        break;
      default:
        ((RdfXmlWriter)writer).endDocument();
        break;
    }
  }

  /**
   * Exports the ontology data into the provided format using the provided
   * writer.
   * 
   * @param out
   * @param format
   */
  public void writeOntologyData(String repositoryID, Writer out, byte format)
    throws Exception {
    loadRepositoryDetails(repositoryID);
    if(sail.transactionStarted()) sail.commitTransaction();

    RdfDocumentWriter writer = null;
    switch(format){
      case OConstants.ONTOLOGY_FORMAT_N3:
        writer = new N3Writer(out);
        break;
      case OConstants.ONTOLOGY_FORMAT_NTRIPLES:
        writer = new NTriplesWriter(out);
        break;
      case OConstants.ONTOLOGY_FORMAT_TURTLE:
        writer = new TurtleWriter(out);
        break;
      default:
        writer = new RdfXmlWriter(out);
        break;
    }
    writeData(writer);
    switch(format){
      case OConstants.ONTOLOGY_FORMAT_N3:
        ((N3Writer)writer).endDocument();
        break;
      case OConstants.ONTOLOGY_FORMAT_NTRIPLES:
        ((NTriplesWriter)writer).endDocument();
        break;
      case OConstants.ONTOLOGY_FORMAT_TURTLE:
        ((TurtleWriter)writer).endDocument();
        break;
      default:
        ((RdfXmlWriter)writer).endDocument();
        break;
    }
  }

  private void writeData(RdfDocumentWriter writer) throws IOException {
    NamespaceIterator nIter = sail.getNamespaces();
    while(nIter.hasNext()) {
      nIter.next();
      writer.setNamespace(nIter.getPrefix(), nIter.getName());
    }
    writer.startDocument();
    writer.writeComment("Ontology Generated from GATE");
    StatementIterator iter = sail.getStatements(null, null, null);
    while(iter.hasNext()) {
      Statement stmt = iter.next();
      writer.writeStatement(stmt.getSubject(), stmt.getPredicate(), stmt
        .getObject());
    }
  }

  /**
   * Returns whether the theSuperClass is indeed a super class of the
   * theSubClassURI.
   * 
   * @param repositoryID
   * @param theSuperClassURI
   * @param theSubClassURI
   * @param direct
   * @return
   */
  public boolean isSuperClassOf(String repositoryID, String theSuperClassURI,
    String theSubClassURI, byte direct) throws GateOntologyException {
    loadRepositoryDetails(repositoryID);

    Resource r = getResource(theSubClassURI);
    String queryRep = "{<" + theSubClassURI + ">}";
    String queryRep1 = "<" + theSubClassURI + ">";
    if(r instanceof BNode) {
      queryRep = "{_:" + theSubClassURI + "}";
      queryRep1 = "_:" + theSubClassURI;
    }

    String query = "";
    query =
      "Select distinct SUPER FROM " + queryRep + " rdfs:subClassOf {SUPER}"
        + " WHERE SUPER!=" + queryRep1 + " MINUS "
        + " select distinct B FROM {B} owl:equivalentClass " + queryRep;

    QueryResultsTable iter = performQuery(query);
    List<String> list = new ArrayList<String>();
    for(int i = 0; i < iter.getRowCount(); i++) {
      list.add(iter.getValue(i, 0).toString());
    }

    if(direct == OConstants.DIRECT_CLOSURE) {
      Set<String> toDelete = new HashSet<String>();
      for(int i = 0; i < list.size(); i++) {
        String string = list.get(i);
        if(toDelete.contains(string)) continue;
        queryRep = "{<" + string + ">}";
        queryRep1 = "<" + string + ">";
        if(getResource(list.get(i)) instanceof BNode) {
          queryRep = "{_:" + string + "}";
          queryRep1 = "_:" + string;
        }

        query =
          "Select distinct SUPER FROM " + queryRep + " rdfs:subClassOf {SUPER}"
            + " WHERE SUPER!=" + queryRep1 + " MINUS "
            + " select distinct B FROM {B} owl:equivalentClass " + queryRep;

        QueryResultsTable qrt = performQuery(query);
        for(int j = 0; j < qrt.getRowCount(); j++) {
          toDelete.add(qrt.getValue(j, 0).toString());
        }
      }
      list.removeAll(toDelete);
    }

    // here if the list contains the uri of the super class
    // we return true;
    return list.contains(theSuperClassURI);
  }

  protected QueryResultsTable performQuery(String serqlQuery)
    throws GateOntologyException {
    String errorMsg =
      "Problem in retrieving data from the repository with query " + serqlQuery;
    try {
      return currentRepository.performTableQuery(QueryLanguage.SERQL,
        serqlQuery);
    }
    catch(AccessDeniedException ade) {
      throw new GateOntologyException(errorMsg, ade);
    }
    catch(IOException ioe) {
      throw new GateOntologyException(errorMsg, ioe);
    }
    catch(MalformedQueryException mqe) {
      throw new GateOntologyException(errorMsg, mqe);
    }
    catch(QueryEvaluationException qee) {
      throw new GateOntologyException(errorMsg, qee);
    }
  }

  /**
   * Returns whether the theSubClass is indeed a sub class of the
   * theSuperClassURI.
   * 
   * @param repositoryID
   * @param theSuperClassURI
   * @param theSubClassURI
   * @param direct
   * @return
   */
  public boolean isSubClassOf(String repositoryID, String theSuperClassURI,
    String theSubClassURI, byte direct) {
    return isSuperClassOf(repositoryID, theSuperClassURI, theSubClassURI,
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
  public Property getPropertyFromOntology(String repositoryID,
    String thePropertyURI) throws GateOntologyException {
    loadRepositoryDetails(repositoryID);
    // here we need to check which type of property it is
    return createPropertyObject(null, thePropertyURI);
  }

  /**
   * Checks whether the two classes defined as same in the ontology.
   * 
   * @param theClassURI1
   * @param theClassURI2
   * @return
   * @throws Exception
   */
  public boolean isEquivalentClassAs(String repositoryID, String theClassURI1,
    String theClassURI2) throws GateOntologyException {
    loadRepositoryDetails(repositoryID);
    Resource r1 = getResource(theClassURI1);
    String queryRep1 = "{<" + theClassURI1 + ">}";
    if(r1 instanceof BNode) {
      queryRep1 = "{_:" + theClassURI1 + "}";
    }

    Resource r2 = getResource(theClassURI2);
    String queryRep2 = "{<" + theClassURI2 + ">}";
    if(r2 instanceof BNode) {
      queryRep2 = "{_:" + theClassURI2 + "}";
    }

    String query =
      "SELECT * FROM " + queryRep1 + " owl:equivalentClass " + queryRep2;
    return performQuery(query).getRowCount() > 0;
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
  public void addAnnotationProperty(String repositoryID, String aPropertyURI)
    throws GateOntologyException {
    loadRepositoryDetails(repositoryID);
    addUUUStatement(aPropertyURI, RDF.TYPE, OWL.ANNOTATIONPROPERTY);
    currentEventsLog.addEvent(new OEvent(aPropertyURI, RDF.TYPE,
      OWL.ANNOTATIONPROPERTY, true));
  }

  /**
   * This method returns a set of all properties where the current resource has
   * been specified as one of the domain resources. Please note that this method
   * is different from the getAllSetProperties() method which returns a set of
   * properties set on the resource. For each property in the ontology, this
   * method checks if the current resource is valid domain. If so, the property
   * is said to be applicable, and otherwise not..
   * 
   * @return
   */
  public Property[] getPropertiesWithResourceAsDomain(String repositoryID,
    String theResourceURI) throws GateOntologyException {

    loadRepositoryDetails(repositoryID);
    List<Property> list = new ArrayList<Property>();

    HashSet<String> toCheck = new HashSet<String>();
    if(sail.hasStatement(getResource(theResourceURI), getURI(RDF.TYPE),
      getResource(OWL.CLASS))) {

      Resource r = getResource(theResourceURI);
      String queryRep = "{<" + theResourceURI + ">}";
      String queryRep1 = "<" + theResourceURI + ">";
      if(r instanceof BNode) {
        queryRep = "{_:" + theResourceURI + "}";
        queryRep1 = "_:" + theResourceURI;
      }

      String query =
        "Select distinct SUPER FROM " + queryRep + " rdfs:subClassOf {SUPER}"
          + " WHERE SUPER!=" + queryRep1 + " MINUS "
          + " select distinct B FROM {B} owl:equivalentClass " + queryRep;

      QueryResultsTable iter = performQuery(query);
      for(int i = 0; i < iter.getRowCount(); i++) {
        toCheck.add(iter.getValue(i, 0).toString());
      }

      toCheck.add(theResourceURI);
    }
    else if(sail.hasStatement(getResource(theResourceURI), getURI(RDF.TYPE),
      getResource(RDF.PROPERTY))) {

      Resource r = getResource(theResourceURI);
      String queryRep = "{<" + theResourceURI + ">}";
      String queryRep1 = "<" + theResourceURI + ">";
      if(r instanceof BNode) {
        queryRep = "{_:" + theResourceURI + "}";
        queryRep1 = "_:" + theResourceURI;
      }

      String query =
        "Select distinct SUPER FROM " + queryRep
          + " rdfs:subPropertyOf {SUPER}" + " WHERE SUPER!=" + queryRep1
          + " MINUS " + " select distinct B FROM {B} owl:equivalentProperty "
          + queryRep;

      QueryResultsTable iter = performQuery(query);
      for(int i = 0; i < iter.getRowCount(); i++) {
        toCheck.add(iter.getValue(i, 0).toString());
      }

      toCheck.add(theResourceURI);
    }
    else {
      // it is an instance
      String query =
        "Select DISTINCT B from {X} rdf:type {B} WHERE X=<" + theResourceURI
          + ">";

      QueryResultsTable iter2 = performQuery(query);
      for(int i = 0; i < iter2.getRowCount(); i++) {
        toCheck.add(iter2.getValue(i, 0).toString().intern());
      }
    }

    String query =
      "Select distinct X FROM {X} rdf:type {<" + OWL.ANNOTATIONPROPERTY + ">}";

    QueryResultsTable iter = performQuery(query);
    for(int i = 0; i < iter.getRowCount(); i++) {
      Value anAnnProp = iter.getValue(i, 0);
      list.add(new Property(OConstants.ANNOTATION_PROPERTY, anAnnProp
        .toString()));
    }

    boolean allowSystemStatements = this.returnSystemStatements;
    this.returnSystemStatements = true;
    Property[] props = listToPropertyArray(list);
    this.returnSystemStatements = allowSystemStatements;

    // now we obtain all datatype properties
    list = new ArrayList<Property>();
    query = "Select X FROM {X} rdf:type {<" + OWL.DATATYPEPROPERTY + ">}";

    iter = performQuery(query);
    for(int i = 0; i < iter.getRowCount(); i++) {
      Value anAnnProp = iter.getValue(i, 0);
      // for each property we obtain its domain and search for the
      // resourceURI in it
      query =
        "select distinct Y from {<" + anAnnProp.toString()
          + ">} rdfs:domain {Y}";
      QueryResultsTable iter1 = performQuery(query);
      Set<String> set = new HashSet<String>();
      for(int j = 0; j < iter1.getRowCount(); j++) {
        set.add(iter1.getValue(j, 0).toString().intern());
      }

      if(set.isEmpty()) {
        list.add(new Property(OConstants.DATATYPE_PROPERTY, anAnnProp
          .toString()));
      }

      set = new HashSet<String>(reduceToMostSpecificClasses(repositoryID, set));

      set.retainAll(toCheck);
      if(!set.isEmpty()) {
        list.add(new Property(OConstants.DATATYPE_PROPERTY, anAnnProp
          .toString()));
      }
    }

    query = "Select X FROM {X} rdf:type {<" + OWL.OBJECTPROPERTY + ">}";

    iter = performQuery(query);
    for(int i = 0; i < iter.getRowCount(); i++) {
      Value anAnnProp = iter.getValue(i, 0);
      // for each property we obtain its domain and search for the
      // resourceURI in it
      query =
        "select distinct Y from {<" + anAnnProp.toString()
          + ">} rdfs:domain {Y}";
      QueryResultsTable iter1 = performQuery(query);
      Set<String> set = new HashSet<String>();
      for(int j = 0; j < iter1.getRowCount(); j++) {
        set.add(iter1.getValue(j, 0).toString().intern());
      }

      set = new HashSet<String>(reduceToMostSpecificClasses(repositoryID, set));

      byte type = OConstants.OBJECT_PROPERTY;

      if(set.isEmpty()) {
        if(isSymmetricProperty(repositoryID, anAnnProp.toString().intern())) {
          type = OConstants.SYMMETRIC_PROPERTY;
        }
        else if(isTransitiveProperty(repositoryID, anAnnProp.toString()
          .intern())) {
          type = OConstants.TRANSITIVE_PROPERTY;
        }

        list.add(new Property(type, anAnnProp.toString()));
        continue;
      }
      set.retainAll(toCheck);
      if(!set.isEmpty()) {
        if(isSymmetricProperty(repositoryID, anAnnProp.toString().intern())) {
          type = OConstants.SYMMETRIC_PROPERTY;
        }
        else if(isTransitiveProperty(repositoryID, anAnnProp.toString()
          .intern())) {
          type = OConstants.TRANSITIVE_PROPERTY;
        }

        list.add(new Property(type, anAnnProp.toString()));
      }
    }
    Property[] props1 = listToPropertyArray(list);
    Property[] toProps = new Property[props.length + props1.length];
    for(int i = 0; i < props.length; i++) {
      toProps[i] = props[i];
    }

    for(int i = 0; i < props1.length; i++) {
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
  public Property[] getPropertiesWithResourceAsRange(String repositoryID,
    String theResourceURI) throws GateOntologyException {
    loadRepositoryDetails(repositoryID);
    List<Property> list = new ArrayList<Property>();

    HashSet<String> toCheck = new HashSet<String>();
    if(sail.hasStatement(getResource(theResourceURI), getURI(RDF.TYPE),
      getResource(OWL.CLASS))) {

      Resource r = getResource(theResourceURI);
      String queryRep = "{<" + theResourceURI + ">}";
      String queryRep1 = "<" + theResourceURI + ">";
      if(r instanceof BNode) {
        queryRep = "{_:" + theResourceURI + "}";
        queryRep1 = "_:" + theResourceURI;
      }

      String query =
        "Select distinct SUPER FROM " + queryRep + " rdfs:subClassOf {SUPER}"
          + " WHERE SUPER!=" + queryRep1 + " MINUS "
          + " select distinct B FROM {B} owl:equivalentClass " + queryRep;

      QueryResultsTable iter = performQuery(query);
      for(int i = 0; i < iter.getRowCount(); i++) {
        toCheck.add(iter.getValue(i, 0).toString());
      }

      toCheck.add(theResourceURI);
    }
    else if(sail.hasStatement(getResource(theResourceURI), getURI(RDF.TYPE),
      getResource(RDF.PROPERTY))) {

      Resource r = getResource(theResourceURI);
      String queryRep = "{<" + theResourceURI + ">}";
      String queryRep1 = "<" + theResourceURI + ">";
      if(r instanceof BNode) {
        queryRep = "{_:" + theResourceURI + "}";
        queryRep1 = "_:" + theResourceURI;
      }

      String query =
        "Select distinct SUPER FROM " + queryRep
          + " rdfs:subPropertyOf {SUPER}" + " WHERE SUPER!=" + queryRep1
          + " MINUS " + " select distinct B FROM {B} owl:equivalentProperty "
          + queryRep;

      QueryResultsTable iter = performQuery(query);
      for(int i = 0; i < iter.getRowCount(); i++) {
        toCheck.add(iter.getValue(i, 0).toString());
      }

      toCheck.add(theResourceURI);
    }
    else {
      // it is an instance
      String query =
        "Select DISTINCT B from {X} rdf:type {B} WHERE X=<" + theResourceURI
          + ">";

      QueryResultsTable iter2 = performQuery(query);
      for(int i = 0; i < iter2.getRowCount(); i++) {
        toCheck.add(iter2.getValue(i, 0).toString().intern());
      }
    }

    String query =
      "Select distinct X FROM {X} rdf:type {<" + OWL.ANNOTATIONPROPERTY + ">}";

    QueryResultsTable iter = performQuery(query);
    for(int i = 0; i < iter.getRowCount(); i++) {
      Value anAnnProp = iter.getValue(i, 0);
      list.add(new Property(OConstants.ANNOTATION_PROPERTY, anAnnProp
        .toString()));
    }

    boolean allowSystemStatements = this.returnSystemStatements;
    this.returnSystemStatements = true;
    Property[] props = listToPropertyArray(list);
    this.returnSystemStatements = allowSystemStatements;

    // now we obtain all datatype properties
    list = new ArrayList<Property>();
    query = "Select X FROM {X} rdf:type {<" + OWL.OBJECTPROPERTY + ">}";

    iter = performQuery(query);
    for(int i = 0; i < iter.getRowCount(); i++) {
      Value anAnnProp = iter.getValue(i, 0);
      // for each property we obtain its domain and search for the
      // resourceURI in it
      query =
        "select distinct Y from {<" + anAnnProp.toString()
          + ">} rdfs:range {Y}";
      QueryResultsTable iter1 = performQuery(query);
      Set<String> set = new HashSet<String>();
      for(int j = 0; j < iter1.getRowCount(); j++) {
        set.add(iter1.getValue(j, 0).toString().intern());
      }

      set = new HashSet<String>(reduceToMostSpecificClasses(repositoryID, set));

      byte type = OConstants.OBJECT_PROPERTY;

      if(set.isEmpty()) {
        if(isSymmetricProperty(repositoryID, anAnnProp.toString().intern())) {
          type = OConstants.SYMMETRIC_PROPERTY;
        }
        else if(isTransitiveProperty(repositoryID, anAnnProp.toString()
          .intern())) {
          type = OConstants.TRANSITIVE_PROPERTY;
        }

        list.add(new Property(type, anAnnProp.toString()));
      }

      set.retainAll(toCheck);
      if(!set.isEmpty()) {
        if(isSymmetricProperty(repositoryID, anAnnProp.toString().intern())) {
          type = OConstants.SYMMETRIC_PROPERTY;
        }
        else if(isTransitiveProperty(repositoryID, anAnnProp.toString()
          .intern())) {
          type = OConstants.TRANSITIVE_PROPERTY;
        }

        list.add(new Property(type, anAnnProp.toString()));
      }
    }
    Property[] props1 = listToPropertyArray(list);
    Property[] toProps = new Property[props.length + props1.length];
    for(int i = 0; i < props.length; i++) {
      toProps[i] = props[i];
    }

    for(int i = 0; i < props1.length; i++) {
      toProps[props.length + i] = props1[i];
    }

    return toProps;
  }

  /**
   * Gets the annotation properties set on the specified resource
   * 
   * @param repositoryID
   * @param theResourceURI
   * @return
   * @throws GateOntologyException
   */
  public Property[] getAnnotationProperties(String repositoryID,
    String theResourceURI) throws GateOntologyException {
    loadRepositoryDetails(repositoryID);
    List<Property> list = new ArrayList<Property>();

    String queryRep =
      getResource(theResourceURI) instanceof BNode ? "{_:" + theResourceURI
        + "}" : "{<" + theResourceURI + ">}";

    String query =
      "Select DISTINCT X FROM {X} rdf:type {<" + OWL.ANNOTATIONPROPERTY
        + ">} WHERE EXISTS (SELECT * FROM " + queryRep + " X {Z})";

    QueryResultsTable iter = performQuery(query);
    for(int i = 0; i < iter.getRowCount(); i++) {
      list.add(new Property(OConstants.ANNOTATION_PROPERTY, iter.getValue(i, 0)
        .toString()));
    }
    boolean allowSystemStatements = this.returnSystemStatements;
    this.returnSystemStatements = true;
    Property[] props = listToPropertyArray(list);
    this.returnSystemStatements = allowSystemStatements;
    return props;
  }

  /**
   * Gets the RDF properties set on the specified resource
   * 
   * @param repositoryID
   * @param theResourceURI
   * @return
   * @throws GateOntologyException
   */
  public Property[] getRDFProperties(String repositoryID, String theResourceURI)
    throws GateOntologyException {
    loadRepositoryDetails(repositoryID);
    List<Property> list = new ArrayList<Property>();
    String queryRep =
      getResource(theResourceURI) instanceof BNode ? "{_:" + theResourceURI
        + "}" : "{<" + theResourceURI + ">}";
    String query =
      "Select distinct X FROM {X} rdf:type {<" + RDF.PROPERTY
        + ">} WHERE EXISTS (SELECT * FROM " + queryRep + " X {Z})";

    QueryResultsTable iter = performQuery(query);
    for(int i = 0; i < iter.getRowCount(); i++) {
      Value anAnnProp = iter.getValue(i, 0);
      String propString = anAnnProp.toString();
      if(isAnnotationProperty(repositoryID, propString)
        || isDatatypeProperty(repositoryID, propString)
        || isObjectProperty(repositoryID, propString)
        || isTransitiveProperty(repositoryID, propString)
        || isSymmetricProperty(repositoryID, propString)) continue;
      list.add(new Property(OConstants.RDF_PROPERTY, anAnnProp.toString()));
    }
    return listToPropertyArray(list);
  }

  /**
   * Gets the datatype properties set on the specified resource
   * 
   * @param repositoryID
   * @param theResourceURI
   * @return
   * @throws GateOntologyException
   */
  public Property[] getDatatypeProperties(String repositoryID,
    String theResourceURI) throws GateOntologyException {
    loadRepositoryDetails(repositoryID);
    List<Property> list = new ArrayList<Property>();
    String queryRep =
      getResource(theResourceURI) instanceof BNode ? "{_:" + theResourceURI
        + "}" : "{<" + theResourceURI + ">}";
    String query =
      "Select distinct X FROM {X} rdf:type {<" + OWL.DATATYPEPROPERTY
        + ">} WHERE EXISTS (SELECT * FROM " + queryRep + " X {Z})";

    QueryResultsTable iter = performQuery(query);
    for(int i = 0; i < iter.getRowCount(); i++) {
      Value anAnnProp = iter.getValue(i, 0);
      list
        .add(new Property(OConstants.DATATYPE_PROPERTY, anAnnProp.toString()));
    }
    return listToPropertyArray(list);
  }

  /**
   * Gets the object properties set on the specified resource
   * 
   * @param repositoryID
   * @param theResourceURI
   * @return
   * @throws GateOntologyException
   */
  public Property[] getObjectProperties(String repositoryID,
    String theResourceURI) throws GateOntologyException {
    loadRepositoryDetails(repositoryID);
    List<Property> list = new ArrayList<Property>();
    String queryRep =
      getResource(theResourceURI) instanceof BNode ? "{_:" + theResourceURI
        + "}" : "{<" + theResourceURI + ">}";
    String query =
      "Select distinct X FROM {X} rdf:type {<" + OWL.OBJECTPROPERTY
        + ">} WHERE EXISTS (SELECT * FROM " + queryRep + " X {Z})";

    QueryResultsTable iter = performQuery(query);
    for(int i = 0; i < iter.getRowCount(); i++) {
      Value anAnnProp = iter.getValue(i, 0);
      list.add(new Property(OConstants.OBJECT_PROPERTY, anAnnProp.toString()));
    }
    return listToPropertyArray(list);
  }

  /**
   * Gets the transitive properties set on the specified resource
   * 
   * @param repositoryID
   * @param theResourceURI
   * @return
   * @throws GateOntologyException
   */
  public Property[] getTransitiveProperties(String repositoryID,
    String theResourceURI) throws GateOntologyException {
    loadRepositoryDetails(repositoryID);
    List<Property> list = new ArrayList<Property>();
    String queryRep =
      getResource(theResourceURI) instanceof BNode ? "{_:" + theResourceURI
        + "}" : "{<" + theResourceURI + ">}";
    String query =
      "Select distinct X FROM {X} rdf:type {<" + OWL.TRANSITIVEPROPERTY
        + ">} WHERE EXISTS (SELECT * FROM " + queryRep + " X {Z})";

    QueryResultsTable iter = performQuery(query);
    for(int i = 0; i < iter.getRowCount(); i++) {
      Value anAnnProp = iter.getValue(i, 0);
      list.add(new Property(OConstants.TRANSITIVE_PROPERTY, anAnnProp
        .toString()));
    }
    return listToPropertyArray(list);
  }

  /**
   * Gets the symmetric properties set on the specified resource
   * 
   * @param repositoryID
   * @param theResourceURI
   * @return
   * @throws GateOntologyException
   */
  public Property[] getSymmetricProperties(String repositoryID,
    String theResourceURI) throws GateOntologyException {
    loadRepositoryDetails(repositoryID);
    List<Property> list = new ArrayList<Property>();
    String queryRep =
      getResource(theResourceURI) instanceof BNode ? "{_:" + theResourceURI
        + "}" : "{<" + theResourceURI + ">}";
    String query =
      "Select distinct X FROM {X} rdf:type {<" + OWL.SYMMETRICPROPERTY
        + ">} WHERE EXISTS (SELECT * FROM " + queryRep + " X {Z})";

    QueryResultsTable iter = performQuery(query);
    for(int i = 0; i < iter.getRowCount(); i++) {
      Value anAnnProp = iter.getValue(i, 0);
      list
        .add(new Property(OConstants.SYMMETRIC_PROPERTY, anAnnProp.toString()));
    }
    return listToPropertyArray(list);
  }

  /**
   * returns if the given property is an Annotation property
   * 
   * @param aPropertyURI
   * @return Done
   */
  public boolean isAnnotationProperty(String repositoryID, String aPropertyURI)
    throws GateOntologyException {
    loadRepositoryDetails(repositoryID);
    String query =
      "Select * FROM {X} rdf:type {<" + OWL.ANNOTATIONPROPERTY + ">} WHERE X=<"
        + aPropertyURI + ">";
    return performQuery(query).getRowCount() > 0;
  }

  /**
   * Adds a new annotation property value and specifies the language.
   * 
   * @param theAnnotationProperty
   *          the annotation property
   * @param value
   *          the value containing some value
   * @return
   */
  public void addAnnotationPropertyValue(String repositoryID,
    String theResourceURI, String theAnnotationPropertyURI, String value,
    String language) throws GateOntologyException {
    // isAnnotationProperty also checks for the correct repository so no
    // need to give a call to it
    if(!isAnnotationProperty(repositoryID, theAnnotationPropertyURI)) { throw new GateOntologyException(
      "No annotation property found with the URI :" + theAnnotationPropertyURI); }
    addUULStatement(theResourceURI, theAnnotationPropertyURI, value, language);
    currentEventsLog.addEvent(new OEvent(theResourceURI,
      theAnnotationPropertyURI, value, true));
  }

  /**
   * Gets the list of annotation property values
   * 
   * @param repositoryID
   * @param theResourceURI
   * @param theAnnotationPropertyURI
   * @return
   */
  public PropertyValue[] getAnnotationPropertyValues(String repositoryID,
    String theResourceURI, String theAnnotationPropertyURI)
    throws GateOntologyException {
    // isAnnotationProperty also checks for the correct repository so no
    // need to give a call to it
    if(!isAnnotationProperty(repositoryID, theAnnotationPropertyURI)) { throw new GateOntologyException(
      "No annotation property found with the URI :" + theAnnotationPropertyURI); }

    Resource r2 = getResource(theResourceURI);
    String queryRep21 = "<" + theResourceURI + ">";
    if(r2 instanceof BNode) {
      queryRep21 = "_:" + theResourceURI;
    }

    List<PropertyValue> list = new ArrayList<PropertyValue>();
    String query =
      "Select DISTINCT Y from {X} <" + theAnnotationPropertyURI
        + "> {Y} WHERE X=" + queryRep21 + " AND isLiteral(Y)";

    QueryResultsTable iter = performQuery(query);
    for(int i = 0; i < iter.getRowCount(); i++) {
      PropertyValue pv;
      Literal literal = (Literal)iter.getValue(i, 0);
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
  public String getAnnotationPropertyValue(String repositoryID,
    String theResourceURI, String theAnnotationPropertyURI, String language)
    throws GateOntologyException {
    // isAnnotationProperty also checks for the correct repository so no
    // need to give a call to it
    if(!isAnnotationProperty(repositoryID, theAnnotationPropertyURI)) { throw new GateOntologyException(
      "No annotation property found with the URI :" + theAnnotationPropertyURI); }

    Resource r2 = getResource(theResourceURI);
    String queryRep21 = "<" + theResourceURI + ">";
    if(r2 instanceof BNode) {
      queryRep21 = "_:" + theResourceURI;
    }

    String query =
      "Select Y from {X} <" + theAnnotationPropertyURI + "> {Y} WHERE X="
        + queryRep21 + " AND isLiteral(Y) AND lang(Y) LIKE \"" + language
        + "\"";

    QueryResultsTable iter = performQuery(query);
    if(iter.getRowCount() > 0) {
      Literal literal = (Literal)iter.getValue(0, 0);
      return literal.getLabel();
    }
    return null;
  }

  /**
   * For the current resource, the method removes the given literal for the
   * given property.
   * 
   * @param theAnnotationProperty
   * @param literal
   */
  public void removeAnnotationPropertyValue(String repositoryID,
    String theResourceURI, String theAnnotationPropertyURI, String value,
    String language) throws GateOntologyException {
    // isAnnotationProperty also checks for the correct repository so no
    // need to give a call to it
    if(!isAnnotationProperty(repositoryID, theAnnotationPropertyURI)) { throw new GateOntologyException(
      "No annotation property found with the URI :" + theAnnotationPropertyURI); }
    startTransaction(null);
    removeUULStatement(theResourceURI, theAnnotationPropertyURI, value,
      language);
    endTransaction(null);
    currentEventsLog.addEvent(new OEvent(theResourceURI,
      theAnnotationPropertyURI, value, false));
  }

  /**
   * Removes all values for a named property.
   * 
   * @param theProperty
   *          the property
   */
  public void removeAnnotationPropertyValues(String repositoryID,
    String theResourceURI, String theAnnotationPropertyURI)
    throws GateOntologyException {
    try {
      // isAnnotationProperty also checks for the correct repository so
      // no
      // need to give a call to it
      if(!isAnnotationProperty(repositoryID, theAnnotationPropertyURI)) { throw new GateOntologyException(
        "No annotation property found with the URI :"
          + theAnnotationPropertyURI); }
      startTransaction(null);
      sail.removeStatements(getResource(theResourceURI),
        getURI(theAnnotationPropertyURI), null);
      endTransaction(null);
      currentEventsLog.addEvent(new OEvent(theResourceURI,
        theAnnotationPropertyURI, null, false));
    }
    catch(SailUpdateException e) {
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
  public void addRDFProperty(String repositoryID, String aPropertyURI,
    String[] domainClassesURIs, String[] rangeClassesTypes)
    throws GateOntologyException {
    loadRepositoryDetails(repositoryID);
    addUUUStatement(aPropertyURI, RDF.TYPE, RDF.PROPERTY);
    currentEventsLog.addEvent(new OEvent(aPropertyURI, RDF.TYPE, RDF.PROPERTY,
      true));

    if(domainClassesURIs != null) {
      for(int i = 0; i < domainClassesURIs.length; i++) {
        addUUUStatement(aPropertyURI, RDFS.DOMAIN, domainClassesURIs[i]);
        currentEventsLog.addEvent(new OEvent(aPropertyURI, RDFS.DOMAIN,
          domainClassesURIs[i], true));
      }
    }
    if(rangeClassesTypes != null) {
      for(int i = 0; i < rangeClassesTypes.length; i++) {
        addUUUStatement(aPropertyURI, RDFS.RANGE, rangeClassesTypes[i]);
        currentEventsLog.addEvent(new OEvent(aPropertyURI, RDFS.RANGE,
          rangeClassesTypes[i], true));
      }
    }
  }

  /**
   * returns if the given property is an RDF property
   * 
   * @param aPropertyURI
   * @return Done
   */
  public boolean isRDFProperty(String repositoryID, String aPropertyURI)
    throws GateOntologyException {
    loadRepositoryDetails(repositoryID);
    boolean found =
      isAnnotationProperty(repositoryID, aPropertyURI)
        || isDatatypeProperty(repositoryID, aPropertyURI)
        || isObjectProperty(repositoryID, aPropertyURI)
        || isTransitiveProperty(repositoryID, aPropertyURI)
        || isSymmetricProperty(repositoryID, aPropertyURI);
    if(!found) {
      String query =
        "Select * FROM {X} rdf:type {<" + RDF.PROPERTY + ">} WHERE X=<"
          + aPropertyURI + ">";
      return performQuery(query).getRowCount() > 0;
    }
    else {
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
  public void addDataTypeProperty(String repositoryID, String aPropertyURI,
    String[] domainClassesURIs, String dataTypeURI)
    throws GateOntologyException {
    loadRepositoryDetails(repositoryID);
    addUUUStatement(aPropertyURI, RDF.TYPE, OWL.DATATYPEPROPERTY);
    currentEventsLog.addEvent(new OEvent(aPropertyURI, RDF.TYPE,
      OWL.DATATYPEPROPERTY, true));
    addUUUStatement(aPropertyURI, RDFS.RANGE, dataTypeURI);
    currentEventsLog.addEvent(new OEvent(aPropertyURI, RDFS.RANGE, dataTypeURI,
      true));

    if(domainClassesURIs != null) {
      for(int i = 0; i < domainClassesURIs.length; i++) {
        addUUUStatement(aPropertyURI, RDFS.DOMAIN, domainClassesURIs[i]);
        currentEventsLog.addEvent(new OEvent(aPropertyURI, RDFS.DOMAIN,
          domainClassesURIs[i], true));
      }
    }
  }

  /**
   * Returns the datatype uri specified for the given datatype property.
   * 
   * @param repositoryID
   * @param theDatatypePropertyURI
   * @return
   * @throws GateOntologyException
   */
  public String getDatatype(String repositoryID, String theDatatypePropertyURI)
    throws GateOntologyException {
    // isAnnotationProperty also checks for the correct repository so no
    // need to give a call to it
    if(!isDatatypeProperty(repositoryID, theDatatypePropertyURI)) { throw new GateOntologyException(
      "Invalid DatatypeProperty :" + theDatatypePropertyURI); }

    String query =
      "Select Z from {<" + theDatatypePropertyURI + ">} rdfs:range" + " {Z}";
    QueryResultsTable iter = performQuery(query);

    String toReturn = null;
    if(iter.getRowCount() > 0) {
      toReturn = iter.getValue(0, 0).toString();
      if(OntologyUtilities.getDataType(toReturn) != null) { return toReturn; }
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
  public void addSymmetricProperty(String repositoryID, String aPropertyURI,
    String[] domainAndRangeClassesURIs) throws GateOntologyException {
    if(debug) print("addSymmetricProperty");
    loadRepositoryDetails(repositoryID);
    addUUUStatement(aPropertyURI, RDF.TYPE, OWL.SYMMETRICPROPERTY);
    currentEventsLog.addEvent(new OEvent(aPropertyURI, RDF.TYPE,
      OWL.SYMMETRICPROPERTY, true));
    if(domainAndRangeClassesURIs != null) {
      for(int i = 0; i < domainAndRangeClassesURIs.length; i++) {
        addUUUStatement(aPropertyURI, RDFS.DOMAIN, domainAndRangeClassesURIs[i]);
        currentEventsLog.addEvent(new OEvent(aPropertyURI, RDFS.DOMAIN,
          domainAndRangeClassesURIs[i], true));
        addUUUStatement(aPropertyURI, RDFS.RANGE, domainAndRangeClassesURIs[i]);
        currentEventsLog.addEvent(new OEvent(aPropertyURI, RDFS.RANGE,
          domainAndRangeClassesURIs[i], true));
      }
    }
  }

  /**
   * Checkes whether the two properties are Equivalent.
   * 
   * @param repositoryID
   * @param aPropertyURI
   * @return
   * @throws GateOntologyException
   */
  public boolean isEquivalentPropertyAs(String repositoryID,
    String aPropertyURI1, String aPropertyURI2) throws GateOntologyException {
    loadRepositoryDetails(repositoryID);
    String query =
      "Select * FROM {<" + aPropertyURI1 + ">} " + OWL.EQUIVALENTPROPERTY
        + " {<" + aPropertyURI2 + ">}";
    return performQuery(query).getRowCount() > 0;
  }

  /**
   * for the given property, the method returns all its super properties
   * 
   * @param aPropertyURI
   * @param direct
   * @return
   */
  public Property[] getSuperProperties(String repositoryID,
    String aPropertyURI, byte direct) throws GateOntologyException {
    loadRepositoryDetails(repositoryID);

    Resource r = getResource(aPropertyURI);
    String queryRep = "{<" + aPropertyURI + ">}";
    String queryRep1 = "<" + aPropertyURI + ">";
    if(r instanceof BNode) {
      queryRep = "{_:" + aPropertyURI + "}";
      queryRep1 = "_:" + aPropertyURI;
    }

    String query = "";
    query =
      "Select distinct SUPER FROM " + queryRep + " rdfs:subPropertyOf {SUPER}"
        + " WHERE SUPER!=" + queryRep1 + " MINUS "
        + " select distinct B FROM {B} owl:equivalentProperty " + queryRep;

    QueryResultsTable iter = performQuery(query);
    List<String> list = new ArrayList<String>();
    for(int i = 0; i < iter.getRowCount(); i++) {
      list.add(iter.getValue(i, 0).toString());
    }

    if(direct == OConstants.DIRECT_CLOSURE) {
      Set<String> toDelete = new HashSet<String>();
      for(int i = 0; i < list.size(); i++) {
        String string = list.get(i);
        if(toDelete.contains(string)) continue;
        queryRep = "{<" + string + ">}";
        queryRep1 = "<" + string + ">";
        if(getResource(list.get(i)) instanceof BNode) {
          queryRep = "{_:" + string + "}";
          queryRep1 = "_:" + string;
        }

        query =
          "Select distinct SUPER FROM " + queryRep
            + " rdfs:subPropertyOf {SUPER}" + " WHERE SUPER!=" + queryRep1
            + " MINUS " + " select distinct B FROM {B} owl:equivalentProperty "
            + queryRep;

        QueryResultsTable qrt = performQuery(query);
        for(int j = 0; j < qrt.getRowCount(); j++) {
          toDelete.add(qrt.getValue(j, 0).toString());
        }
      }
      list.removeAll(toDelete);
    }

    ArrayList<Property> properties = new ArrayList<Property>();
    for(int i = 0; i < list.size(); i++) {
      byte type = getPropertyType(null, list.get(i));
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
  public Property[] getSubProperties(String repositoryID, String aPropertyURI,
    byte direct) throws GateOntologyException {
    loadRepositoryDetails(repositoryID);
    Resource r = getResource(aPropertyURI);
    String queryRep = "{<" + aPropertyURI + ">}";
    String queryRep1 = "<" + aPropertyURI + ">";
    if(r instanceof BNode) {
      queryRep = "{_:" + aPropertyURI + "}";
      queryRep1 = "_:" + aPropertyURI;
    }

    String query = "";
    query =
      "Select distinct SUB FROM {SUB} rdfs:subPropertyOf " + queryRep
        + " WHERE SUB!=" + queryRep1 + " MINUS "
        + " select distinct B FROM {B} owl:equivalentProperty " + queryRep;

    QueryResultsTable iter = performQuery(query);
    List<String> list = new ArrayList<String>();
    for(int i = 0; i < iter.getRowCount(); i++) {
      list.add(iter.getValue(i, 0).toString());
    }

    if(direct == OConstants.DIRECT_CLOSURE) {
      Set<String> toDelete = new HashSet<String>();
      for(int i = 0; i < list.size(); i++) {
        String string = list.get(i);
        if(toDelete.contains(string)) continue;
        queryRep = "{<" + string + ">}";
        queryRep1 = "<" + string + ">";
        if(getResource(list.get(i)) instanceof BNode) {
          queryRep = "{_:" + string + "}";
          queryRep1 = "_:" + string;
        }

        query =
          "Select distinct SUB FROM {SUB} rdfs:subPropertyOf " + queryRep
            + " WHERE SUB!=" + queryRep1 + " MINUS "
            + " select distinct B FROM {B} owl:equivalentProperty " + queryRep;

        QueryResultsTable qrt = performQuery(query);
        for(int j = 0; j < qrt.getRowCount(); j++) {
          toDelete.add(qrt.getValue(j, 0).toString());
        }
      }
      list.removeAll(toDelete);
    }

    ArrayList<Property> properties = new ArrayList<Property>();
    for(int i = 0; i < list.size(); i++) {
      byte type = getPropertyType(null, list.get(i));
      properties.add(new Property(type, list.get(i)));
    }

    return listToPropertyArray(properties);
  }

  /**
   * Checkes whether the two properties have a super-sub relation.
   * 
   * @param repositoryID
   * @param aSuperPropertyURI
   * @param aSubPropertyURI
   * @param direct
   * @return
   * @throws GateOntologyException
   */
  public boolean isSuperPropertyOf(String repositoryID,
    String aSuperPropertyURI, String aSubPropertyURI, byte direct)
    throws GateOntologyException {
    loadRepositoryDetails(repositoryID);

    Resource r = getResource(aSubPropertyURI);
    String queryRep = "{<" + aSubPropertyURI + ">}";
    String queryRep1 = "<" + aSubPropertyURI + ">";
    if(r instanceof BNode) {
      queryRep = "{_:" + aSubPropertyURI + "}";
      queryRep1 = "_:" + aSubPropertyURI;
    }

    String query = "";
    query =
      "Select distinct SUPER FROM " + queryRep + " rdfs:subPropertyOf {SUPER}"
        + " WHERE SUPER!=" + queryRep1 + " MINUS "
        + " select distinct B FROM {B} owl:equivalentProperty " + queryRep;

    QueryResultsTable iter = performQuery(query);
    List<String> list = new ArrayList<String>();
    for(int i = 0; i < iter.getRowCount(); i++) {
      list.add(iter.getValue(i, 0).toString());
    }

    if(direct == OConstants.DIRECT_CLOSURE) {
      Set<String> toDelete = new HashSet<String>();
      for(int i = 0; i < list.size(); i++) {
        String string = list.get(i);
        if(toDelete.contains(string)) continue;
        queryRep = "{<" + string + ">}";
        queryRep1 = "<" + string + ">";
        if(getResource(list.get(i)) instanceof BNode) {
          queryRep = "{_:" + string + "}";
          queryRep1 = "_:" + string;
        }

        query =
          "Select distinct SUPER FROM " + queryRep
            + " rdfs:subPropertyOf {SUPER}" + " WHERE SUPER!=" + queryRep1
            + " MINUS " + " select distinct B FROM {B} owl:equivalentProperty "
            + queryRep;

        QueryResultsTable qrt = performQuery(query);
        for(int j = 0; j < qrt.getRowCount(); j++) {
          toDelete.add(qrt.getValue(j, 0).toString());
        }
      }
      list.removeAll(toDelete);
    }

    return list.contains(aSuperPropertyURI);
  }

  /**
   * Checkes whether the two properties have a super-sub relation.
   * 
   * @param repositoryID
   * @param aSuperPropertyURI
   * @param aSubPropertyURI
   * @param direct
   * @return
   * @throws GateOntologyException
   */
  public boolean isSubPropertyOf(String repositoryID, String aSuperPropertyURI,
    String aSubPropertyURI, byte direct) throws GateOntologyException {
    return isSuperPropertyOf(repositoryID, aSuperPropertyURI, aSubPropertyURI,
      direct);
  }

  /**
   * Given a class and instance URIs, the method checks if the latter is a
   * member of former. If the boolean parameter direct is set to true, the
   * method also checks if the literal is a direct instance of the class.
   * 
   * @param aSuperClassURI
   * @param individualURI
   * @return Done
   */
  public boolean hasIndividual(String repositoryID, String aSuperClassURI,
    String individualURI, byte direct) throws GateOntologyException {
    return this.hasIndividual(repositoryID, aSuperClassURI, individualURI,
      direct == OConstants.DIRECT_CLOSURE);
  }

  /**
   * Returns whether the individual1 is different from the individual2.
   * 
   * @param theInstanceURI1
   * @param theInstanceURI2
   * @return
   * @throws GateOntologyException
   */
  public boolean isDifferentIndividualFrom(String repositoryID,
    String theInstanceURI1, String theInstanceURI2)
    throws GateOntologyException {
    loadRepositoryDetails(repositoryID);
    String query =
      "Select * from {<" + theInstanceURI1 + ">} owl:differentFrom {<"
        + theInstanceURI2 + ">}";
    return performQuery(query).getRowCount() > 0;
  }

  /**
   * Checkes whether the two individuals are same.
   * 
   * @param repositoryID
   * @param individualURI1
   * @param invidualURI2
   * @return
   * @throws GateOntologyException
   */
  public boolean isSameIndividualAs(String repositoryID,
    String theInstanceURI1, String theInstanceURI2)
    throws GateOntologyException {
    loadRepositoryDetails(repositoryID);
    String query =
      "Select * from {<" + theInstanceURI1 + ">} owl:sameAs {<"
        + theInstanceURI2 + ">}";
    return performQuery(query).getRowCount() > 0;
  }

  // *************
  // Instances and properties
  // **************
  /**
   * adds the RDF Property value on the specified instance
   * 
   * @param repositoryID
   * @param anInstanceURI
   * @param anRDFPropertyURI
   * @param aResourceURI
   * @throws InvalidValueException
   */
  public void addRDFPropertyValue(String repositoryID, String anInstanceURI,
    String anRDFPropertyURI, String aResourceURI) throws GateOntologyException {
    loadRepositoryDetails(repositoryID);
    addUUUStatement(anInstanceURI, anRDFPropertyURI, aResourceURI);
    currentEventsLog.addEvent(new OEvent(anInstanceURI, anRDFPropertyURI,
      aResourceURI, true));
  }

  /**
   * Removes the specified RDF Property Value
   * 
   * @param repositoryID
   * @param anInstanceURI
   * @param anRDFPropertyURI
   * @param aResourceURI
   */
  public void removeRDFPropertyValue(String repositoryID, String anInstanceURI,
    String anRDFPropertyURI, String aResourceURI) throws GateOntologyException {
    loadRepositoryDetails(repositoryID);
    startTransaction(null);
    removeUUUStatement(anInstanceURI, anRDFPropertyURI, aResourceURI);
    endTransaction(null);
    currentEventsLog.addEvent(new OEvent(anInstanceURI, anRDFPropertyURI,
      aResourceURI, false));
  }

  /**
   * gets the rdf property values for the specified instance.
   * 
   * @param repositoryID
   * @param anInstanceURI
   * @param anRDFPropertyURI
   * @return resource URIs
   */
  public ResourceInfo[] getRDFPropertyValues(String repositoryID,
    String anInstanceURI, String anRDFPropertyURI) throws GateOntologyException {
    loadRepositoryDetails(repositoryID);
    Resource r = getResource(anInstanceURI);
    String queryRep2 = "<" + anInstanceURI + ">";
    if(r instanceof BNode) {
      queryRep2 = "_:" + anInstanceURI;
    }

    List<String> list = new ArrayList<String>();
    String query =
      "Select DISTINCT Y from {X} <" + anRDFPropertyURI + "> {Y} WHERE X="
        + queryRep2;

    QueryResultsTable iter = performQuery(query);
    for(int i = 0; i < iter.getRowCount(); i++) {
      list.add(iter.getValue(i, 0).toString());
    }
    return listToResourceInfoArray(list);
  }

  /**
   * Removes all the RDF Property values from the given instance.
   * 
   * @param repositoryID
   * @param anInstanceURI
   * @param anRDFPropertyURI
   */
  public void removeRDFPropertyValues(String repositoryID,
    String anInstanceURI, String anRDFPropertyURI) throws GateOntologyException {
    loadRepositoryDetails(repositoryID);
    try {
      startTransaction(repositoryID);
      sail.removeStatements(getResource(anInstanceURI),
        getURI(anRDFPropertyURI), null);
      endTransaction(repositoryID);
      currentEventsLog.addEvent(new OEvent(anInstanceURI, anRDFPropertyURI,
        null, true));
    }
    catch(SailUpdateException sue) {
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
   * @param repositoryID
   * @param anInstanceURI
   * @param aDatatypePropertyURI
   * @param datatypeURI
   * @param value
   * @throws InvalidValueException
   */
  public void addDatatypePropertyValue(String repositoryID,
    String anInstanceURI, String aDatatypePropertyURI, String datatypeURI,
    String value) throws GateOntologyException {
    if(!isDatatypeProperty(repositoryID, aDatatypePropertyURI)) { throw new GateOntologyException(
      "No datatype property exists with URI :" + aDatatypePropertyURI); }
    addUUDStatement(repositoryID, anInstanceURI, aDatatypePropertyURI, value,
      datatypeURI);
    currentEventsLog.addEvent(new OEvent(anInstanceURI, aDatatypePropertyURI,
      value, datatypeURI, true));
  }

  /**
   * Removes the provided value for the given instance.
   * 
   * @param repositoryID
   * @param anInstanceURI
   * @param aDatatypePropertyURI
   * @param datatypeURI
   * @param value
   */
  public void removeDatatypePropertyValue(String repositoryID,
    String anInstanceURI, String aDatatypePropertyURI, String datatypeURI,
    String value) throws GateOntologyException {
    if(!isDatatypeProperty(repositoryID, aDatatypePropertyURI)) { throw new GateOntologyException(
      "No datatype property exists with URI :" + aDatatypePropertyURI); }
    startTransaction(null);
    removeUUDStatement(repositoryID, anInstanceURI, aDatatypePropertyURI,
      value, datatypeURI);
    endTransaction(null);
    currentEventsLog.addEvent(new OEvent(anInstanceURI, aDatatypePropertyURI,
      value, datatypeURI, false));
  }

  /**
   * Gets a list of values for the given Property.
   * 
   * @param repositoryID
   * @param anInstanceURI
   * @param aDatatypePropertyURI
   * @return
   */
  public PropertyValue[] getDatatypePropertyValues(String repositoryID,
    String anInstanceURI, String aDatatypePropertyURI)
    throws GateOntologyException {
    if(!isDatatypeProperty(repositoryID, aDatatypePropertyURI)) { throw new GateOntologyException(
      "No datatype property exists with URI :" + aDatatypePropertyURI); }

    Resource r2 = getResource(anInstanceURI);
    String queryRep21 = "<" + anInstanceURI + ">";
    if(r2 instanceof BNode) {
      queryRep21 = "_:" + anInstanceURI;
    }

    List<PropertyValue> list = new ArrayList<PropertyValue>();
    String query =
      "Select DISTINCT Y from {X} <" + aDatatypePropertyURI + "> {Y} WHERE X="
        + queryRep21 + " AND isLiteral(Y)";

    QueryResultsTable iter = performQuery(query);
    for(int i = 0; i < iter.getRowCount(); i++) {
      PropertyValue pv;
      Literal literal = (Literal)iter.getValue(i, 0);
      String datatype = "http://www.w3.org/2001/XMLSchema#string";
      if(literal.getDatatype() != null) {
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
   * @param repositoryID
   * @param anInstanceURI
   * @param aDatatypePropertyURI
   */
  public void removeDatatypePropertyValues(String repositoryID,
    String anInstanceURI, String aDatatypePropertyURI)
    throws GateOntologyException {
    if(!isDatatypeProperty(repositoryID, aDatatypePropertyURI)) { throw new GateOntologyException(
      "No datatype property exists with URI :" + aDatatypePropertyURI); }
    startTransaction(null);
    removeUUUStatement(anInstanceURI, aDatatypePropertyURI, null);
    endTransaction(null);
    currentEventsLog.addEvent(new OEvent(anInstanceURI, aDatatypePropertyURI,
      null, false));
  }

  // ******************
  // Object, Symmetric and Transitive Properties
  // *****************
  /**
   * Adds the value for the given property (Object, Symmetric and Transitive).
   * 
   * @param repositoryID
   * @param sourceInstanceURI
   * @param anObjectPropertyURI
   * @param theValueInstanceURI
   * @throws InvalidValueException
   */
  public void addObjectPropertyValue(String repositoryID,
    String sourceInstanceURI, String anObjectPropertyURI,
    String theValueInstanceURI) throws GateOntologyException {
    loadRepositoryDetails(repositoryID);
    if(!sail.hasStatement(getResource(anObjectPropertyURI), getURI(RDF.TYPE),
      getResource(OWL.OBJECTPROPERTY))) { throw new GateOntologyException(
      "No object property exists with URI :" + anObjectPropertyURI); }
    addUUUStatement(sourceInstanceURI, anObjectPropertyURI, theValueInstanceURI);
    currentEventsLog.addEvent(new OEvent(sourceInstanceURI,
      anObjectPropertyURI, theValueInstanceURI, true));
  }

  /**
   * Remove the provided value for the given property (Object, Symmetric and
   * Transitive).
   * 
   * @param repositoryID
   * @param sourceInstanceURI
   * @param anObjectPropertyURI
   * @param theValueInstanceURI
   * @return
   */
  public void removeObjectPropertyValue(String repositoryID,
    String sourceInstanceURI, String anObjectPropertyURI,
    String theValueInstanceURI) throws GateOntologyException {
    if(!isObjectProperty(repositoryID, anObjectPropertyURI)) { throw new GateOntologyException(
      "No object property exists with URI :" + anObjectPropertyURI); }

    startTransaction(null);
    removeUUUStatement(sourceInstanceURI, anObjectPropertyURI,
      theValueInstanceURI);
    endTransaction(null);
    currentEventsLog.addEvent(new OEvent(sourceInstanceURI,
      anObjectPropertyURI, theValueInstanceURI, false));
  }

  /**
   * Gets a list of values for the given Property (Object, Symmetric and
   * Transitive).
   * 
   * @param repositoryID
   * @param sourceInstanceURI
   * @param anObjectPropertyURI
   * @return
   */
  public String[] getObjectPropertyValues(String repositoryID,
    String sourceInstanceURI, String anObjectPropertyURI)
    throws GateOntologyException {
    if(!isObjectProperty(repositoryID, anObjectPropertyURI)
      && !isTransitiveProperty(repositoryID, anObjectPropertyURI)
      && !isSymmetricProperty(repositoryID, anObjectPropertyURI)) { throw new GateOntologyException(
      "No object/transitive/symmetric property exists with URI :"
        + anObjectPropertyURI); }

    Resource r = getResource(sourceInstanceURI);
    String queryRep2 = "<" + sourceInstanceURI + ">";
    if(r instanceof BNode) {
      queryRep2 = "_:" + sourceInstanceURI;
    }

    List<String> list = new ArrayList<String>();
    String query =
      "Select DISTINCT Y from {X} <" + anObjectPropertyURI + "> {Y} WHERE X="
        + queryRep2;

    QueryResultsTable iter = performQuery(query);
    for(int i = 0; i < iter.getRowCount(); i++) {
      list.add(iter.getValue(i, 0).toString());
    }
    return listToArray(list);
  }

  /**
   * Removes all property values set for the current property (Object, Symmetric
   * and Transitive).
   * 
   * @param repositoryID
   * @param sourceInstanceURI
   * @param anObjectPropertyURI
   */
  public void removeObjectPropertyValues(String repositoryID,
    String sourceInstanceURI, String anObjectPropertyURI)
    throws GateOntologyException {
    if(!isObjectProperty(repositoryID, anObjectPropertyURI)) { throw new GateOntologyException(
      "No object property exists with URI :" + anObjectPropertyURI); }
    startTransaction(null);
    removeUUUStatement(sourceInstanceURI, anObjectPropertyURI, null);
    endTransaction(null);
    currentEventsLog.addEvent(new OEvent(sourceInstanceURI,
      anObjectPropertyURI, null, false));
  }

  /** This should be called by axis after each call to the operator?* */
  /*
   * public void destroy() { // we don't want to do anything here // because we
   * want to keep alive all our resources // until the logout method is called }
   */

  //****************************************************************************
  // user management methods
  //****************************************************************************
  /**
   * Call to this method is necessary in order to login in to the Sesame server.
   * Unless user is registered with Sesame server, he/she cannot have write or
   * modify access to any of the repositories (unless given write access to
   * world users) available on the server. However, unregistered users are and
   * will be allowed to have read access on all repositories.
   * 
   * @param username
   * @param password
   * @return
   */
  public boolean login(String username, String password) {
    if(debug) print("login");
    try {
      service.login(username, password);
    }
    catch(AccessDeniedException ade) {
      return false;
    }
    int id = getUserID(username, password);
    if(id == -1) { return false; }
    return true;
  }

  /**
   * End the session by logging out
   */
  public void logout(String repositoryID) throws GateOntologyException {
    if(debug) print("logout");
    // mapToRepositoryDetails.remove(repositoryID);
    service.logout();
    if(shutDownOnLogout) {
      service.shutDown();
    }
    currentRepository = null;
    sail = null;
    // System.gc();
  }

  //****************************************************************************
  // repository methods
  //****************************************************************************
  /**
   * Find out the list of repository list
   */
  public String[] getRepositoryList() throws GateOntologyException {
    if(debug) print("getRepositoryList");
    RepositoryList rList = service.getRepositoryList();
    List repositories = rList.getRepositories();
    if(repositories == null) return new String[0];
    String[] reps = new String[repositories.size()];
    for(int i = 0; i < reps.length; i++) {
      RepositoryInfo rInfo = (RepositoryInfo)repositories.get(i);
      reps[i] = rInfo.getRepositoryId();
    }
    return reps;
  }

  /**
   * sets the provided repository as a current repository
   */
  public void setCurrentRepositoryID(String repositoryID)
    throws GateOntologyException {
    if(debug) print("setCurrentRepository with ID " + repositoryID);
    if(sail != null && sail.transactionStarted()) {
      // we need to commit all changes
      sail.commitTransaction();
    }
    try {
      LocalRepository lr = (LocalRepository)service.getRepository(repositoryID);

      if(lr == null) { throw new GateOntologyException("Repository ID "
        + repositoryID + " does not exist!"); }
      if(!(lr.getSail() instanceof OWLIMSchemaRepository)) { throw new GateOntologyException(
        "Repository ID " + repositoryID + "is not an OWLIMSchemaRepository!"); }
      currentRepository = lr;
      sail = (OWLIMSchemaRepository)lr.getSail();

      RepositoryDetails rd = mapToRepositoryDetails.get(repositoryID);
      if(rd == null) {
        rd = new RepositoryDetails();
        rd.repository = currentRepository;
        if(currentEventsLog == null)
          currentEventsLog = new OntologyEventsLog();
        rd.eventsLog = currentEventsLog;
        rd.sail = sail;
        rd.ontologyUrl = ontologyUrl;
        rd.returnSystemStatements = returnSystemStatements;

        mapToRepositoryDetails.put(repositoryID, rd);
      }
      else {
        ontologyUrl = rd.ontologyUrl;
      }
    }
    catch(ConfigurationException ce) {
      throw new GateOntologyException(ce);
    }
    catch(UnknownRepositoryException ure) {
      throw new GateOntologyException(ure);
    }
  }

  /**
   * This method returns the ID of current repository
   */
  public String getCurrentRepositoryID() {
    if(debug) print("getCurrentRepository");
    return currentRepository.getRepositoryId();
  }

  /**
   * Users are allowed to create new repositories and add data into it. In order
   * to create new repository, they don’t necessarily need to be registered. The
   * username and password parameters are used to assign access rights over the
   * repository. Apart from the owner of repository, administrator also gets the
   * full rights over the repository. All other users are given read access.
   * User is also asked to provide a URL, or the RDF data from the ontology.
   * Incase if the url is null or an empty string, an empty graph is created
   * allowing users to add more data into it. Otherwise the graph is populated
   * with the given ontology. The user is also asked to provide the RDF format
   * information (i.e. ''N3'', ''TURTLE'', ''NTRIPLES'' or ''RDFXML'') .
   * 
   * @param repositoryID
   * @param username
   * @param password
   * @param ontoData
   * @param baseURI
   * @param format
   * @param persist
   * @return
   */
  public String createRepository(String repositoryID, String username,
    String password, String ontoData, String baseURI, byte format,
    String absolutePersistLocation, boolean persist,
    boolean returnSystemStatements) throws GateOntologyException {
    if(debug) print("createRepository");
    // if(absolutePersistLocation == null) {
    // try {
    // absolutePersistLocation = new
    // File(gosHomeURL.toURI()).getAbsolutePath();
    // }
    // catch(URISyntaxException e) {
    // throw new GateOntologyException(
    // "Cannot construct persistence location " + "from gosHomeURL", e);
    // }
    // }
    // check if user exists
    if(password == null) password = "";
    createNewUser(username, password);
    this.returnSystemStatements = returnSystemStatements;
    boolean found =
      setRepository(repositoryID, ontoData, true, baseURI, format,
        absolutePersistLocation, persist);
    if(found) { return repositoryID; }
    // we create a new repository
    RepositoryConfig repConfig =
      createNewRepository(repositoryID, ontoData, true, baseURI, persist,
        absolutePersistLocation, username, password, format, false);

    addOntologyData(repositoryID, ontoData, true, baseURI, format);
    service.getSystemConfig().addRepositoryConfig(repConfig);
    if(persist) saveConfiguration();
    if(debug) System.out.println("Repository created!");

    loadRepositoryDetails(repositoryID);
    return repositoryID;
  }

  /**
   * Users are allowed to create new repositories and add data into it. In order
   * to create new repository, they don’t necessarily need to be registered. The
   * username and password parameters are used to assign access rights over the
   * repository. Apart from the owner of repository, administrator also gets the
   * full rights over the repository. All other users are given read access.
   * User is also asked to provide a URL for the ontology. Incase if the url is
   * null or an empty string, an empty graph is created allowing user to add
   * more data into it. Otherwise the graph is populated with the given ontology
   * URL. The user is also asked to provide the RDF format information (i.e.
   * ''N3'', ''TURTLE'', ''NTRIPLES'' or ''RDFXML'') .
   * 
   * @param repositoryID
   * @param username
   * @param password
   * @param ontoFileUrl
   * @param baseURI
   * @param format
   * @param persist
   * @return
   */
  public String createRepositoryFromUrl(String repositoryID, String username,
    String password, String ontoFileUrl, String baseURI, byte format,
    String absolutePersistLocation, boolean persist,
    boolean returnSystemStatements) throws GateOntologyException {
    if(debug) print("createRepository");
    // if(absolutePersistLocation == null) {
    // try {
    // absolutePersistLocation = new
    // File(gosHomeURL.toURI()).getAbsolutePath();
    // }
    // catch(URISyntaxException e) {
    // throw new GateOntologyException(
    // "Cannot construct persistence location " + "from gosHomeURL", e);
    // }
    // }
    // check if user exists
    if(password == null) password = "";
    createNewUser(username, password);
    this.returnSystemStatements = returnSystemStatements;
    boolean found =
      setRepository(repositoryID, ontoFileUrl, false, baseURI, format,
        absolutePersistLocation, persist);
    if(found) { return repositoryID; }
    RepositoryConfig repConfig =
      createNewRepository(repositoryID, ontoFileUrl, false, baseURI, persist,
        absolutePersistLocation, username, password, format, false);
    addOntologyData(repositoryID, ontoFileUrl, false, baseURI, format);
    service.getSystemConfig().addRepositoryConfig(repConfig);
    if(persist) saveConfiguration();
    if(debug) System.out.println("Repository created!");
    this.ontologyUrl = ontoFileUrl;

    loadRepositoryDetails(repositoryID);
    return repositoryID;
  }

  /**
   * Removes the repository with given ID
   * 
   * @param repositoryID
   * @return
   */
  public void removeRepository(String repositoryID, boolean persist)
    throws GateOntologyException {
    loadRepositoryDetails(repositoryID);
    startTransaction(null);
    if(currentRepository == null) return;
    try {
      sail.clearRepository();
    }
    catch(SailUpdateException sue) {
      throw new GateOntologyException("Error while removing repository:"
        + repositoryID, sue);
    }
    service.getSystemConfig().removeRepository(
      currentRepository.getRepositoryId());
    if(persist) saveConfiguration();
    endTransaction(null);
    mapToRepositoryDetails.remove(repositoryID);
  }

  // *******************************************************************
  // *************************** Ontology Methods **********************
  // *******************************************************************
  /**
   * The method removes all data from the available graph.
   */
  public void cleanOntology(String repositoryID) throws GateOntologyException {
    if(debug) print("cleanOntology");
    loadRepositoryDetails(repositoryID);
    if(currentRepository == null) return;
    RepositoryConfig rc =
      service.getSystemConfig().getRepositoryConfig(
        currentRepository.getRepositoryId());
    if(rc == null) return;
    startTransaction(null);
    try {
      sail.clearRepository();
    }
    catch(SailUpdateException sue) {
      throw new GateOntologyException("error while cleaning repository:"
        + repositoryID, sue);
    }

    endTransaction(null);
    currentEventsLog.addEvent(new OEvent("*", "*", "*", false));
  }

  /**
   * This method is useful to export results. Given one of the four RDFFormat
   * parameters (i.e. ''N3'', ''TURTLE'', ''NTRIPLES'' or ''RDFXML'') , the
   * method returns an equivalent string representation of the data in the
   * supplied format.
   * 
   * @param format
   * @return
   */
  public String getOntologyData(String repositoryID, byte format)
    throws GateOntologyException {
    if(debug) print("getOntologyData");
    loadRepositoryDetails(repositoryID);
    // before we extract anything
    // lets commit all our changes
    if(sail.transactionStarted()) sail.commitTransaction();
    try {
      InputStream stream =
        currentRepository.extractRDF(getRDFFormat(format), true, true, true,
          true);
      BufferedReader br = new BufferedReader(new InputStreamReader(stream));
      String line = br.readLine();
      StringBuffer sb = new StringBuffer(1028);
      while(line != null) {
        sb.append(line + "\n");
        line = br.readLine();
      }
      return sb.toString();
    }
    catch(AccessDeniedException ade) {
      throw new GateOntologyException("error while obtaining ontology data",
        ade);
    }
    catch(IOException ioe) {
      throw new GateOntologyException("error while obtaining ontology data",
        ioe);
    }
  }

  /**
   * The method allows adding version information to the repository.
   * 
   * @param versionInfo
   */
  public void setVersion(String repositoryID, String versionInfo)
    throws GateOntologyException {
    loadRepositoryDetails(repositoryID);
    if(debug) print("setVersion");
    addUULStatement(this.ontologyUrl, OWL.VERSIONINFO, versionInfo, null);
    currentEventsLog.addEvent(new OEvent(this.ontologyUrl.toString(),
      OWL.VERSIONINFO, versionInfo, true));
  }

  /**
   * The method returns the version information of the repository.
   * 
   * @return
   */
  public String getVersion(String repositoryID) throws GateOntologyException {
    if(debug) print("getVersion");
    loadRepositoryDetails(repositoryID);
    StatementIterator iter =
      sail.getStatements(getResource(this.ontologyUrl),
        getURI(OWL.VERSIONINFO), null);
    while(iter.hasNext()) {
      Statement stmt = (Statement)iter.next();
      return stmt.getObject().toString();
    }
    return null;
  }

  // *******************************************************************
  // class methods
  // *******************************************************************
  /**
   * The method allows adding a class to repository.
   * 
   * @param classURI
   */
  public void addClass(String repositoryID, String classURI, byte classType)
    throws GateOntologyException {
    loadRepositoryDetails(repositoryID);
    if(debug) print("addClass");
    switch(classType){
      case OConstants.OWL_CLASS:
        addUUUStatement(classURI, RDF.TYPE, OWL.CLASS);
        currentEventsLog.addEvent(new OEvent(classURI, RDF.TYPE, OWL.CLASS,
          true));
        return;
      default:
        addUUUStatement(classURI, RDF.TYPE, OWL.RESTRICTION);
        currentEventsLog.addEvent(new OEvent(classURI, RDF.TYPE,
          OWL.RESTRICTION, true));
        return;
    }
  }

  /**
   * Given a class to delete, it removes it from the repository.
   * 
   * @param repositoryID
   * @param classURI
   * @param removeSubTree
   *          - if set to true, removes all its subclasses and instances as
   *          well, otherwise shifts all subclasses and instances to its parent
   *          node
   * @return a list of other resources, which got removed as a result of this
   *         deletion
   */
  public String[] removeClass(String repositoryID, String classURI,
    boolean removeSubTree) throws GateOntologyException {
    loadRepositoryDetails(repositoryID);
    if(debug) print("removeClass");
    ResourceInfo[] superClasses =
      getSuperClasses(null, classURI, OConstants.DIRECT_CLOSURE);

    List<String> deletedResources = new ArrayList<String>();
    if(removeUUUStatement(classURI, RDF.TYPE, null) == 0) {
      throw new GateOntologyException(classURI + " is not an explicit resource");
    }
    else {
      currentEventsLog.addEvent(new OEvent(classURI, RDF.TYPE, null, false));
      deletedResources.add(classURI);
    }

    try {
      startTransaction(null);
      sail.removeStatements(getResource(classURI), getURI(RDFS.SUBCLASSOF),
        null);
      endTransaction(null);
      currentEventsLog.addEvent(new OEvent(classURI, RDFS.SUBCLASSOF, null,
        false));
    }
    catch(SailUpdateException sue) {
      throw new GateOntologyException("error while removing a class:"
        + classURI, sue);
    }

    // this should happen only if removeSubTree is set to true
    if(removeSubTree) {
      ResourceInfo[] subClasses =
        getSubClasses(null, classURI, OConstants.DIRECT_CLOSURE);
      for(int i = 0; i < subClasses.length; i++) {
        String[] removedResources =
          removeClass(null, subClasses[i].getUri(), true);
        deletedResources.addAll(Arrays.asList(removedResources));
      }
      String[] individuals =
        getIndividuals(null, classURI, OConstants.DIRECT_CLOSURE);
      for(int i = 0; i < individuals.length; i++) {
        String[] removedResources = removeIndividual(null, individuals[i]);
        deletedResources.addAll(Arrays.asList(removedResources));
      }
    }
    else {
      ResourceInfo[] subClasses =
        getSubClasses(null, classURI, OConstants.DIRECT_CLOSURE);
      for(int i = 0; i < subClasses.length; i++) {
        removeSubClass(repositoryID, classURI, subClasses[i].getUri());
        for(int j = 0; j < superClasses.length; j++) {
          addSubClass(repositoryID, superClasses[j].getUri(), subClasses[i]
            .getUri());
        }
      }
      String[] individuals =
        getIndividuals(null, classURI, OConstants.DIRECT_CLOSURE);
      for(int i = 0; i < individuals.length; i++) {
        removeUUUStatement(individuals[i], RDF.TYPE, classURI);
        currentEventsLog.addEvent(new OEvent(individuals[i], RDF.TYPE,
          classURI, false));
        for(int j = 0; j < superClasses.length; j++) {
          addUUUStatement(individuals[i], RDF.TYPE, superClasses[j].getUri());
          currentEventsLog.addEvent(new OEvent(individuals[i], RDF.TYPE,
            superClasses[j].getUri(), true));
        }
      }
    }

    startTransaction(null);
    StatementIterator iter =
      sail.getStatements(null, getURI(RDFS.DOMAIN), getResource(classURI));
    endTransaction(null);
    while(iter.hasNext()) {
      Statement stmt = iter.next();
      Resource resource = stmt.getSubject();
      String[] removedResources =
        removePropertyFromOntology(null, resource.toString(), removeSubTree);
      deletedResources.addAll(Arrays.asList(removedResources));
    }
    startTransaction(null);
    iter = sail.getStatements(null, getURI(RDFS.RANGE), getResource(classURI));
    endTransaction(null);
    while(iter.hasNext()) {
      Statement stmt = iter.next();
      Resource resource = stmt.getSubject();
      String[] removedResources =
        removePropertyFromOntology(null, resource.toString(), removeSubTree);
      deletedResources.addAll(Arrays.asList(removedResources));
    }

    // finaly remove all statements concerning this resource
    try {
      startTransaction(null);
      sail.removeStatements(getResource(classURI), null, null);
      currentEventsLog.addEvent(new OEvent(classURI, null, null, false));
      if(!(getResource(classURI) instanceof BNode)) {
        sail.removeStatements(null, getURI(classURI), null);
        currentEventsLog.addEvent(new OEvent(null, classURI, null, false));
      }
      sail.removeStatements(null, null, getResource(classURI));
      currentEventsLog.addEvent(new OEvent(null, null, classURI, false));

      endTransaction(null);
    }
    catch(SailUpdateException sue) {
      throw new GateOntologyException("error while removing a class:"
        + classURI, sue);
    }

    Collections.reverse(deletedResources);
    return listToArray(deletedResources);
  }

  /**
   * The method returns if the current repository has a class with URI that
   * matches with the class parameter.
   * 
   * @return
   */
  public boolean hasClass(String repositoryID, String classURI)
    throws GateOntologyException {
    if(debug) print("hasClass");
    loadRepositoryDetails(repositoryID);
    return sail.isClass(getResource(classURI));
  }

  /**
   * if top set to true, the method returns only the top classes (i.e. classes
   * with no super class). Otherwise it returns all classes available in
   * repository.
   * 
   * @param top
   * @return
   */
  public ResourceInfo[] getClasses(String repositoryID, boolean top)
    throws GateOntologyException {
    if(debug) print("getClasses");
    loadRepositoryDetails(repositoryID);

    String query = "";
    if(top) {
      query =
        "((Select DISTINCT A FROM "
          + "{A} rdf:type {<http://www.w3.org/2002/07/owl#Class>} UNION SELECT DISTINCT B from {B} rdf:type {<http://www.w3.org/2002/07/owl#Restriction> })"
          + " MINUS "
          + "Select P FROM {P} rdfs:subClassOf {Q} "
          + "WHERE "
          + "P!=Q AND "
          + "P!=ALL(SELECT D FROM {D} owl:equivalentClass {Q})"
          + "AND "
          + "Q!=ALL(SELECT M FROM {J} owl:unionOf {M})"
          + "AND "
          + "Q!=ALL(SELECT N FROM {N} owl:unionOf {K}))";
      // every class is a subclass of itself in owlim
      // query =
      // "(Select DISTINCT Q FROM {P} rdfs:subClassOf {Q} "
      // + "WHERE P!=Q AND "
      // + "P!=ALL(SELECT D FROM {D} owl:equivalentClass {Q}) "
      // + "AND"
      // +
      // " Q!=ALL(SELECT M FROM {M} rdf:type {<http://www.w3.org/2002/07/owl#Restriction>}) "
      // + "INTERSECT"
      // +
      // " (SELECT A FROM {A} rdf:type {<http://www.w3.org/2002/07/owl#Class>}))"
      // +
      // " UNION " +
      // "SELECT DISTINCT B from {B} rdf:type {<http://www.w3.org/2002/07/owl#Restriction>}"
      // ;
    }
    else {
      query =
        "SELECT DISTINCT A from {A} rdf:type {<http://www.w3.org/2002/07/owl#Class>}"
          + " UNION "
          + "SELECT DISTINCT B from {B} rdf:type {<http://www.w3.org/2002/07/owl#Restriction>}";
    }

    QueryResultsTable iter = performQuery(query);
    ArrayList<String> list = new ArrayList<String>();
    for(int i = 0; i < iter.getRowCount(); i++) {
      list.add(iter.getValue(i, 0).toString());
    }
    if(debug) System.out.println("Top Classes : " + list.size());
    return listToResourceInfoArray(list);
  }

  /**
   * Returns if the given class is a top class.
   * 
   * @param classURI
   * @return
   */
  public boolean isTopClass(String repositoryID, String classURI)
    throws GateOntologyException {
    if(debug) print("isTopClass");
    return getSuperClasses(repositoryID, classURI, OConstants.DIRECT_CLOSURE).length == 0;
  }

  /**
   * Returns if the given property is a top property.
   * 
   * @param classURI
   * @return
   */
  public boolean isTopProperty(String repositoryID, String aPropertyURI)
    throws GateOntologyException {
    if(debug) print("isTopProperty");
    return getSuperProperties(repositoryID, aPropertyURI,
      OConstants.DIRECT_CLOSURE).length == 0;
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
  public void addSubClass(String repositoryID, String superClassURI,
    String subClassURI) throws GateOntologyException {
    if(debug) print("addSubClass");
    loadRepositoryDetails(repositoryID);
    addUUUStatement(subClassURI, RDFS.SUBCLASSOF, superClassURI);
    currentEventsLog.addEvent(new OEvent(subClassURI, RDFS.SUBCLASSOF,
      superClassURI, true));
  }

  /**
   * The method creates a new class with the URI as specified in className and
   * adds it as a superClassOf the parentClass. It also adds the provided
   * comment on the subClass.
   * 
   * @param superClassURI
   * @param subClassURI
   */
  public void addSuperClass(String repositoryID, String superClassURI,
    String subClassURI) throws GateOntologyException {
    if(debug) print("addSuperClass");
    loadRepositoryDetails(repositoryID);
    addUUUStatement(subClassURI, RDFS.SUBCLASSOF, superClassURI);
    currentEventsLog.addEvent(new OEvent(subClassURI, RDFS.SUBCLASSOF,
      superClassURI, true));
  }

  /**
   * Removes the subclass relationship
   * 
   * @param superClassURI
   * @param subClassURI
   */
  public void removeSubClass(String repositoryID, String superClassURI,
    String subClassURI) throws GateOntologyException {
    if(debug) print("removeSubClass");
    loadRepositoryDetails(repositoryID);
    removeUUUStatement(subClassURI, RDFS.SUBCLASSOF, superClassURI);
    currentEventsLog.addEvent(new OEvent(subClassURI, RDFS.SUBCLASSOF,
      superClassURI, false));
  }

  /**
   * Removes the superclass relationship
   * 
   * @param superClassURI
   * @param subClassURI
   */
  public void removeSuperClass(String repositoryID, String superClassURI,
    String subClassURI) throws GateOntologyException {
    loadRepositoryDetails(repositoryID);
    if(debug) print("removeSuperClass");
    removeUUUStatement(subClassURI, RDFS.SUBCLASSOF, superClassURI);
    currentEventsLog.addEvent(new OEvent(subClassURI, RDFS.SUBCLASSOF,
      superClassURI, false));
  }

  /**
   * This method returns all sub classes of the given class
   * 
   * @param superClassURI
   * @param direct
   * @return
   */
  public ResourceInfo[] getSubClasses(String repositoryID,
    String superClassURI, byte direct) throws GateOntologyException {
    if(debug) print("getSubClasses");
    loadRepositoryDetails(repositoryID);

    Resource r = getResource(superClassURI);
    String queryRep = "{<" + superClassURI + ">}";
    String queryRep1 = "<" + superClassURI + ">";
    if(r instanceof BNode) {
      queryRep = "{_:" + superClassURI + "}";
      queryRep1 = "_:" + superClassURI;
    }

    String query = "";

    query =
      "select distinct A FROM {A} rdfs:subClassOf " + queryRep + " WHERE A!="
        + queryRep1 + " MINUS "
        + " select distinct B FROM {B} owl:equivalentClass " + queryRep;

    QueryResultsTable iter = performQuery(query);
    List<String> list = new ArrayList<String>();
    for(int i = 0; i < iter.getRowCount(); i++) {
      list.add(iter.getValue(i, 0).toString());
    }

    if(direct == OConstants.DIRECT_CLOSURE) {
      Set<String> toDelete = new HashSet<String>();
      for(int i = 0; i < list.size(); i++) {
        String string = list.get(i);
        if(toDelete.contains(string)) continue;
        queryRep = "{<" + string + ">}";
        queryRep1 = "<" + string + ">";
        if(getResource(list.get(i)) instanceof BNode) {
          queryRep = "{_:" + string + "}";
          queryRep1 = "_:" + string;
        }

        query =
          "select distinct A FROM {A} rdfs:subClassOf " + queryRep
            + " WHERE A!=" + queryRep1 + " MINUS "
            + " select distinct B FROM {B} owl:equivalentClass " + queryRep;

        QueryResultsTable qrt = performQuery(query);
        for(int j = 0; j < qrt.getRowCount(); j++) {
          toDelete.add(qrt.getValue(j, 0).toString());
        }
      }
      list.removeAll(toDelete);
    }

    return listToResourceInfoArray(list);
  }

  /**
   * This method returns all super classes of the given class
   * 
   * @param subClassURI
   * @param direct
   * @return
   */
  public ResourceInfo[] getSuperClasses(String repositoryID,
    String subClassURI, byte direct) throws GateOntologyException {
    if(debug) print("getSuperClasses");
    loadRepositoryDetails(repositoryID);
    Resource r = getResource(subClassURI);
    String queryRep = "{<" + subClassURI + ">}";
    String queryRep1 = "<" + subClassURI + ">";
    if(r instanceof BNode) {
      queryRep = "{_:" + subClassURI + "}";
      queryRep1 = "_:" + subClassURI;
    }

    String query = "";
    query =
      "Select distinct SUPER FROM " + queryRep + " rdfs:subClassOf {SUPER}"
        + " WHERE SUPER!=" + queryRep1 + " MINUS "
        + " select distinct B FROM {B} owl:equivalentClass " + queryRep;

    QueryResultsTable iter = performQuery(query);
    List<String> list = new ArrayList<String>();
    for(int i = 0; i < iter.getRowCount(); i++) {
      list.add(iter.getValue(i, 0).toString());
    }

    if(direct == OConstants.DIRECT_CLOSURE) {
      Set<String> toDelete = new HashSet<String>();
      for(int i = 0; i < list.size(); i++) {
        String string = list.get(i);
        if(toDelete.contains(string)) continue;
        queryRep = "{<" + string + ">}";
        queryRep1 = "<" + string + ">";
        if(getResource(list.get(i)) instanceof BNode) {
          queryRep = "{_:" + string + "}";
          queryRep1 = "_:" + string;
        }

        query =
          "Select distinct SUPER FROM " + queryRep + " rdfs:subClassOf {SUPER}"
            + " WHERE SUPER!=" + queryRep1 + " MINUS "
            + " select distinct B FROM {B} owl:equivalentClass " + queryRep;

        QueryResultsTable qrt = performQuery(query);
        for(int j = 0; j < qrt.getRowCount(); j++) {
          toDelete.add(qrt.getValue(j, 0).toString());
        }
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
  public void setDisjointClassWith(String repositoryID, String class1URI,
    String class2URI) throws GateOntologyException {
    if(debug) print("setDisjointWith");
    loadRepositoryDetails(repositoryID);
    addUUUStatement(class1URI, OWL.DISJOINTWITH, class2URI);
    currentEventsLog.addEvent(new OEvent(class1URI, OWL.DISJOINTWITH,
      class2URI, true));
  }

  /**
   * Sets the classes as same classes
   * 
   * @param class1URI
   * @param class2URI
   */
  public void setEquivalentClassAs(String repositoryID, String class1URI,
    String class2URI) throws GateOntologyException {
    if(debug) print("setEquivalentClassAs");
    loadRepositoryDetails(repositoryID);
    addUUUStatement(class1URI, OWL.EQUIVALENTCLASS, class2URI);
    currentEventsLog.addEvent(new OEvent(class1URI, OWL.EQUIVALENTCLASS,
      class2URI, true));
  }

  /**
   * returns an array of classes which are marked as disjoint for the given
   * class
   * 
   * @param classURI
   * @return
   */
  public String[] getDisjointClasses(String repositoryID, String aClassURI)
    throws GateOntologyException {
    if(debug) print("setDisjointClasses");
    loadRepositoryDetails(repositoryID);

    Resource r1 = getResource(aClassURI);
    String queryRep1 = "<" + aClassURI + ">";
    if(r1 instanceof BNode) {
      queryRep1 = "_:" + aClassURI;
    }

    String query =
      "Select distinct B FROM {A}" + " owl:disjointWith {B} WHERE A!=B AND A="
        + queryRep1;

    QueryResultsTable iter = performQuery(query);
    List<String> list = new ArrayList<String>();
    for(int i = 0; i < iter.getRowCount(); i++) {
      list.add(iter.getValue(i, 0).toString());
    }
    return listToArray(list);
  }

  /**
   * returns an array of classes which are equivalent as the given class
   * 
   * @param aClassURI
   * @return
   */
  public ResourceInfo[] getEquivalentClasses(String repositoryID,
    String aClassURI) throws GateOntologyException {
    if(debug) print("getSameClasses");
    loadRepositoryDetails(repositoryID);
    Resource r1 = getResource(aClassURI);
    String queryRep1 = "<" + aClassURI + ">";
    if(r1 instanceof BNode) {
      queryRep1 = "_:" + aClassURI;
    }

    String query =
      "Select distinct B FROM {A}"
        + " owl:equivalentClass {B} WHERE A!=B AND A=" + queryRep1;

    QueryResultsTable iter = performQuery(query);
    List<String> list = new ArrayList<String>();
    for(int i = 0; i < iter.getRowCount(); i++) {
      list.add(iter.getValue(i, 0).toString());
    }
    return listToResourceInfoArray(list);
  }

  /**
   * Removes the given property
   * 
   * @param repositoryID
   * @param aPropertyURI
   * @param removeSubTree
   *          - if set to true, removes all its subproperties, otherwise shifts
   *          subproperties to its parent property
   * @return a list of URIs of resources deleted as a result of deleting this
   *         property.
   */
  public String[] removePropertyFromOntology(String repositoryID,
    String aPropertyURI, boolean removeSubTree) throws GateOntologyException {
    if(debug) print("removePropertyWithName");
    loadRepositoryDetails(repositoryID);
    List<String> deletedResources = new ArrayList<String>();
    if(removeUUUStatement(aPropertyURI, RDF.TYPE, null) == 0) {
      throw new GateOntologyException(aPropertyURI
        + " is not an explicit Property");
    }
    else {
      currentEventsLog
        .addEvent(new OEvent(aPropertyURI, RDF.TYPE, null, false));
      deletedResources.add(aPropertyURI);
    }

    try {
      startTransaction(null);
      // removing all values set for the current property
      sail.removeStatements(null, getURI(aPropertyURI), null);
      currentEventsLog.addEvent(new OEvent(null, aPropertyURI, null, false));
      sail.removeStatements(getResource(aPropertyURI),
        getURI(RDFS.SUBPROPERTYOF), null);
      currentEventsLog.addEvent(new OEvent(aPropertyURI, RDFS.SUBPROPERTYOF,
        null, false));
      endTransaction(null);
    }
    catch(SailUpdateException sue) {
      throw new GateOntologyException("error while removing a property:"
        + aPropertyURI, sue);
    }

    // this should happen only if removeSubTree is set to true
    if(removeSubTree) {
      Property[] subProps =
        getSubProperties(null, aPropertyURI, OConstants.DIRECT_CLOSURE);
      for(int i = 0; i < subProps.length; i++) {
        if(sail.hasExplicitStatement(getResource(subProps[i].getUri()),
          getURI(RDF.TYPE), null)) continue;
        String[] removedResources =
          removePropertyFromOntology(null, subProps[i].getUri(), true);
        deletedResources.addAll(Arrays.asList(removedResources));
      }
    }
    removeUUUStatement(aPropertyURI, null, null);
    currentEventsLog.addEvent(new OEvent(aPropertyURI, null, null, false));
    removeUUUStatement(null, aPropertyURI, null);
    currentEventsLog.addEvent(new OEvent(null, aPropertyURI, null, false));
    removeUUUStatement(null, null, aPropertyURI);
    currentEventsLog.addEvent(new OEvent(null, null, aPropertyURI, false));
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
  public void addObjectProperty(String repositoryID, String aPropertyURI,
    String[] domainClassesURIs, String[] rangeClassesTypes)
    throws GateOntologyException {
    if(debug) print("addObjectProperty");
    loadRepositoryDetails(repositoryID);
    addUUUStatement(aPropertyURI, RDF.TYPE, OWL.OBJECTPROPERTY);
    currentEventsLog.addEvent(new OEvent(aPropertyURI, RDF.TYPE,
      OWL.OBJECTPROPERTY, true));
    if(domainClassesURIs != null) {
      for(int i = 0; i < domainClassesURIs.length; i++) {
        addUUUStatement(aPropertyURI, RDFS.DOMAIN, domainClassesURIs[i]);
        currentEventsLog.addEvent(new OEvent(aPropertyURI, RDFS.DOMAIN,
          domainClassesURIs[i], true));
      }
    }
    if(rangeClassesTypes != null) {
      for(int i = 0; i < rangeClassesTypes.length; i++) {
        addUUUStatement(aPropertyURI, RDFS.RANGE, rangeClassesTypes[i]);
        currentEventsLog.addEvent(new OEvent(aPropertyURI, RDFS.RANGE,
          rangeClassesTypes[i], true));
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
  public void addTransitiveProperty(String repositoryID, String aPropertyURI,
    String[] domainClassesURIs, String[] rangeClassesTypes)
    throws GateOntologyException {
    if(debug) print("addTransitiveProperty");
    loadRepositoryDetails(repositoryID);
    addUUUStatement(aPropertyURI, RDF.TYPE, OWL.TRANSITIVEPROPERTY);
    currentEventsLog.addEvent(new OEvent(aPropertyURI, RDF.TYPE,
      OWL.TRANSITIVEPROPERTY, true));
    if(domainClassesURIs != null) {
      for(int i = 0; i < domainClassesURIs.length; i++) {
        addUUUStatement(aPropertyURI, RDFS.DOMAIN, domainClassesURIs[i]);
        currentEventsLog.addEvent(new OEvent(aPropertyURI, RDFS.DOMAIN,
          domainClassesURIs[i], true));
      }
    }
    if(rangeClassesTypes != null) {
      for(int i = 0; i < rangeClassesTypes.length; i++) {
        addUUUStatement(aPropertyURI, RDFS.RANGE, rangeClassesTypes[i]);
        currentEventsLog.addEvent(new OEvent(aPropertyURI, RDFS.RANGE,
          rangeClassesTypes[i], true));
      }
    }
  }

  /**
   * The method returns an array of properties. Property is a complex structure,
   * which contains name, comment, information about its domain and range.
   * 
   * @return
   */
  public Property[] getRDFProperties(String repositoryID)
    throws GateOntologyException {
    loadRepositoryDetails(repositoryID);

    List<Property> list = new ArrayList<Property>();
    String query =
      "Select distinct X FROM {X} rdf:type {<" + RDF.PROPERTY + ">}";

    QueryResultsTable iter = performQuery(query);
    for(int i = 0; i < iter.getRowCount(); i++) {
      Value anAnnProp = iter.getValue(i, 0);
      String propString = anAnnProp.toString();
      if(isAnnotationProperty(repositoryID, propString)
        || isDatatypeProperty(repositoryID, propString)
        || isObjectProperty(repositoryID, propString)
        || isTransitiveProperty(repositoryID, propString)
        || isSymmetricProperty(repositoryID, propString)) continue;
      list.add(new Property(OConstants.RDF_PROPERTY, anAnnProp.toString()));
    }
    return listToPropertyArray(list);
  }

  /**
   * The method returns an array of properties. Property is a complex structure,
   * which contains name, comment, information about its domain and range.
   * 
   * @return
   */
  public Property[] getObjectProperties(String repositoryID)
    throws GateOntologyException {
    loadRepositoryDetails(repositoryID);
    List<Property> list = new ArrayList<Property>();
    String query =
      "Select distinct X FROM {X} rdf:type {<" + OWL.OBJECTPROPERTY + ">}";

    QueryResultsTable iter = performQuery(query);
    for(int i = 0; i < iter.getRowCount(); i++) {
      Value anAnnProp = iter.getValue(i, 0);
      list.add(new Property(OConstants.OBJECT_PROPERTY, anAnnProp.toString()));
    }

    query =
      "Select distinct X FROM {X} rdf:type {<" + OWL.SYMMETRICPROPERTY + ">}";

    iter = performQuery(query);
    for(int i = 0; i < iter.getRowCount(); i++) {
      Value anAnnProp = iter.getValue(i, 0);
      list
        .add(new Property(OConstants.SYMMETRIC_PROPERTY, anAnnProp.toString()));
    }

    query =
      "Select distinct X FROM {X} rdf:type {<" + OWL.TRANSITIVEPROPERTY + ">}";

    iter = performQuery(query);
    for(int i = 0; i < iter.getRowCount(); i++) {
      Value anAnnProp = iter.getValue(i, 0);
      list.add(new Property(OConstants.TRANSITIVE_PROPERTY, anAnnProp
        .toString()));
    }

    return listToPropertyArray(list);
  }

  /**
   * The method returns an array of properties. Property is a complex structure,
   * which contains name, comment, information about its domain and range.
   * 
   * @return
   */
  public Property[] getSymmetricProperties(String repositoryID)
    throws GateOntologyException {
    loadRepositoryDetails(repositoryID);
    List<Property> list = new ArrayList<Property>();
    String query =
      "Select distinct X FROM {X} rdf:type {<" + OWL.SYMMETRICPROPERTY + ">}";

    QueryResultsTable iter = performQuery(query);
    for(int i = 0; i < iter.getRowCount(); i++) {
      Value anAnnProp = iter.getValue(i, 0);
      list
        .add(new Property(OConstants.SYMMETRIC_PROPERTY, anAnnProp.toString()));
    }
    return listToPropertyArray(list);
  }

  /**
   * The method returns an array of properties. Property is a complex structure,
   * which contains name, comment, information about its domain and range.
   * 
   * @return
   */
  public Property[] getTransitiveProperties(String repositoryID)
    throws GateOntologyException {
    loadRepositoryDetails(repositoryID);
    List<Property> list = new ArrayList<Property>();
    String query =
      "Select distinct X FROM {X} rdf:type {<" + OWL.TRANSITIVEPROPERTY + ">}";

    QueryResultsTable iter = performQuery(query);
    for(int i = 0; i < iter.getRowCount(); i++) {
      Value anAnnProp = iter.getValue(i, 0);
      list.add(new Property(OConstants.TRANSITIVE_PROPERTY, anAnnProp
        .toString()));
    }
    return listToPropertyArray(list);
  }

  /**
   * The method returns an array of properties. Property is a complex structure,
   * which contains name, comment, information about its domain and range.
   * 
   * @return
   */
  public Property[] getDatatypeProperties(String repositoryID)
    throws GateOntologyException {
    loadRepositoryDetails(repositoryID);
    List<Property> list = new ArrayList<Property>();
    String query =
      "Select distinct X FROM {X} rdf:type {<" + OWL.DATATYPEPROPERTY + ">}";

    QueryResultsTable iter = performQuery(query);
    for(int i = 0; i < iter.getRowCount(); i++) {
      Value anAnnProp = iter.getValue(i, 0);
      list
        .add(new Property(OConstants.DATATYPE_PROPERTY, anAnnProp.toString()));
    }
    return listToPropertyArray(list);
  }

  /**
   * The method returns an array of properties. Property is a complex structure,
   * which contains name, comment, information about its domain and range.
   * 
   * @return
   */
  public Property[] getAnnotationProperties(String repositoryID)
    throws GateOntologyException {
    loadRepositoryDetails(repositoryID);
    List<Property> list = new ArrayList<Property>();
    String query =
      "Select distinct X FROM {X} rdf:type {<" + OWL.ANNOTATIONPROPERTY + ">}";

    QueryResultsTable iter = performQuery(query);
    for(int i = 0; i < iter.getRowCount(); i++) {
      Value anAnnProp = iter.getValue(i, 0);
      list.add(new Property(OConstants.ANNOTATION_PROPERTY, anAnnProp
        .toString()));
    }

    boolean allowSystemStatements = this.returnSystemStatements;
    this.returnSystemStatements = true;
    Property[] props = listToPropertyArray(list);
    this.returnSystemStatements = allowSystemStatements;
    return props;
  }

  /**
   * Given a property, this method returns its domain
   * 
   * @param aPropertyURI
   * @return
   */
  public ResourceInfo[] getDomain(String repositoryID, String aPropertyURI)
    throws GateOntologyException {
    if(debug) print("getDomain");
    if(isAnnotationProperty(repositoryID, aPropertyURI)) { throw new GateOntologyException(
      "AnnotationProperties do no specify any domain or range"); }

    String query =
      "select distinct Y from {<" + aPropertyURI + ">} rdfs:domain {Y}";
    QueryResultsTable iter = performQuery(query);
    List<ResourceInfo> list = new ArrayList<ResourceInfo>();
    for(int i = 0; i < iter.getRowCount(); i++) {
      String classString = iter.getValue(i, 0).toString();
      byte classType = getClassType(repositoryID, classString);
      if(classType == OConstants.ANNONYMOUS_CLASS) continue;
      list.add(new ResourceInfo(classString, classType));
    }
    return reduceToMostSpecificClasses(null, list);
  }

  /**
   * Given a property, this method returns its range
   * 
   * @param aPropertyURI
   * @return
   */
  public ResourceInfo[] getRange(String repositoryID, String aPropertyURI)
    throws GateOntologyException {
    if(debug) print("getRange");
    if(isAnnotationProperty(repositoryID, aPropertyURI)) { throw new GateOntologyException(
      "AnnotationProperties do no specify any domain or range"); }
    if(isDatatypeProperty(null, aPropertyURI)) { throw new GateOntologyException(
      "Please use getDatatype(String repositoryID, String theDatatypeProerptyURI) method instead"); }

    String query =
      "Select distinct Y from {<" + aPropertyURI + ">} rdfs:range {Y}";
    QueryResultsTable iter = performQuery(query);
    List<ResourceInfo> list = new ArrayList<ResourceInfo>();
    for(int i = 0; i < iter.getRowCount(); i++) {
      String classString = iter.getValue(i, 0).toString();
      byte classType = getClassType(repositoryID, classString);
      if(classType == OConstants.ANNONYMOUS_CLASS) continue;
      list.add(new ResourceInfo(classString, classType));
    }
    return reduceToMostSpecificClasses(null, list);
  }

  /**
   * Returns if the provided property is functional
   * 
   * @param aPropertyURI
   * @return
   */
  public boolean isFunctional(String repositoryID, String aPropertyURI)
    throws GateOntologyException {
    if(debug) print("isFunctional");
    loadRepositoryDetails(repositoryID);
    String query =
      "Select * FROM {X} rdf:type {<" + OWL.FUNCTIONALPROPERTY + ">} WHERE X=<"
        + aPropertyURI + ">";
    return performQuery(query).getRowCount() > 0;
  }

  /**
   * sets the current property as functional
   * 
   * @param aPropertyURI
   * @param isFunctional
   */
  public void setFunctional(String repositoryID, String aPropertyURI,
    boolean isFunctional) throws GateOntologyException {
    if(debug) print("setFunctional");
    loadRepositoryDetails(repositoryID);
    if(isFunctional) {
      addUUUStatement(aPropertyURI, RDF.TYPE, OWL.FUNCTIONALPROPERTY);
      currentEventsLog.addEvent(new OEvent(aPropertyURI, RDF.TYPE,
        OWL.FUNCTIONALPROPERTY, true));
    }
    else {
      removeUUUStatement(aPropertyURI, RDF.TYPE, OWL.FUNCTIONALPROPERTY);
      currentEventsLog.addEvent(new OEvent(aPropertyURI, RDF.TYPE,
        OWL.FUNCTIONALPROPERTY, false));
    }
  }

  /**
   * returns if the given property is inverse functional property
   * 
   * @param aPropertyURI
   * @return
   */
  public boolean isInverseFunctional(String repositoryID, String aPropertyURI)
    throws GateOntologyException {
    if(debug) print("isInverseFunctional");
    loadRepositoryDetails(repositoryID);
    String query =
      "Select * FROM {X} rdf:type {<" + OWL.INVERSEFUNCTIONALPROPERTY
        + ">} WHERE X=<" + aPropertyURI + ">";
    return performQuery(query).getRowCount() > 0;
  }

  /**
   * Sets the current property as inverse functional property
   * 
   * @param aPropertyURI
   * @param isInverseFunctional
   */
  public void setInverseFunctional(String repositoryID, String aPropertyURI,
    boolean isInverseFunctional) throws GateOntologyException {
    if(debug) print("setInverseFunctional");
    loadRepositoryDetails(repositoryID);
    if(isInverseFunctional) {
      addUUUStatement(aPropertyURI, RDF.TYPE, OWL.INVERSEFUNCTIONALPROPERTY);
      currentEventsLog.addEvent(new OEvent(aPropertyURI, RDF.TYPE,
        OWL.INVERSEFUNCTIONALPROPERTY, true));
    }
    else {
      removeUUUStatement(aPropertyURI, RDF.TYPE, OWL.INVERSEFUNCTIONALPROPERTY);
      currentEventsLog.addEvent(new OEvent(aPropertyURI, RDF.TYPE,
        OWL.INVERSEFUNCTIONALPROPERTY, false));
    }
  }

  /**
   * returns if the given property is a symmetric property
   * 
   * @param aPropertyURI
   * @return
   */
  public boolean isSymmetricProperty(String repositoryID, String aPropertyURI)
    throws GateOntologyException {
    if(debug) print("isSymmetricProperty");
    loadRepositoryDetails(repositoryID);
    String query =
      "Select * FROM {X} rdf:type {<" + OWL.SYMMETRICPROPERTY + ">} WHERE X=<"
        + aPropertyURI + ">";
    return performQuery(query).getRowCount() > 0;
  }

  /**
   * returns if the given property is a transitive property
   * 
   * @param aPropertyURI
   * @return
   */
  public boolean isTransitiveProperty(String repositoryID, String aPropertyURI)
    throws GateOntologyException {
    if(debug) print("isTransitiveProperty");
    loadRepositoryDetails(repositoryID);
    String query =
      "Select * FROM {X} rdf:type {<" + OWL.TRANSITIVEPROPERTY + ">} WHERE X=<"
        + aPropertyURI + ">";
    return performQuery(query).getRowCount() > 0;
  }

  /**
   * returns if the given property is a datatype property
   * 
   * @param aPropertyURI
   * @return
   */
  public boolean isDatatypeProperty(String repositoryID, String aPropertyURI)
    throws GateOntologyException {
    if(debug) print("isDatatypeProperty");
    loadRepositoryDetails(repositoryID);
    String query =
      "Select * FROM {X} rdf:type {<" + OWL.DATATYPEPROPERTY + ">} WHERE X=<"
        + aPropertyURI + ">";
    return performQuery(query).getRowCount() > 0;
  }

  /**
   * returns if the given property is an object property
   * 
   * @param aPropertyURI
   * @return
   */
  public boolean isObjectProperty(String repositoryID, String aPropertyURI)
    throws GateOntologyException {
    if(debug) print("isObjectProperty");
    loadRepositoryDetails(repositoryID);
    String query =
      "Select * FROM {X} rdf:type {<" + OWL.OBJECTPROPERTY + ">} WHERE X=<"
        + aPropertyURI + ">";
    return performQuery(query).getRowCount() > 0;
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
  public void setEquivalentPropertyAs(String repositoryID, String property1URI,
    String property2URI) throws GateOntologyException {
    if(debug) print("setEquivalentPropertyAs");
    loadRepositoryDetails(repositoryID);
    addUUUStatement(property1URI, OWL.EQUIVALENTPROPERTY, property2URI);
    currentEventsLog.addEvent(new OEvent(property1URI, OWL.EQUIVALENTPROPERTY,
      property2URI, true));
  }

  /**
   * For the given property, this method returns all properties marked as
   * Equivalent as it
   * 
   * @param aPropertyURI
   * @return
   */
  public Property[] getEquivalentPropertyAs(String repositoryID,
    String aPropertyURI) throws GateOntologyException {
    if(debug) print("getEquivalentPropertyAs");
    loadRepositoryDetails(repositoryID);

    String query =
      "Select DISTINCT Y FROM {X} owl:equivalentProperty {Y} WHERE X=<"
        + aPropertyURI + ">";
    QueryResultsTable iter = performQuery(query);
    List<Property> list = new ArrayList<Property>();
    for(int i = 0; i < iter.getRowCount(); i++) {
      list.add(createPropertyObject(null, iter.getValue(i, 0).toString()));
    }
    return listToPropertyArray(list);
  }

  /**
   * For the given properties, this method registers the super, sub relation
   * 
   * @param superPropertyURI
   * @param subPropertyURI
   */
  public void addSuperProperty(String repositoryID, String superPropertyURI,
    String subPropertyURI) throws GateOntologyException {
    if(debug) print("addSuperProperty");
    loadRepositoryDetails(repositoryID);
    addUUUStatement(subPropertyURI, RDFS.SUBPROPERTYOF, superPropertyURI);
    currentEventsLog.addEvent(new OEvent(subPropertyURI, RDFS.SUBPROPERTYOF,
      superPropertyURI, true));
  }

  /**
   * For the given properties, this method removes the super, sub relation
   * 
   * @param superPropertyURI
   * @param subPropertyURI
   */
  public void removeSuperProperty(String repositoryID, String superPropertyURI,
    String subPropertyURI) throws GateOntologyException {
    if(debug) print("removeSuperProperty");
    loadRepositoryDetails(repositoryID);
    removeUUUStatement(subPropertyURI, RDFS.SUBPROPERTYOF, superPropertyURI);
    currentEventsLog.addEvent(new OEvent(subPropertyURI, RDFS.SUBPROPERTYOF,
      superPropertyURI, false));
  }

  /**
   * For the given properties, this method registers the super, sub relation
   * 
   * @param superPropertyURI
   * @param subPropertyURI
   */
  public void addSubProperty(String repositoryID, String superPropertyURI,
    String subPropertyURI) throws GateOntologyException {
    if(debug) print("addSubProperty");
    loadRepositoryDetails(repositoryID);
    addUUUStatement(subPropertyURI, RDFS.SUBPROPERTYOF, superPropertyURI);
    currentEventsLog.addEvent(new OEvent(subPropertyURI, RDFS.SUBPROPERTYOF,
      superPropertyURI, true));
  }

  /**
   * For the given properties, this method removes the super, sub relation
   * 
   * @param superPropertyURI
   * @param subPropertyURI
   */
  public void removeSubProperty(String repositoryID, String superPropertyURI,
    String subPropertyURI) throws GateOntologyException {
    if(debug) print("removeSubProperty");
    loadRepositoryDetails(repositoryID);
    removeUUUStatement(subPropertyURI, RDFS.SUBPROPERTYOF, superPropertyURI);
    currentEventsLog.addEvent(new OEvent(subPropertyURI, RDFS.SUBPROPERTYOF,
      superPropertyURI, false));
  }

  /**
   * for the given property, the method returns all its super properties
   * 
   * @param aPropertyURI
   * @param direct
   * @return
   */
  public Property[] getSuperProperties(String repositoryID,
    String aPropertyURI, boolean direct) throws GateOntologyException {
    return this.getSuperProperties(repositoryID, aPropertyURI, direct
      ? OConstants.DIRECT_CLOSURE
      : OConstants.TRANSITIVE_CLOSURE);
  }

  /**
   * for the given property, the method returns all its sub properties
   * 
   * @param aPropertyURI
   * @param direct
   * @return
   */
  public Property[] getSubProperties(String repositoryID, String aPropertyURI,
    boolean direct) throws GateOntologyException {
    return this.getSubProperties(repositoryID, aPropertyURI, direct
      ? OConstants.DIRECT_CLOSURE
      : OConstants.TRANSITIVE_CLOSURE);
  }

  /**
   * for the given property, the method returns all its inverse properties
   * 
   * @param aPropertyURI
   * @return
   */
  public Property[] getInverseProperties(String repositoryID,
    String aPropertyURI) throws GateOntologyException {
    if(debug) print("getInverseProperties");
    loadRepositoryDetails(repositoryID);

    String query =
      "Select DISTINCT Y FROM {X} owl:inverseOf {Y} WHERE X=<" + aPropertyURI
        + ">";
    QueryResultsTable iter = performQuery(query);
    List<Property> list = new ArrayList<Property>();
    for(int i = 0; i < iter.getRowCount(); i++) {
      list.add(createPropertyObject(null, iter.getValue(i, 0).toString()));
    }
    return listToPropertyArray(list);
  }

  /**
   * property1 is set as inverse of property 2
   * 
   * @param property1URI
   * @param property2URI
   */
  public void setInverseOf(String repositoryID, String propertyURI1,
    String propertyURI2) throws GateOntologyException {
    if(debug) print("setInverseOf");
    loadRepositoryDetails(repositoryID);
    addUUUStatement(propertyURI1, OWL.INVERSEOF, propertyURI2);
    currentEventsLog.addEvent(new OEvent(propertyURI1, OWL.INVERSEOF,
      propertyURI2, true));
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
  public void addIndividual(String repositoryID, String superClassURI,
    String individualURI) throws GateOntologyException {
    if(debug) print("addIndividual");
    loadRepositoryDetails(repositoryID);
    addUUUStatement(individualURI, RDF.TYPE, superClassURI);
    currentEventsLog.addEvent(new OEvent(individualURI, RDF.TYPE,
      superClassURI, true));
  }

  /**
   * The method removes the provided instance from the repository.
   * 
   * @param individual
   * @return
   */
  public String[] removeIndividual(String repositoryID, String individualURI)
    throws GateOntologyException {
    if(debug) print("removeIndividual");
    loadRepositoryDetails(repositoryID);
    int no = removeUUUStatement(individualURI, RDF.TYPE, null);
    if(no == 0)
      throw new GateOntologyException(individualURI
        + " is not an explicit Individual");
    currentEventsLog.addEvent(new OEvent(individualURI, RDF.TYPE, null, false));

    // we need to go though all ontology resources of the ontology
    // check if they have property with value the current resource
    // we need to delete it
    List<Property> properties = new ArrayList<Property>();
    properties.addAll(Arrays.asList(getObjectProperties(null)));
    try {
      startTransaction(null);
      for(int i = 0; i < properties.size(); i++) {
        sail.removeStatements(null, getURI(properties.get(i).getUri()),
          getResource(individualURI));
        currentEventsLog.addEvent(new OEvent(null, properties.get(i).getUri(),
          individualURI, false));
      }
      endTransaction(null);
    }
    catch(SailUpdateException sue) {
      throw new GateOntologyException("error while removing individual:"
        + individualURI, sue);
    }
    removeUUUStatement(individualURI, null, null);
    currentEventsLog.addEvent(new OEvent(individualURI, null, null, false));
    removeUUUStatement(null, null, individualURI);
    currentEventsLog.addEvent(new OEvent(null, null, individualURI, false));
    removeUUUStatement(null, individualURI, null);
    currentEventsLog.addEvent(new OEvent(null, individualURI, null, false));
    return new String[]{individualURI};
  }

  /**
   * The method returns all member instances of the provided class. It returns
   * only the direct instances if the boolean parameter direct is set to true.
   * 
   * @param superClassURI
   * @param direct
   */
  public String[] getIndividuals(String repositoryID, String superClassURI,
    byte direct) throws GateOntologyException {
    if(debug) print("getIndividulas");
    loadRepositoryDetails(repositoryID);
    Resource r = getResource(superClassURI);
    String queryRep = "{<" + superClassURI + ">}";
    if(r instanceof BNode) {
      queryRep = "{_:" + superClassURI + "}";
    }

    // A -> B -> I1

    String query = "";
    if(direct == OConstants.DIRECT_CLOSURE) {
      query = "Select distinct X from {X} serql:directType " + queryRep;
    }
    else {
      query = "Select distinct X from {X} rdf:type " + queryRep;
    }

    QueryResultsTable iter = performQuery(query);
    List<String> list = new ArrayList<String>();
    for(int i = 0; i < iter.getRowCount(); i++) {
      list.add(iter.getValue(i, 0).toString());
    }
    return listToArray(list);
  }

  /**
   * returns all resources registered as individuals in the ontology
   * 
   * @return
   */
  public String[] getIndividuals(String repositoryID)
    throws GateOntologyException {
    if(debug) print("getIndividuals");
    loadRepositoryDetails(repositoryID);

    String query =
      "Select distinct X from {X} rdf:type {} rdf:type {<http://www.w3.org/2002/07/owl#Class>}";
    QueryResultsTable iter = performQuery(query);
    List<String> list = new ArrayList<String>();
    for(int i = 0; i < iter.getRowCount(); i++) {
      list.add(iter.getValue(i, 0).toString());
    }
    return listToArray(list);
  }

  /**
   * Given a class and instance URIs, the method checks if the latter is a
   * member of former. If the boolean parameter direct is set to true, the
   * method also checks if the literal is a direct instance of the class.
   * 
   * @param aSuperClassURI
   * @param individualURI
   * @return
   */
  public boolean hasIndividual(String repositoryID, String aSuperClassURI,
    String individualURI, boolean direct) throws GateOntologyException {
    if(debug) print("hasIndividual");

    loadRepositoryDetails(repositoryID);
    Resource r = getResource(aSuperClassURI);
    String queryRep = "{<" + aSuperClassURI + ">}";
    if(r instanceof BNode) {
      queryRep = "{_:" + aSuperClassURI + "}";
    }

    String query = "";
    if(direct) {
      query =
        "Select * from {X} serql:directType " + queryRep + " WHERE X=<"
          + individualURI + ">";
    }
    else {
      query =
        "Select * from {X} rdf:type " + queryRep + " WHERE X=<" + individualURI
          + ">";
    }

    return performQuery(query).getRowCount() > 0;
  }

  /**
   * For the given individual, the method returns a set of classes for which the
   * individual is registered as instance of
   * 
   * @param individualURI
   */
  public ResourceInfo[] getClassesOfIndividual(String repositoryID,
    String individualURI, byte direct) throws GateOntologyException {
    if(debug) print("getClassesOfIndividual");
    loadRepositoryDetails(repositoryID);
    String query = "";
    if(direct == OConstants.DIRECT_CLOSURE) {
      query =
        "Select DISTINCT B from {X} serql:directType {B} WHERE X=<"
          + individualURI + ">";
    }
    else {
      query =
        "Select DISTINCT B from {X} rdf:type {B} WHERE X=<" + individualURI
          + ">";
    }

    QueryResultsTable iter = performQuery(query);
    List<String> list = new ArrayList<String>();
    for(int i = 0; i < iter.getRowCount(); i++) {
      list.add(iter.getValue(i, 0).toString());
    }
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
  public void setDifferentIndividualFrom(String repositoryID,
    String individual1URI, String individual2URI) throws GateOntologyException {
    if(debug) print("setDifferentIndividualFrom");
    loadRepositoryDetails(repositoryID);
    addUUUStatement(individual1URI, OWL.DIFFERENTFROM, individual2URI);
    currentEventsLog.addEvent(new OEvent(individual1URI, OWL.DIFFERENTFROM,
      individual1URI, true));
  }

  /**
   * for the given individual, the method returns all individuals registered as
   * different from the given individual
   * 
   * @param individualURI
   * @return
   */
  public String[] getDifferentIndividualFrom(String repositoryID,
    String individualURI) throws GateOntologyException {
    if(debug) print("getDifferentIndividualFrom");
    loadRepositoryDetails(repositoryID);

    String query =
      "Select distinct B from {X} owl:differentFrom {B} WHERE X=<"
        + individualURI + ">";
    QueryResultsTable iter = performQuery(query);
    List<String> list = new ArrayList<String>();
    for(int i = 0; i < iter.getRowCount(); i++) {
      list.add(iter.getValue(i, 0).toString());
    }
    return listToArray(list);
  }

  /**
   * individual1 is set as same as the individual2
   * 
   * @param individual1URI
   * @param individual2URI
   */
  public void setSameIndividualAs(String repositoryID, String individual1URI,
    String individual2URI) throws GateOntologyException {
    if(debug) print("setSameIndividualAs");
    loadRepositoryDetails(repositoryID);
    addUUUStatement(individual1URI, OWL.SAMEAS, individual2URI);
    currentEventsLog.addEvent(new OEvent(individual1URI, OWL.SAMEAS,
      individual1URI, true));
  }

  /**
   * for the given individual, the method returns all individuals which are
   * registered as same as the provided individual
   * 
   * @param inidividualURI
   * @return
   */
  public String[] getSameIndividualAs(String repositoryID, String individualURI)
    throws GateOntologyException {
    if(debug) print("getSameIndividualAs");
    loadRepositoryDetails(repositoryID);
    String query =
      "select distinct B from {X} owl:sameAs {B} WHERE X=<" + individualURI
        + "> AND X!=B";
    QueryResultsTable iter = performQuery(query);
    List<String> list = new ArrayList<String>();
    for(int i = 0; i < iter.getRowCount(); i++) {
      list.add(iter.getValue(i, 0).toString());
    }
    return listToArray(list);
  }

  // ***********************************************
  // ********* Restrictions ***********************
  // ***********************************************

  /**
   * This method given a restriction uri returns the value for the onProperty
   * element.
   * 
   * @param repositoryId
   * @param restrictionURI
   * @return
   * @throws GateOntologyException
   */
  public Property getOnPropertyValue(String repositoryId, String restrictionURI)
    throws GateOntologyException {
    if(debug) print("getOnPropertyValue");
    loadRepositoryDetails(repositoryId);
    Resource r1 = getResource(restrictionURI);
    String queryRep = "<" + restrictionURI + ">";
    if(r1 instanceof BNode) {
      queryRep = "_:" + restrictionURI;
    }

    String query =
      "Select distinct B from {X} owl:onProperty {B} WHERE X=" + queryRep;
    QueryResultsTable iter = performQuery(query);
    if(iter.getRowCount() > 0) {
      // here we need to check which type of property it is
      return createPropertyObject(null, iter.getValue(0, 0).toString());
    }
    return null;
  }

  /**
   * This method sets the value for onProperty element on the given restriction.
   * 
   * @param repositoryId
   * @param restrictionURI
   * @param propertyURI
   * @throws GateOntologyException
   */
  public void setOnPropertyValue(String repositoryId, String restrictionURI,
    String propertyURI) throws GateOntologyException {

    if(debug) print("setOnPropertyValue");
    loadRepositoryDetails(repositoryId);
    addUUUStatement(restrictionURI, OWL.ONPROPERTY, propertyURI);
    currentEventsLog.addEvent(new OEvent(restrictionURI, OWL.ONPROPERTY,
      propertyURI, true));
  }

  /**
   * Gets the datatype uri specified on the given restriction uri.
   * 
   * @param repositoryID
   * @param restrictionURI
   * @return
   * @throws GateOntologyException
   */
  public PropertyValue getPropertyValue(String repositoryId,
    String restrictionURI, byte restrictionType) throws GateOntologyException {

    if(debug) print("getDataType");
    loadRepositoryDetails(repositoryId);
    String whatValueURI = null;
    switch(restrictionType){
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
        throw new GateOntologyException("Invalid restriction type :"
          + restrictionType + " for the " + restrictionURI);
    }

    StatementIterator iter =
      sail.getStatements(getResource(restrictionURI), getURI(whatValueURI),
        null);
    if(iter.hasNext()) {
      Value v = iter.next().getObject();
      if(v instanceof Literal) { return new PropertyValue(((Literal)v)
        .getDatatype().toString(), ((Literal)v).getLabel()); }
    }
    return null;
  }

  /**
   * Sets the datatype uri for the given restriction uri.
   * 
   * @param repositoryID
   * @param restrictionURI
   * @param datatypeURI
   * @return
   * @throws GateOntologyException
   */
  public void setPropertyValue(String repositoryId, String restrictionURI,
    byte restrictionType, String value, String datatypeURI)
    throws GateOntologyException {
    if(debug) print("getDataType");
    loadRepositoryDetails(repositoryId);
    String whatValueURI = null;
    switch(restrictionType){
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
        throw new GateOntologyException("Invalid restriction type :"
          + restrictionType + " for the restriction " + restrictionURI);
    }

    StatementIterator iter =
      sail.getStatements(getResource(restrictionURI), getURI(whatValueURI),
        null);
    Statement toDelete = null;
    if(iter.hasNext()) {
      Statement stmt = iter.next();
      Value v = stmt.getObject();
      if(v instanceof Literal) {
        if(((Literal)v).getDatatype().toString().intern() == datatypeURI
          .intern()) {
          toDelete = stmt;
        }
      }
    }

    if(toDelete != null) {
      Literal l = (Literal)toDelete.getObject();
      removeUUUStatement(whatValueURI, l.getLabel(), l.getDatatype().toString());
      currentEventsLog.addEvent(new OEvent(whatValueURI, l.getLabel(), l
        .getDatatype().toString(), false));
    }
    addUUDStatement(repositoryId, restrictionURI, whatValueURI, value,
      datatypeURI);
    currentEventsLog.addEvent(new OEvent(restrictionURI, whatValueURI, value,
      datatypeURI, true));
  }

  /**
   * Gets the cardinality value specified on the given restriction uri.
   * 
   * @param repositoryID
   * @param restrictionURI
   * @param restrictionType
   *          - either of the following constants from the OConstants -
   *          ALL_VALUES_FROM_RESTRICTION, SOME_VALUES_FROM_RESTRICTION, and
   *          HAS_VALUE_RESTRICTION
   * @return
   * @throws GateOntologyException
   */
  public ResourceInfo getRestrictionValue(String repositoryId,
    String restrictionURI, byte restrictionType) throws GateOntologyException {
    if(debug) print("getRestrictionValue");
    loadRepositoryDetails(repositoryId);

    String whatValueURI = null;
    switch(restrictionType){
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
        throw new GateOntologyException("Invalid restriction type:"
          + restrictionType + " for the restriction " + restrictionURI);
    }

    StatementIterator iter =
      sail.getStatements(getResource(restrictionURI), getURI(whatValueURI),
        null);
    if(iter.hasNext()) {
      String resourceURI = iter.next().getObject().toString();
      Resource res = getResource(resourceURI);
      boolean isRestriction =
        sail.hasStatement(res, getURI(RDF.TYPE), getResource(OWL.RESTRICTION));
      byte classType = OConstants.OWL_CLASS;
      if(isRestriction) {
        if(sail.hasStatement(res, getURI(OWL.HASVALUE), null)) {
          classType = OConstants.HAS_VALUE_RESTRICTION;
        }
        else if(sail.hasStatement(res, getURI(OWL.SOMEVALUESFROM), null)) {
          classType = OConstants.SOME_VALUES_FROM_RESTRICTION;
        }
        else if(sail.hasStatement(res, getURI(OWL.ALLVALUESFROM), null)) {
          classType = OConstants.ALL_VALUES_FROM_RESTRICTION;
        }
        else if(sail.hasStatement(res, getURI(OWL.CARDINALITY), null)) {
          classType = OConstants.CARDINALITY_RESTRICTION;
        }
        else if(sail.hasStatement(res, getURI(OWL.MINCARDINALITY), null)) {
          classType = OConstants.MIN_CARDINALITY_RESTRICTION;
        }
        else if(sail.hasStatement(res, getURI(OWL.MAXCARDINALITY), null)) {
          classType = OConstants.MAX_CARDINALITY_RESTRICTION;
        }
      }
      
      if(classType == OConstants.OWL_CLASS) {
        if(res instanceof BNode)
          classType = OConstants.ANNONYMOUS_CLASS;
        else {
          // check if it is an instance
          if(isIndividual(null, resourceURI)) {
            classType = OConstants.INSTANCE;
          }
        }
      }

      return new ResourceInfo(resourceURI, classType);
    }
    return null;
  }

  /**
   * tells if the given URI is registered as an individual
   * @param repositoryID
   * @param individualURI
   * @return
   * @throws GateOntologyException
   */
  public boolean isIndividual(String repositoryID, String individualURI)
          throws GateOntologyException {
    if(debug) print("isIndividual");

    loadRepositoryDetails(repositoryID);
    String query = "Select X from {<" + individualURI
            + ">} rdf:type {X} rdf:type {<http://www.w3.org/2002/07/owl#Class>}";
    QueryResultsTable iter = performQuery(query);
    return iter.getRowCount() > 0;

  }  
  
  /**
   * Sets the cardinality value for the given restriction uri.
   * 
   * @param repositoryID
   * @param restrictionURI
   * @param restrictionType
   *          - either of the following constants from the OConstants -
   *          ALL_VALUES_FROM_RESTRICTION, SOME_VALUES_FROM_RESTRICTION, and
   *          HAS_VALUE_RESTRICTION
   * @param value
   * @return
   * @throws GateOntologyException
   */
  public void setRestrictionValue(String repositoryId, String restrictionURI,
    byte restrictionType, String value) throws GateOntologyException {

    if(debug) print("setRestrictionValue");
    loadRepositoryDetails(repositoryId);

    String whatValueURI = null;
    switch(restrictionType){
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
        throw new GateOntologyException("Invalid restriction type:"
          + restrictionType + " for the restriction " + restrictionURI);

    }

    StatementIterator iter =
      sail.getStatements(getResource(restrictionURI), getURI(whatValueURI),
        null);
    Statement toDelete = null;
    if(iter.hasNext()) {
      Statement stmt = iter.next();
      Value v = stmt.getObject();
      toDelete = stmt;
    }

    if(toDelete != null) {
      String objectString = toDelete.getObject().toString();
      removeUUUStatement(restrictionURI, whatValueURI, objectString);
      currentEventsLog.addEvent(new OEvent(restrictionURI, whatValueURI,
        objectString, false));
    }
    addUUUStatement(restrictionURI, whatValueURI, value);
    currentEventsLog.addEvent(new OEvent(restrictionURI, whatValueURI, value,
      true));
  }

  /**
   * This method tells what type of restriction the given uri refers to. If the
   * given URI is not a restriction, the method returns -1. Otherwise one of the
   * following values from the OConstants class. OWL_CLASS,
   * CARDINALITY_RESTRICTION, MIN_CARDINALITY_RESTRICTION,
   * MAX_CARDINALITY_RESTRICTION, HAS_VALUE_RESTRICTION,
   * ALL_VALUES_FROM_RESTRICTION.
   * 
   * @param repositoryID
   * @param restrictionURI
   * @return
   * @throws GateOntologyException
   */
  public byte getClassType(String repositoryID, String restrictionURI)
    throws GateOntologyException {

    loadRepositoryDetails(repositoryID);
    Resource res = getResource(restrictionURI);
    String rep1 = "<" + restrictionURI + ">";
    if(res instanceof BNode) {
      rep1 = "_:" + restrictionURI;
    }

    if(res instanceof BNode) {
      String query = "select * from {" + rep1 + "} owl:hasValue {B}";
      if(performQuery(query).getRowCount() > 0) { return OConstants.HAS_VALUE_RESTRICTION; }

      query = "select * from {" + rep1 + "} owl:someValuesFrom {B}";
      if(performQuery(query).getRowCount() > 0) { return OConstants.SOME_VALUES_FROM_RESTRICTION; }

      query = "select * from {" + rep1 + "} owl:allValuesFrom {B}";
      if(performQuery(query).getRowCount() > 0) { return OConstants.ALL_VALUES_FROM_RESTRICTION; }

      query = "select * from {" + rep1 + "} owl:cardinality {B}";
      if(performQuery(query).getRowCount() > 0) { return OConstants.CARDINALITY_RESTRICTION; }

      query = "select * from {" + rep1 + "} owl:minCardinality {B}";
      if(performQuery(query).getRowCount() > 0) { return OConstants.MIN_CARDINALITY_RESTRICTION; }

      query = "select * from {" + rep1 + "} owl:maxCardinality {B}";
      if(performQuery(query).getRowCount() > 0) { return OConstants.MAX_CARDINALITY_RESTRICTION; }
    }

    if(res instanceof BNode) {
      return OConstants.ANNONYMOUS_CLASS;
    }
    else {
      return OConstants.OWL_CLASS;
    }
  }

  public String executeQuery(String repositoryID, String serqlQuery)
    throws GateOntologyException {
    loadRepositoryDetails(repositoryID);
    QueryResultsTable resultsTable = null;
    try {
      resultsTable =
        currentRepository.performTableQuery(QueryLanguage.SERQL, serqlQuery);
    }
    catch(IOException e) {
      throw new GateOntologyException("Repository: " + repositoryID
        + "\nError ocurred while executing the query:\n" + serqlQuery, e);
    }
    catch(MalformedQueryException e) {
      throw new GateOntologyException("Repository: " + repositoryID
        + "\nError ocurred while executing the query:\n" + serqlQuery, e);
    }
    catch(QueryEvaluationException e) {
      throw new GateOntologyException("Repository: " + repositoryID
        + "\nError ocurred while executing the query:\n" + serqlQuery, e);
    }
    catch(AccessDeniedException e) {
      throw new GateOntologyException("Repository: " + repositoryID
        + "\nError ocurred while executing the query:\n" + serqlQuery, e);
    }
    return resultsTable.toString();
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
  public void addStatement(String repositoryID, String subjectURI,
    String predicateURI, String objectURI) throws GateOntologyException {
    loadRepositoryDetails(repositoryID);
    try {
      startTransaction(null);
      Resource s = subjectURI != null ? getResource(subjectURI) : null;
      URI p =
        predicateURI != null
          ? sail.getValueFactory().createURI(predicateURI)
          : null;
      Resource o = objectURI != null ? getResource(objectURI) : null;
      sail.addStatement(s, p, o);
      currentEventsLog.addEvent(new OEvent(subjectURI, predicateURI, objectURI,
        true));
      endTransaction(null);
    }
    catch(SailUpdateException e) {
      throw new GateOntologyException(
        "error while adding statement into the repository where subject:"
          + subjectURI + " predicate:" + predicateURI + " objectURI:"
          + objectURI, e);
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
  public void removeStatement(String repositoryID, String subjectURI,
    String predicateURI, String objectURI) throws GateOntologyException {
    loadRepositoryDetails(repositoryID);
    try {
      startTransaction(null);
      Resource s = subjectURI != null ? getResource(subjectURI) : null;
      URI p =
        predicateURI != null
          ? sail.getValueFactory().createURI(predicateURI)
          : null;
      Resource o = objectURI != null ? getResource(objectURI) : null;
      sail.removeStatements(s, p, o);
      currentEventsLog.addEvent(new OEvent(subjectURI, predicateURI, objectURI,
        false));
      endTransaction(null);
    }
    catch(SailUpdateException e) {
      throw new GateOntologyException(
        "error while removing statement from the repository where subject:"
          + subjectURI + " predicate:" + predicateURI + " objectURI:"
          + objectURI, e);
    }

  }

  // ***************************************************************************
  // *********************** Other Utility Methods
  // **************************************************************************
  private void addUUUStatement(String subject, String predicate, String object)
    throws GateOntologyException {
    try {
      startTransaction(null);
      Resource s = subject != null ? getResource(subject) : null;
      URI p =
        predicate != null ? sail.getValueFactory().createURI(predicate) : null;
      Resource o = object != null ? getResource(object) : null;
      sail.addStatement(s, p, o);
      endTransaction(null);
    }
    catch(SailUpdateException e) {
      throw new GateOntologyException(
        "error while adding statement into the repository where subject:"
          + subject + " predicate:" + predicate + " objectURI:" + object, e);
    }
  }

  private void addUULStatement(String subject, String predicate, String object,
    String language) throws GateOntologyException {
    try {
      startTransaction(null);
      Resource s = subject != null ? getResource(subject) : null;
      URI p =
        predicate != null ? sail.getValueFactory().createURI(predicate) : null;
      Literal o = null;
      if(language == null)
        o =
          object != null ? sail.getValueFactory().createLiteral(object) : null;
      else o =
        object != null
          ? sail.getValueFactory().createLiteral(object, language)
          : null;
      sail.addStatement(s, p, o);
      endTransaction(null);
    }
    catch(SailUpdateException e) {
      throw new GateOntologyException(
        "error while adding statement into the repository where subject:"
          + subject + " predicate:" + predicate + " objectURI:" + object, e);
    }
  }

  public void addStatement(String repositoryID, String subject,
    String predicate, String object, String datatype)
    throws GateOntologyException {
    try {
      loadRepositoryDetails(repositoryID);
      startTransaction(null);
      Resource s = subject != null ? getResource(subject) : null;
      URI p =
        predicate != null ? sail.getValueFactory().createURI(predicate) : null;
      URI d = sail.getValueFactory().createURI(datatype);
      Literal l =
        object != null ? sail.getValueFactory().createLiteral(object, d) : null;
      sail.addStatement(s, p, l);
      currentEventsLog.addEvent(new OEvent(subject, predicate, object,
        datatype, true));
      endTransaction(null);
    }
    catch(SailUpdateException e) {
      throw new GateOntologyException(
        "error while adding statement into the repository where subject:"
          + subject + " predicate:" + predicate + " objectURI:" + object, e);
    }
  }

  public void addUUDStatement(String repositoryID, String subject,
    String predicate, String object, String datatype)
    throws GateOntologyException {
    try {
      loadRepositoryDetails(repositoryID);
      startTransaction(null);
      Resource s = subject != null ? getResource(subject) : null;
      URI p =
        predicate != null ? sail.getValueFactory().createURI(predicate) : null;
      URI d = sail.getValueFactory().createURI(datatype);
      Literal l =
        object != null ? sail.getValueFactory().createLiteral(object, d) : null;
      sail.addStatement(s, p, l);
      endTransaction(null);
    }
    catch(SailUpdateException e) {
      throw new GateOntologyException(
        "error while adding statement into the repository where subject:"
          + subject + " predicate:" + predicate + " objectURI:" + object, e);
    }
  }

  private int removeUUUStatement(String subject, String predicate, String object)
    throws GateOntologyException {
    try {
      startTransaction(null);
      Resource s = subject != null ? getResource(subject) : null;
      URI p =
        predicate != null ? sail.getValueFactory().createURI(predicate) : null;
      Resource o = object != null ? getResource(object) : null;
      int no = sail.removeStatements(s, p, o);
      endTransaction(null);
      return no;
    }
    catch(SailUpdateException e) {
      throw new GateOntologyException(
        "error while removing statement from the repository where subject:"
          + subject + " predicate:" + predicate + " objectURI:" + object, e);
    }
  }

  private void removeUULStatement(String subject, String predicate,
    String object, String language) throws GateOntologyException {
    try {
      startTransaction(null);
      Resource s = subject != null ? getResource(subject) : null;
      URI p =
        predicate != null ? sail.getValueFactory().createURI(predicate) : null;
      Literal l = null;
      if(language == null) {
        l =
          object != null ? sail.getValueFactory().createLiteral(object) : null;
      }
      else {
        l =
          object != null ? sail.getValueFactory().createLiteral(object,
            language) : null;
      }
      sail.removeStatements(s, p, l);
      endTransaction(null);
    }
    catch(SailUpdateException e) {
      throw new GateOntologyException(
        "error while removing statement from the repository where subject:"
          + subject + " predicate:" + predicate + " objectURI:" + object, e);
    }
  }

  public void removeUUDStatement(String repositoryID, String subject,
    String predicate, String object, String datatype)
    throws GateOntologyException {
    try {
      loadRepositoryDetails(repositoryID);
      startTransaction(null);
      Resource s = subject != null ? getResource(subject) : null;
      URI p =
        predicate != null ? sail.getValueFactory().createURI(predicate) : null;
      URI d = sail.getValueFactory().createURI(datatype);
      Literal l =
        object != null ? sail.getValueFactory().createLiteral(object) : null;

      sail.removeStatements(s, p, l);

      l =
        object != null ? sail.getValueFactory().createLiteral(object, d) : null;
      sail.removeStatements(s, p, l);
      endTransaction(null);
    }
    catch(SailUpdateException e) {
      throw new GateOntologyException(
        "error while removing statement from the repository where subject:"
          + subject + " predicate:" + predicate + " objectURI:" + object, e);
    }
  }

  public void removeStatement(String repositoryID, String subject,
    String predicate, String object, String datatype)
    throws GateOntologyException {
    try {
      loadRepositoryDetails(repositoryID);
      startTransaction(null);
      Resource s = subject != null ? getResource(subject) : null;
      URI p =
        predicate != null ? sail.getValueFactory().createURI(predicate) : null;
      URI d = sail.getValueFactory().createURI(datatype);
      Literal l =
        object != null ? sail.getValueFactory().createLiteral(object) : null;

      sail.removeStatements(s, p, l);

      l =
        object != null ? sail.getValueFactory().createLiteral(object, d) : null;
      sail.removeStatements(s, p, l);
      currentEventsLog.addEvent(new OEvent(subject, predicate, object,
        datatype, false));
      endTransaction(null);
    }
    catch(SailUpdateException e) {
      throw new GateOntologyException(
        "error while removing statement from the repository where subject:"
          + subject + " predicate:" + predicate + " objectURI:" + object, e);
    }
  }

  public void startTransaction(String repositoryID)
    throws GateOntologyException {
    loadRepositoryDetails(repositoryID);
    if(!sail.transactionStarted()) sail.startTransaction();
  }

  public void endTransaction(String repositoryID) throws GateOntologyException {
    loadRepositoryDetails(repositoryID);
    if(sail.transactionStarted()) sail.commitTransaction();
  }

  public boolean transactionStarted(String repositoryID)
    throws GateOntologyException {
    loadRepositoryDetails(repositoryID);
    return sail.transactionStarted();
  }

  public void commitTransaction(String repositoryID)
    throws GateOntologyException {
    loadRepositoryDetails(repositoryID);

    if(sail != null && sail.transactionStarted()) {
      System.out.println("Commining Changes");
      // we need to commit all changes
      sail.commitTransaction();
    }
  }

  private Property[] listToPropertyArray(List<Property> list) {
    if(list == null) return null;
    ArrayList<Property> subList = new ArrayList<Property>();
    for(int i = 0; i < list.size(); i++) {
      if(hasSystemNameSpace(list.get(i).getUri())) continue;
      subList.add(list.get(i));
    }
    Property[] props = new Property[subList.size()];
    for(int i = 0; i < subList.size(); i++) {
      props[i] = subList.get(i);
    }
    return props;
  }

  private PropertyValue[] listToPropertyValueArray(List<PropertyValue> subList) {
    if(subList == null) return null;
    PropertyValue[] props = new PropertyValue[subList.size()];
    for(int i = 0; i < subList.size(); i++) {
      props[i] = subList.get(i);
    }
    return props;
  }

  private ResourceInfo[] listToResourceInfoArray(List<String> list) {
    if(list == null) return null;
    ArrayList<ResourceInfo> subList = new ArrayList<ResourceInfo>();
    for(int i = 0; i < list.size(); i++) {
      String resourceURI = list.get(i);
      if(hasSystemNameSpace(resourceURI)) continue;
      byte classType = getClassType(null, resourceURI);
      if(classType == OConstants.ANNONYMOUS_CLASS) continue;
      subList.add(new ResourceInfo(list.get(i).toString(), classType));
    }

    ResourceInfo[] strings = new ResourceInfo[subList.size()];
    for(int i = 0; i < subList.size(); i++) {
      strings[i] = subList.get(i);
    }
    return strings;
  }

  /**
   * This method tells whether the resource is imported or added as an explicit
   * statement.
   * 
   * @param repositoryID
   * @param resourceURI
   * @return
   */
  public boolean isImplicitResource(String repositoryID, String resourceURI)
    throws GateOntologyException {
    loadRepositoryDetails(repositoryID);
    return !sail.hasExplicitStatement(getResource(resourceURI),
      getURI(RDF.TYPE), null);
  }

  private String[] listToArray(List<String> list) {
    if(list == null) return null;
    ArrayList<String> subList = new ArrayList<String>();
    for(int i = 0; i < list.size(); i++) {
      if(hasSystemNameSpace(list.get(i))) continue;
      subList.add(list.get(i));
    }
    String[] strings = new String[subList.size()];
    for(int i = 0; i < subList.size(); i++) {
      strings[i] = subList.get(i);
    }
    return strings;
  }

  private URI getURI(String string) {
    Resource rs = resourcesMap.get(string);
    if(rs != null) return (URI)rs;
    rs = sail.getValueFactory().createURI(string);
    resourcesMap.put(string, rs);
    return (URI)rs;
  }

  private Resource getResource(String string) {
    Resource rs = resourcesMap.get(string);
    if(rs != null) return rs;

    try {
      rs = sail.getValueFactory().createURI(string);
      resourcesMap.put(string, rs);
    }
    catch(Exception e) {
      rs = sail.getValueFactory().createBNode(string);
      resourcesMap.put(string, rs);
    }
    return rs;
  }

  private int getUserID(String username, String password) {
    List userInfos = service.getSystemConfig().getUserInfoList();
    for(int i = 0; i < userInfos.size(); i++) {
      UserInfo userInfo = (UserInfo)userInfos.get(i);
      if(userInfo.getLogin().equals(username)
        && userInfo.getPassword().equals(password)) { return userInfo.getID(); }
    }
    return -1;
  }

  private int createUser(String username, String password)
    throws GateOntologyException {
    int counter = service.getSystemConfig().getUnusedUserId();
    service.getSystemConfig().addUser(counter, username, username, password);
    return counter;
  }

  private void saveConfiguration() throws GateOntologyException {
    try {
      if(systemConf == null) return;
      if(debug) System.out.println("System conf : " + systemConf);
      OutputStream os =
        new BufferedOutputStream(new FileOutputStream(systemConf));
      SystemConfigFileHandler.writeConfiguration(service.getSystemConfig(), os);
      os.close();
    }
    catch(IOException e) {
      throw new GateOntologyException("Error while saving configuration file",
        e);
    }
  }

  /*
   * private SystemConfig readConfiguration() throws IOException { if(systemConf
   * == null) { String s = "<?xml version='1.0'?>" + "<system-conf>" + "<admin
   * password=''/>" + "<log dir='plugins/Ontology_Tools/logs' level='3'/>" +
   * "<tmp dir='plugins/Ontology_Tools/tmp'/>" + "<rmi-factory enabled='false'
   * class='com.ontotext.util.rmi.CustomRMIFactory' port='1099'/>" +
   * "<userlist>" + "<user id='1'
   * login='admin'><fullname>Admin</fullname><password>admin</password></user>"
   * + "<user id='3'
   * login='guest'><fullname>Guest</fullname><password>guest</password></user>"
   * + "</userlist>" + "<repositorylist></repositorylist></system-conf>"; Reader
   * reader = new StringReader(s); SystemConfig config =
   * SystemConfigFileHandler.readConfiguration(reader); reader.close(); return
   * config; } else { Reader reader = new BufferedReader(new
   * InputStreamReader(systemConf .openStream())); SystemConfig config =
   * SystemConfigFileHandler.readConfiguration(reader); reader.close(); return
   * config; } }
   */

  /**
   * given a string, return the equivalent RDFFormat
   * 
   * @param format
   * @return
   */
  private RDFFormat getRDFFormat(byte format) {
    switch((int)format){
      case OConstants.ONTOLOGY_FORMAT_N3:
        return RDFFormat.N3;
      case OConstants.ONTOLOGY_FORMAT_NTRIPLES:
        return RDFFormat.NTRIPLES;
      case OConstants.ONTOLOGY_FORMAT_RDFXML:
        return RDFFormat.RDFXML;
      default:
        return RDFFormat.TURTLE;
    }
  }

  private Property createPropertyObject(String repositoryID, String uri)
    throws GateOntologyException {
    byte type = OConstants.ANNOTATION_PROPERTY;
    if(isAnnotationProperty(repositoryID, uri)) {
      type = OConstants.ANNOTATION_PROPERTY;
    } else if(isObjectProperty(repositoryID, uri)) {
      type = OConstants.OBJECT_PROPERTY;
    }
    else if(isDatatypeProperty(null, uri)) {
      type = OConstants.DATATYPE_PROPERTY;
    }
    else if(isTransitiveProperty(null, uri)) {
      type = OConstants.TRANSITIVE_PROPERTY;
    }
    else if(isSymmetricProperty(null, uri)) {
      type = OConstants.SYMMETRIC_PROPERTY;
    }
    else if(sail.isProperty(getResource(uri))) {
      type = OConstants.RDF_PROPERTY;
    }
    else {
      return null;
    }
    return new Property(type, uri);
  }

  private void print(String methodName) {
    System.out.println(methodName + " called.");
  }

  /**
   * Set the provided repository as a current repository
   * 
   * @param repositoryID
   * @param ontoFileUrl
   * @param baseURI
   * @param format
   * @param persist
   * @return
   */
  private boolean setRepository(String repositoryID, String ontoFileUrl,
    boolean isOntologyData, String baseURI, byte format,
    String absolutePersistLocation, boolean persist) {
    // check if repository exists
    if(debug) print("setRepository");
    boolean found = false;
    try {
      setCurrentRepositoryID(repositoryID);
      RepositoryConfig repConfig =
        service.getSystemConfig().getRepositoryConfig(repositoryID);
      if(repConfig != null) {
        // lets find out the new import values those have come through
        // the new
        // ontoFileUrl
        Set<String> importValues =
          getImportValues(repositoryID, ontoFileUrl, baseURI, format,
            absolutePersistLocation, isOntologyData, new HashSet<String>());
        SailConfig syncSail = repConfig.getSail(OWLIM_SCHEMA_REPOSITORY_CLASS);
        if(syncSail != null) {
          String formatToUse = "ntriples";
          switch(format){
            case OConstants.ONTOLOGY_FORMAT_N3:
              formatToUse = "n3";
              break;
            case OConstants.ONTOLOGY_FORMAT_NTRIPLES:
              formatToUse = "ntriples";
              break;
            case OConstants.ONTOLOGY_FORMAT_TURTLE:
              formatToUse = "turtle";
              break;
            default:
              formatToUse = "rdfxml";
              break;
          }
          Map map = syncSail.getConfigParameters();
          if(map == null) map = new HashMap();
          map.put("noPersist", Boolean.toString(!persist));
          map.put("compressFile", "no");
          map.put("dataFormat", "ntriples");
          String imports = (String)map.get("imports");
          String defaultNS = (String)map.get("defaultNS");
          if(imports == null) imports = "";
          if(defaultNS == null) defaultNS = "";
          if(imports.length() > 0) {
            for(String imValue : importValues) {
              imports += ";" + imValue;
              defaultNS += ";" + imValue + "#";
            }
          }
          else {
            imports = owlRDFS.toExternalForm();
            defaultNS = "http://www.w3.org/2002/07/owl#";
          }
          map.put("imports", imports);
          map.put("defaultNS", defaultNS);
          File systemConfFile = null;

          if(systemConf == null) {
            systemConfFile = new File("temp");
          }
          else {
            systemConfFile = systemConf;
          }

          String persistFile =
            absolutePersistLocation == null
              || absolutePersistLocation.trim().length() == 0 ? new File(
              systemConfFile.getParentFile(), repositoryID + ".nt")
              .getAbsolutePath() : new File(new File(absolutePersistLocation),
              repositoryID + ".nt").getAbsolutePath();
          String tempTripplesFile =
            absolutePersistLocation == null
              || absolutePersistLocation.trim().length() == 0 ? new File(
              systemConfFile.getParentFile(), repositoryID + "-tripples.nt")
              .getAbsolutePath() : new File(new File(absolutePersistLocation),
              repositoryID + "-tripples.nt").getAbsolutePath();
          map.put("file", persistFile);
          map.put("new-triples-file", tempTripplesFile);
          map.put("auto-write-time-minutes", "0");
          syncSail.setConfigParameters(map);
        }
      }
      if(ontoFileUrl != null && ontoFileUrl.trim().length() != 0) {
        currentRepository.addData(ontoFileUrl, baseURI, getRDFFormat(format),
          true, this);
        if(persist) saveConfiguration();
        if(debug) System.out.println("Data added!");
      }
      found = true;
    }
    catch(AccessDeniedException exception) {
      // repository doesn't exist
      // lets create one
      if(debug) exception.printStackTrace();
      found = false;
    }
    catch(IOException ioe) {
      if(debug) ioe.printStackTrace();
      found = false;
    }
    catch(GateOntologyException goe) {
      if(debug) goe.printStackTrace();
      found = false;
    }
    return found;
  }

  private HashSet<String> getImportValues(String currentRepository,
    String ontoFileUrl, String baseURI, byte format,
    String absolutePersistLocation, boolean isOntologyData,
    Set<String> parsedValues) {
    if(!isOntologyData) System.out.println("Importing : " + ontoFileUrl);
    String baseUrl = ontoFileUrl;
    ArrayList<String> toReturn = new ArrayList<String>();
    try {
      String dummyRepository = "dummy" + Math.random();
      createNewRepository(dummyRepository, ontoFileUrl, isOntologyData,
        baseURI, false, absolutePersistLocation, "admin", "admin", format, true);
      setCurrentRepositoryID(dummyRepository);
      if(ontoFileUrl != null && ontoFileUrl.trim().length() != 0) {
        if(isOntologyData) {
          this.currentRepository.addData(ontoFileUrl, baseURI,
            getRDFFormat(format), true, this);
        }
        else if(ontoFileUrl.startsWith("file:")) {
          this.currentRepository.addData(new File(new URL(ontoFileUrl)
            .getFile()), baseURI, getRDFFormat(format), true, this);
        }
        else {
          this.currentRepository.addData(new URL(ontoFileUrl), baseURI,
            getRDFFormat(format), true, this);
        }
        StatementIterator si =
          sail.getStatements(null, getURI(OWL.IMPORTS), null);
        si = sail.getStatements(null, getURI(OWL.IMPORTS), null);
        if(si.hasNext()) {
          ontoFileUrl = si.next().getSubject().toString();
          this.ontologyUrl = ontoFileUrl;
        }
        else {
          ontoFileUrl = null;
        }
        if(ontoFileUrl != null) {
          PropertyValue[] values =
            getPropertyValues(dummyRepository, ontoFileUrl, OWL.IMPORTS);

          for(int i = 0; i < values.length; i++) {
            String fileName = values[i].getValue();
            if(fileName
              .equalsIgnoreCase("http://www.w3.org/2000/01/rdf-schema")
              || fileName.equalsIgnoreCase("www.w3.org/2000/01/rdf-schema")) {
              fileName = rdfSchema.toExternalForm();
            }

            String originalFileName = fileName;
            boolean foundFile = false;

            // lets try to find out the file on local system
            int m = 0;
            boolean allFound = true;
            for(; m < fileName.length() && m < ontoFileUrl.length(); m++) {
              if(fileName.charAt(m) != ontoFileUrl.charAt(m)) {
                allFound = false;
                break;
              }
            }

            if(!allFound && m >= 1) {

              // lets find out the index of the last /
              String tempFileName = fileName.substring(0, m);
              m = tempFileName.lastIndexOf('/') + 1;
              if(fileName.charAt(m - 1) == '/') {
                int counter = 1;
                for(int k = m; k < ontoFileUrl.length(); k++) {
                  if(ontoFileUrl.charAt(k) == '/') {
                    counter++;
                  }
                }
                boolean continueProcessing = true;
                String tempString = baseUrl;
                for(int k = 0; k < counter; k++) {
                  int tempIndex = tempString.lastIndexOf('/');
                  if(tempIndex != -1) {
                    tempString = tempString.substring(0, tempIndex);
                  }
                  else {
                    continueProcessing = false;
                    break;
                  }
                }

                if(continueProcessing) {
                  fileName =
                    tempString + fileName.substring(m - 1, fileName.length());

                  fileName =
                    new File(new URL(fileName).getFile()).getAbsolutePath();
                  // lets normalize the name by replacing .. in the path
                  // file://abc/xyz/../../pqr

                  while(true) {
                    int index = fileName.indexOf("/../");
                    if(index == -1) break;
                    tempString = fileName.substring(0, index);
                    int index1 = tempString.lastIndexOf("/");
                    if(index1 == -1) {
                      continueProcessing = false;
                      break;
                    }
                    tempString = tempString.substring(0, index1);
                    fileName =
                      tempString
                        + fileName.substring(index + 3, fileName.length());
                  }

                  if(continueProcessing) {
                    fileName =
                      new File(fileName).toURI().toURL().toExternalForm();
                    if(parsedValues.contains(fileName)) continue;
                    try {
                      // lets check if this file exists
                      if(!new File(new URL(fileName).toURI()).exists()) {
                        // do nothing
                      }
                      else {
                        toReturn.add(fileName);
                        foundFile = true;
                        if(debug)
                          System.out.println("\t File To Be Imported : "
                            + fileName);
                      }
                    }
                    catch(URISyntaxException use) {
                      // do nothing
                    }
                  }
                }
              }
            }

            if(!foundFile) {
              // here we check what user has provided is a valid URL or
              // a relative path
              try {
                System.out.println(originalFileName);
                new URL(originalFileName).openStream();
                if(parsedValues.contains(originalFileName)) continue;
                if(debug)
                  System.out.println("\t URL To Be Imported : "
                    + originalFileName);
                toReturn.add(originalFileName);
                continue;
              }
              catch(MalformedURLException e) {
                throw new GateOntologyException("Invalid Import URL"
                  + originalFileName);
              }
              catch(IOException ioe) {
                throw new GateOntologyException("Invalid Import URL"
                  + originalFileName);
              }
            }
          }
        }
      }
      removeRepository(dummyRepository, false);
      if(currentRepository != null) loadRepositoryDetails(currentRepository);
    }
    catch(AccessDeniedException e) {
      if(debug) e.printStackTrace();
    }
    catch(IOException ioe) {
      if(debug) ioe.printStackTrace();
    }

    HashSet<String> finallyToReturn = new HashSet<String>();
    finallyToReturn.addAll(toReturn);
    parsedValues.addAll(toReturn);
    isOntologyData = false;
    for(String value : toReturn) {
      finallyToReturn.addAll(getImportValues(currentRepository, value, baseURI,
        OConstants.ONTOLOGY_FORMAT_RDFXML, absolutePersistLocation,
        isOntologyData, parsedValues));
    }

    return finallyToReturn;
  }

  /**
   * Creates a new user
   * 
   * @param username
   * @param password
   * @return
   * @throws GateOntologyException
   */
  private int createNewUser(String username, String password)
    throws GateOntologyException {
    if(debug) print("createUser");
    int id = -1;
    if(username != null) {
      id = getUserID(username, password);
      // user does not exist create a new user
      if(id == -1) {
        id = createUser(username, password);
      }
    }
    return id;
  }

  /**
   * Creates a new Repository
   * 
   * @param repositoryID
   * @param baseURI
   * @param persist
   * @param username
   * @param password
   * @return
   * @throws GateOntologyException
   */
  private RepositoryConfig createNewRepository(String repositoryID,
    String ontoFileUrl, boolean isOntologyData, String baseURI,
    boolean persist, String absolutePersistLocation, String username,
    String password, byte format, boolean isDummyRepository)
    throws GateOntologyException {

    RepositoryConfig repConfig;
    try {
      // we create a new repository
      //ClassLoader savedContextLoader =
      //   Thread.currentThread().getContextClassLoader();
      //try {
      //   Thread.currentThread().setContextClassLoader(gate.Gate.getClassLoader());
         repConfig = new RepositoryConfig(repositoryID);
      //} finally {
      //  Thread.currentThread().setContextClassLoader(savedContextLoader);
     // }

      repConfig.setTitle(repositoryID);
      SailConfig syncSail = new SailConfig(OWLIM_SCHEMA_REPOSITORY_CLASS);
      Map map = syncSail.getConfigParameters();
      if(map == null) {
        map = new HashMap();
      }
      String formatToUse = "ntriples";
      switch(format){
        case OConstants.ONTOLOGY_FORMAT_N3:
          formatToUse = "n3";
          break;
        case OConstants.ONTOLOGY_FORMAT_NTRIPLES:
          formatToUse = "ntriples";
          break;
        case OConstants.ONTOLOGY_FORMAT_TURTLE:
          formatToUse = "turtle";
          break;
        default:
          formatToUse = "rdfxml";
          break;
      }
      String imports = owlRDFS.toExternalForm();
      String defaultNS = "http://www.w3.org/2002/07/owl#";
      if(!isDummyRepository) {
        Set<String> importValues =
          getImportValues(null, ontoFileUrl, baseURI, format,
            absolutePersistLocation, isOntologyData, new HashSet<String>());
        for(String imValue : importValues) {
          imports += ";" + imValue;
          defaultNS += ";" + imValue + "#";
        }
      }
      map.put("imports", imports);
      map.put("defaultNS", defaultNS);
      map.put("ruleset", "owl-max");
      map.put("partialRDFS", "true");
      map.put("dropOnRemove", "false");
      map.put("base-URL", baseURI);
      map.put("indexSize", "100000");
      map.put("stackSafe", "true");
      map.put("noPersist", Boolean.toString(!persist));
      map.put("compressFile", "no");
      map.put("dataFormat", "ntriples");

      File systemConfFile = null;

      if(systemConf == null) {
        systemConfFile = new File("temp");
      }
      else {
        systemConfFile = systemConf;
      }

      String persistFile =
        absolutePersistLocation == null
          || absolutePersistLocation.trim().length() == 0 ? new File(
          systemConfFile.getParentFile(), repositoryID + ".nt")
          .getAbsolutePath() : new File(new File(absolutePersistLocation),
          repositoryID + ".nt").getAbsolutePath();
      String tempTripplesFile =
        absolutePersistLocation == null
          || absolutePersistLocation.trim().length() == 0 ? new File(
          systemConfFile.getParentFile(), repositoryID + "-tripples.nt")
          .getAbsolutePath() : new File(new File(absolutePersistLocation),
          repositoryID + "-tripples.nt").getAbsolutePath();
      map.put("file", persistFile);
      map.put("new-triples-file", tempTripplesFile);
      map.put("auto-write-time-minutes", "0");
      syncSail.setConfigParameters(map);
      repConfig.addSail(syncSail);
      repConfig.setWorldReadable(true);
      repConfig.setWorldWriteable(true);
//      ClassLoader savedContextLoader2 =
//Thread.currentThread().getContextClassLoader();
//try {
//  Thread.currentThread().setContextClassLoader(gate.Gate.getClassLoader());

      service.createRepository(repConfig);
//      }
//finally {
//  Thread.currentThread().setContextClassLoader(savedContextLoader);
//}
      setCurrentRepositoryID(repositoryID);
      if(username != null) {
        service.getSystemConfig().setReadAccess(repositoryID, username, true);
        service.getSystemConfig().setWriteAccess(repositoryID, username, true);
      }
      return repConfig;
    }
    catch(ConfigurationException ce) {
      throw new GateOntologyException(
        "error while creating a new repository because " + ce.getMessage(), ce);
    }
  }

  /**
   * Adds ontology Data
   * 
   * @param ontoFileUrl
   * @param baseURI
   * @param format
   * @throws GateOntologyException
   */
  private void addOntologyData(String repositoryID, String ontoFileUrl,
    boolean isOntologyData, String baseURI, byte format)
    throws GateOntologyException {
    try {
      if(ontoFileUrl != null && ontoFileUrl.trim().length() != 0) {
        boolean findURL = false;
        if(isOntologyData) {
          currentRepository.addData(ontoFileUrl, baseURI, getRDFFormat(format),
            true, this);

          findURL = true;
        }
        else if(ontoFileUrl.startsWith("file:")) {
          currentRepository.addData(new File(new URL(ontoFileUrl).getFile()),
            baseURI, getRDFFormat(format), true, this);
          findURL = true;
        }
        else {
          currentRepository.addData(new URL(ontoFileUrl), baseURI,
            getRDFFormat(format), true, this);
        }
        StatementIterator si =
          sail.getStatements(null, getURI(OWL.IMPORTS), null);
        if(si.hasNext()) {
          ontoFileUrl = si.next().getSubject().toString();
          this.ontologyUrl = ontoFileUrl;
        }
        else {
          ontoFileUrl = null;
        }
        if(debug) System.out.println("Data added!");
      }
    }
    catch(IOException e) {
      throw new GateOntologyException(
        "error while adding ontology data into the repository because "
          + e.getMessage(), e);
    }
    catch(AccessDeniedException ade) {
      throw new GateOntologyException(
        "error while adding ontology data into the repository because "
          + ade.getMessage(), ade);
    }
  }

  /**
   * This method is used to obtain the most specific classes
   * 
   * @param repositoryID
   * @param values
   * @return
   * @throws GateOntologyException
   */
  private ResourceInfo[] reduceToMostSpecificClasses(String repositoryID,
    List<ResourceInfo> values) throws GateOntologyException {
    if(values == null || values.isEmpty()) return new ResourceInfo[0];
    List<String> classes = new ArrayList<String>();
    for(int i = 0; i < values.size(); i++) {
      classes.add(values.get(i).getUri());
    }
    outer: for(int i = 0; i < classes.size(); i++) {
      String c = classes.get(i);
      // if the class's children appear in list, it is not the most
      // specific class

      Resource r = getResource(c);
      String queryRep = "{<" + c + ">}";
      String queryRep1 = "<" + c + ">";
      if(r instanceof BNode) {
        queryRep = "{_:" + c + "}";
        queryRep1 = "_:" + c;
      }

      String query =
        "select distinct A FROM {A} rdfs:subClassOf " + queryRep + " WHERE A!="
          + queryRep1 + " MINUS "
          + " select distinct B FROM {B} owl:equivalentClass " + queryRep;

      QueryResultsTable iter = performQuery(query);
      List<String> list = new ArrayList<String>();
      for(int j = 0; j < iter.getRowCount(); j++) {
        list.add(iter.getValue(j, 0).toString());
      }

      for(int j = 0; j < list.size(); j++) {
        if(classes.contains(list.get(j))) {
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
   * @param repositoryID
   * @param values
   * @return
   * @throws GateOntologyException
   */
  private List<String> reduceToMostSpecificClasses(String repositoryID,
    Set<String> values) throws GateOntologyException {
    if(values == null || values.isEmpty()) return new ArrayList<String>();
    List<String> classes = new ArrayList<String>(values);
    outer: for(int i = 0; i < classes.size(); i++) {
      String c = classes.get(i);
      // if the class's children appear in list, it is not the most
      // specific class

      Resource r = getResource(c);
      String queryRep = "{<" + c + ">}";
      String queryRep1 = "<" + c + ">";
      if(r instanceof BNode) {
        queryRep = "{_:" + c + "}";
        queryRep1 = "_:" + c;
      }

      String query =
        "select distinct A FROM {A} rdfs:subClassOf " + queryRep + " WHERE A!="
          + queryRep1 + " MINUS "
          + " select distinct B FROM {B} owl:equivalentClass " + queryRep;

      QueryResultsTable iter = performQuery(query);
      List<String> list = new ArrayList<String>();
      for(int j = 0; j < iter.getRowCount(); j++) {
        list.add(iter.getValue(j, 0).toString());
      }

      for(int j = 0; j < list.size(); j++) {
        if(classes.contains(list.get(j))) {
          classes.remove(i);
          i--;
          continue outer;
        }
      }
    }
    return classes;
  }

  private void loadRepositoryDetails(String repositoryID)
    throws GateOntologyException {
    if(sail != null && repositoryID == null) return;
    RepositoryDetails rd = mapToRepositoryDetails.get(repositoryID);
    if(rd == null) { throw new GateOntologyException("Repository :"
      + repositoryID + " does not exist"); }
    if(currentRepository == rd.repository) return;

    currentRepository = rd.repository;
    currentEventsLog = rd.eventsLog;
    sail = rd.sail;
    ontologyUrl = rd.ontologyUrl;
    returnSystemStatements = rd.returnSystemStatements;
  }

  private byte getPropertyType(String repositoryID, String aPropertyURI)
    throws GateOntologyException {
    if(isDatatypeProperty(repositoryID, aPropertyURI))
      return OConstants.DATATYPE_PROPERTY;
    else if(isTransitiveProperty(null, aPropertyURI))
      return OConstants.TRANSITIVE_PROPERTY;
    else if(isSymmetricProperty(null, aPropertyURI))
      return OConstants.SYMMETRIC_PROPERTY;
    else if(isObjectProperty(null, aPropertyURI))
      return OConstants.OBJECT_PROPERTY;
    else if(isAnnotationProperty(null, aPropertyURI))
      return OConstants.ANNOTATION_PROPERTY;
    else return OConstants.RDF_PROPERTY;
  }

  private PropertyValue[] getPropertyValues(String repositoryID,
    String aResourceURI, String aPropertyURI) throws GateOntologyException {
    if(debug) print("getPropertyValues");
    loadRepositoryDetails(repositoryID);

    Resource r = getResource(aResourceURI);
    String rep1 = "<" + aResourceURI + ">";
    String rep2 = "{" + rep1 + "}";
    if(r instanceof BNode) {
      rep1 = "_:" + aResourceURI;
      rep2 = "{" + rep1 + "}";
    }
    String query =
      "Select DISTINCT Y from " + rep2 + " <" + aPropertyURI + "> {Y}";
    QueryResultsTable qrt = performQuery(query);
    List<PropertyValue> list = new ArrayList<PropertyValue>();
    for(int i = 0; i < qrt.getRowCount(); i++) {
      list.add(new PropertyValue(String.class.getName(), qrt.getValue(i, 0)
        .toString()));
    }
    return listToPropertyValueArray(list);
  }

  public boolean hasSystemNameSpace(String uri) {
    if(returnSystemStatements) return false;
    Boolean val = hasSystemNameSpace.get(uri);
    if(val == null) {
      val = new Boolean(Utils.hasSystemNameSpace(uri));
      hasSystemNameSpace.put(uri, val);
    }
    return val.booleanValue();
  }

  class RepositoryDetails {

    public RepositoryDetails() {
      eventsLog = new OntologyEventsLog();
    }

    /**
     * OWLIMSchemaRepository is used as an interaction layer on top of Sesame
     * server. The class provides various methods of manipulating ontology data.
     */
    OWLIMSchemaRepository sail;

    /**
     * The reference of currently selected repository is stored in this variable
     */
    SesameRepository repository;

    OntologyEventsLog eventsLog;

    /**
     * Ontology URL
     */
    private String ontologyUrl;

    /**
     * Whether to return system statements
     */
    private boolean returnSystemStatements;
  }

  public SesameRepository getSesameRepository(String repositoryID)
    throws GateOntologyException {
    loadRepositoryDetails(repositoryID);
    return currentRepository;
  }
}
