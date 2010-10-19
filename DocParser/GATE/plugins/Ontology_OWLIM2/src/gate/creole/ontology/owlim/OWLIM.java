package gate.creole.ontology.owlim;

import gate.creole.ontology.GateOntologyException;

import javax.jws.WebMethod;
import javax.jws.WebParam;
import javax.jws.WebResult;
import javax.jws.WebService;
import javax.xml.ws.ResponseWrapper;

@WebService(targetNamespace = "http://gate.ac.uk/ns/ontology/owlim")
public interface OWLIM extends java.rmi.Remote {

  /**
   * This method reports the events observed. Each event is a tupple consists of the following:
   * + or - that indicates addition or removal of the tripple.
   * subject - URI or * if all 
   * predicate - URI or * if all
   * object - URI or * if all
   * datatype -  URI or * if all
   * @param repositoryID
   * @return
   * @throws GateOntologyException
   */
  @WebMethod
  @WebResult(targetNamespace = "http://gate.ac.uk/ns/ontology/owlim")
  @ResponseWrapper(className = "gate.creole.ontology.owlim.StringArrayResponse")
  public String[] getEventsLog(
          @WebParam(name = "repositoryID") String repositoryID)
          throws GateOntologyException;
  
  /**
   * Gets the default name space for this ontology. The defaultNameSpace
   * is (by default) used for the newly created resources.
   * 
   * @return a String value.
   */
  @WebMethod
  @WebResult(targetNamespace = "http://gate.ac.uk/ns/ontology/owlim")
  public String getDefaultNameSpace(
          @WebParam(name = "repositoryID") String repositoryID)
          throws GateOntologyException;

  /**
   * Adds the ontology data
   * 
   * @param repositoryID
   * @param data
   * @param baseURI
   * @param format
   * @throws GateOntologyException
   */
  @WebMethod
  @WebResult(targetNamespace = "http://gate.ac.uk/ns/ontology/owlim")
  public void addOntologyData(
          @WebParam(name = "repositoryID") String repositoryID,
          @WebParam(name = "data") String data,
          @WebParam(name = "baseURI") String baseURI,
          @WebParam(name = "format") byte format)
          throws GateOntologyException;

  /**
   * This method tells whether the resource is imported or added as an explicit statement.
   * @param repositoryID
   * @param resourceID
   * @return
   */
  @WebMethod
  @WebResult(targetNamespace = "http://gate.ac.uk/ns/ontology/owlim")
  public boolean isImplicitResource(
          @WebParam(name = "repositoryID") String repositoryID,
          @WebParam(name = "resourceID") String resourceID)
          throws GateOntologyException ;
  
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
  @WebMethod
  @WebResult(targetNamespace = "http://gate.ac.uk/ns/ontology/owlim")
  public boolean isSuperClassOf(
          @WebParam(name = "repositoryID") String repositoryID,
          @WebParam(name = "theSuperClassURI") String theSuperClassURI,
          @WebParam(name = "theSubClassURI") String theSubClassURI,
          @WebParam(name = "direct") byte direct)
          throws GateOntologyException;

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
  @WebMethod
  @WebResult(targetNamespace = "http://gate.ac.uk/ns/ontology/owlim")
  public boolean isSubClassOf(
          @WebParam(name = "repositoryID") String repositoryID,
          @WebParam(name = "theSuperClassURI") String theSuperClassURI,
          @WebParam(name = "theSubClassURI") String theSubClassURI,
          @WebParam(name = "direct") byte direct)
          throws GateOntologyException;

  /**
   * Given a property URI, this method returns an object of Property
   * 
   * @param repositoryID
   * @param thePropertyURI
   * @return
   * @throws GateOntologyException
   */
  @WebMethod
  @WebResult(targetNamespace = "http://gate.ac.uk/ns/ontology/owlim")
  public Property getPropertyFromOntology(
          @WebParam(name = "repositoryID") String repositoryID,
          @WebParam(name = "thePropertyURI") String thePropertyURI)
          throws GateOntologyException;

  /**
   * Checks whether the two classes defined as same in the ontology.
   * 
   * @param theClassURI1
   * @param theClassURI2
   * @return
   * @throws Exception
   */
  @WebMethod
  @WebResult(targetNamespace = "http://gate.ac.uk/ns/ontology/owlim")
  public boolean isEquivalentClassAs(
          @WebParam(name = "repositoryID") String repositoryID,
          @WebParam(name = "theClassURI1") String theClassURI1,
          @WebParam(name = "theClassURI2") String theClassURI2)
          throws GateOntologyException;

  // *******************************************************************
  // property methods
  // *******************************************************************
  // **************
  // Annotation Property
  // ************
  /**
   * Creates a new AnnotationProperty.
   * 
   * @param aPropertyURI URI of the property to be added into the
   *          ontology. Done
   */
  @WebMethod
  @WebResult(targetNamespace = "http://gate.ac.uk/ns/ontology/owlim")
  public void addAnnotationProperty(
          @WebParam(name = "repositoryID") String repositoryID,
          @WebParam(name = "aPropertyURI") String aPropertyURI)
          throws GateOntologyException;

  /**
   * Gets the annotation properties set on the specified resource
   * 
   * @param repositoryID
   * @param theResourceURI
   * @return
   * @throws GateOntologyException
   */
  @WebMethod(operationName = "getAnnotationPropertiesForResource")
  @WebResult(targetNamespace = "http://gate.ac.uk/ns/ontology/owlim")
  @ResponseWrapper(className = "gate.creole.ontology.owlim.PropertyArrayResponse")
  public Property[] getAnnotationProperties(
          @WebParam(name = "repositoryID") String repositoryID,
          @WebParam(name = "theResourceURI") String theResourceURI)
          throws GateOntologyException;

  /**
   * Gets the RDF properties set on the specified resource
   * 
   * @param repositoryID
   * @param theResourceURI
   * @return
   * @throws GateOntologyException
   */
  @WebMethod(operationName = "getRDFPropertiesForResource")
  @WebResult(targetNamespace = "http://gate.ac.uk/ns/ontology/owlim")
  @ResponseWrapper(className = "gate.creole.ontology.owlim.PropertyArrayResponse")
  public Property[] getRDFProperties(
          @WebParam(name = "repositoryID") String repositoryID,
          @WebParam(name = "theResourceURI") String theResourceURI)
          throws GateOntologyException;

  /**
   * Gets the datatype properties set on the specified resource
   * 
   * @param repositoryID
   * @param theResourceURI
   * @return
   * @throws GateOntologyException
   */
  @WebMethod(operationName = "getDatatypePropertiesForResource")
  @WebResult(targetNamespace = "http://gate.ac.uk/ns/ontology/owlim")
  @ResponseWrapper(className = "gate.creole.ontology.owlim.PropertyArrayResponse")
  public Property[] getDatatypeProperties(
          @WebParam(name = "repositoryID") String repositoryID,
          @WebParam(name = "theResourceURI") String theResourceURI)
          throws GateOntologyException;

  /**
   * Gets the object properties set on the specified resource
   * 
   * @param repositoryID
   * @param theResourceURI
   * @return
   * @throws GateOntologyException
   */
  @WebMethod(operationName = "getObjectPropertiesForResource")
  @WebResult(targetNamespace = "http://gate.ac.uk/ns/ontology/owlim")
  @ResponseWrapper(className = "gate.creole.ontology.owlim.PropertyArrayResponse")
  public Property[] getObjectProperties(
          @WebParam(name = "repositoryID") String repositoryID,
          @WebParam(name = "theResourceURI") String theResourceURI)
          throws GateOntologyException;

  /**
   * Gets the transitive properties set on the specified resource
   * 
   * @param repositoryID
   * @param theResourceURI
   * @return
   * @throws GateOntologyException
   */
  @WebMethod(operationName = "getTransitivePropertiesForResource")
  @WebResult(targetNamespace = "http://gate.ac.uk/ns/ontology/owlim")
  @ResponseWrapper(className = "gate.creole.ontology.owlim.PropertyArrayResponse")
  public Property[] getTransitiveProperties(
          @WebParam(name = "repositoryID") String repositoryID,
          @WebParam(name = "theResourceURI") String theResourceURI)
          throws GateOntologyException;

  /**
   * Gets the symmetric properties set on the specified resource
   * 
   * @param repositoryID
   * @param theResourceURI
   * @return
   * @throws GateOntologyException
   */
  @WebMethod(operationName = "getSymmetricPropertiesForResource")
  @WebResult(targetNamespace = "http://gate.ac.uk/ns/ontology/owlim")
  @ResponseWrapper(className = "gate.creole.ontology.owlim.PropertyArrayResponse")
  public Property[] getSymmetricProperties(
          @WebParam(name = "repositoryID") String repositoryID,
          @WebParam(name = "theResourceURI") String theResourceURI)
          throws GateOntologyException;

  /**
   * returns if the given property is an Annotation property
   * 
   * @param aPropertyURI
   * @return Done
   */
  @WebMethod
  @WebResult(targetNamespace = "http://gate.ac.uk/ns/ontology/owlim")
  public boolean isAnnotationProperty(
          @WebParam(name = "repositoryID") String repositoryID,
          @WebParam(name = "aPropertyURI") String aPropertyURI)
          throws GateOntologyException;

  /**
   * Adds a new annotation property value and specifies the language.
   * 
   * @param theAnnotationProperty the annotation property
   * @param value the value containing some value
   * @return
   */
  @WebMethod
  @WebResult(targetNamespace = "http://gate.ac.uk/ns/ontology/owlim")
  public void addAnnotationPropertyValue(
          @WebParam(name = "repositoryID") String repositoryID,
          @WebParam(name = "theResourceURI") String theResourceURI,
          @WebParam(name = "theAnnotationPropertyURI") String theAnnotationPropertyURI,
          @WebParam(name = "value") String value,
          @WebParam(name = "language") String language)
          throws GateOntologyException;

  /**
   * Gets the list of annotation property values
   * 
   * @param repositoryID
   * @param theResourceURI
   * @param theAnnotationPropertyURI
   * @return
   */
  @WebMethod
  @WebResult(targetNamespace = "http://gate.ac.uk/ns/ontology/owlim")
  @ResponseWrapper(className = "gate.creole.ontology.owlim.PropertyValueArrayResponse")
  public PropertyValue[] getAnnotationPropertyValues(
          @WebParam(name = "repositoryID") String repositoryID,
          @WebParam(name = "theResourceURI") String theResourceURI,
          @WebParam(name = "theAnnotationPropertyURI") String theAnnotationPropertyURI)
          throws GateOntologyException;

  /**
   * Gets the annotation property for the given resource uri.
   * 
   * @param repositoryID
   * @param theResourceURI
   * @param theAnnotationPropertyURI
   * @param language
   * @return
   */
  @WebMethod
  @WebResult(targetNamespace = "http://gate.ac.uk/ns/ontology/owlim")
  public String getAnnotationPropertyValue(
          @WebParam(name = "repositoryID") String repositoryID,
          @WebParam(name = "theResourceURI") String theResourceURI,
          @WebParam(name = "theAnnotationPropertyURI") String theAnnotationPropertyURI,
          @WebParam(name = "language") String language)
          throws GateOntologyException;

  /**
   * For the current resource, the method removes the given literal for
   * the given property.
   * 
   * @param theAnnotationProperty
   * @param literal
   */
  @WebMethod
  @WebResult(targetNamespace = "http://gate.ac.uk/ns/ontology/owlim")
  public void removeAnnotationPropertyValue(
          @WebParam(name = "repositoryID") String repositoryID,
          @WebParam(name = "theResourceURI") String theResourceURI,
          @WebParam(name = "theAnnotationPropertyURI") String theAnnotationPropertyURI,
          @WebParam(name = "value") String value,
          @WebParam(name = "language") String language)
          throws GateOntologyException;

  /**
   * Removes all values for a named property.
   * 
   * @param theProperty the property
   */
  @WebMethod
  @WebResult(targetNamespace = "http://gate.ac.uk/ns/ontology/owlim")
  public void removeAnnotationPropertyValues(
          @WebParam(name = "repositoryID") String repositoryID,
          @WebParam(name = "theResourceURI") String theResourceURI,
          @WebParam(name = "theAnnotationPropertyURI") String theAnnotationPropertyURI)
          throws GateOntologyException;

  // **************
  // RDFProperties
  // *************
  /**
   * The method adds a generic property specifiying domain and range for
   * the same. All classes specified in domain and range must exist.
   * 
   * @param aPropertyURI
   * @param domainClassesURIs
   * @param rangeClassesTypes Done
   */
  @WebMethod
  @WebResult(targetNamespace = "http://gate.ac.uk/ns/ontology/owlim")
  public void addRDFProperty(
          @WebParam(name = "repositoryID") String repositoryID,
          @WebParam(name = "aPropertyURI") String aPropertyURI,
          @WebParam(name = "domainClassesURIs") String[] domainClassesURIs,
          @WebParam(name = "rangeClassesTypes") String[] rangeClassesTypes)
          throws GateOntologyException;

  /**
   * returns if the given property is an RDF property
   * 
   * @param aPropertyURI
   * @return Done
   */
  @WebMethod
  @WebResult(targetNamespace = "http://gate.ac.uk/ns/ontology/owlim")
  public boolean isRDFProperty(
          @WebParam(name = "repositoryID") String repositoryID,
          @WebParam(name = "aPropertyURI") String aPropertyURI)
          throws GateOntologyException;

  // **************
  // Datatype Properties
  // *************
  /**
   * The method adds a data type property specifiying domain and range
   * for the same. All classes specified in domain and range must exist.
   * 
   * @param aPropertyURI
   * @param domainClassesURIs
   * @param dataTypeURI Done
   */
  @WebMethod
  @WebResult(targetNamespace = "http://gate.ac.uk/ns/ontology/owlim")
  public void addDataTypeProperty(
          @WebParam(name = "repositoryID") String repositoryID,
          @WebParam(name = "aPropertyURI") String aPropertyURI,
          @WebParam(name = "domainClassesURIs") String[] domainClassesURIs,
          @WebParam(name = "dataTypeURI") String dataTypeURI)
          throws GateOntologyException;

  /**
   * Returns the datatype uri specified for the given datatype property.
   * 
   * @param repositoryID
   * @param theDatatypePropertyURI
   * @return
   * @throws GateOntologyException
   */
  @WebMethod
  @WebResult(targetNamespace = "http://gate.ac.uk/ns/ontology/owlim")
  public String getDatatype(
          @WebParam(name = "repositoryID") String repositoryID,
          @WebParam(name = "theDatatypePropertyURI") String theDatatypePropertyURI)
          throws GateOntologyException;

  // **************
  // Symmetric Properties
  // *************
  /**
   * The method adds a symmetric property specifiying domain and range
   * for the same. All classes specified in domain and range must exist.
   * 
   * @param aPropertyURI
   * @param domainAndRangeClassesURIs Done
   */
  @WebMethod
  @WebResult(targetNamespace = "http://gate.ac.uk/ns/ontology/owlim")
  public void addSymmetricProperty(
          @WebParam(name = "repositoryID") String repositoryID,
          @WebParam(name = "aPropertyURI") String aPropertyURI,
          @WebParam(name = "domainAndRangeClassesURIs") String[] domainAndRangeClassesURIs)
          throws GateOntologyException;

  /**
   * Checkes whether the two properties are Equivalent.
   * 
   * @param repositoryID
   * @param aPropertyURI
   * @return
   * @throws GateOntologyException
   */
  @WebMethod
  @WebResult(targetNamespace = "http://gate.ac.uk/ns/ontology/owlim")
  public boolean isEquivalentPropertyAs(
          @WebParam(name = "repositoryID") String repositoryID,
          @WebParam(name = "aPropertyURI1") String aPropertyURI1,
          @WebParam(name = "aPropertyURI2") String aPropertyURI2)
          throws GateOntologyException;

  /**
   * for the given property, the method returns all its super properties
   * 
   * @param aPropertyURI
   * @param direct
   * @return
   */
  @WebMethod
  @WebResult(targetNamespace = "http://gate.ac.uk/ns/ontology/owlim")
  @ResponseWrapper(className = "gate.creole.ontology.owlim.PropertyArrayResponse")
  public Property[] getSuperProperties(
          @WebParam(name = "repositoryID") String repositoryID,
          @WebParam(name = "aPropertyURI") String aPropertyURI,
          @WebParam(name = "direct") byte direct)
          throws GateOntologyException;

  /**
   * for the given property, the method returns all its sub properties
   * 
   * @param aPropertyURI
   * @param direct
   * @return
   */
  @WebMethod
  @WebResult(targetNamespace = "http://gate.ac.uk/ns/ontology/owlim")
  @ResponseWrapper(className = "gate.creole.ontology.owlim.PropertyArrayResponse")
  public Property[] getSubProperties(
          @WebParam(name = "repositoryID") String repositoryID,
          @WebParam(name = "aPropertyURI") String aPropertyURI,
          @WebParam(name = "direct") byte direct)
          throws GateOntologyException;

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
  @WebMethod
  @WebResult(targetNamespace = "http://gate.ac.uk/ns/ontology/owlim")
  public boolean isSuperPropertyOf(
          @WebParam(name = "repositoryID") String repositoryID,
          @WebParam(name = "aSuperPropertyURI") String aSuperPropertyURI,
          @WebParam(name = "aSubPropertyURI") String aSubPropertyURI,
          @WebParam(name = "direct") byte direct)
          throws GateOntologyException;

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
  @WebMethod
  @WebResult(targetNamespace = "http://gate.ac.uk/ns/ontology/owlim")
  public boolean isSubPropertyOf(
          @WebParam(name = "repositoryID") String repositoryID,
          @WebParam(name = "aSuperPropertyURI") String aSuperPropertyURI,
          @WebParam(name = "aSubPropertyURI") String aSubPropertyURI,
          @WebParam(name = "direct") byte direct)
          throws GateOntologyException;

  /**
   * Given a class and instance URIs, the method checks if the latter is
   * a member of former. If the boolean parameter direct is set to true,
   * the method also checks if the literal is a direct instance of the
   * class.
   * 
   * @param aSuperClassURI
   * @param individualURI
   * @return Done
   */
  @WebMethod
  @WebResult(targetNamespace = "http://gate.ac.uk/ns/ontology/owlim")
  public boolean hasIndividual(
          @WebParam(name = "repositoryID") String repositoryID,
          @WebParam(name = "aSuperClassURI") String aSuperClassURI,
          @WebParam(name = "individualURI") String individualURI,
          @WebParam(name = "direct") byte direct)
          throws GateOntologyException;

  /**
   * Returns whether the individual1 is different from the individual2.
   * 
   * @param theInstanceURI1
   * @param theInstanceURI2
   * @return
   * @throws GateOntologyException
   */
  @WebMethod
  @WebResult(targetNamespace = "http://gate.ac.uk/ns/ontology/owlim")
  public boolean isDifferentIndividualFrom(
          @WebParam(name = "repositoryID") String repositoryID,
          @WebParam(name = "theInstanceURI1") String theInstanceURI1,
          @WebParam(name = "theInstanceURI2") String theInstanceURI2)
          throws GateOntologyException;

  /**
   * Checkes whether the two individuals are same.
   * 
   * @param repositoryID
   * @param individualURI1
   * @param invidualURI2
   * @return
   * @throws GateOntologyException
   */
  @WebMethod
  @WebResult(targetNamespace = "http://gate.ac.uk/ns/ontology/owlim")
  public boolean isSameIndividualAs(
          @WebParam(name = "repositoryID") String repositoryID,
          @WebParam(name = "theInstanceURI1") String theInstanceURI1,
          @WebParam(name = "theInstanceURI2") String theInstanceURI2)
          throws GateOntologyException;

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
  @WebMethod
  @WebResult(targetNamespace = "http://gate.ac.uk/ns/ontology/owlim")
  public void addRDFPropertyValue(
          @WebParam(name = "repositoryID") String repositoryID,
          @WebParam(name = "anInstanceURI") String anInstanceURI,
          @WebParam(name = "anRDFPropertyURI") String anRDFPropertyURI,
          @WebParam(name = "aResourceURI") String aResourceURI)
          throws GateOntologyException;

  /**
   * Removes the specified RDF Property Value
   * 
   * @param repositoryID
   * @param anInstanceURI
   * @param anRDFPropertyURI
   * @param aResourceURI
   */
  @WebMethod
  @WebResult(targetNamespace = "http://gate.ac.uk/ns/ontology/owlim")
  public void removeRDFPropertyValue(
          @WebParam(name = "repositoryID") String repositoryID,
          @WebParam(name = "anInstanceURI") String anInstanceURI,
          @WebParam(name = "anRDFPropertyURI") String anRDFPropertyURI,
          @WebParam(name = "aResourceURI") String aResourceURI)
          throws GateOntologyException;

  /**
   * gets the rdf property values for the specified instance.
   * 
   * @param repositoryID
   * @param anInstanceURI
   * @param anRDFPropertyURI
   * @return resource URIs
   */
  @WebMethod
  @WebResult(targetNamespace = "http://gate.ac.uk/ns/ontology/owlim")
  @ResponseWrapper(className = "gate.creole.ontology.owlim.ResourceInfoArrayResponse")
  public ResourceInfo[] getRDFPropertyValues(
          @WebParam(name = "repositoryID") String repositoryID,
          @WebParam(name = "anInstanceURI") String anInstanceURI,
          @WebParam(name = "anRDFPropertyURI") String anRDFPropertyURI)
          throws GateOntologyException;

  /**
   * Removes all the RDF Property values from the given instance.
   * 
   * @param repositoryID
   * @param anInstanceURI
   * @param anRDFPropertyURI
   */
  @WebMethod
  @WebResult(targetNamespace = "http://gate.ac.uk/ns/ontology/owlim")
  public void removeRDFPropertyValues(
          @WebParam(name = "repositoryID") String repositoryID,
          @WebParam(name = "anInstanceURI") String anInstanceURI,
          @WebParam(name = "anRDFPropertyURI") String anRDFPropertyURI)
          throws GateOntologyException;

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
  @WebMethod
  @WebResult(targetNamespace = "http://gate.ac.uk/ns/ontology/owlim")
  public void addDatatypePropertyValue(
          @WebParam(name = "repositoryID") String repositoryID,
          @WebParam(name = "anInstanceURI") String anInstanceURI,
          @WebParam(name = "aDatatypePropertyURI") String aDatatypePropertyURI,
          @WebParam(name = "datatypeURI") String datatypeURI,
          @WebParam(name = "value") String value)
          throws GateOntologyException;

  /**
   * Removes the provided value for the given instance.
   * 
   * @param repositoryID
   * @param anInstanceURI
   * @param aDatatypePropertyURI
   * @param datatypeURI
   * @param value
   */
  @WebMethod
  @WebResult(targetNamespace = "http://gate.ac.uk/ns/ontology/owlim")
  public void removeDatatypePropertyValue(
          @WebParam(name = "repositoryID") String repositoryID,
          @WebParam(name = "anInstanceURI") String anInstanceURI,
          @WebParam(name = "aDatatypePropertyURI") String aDatatypePropertyURI,
          @WebParam(name = "datatypeURI") String datatypeURI,
          @WebParam(name = "value") String value)
          throws GateOntologyException;

  /**
   * Gets a list of values for the given Property.
   * 
   * @param repositoryID
   * @param anInstanceURI
   * @param aDatatypePropertyURI
   * @return
   */
  @WebMethod
  @WebResult(targetNamespace = "http://gate.ac.uk/ns/ontology/owlim")
  @ResponseWrapper(className = "gate.creole.ontology.owlim.PropertyValueArrayResponse")
  public PropertyValue[] getDatatypePropertyValues(
          @WebParam(name = "repositoryID") String repositoryID,
          @WebParam(name = "anInstanceURI") String anInstanceURI,
          @WebParam(name = "aDatatypePropertyURI") String aDatatypePropertyURI)
          throws GateOntologyException;

  /**
   * Removes all property values set on the provided instance for the
   * current property.
   * 
   * @param repositoryID
   * @param anInstanceURI
   * @param aDatatypePropertyURI
   */
  @WebMethod
  @WebResult(targetNamespace = "http://gate.ac.uk/ns/ontology/owlim")
  public void removeDatatypePropertyValues(
          @WebParam(name = "repositoryID") String repositoryID,
          @WebParam(name = "anInstanceURI") String anInstanceURI,
          @WebParam(name = "aDatatypePropertyURI") String aDatatypePropertyURI)
          throws GateOntologyException;

  // ******************
  // Object, Symmetric and Transitive Properties
  // *****************
  /**
   * Adds the value for the given property (Object, Symmetric and
   * Transitive).
   * 
   * @param repositoryID
   * @param sourceInstanceURI
   * @param anObjectPropertyURI
   * @param theValueInstanceURI
   * @throws InvalidValueException
   */
  @WebMethod
  @WebResult(targetNamespace = "http://gate.ac.uk/ns/ontology/owlim")
  public void addObjectPropertyValue(
          @WebParam(name = "repositoryID") String repositoryID,
          @WebParam(name = "sourceInstanceURI") String sourceInstanceURI,
          @WebParam(name = "anObjectPropertyURI") String anObjectPropertyURI,
          @WebParam(name = "theValueInstanceURI") String theValueInstanceURI)
          throws GateOntologyException;

  /**
   * Remove the provided value for the given property (Object, Symmetric
   * and Transitive).
   * 
   * @param repositoryID
   * @param sourceInstanceURI
   * @param anObjectPropertyURI
   * @param theValueInstanceURI
   * @return
   */
  @WebMethod
  @WebResult(targetNamespace = "http://gate.ac.uk/ns/ontology/owlim")
  public void removeObjectPropertyValue(
          @WebParam(name = "repositoryID") String repositoryID,
          @WebParam(name = "sourceInstanceURI") String sourceInstanceURI,
          @WebParam(name = "anObjectPropertyURI") String anObjectPropertyURI,
          @WebParam(name = "theValueInstanceURI") String theValueInstanceURI)
          throws GateOntologyException;

  /**
   * Gets a list of values for the given Property (Object, Symmetric and
   * Transitive).
   * 
   * @param repositoryID
   * @param sourceInstanceURI
   * @param anObjectPropertyURI
   * @return
   */
  @WebMethod
  @WebResult(targetNamespace = "http://gate.ac.uk/ns/ontology/owlim")
  @ResponseWrapper(className = "gate.creole.ontology.owlim.StringArrayResponse")
  public String[] getObjectPropertyValues(
          @WebParam(name = "repositoryID") String repositoryID,
          @WebParam(name = "sourceInstanceURI") String sourceInstanceURI,
          @WebParam(name = "anObjectPropertyURI") String anObjectPropertyURI)
          throws GateOntologyException;

  /**
   * Removes all property values set for the current property (Object,
   * Symmetric and Transitive).
   * 
   * @param repositoryID
   * @param sourceInstanceURI
   * @param anObjectPropertyURI
   */
  @WebMethod
  @WebResult(targetNamespace = "http://gate.ac.uk/ns/ontology/owlim")
  public void removeObjectPropertyValues(
          @WebParam(name = "repositoryID") String repositoryID,
          @WebParam(name = "sourceInstanceURI") String sourceInstanceURI,
          @WebParam(name = "anObjectPropertyURI") String anObjectPropertyURI)
          throws GateOntologyException;

  // ****************************************************************************
  // user management methods
  // ****************************************************************************
  /**
   * Call to this method is necessary in order to login in to the Sesame
   * server. Unless user is registered with Sesame server, he/she cannot
   * have write or modify access to any of the repositories (unless
   * given write access to world users) available on the server.
   * However, unregistered users are and will be allowed to have read
   * access on all repositories.
   * 
   * @param username
   * @param password
   * @return
   */
  @WebMethod
  @WebResult(targetNamespace = "http://gate.ac.uk/ns/ontology/owlim")
  public boolean login(
          @WebParam(name = "username") String username,
          @WebParam(name = "password") String password)
          throws GateOntologyException;

  /**
   * End the session by logging out
   */
  @WebMethod
  @WebResult(targetNamespace = "http://gate.ac.uk/ns/ontology/owlim")
  public void logout(
          @WebParam(name = "repositoryID") String repositoryID)
          throws GateOntologyException;

  // ****************************************************************************
  // repository methods
  // ****************************************************************************
  /**
   * Find out the list of repository list
   */
  @WebMethod
  @WebResult(targetNamespace = "http://gate.ac.uk/ns/ontology/owlim")
  @ResponseWrapper(className = "gate.creole.ontology.owlim.StringArrayResponse")
  public String[] getRepositoryList()
          throws GateOntologyException;

  /**
   * sets the provided repository as a current repository
   */
  @WebMethod
  @WebResult(targetNamespace = "http://gate.ac.uk/ns/ontology/owlim")
  public void setCurrentRepositoryID(
          @WebParam(name = "repositoryID") String repositoryID)
          throws GateOntologyException;

  /**
   * This method returns the ID of current repository
   */
  @WebMethod
  @WebResult(targetNamespace = "http://gate.ac.uk/ns/ontology/owlim")
  public String getCurrentRepositoryID()
          throws GateOntologyException;

  /**
   * Users are allowed to create new repositories and add data into it.
   * In order to create new repository, they don’t necessarily need to
   * be registered. The username and password parameters are used to
   * assign access rights over the repository. Apart from the owner of
   * repository, administrator also gets the full rights over the
   * repository. All other users are given read access. User is also
   * asked to provide a URL, or the RDF data from the ontology. Incase
   * if the url is null or an empty string, an empty graph is created
   * allowing users to add more data into it. Otherwise the graph is
   * populated with the given ontology. The user is also asked to
   * provide the RDF format information (i.e. ''N3'', ''TURTLE'',
   * ''NTRIPLES'' or ''RDFXML'') .
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
  @WebMethod
  @WebResult(targetNamespace = "http://gate.ac.uk/ns/ontology/owlim")
  public String createRepository(
          @WebParam(name = "repositoryID") String repositoryID,
          @WebParam(name = "username") String username,
          @WebParam(name = "password") String password,
          @WebParam(name = "ontoData") String ontoData,
          @WebParam(name = "baseURI") String baseURI,
          @WebParam(name = "format") byte format,
          @WebParam(name = "absolutePersistLocation") String absolutePersistLocation,
          @WebParam(name = "persist") boolean persist,
          @WebParam(name = "returnSystemStatements") boolean returnSystemStatements)
          throws GateOntologyException;

  /**
   * Users are allowed to create new repositories and add data into it.
   * In order to create new repository, they don’t necessarily need to
   * be registered. The username and password parameters are used to
   * assign access rights over the repository. Apart from the owner of
   * repository, administrator also gets the full rights over the
   * repository. All other users are given read access. User is also
   * asked to provide a URL for the ontology. Incase if the url is null
   * or an empty string, an empty graph is created allowing user to add
   * more data into it. Otherwise the graph is populated with the given
   * ontology URL. The user is also asked to provide the RDF format
   * information (i.e. ''N3'', ''TURTLE'', ''NTRIPLES'' or ''RDFXML'') .
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
  @WebMethod
  @WebResult(targetNamespace = "http://gate.ac.uk/ns/ontology/owlim")
  public String createRepositoryFromUrl(
          @WebParam(name = "repositoryID") String repositoryID,
          @WebParam(name = "username") String username,
          @WebParam(name = "password") String password,
          @WebParam(name = "ontoFileUrl") String ontoFileUrl,
          @WebParam(name = "baseURI") String baseURI,
          @WebParam(name = "format") byte format,
          @WebParam(name = "absolutePersistLocation") String absolutePersistLocation,
          @WebParam(name = "persist") boolean persist,
          @WebParam(name = "returnSystemStatements") boolean returnSystemStatements)
          throws GateOntologyException;

  /**
   * Removes the repository with given ID
   * 
   * @param repositoryID
   * @return
   */
  @WebMethod
  @WebResult(targetNamespace = "http://gate.ac.uk/ns/ontology/owlim")
  public void removeRepository(
          @WebParam(name = "repositoryID") String repositoryID,
          @WebParam(name = "persist") boolean persist)
          throws GateOntologyException;

  // *******************************************************************
  // *************************** Ontology Methods **********************
  // *******************************************************************
  /**
   * The method removes all data from the available graph.
   */
  @WebMethod
  @WebResult(targetNamespace = "http://gate.ac.uk/ns/ontology/owlim")
  public void cleanOntology(
          @WebParam(name = "repositoryID") String repositoryID)
          throws GateOntologyException;

  /**
   * This method is useful to export results. Given one of the four
   * RDFFormat parameters (i.e. ''N3'', ''TURTLE'', ''NTRIPLES'' or
   * ''RDFXML'') , the method returns an equivalent string
   * representation of the data in the supplied format.
   * 
   * @param format
   * @return
   */
  @WebMethod
  @WebResult(targetNamespace = "http://gate.ac.uk/ns/ontology/owlim")
  public String getOntologyData(
          @WebParam(name = "repositoryID") String repositoryID,
          @WebParam(name = "format") byte format)
          throws GateOntologyException;

  /**
   * The method allows adding version information to the repository.
   * 
   * @param versionInfo
   */
  @WebMethod
  @WebResult(targetNamespace = "http://gate.ac.uk/ns/ontology/owlim")
  public void setVersion(
          @WebParam(name = "repositoryID") String repositoryID,
          @WebParam(name = "versionInfo") String versionInfo)
          throws GateOntologyException;

  /**
   * The method returns the version information of the repository.
   * 
   * @return
   */
  @WebMethod
  @WebResult(targetNamespace = "http://gate.ac.uk/ns/ontology/owlim")
  public String getVersion(
          @WebParam(name = "repositoryID") String repositoryID)
          throws GateOntologyException;

  // *******************************************************************
  // class methods
  // *******************************************************************
  /**
   * The method allows adding a class to repository.
   * 
   * @param classURI
   * @param classType - one of the following constant values from the
   *          OConstants class. OWL_CLASS, CARDINALITY_RESTRICTION,
   *          MIN_CARDINALITY_RESTRICTION, MAX_CARDINALITY_RESTRICTION,
   *          HAS_VALUE_RESTRICTION, ALL_VALUES_FROM_RESTRICTION.
   */
  @WebMethod
  @WebResult(targetNamespace = "http://gate.ac.uk/ns/ontology/owlim")
  public void addClass(
          @WebParam(name = "repositoryID") String repositoryID,
          @WebParam(name = "classURI") String classURI,
          @WebParam(name = "classType") byte classType)
          throws GateOntologyException;

  /**
   * Given a class to delete, it removes it from the repository.
   * 
   * @param repositoryID
   * @param classURI
   * @param deleteSubTree
   * @return a list of other resources, which got removed as a result of
   *         this deletion
   */
  @WebMethod
  @WebResult(targetNamespace = "http://gate.ac.uk/ns/ontology/owlim")
  @ResponseWrapper(className = "gate.creole.ontology.owlim.StringArrayResponse")
  public String[] removeClass(
          @WebParam(name = "repositoryID") String repositoryID,
          @WebParam(name = "classURI") String classURI,
          @WebParam(name = "deleteSubTree") boolean deleteSubTree)
          throws GateOntologyException;

  
  /**
   * The method returns if the current repository has a class with URI
   * that matches with the class parameter.
   * 
   * @return
   */
  @WebMethod
  @WebResult(targetNamespace = "http://gate.ac.uk/ns/ontology/owlim")
  public boolean hasClass(
          @WebParam(name = "repositoryID") String repositoryID,
          @WebParam(name = "classURI") String classURI)
          throws GateOntologyException;

  /**
   * if top set to true, the method returns only the top classes (i.e.
   * classes with no super class). Otherwise it returns all classes
   * available in repository.
   * 
   * @param top
   * @return
   */
  @WebMethod
  @WebResult(targetNamespace = "http://gate.ac.uk/ns/ontology/owlim")
  @ResponseWrapper(className = "gate.creole.ontology.owlim.ResourceInfoArrayResponse")
  public ResourceInfo[] getClasses(
          @WebParam(name = "repositoryID") String repositoryID,
          @WebParam(name = "top") boolean top)
          throws GateOntologyException;

  /**
   * Returns if the given class is a top class. It also returns false if
   * the class is an instance of BNode
   * 
   * @param classURI
   * @return
   */
  @WebMethod
  @WebResult(targetNamespace = "http://gate.ac.uk/ns/ontology/owlim")
  public boolean isTopClass(
          @WebParam(name = "repositoryID") String repositoryID,
          @WebParam(name = "classURI") String classURI)
          throws GateOntologyException;

  // ****************************************************************************
  // relations among classes
  // ****************************************************************************
  /**
   * The method creates a new class with the URI as specified in
   * className and adds it as a subClassOf the parentClass. It also adds
   * the provided comment on the subClass.
   * 
   * @param superClassURI
   * @param subClassURI
   */
  @WebMethod
  @WebResult(targetNamespace = "http://gate.ac.uk/ns/ontology/owlim")
  public void addSubClass(
          @WebParam(name = "repositoryID") String repositoryID,
          @WebParam(name = "superClassURI") String superClassURI,
          @WebParam(name = "subClassURI") String subClassURI)
          throws GateOntologyException;

  /**
   * The method creates a new class with the URI as specified in
   * className and adds it as a superClassOf the parentClass. It also
   * adds the provided comment on the subClass.
   * 
   * @param superClassURI
   * @param subClassURI
   */
  @WebMethod
  @WebResult(targetNamespace = "http://gate.ac.uk/ns/ontology/owlim")
  public void addSuperClass(
          @WebParam(name = "repositoryID") String repositoryID,
          @WebParam(name = "superClassURI") String superClassURI,
          @WebParam(name = "subClassURI") String subClassURI)
          throws GateOntologyException;

  /**
   * Removes the subclass relationship
   * 
   * @param superClassURI
   * @param subClassURI
   */
  @WebMethod
  @WebResult(targetNamespace = "http://gate.ac.uk/ns/ontology/owlim")
  public void removeSubClass(
          @WebParam(name = "repositoryID") String repositoryID,
          @WebParam(name = "superClassURI") String superClassURI,
          @WebParam(name = "subClassURI") String subClassURI)
          throws GateOntologyException;

  /**
   * Removes the superclass relationship
   * 
   * @param superClassURI
   * @param subClassURI
   */
  @WebMethod
  @WebResult(targetNamespace = "http://gate.ac.uk/ns/ontology/owlim")
  public void removeSuperClass(
          @WebParam(name = "repositoryID") String repositoryID,
          @WebParam(name = "superClassURI") String superClassURI,
          @WebParam(name = "subClassURI") String subClassURI)
          throws GateOntologyException;

  /**
   * This method returns all sub classes of the given class
   * 
   * @param superClassURI
   * @param direct
   * @return
   */
  @WebMethod
  @WebResult(targetNamespace = "http://gate.ac.uk/ns/ontology/owlim")
  @ResponseWrapper(className = "gate.creole.ontology.owlim.ResourceInfoArrayResponse")
  public ResourceInfo[] getSubClasses(
          @WebParam(name = "repositoryID") String repositoryID,
          @WebParam(name = "superClassURI") String superClassURI,
          @WebParam(name = "direct") byte direct)
          throws GateOntologyException;

  /**
   * This method returns all super classes of the given class
   * 
   * @param subClassURI
   * @param direct
   * @return
   */
  @WebMethod
  @WebResult(targetNamespace = "http://gate.ac.uk/ns/ontology/owlim")
  @ResponseWrapper(className = "gate.creole.ontology.owlim.ResourceInfoArrayResponse")
  public ResourceInfo[] getSuperClasses(
          @WebParam(name = "repositoryID") String repositoryID,
          @WebParam(name = "subClassURI") String subClassURI,
          @WebParam(name = "direct") byte direct)
          throws GateOntologyException;

  /**
   * Sets the classes as disjoint
   * 
   * @param class1URI
   * @param class2URI
   */
  @WebMethod
  @WebResult(targetNamespace = "http://gate.ac.uk/ns/ontology/owlim")
  public void setDisjointClassWith(
          @WebParam(name = "repositoryID") String repositoryID,
          @WebParam(name = "class1URI") String class1URI,
          @WebParam(name = "class2URI") String class2URI)
          throws GateOntologyException;

  /**
   * Sets the classes as same classes
   * 
   * @param class1URI
   * @param class2URI
   */
  @WebMethod
  @WebResult(targetNamespace = "http://gate.ac.uk/ns/ontology/owlim")
  public void setEquivalentClassAs(
          @WebParam(name = "repositoryID") String repositoryID,
          @WebParam(name = "class1URI") String class1URI,
          @WebParam(name = "class2URI") String class2URI)
          throws GateOntologyException;

  /**
   * returns an array of classes which are marked as disjoint for the
   * given class
   * 
   * @param classURI
   * @return
   */
  @WebMethod
  @WebResult(targetNamespace = "http://gate.ac.uk/ns/ontology/owlim")
  @ResponseWrapper(className = "gate.creole.ontology.owlim.StringArrayResponse")
  public String[] getDisjointClasses(
          @WebParam(name = "repositoryID") String repositoryID,
          @WebParam(name = "classURI") String classURI)
          throws GateOntologyException;

  /**
   * returns an array of classes which are equivalent as the given class
   * 
   * @param aClassURI
   * @return
   */
  @WebMethod
  @WebResult(targetNamespace = "http://gate.ac.uk/ns/ontology/owlim")
  @ResponseWrapper(className = "gate.creole.ontology.owlim.ResourceInfoArrayResponse")
  public ResourceInfo[] getEquivalentClasses(
          @WebParam(name = "repositoryID") String repositoryID,
          @WebParam(name = "aClassURI") String aClassURI)
          throws GateOntologyException;

  /**
   * Removes the given property
   * @param repositoryID 
   * @param aPropertyURI
   * @param removeSubTree
   * @return a list of names of resources deleted as a result of deleting this property from the ontology.
   */
  @WebMethod
  @WebResult(targetNamespace = "http://gate.ac.uk/ns/ontology/owlim")
  @ResponseWrapper(className = "gate.creole.ontology.owlim.StringArrayResponse")
  public String[] removePropertyFromOntology(
          @WebParam(name = "repositoryID") String repositoryID,
          @WebParam(name = "aPropertyURI") String aPropertyURI,
          @WebParam(name = "removeSubTree") boolean removeSubTree)
          throws GateOntologyException;

  /**
   * The method adds an object property specifiying domain and range for
   * the same. All classes specified in domain and range must exist.
   * 
   * @param aPropertyURI
   * @param domainClassesURIs
   * @param rangeClassesTypes
   */
  @WebMethod
  @WebResult(targetNamespace = "http://gate.ac.uk/ns/ontology/owlim")
  public void addObjectProperty(
          @WebParam(name = "repositoryID") String repositoryID,
          @WebParam(name = "aPropertyURI") String aPropertyURI,
          @WebParam(name = "domainClassesURIs") String[] domainClassesURIs,
          @WebParam(name = "rangeClassesTypes") String[] rangeClassesTypes)
          throws GateOntologyException;

  /**
   * The method adds a transitive property specifiying domain and range
   * for the same. All classes specified in domain and range must exist.
   * 
   * @param aPropertyURI
   * @param domainClassesURIs
   * @param rangeClassesTypes
   */
  @WebMethod
  @WebResult(targetNamespace = "http://gate.ac.uk/ns/ontology/owlim")
  public void addTransitiveProperty(
          @WebParam(name = "repositoryID") String repositoryID,
          @WebParam(name = "aPropertyURI") String aPropertyURI,
          @WebParam(name = "domainClassesURIs") String[] domainClassesURIs,
          @WebParam(name = "rangeClassesTypes") String[] rangeClassesTypes)
          throws GateOntologyException;

  /**
   * The method returns an array of properties. Property is a complex
   * structure, which contains name, comment, information about its
   * domain and range.
   * 
   * @return
   */
  @WebMethod
  @WebResult(targetNamespace = "http://gate.ac.uk/ns/ontology/owlim")
  @ResponseWrapper(className = "gate.creole.ontology.owlim.PropertyArrayResponse")
  public Property[] getRDFProperties(
          @WebParam(name = "repositoryID") String repositoryID)
          throws GateOntologyException;

  /**
   * The method returns an array of properties. Property is a complex
   * structure, which contains name, comment, information about its
   * domain and range.
   * 
   * @return
   */
  @WebMethod
  @WebResult(targetNamespace = "http://gate.ac.uk/ns/ontology/owlim")
  @ResponseWrapper(className = "gate.creole.ontology.owlim.PropertyArrayResponse")
  public Property[] getObjectProperties(
          @WebParam(name = "repositoryID") String repositoryID)
          throws GateOntologyException;

  /**
   * The method returns an array of properties. Property is a complex
   * structure, which contains name, comment, information about its
   * domain and range.
   * 
   * @return
   */
  @WebMethod
  @WebResult(targetNamespace = "http://gate.ac.uk/ns/ontology/owlim")
  @ResponseWrapper(className = "gate.creole.ontology.owlim.PropertyArrayResponse")
  public Property[] getSymmetricProperties(
          @WebParam(name = "repositoryID") String repositoryID)
          throws GateOntologyException;

  /**
   * The method returns an array of properties. Property is a complex
   * structure, which contains name, comment, information about its
   * domain and range.
   * 
   * @return
   */
  @WebMethod
  @WebResult(targetNamespace = "http://gate.ac.uk/ns/ontology/owlim")
  @ResponseWrapper(className = "gate.creole.ontology.owlim.PropertyArrayResponse")
  public Property[] getTransitiveProperties(
          @WebParam(name = "repositoryID") String repositoryID)
          throws GateOntologyException;

  /**
   * The method returns an array of properties. Property is a complex
   * structure, which contains name, comment, information about its
   * domain and range.
   * 
   * @return
   */
  @WebMethod
  @WebResult(targetNamespace = "http://gate.ac.uk/ns/ontology/owlim")
  @ResponseWrapper(className = "gate.creole.ontology.owlim.PropertyArrayResponse")
  public Property[] getDatatypeProperties(
          @WebParam(name = "repositoryID") String repositoryID)
          throws GateOntologyException;

  /**
   * The method returns an array of properties. Property is a complex
   * structure, which contains name, comment, information about its
   * domain and range.
   * 
   * @return
   */
  @WebMethod
  @WebResult(targetNamespace = "http://gate.ac.uk/ns/ontology/owlim")
  @ResponseWrapper(className = "gate.creole.ontology.owlim.PropertyArrayResponse")
  public Property[] getAnnotationProperties(
          @WebParam(name = "repositoryID") String repositoryID)
          throws GateOntologyException;

  /**
   * Given a property, this method returns its domain
   * 
   * @param aPropertyURI
   * @return
   */
  @WebMethod
  @WebResult(targetNamespace = "http://gate.ac.uk/ns/ontology/owlim")
  @ResponseWrapper(className = "gate.creole.ontology.owlim.ResourceInfoArrayResponse")
  public ResourceInfo[] getDomain(
          @WebParam(name = "repositoryID") String repositoryID,
          @WebParam(name = "aPropertyURI") String aPropertyURI)
          throws GateOntologyException;

  /**
   * Given a property, this method returns its range
   * 
   * @param aPropertyURI
   * @return
   */
  @WebMethod
  @WebResult(targetNamespace = "http://gate.ac.uk/ns/ontology/owlim")
  @ResponseWrapper(className = "gate.creole.ontology.owlim.ResourceInfoArrayResponse")
  public ResourceInfo[] getRange(
          @WebParam(name = "repositoryID") String repositoryID,
          @WebParam(name = "aPropertyURI") String aPropertyURI)
          throws GateOntologyException;

  /**
   * Returns if the provided property is functional
   * 
   * @param aPropertyURI
   * @return
   */
  @WebMethod
  @WebResult(targetNamespace = "http://gate.ac.uk/ns/ontology/owlim")
  public boolean isFunctional(
          @WebParam(name = "repositoryID") String repositoryID,
          @WebParam(name = "aPropertyURI") String aPropertyURI)
          throws GateOntologyException;

  /**
   * sets the current property as functional
   * 
   * @param aPropertyURI
   * @param isFunctional
   */
  @WebMethod
  @WebResult(targetNamespace = "http://gate.ac.uk/ns/ontology/owlim")
  public void setFunctional(
          @WebParam(name = "repositoryID") String repositoryID,
          @WebParam(name = "aPropertyURI") String aPropertyURI,
          @WebParam(name = "isFunctional") boolean isFunctional)
          throws GateOntologyException;

  /**
   * returns if the given property is inverse functional property
   * 
   * @param aPropertyURI
   * @return
   */
  @WebMethod
  @WebResult(targetNamespace = "http://gate.ac.uk/ns/ontology/owlim")
  public boolean isInverseFunctional(
          @WebParam(name = "repositoryID") String repositoryID,
          @WebParam(name = "aPropertyURI") String aPropertyURI)
          throws GateOntologyException;

  /**
   * Sets the current property as inverse functional property
   * 
   * @param aPropertyURI
   * @param isInverseFunctional
   */
  @WebMethod
  @WebResult(targetNamespace = "http://gate.ac.uk/ns/ontology/owlim")
  public void setInverseFunctional(
          @WebParam(name = "repositoryID") String repositoryID,
          @WebParam(name = "aPropertyURI") String aPropertyURI,
          @WebParam(name = "isInverseFunctional") boolean isInverseFunctional)
          throws GateOntologyException;

  /**
   * returns if the given property is a symmetric property
   * 
   * @param aPropertyURI
   * @return
   */
  @WebMethod
  @WebResult(targetNamespace = "http://gate.ac.uk/ns/ontology/owlim")
  public boolean isSymmetricProperty(
          @WebParam(name = "repositoryID") String repositoryID,
          @WebParam(name = "aPropertyURI") String aPropertyURI)
          throws GateOntologyException;

  /**
   * returns if the given property is a transitive property
   * 
   * @param aPropertyURI
   * @return
   */
  @WebMethod
  @WebResult(targetNamespace = "http://gate.ac.uk/ns/ontology/owlim")
  public boolean isTransitiveProperty(
          @WebParam(name = "repositoryID") String repositoryID,
          @WebParam(name = "aPropertyURI") String aPropertyURI)
          throws GateOntologyException;

  /**
   * returns if the given property is a datatype property
   * 
   * @param aPropertyURI
   * @return
   */
  @WebMethod
  @WebResult(targetNamespace = "http://gate.ac.uk/ns/ontology/owlim")
  public boolean isDatatypeProperty(
          @WebParam(name = "repositoryID") String repositoryID,
          @WebParam(name = "aPropertyURI") String aPropertyURI)
          throws GateOntologyException;

  /**
   * returns if the given property is an object property
   * 
   * @param aPropertyURI
   * @return
   */
  @WebMethod
  @WebResult(targetNamespace = "http://gate.ac.uk/ns/ontology/owlim")
  public boolean isObjectProperty(
          @WebParam(name = "repositoryID") String repositoryID,
          @WebParam(name = "aPropertyURI") String aPropertyURI)
          throws GateOntologyException;

  // *************************************
  // Relations among properties
  // *************************************
  /**
   * Sets two properties as same
   * 
   * @param property1URI
   * @param property2URI
   */
  @WebMethod
  @WebResult(targetNamespace = "http://gate.ac.uk/ns/ontology/owlim")
  public void setEquivalentPropertyAs(
          @WebParam(name = "repositoryID") String repositoryID,
          @WebParam(name = "property1URI") String property1URI,
          @WebParam(name = "property2URI") String property2URI)
          throws GateOntologyException;

  /**
   * For the given property, this method returns all properties marked
   * as Equivalent as it
   * 
   * @param aPropertyURI
   * @return
   */
  @WebMethod
  @WebResult(targetNamespace = "http://gate.ac.uk/ns/ontology/owlim")
  @ResponseWrapper(className = "gate.creole.ontology.owlim.PropertyArrayResponse")
  public Property[] getEquivalentPropertyAs(
          @WebParam(name = "repositoryID") String repositoryID,
          @WebParam(name = "aPropertyURI") String aPropertyURI)
          throws GateOntologyException;

  /**
   * For the given properties, this method registers the super, sub
   * relation
   * 
   * @param superPropertyURI
   * @param subPropertyURI
   */
  @WebMethod
  @WebResult(targetNamespace = "http://gate.ac.uk/ns/ontology/owlim")
  public void addSuperProperty(
          @WebParam(name = "repositoryID") String repositoryID,
          @WebParam(name = "superPropertyURI") String superPropertyURI,
          @WebParam(name = "subPropertyURI") String subPropertyURI)
          throws GateOntologyException;

  /**
   * For the given properties, this method removes the super, sub
   * relation
   * 
   * @param superPropertyURI
   * @param subPropertyURI
   */
  @WebMethod
  @WebResult(targetNamespace = "http://gate.ac.uk/ns/ontology/owlim")
  public void removeSuperProperty(
          @WebParam(name = "repositoryID") String repositoryID,
          @WebParam(name = "superPropertyURI") String superPropertyURI,
          @WebParam(name = "subPropertyURI") String subPropertyURI)
          throws GateOntologyException;

  /**
   * For the given properties, this method registers the super, sub
   * relation
   * 
   * @param superPropertyURI
   * @param subPropertyURI
   */
  @WebMethod
  @WebResult(targetNamespace = "http://gate.ac.uk/ns/ontology/owlim")
  public void addSubProperty(
          @WebParam(name = "repositoryID") String repositoryID,
          @WebParam(name = "superPropertyURI") String superPropertyURI,
          @WebParam(name = "subPropertyURI") String subPropertyURI)
          throws GateOntologyException;

  /**
   * For the given properties, this method removes the super, sub
   * relation
   * 
   * @param superPropertyURI
   * @param subPropertyURI
   */
  @WebMethod
  @WebResult(targetNamespace = "http://gate.ac.uk/ns/ontology/owlim")
  public void removeSubProperty(
          @WebParam(name = "repositoryID") String repositoryID,
          @WebParam(name = "superPropertyURI") String superPropertyURI,
          @WebParam(name = "subPropertyURI") String subPropertyURI)
          throws GateOntologyException;

  /**
   * for the given property, the method returns all its super properties
   * 
   * @param aPropertyURI
   * @param direct
   * @return
   */
  @WebMethod(operationName = "getSuperPropertiesBoolClosure")
  @WebResult(targetNamespace = "http://gate.ac.uk/ns/ontology/owlim")
  @ResponseWrapper(className = "gate.creole.ontology.owlim.PropertyArrayResponse")
  public Property[] getSuperProperties(
          @WebParam(name = "repositoryID") String repositoryID,
          @WebParam(name = "aPropertyURI") String aPropertyURI,
          @WebParam(name = "direct") boolean direct)
          throws GateOntologyException;

  /**
   * for the given property, the method returns all its sub properties
   * 
   * @param aPropertyURI
   * @param direct
   * @return
   */
  @WebMethod(operationName = "getSubPropertiesBoolClosure")
  @WebResult(targetNamespace = "http://gate.ac.uk/ns/ontology/owlim")
  @ResponseWrapper(className = "gate.creole.ontology.owlim.PropertyArrayResponse")
  public Property[] getSubProperties(
          @WebParam(name = "repositoryID") String repositoryID,
          @WebParam(name = "aPropertyURI") String aPropertyURI,
          @WebParam(name = "direct") boolean direct)
          throws GateOntologyException;

  /**
   * for the given property, the method returns all its inverse
   * properties
   * 
   * @param aPropertyURI
   * @return
   */
  @WebMethod
  @WebResult(targetNamespace = "http://gate.ac.uk/ns/ontology/owlim")
  @ResponseWrapper(className = "gate.creole.ontology.owlim.PropertyArrayResponse")
  public Property[] getInverseProperties(
          @WebParam(name = "repositoryID") String repositoryID,
          @WebParam(name = "aPropertyURI") String aPropertyURI)
          throws GateOntologyException;

  /**
   * property1 is set as inverse of property 2
   * 
   * @param property1URI
   * @param property2URI
   */
  @WebMethod
  @WebResult(targetNamespace = "http://gate.ac.uk/ns/ontology/owlim")
  public void setInverseOf(
          @WebParam(name = "repositoryID") String repositoryID,
          @WebParam(name = "propertyURI1") String propertyURI1,
          @WebParam(name = "propertyURI2") String propertyURI2)
          throws GateOntologyException;

  // *******************************************************************
  // *************************** Instance Methods **********************
  // *******************************************************************
  /**
   * The method adds a new instance (literal) into the repository. It
   * then creates a statement indicating membership relation with the
   * provided class.
   * 
   * @param superClassURI
   * @param individualURI
   */
  @WebMethod
  @WebResult(targetNamespace = "http://gate.ac.uk/ns/ontology/owlim")
  public void addIndividual(
          @WebParam(name = "repositoryID") String repositoryID,
          @WebParam(name = "superClassURI") String superClassURI,
          @WebParam(name = "individualURI") String individualURI)
          throws GateOntologyException;

  /**
   * The method removes the provided instance from the repository.
   * 
   * @param individual
   * @return
   */
  @WebMethod
  @WebResult(targetNamespace = "http://gate.ac.uk/ns/ontology/owlim")
  @ResponseWrapper(className = "gate.creole.ontology.owlim.StringArrayResponse")
  public String[] removeIndividual(
          @WebParam(name = "repositoryID") String repositoryID,
          @WebParam(name = "individualURI") String individualURI)
          throws GateOntologyException;

  /**
   * The method returns all member instances of the provided class. It
   * returns only the direct instances if the boolean parameter direct
   * is set to true.
   * 
   * @param superClassURI
   * @param direct
   */
  @WebMethod(operationName = "getIndividualsOfClass")
  @WebResult(targetNamespace = "http://gate.ac.uk/ns/ontology/owlim")
  @ResponseWrapper(className = "gate.creole.ontology.owlim.StringArrayResponse")
  public String[] getIndividuals(
          @WebParam(name = "repositoryID") String repositoryID,
          @WebParam(name = "superClassURI") String superClassURI,
          @WebParam(name = "direct") byte direct)
          throws GateOntologyException;

  /**
   * returns all resources registered as individuals in the ontology
   * 
   * @return
   */
  @WebMethod
  @WebResult(targetNamespace = "http://gate.ac.uk/ns/ontology/owlim")
  @ResponseWrapper(className = "gate.creole.ontology.owlim.StringArrayResponse")
  public String[] getIndividuals(
          @WebParam(name = "repositoryID") String repositoryID)
          throws GateOntologyException;

  /**
   * Given a class and instance URIs, the method checks if the latter is
   * a member of former. If the boolean parameter direct is set to true,
   * the method also checks if the literal is a direct instance of the
   * class.
   * 
   * @param aSuperClassURI
   * @param individualURI
   * @return
   */
  @WebMethod(operationName = "hasIndividualBoolClosure")
  @WebResult(targetNamespace = "http://gate.ac.uk/ns/ontology/owlim")
  public boolean hasIndividual(
          @WebParam(name = "repositoryID") String repositoryID,
          @WebParam(name = "aSuperClassURI") String aSuperClassURI,
          @WebParam(name = "individualURI") String individualURI,
          @WebParam(name = "direct") boolean direct)
          throws GateOntologyException;

  /**
   * For the given individual, the method returns a set of classes for
   * which the individual is registered as instance of
   * 
   * @param individualURI
   */
  @WebMethod
  @WebResult(targetNamespace = "http://gate.ac.uk/ns/ontology/owlim")
  @ResponseWrapper(className = "gate.creole.ontology.owlim.ResourceInfoArrayResponse")
  public ResourceInfo[] getClassesOfIndividual(
          @WebParam(name = "repositoryID") String repositoryID,
          @WebParam(name = "individualURI") String individualURI,
          @WebParam(name = "direct") byte direct)
          throws GateOntologyException;

  // *******************************************************************
  // relations among individuals
  // *******************************************************************
  /**
   * individual1 is sets as different individual from individual2
   * 
   * @param individual1URI
   * @param individual2URI
   */
  @WebMethod
  @WebResult(targetNamespace = "http://gate.ac.uk/ns/ontology/owlim")
  public void setDifferentIndividualFrom(
          @WebParam(name = "repositoryID") String repositoryID,
          @WebParam(name = "individual1URI") String individual1URI,
          @WebParam(name = "individual2URI") String individual2URI)
          throws GateOntologyException;

  /**
   * for the given individual, the method returns all individuals
   * registered as different from the given individual
   * 
   * @param individualURI
   * @return
   */
  @WebMethod
  @WebResult(targetNamespace = "http://gate.ac.uk/ns/ontology/owlim")
  @ResponseWrapper(className = "gate.creole.ontology.owlim.StringArrayResponse")
  public String[] getDifferentIndividualFrom(
          @WebParam(name = "repositoryID") String repositoryID,
          @WebParam(name = "individualURI") String individualURI)
          throws GateOntologyException;

  /**
   * individual1 is set as same as the individual2
   * 
   * @param individual1URI
   * @param individual2URI
   */
  @WebMethod
  @WebResult(targetNamespace = "http://gate.ac.uk/ns/ontology/owlim")
  public void setSameIndividualAs(
          @WebParam(name = "repositoryID") String repositoryID,
          @WebParam(name = "individual1URI") String individual1URI,
          @WebParam(name = "individual2URI") String individual2URI)
          throws GateOntologyException;

  /**
   * for the given individual, the method returns all individuals which
   * are registered as same as the provided individual
   * 
   * @param inidividualURI
   * @return
   */
  @WebMethod
  @WebResult(targetNamespace = "http://gate.ac.uk/ns/ontology/owlim")
  @ResponseWrapper(className = "gate.creole.ontology.owlim.StringArrayResponse")
  public String[] getSameIndividualAs(
          @WebParam(name = "repositoryID") String repositoryID,
          @WebParam(name = "individualURI") String individualURI)
          throws GateOntologyException;

  // ***********************************************
  // ********* Restrictions ***********************
  // ***********************************************

  /**
   * This method given a restriction uri returns the value for the
   * onProperty element.
   * 
   * @param repositoryId
   * @param restrictionURI
   * @return
   * @throws GateOntologyException
   */
  @WebMethod
  @WebResult(targetNamespace = "http://gate.ac.uk/ns/ontology/owlim")
  public Property getOnPropertyValue(
          @WebParam(name = "repositoryId") String repositoryId,
          @WebParam(name = "restrictionURI") String restrictionURI)
          throws GateOntologyException;

  /**
   * This method sets the value for onProperty element on the given
   * restriction.
   * 
   * @param repositoryId
   * @param restrictionURI
   * @param propertyURI
   * @throws GateOntologyException
   */
  @WebMethod
  @WebResult(targetNamespace = "http://gate.ac.uk/ns/ontology/owlim")
  public void setOnPropertyValue(
          @WebParam(name = "repositoryId") String repositoryId,
          @WebParam(name = "restrictionURI") String restrictionURI,
          @WebParam(name = "propertyURI") String propertyURI)
          throws GateOntologyException;

  /**
   * Gets the property value specified on the given restriction uri.
   * 
   * @param repositoryID
   * @param restrictionURI
   * @param restrictionType
   * @return
   * @throws GateOntologyException
   */
  @WebMethod
  @WebResult(targetNamespace = "http://gate.ac.uk/ns/ontology/owlim")
  public PropertyValue getPropertyValue(
          @WebParam(name = "repositoryID") String repositoryID,
          @WebParam(name = "restrictionURI") String restrictionURI,
          @WebParam(name = "restrictionType") byte restrictionType)
          throws GateOntologyException;

  /**
   * Sets the datatype uri for the given restriction uri.
   * 
   * @param repositoryID
   * @param restrictionURI
   * @param restrictionType
   * @param value
   * @param datatypeURI
   * @throws GateOntologyException
   */
  @WebMethod
  @WebResult(targetNamespace = "http://gate.ac.uk/ns/ontology/owlim")
  public void setPropertyValue(
          @WebParam(name = "repositoryID") String repositoryID,
          @WebParam(name = "restrictionURI") String restrictionURI,
          @WebParam(name = "restrictionType") byte restrictionType,
          @WebParam(name = "value") String value,
          @WebParam(name = "datatypeURI") String datatypeURI)
          throws GateOntologyException;

  /**
   * Gets the cardinality value specified on the given restriction uri.
   * 
   * @param repositoryID
   * @param restrictionURI
   * @param restrictionType - either of the following constants from the
   *          OConstants - ALL_VALUES_FROM_RESTRICTION,
   *          SOME_VALUES_FROM_RESTRICTION, and HAS_VALUE_RESTRICTION
   * @return
   * @throws GateOntologyException
   */
  @WebMethod
  @WebResult(targetNamespace = "http://gate.ac.uk/ns/ontology/owlim")
  public ResourceInfo getRestrictionValue(
          @WebParam(name = "repositoryID") String repositoryID,
          @WebParam(name = "restrictionURI") String restrictionURI,
          @WebParam(name = "restrictionType") byte restrictionType)
          throws GateOntologyException;

  /**
   * Sets the cardinality value for the given restriction uri.
   * 
   * @param repositoryID
   * @param restrictionURI
   * @param restrictionType - either of the following constants from the
   *          OConstants - ALL_VALUES_FROM_RESTRICTION,
   *          SOME_VALUES_FROM_RESTRICTION, and HAS_VALUE_RESTRICTION
   * @param value
   * @return
   * @throws GateOntologyException
   */
  @WebMethod
  @WebResult(targetNamespace = "http://gate.ac.uk/ns/ontology/owlim")
  public void setRestrictionValue(
          @WebParam(name = "repositoryID") String repositoryID,
          @WebParam(name = "restrictionURI") String restrictionURI,
          @WebParam(name = "restrictionType") byte restrictionType,
          @WebParam(name = "value") String value)
          throws GateOntologyException;

  /**
   * This method tells what type of restriction the given uri refers to.
   * If the given URI is not a restriction, the method returns -1.
   * Otherwise one of the following values from the OConstants class.
   * OWL_CLASS, CARDINALITY_RESTRICTION, MIN_CARDINALITY_RESTRICTION,
   * MAX_CARDINALITY_RESTRICTION, HAS_VALUE_RESTRICTION,
   * ALL_VALUES_FROM_RESTRICTION.
   * 
   * @param repositoryID
   * @param restrictionURI
   * @return
   * @throws GateOntologyException
   */
  @WebMethod
  @WebResult(targetNamespace = "http://gate.ac.uk/ns/ontology/owlim")
  public byte getClassType(
          @WebParam(name = "repositoryID") String repositoryID,
          @WebParam(name = "restrictionURI") String restrictionURI)
          throws GateOntologyException;

  
  @WebMethod
  @WebResult(targetNamespace = "http://gate.ac.uk/ns/ontology/owlim")
  @ResponseWrapper(className = "gate.creole.ontology.owlim.PropertyArrayResponse")
  public Property[] getPropertiesWithResourceAsDomain(
          @WebParam(name = "repositoryID") String repositoryID,
          @WebParam(name = "theResourceURI") String theResourceURI)
          throws GateOntologyException;
  
  
  @WebMethod
  @WebResult(targetNamespace = "http://gate.ac.uk/ns/ontology/owlim")
  @ResponseWrapper(className = "gate.creole.ontology.owlim.PropertyArrayResponse")
  public Property[] getPropertiesWithResourceAsRange(
          @WebParam(name = "repositoryID") String repositoryID,
          @WebParam(name = "theResourceURI") String theResourceURI)
          throws GateOntologyException;
  
  // ****************************************************
  // ******************** Generic statements ************
  // ****************************************************
  /**
   * The method is useful for adding statements into the graph. All
   * three values must exist in repository. These values are cast in
   * Resources and then added into the graph of repository.
   * 
   * @param subjectURI
   * @param predicateURI
   * @param objectURI
   */
  @WebMethod
  @WebResult(targetNamespace = "http://gate.ac.uk/ns/ontology/owlim")
  public void addStatement(
          @WebParam(name = "repositoryID") String repositoryID,
          @WebParam(name = "subjectURI") String subjectURI,
          @WebParam(name = "predicateURI") String predicateURI,
          @WebParam(name = "objectURI") String objectURI)
          throws GateOntologyException;

  /**
   * The method is useful for removing statements from the graph of
   * repository. All three values must exist in repository. these values
   * are cast in Resources and then removed from teh graph of
   * repository.
   * 
   * @param subjectURI
   * @param predicateURI
   * @param objectURI
   */
  @WebMethod
  @WebResult(targetNamespace = "http://gate.ac.uk/ns/ontology/owlim")
  public void removeStatement(
          @WebParam(name = "repositoryID") String repositoryID,
          @WebParam(name = "subjectURI") String subjectURI,
          @WebParam(name = "predicateURI") String predicateURI,
          @WebParam(name = "objectURI") String objectURI)
          throws GateOntologyException;


  /**
   * The method is useful for adding statements into the graph. All
   * three values must exist in repository. These values are cast in
   * Resources and then added into the graph of repository.
   * 
   * @param subjectURI
   * @param predicateURI
   * @param objectURI
   * @param datatype
   */
  @WebMethod(operationName = "addStatementWithDatatype")
  @WebResult(targetNamespace = "http://gate.ac.uk/ns/ontology/owlim")
  public void addStatement(
          @WebParam(name = "repositoryID") String repositoryID,
          @WebParam(name = "subject") String subject,
          @WebParam(name = "predicate") String predicate,
          @WebParam(name = "object") String object,
          @WebParam(name = "datatype") String datatype)
          throws GateOntologyException;

  /**
   * The method is useful for adding statements into the graph. All
   * three values must exist in repository. These values are cast in
   * Resources and then added into the graph of repository.
   * 
   * @param subjectURI
   * @param predicateURI
   * @param objectURI
   * @param datatype
   */
  @WebMethod(operationName = "removeStatementWithDatatype")
  @WebResult(targetNamespace = "http://gate.ac.uk/ns/ontology/owlim")
  public void removeStatement(
          @WebParam(name = "repositoryID") String repositoryID,
          @WebParam(name = "subject") String subject,
          @WebParam(name = "predicate") String predicate,
          @WebParam(name = "object") String object,
          @WebParam(name = "datatype") String datatype)
          throws GateOntologyException;

  
  /**
   * The method executes the query on repository and returns the toString()
   * result of the QueryResultTable.
   * @param sparqlQuery
   * @return
   */
  @WebMethod(operationName = "executeSerqlQuery")
  @WebResult(targetNamespace = "http://gate.ac.uk/ns/ontology/owlim")
  public String executeQuery(
          @WebParam(name = "repositoryID") String repositoryID, 
          @WebParam(name = "serqlQuery")  String serqlQuery) 
          throws GateOntologyException;
}
