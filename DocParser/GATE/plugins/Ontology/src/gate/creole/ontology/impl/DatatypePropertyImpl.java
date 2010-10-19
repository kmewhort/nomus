/*
 *  DatatypePropertyImpl.java
 *
 *  Niraj Aswani, 09/March/07
 *
 *  $Id: DatatypePropertyImpl.java 12602 2010-05-06 14:28:51Z ian_roberts $
 */
package gate.creole.ontology.impl;

import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;
import gate.creole.ontology.DataType;
import gate.creole.ontology.DatatypeProperty;
import gate.creole.ontology.GateOntologyException;
import gate.creole.ontology.OClass;
import gate.creole.ontology.OConstants;
import gate.creole.ontology.OInstance;
import gate.creole.ontology.OResource;
import gate.creole.ontology.OURI;
import gate.creole.ontology.Ontology;


/**
 * Implementation of the DatatypeProperty
 * @author niraj
 * 
 */
public class DatatypePropertyImpl extends RDFPropertyImpl implements
                                                         DatatypeProperty {
  /**
   * Constructor
   * @param aURI
   * @param ontology
   * @param owlimPort
   */
  public DatatypePropertyImpl(OURI aURI, Ontology ontology,
          OntologyService owlimPort) {
    super(aURI, ontology, owlimPort); 
  }

  /*
   * (non-Javadoc)
   * 
   * @see gate.creole.ontology.DatatypeProperty#getDataType()
   */
  public DataType getDataType() {
      String datatypeURI = ontologyService.getDatatype(this.nodeId
              .toString());
      if(datatypeURI == null) { return DataType.getStringDataType(); }
      return OntologyUtilities.getDataType(datatypeURI);
  }

  /*
   * (non-Javadoc)
   * 
   * @see gate.creole.ontology.DatatypeProperty#isValidDataTypeValue(java.lang.String)
   */
  public boolean isValidDataTypeValue(String value) {
    return getDataType().isValidValue(value);
  }

  /*
   * (non-Javadoc)
   * 
   * @see gate.creole.ontology.DatatypeProperty#isValidDomain(gate.creole.ontology.OInstance)
   */
public boolean isValidDomain(OInstance anInstance) {
      ResourceInfo[] oClasses = ontologyService.getDomain(this.nodeId
              .toString());
      if(oClasses.length == 0) return true;
      
      // obtain sub classes of
      // TODO: deal with use of getOResourcesFromMap here!!!!
      Set<String> listOfOClasses = new HashSet<String>();
      for(int i = 0; i < oClasses.length; i++) {
        listOfOClasses.add(oClasses[i].getUri());
        OResource resource = ontology.getOResourceFromMap(oClasses[i].getUri());
        if(resource != null && resource instanceof OClass) {
          Set<OClass> classes = ((OClass)resource).getSubClasses(OConstants.Closure.TRANSITIVE_CLOSURE);
          Iterator<OClass> iter = classes.iterator();
          while(iter.hasNext()) {
            listOfOClasses.add(iter.next().getONodeID().toString());
           }
        }
      }
      // we need to obtain all the classes of anInstance
      ResourceInfo[] instanceOClasses = ontologyService.getClassesOfIndividual(
              anInstance.getOURI().toString(),
              OConstants.Closure.DIRECT_CLOSURE);
      Set<String> listOfICs = new HashSet<String>();
      for(int i = 0; i < instanceOClasses.length; i++) {
        listOfICs.add(instanceOClasses[i].getUri());
      }
      
      return listOfOClasses.containsAll(listOfICs);
  }
  /*
   * (non-Javadoc)
   * 
   * @see gate.creole.ontology.RDFProperty#isValidRange(gate.creole.ontology.OResource)
   */
  public boolean isValidRange(OResource aResource) {
    throw new GateOntologyException(
            "Datatype Properties do not have Range, but a Datatype. Please use the isValidDatatypeValue(String value) method.");
  }

  /*
   * (non-Javadoc)
   * 
   * @see gate.creole.ontology.RDFProperty#isValidDomain(gate.creole.ontology.OResource)
   */
  public boolean isValidDomain(OResource aResource) {
    if(aResource instanceof OInstance)
      return isValidDomain((OInstance)aResource);
    return false;
  }

  /*
   * (non-Javadoc)
   * 
   * @see gate.creole.ontology.RDFProperty#getDomain()
   */
  public Set<OResource> getDomain() {
      ResourceInfo[] list = ontologyService.getDomain(nodeId.toString());
      // this is a list of classes
      Set<OResource> domain = new HashSet<OResource>();
      // these resources can be anything - an instance, a property, or a class
      for(int i = 0; i < list.length; i++) {
        domain.add(Utils.createOClass(this.ontology,
                this.ontologyService, list[i].getUri(), list[i].getClassType()));
      }
      return domain;
  }

  /*
   * (non-Javadoc)
   * 
   * @see gate.creole.ontology.RDFProperty#getRange()
   */
  public Set<OResource> getRange() {
    throw new GateOntologyException(
            "Datatype Properties do not have Range, but a Datatype specified. Please use the getDatatype() method.");
  }
}
