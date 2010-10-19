/*
 *  ObjectPropertyImpl.java
 *
 *  Niraj Aswani, 09/March/07
 *
 *  $Id: ObjectPropertyImpl.java 12602 2010-05-06 14:28:51Z ian_roberts $
 */
package gate.creole.ontology.impl;

import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;

import gate.creole.ontology.GateOntologyException;
import gate.creole.ontology.OClass;
import gate.creole.ontology.OConstants;
import gate.creole.ontology.OInstance;
import gate.creole.ontology.OResource;
import gate.creole.ontology.OURI;
import gate.creole.ontology.ObjectProperty;
import gate.creole.ontology.Ontology;

/**
 * Implementation of the ObjectProperty
 * 
 * @author niraj
 * 
 */
public class ObjectPropertyImpl extends RDFPropertyImpl implements
                                                       ObjectProperty {
  /**
   * Constructor
   * 
   * @param aURI
   * @param ontology
   * @param repositoryID
   * @param owlimPort
   */
  public ObjectPropertyImpl(OURI aURI, Ontology ontology,
          OntologyService owlimPort) {
    super(aURI, ontology, owlimPort);
  }

  /*
   * (non-Javadoc)
   * 
   * @see gate.creole.ontology.ObjectProperty#getInverseProperties()
   */
  public Set<ObjectProperty> getInverseProperties() {
    Property[] properties = ontologyService.getInverseProperties(
            this.nodeId.toString());
    Set<ObjectProperty> set = new HashSet<ObjectProperty>();
    for(int i = 0; i < properties.length; i++) {
      byte type = properties[i].getType();
      if(type != OConstants.OBJECT_PROPERTY
              && type != OConstants.SYMMETRIC_PROPERTY
              && type != OConstants.TRANSITIVE_PROPERTY)
        throw new GateOntologyException(
                "Invalid Property type returned as an inverse property");
      set.add((ObjectProperty)Utils.createOProperty(
              this.ontology, this.ontologyService, properties[i].getUri(), properties[i]
                      .getType()));
    }
    return set;
  }

  /*
   * (non-Javadoc)
   * 
   * @see gate.creole.ontology.ObjectProperty#setInverseOf(gate.creole.ontology.ObjectProperty)
   */
  public void setInverseOf(ObjectProperty theInverse) {
    if(this == theInverse) {
      Utils
              .warning("setInverseOf(ObjectProperty) : The source and the argument properties are referring to the same property");
      return;
    }

    ontologyService.setInverseOf(nodeId.toString(), theInverse.getOURI()
            .toString());
  }

  /*
   * (non-Javadoc)
   * 
   * @see gate.creole.ontology.ObjectProperty#isValidRange(gate.creole.ontology.OInstance)
   */
  public boolean isValidRange(OInstance anInstance) {
    ResourceInfo[] oClasses = ontologyService.getRange(this.nodeId
            .toString());
    if(oClasses.length == 0) return true;
    // obtain sub classes of
    Set<String> listOfOClasses = new HashSet<String>();
    for(int i = 0; i < oClasses.length; i++) {
      listOfOClasses.add(oClasses[i].getUri());
      // TODO: properly remove use of map!
      OResource resource = ontology.getOResourceFromMap(oClasses[i].getUri());
      //OClass c = ontology.getOClass(new URI(oClasses[i].getUri()));
      if(resource != null && resource instanceof OClass) {
        Set<OClass> classes = ((OClass)resource)
                .getSubClasses(OConstants.TRANSITIVE_CLOSURE);
        Iterator<OClass> iter = classes.iterator();
        while(iter.hasNext()) {
          listOfOClasses.add(iter.next().getONodeID().toString());
        }
      }
    }
    // we need to obstain all the classes of anInstance
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
   * @see gate.creole.ontology.DatatypeProperty#isValidDomain(gate.creole.ontology.OInstance)
   */
  public boolean isValidDomain(OInstance anInstance) {
    ResourceInfo[] oClasses = ontologyService.getDomain(this.nodeId
            .toString());
    if(oClasses.length == 0) return true;
    // obtain sub classes of
    Set<String> listOfOClasses = new HashSet<String>();
    for(int i = 0; i < oClasses.length; i++) {
      listOfOClasses.add(oClasses[i].getUri());
      OResource resource = ontology.getOResourceFromMap(oClasses[i].getUri());
      if(resource != null && resource instanceof OClass) {
        Set<OClass> classes = ((OClass)resource)
                .getSubClasses(OConstants.TRANSITIVE_CLOSURE);
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
    if(aResource instanceof OInstance)
      return isValidRange((OInstance)aResource);
    return false;
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
    // these resources can be anything - an instance, a property, or a
    // class
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
    ResourceInfo[] list = ontologyService.getRange(nodeId.toString());
    // this is a list of classes
    Set<OResource> domain = new HashSet<OResource>();
    // these resources can be anything - an instance, a property, or a
    // class
    for(int i = 0; i < list.length; i++) {
      domain.add(Utils.createOClass(this.ontology,
              this.ontologyService, list[i].getUri(), list[i].getClassType()));
    }
    return domain;
  }
}
