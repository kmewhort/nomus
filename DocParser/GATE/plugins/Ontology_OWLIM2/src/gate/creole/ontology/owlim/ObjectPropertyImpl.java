/*
 *  ObjectPropertyImpl.java
 *
 *  Niraj Aswani, 09/March/07
 *
 *  $Id: ObjectPropertyImpl.java 11598 2009-10-13 13:44:17Z johann_p $
 */
package gate.creole.ontology.owlim;

import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;

import gate.creole.ontology.GateOntologyException;
import gate.creole.ontology.OClass;
import gate.creole.ontology.OConstants;
import gate.creole.ontology.OInstance;
import gate.creole.ontology.OResource;
import gate.creole.ontology.ObjectProperty;
import gate.creole.ontology.Ontology;
import gate.creole.ontology.URI;

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
  public ObjectPropertyImpl(URI aURI, Ontology ontology, String repositoryID,
          OWLIM owlimPort) {
    super(aURI, ontology, repositoryID, owlimPort);
  }

  /*
   * (non-Javadoc)
   * 
   * @see gate.creole.ontology.ObjectProperty#getInverseProperties()
   */
  public Set<ObjectProperty> getInverseProperties() {
    Property[] properties = owlim.getInverseProperties(this.repositoryID,
            this.uri.toString());
    Set<ObjectProperty> set = new HashSet<ObjectProperty>();
    for(int i = 0; i < properties.length; i++) {
      byte type = properties[i].getType();
      if(type != OConstants.OBJECT_PROPERTY
              && type != OConstants.SYMMETRIC_PROPERTY
              && type != OConstants.TRANSITIVE_PROPERTY)
        throw new GateOntologyException(
                "Invalid Property type returned as an inverse property");
      set.add((ObjectProperty)Utils.createOProperty(this.repositoryID,
              this.ontology, this.owlim, properties[i].getUri(), properties[i]
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

    owlim.setInverseOf(this.repositoryID, uri.toString(), theInverse.getURI()
            .toString());
  }

  /*
   * (non-Javadoc)
   * 
   * @see gate.creole.ontology.ObjectProperty#isValidRange(gate.creole.ontology.OInstance)
   */
  public boolean isValidRange(OInstance anInstance) {
    ResourceInfo[] oClasses = owlim.getRange(this.repositoryID, this.uri
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
          listOfOClasses.add(iter.next().getURI().toString());
        }
      }
    }
    // we need to obtain all the classes of anInstance
    ResourceInfo[] instanceOClasses = owlim.getClassesOfIndividual(
            this.repositoryID, anInstance.getURI().toString(),
            OConstants.DIRECT_CLOSURE);
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
    ResourceInfo[] oClasses = owlim.getDomain(this.repositoryID, this.uri
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
          listOfOClasses.add(iter.next().getURI().toString());
        }
      }
    }
    // we need to obtain all the classes of anInstance
    ResourceInfo[] instanceOClasses = owlim.getClassesOfIndividual(
            this.repositoryID, anInstance.getURI().toString(),
            OConstants.DIRECT_CLOSURE);
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
    ResourceInfo[] list = owlim.getDomain(this.repositoryID, uri.toString());
    // this is a list of classes
    Set<OResource> domain = new HashSet<OResource>();
    // these resources can be anything - an instance, a property, or a
    // class
    for(int i = 0; i < list.length; i++) {
      domain.add(Utils.createOClass(this.repositoryID, this.ontology,
              this.owlim, list[i].getUri(), list[i].getClassType()));
    }
    return domain;
  }

  /*
   * (non-Javadoc)
   * 
   * @see gate.creole.ontology.RDFProperty#getRange()
   */
  public Set<OResource> getRange() {
    ResourceInfo[] list = owlim.getRange(this.repositoryID, uri.toString());
    // this is a list of classes
    Set<OResource> domain = new HashSet<OResource>();
    // these resources can be anything - an instance, a property, or a
    // class
    for(int i = 0; i < list.length; i++) {
      domain.add(Utils.createOClass(this.repositoryID, this.ontology,
              this.owlim, list[i].getUri(), list[i].getClassType()));
    }
    return domain;
  }
}
