/*
 *  RDFPropertyImpl.java
 *
 *  Niraj Aswani, 09/March/07
 *
 *  $Id: RDFPropertyImpl.java 12602 2010-05-06 14:28:51Z ian_roberts $
 */
package gate.creole.ontology.impl;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;
import gate.creole.ontology.AnnotationProperty;
import gate.creole.ontology.GateOntologyException;
import gate.creole.ontology.OClass;
import gate.creole.ontology.OConstants;
import gate.creole.ontology.OConstants.Closure;
import gate.creole.ontology.OInstance;
import gate.creole.ontology.OResource;
import gate.creole.ontology.Ontology;
import gate.creole.ontology.RDFProperty;
import gate.creole.ontology.OURI;
import gate.util.ClosableIterator;

/**
 * Implementation of the RDFProperty
 * 
 * @author niraj
 * 
 */
public class RDFPropertyImpl extends OResourceImpl implements RDFProperty {
  /**
   * Constructor
   * 
   * @param aURI
   * @param ontology
   * @param owlimPort
   */
  public RDFPropertyImpl(OURI aURI, Ontology ontology,
          OntologyService owlimPort) {
    super(aURI, ontology, owlimPort);
  }

  /*
   * (non-Javadoc)
   * 
   * @see gate.creole.ontology.RDFProperty#setSamePropertyAs(gate.creole.ontology.RDFProperty)
   */
  public void setEquivalentPropertyAs(RDFProperty theProperty) {
    if(this == theProperty) {
      Utils
              .warning("setEquivalentPropertyAs(RDFProperty) : The source and the argument properties refer to the same property and therefore cannot be set as equivalent");
      return;
    }

    ontologyService.setEquivalentPropertyAs(nodeId.toString(), theProperty
            .getOURI().toString());
    ontology.fireResourceRelationChanged(this, theProperty, OConstants.EQUIVALENT_PROPERTY_EVENT);
  }

  /*
   * (non-Javadoc)
   * 
   * @see gate.creole.ontology.RDFProperty#getSamePropertyAs()
   */
  public Set<RDFProperty> getEquivalentPropertyAs() {
    Property[] properties = ontologyService.getEquivalentPropertyAs(nodeId
            .toString());
    Set<RDFProperty> set = new HashSet<RDFProperty>();
    for(int i = 0; i < properties.length; i++) {
      set.add(Utils.createOProperty(this.ontology,
              this.ontologyService, properties[i].getUri(), properties[i].getType()));
    }
    return set;
  }

  /*
   * (non-Javadoc)
   * 
   * @see gate.creole.ontology.RDFProperty#isSamePropertyAs(gate.creole.ontology.RDFProperty)
   */
  public boolean isEquivalentPropertyAs(RDFProperty theProperty) {
    return ontologyService.isEquivalentPropertyAs(nodeId.toString(),
            theProperty.getOURI().toString());
  }

  /*
   * (non-Javadoc)
   * 
   * @see gate.creole.ontology.RDFProperty#getSuperProperties(byte)
   */
  public Set<RDFProperty> getSuperProperties(byte closure) {
    return getSuperProperties(
        closure == OConstants.DIRECT_CLOSURE ?
          Closure.DIRECT_CLOSURE : Closure.TRANSITIVE_CLOSURE);
    //throw new UnsupportedOperationException("Method not supported anymore with this parameter type");
  }


  public Set<RDFProperty> getSuperProperties(Closure closure) { 
    Property[] properties = ontologyService.getSuperProperties(nodeId
            .toString(), closure);
    Set<RDFProperty> set = new HashSet<RDFProperty>();
    for(int i = 0; i < properties.length; i++) {
      set.add(Utils.createOProperty(this.ontology,
              this.ontologyService, properties[i].getUri(), properties[i].getType()));
    }
    return set;
  }

  /*
   * (non-Javadoc)
   * 
   * @see gate.creole.ontology.RDFProperty#isSuperPropertyOf(gate.creole.ontology.RDFProperty,
   *      byte)
   */
  public boolean isSuperPropertyOf(RDFProperty theProperty, byte closure) {
    //throw new UnsupportedOperationException("Method not supported anymore with this parameter type");
    return isSuperPropertyOf(theProperty,
        closure == OConstants.DIRECT_CLOSURE ?
          Closure.DIRECT_CLOSURE : Closure.TRANSITIVE_CLOSURE);
  }

  public boolean isSuperPropertyOf(RDFProperty theProperty, Closure closure) {
    return ontologyService.isSuperPropertyOf(nodeId.toString(),
            theProperty.getOURI().toString(), closure);
  }

  /*
   * (non-Javadoc)
   * 
   * @see gate.creole.ontology.RDFProperty#addSubProperty(gate.creole.ontology.RDFProperty)
   */
  public void addSubProperty(RDFProperty theProperty) {
    // lets first check if the current class is a subclass of the
    // subClass. If so,
    // we don't allow this.
    if(this == theProperty) {
      Utils
              .warning("addSubProperty(RDFProperty) : The super and sub properties are same.");
      return;
    }

    if(this.isSubPropertyOf(theProperty, OConstants.Closure.TRANSITIVE_CLOSURE)) {
      Utils.warning(theProperty.getOURI().toString()
              + " is a super property of " + this.getOURI().toString());
      return;
    }

    if(!(this.getClass().getName().equals(theProperty.getClass().getName()))) {
      Utils.warning(this.getOURI().toString() + " and "
              + theProperty.getOURI().toString()
              + " must be of the same property type "
              + this.getOURI().toString());
      return;
    }

    ontologyService.addSubProperty(nodeId.toString(), theProperty
            .getOURI().toString());
    ontology.fireResourceRelationChanged(this, theProperty, OConstants.SUB_PROPERTY_ADDED_EVENT);    
  }

  /*
   * (non-Javadoc)
   * 
   * @see gate.creole.ontology.RDFProperty#removeSubProperty(gate.creole.ontology.RDFProperty)
   */
  public void removeSubProperty(RDFProperty theProperty) {
    ontologyService.removeSubProperty(nodeId.toString(), theProperty
            .getOURI().toString());
    ontology.fireResourceRelationChanged(this, theProperty, OConstants.SUB_PROPERTY_REMOVED_EVENT);    
  }

  /*
   * (non-Javadoc)
   * 
   * @see gate.creole.ontology.RDFProperty#getSubProperties(byte)
   */
  public Set<RDFProperty> getSubProperties(byte closure) {
    //throw new UnsupportedOperationException("Method not supported anymore with this parameter type");
    return getSubProperties(
        closure == OConstants.DIRECT_CLOSURE ?
          Closure.DIRECT_CLOSURE : Closure.TRANSITIVE_CLOSURE);
  }

  public Set<RDFProperty> getSubProperties(Closure closure) {
    Property[] properties = ontologyService.getSubProperties(
            nodeId.toString(), closure);
    Set<RDFProperty> set = new HashSet<RDFProperty>();
    for(int i = 0; i < properties.length; i++) {
      set.add(Utils.createOProperty(this.ontology,
              this.ontologyService, properties[i].getUri(), properties[i].getType()));
    }
    return set;
  }

  /*
   * (non-Javadoc)
   * 
   * @see gate.creole.ontology.RDFProperty#isSubPropertyOf(gate.creole.ontology.RDFProperty,
   *      byte)
   */
  public boolean isSubPropertyOf(RDFProperty theProperty, byte closure) {
    //throw new UnsupportedOperationException("Method not supported anymore with this parameter type");
    return isSubPropertyOf(theProperty,
        closure == OConstants.DIRECT_CLOSURE ?
          Closure.DIRECT_CLOSURE : Closure.TRANSITIVE_CLOSURE);
  }

  public boolean isSubPropertyOf(RDFProperty theProperty, Closure closure) {
    return ontologyService.isSubPropertyOf(theProperty.getOURI()
            .toString(), nodeId.toString(), closure);
  }

  /*
   * (non-Javadoc)
   * 
   * @see gate.creole.ontology.RDFProperty#isFunctional()
   */
  public boolean isFunctional() {
    return ontologyService.isFunctional(nodeId.toString());
  }

  /*
   * (non-Javadoc)
   * 
   * @see gate.creole.ontology.RDFProperty#setFunctional(boolean)
   */
  public void setFunctional(boolean functional) {
    ontologyService.setFunctional(nodeId.toString(), functional);
  }

  /*
   * (non-Javadoc)
   * 
   * @see gate.creole.ontology.RDFProperty#isInverseFunctional()
   */
  public boolean isInverseFunctional() {
    return ontologyService.isInverseFunctional(nodeId.toString());
  }

  /*
   * (non-Javadoc)
   * 
   * @see gate.creole.ontology.RDFProperty#setInverseFunctional(boolean)
   */
  public void setInverseFunctional(boolean inverseFunctional) {
    ontologyService.setInverseFunctional(nodeId.toString(),
            inverseFunctional);
  }

  /*
   * (non-Javadoc)
   * 
   * @see gate.creole.ontology.RDFProperty#isValidRange(gate.creole.ontology.OResource)
   */
  public boolean isValidRange(OResource aResource) {
    ResourceInfo[] listOfOResources = ontologyService.getRange(nodeId
            .toString());
    if(listOfOResources.length == 0) {
      return true;
    }
    // lets first make a easy move
    List<String> list = new ArrayList<String>();
    for(int i = 0; i < listOfOResources.length; i++) {
      list.add(listOfOResources[i].getUri());
    }
    if(list.contains(aResource.getONodeID().toString())) {
      return true;
    }
    if(aResource instanceof OInstance) {
      // lets find out all its super classes
      ResourceInfo[] oClasses = ontologyService.getClassesOfIndividual(
              aResource.getONodeID().toString(), OConstants.Closure.TRANSITIVE_CLOSURE);
      // if any of them is in listOfOResource, we return true, else
      // false
      List<String> oClassList = new ArrayList<String>();
      for(int i = 0; i < oClasses.length; i++) {
        oClassList.add(oClasses[i].getUri());
      }
      if(Collections.disjoint(oClassList, list))
        return false;
      else return true;
    }
    if(aResource instanceof OClass) {
      // lets find out all its super classes
      ResourceInfo[] oClasses = ontologyService.getSuperClasses(
              aResource.getONodeID().toString(), OConstants.Closure.TRANSITIVE_CLOSURE);
      // if any of them is in listOfOResource, we return true, else
      // false
      List<String> oClassList = new ArrayList<String>();
      for(int i = 0; i < oClasses.length; i++) {
        oClassList.add(oClasses[i].getUri());
      }
      if(Collections.disjoint(oClassList, list))
        return false;
      else return true;
    }
    if(aResource instanceof RDFProperty
            && !(aResource instanceof AnnotationProperty)) {
      Property[] oProps = ontologyService.getSuperProperties(aResource
              .getONodeID().toString(), OConstants.Closure.TRANSITIVE_CLOSURE);
      for(int i = 0; i < oProps.length; i++) {
        if(list.contains(oProps[i].getUri())) {
          return true;
        }
      }
    }
    return false;
  }

  /*
   * (non-Javadoc)
   * 
   * @see gate.creole.ontology.RDFProperty#isValidDomain(gate.creole.ontology.OResource)
   */
  public boolean isValidDomain(OResource aResource) {
    ResourceInfo[] listOfOResources = ontologyService.getDomain(nodeId
            .toString());
    if(listOfOResources.length == 0) return true;

    Set<String> list = new HashSet<String>();
    for(int i = 0; i < listOfOResources.length; i++) {
      list.add(listOfOResources[i].getUri().toString());
      OResource resource = ontology.getOResourceFromMap(listOfOResources[i]
              .getUri());
      if(resource != null && resource instanceof OClass) {
        Set<OClass> classes = ((OClass)resource)
                .getSubClasses(OConstants.Closure.TRANSITIVE_CLOSURE);
        Iterator<OClass> iter = classes.iterator();
        while(iter.hasNext()) {
          list.add(iter.next().getONodeID().toString());
        }
      }
      else if(resource != null && resource instanceof RDFProperty
              && !(resource instanceof AnnotationProperty)) {
        Set<RDFProperty> props = ((RDFProperty)resource)
                .getSubProperties(OConstants.Closure.TRANSITIVE_CLOSURE);
        Iterator<RDFProperty> iter = props.iterator();
        while(iter.hasNext()) {
          list.add(iter.next().getOURI().toString());
        }
      }
    }

    if(list.contains(aResource.getONodeID().toString())) {
      return true;
    }
    if(aResource instanceof OInstance) {
      // lets find out all its super classes
      ResourceInfo[] oClasses = ontologyService.getClassesOfIndividual(
              aResource.getONodeID().toString(), OConstants.Closure.DIRECT_CLOSURE);
      // if any of them is in listOfOResource, we return true, else
      // false
      Set<String> oClassList = new HashSet<String>();
      for(int i = 0; i < oClasses.length; i++) {
        oClassList.add(oClasses[i].getUri());
      }
      return list.containsAll(oClassList);
    }

    if(aResource instanceof OClass) {
      return list.contains(aResource);
    }

    if(aResource instanceof RDFProperty
            && !(aResource instanceof AnnotationProperty)) {
      if(list.contains(aResource.getONodeID().toString())) {
        return true;
      }
    }

    return false;
  }

  /*
   * (non-Javadoc)
   * 
   * @see gate.creole.ontology.RDFProperty#getDomain()
   */
  public Set<OResource> getDomain() {
    ResourceInfo[] list = ontologyService.getDomain(nodeId.toString());
    Set<OResource> domain = new HashSet<OResource>();
    //List<String> individuals = Arrays.asList(ontologyService
    //        .getIndividuals());
      ClosableIterator<OInstance> ii = ontologyService.getInstancesIterator(null, null);
      List<String> individuals = new ArrayList<String>();
      while(ii.hasNext()) {
        individuals.add(ii.next().getOURI().toString());
      }

    // these resources can be anything - an instance, a property, or a
    // class
    for(int i = 0; i < list.length; i++) {
      // lets first search if it is available in ontology cache
      OResource resource = null;
      if(individuals.contains(list[i].toString())) {
        domain.add(Utils.createOInstance(this.ontology,
                this.ontologyService, list[i].getUri()));
        continue;
      }
      // otherwise we need to create it
      if(ontologyService.hasClass(list[i].getUri())) {
        // lets first check if this is a valid URI
        domain.add(Utils.createOClass(this.ontology,
                this.ontologyService, list[i].getUri(), list[i].getClassType()));
        continue;
      }
      Property prop = ontologyService.getPropertyFromOntology(list[i]
              .getUri());
      domain.add(Utils.createOProperty(this.ontology,
              this.ontologyService, prop.getUri(), prop.getType()));
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
    Set<OResource> domain = new HashSet<OResource>();
    //List<String> individuals = Arrays.asList(ontologyService
    //        .getIndividuals());
      ClosableIterator<OInstance> ii = ontologyService.getInstancesIterator(null,null);
      List<String> individuals = new ArrayList<String>();
      while(ii.hasNext()) {
        individuals.add(ii.next().getOURI().toString());
      }

    // these resources can be anything - an instance, a property, or a
    // class
    for(int i = 0; i < list.length; i++) {
      // lets first search if it is available in ontology cache
      OResource resource = null; // ontology.getOResourceFromMap(list[i].getUri());
      if(individuals.contains(list[i].toString())) {
        domain.add(Utils.createOInstance(this.ontology,
                this.ontologyService, list[i].getUri()));
        continue;
      }
      // otherwise we need to create it
      if(ontologyService.hasClass(list[i].getUri())) {
        domain.add(Utils.createOClass(this.ontology,
                this.ontologyService, list[i].getUri(), list[i].getClassType()));
        continue;
      }
      Property prop = ontologyService.getPropertyFromOntology(list[i]
              .getUri());
      domain.add(Utils.createOProperty(this.ontology,
              this.ontologyService, prop.getUri(), prop.getType()));
    }
    return domain;
  }

  public OURI getOURI() {
    if(getONodeID().isAnonymousResource()) {
      throw new GateOntologyException("Cannot return OURI, property strangely is a blank node");
    } else {
      return (OURI)getONodeID();
    }
  }
}
