/*
 *  RDFPropertyImpl.java
 *
 *  Niraj Aswani, 09/March/07
 *
 *  $Id: RDFPropertyImpl.java 12549 2010-04-26 13:52:40Z ian_roberts $
 */
package gate.creole.ontology.owlim;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;
import gate.creole.ontology.AnnotationProperty;
import gate.creole.ontology.OClass;
import gate.creole.ontology.OConstants;
import gate.creole.ontology.OConstants.Closure;
import gate.creole.ontology.OInstance;
import gate.creole.ontology.OResource;
import gate.creole.ontology.OURI;
import gate.creole.ontology.Ontology;
import gate.creole.ontology.RDFProperty;
import gate.creole.ontology.URI;

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
   * @param repositoryID
   * @param owlimPort
   */
  public RDFPropertyImpl(URI aURI, Ontology ontology, String repositoryID,
          OWLIM owlimPort) {
    super(aURI, ontology, repositoryID, owlimPort);
  }

  /*
   * (non-Javadoc)
   * 
   * @see
   * gate.creole.ontology.RDFProperty#setSamePropertyAs(gate.creole.
   * ontology.RDFProperty)
   */
  public void setEquivalentPropertyAs(RDFProperty theProperty) {
    if(this == theProperty) {
      Utils
              .warning("setEquivalentPropertyAs(RDFProperty) : The source and the argument properties refer to the same property and therefore cannot be set as equivalent");
      return;
    }

    owlim.setEquivalentPropertyAs(repositoryID, uri.toString(), theProperty
            .getURI().toString());
    ontology.fireResourceRelationChanged(this, theProperty,
            OConstants.EQUIVALENT_PROPERTY_EVENT);
  }

  /*
   * (non-Javadoc)
   * 
   * @see gate.creole.ontology.RDFProperty#getSamePropertyAs()
   */
  public Set<RDFProperty> getEquivalentPropertyAs() {
    Property[] properties = owlim.getEquivalentPropertyAs(repositoryID, uri
            .toString());
    Set<RDFProperty> set = new HashSet<RDFProperty>();
    for(int i = 0; i < properties.length; i++) {
      set.add(Utils.createOProperty(this.repositoryID, this.ontology,
              this.owlim, properties[i].getUri(), properties[i].getType()));
    }
    return set;
  }

  /*
   * (non-Javadoc)
   * 
   * @see
   * gate.creole.ontology.RDFProperty#isSamePropertyAs(gate.creole.ontology
   * .RDFProperty)
   */
  public boolean isEquivalentPropertyAs(RDFProperty theProperty) {
    return owlim.isEquivalentPropertyAs(this.repositoryID, uri.toString(),
            theProperty.getURI().toString());
  }

  /*
   * (non-Javadoc)
   * 
   * @see gate.creole.ontology.RDFProperty#getSuperProperties(byte)
   */
  public Set<RDFProperty> getSuperProperties(byte closure) {
    Property[] properties = owlim.getSuperProperties(repositoryID, uri
            .toString(), closure);
    Set<RDFProperty> set = new HashSet<RDFProperty>();
    for(int i = 0; i < properties.length; i++) {
      set.add(Utils.createOProperty(this.repositoryID, this.ontology,
              this.owlim, properties[i].getUri(), properties[i].getType()));
    }
    return set;
  }

  public Set<RDFProperty> getSuperProperties(Closure closure) {
    byte bclosure = closure == Closure.DIRECT_CLOSURE
            ? OConstants.DIRECT_CLOSURE
            : OConstants.TRANSITIVE_CLOSURE;
    return getSuperProperties(bclosure);
  }

  /*
   * (non-Javadoc)
   * 
   * @see
   * gate.creole.ontology.RDFProperty#isSuperPropertyOf(gate.creole.
   * ontology.RDFProperty, byte)
   */
  public boolean isSuperPropertyOf(RDFProperty theProperty, byte closure) {
    return owlim.isSuperPropertyOf(this.repositoryID, uri.toString(),
            theProperty.getURI().toString(), closure);
  }

  public boolean isSuperPropertyOf(RDFProperty theProperty, Closure closure) {
    byte bclosure = closure == Closure.DIRECT_CLOSURE
    ? OConstants.DIRECT_CLOSURE
    : OConstants.TRANSITIVE_CLOSURE;
    return isSuperPropertyOf(theProperty, bclosure);
  }

  /*
   * (non-Javadoc)
   * 
   * @see
   * gate.creole.ontology.RDFProperty#addSubProperty(gate.creole.ontology
   * .RDFProperty)
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

    if(this.isSubPropertyOf(theProperty, OConstants.TRANSITIVE_CLOSURE)) {
      Utils.warning(theProperty.getURI().toString()
              + " is a super property of " + this.getURI().toString());
      return;
    }

    if(!(this.getClass().getName().equals(theProperty.getClass().getName()))) {
      Utils.warning(this.getURI().toString() + " and "
              + theProperty.getURI().toString()
              + " must be of the same property type "
              + this.getURI().toString());
      return;
    }

    owlim.addSubProperty(this.repositoryID, uri.toString(), theProperty
            .getURI().toString());
    ontology.fireResourceRelationChanged(this, theProperty,
            OConstants.SUB_PROPERTY_ADDED_EVENT);
  }

  /*
   * (non-Javadoc)
   * 
   * @see
   * gate.creole.ontology.RDFProperty#removeSubProperty(gate.creole.
   * ontology.RDFProperty)
   */
  public void removeSubProperty(RDFProperty theProperty) {
    owlim.removeSubProperty(this.repositoryID, uri.toString(), theProperty
            .getURI().toString());
    ontology.fireResourceRelationChanged(this, theProperty,
            OConstants.SUB_PROPERTY_REMOVED_EVENT);
  }

  /*
   * (non-Javadoc)
   * 
   * @see gate.creole.ontology.RDFProperty#getSubProperties(byte)
   */
  public Set<RDFProperty> getSubProperties(byte closure) {
    Property[] properties = owlim.getSubProperties(repositoryID,
            uri.toString(), closure);
    Set<RDFProperty> set = new HashSet<RDFProperty>();
    for(int i = 0; i < properties.length; i++) {
      set.add(Utils.createOProperty(this.repositoryID, this.ontology,
              this.owlim, properties[i].getUri(), properties[i].getType()));
    }
    return set;
  }

  public Set<RDFProperty> getSubProperties(Closure closure) {
    byte bclosure = closure == Closure.DIRECT_CLOSURE
    ? OConstants.DIRECT_CLOSURE
    : OConstants.TRANSITIVE_CLOSURE;
    return getSubProperties(bclosure);
  }

  /*
   * (non-Javadoc)
   * 
   * @see
   * gate.creole.ontology.RDFProperty#isSubPropertyOf(gate.creole.ontology
   * .RDFProperty, byte)
   */
  public boolean isSubPropertyOf(RDFProperty theProperty, byte closure) {
    return owlim.isSubPropertyOf(this.repositoryID, theProperty.getURI()
            .toString(), uri.toString(), closure);
  }

  public boolean isSubPropertyOf(RDFProperty theProperty, Closure closure) {
    byte bclosure = closure == Closure.DIRECT_CLOSURE
    ? OConstants.DIRECT_CLOSURE
    : OConstants.TRANSITIVE_CLOSURE;

    return isSubPropertyOf(theProperty, bclosure);
  }

  /*
   * (non-Javadoc)
   * 
   * @see gate.creole.ontology.RDFProperty#isFunctional()
   */
  public boolean isFunctional() {
    return owlim.isFunctional(this.repositoryID, uri.toString());
  }

  /*
   * (non-Javadoc)
   * 
   * @see gate.creole.ontology.RDFProperty#setFunctional(boolean)
   */
  public void setFunctional(boolean functional) {
    owlim.setFunctional(this.repositoryID, uri.toString(), functional);
  }

  /*
   * (non-Javadoc)
   * 
   * @see gate.creole.ontology.RDFProperty#isInverseFunctional()
   */
  public boolean isInverseFunctional() {
    return owlim.isInverseFunctional(this.repositoryID, uri.toString());
  }

  /*
   * (non-Javadoc)
   * 
   * @see gate.creole.ontology.RDFProperty#setInverseFunctional(boolean)
   */
  public void setInverseFunctional(boolean inverseFunctional) {
    owlim.setInverseFunctional(this.repositoryID, uri.toString(),
            inverseFunctional);
  }

  /*
   * (non-Javadoc)
   * 
   * @see
   * gate.creole.ontology.RDFProperty#isValidRange(gate.creole.ontology
   * .OResource)
   */
  public boolean isValidRange(OResource aResource) {
    ResourceInfo[] listOfOResources = owlim.getRange(this.repositoryID, uri
            .toString());
    if(listOfOResources.length == 0) return true;
    // lets first make a easy move
    List<String> list = new ArrayList<String>();
    for(int i = 0; i < listOfOResources.length; i++) {
      list.add(listOfOResources[i].getUri());
    }
    if(list.contains(aResource.getURI().toString())) {
      return true;
    }
    if(aResource instanceof OInstance) {
      // lets find out all its super classes
      ResourceInfo[] oClasses = owlim.getClassesOfIndividual(this.repositoryID,
              aResource.getURI().toString(), OConstants.TRANSITIVE_CLOSURE);
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
      ResourceInfo[] oClasses = owlim.getSuperClasses(this.repositoryID,
              aResource.getURI().toString(), OConstants.TRANSITIVE_CLOSURE);
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
      Property[] oProps = owlim.getSuperProperties(this.repositoryID, aResource
              .getURI().toString(), OConstants.TRANSITIVE_CLOSURE);
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
   * @see
   * gate.creole.ontology.RDFProperty#isValidDomain(gate.creole.ontology
   * .OResource)
   */
  public boolean isValidDomain(OResource aResource) {
    ResourceInfo[] listOfOResources = owlim.getDomain(this.repositoryID, uri
            .toString());
    if(listOfOResources.length == 0) return true;

    Set<String> list = new HashSet<String>();
    for(int i = 0; i < listOfOResources.length; i++) {
      list.add(listOfOResources[i].getUri().toString());
      OResource resource = ontology.getOResourceFromMap(listOfOResources[i]
              .getUri());
      if(resource != null && resource instanceof OClass) {
        Set<OClass> classes = ((OClass)resource)
                .getSubClasses(OConstants.TRANSITIVE_CLOSURE);
        Iterator<OClass> iter = classes.iterator();
        while(iter.hasNext()) {
          list.add(iter.next().getURI().toString());
        }
      }
      else if(resource != null && resource instanceof RDFProperty
              && !(resource instanceof AnnotationProperty)) {
        Set<RDFProperty> props = ((RDFProperty)resource)
                .getSubProperties(OConstants.TRANSITIVE_CLOSURE);
        Iterator<RDFProperty> iter = props.iterator();
        while(iter.hasNext()) {
          list.add(iter.next().getURI().toString());
        }
      }
    }

    if(list.contains(aResource.getURI().toString())) {
      return true;
    }
    if(aResource instanceof OInstance) {
      // lets find out all its super classes
      ResourceInfo[] oClasses = owlim.getClassesOfIndividual(this.repositoryID,
              aResource.getURI().toString(), OConstants.DIRECT_CLOSURE);
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
      if(list.contains(aResource.getURI().toString())) {
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
    ResourceInfo[] list = owlim.getDomain(this.repositoryID, uri.toString());
    Set<OResource> domain = new HashSet<OResource>();
    List<String> individuals = Arrays.asList(owlim
            .getIndividuals(this.repositoryID));
    // these resources can be anything - an instance, a property, or a
    // class
    for(int i = 0; i < list.length; i++) {
      // lets first search if it is available in ontology cache
      OResource resource = ontology.getOResourceFromMap(list[i].getUri());
      if(resource != null) {
        domain.add(resource);
        continue;
      }
      if(individuals.contains(list[i])) {
        domain.add(Utils.createOInstance(this.repositoryID, this.ontology,
                this.owlim, list[i].getUri()));
        continue;
      }
      // otherwise we need to create it
      if(owlim.hasClass(this.repositoryID, list[i].getUri())) {
        // lets first check if this is a valid URI
        domain.add(Utils.createOClass(this.repositoryID, this.ontology,
                this.owlim, list[i].getUri(), list[i].getClassType()));
        continue;
      }
      Property prop = owlim.getPropertyFromOntology(this.repositoryID, list[i]
              .getUri());
      domain.add(Utils.createOProperty(this.repositoryID, this.ontology,
              this.owlim, prop.getUri(), prop.getType()));
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
    Set<OResource> domain = new HashSet<OResource>();
    List<String> individuals = Arrays.asList(owlim
            .getIndividuals(this.repositoryID));
    // these resources can be anything - an instance, a property, or a
    // class
    for(int i = 0; i < list.length; i++) {
      // lets first search if it is available in ontology cache
      OResource resource = ontology.getOResourceFromMap(list[i].getUri());
      if(resource != null) {
        domain.add(resource);
        continue;
      }
      if(individuals.contains(list[i])) {
        domain.add(Utils.createOInstance(this.repositoryID, this.ontology,
                this.owlim, list[i].getUri()));
        continue;
      }
      // otherwise we need to create it
      if(owlim.hasClass(this.repositoryID, list[i].getUri())) {
        domain.add(Utils.createOClass(this.repositoryID, this.ontology,
                this.owlim, list[i].getUri(), list[i].getClassType()));
        continue;
      }
      Property prop = owlim.getPropertyFromOntology(this.repositoryID, list[i]
              .getUri());
      domain.add(Utils.createOProperty(this.repositoryID, this.ontology,
              this.owlim, prop.getUri(), prop.getType()));
    }
    return domain;
  }

  public OURI getOURI() {
    throw new UnsupportedOperationException(
            "Not supported in this implementation");
  }

}
