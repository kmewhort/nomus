/*
 *  OResourceImpl.java
 *
 *  Niraj Aswani, 09/March/07
 *
 *  $Id: OResourceImpl.java 12593 2010-05-05 17:05:06Z ian_roberts $
 */
package gate.creole.ontology.owlim;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Locale;
import java.util.Set;
import org.openrdf.vocabulary.RDFS;

import gate.creole.ontology.AnnotationProperty;
import gate.creole.ontology.GateOntologyException;
import gate.creole.ontology.Literal;
import gate.creole.ontology.OConstants;
import gate.creole.ontology.ONodeID;
import gate.creole.ontology.OResource;
import gate.creole.ontology.Ontology;
import gate.creole.ontology.RDFProperty;
import gate.creole.ontology.URI;

/**
 * Constructor
 * 
 * @author niraj
 * 
 */
public class OResourceImpl implements OResource {

  /**
   * ID of the repository
   */
  protected String repositoryID;

  /**
   * instance of the OWLIMServices
   */
  protected OWLIM owlim;

  /**
   * URI of the resource
   */
  protected URI uri;

  /**
   * The ontology the current resource belongs to
   */
  protected Ontology ontology;

  /**
   * Constructor
   * 
   * @param aURI
   * @param repositoryID
   * @param owlimPort
   */
  public OResourceImpl(URI aURI, Ontology ontology, String repositoryID,
          OWLIM owlimPort) {
    this.uri = aURI;
    this.repositoryID = repositoryID;
    this.owlim = owlimPort;
    this.ontology = ontology;
  }

  /*
   * (non-Javadoc)
   * 
   * @see gate.creole.ontology.OResource#getURI()
   */
  public URI getURI() {
    return this.uri;
  }

  /*
   * (non-Javadoc)
   * 
   * @see gate.creole.ontology.OResource#getURI()
   */
  public void setURI(URI uri) {
    throw new GateOntologyException(
            "This operation is not allowed in this version!");
  }

  /**
   * This method returns a set of labels specified on this resource.
   * 
   * @return
   */
  public Set<Literal> getLabels() {

    PropertyValue[] pvalues = owlim.getAnnotationPropertyValues(
            this.repositoryID, this.uri.toString(), RDFS.LABEL);

    Set<Literal> toReturn = new HashSet<Literal>();
    for(PropertyValue pv : pvalues) {
      toReturn.add(new Literal(pv.getValue(), OntologyUtilities.getLocale(pv
              .getDatatype())));
    }

    return toReturn;
  }

  /**
   * This method returns a set of comments specified on this resource.
   * 
   * @return
   */
  public Set<Literal> getComments() {
    PropertyValue[] pvalues = owlim.getAnnotationPropertyValues(
            this.repositoryID, this.uri.toString(), RDFS.COMMENT);

    Set<Literal> toReturn = new HashSet<Literal>();
    for(PropertyValue pv : pvalues) {
      toReturn.add(new Literal(pv.getValue(), OntologyUtilities.getLocale(pv
              .getDatatype())));
    }

    return toReturn;
  }

  /*
   * (non-Javadoc)
   * 
   * @see gate.creole.ontology.OResource#getComment(java.lang.String)
   */
  public String getComment(Locale language) {
    return owlim.getAnnotationPropertyValue(this.repositoryID, this.uri
            .toString(), RDFS.COMMENT, language != null ? language
            .getLanguage() : null);
  }

  /*
   * (non-Javadoc)
   * 
   * @see gate.creole.ontology.OResource#setComment(java.lang.String,
   *      java.lang.String)
   */
  public void setComment(String aComment, Locale language) {
    owlim.addAnnotationPropertyValue(this.repositoryID, this.uri.toString(),
            RDFS.COMMENT, aComment, language != null
                    ? language.getLanguage()
                    : null);
    Literal l = new Literal(aComment, language);
    ontology.fireResourcePropertyValueChanged(this, (RDFProperty) ontology.getOResourceFromMap(RDFS.COMMENT), (Object)l, OConstants.ANNOTATION_PROPERTY_VALUE_ADDED_EVENT);
  }

  /*
   * (non-Javadoc)
   * 
   * @see gate.creole.ontology.OResource#getLabel(java.lang.String)
   */
  public String getLabel(Locale language) {
    return owlim.getAnnotationPropertyValue(this.repositoryID, this.uri
            .toString(), RDFS.LABEL, language != null
            ? language.getLanguage()
            : null);
  }

  /*
   * (non-Javadoc)
   * 
   * @see gate.creole.ontology.OResource#setLabel(java.lang.String,
   *      java.lang.String)
   */
  public void setLabel(String aLabel, Locale language) {
    owlim.addAnnotationPropertyValue(this.repositoryID, this.uri.toString(),
            RDFS.LABEL, aLabel, language != null
                    ? language.getLanguage()
                    : null);
    Literal l = new Literal(aLabel, language);
    ontology.fireResourcePropertyValueChanged(this, (RDFProperty) ontology.getOResourceFromMap(RDFS.LABEL), (Object)l, OConstants.ANNOTATION_PROPERTY_VALUE_ADDED_EVENT);
  }

  /*
   * (non-Javadoc)
   * 
   * @see gate.creole.ontology.OResource#getName()
   */
  public String getName() {
    return this.uri.getResourceName();
  }

  /*
   * (non-Javadoc)
   * 
   * @see gate.creole.ontology.OResource#getOntology()
   */
  public Ontology getOntology() {
    return this.ontology;
  }

  /*
   * (non-Javadoc)
   * 
   * @see gate.creole.ontology.OResource#addAnnotationPropertyValue(gate.creole.ontology.AnnotationProperty,
   *      gate.creole.ontology.Literal)
   */
  public void addAnnotationPropertyValue(
          AnnotationProperty theAnnotationProperty, Literal literal) {
    OResource res = ontology.getOResourceFromMap(theAnnotationProperty.getURI()
            .toString());
    if(res == null) {
      Utils
              .error(theAnnotationProperty.getURI().toString()
                      + " does not exist");
      return;
    }

    if(!(res instanceof AnnotationProperty)) {
      Utils.error(theAnnotationProperty.getURI().toString()
              + " is not a registered annotation property");
      return;
    }

    owlim.addAnnotationPropertyValue(this.repositoryID, this.uri.toString(),
            theAnnotationProperty.getURI().toString(), literal.getValue(),
            literal.getLanguage() != null
                    ? literal.getLanguage().getLanguage()
                    : null);
    ontology.fireResourcePropertyValueChanged(this, theAnnotationProperty, literal, OConstants.ANNOTATION_PROPERTY_VALUE_ADDED_EVENT);
  }

  /*
   * (non-Javadoc)
   * 
   * @see gate.creole.ontology.OResource#getAnnotationPropertyValues(gate.creole.ontology.AnnotationProperty)
   */
  public List<Literal> getAnnotationPropertyValues(
          AnnotationProperty theAnnotationProperty) {
    PropertyValue[] propValues = owlim.getAnnotationPropertyValues(
            this.repositoryID, this.uri.toString(), theAnnotationProperty
                    .getURI().toString());
    List<Literal> list = new ArrayList<Literal>();
    for(int i = 0; i < propValues.length; i++) {
      Literal l = new Literal(propValues[i].getValue(), OntologyUtilities
              .getLocale(propValues[i].getDatatype()));
      list.add(l);
    }
    return list;
  }

  /*
   * (non-Javadoc)
   * 
   * @see gate.creole.ontology.OResource#removeAnnotationPropertyValue(gate.creole.ontology.AnnotationProperty,
   *      gate.creole.ontology.Literal)
   */
  public void removeAnnotationPropertyValue(
          AnnotationProperty theAnnotationProperty, Literal literal) {
    owlim.removeAnnotationPropertyValue(this.repositoryID, this.uri.toString(),
            theAnnotationProperty.getURI().toString(), literal.getValue(),
            literal.getLanguage() != null
                    ? literal.getLanguage().getLanguage()
                    : null);
    ontology.fireResourcePropertyValueChanged(this, theAnnotationProperty, literal, OConstants.ANNOTATION_PROPERTY_VALUE_REMOVED_EVENT);
  }

  /*
   * (non-Javadoc)
   * 
   * @see gate.creole.ontology.OResource#removeAnnotationPropertyValues(gate.creole.ontology.AnnotationProperty)
   */
  public void removeAnnotationPropertyValues(
          AnnotationProperty theAnnotationProperty) {
    owlim.removeAnnotationPropertyValues(this.repositoryID,
            this.uri.toString(), theAnnotationProperty.getURI().toString());
    ontology.fireResourcePropertyValueChanged(this, theAnnotationProperty, null, OConstants.ANNOTATION_PROPERTY_VALUE_REMOVED_EVENT);
  }

  /**
   * This method returns the annotation properties set on this resource.
   * 
   * @return
   */
  public Set<AnnotationProperty> getSetAnnotationProperties() {
    Property[] properties = owlim.getAnnotationProperties(this.repositoryID,
            this.uri.toString());
    Set<AnnotationProperty> annotProps = new HashSet<AnnotationProperty>();
    for(int i = 0; i < properties.length; i++) {
      if(properties[i].getType() != OConstants.ANNOTATION_PROPERTY) {
        throw new GateOntologyException("The property :"
                + properties[i].getUri()
                + " returned from the repository is not an AnnotationProperty");
      }
      String propUri = properties[i].getUri();
      OResource resource = ontology.getOResourceFromMap(propUri);
      if(resource == null) {
        resource = new AnnotationPropertyImpl(new URI(propUri, false),
                this.ontology, this.repositoryID, owlim);
        ontology.addOResourceToMap(propUri, resource);
      }
      annotProps.add((AnnotationProperty)resource);
    }
    return annotProps;
  }

  /**
   * Checks if the resource has the provided annotation property set on
   * it with the specified value.
   * 
   * @param aProperty
   * @param aValue
   * @return
   */
  public boolean hasAnnotationPropertyWithValue(AnnotationProperty aProperty,
          Literal aValue) {
    List<Literal> literals = getAnnotationPropertyValues(aProperty);
    for(Literal l : literals) {
      if(l.getValue().equals(aValue.getValue())) {
        if(l.getDataType() != null && aValue.getDataType() != null) {
          if(!aValue.getDataType().getXmlSchemaURIString().equals(
                  l.getDataType().getXmlSchemaURIString())) continue;
        }
        
        if(l.getLanguage() != null && aValue.getLanguage() != null) {
          if(!aValue.getLanguage().toString().equals(l.getLanguage().toString())) continue;          
        }
        return true;
      }
    }
    return false;
  }

  /**
   * This method returns all the set properties set on this resource.
   * 
   * @return
   */
  public Set<RDFProperty> getAllSetProperties() {
    Set<RDFProperty> toReturn = new HashSet<RDFProperty>();
    toReturn.addAll(getSetAnnotationProperties());
    return toReturn;
  }

  /**
   * This method returns a set of all properties where the current
   * resource has been specified as one of the domain resources. Please
   * note that this method is different from the getAllSetProperties()
   * method which returns a set of properties set on the resource. For
   * each property in the ontology, this method checks if the current
   * resource is valid domain. If so, the property is said to be
   * applicable, and otherwise not..
   * 
   * @return
   */
  public Set<RDFProperty> getPropertiesWithResourceAsDomain() {
    Set<RDFProperty> toReturn = new HashSet<RDFProperty>();
    Property[] properties = this.owlim.getPropertiesWithResourceAsDomain(
            this.repositoryID, this.getURI().toString());
    for(int i = 0; i < properties.length; i++) {
      toReturn.add((RDFProperty)Utils.createOProperty(this.repositoryID,
              this.ontology, owlim, properties[i].getUri(), properties[i]
                      .getType()));
    }
    return toReturn;
  }

  /**
   * This method returns a set of all properties where the current
   * resource has been specified as one of the range resources. Please
   * note that this method is different from the getAllSetProperties()
   * method which returns a set of properties set on the resource. For
   * each property in the ontology, this method checks if the current
   * resource is valid range. If so, the property is said to be
   * applicable, and otherwise not.
   * 
   * @return
   */
  public Set<RDFProperty> getPropertiesWithResourceAsRange() {
    Set<RDFProperty> toReturn = new HashSet<RDFProperty>();
    Property[] properties = this.owlim.getPropertiesWithResourceAsRange(
            this.repositoryID, this.getURI().toString());
    for(int i = 0; i < properties.length; i++) {
      toReturn.add((RDFProperty)Utils.createOProperty(this.repositoryID,
              this.ontology, owlim, properties[i].getUri(), properties[i]
                      .getType()));
    }
    return toReturn;
  }

  /**
   * String representation of the resource: its name and not the URI.
   */
  public String toString() {
    return this.getName();
  }

  /**
   * HashCode for this resource.
   */
  public int hashCode() {
    return this.getURI().toString().hashCode();
  }

  /**
   * equals method overriden tocompare URIs
   */
  public boolean equals(Object a) {
    if(a instanceof OResource) {
      return ((OResource)a).getURI().toString()
              .equals(this.getURI().toString());
    }
    return false;
  }

  public ONodeID getONodeID() {
    return getURI();
  }

}
