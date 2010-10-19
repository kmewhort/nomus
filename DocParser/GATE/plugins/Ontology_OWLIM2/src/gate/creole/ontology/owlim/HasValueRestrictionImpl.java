/**
 * 
 */
package gate.creole.ontology.owlim; 

import gate.creole.ontology.HasValueRestriction;
import gate.creole.ontology.InvalidValueException;
import gate.creole.ontology.Literal;
import gate.creole.ontology.OConstants;
import gate.creole.ontology.OResource;
import gate.creole.ontology.Ontology;
import gate.creole.ontology.RDFProperty;
import gate.creole.ontology.URI;
import gate.util.GateRuntimeException;

/**
 * @author niraj
 *
 */
public class HasValueRestrictionImpl extends OClassImpl implements
                                                       HasValueRestriction {

  /**
   * @param aURI
   * @param ontology
   * @param repositoryID
   * @param owlimPort
   */
  public HasValueRestrictionImpl(URI aURI, Ontology ontology,
          String repositoryID, OWLIM owlimPort) {
    super(aURI, ontology, repositoryID, owlimPort);
  }

  /*
   * (non-Javadoc)
   * 
   * @see gate.creole.ontology.HasValuesFromRestriction#getHasValue()
   */
  public Object getHasValue() {
      ResourceInfo resource = owlim.getRestrictionValue(this.repositoryID,
              this.uri.toString(), OConstants.HAS_VALUE_RESTRICTION);
      RDFProperty prop = getOnPropertyValue();
      if(prop instanceof DatatypePropertyImpl) {
        try {
          return new Literal(resource.getUri(),((DatatypePropertyImpl)prop).getDataType());
        } catch(InvalidValueException ive) {
          throw new GateRuntimeException(ive);
        }
      }
      
      if(resource.getClassType() == OConstants.INSTANCE)
        return Utils.createOInstance(this.repositoryID, this.ontology,
                this.owlim, resource.getUri());
      
      return Utils.createOClass(this.repositoryID, this.ontology, this.owlim,
              resource.getUri(), resource.getClassType());
  }
  
  /**
   * Sets the resource as a restricted value.
   * 
   * @param resource
   */
  public void setHasValue(OResource resource) {
    owlim.setRestrictionValue(this.repositoryID, this.uri.toString(),
            OConstants.HAS_VALUE_RESTRICTION, resource.getURI()
                    .toString());
  }
  

  /**
   * Sets the resource as a restricted value.
   * 
   * @param resource
   */
  public void setHasValue(Literal literal) {
    owlim.setRestrictionValue(this.repositoryID, this.uri.toString(),
            OConstants.HAS_VALUE_RESTRICTION, literal.getValue());
  }
  
  /*
   * (non-Javadoc)
   * 
   * @see gate.creole.ontology.Restriction#getOnPropertyValue()
   */
  public RDFProperty getOnPropertyValue() {
      Property property = owlim.getOnPropertyValue(this.repositoryID, this.uri
              .toString());
      return Utils.createOProperty(repositoryID, ontology, owlim, property
              .getUri(), property.getType());
  }

  /*
   * (non-Javadoc)
   * 
   * @see gate.creole.ontology.Restriction#setOnPropertyValue(gate.creole.ontology.RDFProperty)
   */
  public void setOnPropertyValue(RDFProperty property) {
      owlim.setOnPropertyValue(this.repositoryID, this.uri.toString(), property
              .getURI().toString());
      ontology.fireResourceRelationChanged(this, property,OConstants.RESTRICTION_ON_PROPERTY_VALUE_CHANGED);
  }
}
