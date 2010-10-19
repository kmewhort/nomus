/**
 * 
 */
package gate.creole.ontology.owlim;

import gate.creole.ontology.OConstants;
import gate.creole.ontology.OResource;
import gate.creole.ontology.Ontology;
import gate.creole.ontology.RDFProperty;
import gate.creole.ontology.SomeValuesFromRestriction;
import gate.creole.ontology.URI;

/**
 * @author niraj
 * 
 */
public class SomeValuesFromRestrictionImpl extends OClassImpl implements
                                                             SomeValuesFromRestriction {

  /**
   * @param aURI
   * @param ontology
   * @param repositoryID
   * @param owlimPort
   */
  public SomeValuesFromRestrictionImpl(URI aURI, Ontology ontology,
          String repositoryID, OWLIM owlimPort) {
    super(aURI, ontology, repositoryID, owlimPort);
  }

  /*
   * (non-Javadoc)
   * 
   * @see gate.creole.ontology.SomeValuesFromRestriction#getHasValue()
   */
  public OResource getHasValue() {
      ResourceInfo resource = owlim.getRestrictionValue(this.repositoryID,
              this.uri.toString(), OConstants.SOME_VALUES_FROM_RESTRICTION);

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
            OConstants.SOME_VALUES_FROM_RESTRICTION, resource.getURI()
                    .toString());
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
      ontology.fireResourceRelationChanged(this, property, OConstants.RESTRICTION_ON_PROPERTY_VALUE_CHANGED);
  }

}
