/**
 * 
 */
package gate.creole.ontology.owlim;

import gate.creole.ontology.DataType;
import gate.creole.ontology.InvalidValueException;
import gate.creole.ontology.MinCardinalityRestriction;
import gate.creole.ontology.OConstants;
import gate.creole.ontology.Ontology;
import gate.creole.ontology.RDFProperty;
import gate.creole.ontology.URI;

/**
 * @author niraj
 * 
 */
public class MinCardinalityRestrictionImpl
    extends OClassImpl
    implements MinCardinalityRestriction
{

  /**
   * @param aURI
   * @param ontology
   * @param repositoryID
   * @param owlimPort
   */
  public MinCardinalityRestrictionImpl(URI aURI, Ontology ontology,
          String repositoryID, OWLIM owlimPort) {
    super(aURI, ontology, repositoryID, owlimPort);
  }

  /*
   * (non-Javadoc)
   * 
   * @see gate.creole.ontology.MinCardinalityRestriction#getValue()
   */
  public String getValue() {
    PropertyValue pv = owlim.getPropertyValue(repositoryID, this.getURI()
            .toString(), OConstants.MIN_CARDINALITY_RESTRICTION);
    return pv.getValue();
  }

  public void setValue(String value, DataType datatype)
          throws InvalidValueException {
    if(!datatype.isValidValue(value))
      throw new InvalidValueException(value + " is not valid for datatype "
              + datatype.getXmlSchemaURIString());

    owlim.setPropertyValue(repositoryID, this.getURI().toString(),
            OConstants.MIN_CARDINALITY_RESTRICTION, value, datatype
                    .getXmlSchemaURIString());
  }

  /*
   * (non-Javadoc)
   * 
   * @see gate.creole.ontology.CardinalityRestriction#getDataType()
   */
  public DataType getDataType() {
    PropertyValue pv = owlim.getPropertyValue(repositoryID, this.getURI()
            .toString(), OConstants.MIN_CARDINALITY_RESTRICTION);
    return OntologyUtilities.getDataType(pv.getDatatype());
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
