package gate.creole.ontology.owlim;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlType;

/**
 * Special response wrapper for PropertyValue[] responses, to treat an empty
 * response as an empty array rather than null.
 */
@XmlAccessorType(XmlAccessType.PROPERTY)
@XmlType(name = "propertyValueArrayResponse")
public class PropertyValueArrayResponse {
  private PropertyValue[] _return;

  public void setReturn(PropertyValue[] _return) {
    this._return = _return;
  }

  public PropertyValue[] getReturn() {
    if(_return == null) {
      return new PropertyValue[0];
    }
    else {
      return _return;
    }
  }
}
