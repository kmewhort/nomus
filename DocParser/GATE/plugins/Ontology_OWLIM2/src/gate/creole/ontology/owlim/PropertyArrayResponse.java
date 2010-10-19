package gate.creole.ontology.owlim;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlType;

/**
 * Special response wrapper for Property[] responses, to treat an empty
 * response as an empty array rather than null.
 */
@XmlAccessorType(XmlAccessType.PROPERTY)
@XmlType(name = "propertyArrayResponse")
public class PropertyArrayResponse {
  private Property[] _return;

  public void setReturn(Property[] _return) {
    this._return = _return;
  }

  public Property[] getReturn() {
    if(_return == null) {
      return new Property[0];
    }
    else {
      return _return;
    }
  }
}
