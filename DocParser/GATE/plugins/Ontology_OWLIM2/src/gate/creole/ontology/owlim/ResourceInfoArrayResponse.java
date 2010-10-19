package gate.creole.ontology.owlim;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlType;

/**
 * Special response wrapper for ResourceInfo[] responses, to treat an empty
 * response as an empty array rather than null.
 */
@XmlAccessorType(XmlAccessType.PROPERTY)
@XmlType(name = "resourceInfoArrayResponse")
public class ResourceInfoArrayResponse {
  private ResourceInfo[] _return;

  public void setReturn(ResourceInfo[] _return) {
    this._return = _return;
  }

  public ResourceInfo[] getReturn() {
    if(_return == null) {
      return new ResourceInfo[0];
    }
    else {
      return _return;
    }
  }
}
