package gate.creole.ontology.owlim;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlType;

/**
 * Special response wrapper for String[] responses, to treat an empty
 * response as an empty array rather than null.
 */
@XmlAccessorType(XmlAccessType.PROPERTY)
@XmlType(name = "stringArrayResponse")
public class StringArrayResponse {
  private String[] _return;

  public void setReturn(String[] _return) {
    this._return = _return;
  }

  public String[] getReturn() {
    if(_return == null) {
      return new String[0];
    }
    else {
      return _return;
    }
  }
}
