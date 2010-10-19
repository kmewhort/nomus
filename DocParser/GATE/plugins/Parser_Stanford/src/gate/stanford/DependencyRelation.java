package gate.stanford;

import java.io.Serializable;

/**
 * Simple class representing a single dependency relation.  The "target"
 * is the Annotation ID of the dependent; the "type" is the dependency 
 * tag (<a href="http://nlp.stanford.edu/software/parser-faq.shtml#c">the
 * Stanford Parser documentation</a> contains links to the tagset</a>; for example,
 * nsubj = "nominal subject", dobj = "direct object).
 */
public class DependencyRelation implements Serializable {
  /**
   * Serial version UID.
   */
  private static final long serialVersionUID = -7842607116149222052L;

  /**
   * The type of the dependency relation (det, amod, etc.).
   */
  private String type;
  
  /**
   * The ID of the token that is the target of this relation.
   */
  private Integer targetId;
  
  public DependencyRelation(String type, Integer targetId) {
    this.type = type;
    this.targetId = targetId;
  }

  /**
   * Return the dependency tag (type).
   * @return the dependency tag
   */
  public String getType() {
    return type;
  }

  /**
   * Set the dependency tag.
   * @param type dependency tag
   */
  public void setType(String type) {
    this.type = type;
  }

  /**
   * Return the GATE Annotation ID of the dependent.
   * @return the Annotation ID
   */
  public Integer getTargetId() {
    return targetId;
  }

  /**
   * Set the Annotation ID of the dependent.
   * @param targetId the Annotation ID
   */
  public void setTargetId(Integer targetId) {
    this.targetId = targetId;
  }
  
  /**
   * Format the data structure for display.
   * For example, if type is "dobj" and the dependent has Annotation ID 37,
   * return the String "dobj(37)". 
   */
  public String toString() {
    return type + "(" + targetId + ")";
  }
}
