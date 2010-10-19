package gate.creole.ontology.owlim;

import gate.util.GateRuntimeException;

/**
 * Ontology Event describes a change in the ontology.
 * 
 * @author niraj
 */
public class OEvent {

  /**
   * Contruction
   * 
   * @param subject - affected subject. Can be * if referring to all
   *          values.
   * @param predicate - affected predicate. Can be * if referring to all
   *          values.
   * @param object - affected object. Can be * if referring to all
   *          value.
   * @param toAdd - indicates if this statement is added to or removed
   *          from the ontology.
   */
  public OEvent(String subject, String predicate, String object, boolean toAdd) {
    this.subject = subject == null ? "*" : subject;
    this.predicate = predicate == null ? "*" : predicate;
    this.object = object == null ? "*" : object;
    this.toAdd = toAdd;
  }

  /**
   * Contruction
   * 
   * @param subject - affected subject. Can be * if referring to all
   *          values.
   * @param predicate - affected predicate. Can be * if referring to all
   *          values.
   * @param object - affected object. Can be * if referring to all
   *          value.
   * @param datatype - if the object value refers to a literal, one
   *          needs to specify the datatype.
   * @param toAdd - indicates if this statement is added to or removed
   *          from the ontology.
   */
  public OEvent(String subject, String predicate, String object,
          String datatype, boolean toAdd) {
    this(subject, predicate, object, toAdd);
    this.datatype = datatype == null ? "*" : datatype;
  }

  /**
   * Given a toString() representation of an an event, this method
   * converts it into the OEvent object.
   * 
   * @param eventDesc
   * @return an instance of OEvent class.
   */
  public static OEvent parseEvent(String eventDesc) {
    // the first character is either - or +
    char c = eventDesc.charAt(0);
    boolean add = c == '-' ? false : true;
    String neventDesc = eventDesc.substring(3, eventDesc.length() - 1);
    // each string is delimited with "> <"
    String parts[] = neventDesc.split("> <");

    if(parts.length == 3) {
      return new OEvent(removeEscapeChar(parts[0]), removeEscapeChar(parts[1]),
              removeEscapeChar(parts[2]), add);
    }
    else if(parts.length == 4) {
      return new OEvent(removeEscapeChar(parts[0]), removeEscapeChar(parts[1]),
              removeEscapeChar(parts[2]), removeEscapeChar(parts[3]), add);
    }
    else {
      throw new GateRuntimeException("Invalid event description " + eventDesc);
    }
  }

  /**
   * This method removes the \ before the escaped characters.
   * 
   * @param string
   * @return
   */
  private static String removeEscapeChar(String string) {
    String toReturn = "";
    if(string.equals("*")) return string;
    string = string.substring(0, string.length());
    for(int i = 0; i < string.length(); i++) {
      char c = string.charAt(i);
      if(c == '\\') {
        // check if the next character is ", < or >
        if(i + 1 < string.length()) {
          char ch1 = string.charAt(i + 1);
          if(ch1 == '"' || ch1 == '<' || ch1 == '>') {
            toReturn += ch1;
            i++;
            continue;
          }
        }
      }
      toReturn += c;
    }
    return toReturn;
  }

  /**
   * Gives a string representation for the OEvent instances. This is
   * what is stored in the changelog for every instance of OEvent.
   */
  public String toString() {
    // lets replace any " with \", new line with space
    String subject1 = "<" + getEscapedString(subject) + ">";
    String predicate1 = "<" + getEscapedString(predicate) + ">";
    String object1 = "<" + getEscapedString(object) + ">";
    String datatype1 = datatype == null ? null : "<"
            + getEscapedString(datatype) + ">";

    return (toAdd ? "+" : "-") + " " + subject1 + " " + predicate1 + " "
            + object1 + (datatype1 == null ? "" : " " + datatype1);
  }

  /**
   * Escapes the characters which are part of the changelog syntax.
   * 
   * @param string
   * @return
   */
  private String getEscapedString(String string) {
    String toReturn = "";
    for(char c : string.toCharArray()) {
      if(c == '"') {
        toReturn += "\\\"";
      }
      else if(c == '<') {
        toReturn += "\\<";
      }
      else if(c == '>') {
        toReturn += "\\>";
      }
      else if(c == '\n') {
        toReturn += " ";
      }
      else {
        toReturn += c + "";
      }
    }
    return toReturn;
  }

  /**
   * Subject in the triple that this event refers to.
   */
  private String subject;

  /**
   * Predicate in the triple that this event refers to.
   */
  private String predicate;

  /**
   * Object in the triple that this event refers to.
   */
  private String object;

  /**
   * Datatype in the triple that this event refers to.
   */
  private String datatype;

  /**
   * Indicates if this triple was added to or deleted from the ontology.
   */
  private boolean toAdd;

  /**
   * Returns the value of subject in the triple this event object refers
   * to.
   * 
   * @return
   */
  public String getSubject() {
    return subject;
  }

  /**
   * Sets the value for subject in the triple this event object refers
   * to.
   * 
   * @param subject
   */
  public void setSubject(String subject) {
    this.subject = subject;
  }

  /**
   * Returns the value of predicate in the triple this event object
   * refers to.
   * 
   * @return
   */
  public String getPredicate() {
    return predicate;
  }

  /**
   * Sets the value for predicate in the triple this event object refers
   * to.
   * 
   * @param predicate
   */
  public void setPredicate(String predicate) {
    this.predicate = predicate;
  }

  /**
   * Returns the value of object in the triple this event object refers
   * to.
   * 
   * @return
   */
  public String getObject() {
    return object;
  }

  /**
   * Sets the value for object in the triple this event object refers
   * to.
   * 
   * @param object
   */
  public void setObject(String object) {
    this.object = object;
  }

  /**
   * Returns the value of datatype in the triple this event object
   * refers to.
   * 
   * @return
   */
  public String getDatatype() {
    return datatype;
  }

  /**
   * Sets the value for datatype in the triple this event object refers
   * to.
   * 
   * @param datatype
   */
  public void setDatatype(String datatype) {
    this.datatype = datatype;
  }

  /**
   * Returns true if this triple is to be added or has been added to the
   * ontology, false otherwise.
   * 
   * @return
   */
  public boolean getToAdd() {
    return toAdd;
  }

  /**
   * If sets to true, indicates that this triple should be added/has
   * been added to the ontology. It should be set to false otherwise.
   * 
   * @param toAdd
   */
  public void setToAdd(boolean toAdd) {
    this.toAdd = toAdd;
  }

}
