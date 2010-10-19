/**
 * 
 */
package gate.creole.ontology.owlim;

import org.openrdf.vocabulary.OWL;

import gate.creole.ontology.*;
import gate.util.GateRuntimeException;

/**
 * @author niraj
 * 
 */
public class Utils {

  /**
   * Given required parameters, this method, based on the provided type,
   * returns an appropriate object of a property.
   * 
   * @param repositoryID
   * @param ontology
   * @param owlim
   * @param uri
   * @param type
   * @return
   */
  public static RDFProperty createOProperty(String repositoryID,
          Ontology ontology, OWLIM owlim, String uri, byte type) {
    RDFProperty prop = (RDFProperty)ontology.getOResourceFromMap(uri);
    if(prop != null) return prop;
    switch(type) {
      case OConstants.ANNOTATION_PROPERTY:
        prop = new AnnotationPropertyImpl(new URI(uri, false), ontology,
                repositoryID, owlim);
        break;
      case OConstants.RDF_PROPERTY:
        prop = new RDFPropertyImpl(new URI(uri, false), ontology, repositoryID,
                owlim);
        break;
      case OConstants.OBJECT_PROPERTY:
        prop = new ObjectPropertyImpl(new URI(uri, false), ontology,
                repositoryID, owlim);
        break;
      case OConstants.SYMMETRIC_PROPERTY:
        prop = new SymmetricPropertyImpl(new URI(uri, false), ontology,
                repositoryID, owlim);
        break;
      case OConstants.TRANSITIVE_PROPERTY:
        prop = new TransitivePropertyImpl(new URI(uri, false), ontology,
                repositoryID, owlim);
        break;
      case OConstants.DATATYPE_PROPERTY:
        prop = new DatatypePropertyImpl(new URI(uri, false), ontology,
                repositoryID, owlim);
        break;
    }
    ontology.addOResourceToMap(uri, prop);
    return prop;
  }

  /**
   * Creates a new instance of Ontology Class
   * 
   * @param repositoryID
   * @param ontology
   * @param owlim
   * @param uri
   * @param isAnonymousClass
   * @return
   */
  public static OClass createOClass(String repositoryID, Ontology ontology,
          OWLIM owlim, String uri, byte classType) {
    OClass aClass = (OClass)ontology.getOResourceFromMap(uri);
    if(aClass != null) return aClass;
    switch(classType) {
      case OConstants.HAS_VALUE_RESTRICTION:
        aClass = new HasValueRestrictionImpl(new URI(uri, true), ontology,
                repositoryID, owlim);
        break;
      case OConstants.ALL_VALUES_FROM_RESTRICTION:
        aClass = new AllValuesFromRestrictionImpl(new URI(uri, true), ontology,
                repositoryID, owlim);
        break;
      case OConstants.SOME_VALUES_FROM_RESTRICTION:
        aClass = new SomeValuesFromRestrictionImpl(new URI(uri, true),
                ontology, repositoryID, owlim);
        break;
      case OConstants.CARDINALITY_RESTRICTION:
        aClass = new CardinalityRestrictionImpl(new URI(uri, true), ontology,
                repositoryID, owlim);
        break;
      case OConstants.MIN_CARDINALITY_RESTRICTION:
        aClass = new MinCardinalityRestrictionImpl(new URI(uri, true),
                ontology, repositoryID, owlim);
        break;
      case OConstants.MAX_CARDINALITY_RESTRICTION:
        aClass = new MaxCardinalityRestrictionImpl(new URI(uri, true),
                ontology, repositoryID, owlim);
        break;
      case OConstants.ANNONYMOUS_CLASS:
        aClass = new AnonymousClassImpl(new URI(uri, true),
            ontology, repositoryID, owlim);
        break;
      default:
        aClass = new OClassImpl(new URI(uri, false), ontology, repositoryID,
                owlim);
        break;
    }

    ontology.addOResourceToMap(uri, aClass);
    return aClass;
  }

  public static String getRestrictionName(byte classType) {
    String className = "Unknown";
    switch(classType) {
      case OConstants.HAS_VALUE_RESTRICTION:
        className = OWL.HASVALUE;
        break;
      case OConstants.ALL_VALUES_FROM_RESTRICTION:
        className = OWL.ALLVALUESFROM;
        break;
      case OConstants.SOME_VALUES_FROM_RESTRICTION:
        className = OWL.SOMEVALUESFROM;
        break;
      case OConstants.CARDINALITY_RESTRICTION:
        className = OWL.CARDINALITY;
        break;
      case OConstants.MIN_CARDINALITY_RESTRICTION:
        className = OWL.MINCARDINALITY;
        break;
      case OConstants.MAX_CARDINALITY_RESTRICTION:
        className = OWL.MINCARDINALITY;
        break;
      case OConstants.ANNONYMOUS_CLASS:
        className = "Annonymous";
        break;
    }
    return className; 
  }

  public static String getRestrictionName(Restriction res) {
    String className = "Unknown";
    if(res instanceof HasValueRestriction) {
      className = OWL.HASVALUE;
    } else if(res instanceof AllValuesFromRestriction) {
        className = OWL.ALLVALUESFROM;
    } else if(res instanceof SomeValuesFromRestriction) {
        className = OWL.SOMEVALUESFROM;
    } else if(res instanceof CardinalityRestriction) {
        className = OWL.CARDINALITY;
    } else if(res instanceof MinCardinalityRestriction) {
        className = OWL.MINCARDINALITY;
    } else if(res instanceof MaxCardinalityRestriction) {
        className = OWL.MAXCARDINALITY;
    } else if(res instanceof AnonymousClassImpl) {
        className = "Annonymous";
    }
    return className; 
  }
  
  
  /**
   * Creates a new instance of Ontology Instance
   * 
   * @param repositoryID
   * @param ontology
   * @param owlim
   * @param uri
   * @return
   */
  public static OInstance createOInstance(String repositoryID,
          Ontology ontology, OWLIM owlim, String uri) {
    OResource aResource = ontology.getOResourceFromMap(uri);
    if(aResource instanceof OInstance || aResource == null){
      OInstance anInstance = (OInstance)aResource;
      if(anInstance != null) return anInstance;
      anInstance = new OInstanceImpl(new URI(uri, false), ontology, repositoryID,
              owlim);
      ontology.addOResourceToMap(uri, anInstance);
      return anInstance;
    }else{
      throw new GateOntologyException("Expecting " + uri + 
              " to be an instance but it is a \"" + 
              aResource.getClass().getCanonicalName() + "\" instead!");
    }
  }

  /**
   * Utility method that shows warning to the user.
   * 
   * @param warningMsg - message to be displayed to the user
   */
  public static void warning(String warningMsg) {
    System.err.println("WARNING :" + warningMsg);
  }

  /**
   * Utility method that throws a GateRuntimeException to the user.
   * 
   * @param warningMsg - message to be displayed to the user
   */
  public static void error(String errorMsg) {
    throw new GateRuntimeException("ERROR :" + errorMsg);
  }

  public static boolean hasSystemNameSpace(String uri) {
    if(uri.startsWith("http://www.w3.org/2002/07/owl#"))
      return true;
    else if(uri.startsWith("http://www.w3.org/2001/XMLSchema#"))
      return true;
    else if(uri.startsWith("http://www.w3.org/2000/01/rdf-schema#"))
      return true;
    else if(uri.startsWith("http://www.w3.org/1999/02/22-rdf-syntax-ns#"))
      return true;
    else return false;
  }

}
