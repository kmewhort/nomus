package gate.creole.ontology.owlim;

import gate.creole.ontology.*;
import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import org.openrdf.vocabulary.OWL;
import org.openrdf.vocabulary.RDF;
import org.openrdf.vocabulary.RDFS;

public class OntologyEventsLog implements OConstants {

  private List<OEvent> events = new ArrayList<OEvent>();

  public void addOResouceAddedEvent(Ontology ontology, OResource res) {
    if(res instanceof Restriction) {
      String property = ((Restriction)res).getOnPropertyValue().getURI()
              .toString();
      if(res instanceof CardinalityRestriction) {
        CardinalityRestriction cr = (CardinalityRestriction)res;
        DataType dt = cr.getDataType();
        String value = cr.getValue();
        addEvent(new OEvent(res.getURI().toString(), RDF.TYPE, OWL.CARDINALITY,
                true));
        addEvent(new OEvent(res.getURI().toString(), OWL.ONPROPERTY, property,
                true));
        addEvent(new OEvent(res.getURI().toString(), OWL.CARDINALITY, value, dt
                .getXmlSchemaURIString(), true));
      }
      else if(res instanceof MinCardinalityRestriction) {
        MinCardinalityRestriction cr = (MinCardinalityRestriction)res;
        DataType dt = cr.getDataType();
        String value = cr.getValue();
        addEvent(new OEvent(res.getURI().toString(), RDF.TYPE,
                OWL.MINCARDINALITY, true));
        addEvent(new OEvent(res.getURI().toString(), OWL.ONPROPERTY, property,
                true));
        addEvent(new OEvent(res.getURI().toString(), OWL.MINCARDINALITY, value,
                dt.getXmlSchemaURIString(), true));
      }
      else if(res instanceof MaxCardinalityRestriction) {
        MaxCardinalityRestriction cr = (MaxCardinalityRestriction)res;
        DataType dt = cr.getDataType();
        String value = cr.getValue();
        addEvent(new OEvent(res.getURI().toString(), RDF.TYPE,
                OWL.MINCARDINALITY, true));
        addEvent(new OEvent(res.getURI().toString(), OWL.ONPROPERTY, property,
                true));
        addEvent(new OEvent(res.getURI().toString(), OWL.MAXCARDINALITY, value,
                dt.getXmlSchemaURIString(), true));
      }
      else if(res instanceof AllValuesFromRestriction) {
        AllValuesFromRestriction cr = (AllValuesFromRestriction)res;
        String hasValue = cr.getHasValue().getURI().toString();
        addEvent(new OEvent(res.getURI().toString(), RDF.TYPE,
                OWL.ALLVALUESFROM, true));
        addEvent(new OEvent(res.getURI().toString(), OWL.ONPROPERTY, property,
                true));
        addEvent(new OEvent(res.getURI().toString(), OWL.ALLVALUESFROM,
                hasValue, true));
      }
      else if(res instanceof SomeValuesFromRestriction) {
        SomeValuesFromRestriction cr = (SomeValuesFromRestriction)res;
        String hasValue = cr.getHasValue().getURI().toString();
        addEvent(new OEvent(res.getURI().toString(), RDF.TYPE,
                OWL.SOMEVALUESFROM, true));
        addEvent(new OEvent(res.getURI().toString(), OWL.ONPROPERTY, property,
                true));
        addEvent(new OEvent(res.getURI().toString(), OWL.SOMEVALUESFROM,
                hasValue, true));
      }
      else if(res instanceof HasValueRestriction) {
        HasValueRestriction cr = (HasValueRestriction)res;
        String hasValue = "";
        if(cr.getOnPropertyValue() instanceof DatatypePropertyImpl) {
          hasValue = ((Literal)cr.getHasValue()).getValue();
        }
        else {
          hasValue = ((OResource)cr.getHasValue()).getURI().toString();
        }
        addEvent(new OEvent(res.getURI().toString(), RDF.TYPE, OWL.HASVALUE,
                true));
        addEvent(new OEvent(res.getURI().toString(), OWL.ONPROPERTY, property,
                true));
        addEvent(new OEvent(res.getURI().toString(), OWL.HASVALUE, hasValue,
                true));
      }
    }
    else if(res instanceof AnonymousClass) {
      addEvent(new OEvent(res.getURI().toString(), RDF.TYPE, OWL.CLASS, true));
    }
    else if(res instanceof OClass) {
      addEvent(new OEvent(res.getURI().toString(), RDF.TYPE, OWL.CLASS, true));
    }
    else if(res instanceof RDFProperty) {
      if(res instanceof AnnotationProperty) {
        addEvent(new OEvent(res.getURI().toString(), RDF.TYPE,
                OWL.ANNOTATIONPROPERTY, true));
      }
      else if(res instanceof DatatypeProperty) {
        DatatypeProperty dp = (DatatypeProperty)res;
        addEvent(new OEvent(res.getURI().toString(), RDF.TYPE,
                OWL.DATATYPEPROPERTY, true));
        addEvent(new OEvent(res.getURI().toString(), RDFS.RANGE, dp
                .getDataType().getXmlSchemaURIString(), true));
        for(OResource domain : dp.getDomain()) {
          addEvent(new OEvent(res.getURI().toString(), RDFS.DOMAIN, domain
                  .getURI().toString(), true));
        }
      }
      else if(res instanceof SymmetricProperty) {
        SymmetricProperty dp = (SymmetricProperty)res;
        addEvent(new OEvent(res.getURI().toString(), RDF.TYPE,
                OWL.SYMMETRICPROPERTY, true));
        for(OResource domain : dp.getDomain()) {
          addEvent(new OEvent(res.getURI().toString(), RDFS.DOMAIN, domain
                  .getURI().toString(), true));
        }
        for(OResource domain : dp.getRange()) {
          addEvent(new OEvent(res.getURI().toString(), RDFS.RANGE, domain
                  .getURI().toString(), true));
        }

      }
      else if(res instanceof TransitiveProperty) {
        TransitiveProperty dp = (TransitiveProperty)res;
        addEvent(new OEvent(res.getURI().toString(), RDF.TYPE,
                OWL.TRANSITIVEPROPERTY, true));
        for(OResource domain : dp.getDomain()) {
          addEvent(new OEvent(res.getURI().toString(), RDFS.DOMAIN, domain
                  .getURI().toString(), true));
        }
        for(OResource domain : dp.getRange()) {
          addEvent(new OEvent(res.getURI().toString(), RDFS.RANGE, domain
                  .getURI().toString(), true));
        }
      }
      else if(res instanceof ObjectProperty) {
        ObjectProperty dp = (ObjectProperty)res;
        addEvent(new OEvent(res.getURI().toString(), RDF.TYPE,
                OWL.OBJECTPROPERTY, true));
        for(OResource domain : dp.getDomain()) {
          addEvent(new OEvent(res.getURI().toString(), RDFS.DOMAIN, domain
                  .getURI().toString(), true));
        }
        for(OResource domain : dp.getRange()) {
          addEvent(new OEvent(res.getURI().toString(), RDFS.RANGE, domain
                  .getURI().toString(), true));
        }
      }
      else {
        RDFProperty dp = (RDFProperty)res;
        addEvent(new OEvent(res.getURI().toString(), RDF.TYPE, RDF.PROPERTY,
                true));
        for(OResource domain : dp.getDomain()) {
          addEvent(new OEvent(res.getURI().toString(), RDFS.DOMAIN, domain
                  .getURI().toString(), true));
        }
        for(OResource domain : dp.getRange()) {
          addEvent(new OEvent(res.getURI().toString(), RDFS.RANGE, domain
                  .getURI().toString(), true));
        }
      }
    }
    else if(res instanceof OInstance) {
      OClass aClass = ((OInstance)res).getOClasses(OConstants.DIRECT_CLOSURE)
              .iterator().next();
      addEvent(new OEvent(res.getURI().toString(), RDF.TYPE, aClass.getURI()
              .toString(), true));
    }
  }

  public void addOResoucesRemovedEvent(Ontology ontology,
          String[] removedResources) {
    for(String res : removedResources) {
      addEvent(new OEvent(res, "*", "*", false));
      addEvent(new OEvent("*", res, "*", false));
      addEvent(new OEvent("*", "*", res, false));
    }
  }

  public void addOntologyResetEvent(Ontology ontology) {
    addEvent(new OEvent("*", "*", "*", false));
  }

  public void addResourceRelationChangedEvent(Ontology ontology,
          OResource resource1, OResource resource2, int eventType) {
    switch(eventType) {
      case SUB_CLASS_ADDED_EVENT:
        addEvent(new OEvent(resource2.getURI().toString(), RDFS.SUBCLASSOF,
                resource1.getURI().toString(), true));
        break;
      case SUB_CLASS_REMOVED_EVENT:
        addEvent(new OEvent(resource2.getURI().toString(), RDFS.SUBCLASSOF,
                resource1.getURI().toString(), false));
        break;
      case EQUIVALENT_CLASS_EVENT:
        addEvent(new OEvent(resource1.getURI().toString(), OWL.EQUIVALENTCLASS,
                resource2.getURI().toString(), true));
        break;
      case EQUIVALENT_PROPERTY_EVENT:
        addEvent(new OEvent(resource1.getURI().toString(),
                OWL.EQUIVALENTPROPERTY, resource2.getURI().toString(), true));
        break;
      case DIFFERENT_INSTANCE_EVENT:
        addEvent(new OEvent(resource1.getURI().toString(), OWL.DIFFERENTFROM,
                resource2.getURI().toString(), true));
        break;
      case SAME_INSTANCE_EVENT:
        addEvent(new OEvent(resource1.getURI().toString(), OWL.SAMEAS,
                resource2.getURI().toString(), true));
        break;
      case RESTRICTION_ON_PROPERTY_VALUE_CHANGED:
        addEvent(new OEvent(resource1.getURI().toString(), OWL.ONPROPERTY,
                resource2.getURI().toString(), true));
        break;
      case SUB_PROPERTY_ADDED_EVENT:
        addEvent(new OEvent(resource2.getURI().toString(), RDFS.SUBPROPERTYOF,
                resource1.getURI().toString(), true));
        break;
      case SUB_PROPERTY_REMOVED_EVENT:
        addEvent(new OEvent(resource2.getURI().toString(), RDFS.SUBPROPERTYOF,
                resource1.getURI().toString(), false));
        break;
    }
  }

  public void addResourcePropertyValueChangedEvent(Ontology ontology,
          OResource resource, RDFProperty property, Object value, int eventType) {
    switch(eventType) {
      case ANNOTATION_PROPERTY_VALUE_ADDED_EVENT:
        if(value != null && value instanceof Literal) {
          Literal l = (Literal)value;
          addEvent(new OEvent(resource.getURI().toString(), property.getURI()
                  .toString(), l.getValue(), l.getDataType().getXmlSchemaURIString(),
                  true));
        }
        break;
      case DATATYPE_PROPERTY_VALUE_ADDED_EVENT:
        if(value != null && value instanceof Literal) {
          Literal l = (Literal)value;
          addEvent(new OEvent(resource.getURI().toString(), property.getURI()
                  .toString(), l.getValue(), l.getDataType().getXmlSchemaURIString(),
                  true));
        }
        break;
      case OBJECT_PROPERTY_VALUE_ADDED_EVENT:
        if(value != null && value instanceof OInstance) {
          addEvent(new OEvent(resource.getURI().toString(), property.getURI()
                  .toString(), ((OInstance)value).getURI().toString(), true));
        }
        break;

      case RDF_PROPERTY_VALUE_ADDED_EVENT:
        if(value != null && value instanceof OResource) {
          addEvent(new OEvent(resource.getURI().toString(), property.getURI()
                  .toString(), ((OResource)value).getURI().toString(), true));
        }
        break;

      case ANNOTATION_PROPERTY_VALUE_REMOVED_EVENT:
        if(value != null && value instanceof Literal) {
          Literal l = (Literal)value;
          addEvent(new OEvent(resource.getURI().toString(), property.getURI()
                  .toString(), l.getValue(), l.getDataType().getXmlSchemaURIString(),
                  false));
        }
        break;
      case DATATYPE_PROPERTY_VALUE_REMOVED_EVENT:
        if(value != null && value instanceof Literal) {
          Literal l = (Literal)value;
          addEvent(new OEvent(resource.getURI().toString(), property.getURI()
                  .toString(), l.getValue(), l.getDataType().getXmlSchemaURIString(),
                  false));
        }
        break;

      case OBJECT_PROPERTY_VALUE_REMOVED_EVENT:
        if(value != null && value instanceof OInstance) {
          addEvent(new OEvent(resource.getURI().toString(), property.getURI()
                  .toString(), ((OInstance)value).getURI().toString(), false));
        }
        break;

      case RDF_PROPERTY_VALUE_REMOVED_EVENT:
        if(value != null && value instanceof OResource) {
          addEvent(new OEvent(resource.getURI().toString(), property.getURI()
                  .toString(), ((OResource)value).getURI().toString(), false));
        }
        break;
    }
  }

  public void addEvent(OEvent tripple) {
    events.add(tripple);
  }

  public List<OEvent> getEvents() {
    return this.events;
  }

  public static void exportLog(List<OEvent> events, File file)
          throws IOException {

    BufferedWriter bw = new BufferedWriter(new FileWriter(file));
    for(OEvent oe : events) {
      bw.write(oe.toString());
      bw.newLine();
    }
    bw.close();
  }

  public static void exportLog(String[] events, File file) throws IOException {

    BufferedWriter bw = new BufferedWriter(new FileWriter(file));
    for(String oe : events) {
      bw.write(oe);
      bw.newLine();
    }
    bw.close();
  }

  public static List<OEvent> importLog(File file) throws IOException {
    List<OEvent> events = new ArrayList<OEvent>();
    BufferedReader br = new BufferedReader(new FileReader(file));
    String line = br.readLine();
    while(line != null) {
      events.add(OEvent.parseEvent(line));
      line = br.readLine();
    }
    br.close();
    return events;
  }

}
