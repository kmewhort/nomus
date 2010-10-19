/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package gate.creole.ontology.impl.sesame;

import gate.creole.ontology.InvalidValueException;
import java.util.Locale;
import gate.creole.ontology.DataType;
import gate.creole.ontology.GateOntologyException;
import gate.creole.ontology.LiteralOrONodeID;
import gate.creole.ontology.ONodeID;
import org.openrdf.model.Literal;
import org.openrdf.model.URI;
import org.openrdf.model.Value;
import org.openrdf.model.impl.BNodeImpl;
import org.openrdf.model.impl.LiteralImpl;
import org.openrdf.model.impl.URIImpl;
import org.openrdf.model.vocabulary.XMLSchema;

/**
 *
 * @author johann
 */
public class UtilConvert {
  public static gate.creole.ontology.Literal toGateLiteral(
      org.openrdf.model.Literal sesameLiteral) {
    URI dtu = sesameLiteral.getDatatype();
    DataType dt;
    String lang = sesameLiteral.getLanguage();
    if(dtu == null) {
      if(lang != null) {
        //System.err.println("Could not get a datatype for literal, using string: "+sesameLiteral);
        dt = DataType.getStringDataType();
      } else {
        //System.err.println("Could not get a datatype for literal, using anyType: "+sesameLiteral);
        dt = new DataType(new OURIImpl("http://www.w3.org/2001/XMLSchema#anyType"));
      }
    } else {
      dt = toGateDataType(sesameLiteral.getDatatype());
    }
    Locale locale = null;
    if(lang != null) {
      locale = lang2locale(lang);
    }
    if(locale != null) {
      return new gate.creole.ontology.Literal(sesameLiteral.getLabel(),locale);
    } else {
      try {
        return new gate.creole.ontology.Literal(sesameLiteral.getLabel(), dt);
      } catch (InvalidValueException ex) {
        // TODO: what to do here?
        throw new GateOntologyException(
            "Could not convert literal from Sesame: "+sesameLiteral);
      }
    }
  }

  public static Literal toSesameLiteral(gate.creole.ontology.Literal lit) {
    DataType dt = lit.getDataType();
    return new LiteralImpl(lit.toString(),new URIImpl(dt.getXmlSchemaURIString()));
  }
  public static Value toSesameValue(LiteralOrONodeID val) {
    Value theValue = null;
    if(val.isLiteral()) {
      toSesameLiteral(val.getLiteral());
    } else {
      ONodeID id = val.getONodeID();
      return toSesameValue(id);
    }
    return theValue;
  }

  public static Value toSesameValue(ONodeID id) {
    if(id.isAnonymousResource()) {
      return new BNodeImpl(id.getResourceName().substring(2));
    } else {
      return new URIImpl(id.toString());
    }
  }

  public static gate.creole.ontology.DataType toGateDataType(org.openrdf.model.URI uri) {
    DataType dt = new DataType(new OURIImpl(uri.toString()));
    return dt;
  }

  public static Locale lang2locale(String lang) {
    if(lang == null) {
      return null;
    }
    Locale locale = new Locale(lang);
    System.out.println("Trying to convert language to locale: "+lang+"="+locale);
    return locale;
  }

}
