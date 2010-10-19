/*
 *  Copyright (c) 1995-2010, The University of Sheffield. See the file
 *  COPYRIGHT.txt in the software or at http://gate.ac.uk/gate/COPYRIGHT.txt
 *
 *  This file is part of GATE (see http://gate.ac.uk/), and is free
 *  software, licenced under the GNU Library General Public License,
 *  Version 2, June 1991 (in the distribution as file licence.html,
 *  and also available at http://gate.ac.uk/gate/licence.html).
 *
 *  Johann Petrak 2009-08-13
 *
 *  $Id: AbstractOntologyImplSesame.java 12237 2010-02-10 01:41:28Z johann_p $
 */
package gate.creole.ontology.impl.sesame;

import gate.creole.ontology.GateOntologyException;
import gate.creole.ontology.OBNodeID;
import gate.creole.ontology.OConstants;
import gate.creole.ontology.OConstants.OntologyFormat;
import gate.creole.ontology.OConstants.QueryLanguage;
import gate.creole.ontology.OURI;
import gate.creole.ontology.OntologyBooleanQuery;
import gate.creole.ontology.OntologyTupleQuery;
import gate.creole.ontology.impl.AbstractOntologyImpl;
import gate.creole.ontology.impl.Utils;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.Reader;
import java.io.Writer;
import java.util.Set;

/**
 *
 * @author johann
 */
public abstract class AbstractOntologyImplSesame extends AbstractOntologyImpl {


  public void readOntologyData(java.net.URL theURL, String baseURI,
      OConstants.OntologyFormat format, boolean asImport) {
    InputStream is;
    try {
      is = theURL.openStream();
    } catch (IOException ex) {
      throw new GateOntologyException("Problem reading from URL " + theURL, ex);
    }
    boolean isBaseURIset = true;
    if(baseURI == null || baseURI.length() == 0) {
      isBaseURIset = false;
      baseURI = OConstants.ONTOLOGY_DEFAULT_BASE_URI;
    }
    ((OntologyServiceImplSesame) ontologyService).readOntologyData(is,
        baseURI, format, asImport);
    try {
      is.close();
    } catch (IOException ex) {
      throw new GateOntologyException("Problem closing stream from URL " + theURL, ex);
    }
    if(!asImport) {
      if(isBaseURIset && getDefaultNameSpace() == null) {
        setDefaultNameSpace(baseURI);
      }
      Set<OURI> us = addOntologyURIs();
      int n = us.size();
      if(n == 0) {
        Utils.warning("No ontology URI found for ontology loaded from "+theURL);
      } else if(n > 1) {
        Utils.warning("More than one("+n+") ontology URI found for ontology loaded from "+theURL+": "+us);
      } else {
        setDefaultNameSpaceFromOntologyURI();
      }
    }
  }

  public void readOntologyData(File selectedFile, String baseURI,
      OConstants.OntologyFormat format, boolean asImport) {
    boolean isBaseURIset = true;
    if(baseURI == null || baseURI.length() == 0) {
      isBaseURIset = false;
      baseURI = OConstants.ONTOLOGY_DEFAULT_BASE_URI;
    }
   ((OntologyServiceImplSesame) ontologyService).readOntologyData(selectedFile,
        baseURI, format, asImport);
    if(!asImport) {
      if(isBaseURIset && getDefaultNameSpace() == null) {
        setDefaultNameSpace(baseURI);
      }
      Set<OURI> us = addOntologyURIs();
      int n = us.size();
      if(n == 0) {
        Utils.warning("No ontology URI found for ontology loaded from "+selectedFile.getAbsolutePath());
      } else if(n > 1) {
        Utils.warning("More than one("+n+") ontology URI found for ontology loaded from "+selectedFile.getAbsolutePath()+": "+us);
      } else {
        setDefaultNameSpaceFromOntologyURI();
      }
    }
  }

  public void readOntologyData(Reader in, String baseURI, OntologyFormat format,
      boolean asImport) {
    boolean isBaseURIset = true;
    if(baseURI == null || baseURI.length() == 0) {
      isBaseURIset = false;
      baseURI = OConstants.ONTOLOGY_DEFAULT_BASE_URI;
    }
    ((OntologyServiceImplSesame) ontologyService).readOntologyData(in,
        baseURI, format, asImport);
    if(!asImport) {
      if(isBaseURIset && getDefaultNameSpace() == null) {
        setDefaultNameSpace(baseURI);
      }
      Set<OURI> us = addOntologyURIs();
      int n = us.size();
      if(n == 0) {
        Utils.warning("No ontology URI found for ontology loaded");
      } else if(n > 1) {
        Utils.warning("More than one("+n+") ontology URI found for ontology loaded: "+us);
      } else {
        setDefaultNameSpaceFromOntologyURI();
      }
    }
  }

  public void readOntologyData(InputStream in, String baseURI,
      OntologyFormat format, boolean asImport) {
    boolean isBaseURIset = true;
    if(baseURI == null || baseURI.length() == 0) {
      isBaseURIset = false;
      baseURI = OConstants.ONTOLOGY_DEFAULT_BASE_URI;
    }
    ((OntologyServiceImplSesame) ontologyService).readOntologyData(in,
        baseURI, format, asImport);
    if(!asImport) {
      if(isBaseURIset && getDefaultNameSpace() == null) {
        setDefaultNameSpace(baseURI);
      }
      Set<OURI> us = addOntologyURIs();
      int n = us.size();
      if(n == 0) {
        Utils.warning("No ontology URI found for ontology loaded");
      } else if(n > 1) {
        Utils.warning("More than one("+n+") ontology URI found for ontology loaded: "+us);
      } else {
        setDefaultNameSpaceFromOntologyURI();
      }
    }
  }

  private void setDefaultNameSpaceFromOntologyURI() {
    if(getDefaultNameSpace() == null && getOntologyURIs().size() == 1) {
      String uri = getOntologyURIs().get(0).toString();
      if(!uri.endsWith("#")) {
        uri = uri + "#";
      }
      setDefaultNameSpace(uri);
    }
  }

  public void writeOntologyData(File selectedFile,
      OConstants.OntologyFormat format, boolean includeImports) {
    FileWriter fw;
    try {
      fw = new FileWriter(selectedFile);
    } catch (IOException ex) {
      throw new GateOntologyException("Could not open writer for file " +
          selectedFile.getAbsolutePath(), ex);
    }
    ((OntologyServiceImplSesame) ontologyService).writeOntologyData(fw,
        format, includeImports);
  }

  public void writeOntologyData(OutputStream out, OntologyFormat format,
      boolean includeImports) {
    ((OntologyServiceImplSesame) ontologyService).writeOntologyData(out,
        format, includeImports);
  }

  public void writeOntologyData(Writer out, OntologyFormat format,
      boolean includeImports) {
    ((OntologyServiceImplSesame) ontologyService).writeOntologyData(out,
        format, includeImports);
  }

  /**
   * Load the system imports into a repository that does not have them
   * loaded yet.
   */
  public void loadSystemImports() {
    // according to a discussion with Ivan Peikov from OntoText, these
    // imports are not necessary any more. For now this method is left
    // here for reference and the code of the LRs is not chenged, but
    // if no "system" imports are needed anywhere in the future, all the
    // relevant code can be removed.
    // UPDATE 2010-02-10: as it turns out, the owl.rdfs import is needed
    // to correctly define the predefined owl:AnnotationProperty properties
    // like rdfs:label
    
    pluginDir = getPluginDir();
    File owlFile = new File(new File(pluginDir, "config"), "owl.rdfs");
    ((OntologyServiceImplSesame) ontologyService).loadSystemImport(owlFile,
        "http://www.w3.org/2002/07/owl#", OConstants.OntologyFormat.RDFXML);

    //File rdfsFile = new File(new File(pluginDir, "config"), "rdf-schema.rdf");
    //((OntologyServiceImplSesame) ontologyService).loadSystemImport(rdfsFile,
    //    "http://www.w3.org/2000/01/rdf-schema#", OConstants.OntologyFormat.RDFXML);
    
  }

  @Override
  public void cleanOntology() {
    super.cleanOntology();
    loadSystemImports();
  }

  public OURI createOURI(String uriString) {
    return new OURIImpl(uriString);
  }

  public OURI createOURIForName(String resourceName) {
    // TODO: check and normalize resourceName
    String baseURI = getDefaultNameSpace();
    if(baseURI == null) {
      throw new GateOntologyException("Cannot create OURI, no system name space (base URI) set");
    }
    return new OURIImpl(baseURI+resourceName);
  }

  public OURI createOURIForName(String resourceName, String baseURI) {
    // TODO: check and normalize resource name, maybe also URI, or do the
    // latter in the OURI constructor?
    return new OURIImpl(baseURI+resourceName);
  }

  public OURI generateOURI(String resourceName) {
    String baseURI = getDefaultNameSpace();
    if(baseURI == null) {
      throw new GateOntologyException("No default name space set, cannot generate OURI");
    }
    return generateOURI(resourceName, baseURI);
  }

  public OURI generateOURI(String resourceName, String baseURI) {
    if(resourceName == null) {
      resourceName = "";
    }
    // TODO: check and normalize resource name so it is a valid part of an IRI

    // now append our generated suffix
    resourceName = 
        resourceName +
        Long.toString(System.currentTimeMillis(),36) +
        Integer.toString(Math.abs(randomGenerator.nextInt(1296)),36);
    OURI uri = null;
    while(true) {
      uri = createOURIForName(resourceName);
      if (!((OntologyServiceImplSesame)ontologyService).containsURI(uri)) {
        break;
      }
    }
    return uri;
  }

  public OBNodeID createOBNodeID(String id) {
    return new OBNodeIDImpl(id);
  }

  public OntologyBooleanQuery createBooleanQuery(String query, QueryLanguage lang) {
    return new UtilBooleanQuery(
        ((OntologyServiceImplSesame)ontologyService).getRepositoryConnection(),
        query, lang);
  }

  public OntologyTupleQuery createTupleQuery(String query, QueryLanguage lang) {
    return new UtilTupleQueryIterator(
        ((OntologyServiceImplSesame)ontologyService).getRepositoryConnection(),
        query, lang);
    
  }

  @Deprecated
  public String executeQuery(String serqlQuery) {
    return ((OntologyServiceImplSesame)ontologyService).executeQuery(serqlQuery);
  }


}
