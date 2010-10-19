package gate.compound.impl;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.io.StringReader;
import java.io.UnsupportedEncodingException;
import java.net.URL;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import gate.*;
import gate.alignment.Alignment;
import gate.creole.ResourceInstantiationException;
import gate.util.GateRuntimeException;

/**
 * Implemention of the CompoundDocument. Compound Document is a set of
 * one or more documents. It provides a more convenient way to group
 * documents and interpret them as a single document. It has a
 * capability to switch the focus among the different memebers of it.
 * 
 * @author niraj
 */
public class CompoundDocumentFromXml extends CompoundDocumentImpl {

  private static final long serialVersionUID = 8114328411647768889L;

  /** Initialise this resource, and return it. */
  public Resource init() throws ResourceInstantiationException {
    // set up the source URL and create the content
    if(sourceUrl == null) {
      throw new ResourceInstantiationException(
              "The sourceURL and document's content were null.");
    }

    try {
      StringBuilder xmlString = new StringBuilder();
      BufferedReader br = new BufferedReader(new InputStreamReader(sourceUrl
              .openStream(), "utf-8"));
      String line = br.readLine();
      while(line != null) {
        xmlString.append("\n").append(line);
        line = br.readLine();
      }

      StringReader reader = new StringReader(xmlString.toString());
      com.thoughtworks.xstream.XStream xstream = new com.thoughtworks.xstream.XStream(
              new com.thoughtworks.xstream.io.xml.StaxDriver());

      // asking the xstream library to use gate class loader
      xstream.setClassLoader(Gate.getClassLoader());

      // reading the xml object
      Map<String, Object> globalMap = (HashMap<String, Object>)xstream
              .fromXML(reader);

      // now we read individual information
      Map<String, String> docXmls = (HashMap<String, String>)globalMap
              .get("docXmls");
      Map<String, Object> features = (Map<String, Object>)globalMap
              .get("feats");

      String encoding = (String)features.get("encoding");
      super.setEncoding(encoding);

      File tempFile = File.createTempFile("example", ".xml");
      File tempFolder = new File(tempFile.getParentFile(), "temp"
              + Gate.genSym());

      if(!tempFolder.exists() && !tempFolder.mkdirs()) {
        throw new GateRuntimeException("Temporary folder "
                + tempFolder.getAbsolutePath() + " could not be created");
      }
      tempFile.deleteOnExit();
      tempFolder.deleteOnExit();

      URL sourceUrl = null;
      List<String> docIDs = new ArrayList<String>();
      for(String id : docXmls.keySet()) {
        docIDs.add(id);
        File newFile = new File("X." + id + ".xml");
        newFile.deleteOnExit();
        BufferedWriter bw = new BufferedWriter(new OutputStreamWriter(
                new FileOutputStream(newFile), encoding));
        bw.write(docXmls.get(id));
        bw.flush();
        bw.close();
        sourceUrl = newFile.toURI().toURL();
      }
      super.setDocumentIDs(docIDs);
      super.setSourceUrl(sourceUrl);

      String name = (String)features.get("name");
      features.remove("name");
      FeatureMap fets = Factory.newFeatureMap();
      for(String s : features.keySet()) {
        fets.put(s, features.get(s));
      }

      this.setName(name);
      super.init();

      Document aDoc = getCurrentDocument();
      setCurrentDocument(null);

      FeatureMap docFeatures = (FeatureMap)globalMap.get("docFeats");
      for(Object key : docFeatures.keySet()) {
        Object value = docFeatures.get(key);
        if(value instanceof Alignment) {
          ((Alignment)value).setSourceDocument(this);
        }
      }

      setFeatures(docFeatures);

      if(aDoc != null) setCurrentDocument(aDoc.getName());
      br.close();
    }
    catch(UnsupportedEncodingException uee) {
      throw new ResourceInstantiationException(uee);
    }
    catch(IOException ioe) {
      throw new ResourceInstantiationException(ioe);
    }
    return this;
  } // init()
}