/*
 *  YahooPR.java
 *
 *  Copyright (c) 1995-2010, The University of Sheffield. See the file
 *  COPYRIGHT.txt in the software or at http://gate.ac.uk/gate/COPYRIGHT.txt
 *
 *  This file is part of GATE (see http://gate.ac.uk/), and is free
 *  software, licenced under the GNU Library General Public License,
 *  Version 2, June 1991 (in the distribution as file licence.html,
 *  and also available at http://gate.ac.uk/gate/licence.html).
 *  $Id: YahooPR.java 12006 2009-12-01 17:24:28Z thomas_heitz $  
 */
package gate.yahoo;

import gate.*;
import gate.ProcessingResource;
import gate.Resource;
import gate.creole.*;
import gate.corpora.*;
import gate.*;
import java.util.*;
import com.yahoo.search.*;

/**
 * Given a query and other required parameters, this PR searches on YAHOO to retrieve the
 * top n documents that match the query. Each found document is converted in to a GATE document and
 * populated in the provided corpus.
 * 
 * @author niraj
 */
public class YahooPR extends AbstractLanguageAnalyser implements
                                                     ProcessingResource {
  /**
   * Search Query
   */
  private String query = null;

  /**
   * No of documents to retrieve
   */
  private int limit = -1;

  /**
   * One has to obtain an ID to use Yahoo Search Engine. Visit www.yahoo.com for
   * more information.
   */
  private String applicationID = null;

  /**
   * If set to false, the corpus is emptied prior to adding found documents by
   * yahoo.
   */
  private Boolean corpusAppendMode;

  /**
   * One can specify a list of pages which need to be excluded from the search.
   */
  private ArrayList pagesToExclude;

  /**
   * One can also specify the types of files to search for
   */
  private String fileFormat = YahooSearch.ALL;

  /**
   * Instance of a YahooSearch - it is a real logic to search on yahoo using the
   * yahoo library.
   */
  private YahooSearch searcher;

  /** Constructor of the class */
  public YahooPR() {
  }

  /** Initialise this resource, and return it. */
  public Resource init() throws ResourceInstantiationException {
    if(applicationID == null) { throw new ResourceInstantiationException(
            "ApplicationID not provided"); }
    searcher = new YahooSearch(applicationID);
    return super.init();
  }

  /**
   * Reinitialises the processing resource. After calling this method the
   * resource should be in the state it is after calling init. If the resource
   * depends on external resources (such as rules files) then the resource will
   * re-read those resources. If the data used to create the resource has
   * changed since the resource has been created then the resource will change
   * too after calling reInit().
   */
  public void reInit() throws ResourceInstantiationException {
    init();
  }

  /**
   * This method runs the coreferencer. It assumes that all the needed
   * parameters are set. If they are not, an exception will be fired.
   */
  public void execute() throws ExecutionException {
    if(corpus == null) { throw new ExecutionException(
            "Corpus to store results in is not provided"); }
    if(query == null) { throw new ExecutionException("Query is not initialized"); }
    if(limit <= 0) { throw new ExecutionException("Limit is not initialized"); }
    if(!corpusAppendMode.booleanValue()) {
      while(corpus.size() > 0) {
        Resource resource = (Resource)corpus.get(0);
        corpus.remove(0);
        Factory.deleteResource(resource);
      }
    }
    try {
      searcher.setFormat(fileFormat);
      WebSearchResult[] results = searcher.search(query, this.limit);
      if(results == null) { return; }
      // for each result we need to create a gate document
      // and add it into the provided corpus
      for(int i = 0; i < results.length; i++) {
        String urlString = results[i].getUrl();
        if(pagesToExclude != null && pagesToExclude.contains(urlString)) {
          continue;
        }
        try {
          String docName = urlString + "_" + Gate.genSym();
          FeatureMap params = Factory.newFeatureMap();
          params.put(Document.DOCUMENT_URL_PARAMETER_NAME, urlString);
          Document doc = (Document)Factory.createResource(DocumentImpl.class
                  .getName(), params, null, docName);
          corpus.add(doc);
          if(corpus.getLRPersistenceId() != null) {
            // persistent corpus -> unload the document
            corpus.unloadDocument(doc);
            Factory.deleteResource(doc);
          }
        } catch(Exception e) {
          System.out.println("Ignoring : " + urlString);
        }
      }
    } catch(Exception e) {
      throw new ExecutionException(e);
    }
  }

  public void setQuery(String query) {
    this.query = query;
  }

  public String getQuery() {
    return this.query;
  }

  /**
   * One has to obtain an ID to use Yahoo Search Engine. Visit www.yahoo.com for
   * more information.
   */
  public void setApplicationID(String key) {
    this.applicationID = key;
  }

  /**
   * Returns the set application ID
   * 
   * @return
   */
  public String getApplicationID() {
    return this.applicationID;
  }

  /**
   * Number of documents to search for
   * 
   * @param limit
   */
  public void setLimit(Integer limit) {
    this.limit = limit.intValue();
  }

  /**
   * Number of documents to search for
   * 
   * @return
   */
  public Integer getLimit() {
    return new Integer(this.limit);
  }

  /**
   * The corpus in which all the found documents are populated
   */
  public Corpus getCorpus() {
    return corpus;
  }

  /**
   * The corpus in which all the found documents are populated
   */
  public void setCorpus(Corpus corpus) {
    this.corpus = corpus;
  }

  /**
   * If set to false, the corpus is emptied prior to adding found documents by
   * yahoo.
   */
  public void setCorpusAppendMode(Boolean appendMode) {
    this.corpusAppendMode = appendMode;
  }

  /**
   * If set to false, the corpus is emptied prior to adding found documents by
   * yahoo.
   */
  public Boolean getCorpusAppendMode() {
    return this.corpusAppendMode;
  }

  /**
   * List of pages which need to be excluded from the search.
   */
  public void setPagesToExclude(List pagesToExclude) {
    this.pagesToExclude = new ArrayList();
    // pagesToExclude is an optional param.
    // If it is null, the list should be empty.
    if(pagesToExclude == null) return;
    Iterator iterator = pagesToExclude.iterator();
    while(iterator.hasNext()) {
      String page = (String)iterator.next();
      page = page.toLowerCase();
      this.pagesToExclude.add(page);
    }
  }

  /**
   * A list of pages which need to be excluded from the search.
   */
  public List getPagesToExclude() {
    return this.pagesToExclude;
  }

  /**
   * Supported File Formats: "all", "html", "msword", "pdf", "ppt", "rss",
   * "txt", "xls"
   */
  public String getFileFormat() {
    return fileFormat;
  }

  /**
   * Supported File Formats: "all", "html", "msword", "pdf", "ppt", "rss",
   * "txt", "xls"
   */
  public void setFileFormat(String fileFormat) {
    this.fileFormat = fileFormat;
  }
}