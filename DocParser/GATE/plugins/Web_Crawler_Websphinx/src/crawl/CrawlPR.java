/*
 *  CrawlPR.java
 *
 *  Copyright (c) 1995-2010, The University of Sheffield. See the file
 *  COPYRIGHT.txt in the software or at http://gate.ac.uk/gate/COPYRIGHT.txt
 *
 *  This file is part of GATE (see http://gate.ac.uk/), and is free
 *  software, licenced under the GNU Library General Public License,
 *  Version 2, June 1991 (in the distribution as file licence.html,
 *  and also available at http://gate.ac.uk/gate/licence.html).
 *
 *  Google API and other sources subject to Google License. Please
 *  see http://www.google.com/apis/
 */

package crawl;

import java.net.URL;
import java.util.*;
import gate.Corpus;
import gate.Document;
import gate.Factory;
import gate.ProcessingResource;
import gate.Resource;
import gate.creole.*;
import gate.util.*;
import websphinx.*;

public class CrawlPR 
  extends AbstractLanguageAnalyser 
  implements ProcessingResource {

  private static final long serialVersionUID = 7119190892757004776L;
  
  private String root = null;
  private int depth = -1;
  private Corpus outputCorpus = null;
  private Boolean dfs = null;
  private SphinxWrapper crawler;
  private String domain = null;
  private Corpus source = null;
  private int max = -1;

  /** Constructor of the class */
  public CrawlPR() {

  }

  /** Initialise this resource, and return it. */
  public Resource init() throws ResourceInstantiationException {
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
   * Override the default behaviour by interrupting the SphinxWrapper itself.  Otherwise,
   * the SphinxWrapper would run uncontrollably.
   * @throws ExecutionInterruptedException 
   */
  public void interrupt() {
    this.interrupted = true;
    if (crawler != null) {
      crawler.interrupt();
    }
    
  }
  
  
  /**
   * This method runs the crawler. It assumes that all the needed
   * parameters are set. If they are not, an exception will be fired.
   */
  public void execute() throws ExecutionException {
    interrupted = false;
    crawler = new SphinxWrapper();
    crawler.resetCounter();
    
    if(outputCorpus == null) { throw new ExecutionException(
    "Output Corpus cannot be null"); }

    if(root == null && source == null) { throw new ExecutionException(
    "Either root or source must be initialized"); }
    if(depth == -1) { throw new ExecutionException("Limit is not initialized"); }
    if(dfs == null) { throw new ExecutionException("dfs is not initialized"); }
    if(domain == null) { throw new ExecutionException(
    "domain type is not initialized.. Set to either SERVER/SUBTREE/WEB"); }

    try {
      crawler.setCorpus(outputCorpus);
      crawler.setDepth(depth);
      crawler.setDepthFirst(dfs.booleanValue());
      
      if(domain == "SUBTREE") {
        crawler.setDomain(Crawler.SUBTREE);
      }
      else if(domain == "SERVER") {
        crawler.setDomain(Crawler.SERVER);
      }
      else {
        crawler.setDomain(Crawler.WEB);
      }
      
      if(max != -1) {
        crawler.setMaxPages(max);
      }
      
      if(root != null && (root.length() > 0)) {
        crawler.setStart(root);
      }
      else {
        Corpus roots = (Corpus) source;
        List<URL> urls = new ArrayList<URL>();
        for(int i = 0; i < roots.size(); i++) {
          boolean docWasLoaded = roots.isDocumentLoaded(i);
          Document doc = (Document) roots.get(i);
          URL url = doc.getSourceUrl();
          if (url != null) {
            System.out.println("adding   " + url.toString());
            urls.add(url);
          }
          else {
            System.out.println("skipping " + doc.getName());
          }
          
          
          if(! docWasLoaded) {
            roots.unloadDocument(doc);
            Factory.deleteResource(doc);
          }
        }
        crawler.setStarts(urls);
      }

      crawler.start();
      if (interrupted) {
        throw new ExecutionInterruptedException();
      }
    }
    catch(Exception e) {
      String nl = Strings.getNl();
      Err.prln("  Exception was: " + e + nl + nl);
      e.printStackTrace();
    }
  }

  public void setRoot(String root) {
    this.root = root;
  }

  public String getRoot() {
    return this.root;
  }

  public void setDepth(Integer limit) {
    this.depth = limit.intValue();
  }

  public Integer getDepth() {
    return new Integer(this.depth);
  }

  public void setDfs(Boolean dfs) {
    this.dfs = dfs;
  }

  public Boolean getDfs() {
    return this.dfs;
  }

  public void setDomain(String domain) {
    this.domain = domain;
  }

  public String getDomain() {
    return this.domain;
  }

  public void setSource(Corpus source) {
    this.source = source;
  }

  public Corpus getSource() {
    return this.source;
  }

  public void setMax(Integer max) {
    this.max = max.intValue();
  }

  public Integer getMax() {
    return new Integer(this.max);
  }

  public Corpus getOutputCorpus() {
    return outputCorpus;
  }

  public void setOutputCorpus(Corpus outputCorpus) {
    this.outputCorpus = outputCorpus;
  }

}