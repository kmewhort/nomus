package crawl;

import websphinx.*;

import gate.creole.*;
import gate.persist.PersistenceException;
import gate.security.SecurityException;
import gate.util.*;
import gate.corpora.*;
import gate.*;

import java.net.*;
import java.util.List;


public class SphinxWrapper extends Crawler{

  private static final long serialVersionUID = -4720932687929012109L;
  
  private Corpus corpus = null;
  private static int maxPages = -1;
  private static int count = 0;

  
  protected void resetCounter() {
    count = 0;
  }
  
  
  @SuppressWarnings("unchecked")
  public void visit(Page p) {
    if ((maxPages != -1) && (count >= maxPages)) {
      count = 0;
      super.stop();
      return;
    }

    String docName = p.toURL() + "_" + Gate.genSym();
    FeatureMap params = Factory.newFeatureMap();
    params.put(Document.DOCUMENT_URL_PARAMETER_NAME, p.toURL());
    try {
      Document doc = (Document) Factory.createResource(
              DocumentImpl.class.getName(), params, null, docName
      );
      corpus.add(doc);
      if (corpus.getLRPersistenceId() != null) {
        corpus.unloadDocument(doc);
        Factory.deleteResource(doc);
      }

      System.out.println(count+" ["+p.getDepth()+"] "+p.toURL());
      count++;
    }
    catch (ResourceInstantiationException e) {
      String nl = Strings.getNl();
      Err.prln(
              "WARNING: could not intantiate document" + nl +
              "  Document name was: " + docName + nl +
              "  Exception was: " + e + nl + nl
      );
    }
  }

  public boolean shouldVisit(Link l) {
    return super.shouldVisit(l);
  }

  public void setDepth(int depth) {
    super.setMaxDepth(depth);
  }

  public void setMaxPages(int max) {
    maxPages = max;
  }

  public int getMaxPages() {
    return maxPages;
  }

  protected void interrupt()  {
    super.stop();
    if (corpus.getLRPersistenceId() != null) {
      try {
        corpus.sync();
      }
      catch(PersistenceException e) {
        e.printStackTrace();
      }
      catch(SecurityException e) {
        e.printStackTrace();
      }
    }
  }

  
  public void setStart(String root) {
    try {
      URL url = new URL(root);
      Link link = new Link(url);
      super.setRoot(link);
    }
    catch (MalformedURLException me) {
      System.err.println("Malformed url "+root);
      me.printStackTrace();
    }
  }

  public void setStart(URL root) {
    Link link = new Link(root);
    super.setRoot(link);
  }


  public void setStarts(List<URL> roots) {
    int n = roots.size();
    Link[] links = new Link[roots.size()];
    for (int i=0; i < n ; i++) {
      links[i] = new Link(roots.get(i));
    }
    super.setRoots(links);
  }

  public void setCorpus(Corpus corpus) {
    this.corpus = corpus;
  }

  public void start() {
    super.run();
  }

}