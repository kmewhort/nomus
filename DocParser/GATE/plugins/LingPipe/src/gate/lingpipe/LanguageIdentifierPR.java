package gate.lingpipe;

import gate.ProcessingResource;
import gate.Resource;
import gate.creole.AbstractLanguageAnalyser;
import gate.creole.ExecutionException;
import gate.creole.ResourceInstantiationException;
import gate.util.GateRuntimeException;

import java.io.File;
import java.io.IOException;
import java.net.URISyntaxException;
import java.net.URL;

import com.aliasi.classify.Classification;
import com.aliasi.classify.Classifier;
import com.aliasi.util.AbstractExternalizable;

/**
 * A Processing resource to identify language of the document based on
 * LingPipe language identifier classifier. Please download appropriate
 * models from the LingPipe website. see
 * http://alias-i.com/lingpipe/web/models.html
 * 
 * The default model supplied with GATE distribution is based on the
 * Leipzig corpora collection that consists of the data in the following
 * languages: Catalan (cat), Danish (dk), English (en), Estonian (ee),
 * Finnish (fi), French (fr), German (de), Italian (it), Japanese (jp),
 * Korean (kr), Norwegian (no), Sorbian (sorb), Swedish (se), and
 * Turkish (tr).
 * 
 * Should you want to train models on other languages or different
 * dataset, please refer to the URL: *
 * http://alias-i.com/lingpipe/demos/tutorial/langid/read-me.html.
 * 
 * @author niraj
 * 
 */
public class LanguageIdentifierPR extends AbstractLanguageAnalyser implements
                                                                  ProcessingResource {

  /** File which cotains model for NE */
  protected URL modelFileUrl;

  /** Model file extracted from the URL */
  protected File modelFile;

  /** classifier object */
  protected Classifier<CharSequence, Classification> classifier;

  /** document feature name */
  protected String languageIdFeatureName;

  /**
   * Initializes this resource
   * 
   * @return Resource
   * @throws ResourceInstantiationException
   */
  public Resource init() throws ResourceInstantiationException {
    if(modelFileUrl == null)
      throw new ResourceInstantiationException("No model file provided!");

    try {
      modelFile = new File(modelFileUrl.toURI());
    }
    catch(URISyntaxException e) {
      throw new ResourceInstantiationException(e);
    }

    if(modelFile == null || !modelFile.exists()) {
      throw new ResourceInstantiationException("modelFile:"
              + modelFileUrl.toString() + " does not exists");
    }

    try {
      classifier = (Classifier<CharSequence, Classification>)AbstractExternalizable
              .readObject(modelFile);
    }
    catch(IOException e) {
      throw new ResourceInstantiationException(e);
    }
    catch(ClassNotFoundException e) {
      throw new ResourceInstantiationException(e);
    }

    return this;
  }

  /**
   * Method is executed after the init() method has finished its
   * execution. <BR>
   * 
   * @throws ExecutionException
   */
  public void execute() throws ExecutionException {
    // lets start the progress and initialize the progress counter
    fireProgressChanged(0);

    // If no document provided to process throw an exception
    if(document == null) {
      fireProcessFinished();
      throw new GateRuntimeException("No document to process!");
    }

    // langugage ID feature Name
    if(languageIdFeatureName == null
            || languageIdFeatureName.trim().length() == 0)
      languageIdFeatureName = "lang";

    String docText = document.getContent().toString();
    Classification classification = classifier.classify(docText);

    document.getFeatures().put(languageIdFeatureName,
            classification.bestCategory());

    // process finished, acknowledge user about this.
    fireProcessFinished();
  }

  /**
   * gets the model to be used for identifying language of the document
   * 
   * @return
   */
  public URL getModelFileUrl() {
    return modelFileUrl;
  }

  /**
   * sets the model to be used for identifying language of the document
   * 
   * @param modelFileUrl
   */
  public void setModelFileUrl(URL modelFileUrl) {
    this.modelFileUrl = modelFileUrl;
  }

  /**
   * gets name of the feature which is used for storing the identified
   * language
   * 
   * @return
   */
  public String getLanguageIdFeatureName() {
    return languageIdFeatureName;
  }

  /**
   * sets name of the feature that should be used for storing the
   * identified language of the document
   * 
   * @param languageIdFeatureName
   */
  public void setLanguageIdFeatureName(String languageIdFeatureName) {
    this.languageIdFeatureName = languageIdFeatureName;
  }

}
