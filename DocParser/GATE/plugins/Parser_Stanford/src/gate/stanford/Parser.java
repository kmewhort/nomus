package gate.stanford;

import edu.stanford.nlp.parser.lexparser.*;
import edu.stanford.nlp.trees.*;
import gate.*;
import gate.util.*;
import gate.creole.*;
import gate.creole.metadata.*;

import java.io.*;
import java.net.*;
import java.util.*;

import edu.stanford.nlp.ling.*;

/**
 * GATE PR wrapper around the Stanford Parser. This class expects to find Token
 * and Sentence annotations (such as those created by the ANNIE tokenizer and
 * splitter) already in the inputAS and transforms them into suitable data
 * structures, which it feeds to the LexicalizedParser. The parser's output can
 * be stored in the outputAS in various ways, controlled by CREOLE run-time
 * parameters.
 * @author adam
 */
@CreoleResource(name = "StanfordParser", comment = "Stanford parser wrapper",
        helpURL = "http://gate.ac.uk/userguide/sec:parsers:stanford")
public class Parser extends AbstractLanguageAnalyser 
implements ProcessingResource {

  private static final long serialVersionUID = -3062171258011850283L;

  protected edu.stanford.nlp.parser.lexparser.LexicalizedParser stanfordParser;

  /* Type "SyntaxTreeNode" with feature "cat" is compatible with the 
   * classic SyntaxTreeViewer.  */
  private static final String OUTPUT_PHRASE_TYPE   = "SyntaxTreeNode" ;
  private static final String PSG_TAG_FEATURE      = "cat" ;
  
  /* But "category" feature is compatible with the ANNIE POS tagger.  */
  private static final String  POS_TAG_FEATURE    = ANNIEConstants.TOKEN_CATEGORY_FEATURE_NAME;
  private static final String  inputSentenceType  = ANNIEConstants.SENTENCE_ANNOTATION_TYPE;
  private static final String  inputTokenType     = ANNIEConstants.TOKEN_ANNOTATION_TYPE;

  private static final String DEP_ANNOTATION_TYPE   = "Dependency";
  private static final String DEP_ARG_FEATURE       = "args";
  private static final String DEP_LABEL_FEATURE     = "kind"; 

  protected String                         annotationSetName;
  protected AnnotationSet                  annotationSet;
  protected gate.Document                  document;
  private   URL                            parserFile;
  protected boolean                        debugMode;
  private   boolean                        reusePosTags;

  private OffsetComparator                 offsetComparator;
  private Map<String, String>              tagMap;
  protected StanfordSentence               stanfordSentence;
  protected GrammaticalStructureFactory gsf;
  

  /*  CREOLE parameters for optional mapping  */
  private boolean                          useMapping = false; 
  private URL                              mappingFileURL;
  
  /*  internal variables for mapping  */
  private File                             mappingFile;
  private boolean                          mappingLoaded = false;
  
  /*  CREOLE parameters: what are we going to annotate, and how?  */
  private boolean                          addConstituentAnnotations;
  private boolean                          addDependencyFeatures;
  private boolean                          addDependencyAnnotations;
  private boolean                          addPosTags;


  

  /**
   * The {@link TreebankLangParserParams} implementation to use. This is
   * where we get the language pack, and then the
   * {@link GrammaticalStructureFactory} used to extract the
   * dependencies from the parse. In most cases you should leave this at
   * the default value, which is suitable for English text.
   */
  protected String tlppClass =
          "edu.stanford.nlp.parser.lexparser.EnglishTreebankParserParams";

  public String getTlppClass() {
    return tlppClass;
  }

  @CreoleParameter(comment = "Class name of the TreebankLangParserParams "
      + "implementation used to extract the dependencies",
      defaultValue = 
          "edu.stanford.nlp.parser.lexparser.EnglishTreebankParserParams")
  public void setTlppClass(String tlppClass) {
    this.tlppClass = tlppClass;
  }

  /**
   * The name of the feature to add to tokens. The feature value is a
   * {@link List} of {@link DependencyRelation} objects giving the
   * dependencies from this token to other tokens.
   */
  protected String dependenciesFeature = "dependencies";



  /**
   * Parse the current document.  (This is the principal 
   * method called by a CorpusController.)
   */
  public void execute() throws ExecutionException {
    annotationSet  = convertASName(annotationSetName);
    
    if (debugMode) {
      System.out.println("Parsing document: " + document.getName());
    }

    if (useMapping && (! mappingLoaded) ) {
      System.err.println("Warning: no mapping loaded!");
    }
    
    
    if (addConstituentAnnotations || addDependencyFeatures || addDependencyAnnotations) {
      parseSentences();
    }
    else {
      System.err.println("There is nothing for the parser to do.");
      System.err.println("Please enable at least one of the \"add...\" options.");
    }
  }

  
  /**
   * Initialize the Parser resource.  In particular, load the trained data
   * file.
   */
  public Resource init() throws ResourceInstantiationException {
    instantiateStanfordParser();
    if (mappingFile != null) {
      loadTagMapping(mappingFile);
    }
    offsetComparator = new OffsetComparator();
    super.init();
    
    if(tlppClass == null || tlppClass.equals("")) {
      throw new ResourceInstantiationException(
              "TLPP class name must be specified");
    }
    try {
      Class<?> tlppClassObj =
              Class.forName(tlppClass, true, Gate.getClassLoader());
      if(!TreebankLangParserParams.class.isAssignableFrom(tlppClassObj)) {
        throw new ResourceInstantiationException(tlppClassObj
                + " does not implement "
                + TreebankLangParserParams.class.getName());
      }
      TreebankLangParserParams tlpp =
              TreebankLangParserParams.class.cast(tlppClassObj.newInstance());
      gsf = tlpp.treebankLanguagePack().grammaticalStructureFactory();
    }
    catch(ClassNotFoundException e) {
      throw new ResourceInstantiationException("Class " + tlppClass
              + " not found", e);
    }
    catch(InstantiationException e) {
      throw new ResourceInstantiationException("Error creating TLPP object", e);
    }
    catch(IllegalAccessException e) {
      throw new ResourceInstantiationException("Error creating TLPP object", e);
    }
    return this;
  }


  /**
   * Re-initialize the Parser resource.  In particular, reload the trained
   * data file.
   */
  @Override public void reInit() throws ResourceInstantiationException {
    init();
  }  



  /**
   * Find all the Sentence annotations and iterate through them, parsing one
   * sentence at a time and storing the result in the output AS. (Sentences are
   * scanned for Tokens. You have to run the ANNIE tokenizer and splitter before
   * this PR.)
   */
  @SuppressWarnings("unchecked")
  private void parseSentences() { 
    List<Annotation> sentences = new ArrayList<Annotation>(annotationSet.get(inputSentenceType));
    java.util.Collections.sort(sentences, offsetComparator);
    Iterator<Annotation> sentenceIter = sentences.iterator();

    Tree tree;
    int debugNbrS, debugS;
    debugS = 0;
    debugNbrS = sentences.size();

    while (sentenceIter.hasNext()) {
      debugS++;
      tree = parseOneSentence(sentenceIter.next(), debugS);
      
      // Here null is the result from an empty Sentence.
      if (tree != null) {
        if (addConstituentAnnotations || addPosTags) {
          annotatePhraseStructureRecursively(tree, tree);
        }

        if (addDependencyFeatures || addDependencyAnnotations) {
          annotateDependencies(tree);
        }

        if (debugMode) {
          System.out.println("Parsed sentence " + debugS + " of " + debugNbrS);
        }
      }
      
      else if (debugMode) {
        System.out.println("Ignored empty sentence " + debugS + " of " + debugNbrS);
      }
      
    }
  }


  /**
   * Generate the special data structure for one sentence and pass the List of
   * Word to the parser.
   * 
   * @param sentence
   *          the Sentence annotation
   * @param s
   *          sentence number of debugging output
   * @param ofS
   *          total number of sentences for debugging output
   * @return  null if the sentence is empty
   */
  private Tree parseOneSentence(Annotation sentence, int sentenceNo) {
    Tree result = null;
    
    stanfordSentence = new StanfordSentence(sentence, inputTokenType, annotationSet, reusePosTags);

    /* Ignore an empty Sentence (sometimes the regex splitter can create one
     * with no Token annotations in it).
     */
    if ( stanfordSentence.isNotEmpty() ) {
      List<Word> wordList = stanfordSentence.getWordList();

      if (reusePosTags) {
        int nbrMissingTags = stanfordSentence.numberOfMissingPosTags();
        if (nbrMissingTags > 0)  {
          double percentMissing = Math.ceil(100.0 * ((float) nbrMissingTags) /
                  ((float) stanfordSentence.numberOfTokens()) );
          System.err.println("Warning (sentence " + sentenceNo + "): " + (int) percentMissing 
                  + "% of the Tokens are missing POS tags." );
        }
      }

      stanfordParser.parse(wordList);
      result = stanfordParser.getBestParse();
    }
    
    return result;
  }


  /**
   * Generate a SyntaxTreeNode Annotation corresponding to this Tree.  Work 
   * recursively so that the annotations are actually generated from the 
   * bottom up, in order to build the consists list of annotation IDs.
   * 
   * @param tree  the current subtree
   * @param rootTree  the whole sentence, used to find the span of the current subtree
   * @return a GATE Annotation of type "SyntaxTreeNode"
   */
  protected Annotation annotatePhraseStructureRecursively(Tree tree, Tree rootTree) {
    Annotation annotation = null;
    Annotation child;
    String label   = tree.value();

    List<Tree> children = tree.getChildrenAsList();

    if (children.size() == 0) {
      return null;
    }
    /* implied else */

    /* following line generates ClassCastException
     * 		IntPair span = tree.getSpan();
     * edu.stanford.nlp.ling.CategoryWordTag
     * at edu.stanford.nlp.trees.Tree.getSpan(Tree.java:393)
     * but I think it's a bug in the parser, so I'm hacking 
     * around it as follows. */
    int startPos = Trees.leftEdge(tree, rootTree);
    int endPos   = Trees.rightEdge(tree, rootTree);
    
    Long startNode = stanfordSentence.startPos2offset(startPos);
    Long endNode   = stanfordSentence.endPos2offset(endPos);

    List<Integer> consists = new ArrayList<Integer>();

    Iterator<Tree> childIter = children.iterator();
    while (childIter.hasNext()) {
      child = annotatePhraseStructureRecursively(childIter.next(), rootTree);
      if  ( (child != null)  &&
        (! child.getType().equals(inputTokenType) )) {
        consists.add(child.getId());
      }
    }
    annotation = annotatePhraseStructureConstituent(startNode, endNode, label, consists, tree.depth());

    return annotation;
  }



  /**
   * Record one constituent as an annotation.
   * 
   * @param startOffset
   * @param endOffset
   * @param label
   * @param consists
   * @param depth
   * @return
   */
  private Annotation annotatePhraseStructureConstituent(Long startOffset, Long endOffset, String label, 
    List<Integer> consists, int depth) {
    Annotation phrAnnotation = null;
    Integer phrID;

    try {
      String cat;
      if (useMapping && mappingLoaded) {
        cat  = translateTag(label);
      }
      else {
        cat = label; 
      }
      
      if (addConstituentAnnotations) {
        String text = document.getContent().getContent(startOffset, endOffset).toString();
        FeatureMap fm = gate.Factory.newFeatureMap();
        fm.put(PSG_TAG_FEATURE, cat);
        fm.put("text", text);

        /* Ignore empty list features on the token-equivalent annotations. */
        if (consists.size() > 0) {
          fm.put("consists", consists);
        }

        phrID  = annotationSet.add(startOffset, endOffset, OUTPUT_PHRASE_TYPE, fm);
        phrAnnotation = annotationSet.get(phrID);
        recordID(annotationSet, phrID);
      }

      if ( addPosTags && (depth == 1) ) {
        /* Expected to be a singleton set! */
        AnnotationSet tokenSet = annotationSet.get(inputTokenType, startOffset, endOffset);
        if (tokenSet.size() == 1) {
          Annotation token = tokenSet.iterator().next();

          /* Add POS tag to token.  
           * (Note: GATE/Hepple uses "(" and ")" for Penn/Stanford's
           * "-LRB-" and "-RRB-". */
          String hepCat = StanfordSentence.unescapePosTag(cat);
          token.getFeatures().put(POS_TAG_FEATURE, hepCat);
          
        }
        else {
          System.err.println("Found a tokenSet with " + tokenSet.size() + " members!");
        }
      }
    }
    catch (InvalidOffsetException e) {
      e.printStackTrace();
    }
    
    return phrAnnotation;
  }

  
  
  @SuppressWarnings("unchecked")
  private void annotateDependencies(Tree tree) {
    GrammaticalStructure gs = gsf.newGrammaticalStructure(tree);
    Collection<TypedDependency> deps = gs.typedDependencies();
    String dependencyKind;
    FeatureMap depFeatures;
    Integer dependentTokenID, governorTokenID;
    List<Integer> argList;
    Long offsetLH0, offsetRH0, offsetLH1, offsetRH1, depLH, depRH;
    Annotation governorToken, dependentToken;

    for(TypedDependency d : deps) {
      if(debugMode) {
        System.out.println(d);
      }

      int governorIndex = ((HasIndex) d.gov().label()).index() - 1;
      governorToken  = stanfordSentence.startPos2token(governorIndex);
      
      int dependentIndex = ((HasIndex) d.dep().label()).index() - 1;
      dependentToken = stanfordSentence.startPos2token(dependentIndex);

      dependencyKind = d.reln().toString();
      governorTokenID = governorToken.getId();
      dependentTokenID = dependentToken.getId();

      if (addDependencyFeatures) {
        List<DependencyRelation> depsForTok =
          (List<DependencyRelation>) governorToken.getFeatures().get(dependenciesFeature);

        if(depsForTok == null) {
          depsForTok = new ArrayList<DependencyRelation>();
          governorToken.getFeatures().put(dependenciesFeature, depsForTok);
        }

        depsForTok.add(new DependencyRelation(dependencyKind, dependentTokenID));
      }
      
      
      if (addDependencyAnnotations) {
        depFeatures = gate.Factory.newFeatureMap();
        argList = new ArrayList<Integer>();
        argList.add(governorTokenID);
        argList.add(dependentTokenID);
        depFeatures.put(DEP_ARG_FEATURE, argList);
        depFeatures.put(DEP_LABEL_FEATURE, dependencyKind);
      
        offsetLH0 = governorToken.getStartNode().getOffset();
        offsetRH0 = governorToken.getEndNode().getOffset();
        offsetLH1 = dependentToken.getStartNode().getOffset();
        offsetRH1 = dependentToken.getEndNode().getOffset();
      
        depLH = Math.min(offsetLH0, offsetLH1);
        depRH = Math.max(offsetRH0, offsetRH1);
      
        try {
          annotationSet.add(depLH, depRH, DEP_ANNOTATION_TYPE, depFeatures);
        }
        catch(InvalidOffsetException e) {
          e.printStackTrace();
        }
      }
    }
  }

  

  private void instantiateStanfordParser()
    throws ResourceInstantiationException {
    try {
      String filepath = Files.fileFromURL(parserFile).getAbsolutePath();
      stanfordParser = new LexicalizedParser(filepath);
    }
    catch(Exception e) {
      throw new ResourceInstantiationException(e);
    }
  }	


  private void loadTagMapping(File mappingFile)  { 
    tagMap = new HashMap<String, String>();
    mappingLoaded = false;

    try {
      if (mappingFile.exists() && mappingFile.canRead()) {

        BufferedReader br = new BufferedReader(new FileReader(mappingFile));
        String line = "";

        // read until it reaches to an end of the file
        while((line = br.readLine()) != null) {
          // two columns delimited by whitespace 
          String [] data = line.split("\\s+", 2);

          // are there key and value available
          if(data == null || data.length < 2) {
            continue;
          } else {
            // and add it to the map
            tagMap.put(data[0].trim(), data[1].trim());
          }
        }

        br.close();
      }

      else {
        System.err.println("Can't find or read mapping file " 
          + mappingFile.getPath() + " so no mappings will be used.");
      }
    } 
    catch(Exception e) {
      System.err.println("Exception trying to load mapping file "
        + mappingFile.getPath());
      e.printStackTrace();
    }

    int nbrMapped = tagMap.size();
    System.out.println("Loaded " + nbrMapped + " mappings from file " + mappingFile);
    mappingLoaded = (nbrMapped > 0);
  }


  /**
   * This method stores the annotation ID as a value of feature "ID" on the
   * relevant annotation. (Mainly to make the ID visible in the GUI for
   * debugging.)
   * 
   * @param annSet
   * @param annotationID
   */
  private void recordID(AnnotationSet annSet, Integer annotationID) {
    annSet.get(annotationID).getFeatures().put("ID", annotationID);
  }


  /**
   * Translate the tag in the map, or leave it the same if there is no
   * translation.
   * 
   * @param stanfordTag
   * @return
   */
  private String translateTag(String stanfordTag) {
    String translatedTag = stanfordTag;

    if (tagMap.containsKey(stanfordTag)) {
      translatedTag = tagMap.get(stanfordTag);
    }
    
    return translatedTag;
  }


  protected AnnotationSet convertASName(String name) {
    if ((name == null) || name.equals("") ) {
      return document.getAnnotations();
    }

    /* implied else */
    return document.getAnnotations(name);
  }


  /* get & set methods for the CREOLE parameters */

  @Optional
  @RunTime
  @CreoleParameter(comment = "annotationSet used for input (Token and "
      + "Sentence annotations) and output")
  public void setAnnotationSetName(String annotationSetName) {
    this.annotationSetName = annotationSetName;
  }

  public String getAnnotationSetName() {
    return this.annotationSetName;
  }

  @Optional
  @CreoleParameter(comment = "path to the parser's grammar file",
      defaultValue = "resources/englishPCFG.ser.gz")
  public void setParserFile(URL parserFile) {
    this.parserFile = parserFile;
  }

  public URL getParserFile() {
    return this.parserFile;
  }

  @RunTime
  @CreoleParameter(comment = "The document to be processed")
  public void setDocument(gate.Document document) {
    this.document = document;
  }

  public gate.Document getDocument() {
    return this.document;
  }

  @RunTime
  @CreoleParameter(comment = "verbose mode for debugging",
      defaultValue = "false")
  public void setDebug(Boolean debug) {
    this.debugMode = debug.booleanValue();
  }

  public Boolean getDebug() {
    return new Boolean(this.debugMode);
  }
  
  @RunTime
  @CreoleParameter(comment = "Re-use existing POS tags on tokens",
      defaultValue = "false")
  public void setReusePosTags(Boolean reusePosTags) {
    this.reusePosTags = reusePosTags.booleanValue();
  }

  public Boolean getReusePosTags() {
    return new Boolean(this.reusePosTags);
  }
  
  @RunTime
  @CreoleParameter(comment = "Create POS tags on the Token annotations",
      defaultValue = "false")
  public void setAddPosTags(Boolean posTagTokens) {
    this.addPosTags = posTagTokens.booleanValue();
  }
  
  public Boolean getAddPosTags() {
    return new Boolean(this.addPosTags);
  }

  @RunTime
  @CreoleParameter(comment = "use tag mapping",
      defaultValue = "false")
  public void setUseMapping(Boolean useMapping) {
    this.useMapping = useMapping.booleanValue();
  }
  
  public Boolean getUseMapping() {
    return new Boolean(this.useMapping);
  }
  
  @RunTime
  @CreoleParameter(comment = "Create dependency features on Token annotations",
      defaultValue = "true")
  public void setAddDependencyFeatures(Boolean useDependency) {
    this.addDependencyFeatures = useDependency.booleanValue();
  }
  
  public Boolean getAddDependencyFeatures() {
    return new Boolean(this.addDependencyFeatures);
  }
  
  @RunTime
  @CreoleParameter(comment = "Create annotations to show dependencies",
      defaultValue = "true")
  public void setAddDependencyAnnotations(Boolean useDependency) {
    this.addDependencyAnnotations = useDependency.booleanValue();
  }
  
  public Boolean getAddDependencyAnnotations() {
    return new Boolean(this.addDependencyAnnotations);
  }
  
  @RunTime
  @CreoleParameter(comment = "Create annotations to show phrase structures",
      defaultValue = "true")
  public void setAddConstituentAnnotations(Boolean usePhraseStructure) {
    this.addConstituentAnnotations = usePhraseStructure.booleanValue();
  }
  
  public Boolean getAddConstituentAnnotations() {
    return new Boolean(this.addConstituentAnnotations);
  }
  
  /* Made mappingFile an init parameter to simplify things.
   * The CREOLE parameter is called "mappingFile" but it's actually a URL.
   */
  @Optional
  @CreoleParameter(comment = "path to the tag mapping file")
  public void setMappingFile(URL mappingFileURL) {
    this.mappingFile = null; // override below
    this.mappingFileURL = mappingFileURL;

    if ( (this.mappingFileURL != null) &&
      (! this.mappingFileURL.toString().trim().equals("")) ) {
      try {
        this.mappingFile = new File(this.mappingFileURL.toURI());
      }
      catch(URISyntaxException e) {
        e.printStackTrace();
      }
    }

  }

  public URL getMappingFile() {
    return this.mappingFileURL;
  }


}
