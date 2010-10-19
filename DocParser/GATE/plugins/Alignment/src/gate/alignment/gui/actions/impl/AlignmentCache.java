package gate.alignment.gui.actions.impl;

import gate.Annotation;
import gate.AnnotationSet;
import gate.Document;
import gate.alignment.Alignment;
import gate.alignment.AlignmentActionInitializationException;
import gate.alignment.AlignmentException;
import gate.alignment.gui.AlignmentEditor;
import gate.alignment.gui.PreDisplayAction;
import gate.alignment.gui.actions.impl.AbstractAlignmentAction;
import gate.compound.CompoundDocument;
import gate.util.GateRuntimeException;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.SortedSet;
import java.util.TreeSet;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Maintains alignment cache such that once aligned, user doesn't need to align 
 * it again.
 * @author Niraj Aswani
 *
 */
public class AlignmentCache extends AbstractAlignmentAction implements
                                                              PreDisplayAction {
  /**
   * Cache that remembers alignments done by the user.
   */
  private HashMap<String, SortedSet<String>> _cache = null;

  /**
   * feature to use for cache
   */
  private String featureToUse = "string";

  /**
   * Writer to write cache to a file
   */
  private BufferedWriter bw = null;

  int srcHighestLen = 1;

  int tgtHighestLen = 1;

  int srcMinLen = 10;

  int tgtMinLen = 10;

  /** args supplied in the actions.conf */
  String[] suppliedArgs = null;

  // is init called?
  boolean initCalled = false;

  /**
   * Init method called by the editor when alignment cache instance is created.
   */
  public void init(String[] args) throws AlignmentActionInitializationException {
    if(initCalled) return;
    initCalled = true;
    this.suppliedArgs = args;
    super.init(args);
    
    // has user provided a feature to use?
    if(args.length < 1) {
      throw new AlignmentActionInitializationException("expecting atleast one parameter: featureToUse");
    }
    
    featureToUse = args[0];
    
    // if user has provided a file with stored cache
    if(_cache == null) {
      _cache = new HashMap<String, SortedSet<String>>();
      
      // file where the cache is stored, if user has provided one
      if(args.length > 1) {
        try {
          //TODO this should be mapped to relative path
          if(new File(args[1]).exists()) {
            BufferedReader br =
                    new BufferedReader(new InputStreamReader(
                            new FileInputStream(new File(args[1])), "UTF-8"));

            String line = br.readLine();
            while(line != null) {
              if(line.trim().length() == 0 || line.trim().startsWith("#")) {
                line = br.readLine();
                continue;
              }

              String[] words = line.split("\t");
              SortedSet<String> tgtEntries = _cache.get(words[0].trim());
              if(tgtEntries == null) {
                tgtEntries = new TreeSet<String>(new Comparator<String>() {
                  public int compare(String s1, String s2) {
                    String[] s1Array = s1.split("[ ]+");
                    String[] s2Array = s2.split("[ ]+");
                    int diff = s2Array.length - s1Array.length;
                    if(diff == 0) return s2.compareTo(s1);
                    else return diff;
                  }
                });
                _cache.put(words[0], tgtEntries);
              }
              tgtEntries.add(words[1].trim());
              int srcLen = Integer.parseInt(words[2]);
              int tgtLen = Integer.parseInt(words[3]);
              if(srcLen > srcHighestLen) {
                srcHighestLen = srcLen;
              }

              if(srcMinLen > srcLen) {
                srcMinLen = srcLen;
              }

              if(tgtMinLen > tgtLen) {
                tgtMinLen = tgtLen;
              }

              if(tgtLen > tgtHighestLen) {
                tgtHighestLen = tgtLen;
              }

              line = br.readLine();
            }
            br.close();
          }
          
          // we'll rewrite this file at the end
          bw =
                  new BufferedWriter(new OutputStreamWriter(
                          new FileOutputStream(new File(args[1]), true),
                          "UTF-8"));
        } catch(IOException ioe) {
          throw new AlignmentActionInitializationException(ioe);
        }
      }
    }
  }

  private String getText(Annotation annot, Document document) {

    if(annot.getFeatures().containsKey(featureToUse)) { return annot
            .getFeatures().get(featureToUse).toString(); }

    return document.getContent().toString().substring(
            annot.getStartNode().getOffset().intValue(),
            annot.getEndNode().getOffset().intValue()).toLowerCase();
  }

  int lengthInNoOfWords = 1;

  public String getText(Set<Annotation> annotations, Document document,
          AnnotationSet set) {
    if(annotations == null || annotations.isEmpty()) return null;

    List<Annotation> annotsList = new ArrayList<Annotation>(annotations);
    Collections.sort(annotsList, new gate.util.OffsetComparator());
    String toReturn = "";
    List<Annotation> allAnnots = null;
    int indexOfAnnot = 0;

    lengthInNoOfWords = 0;

    for(int i = 0; i < annotsList.size(); i++) {
      lengthInNoOfWords++;
      Annotation annot = annotsList.get(i);
      if(i == 0) {
        allAnnots = new ArrayList<Annotation>(set.get(annot.getType()));
        Collections.sort(allAnnots, new gate.util.OffsetComparator());
        indexOfAnnot = allAnnots.indexOf(annot);
        if(annot.getFeatures().get(featureToUse) == null) {
          toReturn =
                  document.getContent().toString().substring(
                          annot.getStartNode().getOffset().intValue(),
                          annot.getEndNode().getOffset().intValue());
        } else {
          toReturn = (String)annot.getFeatures().get(featureToUse);
        }
      }

      if(i != 0) {
        int tempIndex = allAnnots.indexOf(annot);
        if(indexOfAnnot + 1 != tempIndex) {
          toReturn += " [^ ]+ ";
        } else {
          toReturn += " ";
        }
        if(annot.getFeatures().get(featureToUse) == null) {
          toReturn +=
                  document.getContent().toString().substring(
                          annot.getStartNode().getOffset().intValue(),
                          annot.getEndNode().getOffset().intValue());
        } else {
          toReturn += (String)annot.getFeatures().get(featureToUse);
        }
        indexOfAnnot = tempIndex;
      }
    }
    return toReturn;
  }

  public boolean invokeWithAlignAction() {
    return true;
  }

  public String getCaption() {
    return null;
  }

  public boolean invokeForAlignedAnnotation() {
    return false;
  }

  public boolean invokeForUnhighlightedUnalignedAnnotation() {
    return false;
  }

  public void cleanup() {
    if(bw != null) {
      try {
        bw.close();
      } catch(IOException ioe) {
        throw new GateRuntimeException(ioe);
      }
    }
  }

  public String getToolTip() {
    return "When user clicks on the Align button, "
            + "the selected annotations are recored in alignment cache"
            + " for future automatic correction.";
  }

  public void execute(AlignmentEditor editor, CompoundDocument document,
          Document srcDocument, String srcAS, Annotation srcAnnotation,
          Document tgtDocument, String tgtAS, Annotation tgtAnnotation)
          throws AlignmentException {

    // from it, obtain the alignment object
    Alignment alignment = document.getAlignmentInformation(editor.getAlignmentFeatureName());

    List<Annotation> srcTokens = null;
    List<Annotation> tgtTokens = null;

    AnnotationSet sAS = srcAS == null ? srcDocument.getAnnotations() : srcDocument.getAnnotations(srcAS);
    AnnotationSet tAS = srcAS == null ? tgtDocument.getAnnotations() : tgtDocument.getAnnotations(tgtAS);
    
    List<Annotation> srcSet = new ArrayList<Annotation>(sAS.getContained(srcAnnotation.getStartNode().getOffset(), srcAnnotation.getEndNode().getOffset()).get(editor.getSourceUnitOfAlignment()));
    List<Annotation> tgtSet = new ArrayList<Annotation>(tAS.getContained(tgtAnnotation.getStartNode().getOffset(), tgtAnnotation.getEndNode().getOffset()).get(editor.getTargetUnitOfAlignment()));

    srcTokens = new ArrayList<Annotation>(srcSet);
    Collections.sort(srcTokens, new gate.util.OffsetComparator());
    tgtTokens = new ArrayList<Annotation>(tgtSet);
    Collections.sort(tgtTokens, new gate.util.OffsetComparator());

    HashMap<String, Annotation> sstOffsets = new HashMap<String, Annotation>();
    HashMap<String, Annotation> senOffsets = new HashMap<String, Annotation>();
    HashMap<String, Annotation> tstOffsets = new HashMap<String, Annotation>();
    HashMap<String, Annotation> tenOffsets = new HashMap<String, Annotation>();

    String srcString = "";
    for(int i = 0; i < srcTokens.size(); i++) {
      Annotation annot = srcTokens.get(i);
      srcString += (i == 0 ? "" : " ");
      sstOffsets.put(srcString.length() + "", annot);
      srcString += getText(annot, srcDocument);
      senOffsets.put(srcString.length() + "", annot);
    }

    String tgtString = "";
    for(int i = 0; i < tgtTokens.size(); i++) {
      Annotation annot = tgtTokens.get(i);
      tgtString += (i == 0 ? "" : " ");
      tstOffsets.put(tgtString.length() + "", annot);
      tgtString += getText(annot, tgtDocument);
      tenOffsets.put(tgtString.length() + "", annot);
    }

    for(String s : _cache.keySet()) {
      List<Set<Annotation>> srcGroups = new ArrayList<Set<Annotation>>();
      List<Set<Annotation>> tgtGroups = new ArrayList<Set<Annotation>>();

      Pattern p = Pattern.compile(s, Pattern.UNICODE_CASE);
      Matcher m = p.matcher(srcString);
      
      int startIndex = 0;

      //there can be multiple matches
      whileLoop:while(m.find(startIndex)) {
        
        int start = m.start(0);
        int end = start + m.group(0).length();

        startIndex = end;
        // find the firstToken using start as a startOffset
        // find the lastToken using end as a endOffset
        Annotation aFirstToken = sstOffsets.get("" + start);
        if(aFirstToken == null) continue;

        Annotation aLastToken = senOffsets.get("" + end);
        if(aLastToken == null) continue;

        // if any of the tokens are aligned, continue with the next match
        int sst = srcTokens.indexOf(aFirstToken);
        int sen = srcTokens.indexOf(aLastToken);

        Set<Annotation> sGrp = new HashSet<Annotation>();
        
        for(int k = sst; k <= sen; k++) {
          Annotation sAnnot = srcTokens.get(k);
          if(alignment.isAnnotationAligned(sAnnot)) continue whileLoop;
          sGrp.add(sAnnot);
        }
        srcGroups.add(sGrp);
      }

      // multiple target strings for the source string
      Set<String> tStrings = _cache.get(s);
      for(String t : tStrings) {
        Pattern tPat = Pattern.compile(t, Pattern.UNICODE_CASE);
        Matcher tMat = tPat.matcher(tgtString);
        int tStartIndex = 0;
        
        whileLoop: while(tMat.find(tStartIndex)) {
          int tStart = tMat.start(0);
          int tEnd = tStart + tMat.group(0).length();
          tStartIndex = tEnd;

          Annotation tFirstToken = tstOffsets.get("" + tStart);
          Annotation tLastToken = tenOffsets.get("" + tEnd);
          if(tFirstToken == null || tLastToken == null) {
            tStartIndex = tStart + 1;
            continue whileLoop;
          }

          int tst = tgtTokens.indexOf(tFirstToken);
          int ten = tgtTokens.indexOf(tLastToken);
          
          Set<Annotation> tGrp = new HashSet<Annotation>();
          
          for(int k = tst; k <= ten; k++) {
            Annotation tAnnot = tgtTokens.get(k);
            if(alignment.isAnnotationAligned(tAnnot)) continue whileLoop;
            tGrp.add(tAnnot);
          }

          tgtGroups.add(tGrp);
        }
      }
      
      for(Set<Annotation> sGrp : srcGroups) {
        for(Set<Annotation> tGrp : tgtGroups) {
          for(Annotation sAnn : sGrp) {
            sAnn.getFeatures().put(Alignment.ALIGNMENT_METHOD_FEATURE_NAME, "_cache");
            for(Annotation tAnn : tGrp) {
              alignment.align(sAnn, srcAS, srcDocument, tAnn, tgtAS, tgtDocument);
              tAnn.getFeatures().put(Alignment.ALIGNMENT_METHOD_FEATURE_NAME, "_cache");
            }
          }
        }
      }
    }
    
  }

  public void execute(AlignmentEditor editor, CompoundDocument document,
          Document srcDocument, String srcAS,
          Set<Annotation> srcAlignedAnnotations, Document tgtDocument,
          String tgtAS, Set<Annotation> tgtAlignedAnnotations,
          Annotation clickedAnnotation) throws AlignmentException {
    if(srcAlignedAnnotations == null || srcAlignedAnnotations.isEmpty())
      return;
    if(tgtAlignedAnnotations == null || tgtAlignedAnnotations.isEmpty())
      return;

    AnnotationSet sAS = srcAS == null ? srcDocument.getAnnotations() : srcDocument.getAnnotations(srcAS);
    String sourceText = getText(srcAlignedAnnotations, srcDocument, sAS);
    int srcLen = lengthInNoOfWords;
    if(srcMinLen > srcLen) {
      srcMinLen = srcLen;
    }

    AnnotationSet tAS = srcAS == null ? tgtDocument.getAnnotations() : tgtDocument.getAnnotations(tgtAS);
    String targetText = getText(tgtAlignedAnnotations, tgtDocument, tAS);
    int tgtLen = lengthInNoOfWords;
    if(tgtMinLen > tgtLen) {
      tgtMinLen = tgtLen;
    }

    if(_cache.containsKey(sourceText)) {
      if(_cache.get(sourceText).contains(targetText)) {
        return; 
      }
    }

    SortedSet<String> sourceTexts = _cache.get(sourceText);
    if(sourceTexts == null) {
      sourceTexts = new TreeSet<String>(new Comparator<String>() {
        public int compare(String s1, String s2) {
          String[] s1Array = s1.split("[ ]+");
          String[] s2Array = s2.split("[ ]+");
          int diff = s2Array.length - s1Array.length;
          if(diff == 0) return s2.compareTo(s1);
          else return diff;
        }
      });
      _cache.put(sourceText, sourceTexts);
    }
    
    if(!sourceTexts.contains(targetText)) {
      sourceTexts.add(targetText);  
      try {
        if(bw == null) {
          init(suppliedArgs);
        }
        
        bw.write(sourceText + "\t" + targetText + "\t" + srcLen + "\t" + tgtLen);
        bw.newLine();
        bw.flush();
      } catch(IOException ioe) {
        throw new AlignmentException(ioe);
      } catch(AlignmentActionInitializationException e) {
        throw new AlignmentException(e);
      }
    }
  }
}