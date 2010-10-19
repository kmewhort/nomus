package gate.alignment.gui.actions.impl;

import java.util.Set;
import gate.Annotation;
import gate.Document;
import gate.alignment.Alignment;
import gate.alignment.AlignmentException;
import gate.alignment.gui.AlignmentEditor;
import gate.compound.CompoundDocument;

/**
 * It uses the highlighted annotations from the editor and unaligns them
 * with one other.
 * 
 * @author niraj
 * 
 */
public class RemoveAlignmentAction extends AbstractAlignmentAction {

  /**
   * non-javadoc
   * 
   * @see AlignmentAction.execute(...)
   */
  public void execute(AlignmentEditor editor, CompoundDocument document,
          Document srcDocument, String srcAS,
          Set<Annotation> srcAlignedAnnotations, Document tgtDocument,
          String tgtAS, Set<Annotation> tgtAlignedAnnotations,
          Annotation clickedAnnotation) throws AlignmentException {

    // alignment object
    Alignment alignment = document.getAlignmentInformation(editor
            .getAlignmentFeatureName());
    if(srcAlignedAnnotations == null || srcAlignedAnnotations.isEmpty())
      return;
    if(tgtAlignedAnnotations == null || tgtAlignedAnnotations.isEmpty())
      return;
    for(Annotation srcAnnotation : srcAlignedAnnotations) {
      for(Annotation tgtAnnotation : tgtAlignedAnnotations) {
        if(alignment.areTheyAligned(srcAnnotation, tgtAnnotation)) {
          alignment.unalign(srcAnnotation, srcAS, srcDocument, tgtAnnotation,
                  tgtAS, tgtDocument);

          if(alignment.getAlignedAnnotations(srcAnnotation).size() == 0) {
            srcAnnotation.getFeatures().remove(
                    Alignment.ALIGNMENT_METHOD_FEATURE_NAME);
          }

          if(alignment.getAlignedAnnotations(tgtAnnotation).size() == 0) {
            tgtAnnotation.getFeatures().remove(
                    Alignment.ALIGNMENT_METHOD_FEATURE_NAME);
          }
        }
      }
    }
    editor.clearLatestAnnotationsSelection();
  }

  /**
   * @return "Remove Alignment"
   */
  public String getCaption() {
    return "Remove Alignment";
  }

  /**
   * @return false
   */
  public boolean invokeForHighlightedUnalignedAnnotation() {
    return false;
  }

  /**
   * @return false
   */
  public boolean invokeForUnhighlightedUnalignedAnnotation() {
    return false;
  }

  /**
   * Description of the class
   */
  public String getToolTip() {
    return "Removes the alignment for selected annotations";
  }

}
