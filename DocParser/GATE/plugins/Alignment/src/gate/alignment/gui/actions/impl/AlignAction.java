package gate.alignment.gui.actions.impl;

import java.util.Set;
import gate.Annotation;
import gate.Document;
import gate.alignment.Alignment;
import gate.alignment.AlignmentException;
import gate.alignment.gui.AlignmentEditor;
import gate.compound.CompoundDocument;

/**
 * It uses the highlighted annotations from the editor and aligns them
 * with one other.
 * 
 * @author niraj
 * 
 */
public class AlignAction extends AbstractAlignmentAction {

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

    // so first of all clear the latestSelection
    editor.clearLatestAnnotationsSelection();

    if(srcAlignedAnnotations == null || srcAlignedAnnotations.isEmpty())
      return;
    if(tgtAlignedAnnotations == null || tgtAlignedAnnotations.isEmpty())
      return;
    for(Annotation srcAnnotation : srcAlignedAnnotations) {
      for(Annotation tgtAnnotation : tgtAlignedAnnotations) {
        if(!alignment.areTheyAligned(srcAnnotation, tgtAnnotation)) {

          if(!alignment.isAnnotationAligned(srcAnnotation)) {
            srcAnnotation.getFeatures().put(
                    Alignment.ALIGNMENT_METHOD_FEATURE_NAME, "manual");
          }

          if(!alignment.isAnnotationAligned(tgtAnnotation)) {
            tgtAnnotation.getFeatures().put(
                    Alignment.ALIGNMENT_METHOD_FEATURE_NAME, "manual");
          }

          alignment.align(srcAnnotation, srcAS, srcDocument, tgtAnnotation,
                  tgtAS, tgtDocument);

        }
      }
    }
  }

  /**
   * @return "Align"
   */
  public String getCaption() {
    return "Align";
  }

  /**
   * @return false
   */
  public boolean invokeForAlignedAnnotation() {
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
    return "Aligns the selected source and target annotations";
  }
}
