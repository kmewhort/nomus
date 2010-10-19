package gate.alignment.gui.actions.impl;

import java.util.Set;
import gate.Annotation;
import gate.Document;
import gate.alignment.AlignmentException;
import gate.alignment.gui.AlignmentEditor;
import gate.compound.CompoundDocument;

/**
 * It uses the highlighted annotations and dehighlights them.
 * 
 * @author niraj
 * 
 */
public class ResetAction extends AbstractAlignmentAction {
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

    editor.clearLatestAnnotationsSelection();
  }

  /**
   * @return "Reset Selection"
   */
  public String getCaption() {
    return "Reset Selection";
  }

  /**
   * @return false
   */
  public boolean invokeForAlignedAnnotation() {
    return false;
  }

  /**
   * return false
   */
  public boolean invokeForUnhighlightedUnalignedAnnotation() {
    return false;
  }

  /**
   * @return Description of the class
   */
  public String getToolTip() {
    return "Dehighlight selected annotations";
  }

}
