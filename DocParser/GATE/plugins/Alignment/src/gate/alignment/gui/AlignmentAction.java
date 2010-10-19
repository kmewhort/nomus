package gate.alignment.gui;

import java.util.Set;
import gate.Annotation;
import gate.Document;
import gate.alignment.AlignmentActionInitializationException;
import gate.alignment.AlignmentException;
import gate.compound.CompoundDocument;

import javax.swing.Icon;

/**
 * This interface represents an AlignmentAction. AlignAction,
 * ResetAction and RemoveAlignmentAction are examples of the
 * AlignmentAction. In other words, it represents an action that could
 * take place after the pair is displayed and before the user specifies
 * to change his/her focus onto a new sentence pair. If not one of the
 * above three, such an action could be executed with any of the
 * AlignAction or RemoveAlignmentAction.
 * 
 * @author niraj
 * 
 */
public interface AlignmentAction {

  /**
   * This method is invoked whenever users click on the "Align" button
   * in the alignment editor gui.
   * 
   * @param editor - alignment editor
   * @param document - compound document, the alignment editor belongs
   *          to
   * @param srcDocument - a member of the compound document that has
   *          been selected as a source document.
   * @param srcAS - annotation set to which the annotations being
   *          aligned belong to.
   * @param srcAlignedAnnotations - annotations from the source document
   *          that are being aligned.
   * @param tgtDocument - a member of the compound document that has
   *          been selected as a target document.
   * @param tgtAS - annotation set to which the annotations being
   *          aligned belong to.
   * @param tgtAlignedAnnotations - annotations from the target document
   *          that are being aligned.
   * @param clickedAnnotation - the last annotation on which the user
   *          had right clicked to invoke and execute this action.
   * @throws AlignmentException
   */
  public void execute(AlignmentEditor editor, CompoundDocument document,
          Document srcDocument, String srcAS,
          Set<Annotation> srcAlignedAnnotations, Document tgtDocument,
          String tgtAS, Set<Annotation> tgtAlignedAnnotations,
          Annotation clickedAnnotation) throws AlignmentException;

  /**
   * Keep this null in order to be called along with the default align
   * action. Otherwise it is shown with this title under the options tab
   * in the alignment editor.
   * 
   * @return
   */
  public String getCaption();

  /**
   * Keep this null in order to be called along with the default align
   * action. Otherwise it is shown with this icon under the options tab
   * in the alignment editor.
   * 
   * @return
   */
  public Icon getIcon();

  /**
   * Icon's absolte path on the filesystem.
   * 
   * @return
   */
  public String getIconPath();

  /**
   * Indicates if this action should be displayed when user right clicks
   * on an aligned annotation.
   * 
   * @return
   */
  public boolean invokeForAlignedAnnotation();

  /**
   * Indicates if this action should be displayed when user right clicks
   * on an annotation that is highlighted but is not aligned.
   * 
   * @return
   */
  public boolean invokeForHighlightedUnalignedAnnotation();

  /**
   * indicates if this action should be displayed when user right clicks
   * on an annotation that is not highlighted and is not aligned.
   * 
   * @return
   */
  public boolean invokeForUnhighlightedUnalignedAnnotation();

  /**
   * This method should be used for initializing any resources required
   * by the execute() method. This method is called whenever it loaded
   * for the first time.
   * 
   * @param args
   * @throws AlignmentActionInitializationException
   */
  public void init(String[] args) throws AlignmentActionInitializationException;

  /**
   * This method should free up the memory by releasing any resources
   * occupied this method. It is called just before the alignment editor
   * is closed.
   */
  public void cleanup();

  /**
   * Indicates if this action should be called along with the default
   * align action.
   * 
   * @return
   */
  public boolean invokeWithAlignAction();

  /**
   * Indicates if this action should be called along with the unalign
   * action.
   * 
   * @return
   */
  public boolean invokeWithRemoveAction();

  /**
   * A tooltip to show whenever user puts his/her mouse on the
   * caption/icon of this action in the alignment editor.
   * 
   * @return
   */
  public String getToolTip();
}
