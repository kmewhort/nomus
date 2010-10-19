package gate.alignment.gui;

import java.util.Set;

import gate.Annotation;
import gate.Document;
import gate.alignment.AlignmentActionInitializationException;
import gate.alignment.AlignmentException;
import gate.compound.CompoundDocument;

/**
 * Implementers of these are called when user says that the alignment is
 * finished.
 * 
 * @author niraj
 */
public interface FinishedAlignmentAction {

  /**
   * This method is called when user says that the alignment is
   * finished.
   * 
   * @param editor - alignment editor
   * @param document - compound document that this alignment editor
   *          belongs to.
   * @param srcDocument - a member of the compound document that is
   *          selected as the source document.
   * @param srcAS - annotation set of the source document from which to
   *          obtain annotations from.
   * @param srcAnnotations - annotations of the current pair that is
   *          being aligned and belong to the source document.
   * @param tgtDocument - a member of the compound document that is
   *          selected as the target document.
   * @param tgtAS - annotation set of the target document from which to
   *          obtain annotations from.
   * @param tgtAnnotations - annotations of the current pair that is
   *          being aligned and belong to the target document.
   * @throws AlignmentException
   */
  public void execute(AlignmentEditor editor, CompoundDocument document,
          Document srcDocument, String srcAS, Set<Annotation> srcAnnotations,
          Document tgtDocument, String tgtAS, Set<Annotation> tgtAnnotations)
          throws AlignmentException;

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

}
