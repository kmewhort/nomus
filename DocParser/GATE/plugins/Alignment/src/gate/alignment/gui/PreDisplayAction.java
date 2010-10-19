package gate.alignment.gui;

import gate.Annotation;
import gate.Document;
import gate.alignment.AlignmentActionInitializationException;
import gate.alignment.AlignmentException;
import gate.compound.CompoundDocument;

/**
 * Implementers of these are called just before the pair is displayed.
 * 
 * @author niraj
 */
public interface PreDisplayAction {

  /**
   * This method is called just before the pair is displayed.
   * 
   * @param editor - alignment editor
   * @param document - compound document that the alignment editor
   *          belongs to
   * @param srcDocument - a member of the compound document that has
   *          been selected as the source document
   * @param srcAS - annotation set of the source document from which to
   *          obtain annotations from.
   * @param srcAnnotation - annotation from the source document that is
   *          a parent of alignment unit being displayed
   * @param tgtDocument - a member of the compound document that has
   *          been selected as the target document
   * @param tgtAS - annotation set of the target document from which to
   *          obtain annotations from.
   * @param tgtAnnotation - annotation from the target document that is
   *          a parent of alignment unit being displayed
   * @throws AlignmentException
   */
  public void execute(AlignmentEditor editor, CompoundDocument document,
          Document srcDocument, String srcAS, Annotation srcAnnotation,
          Document tgtDocument, String tgtAS, Annotation tgtAnnotation)
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
