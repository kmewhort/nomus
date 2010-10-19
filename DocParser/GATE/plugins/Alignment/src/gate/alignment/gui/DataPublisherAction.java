package gate.alignment.gui;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;

import javax.swing.table.AbstractTableModel;

import gate.Annotation;
import gate.Document;
import gate.alignment.AlignmentActionInitializationException;
import gate.alignment.AlignmentException;
import gate.compound.CompoundDocument;

/**
 * Implementers of these are resources publishes their data to the outer world.
 * 
 * @author niraj
 */
public interface DataPublisherAction {

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

  public void setDataModel(DefaultDataModel ddm);
  
  public int getColumnCount();

  public int getRowCount();

  public String getValueAt(int rowIndex, int columnIndex);

  public String getColumnName(int column);
  
  public String getTableTitle();
}
