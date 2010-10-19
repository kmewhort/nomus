package gate.alignment.gui;

import javax.swing.table.AbstractTableModel;

public class DefaultDataModel extends AbstractTableModel {
  private DataPublisherAction dpa;

  public DefaultDataModel(DataPublisherAction dpa) {
    this.dpa = dpa;
  }

  public int getColumnCount() {

    return dpa.getColumnCount();
  }

  public int getRowCount() {
    return dpa.getRowCount();
  }

  public Object getValueAt(int rowIndex, int columnIndex) {
    return dpa.getValueAt(rowIndex, columnIndex);
  }

  @Override
  public Class<?> getColumnClass(int columnIndex) {
    return String.class;

  }

  @Override
  public String getColumnName(int column) {
    return dpa.getColumnName(column);
  }

  @Override
  public boolean isCellEditable(int rowIndex, int columnIndex) {
    return false;
  }
}
