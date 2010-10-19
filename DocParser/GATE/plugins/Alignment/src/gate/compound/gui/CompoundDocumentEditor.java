package gate.compound.gui;

import java.awt.BorderLayout;
import java.awt.Component;
import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;
import java.io.File;
import java.util.*;

import javax.swing.*;

import gate.*;
import gate.compound.CompoundDocument;
import gate.compound.CompoundDocumentEvent;
import gate.compound.CompoundDocumentListener;
import gate.compound.impl.AbstractCompoundDocument;
import gate.corpora.DocumentImpl;
import gate.creole.*;
import gate.event.ProgressListener;
import gate.gui.ActionsPublisher;
import gate.gui.Handle;
import gate.gui.MainFrame;
import gate.gui.NameBearerHandle;
import gate.util.GateException;
import gate.util.GateRuntimeException;

import java.io.*;

/**
 * This is an extention of the GATE Document viewer/editor. This class
 * provides the implementation for CompoundDocument Editor. Compound
 * document is a set of multiple documents. this class simply wrapps all
 * document editors for all compound document's member documents under a
 * single component.
 */

public class CompoundDocumentEditor extends AbstractVisualResource
                                                                  implements
                                                                  ActionsPublisher,
                                                                  ProgressListener,
                                                                  CompoundDocumentListener {

  private static final long serialVersionUID = -7623216613025540025L;

  private JTabbedPane tabbedPane;

  private HashMap<String, Handle> documentsMap;

  protected JToolBar toolbar;

  protected NewDocumentAction newDocumentAction;

  protected RemoveDocumentsAction removeDocumentsAction;

  /**
   * The document view is just an empty shell. This method publishes the
   * actions from the contained views.
   */
  public List getActions() {
    List actions = new ArrayList();
    actions.add(new SaveAllDocuments());
    actions.add(new SaveAsASingleXML());
    actions.add(new SwitchDocument());
    actions.add(new LoadFromXML());
    actions.add(new PopulateCorpus());
    actions.add(new PopulateCorpusFromXML());
    return actions;
  }

  /*
   * (non-Javadoc)
   * 
   * @see gate.Resource#init()
   */
  public Resource init() throws ResourceInstantiationException {
    tabbedPane = new JTabbedPane();
    documentsMap = new HashMap<String, Handle>();
    toolbar = new JToolBar();
    toolbar.setFloatable(false);
    toolbar.add(newDocumentAction = new NewDocumentAction());
    toolbar.add(removeDocumentsAction = new RemoveDocumentsAction());

    this.setLayout(new java.awt.BorderLayout());
    this.add(tabbedPane, java.awt.BorderLayout.CENTER);
    this.add(toolbar, BorderLayout.NORTH);
    return this;
  }

  /*
   * (non-Javadoc)
   * 
   * @see gate.VisualResource#setTarget(java.lang.Object)
   */
  public void setTarget(Object target) {
    this.document = (Document)target;
  }

  /**
   * Used by the main GUI to tell this VR what handle created it. The
   * VRs can use this information e.g. to add items to the popup for the
   * resource.
   */
  public void setHandle(Handle handle) {
    super.setHandle(handle);
    Map documents = ((CompoundDocument)this.document).getDocuments();
    ((CompoundDocument)this.document).addCompoundDocumentListener(this);

    Iterator iter = documents.values().iterator();
    try {
      while(iter.hasNext()) {
        Document doc = (Document)iter.next();
        NameBearerHandle nbHandle = new NameBearerHandle(doc, Main
                .getMainFrame());
        JComponent largeView = nbHandle.getLargeView();
        if(largeView != null) {
          tabbedPane.addTab(nbHandle.getTitle(), nbHandle.getIcon(), largeView,
                  nbHandle.getTooltipText());
          documentsMap.put(doc.getName(), nbHandle);

        }
      }
    }
    catch(Exception e) {
      throw new RuntimeException(e);
    }
  }

  class NewDocumentAction extends AbstractAction {
    public NewDocumentAction() {
      super("Add document", MainFrame.getIcon("add-document"));
      putValue(SHORT_DESCRIPTION,
              "Add new document(s) to this compound document");
      putValue(MNEMONIC_KEY, KeyEvent.VK_ENTER);
    }

    public void actionPerformed(ActionEvent e) {
      try {
        // get all the documents loaded in the system
        java.util.List loadedDocuments = Gate.getCreoleRegister()
                .getAllInstances("gate.Document");
        if(loadedDocuments == null || loadedDocuments.isEmpty()) {
          JOptionPane.showMessageDialog(CompoundDocumentEditor.this,
                  "There are no documents available in the system.\n"
                          + "Please load some and try again.", "GATE",
                  JOptionPane.ERROR_MESSAGE);
          return;
        }

        Vector docNames = new Vector();
        for(int i = 0; i < loadedDocuments.size(); i++) {
          Document doc = (Document)loadedDocuments.get(i);
          if(doc instanceof CompoundDocument) {
            loadedDocuments.remove(i);
            i--;
            continue;
          }
          docNames.add(doc.getName());
        }

        JList docList = new JList(docNames);
        JOptionPane dialog = new JOptionPane(new JScrollPane(docList),
                JOptionPane.QUESTION_MESSAGE, JOptionPane.OK_CANCEL_OPTION);
        dialog.createDialog(CompoundDocumentEditor.this,
                "Add document(s) to compound document").setVisible(true);

        if(((Integer)dialog.getValue()).intValue() == JOptionPane.OK_OPTION) {
          int[] selection = docList.getSelectedIndices();
          for(int i = 0; i < selection.length; i++) {
            Document doc = (Document)loadedDocuments.get(selection[i]);
            ((CompoundDocument)document).addDocument(doc.getName(), doc);
          }
        }
      }
      catch(GateException ge) {
        // gate.Document is not registered in creole.xml....what is!?
        throw new GateRuntimeException(
                "gate.Document is not registered in the creole register!\n"
                        + "Something must be terribly wrong...take a vacation!");
      }
    }
  }

  class RemoveDocumentsAction extends AbstractAction {
    public RemoveDocumentsAction() {
      super("Remove documents", MainFrame.getIcon("remove-document"));
      putValue(SHORT_DESCRIPTION,
              "Removes selected document(s) from this corpus");
      putValue(MNEMONIC_KEY, KeyEvent.VK_DELETE);
    }

    public void actionPerformed(ActionEvent e) {
      if(tabbedPane.getSelectedIndex() >= 0) {
        String docName = tabbedPane.getTitleAt(tabbedPane.getSelectedIndex());
        ((CompoundDocument)document).removeDocument(docName);
      }
    }
  }

  class SaveAllDocuments extends AbstractAction {

    private static final long serialVersionUID = -1377052643002026640L;

    public SaveAllDocuments() {
      super("Save All Documents As XML!");
    }

    public void actionPerformed(ActionEvent ae) {
      CompoundDocument cd = (CompoundDocument)document;
      JFileChooser fileChooser = new JFileChooser();
      fileChooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
      try {
        fileChooser.showSaveDialog(Main.getMainFrame());
        File dir = null;
        if((dir = fileChooser.getSelectedFile()) == null) {
          return;
        }

        List<String> docIDs = cd.getDocumentIDs();
        for(int i = 0; i < docIDs.size(); i++) {
          Document doc = cd.getDocument(docIDs.get(i));
          File file = null;
          if(doc.getName().equals("Composite")) {
            file = new File(dir.getAbsolutePath() + "/Composite.xml");
          }
          else {
            file = new File(doc.getSourceUrl().getFile());
            file = new File(dir.getAbsolutePath() + "/" + file.getName());
          }

          BufferedWriter bw = new BufferedWriter(
                  new OutputStreamWriter(new FileOutputStream(file),
                          ((DocumentImpl)doc).getEncoding()));
          bw.write(doc.toXml());
          bw.flush();
          bw.close();
        }
      }
      catch(Exception e) {
        e.printStackTrace();
      }
    }
  }

  class LoadFromXML extends AbstractAction {

    private static final long serialVersionUID = -1377052643002026640L;

    public LoadFromXML() {
      super("Compound Document from XML");
    }

    public void actionPerformed(ActionEvent ae) {
      CompoundDocument cd = (CompoundDocument)document;

      JFileChooser fileChooser = new JFileChooser();
      fileChooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
      try {
        fileChooser.showOpenDialog(Main.getMainFrame());
        File fileToOpen = null;
        if((fileToOpen = fileChooser.getSelectedFile()) == null) {
          return;
        }

        StringBuilder xmlString = new StringBuilder();
        BufferedReader br = new BufferedReader(new InputStreamReader(
                new FileInputStream(fileToOpen), "utf-8"));
        String line = br.readLine();
        while(line != null) {
          xmlString.append("\n").append(line);
          line = br.readLine();
        }
        AbstractCompoundDocument.fromXml(xmlString.toString());
        br.close();
      }
      catch(Exception e) {
        e.printStackTrace();
      }
    }
  }

  class PopulateCorpusFromXML extends AbstractAction {

    private static final long serialVersionUID = -1377052643002026640L;

    public PopulateCorpusFromXML() {
      super("Populate Corpus From XML");
    }

    public void actionPerformed(ActionEvent ae) {
      JFileChooser fileChooser = new JFileChooser();
      fileChooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
      try {
        fileChooser.showOpenDialog(Main.getMainFrame());
        File fileToOpen = null;
        if((fileToOpen = fileChooser.getSelectedFile()) == null) {
          return;
        }

        File[] files = fileToOpen.listFiles(new FilenameFilter() {
          public boolean accept(File dir, String name) {
            if(name.endsWith(".xml")) {
              return true;
            }
            return false;
          }
        });

        // file to Open is a directory
        String corpusName = JOptionPane.showInputDialog("Enter CorpusName");
        Corpus corpusToUse = Factory.newCorpus(corpusName);

        for(File aFile : files) {
          StringBuilder xmlString = new StringBuilder();
          BufferedReader br = new BufferedReader(new InputStreamReader(
                  new FileInputStream(aFile), "utf-8"));
          String line = br.readLine();
          while(line != null) {
            xmlString.append("\n").append(line);
            line = br.readLine();
          }
          CompoundDocument doc = AbstractCompoundDocument.fromXml(xmlString
                  .toString());
          corpusToUse.add(doc);
          br.close();
        }
      }
      catch(Exception e) {
        e.printStackTrace();
      }
    }
  }

  class PopulateCorpus extends AbstractAction {

    private static final long serialVersionUID = -1377052643002026640L;

    public PopulateCorpus() {
      super("Populate Corpus With Compound Documents");
    }

    public void actionPerformed(ActionEvent ae) {
      JFileChooser fileChooser = new JFileChooser();
      fileChooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
      try {
        fileChooser.showOpenDialog(Main.getMainFrame());
        File fileToOpen = null;
        if((fileToOpen = fileChooser.getSelectedFile()) == null) {
          return;
        }

        // file to Open is a directory
        String corpusName = JOptionPane.showInputDialog("Enter CorpusName");
        Corpus corpusToUse = Factory.newCorpus(corpusName);

        // file to Open is a directory
        String langCodes = JOptionPane
                .showInputDialog("Enter language codes (comma separated)");
        final String[] codes = langCodes.split(",");

        File[] files = fileToOpen.listFiles(new FilenameFilter() {
          public boolean accept(File dir, String name) {
            if(name.indexOf("." + codes[0] + ".") > 0) {
              return true;
            }
            return false;
          }
        });

        List<String> docIds = new ArrayList<String>();
        for(String code : codes) {
          docIds.add(code);
        }

        for(File aFile : files) {
          FeatureMap fets = Factory.newFeatureMap();
          fets.put("sourceUrl", aFile.toURL());
          fets.put("documentIDs", docIds);
          fets.put("encoding", "UTF-8");
          CompoundDocument cd = (CompoundDocument)Factory.createResource(
                  "gate.compound.impl.CompoundDocumentImpl", fets);
          corpusToUse.add(cd);
        }
      }
      catch(Exception e) {
        e.printStackTrace();
      }
    }
  }

  public class SaveAsASingleXML extends AbstractAction {

    private static final long serialVersionUID = -1377052643002026640L;

    public SaveAsASingleXML() {
      super("Save in a single XML Document");
    }

    public void actionPerformed(ActionEvent ae) {
      CompoundDocument cd = (CompoundDocument)document;

      JFileChooser fileChooser = new JFileChooser();
      fileChooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
      try {
        fileChooser.showSaveDialog(Main.getMainFrame());
        File fileToSaveIn = null;
        if((fileToSaveIn = fileChooser.getSelectedFile()) == null) {
          return;
        }

        String xml = AbstractCompoundDocument.toXmlAsASingleDocument(cd);
        BufferedWriter bw = new BufferedWriter(new OutputStreamWriter(
                new FileOutputStream(fileToSaveIn), cd.getEncoding()));
        bw.write(xml);
        bw.flush();
        bw.close();
      }
      catch(Exception e) {
        e.printStackTrace();
      }
    }
  }

  class SwitchDocument extends AbstractAction {

    private static final long serialVersionUID = -1377052643002026640L;

    public SwitchDocument() {
      super("Switch Document");
    }

    public void actionPerformed(ActionEvent ae) {
      CompoundDocument cd = (CompoundDocument)document;
      List<String> docIDs = cd.getDocumentIDs();
      JComboBox box = new JComboBox(docIDs.toArray());
      Object[] options = {"OK", "CANCEL"};
      int reply = JOptionPane.showOptionDialog(MainFrame.getInstance(), box,
              "Select the document ID to switch to...",
              JOptionPane.OK_CANCEL_OPTION, JOptionPane.QUESTION_MESSAGE, null,
              options, options[0]);
      if(reply == JOptionPane.OK_OPTION) {
        String documentID = (String)box.getSelectedItem();
        ((CompoundDocument)document).setCurrentDocument(documentID);
      }
    }
  }

  protected Document document;

  public void processFinished() {
    ((CompoundDocument)this.document).setCurrentDocument("null");
  }

  public void progressChanged(int prgress) {

  }

  public void documentAdded(CompoundDocumentEvent event) {
    try {
      Document doc = event.getSource().getDocument(event.getDocumentID());
      final NameBearerHandle nbHandle = new NameBearerHandle(doc, Main
              .getMainFrame());
      final JComponent largeView = nbHandle.getLargeView();
      if(largeView != null) {
        SwingUtilities.invokeLater(new Runnable() {
          public void run() {
            tabbedPane.addTab(nbHandle.getTitle(), nbHandle.getIcon(),
                    largeView, nbHandle.getTooltipText());
          }
        });
        documentsMap.put(doc.getName(), nbHandle);
      }
      tabbedPane.updateUI();
    }
    catch(Exception e) {
      e.printStackTrace();
    }
  }

  public void documentRemoved(CompoundDocumentEvent event) {
    Handle handle = (Handle)documentsMap.remove(event.getDocumentID());
    if(handle != null) {
      tabbedPane.remove(handle.getLargeView());
      tabbedPane.updateUI();
      // clean up the document's VRs
      handle.cleanup();
      Document doc = event.getSource().getDocument(event.getDocumentID());
      if(Gate.getHiddenAttribute(doc.getFeatures())) {
        Factory.deleteResource(event.getSource().getDocument(
                event.getDocumentID()));
      }
    }
  }
  
  /**
   * Clean up the VRs for our component documents.
   */
  public void cleanup() {
    for(Handle h : documentsMap.values()) {
      tabbedPane.remove(h.getLargeView());
      h.cleanup();
    }
  }

}
