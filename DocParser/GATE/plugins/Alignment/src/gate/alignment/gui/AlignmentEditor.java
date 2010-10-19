package gate.alignment.gui;

import java.util.*;
import java.util.List;
import java.util.Timer;

import javax.swing.*;
import javax.swing.border.TitledBorder;
import javax.swing.event.MouseInputAdapter;
import javax.swing.table.DefaultTableModel;

import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseEvent;
import java.awt.geom.Line2D;
import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.net.MalformedURLException;
import java.net.URISyntaxException;
import java.net.URL;

import gate.*;
import gate.alignment.*;
import gate.alignment.gui.actions.impl.AlignAction;
import gate.alignment.gui.actions.impl.RemoveAlignmentAction;
import gate.alignment.gui.actions.impl.ResetAction;
import gate.compound.CompoundDocument;
import gate.compound.impl.AbstractCompoundDocument;
import gate.creole.*;
import gate.gui.MainFrame;
import gate.swing.ColorGenerator;
import gate.swing.XJTable;
import gate.util.GateException;
import gate.util.GateRuntimeException;

/**
 * This class provides an editor for aligning texts in a compound
 * document.
 */
public class AlignmentEditor extends AbstractVisualResource implements
                                                           ActionListener,
                                                           AlignmentListener {

  private static final long serialVersionUID = -2867467022258265114L;

  /**
   * default actions config file.
   */
  private static final String ACTIONS_CONFIG_FILE = "actions.conf";

  /**
   * scrollpane that holds all the sourcePanel, targetPanel and the
   * linesCanvas
   */
  private JScrollPane waScrollPane;

  /**
   * text field that allows users to input the class name for iterating
   * method
   */
  private JTextField iteratingMethodTF;

  /**
   * panel with source document settings
   */
  private JPanel sourceDocPanel;

  /**
   * panel with target document settings
   */
  private JPanel targetDocPanel;

  /**
   * panel with iteration and alignment feature options
   */
  private JPanel iteratingPanel;

  /**
   * panel with various buttons
   */
  private JPanel populateButtonsPanel;

  /**
   * main split pane, that contains settings and the actual alignment
   * gui
   */
  private JSplitPane mainPanel;

  /**
   * panel with settings related options
   */
  private JPanel paramPanel;

  /**
   * panel with word alignment GUI components
   */
  private JPanel waPanel;

  /**
   * properties panel that shows various options available to user when
   * aligning
   */
  private JPanel propertiesPanel;

  /**
   * holds tabbed panes
   */
  private JTabbedPane tableTabbedPane;

  /**
   * source panel that has labels for each individual alignment unit in
   * the source parent of alignment unit
   */
  private JPanel sourcePanel;

  /**
   * target panel that has labels for each individual alignment unit in
   * the target parent of alignment unit
   */
  private JPanel targetPanel;

  /**
   * list of available documentIDs - chosen one is selected as the
   * source document.
   */
  private JComboBox sourceDocumentId;

  /**
   * list of available documentIDs - chosen one is selected as the
   * target document.
   */
  private JComboBox targetDocumentId;

  /**
   * annotation set to used from the source document
   */
  private JComboBox sourceASName;

  /**
   * annotation set to be used from the target document
   */
  private JComboBox targetASName;

  /**
   * annotation type to be used as a unit to align in the source
   * document
   */
  private JComboBox sourceUnitOfAlignment;

  /**
   * annotation type to be used as a unit to align in the target
   * document
   */
  private JComboBox targetUnitOfAlignment;

  /**
   * annotation type to be used as parent of unit to align in the source
   * document
   */
  private JComboBox sourceParentOfUnitOfAlignment;

  /**
   * annotation type to be used as parent of unit to align in the target
   * document
   */
  private JComboBox targetParentOfUnitOfAlignment;

  /**
   * name of the feature used as a key in the document feature to hold
   * alignment mappings
   */
  private JComboBox alignmentFeatureNames;

  /**
   * indicates if the links should be displayed
   */
  private JToggleButton showLinks;

  /**
   * Buttons for different actions
   */
  private JButton next, previous, loadActions, saveDocument;

  /**
   * Indicates if the alignment data should be populated
   */
  private JToggleButton populate;

  /**
   * canvas used for drawing links between the alignment units
   */
  private MappingsPanel linesCanvas;

  /**
   * The document, this alignment editor belongs to.
   */
  private CompoundDocument document;

  /**
   * Alignment factory, that provides various methods to populate data
   * and iterate over alignment pairs.
   */
  private AlignmentFactory alignFactory;

  /**
   * Alignment object, that is used for storing alignment informaiton
   */
  private Alignment alignment;

  /**
   * mappings for annotations and their highlights
   */
  private HashMap<Annotation, AnnotationHighlight> sourceHighlights;

  /**
   * mappings for annotations and their highlights
   */
  private HashMap<Annotation, AnnotationHighlight> targetHighlights;

  /**
   * Remembers the selected annotations
   */
  private List<Annotation> sourceLatestAnnotationsSelection;

  /**
   * Remembers the selected annotations
   */
  private List<Annotation> targetLatestAnnotationsSelection;

  /**
   * a list of alignment actions available to the user
   */
  private List<AlignmentAction> allActions;

  /**
   * A color that is being used for current highlighting
   */
  private Color color;

  /**
   * used for generating random colors
   */
  private ColorGenerator colorGenerator = new ColorGenerator();

  /**
   * Default font-size
   */
  public static final int TEXT_SIZE = 20;

  /**
   * mappings for menu item and associated alignment action
   */
  private Map<JMenuItem, AlignmentAction> actions;

  /**
   * mappings for menu items and their captions
   */
  private Map<String, JMenuItem> actionsMenuItemByCaption;

  /**
   * annotation highlight with the mouse on it
   */
  private AnnotationHighlight currentAnnotationHightlight = null;

  /**
   * instance of the alignment editor with focus on it
   */
  private AlignmentEditor thisInstance = null;

  /**
   * default align action - i.e. what happens when user clicks on the
   * align button
   */
  private AlignAction alignAction = null;

  /**
   * mappings for alignment actions and its respective check box - which
   * if checked, indicates that the respective alignment action should
   * be executed.
   */
  private HashMap<AlignmentAction, PropertyActionCB> actionsCBMap = null;

  /**
   * default unalign action - i.e. what happens when user clicks on the
   * unalign button
   */
  private RemoveAlignmentAction removeAlignmentAction = null;

  /**
   * list of actions that should be executed before a pair is displayed
   * on the screen.
   */
  private List<PreDisplayAction> preDisplayActions = null;

  /**
   * list of actions that should be executed after a user has indicated
   * that the alignment for the given pair is finished.
   */
  private List<FinishedAlignmentAction> finishedAlignmentActions = null;

  /**
   * actions to store data publishers' instances
   */
  private List<DataPublisherAction> dataPublisherActions = null;

  /**
   * id of selected source document
   */
  private String selectedSourceID;
  
  /**
   * id of the selected target document
   */
  private String selectedTargetID;
  
  /**
   * properties Pane to hold properties panel
   */
  private JScrollPane propertiesPane;
  
  /*
   * (non-Javadoc)
   * 
   * @see gate.Resource#init()
   */
  public Resource init() throws ResourceInstantiationException {
    sourceHighlights = new HashMap<Annotation, AnnotationHighlight>();
    targetHighlights = new HashMap<Annotation, AnnotationHighlight>();
    actionsCBMap = new HashMap<AlignmentAction, PropertyActionCB>();
    sourceLatestAnnotationsSelection = new ArrayList<Annotation>();
    targetLatestAnnotationsSelection = new ArrayList<Annotation>();

    actions = new HashMap<JMenuItem, AlignmentAction>();
    actionsMenuItemByCaption = new HashMap<String, JMenuItem>();
    allActions = new ArrayList<AlignmentAction>();
    preDisplayActions = new ArrayList<PreDisplayAction>();
    finishedAlignmentActions = new ArrayList<FinishedAlignmentAction>();
    dataPublisherActions = new ArrayList<DataPublisherAction>();

    thisInstance = this;
    return this;
  }

  /**
   * Initialize the GUI
   */
  private void initGui() {
    mainPanel = new JSplitPane(JSplitPane.VERTICAL_SPLIT);
    paramPanel = new JPanel();
    paramPanel.setLayout(new BoxLayout(paramPanel, BoxLayout.Y_AXIS));

    waPanel = new JPanel(new BorderLayout());

    sourceDocumentId = new JComboBox(new DefaultComboBoxModel());
    sourceDocumentId.setEditable(false);

    targetDocumentId = new JComboBox(new DefaultComboBoxModel());
    targetDocumentId.setEditable(false);

    sourceDocumentId.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent ae) {
        selectedSourceID = (String) sourceDocumentId.getSelectedItem();
        populateAS(selectedSourceID, sourceASName);
      }
    });

    targetDocumentId.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent ae) {
        selectedTargetID = (String) targetDocumentId.getSelectedItem();
        populateAS(selectedTargetID, targetASName);
      }
    });

    sourceASName = new JComboBox(new DefaultComboBoxModel());
    sourceASName.setPrototypeDisplayValue("AnnotationSetName");
    sourceASName.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent ae) {

        populateParentOfUnitOfAlignment(selectedSourceID, sourceParentOfUnitOfAlignment);
      }
    });

    targetASName = new JComboBox(new DefaultComboBoxModel());
    targetASName.setPrototypeDisplayValue("AnnotationSetName");
    targetASName.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent ae) {
        populateParentOfUnitOfAlignment(selectedTargetID, targetParentOfUnitOfAlignment);
      }
    });

    sourceParentOfUnitOfAlignment = new JComboBox(new DefaultComboBoxModel());
    sourceParentOfUnitOfAlignment.setPrototypeDisplayValue("AnnotationSetName");
    sourceParentOfUnitOfAlignment.setEditable(false);
    sourceParentOfUnitOfAlignment.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent ae) {
        populateUnitOfAlignment(selectedSourceID,
                sourceUnitOfAlignment);
      }
    });

    targetParentOfUnitOfAlignment = new JComboBox(new DefaultComboBoxModel());
    targetParentOfUnitOfAlignment.setPrototypeDisplayValue("AnnotationSetName");
    targetParentOfUnitOfAlignment.setEditable(false);
    targetParentOfUnitOfAlignment.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent ae) {
        populateUnitOfAlignment(selectedTargetID,
                targetUnitOfAlignment);
      }
    });

    sourceUnitOfAlignment = new JComboBox(new DefaultComboBoxModel());
    sourceUnitOfAlignment.setPrototypeDisplayValue("AnnotationSetName");
    sourceUnitOfAlignment.setEditable(false);

    targetUnitOfAlignment = new JComboBox(new DefaultComboBoxModel());
    targetUnitOfAlignment.setPrototypeDisplayValue("AnnotationSetName");
    targetUnitOfAlignment.setEditable(false);

    alignmentFeatureNames = new JComboBox(new DefaultComboBoxModel());
    alignmentFeatureNames.setEditable(true);
    ((DefaultComboBoxModel)alignmentFeatureNames.getModel())
            .addElement(AlignmentFactory.ALIGNMENT_FEATURE_NAME);
    alignmentFeatureNames.setPrototypeDisplayValue("AnnotationSetName");

    sourceDocPanel = new JPanel(new FlowLayout(FlowLayout.LEFT));
    targetDocPanel = new JPanel(new FlowLayout(FlowLayout.LEFT));
    iteratingPanel = new JPanel(new FlowLayout(FlowLayout.LEFT));
    populateButtonsPanel = new JPanel(new FlowLayout(FlowLayout.LEFT));

    sourceDocPanel.add(new JLabel("sourceDoc:"));
    sourceDocPanel.add(sourceDocumentId);
    sourceDocPanel.add(new JLabel("annotationSet:"));
    sourceDocPanel.add(sourceASName);
    sourceDocPanel.add(new JLabel("parentOfAlignmentUnit:"));
    sourceDocPanel.add(sourceParentOfUnitOfAlignment);
    sourceDocPanel.add(new JLabel("unitOfAlignment:"));
    sourceDocPanel.add(sourceUnitOfAlignment);

    targetDocPanel.add(new JLabel("targetDoc:"));
    targetDocPanel.add(targetDocumentId);
    targetDocPanel.add(new JLabel("annotationSet:"));
    targetDocPanel.add(targetASName);
    targetDocPanel.add(new JLabel("parentOfAlignmentUnit:"));
    targetDocPanel.add(targetParentOfUnitOfAlignment);
    targetDocPanel.add(new JLabel("unitOfAlignment:"));
    targetDocPanel.add(targetUnitOfAlignment);

    iteratingPanel.add(new JLabel("iteratingMethodClassName:"));
    iteratingMethodTF = new JTextField(
            "gate.alignment.gui.DefaultIteratingMethod", 30);
    iteratingPanel.add(iteratingMethodTF);

    alignmentFeatureNames
            .setSelectedItem(AlignmentFactory.ALIGNMENT_FEATURE_NAME);
    alignmentFeatureNames.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent ae) {
        populateAlignmentFeatureNames();
      }
    });

    populate = new JToggleButton("Populate");
    populate.addActionListener(this);

    previous = new JButton("< Previous");
    previous.addActionListener(this);

    next = new JButton("Next >");
    next.addActionListener(this);

    showLinks = new JToggleButton("Horizontal View");
    showLinks.setSelected(true);
    showLinks.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent ae) {
        if(!showLinks.isSelected()) {
          sourcePanel.setLayout(new BoxLayout(sourcePanel, BoxLayout.Y_AXIS));
          targetPanel.setLayout(new BoxLayout(targetPanel, BoxLayout.Y_AXIS));
          waPanel.remove(sourcePanel);
          waPanel.remove(targetPanel);
          waPanel.setLayout(new GridLayout(1, 2));
          waPanel.add(sourcePanel);
          waPanel.add(targetPanel);
//          sourcePanel.setPreferredSize(new Dimension(230, 500));
//          targetPanel.setPreferredSize(new Dimension(230, 500));
        }
        else {
          sourcePanel.setLayout(new BoxLayout(sourcePanel, BoxLayout.X_AXIS));
          targetPanel.setLayout(new BoxLayout(targetPanel, BoxLayout.X_AXIS));
          waPanel.remove(sourcePanel);
          waPanel.remove(targetPanel);
          waPanel.setLayout(new BorderLayout());
          waPanel.add(sourcePanel, BorderLayout.NORTH);
          waPanel.add(targetPanel, BorderLayout.SOUTH);
//          sourcePanel.setPreferredSize(new Dimension(500, 20));
//          targetPanel.setPreferredSize(new Dimension(230, 20));
        }

        if(linesCanvas != null) {
          if(showLinks.isSelected()) {
            waPanel.add(linesCanvas, BorderLayout.CENTER);
          }
          else {
            waPanel.remove(linesCanvas);
          }
          waPanel.revalidate();
          waPanel.updateUI();
        }
        refresh();
      }
    });

    saveDocument = new JButton(new SaveAsASingleXML());
    loadActions = new JButton("Load Actions");
    loadActions.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent ae) {
        int mode = 0;
        JFileChooser fileChooser = null;
        try {
          fileChooser = Main.getMainFrame().getFileChooser();
          mode = fileChooser.getFileSelectionMode();
          fileChooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
          int answer = fileChooser.showOpenDialog(MainFrame.getInstance());
          
          if(answer == JFileChooser.APPROVE_OPTION) {
            File selectedFile = fileChooser.getSelectedFile();
            if(selectedFile == null) {
              return;
            }
            else {
              readActions(selectedFile);
            }
          }
        }
        catch(GateException ge) {
          throw new GateRuntimeException(ge);
        } finally {
          if(fileChooser != null)
            fileChooser.setFileSelectionMode(mode);
        }
      }
    });

    iteratingPanel.add(new JLabel("alignmentFeatureName:"));
    iteratingPanel.add(alignmentFeatureNames);
    populateButtonsPanel.add(populate);
    populateButtonsPanel.add(previous);
    populateButtonsPanel.add(next);
    populateButtonsPanel.add(loadActions);
    populateButtonsPanel.add(showLinks);
    populateButtonsPanel.add(saveDocument);

    paramPanel.add(sourceDocPanel);
    paramPanel.add(targetDocPanel);
    paramPanel.add(iteratingPanel);
    paramPanel.add(populateButtonsPanel);

    paramPanel.setBorder(new TitledBorder("Settings"));
    mainPanel.add(new JScrollPane(paramPanel)/* , BorderLayout.NORTH */);

    sourcePanel = new JPanel();
    sourcePanel.setLayout(new BoxLayout(sourcePanel, BoxLayout.X_AXIS));
    sourcePanel.setBackground(Color.WHITE);
    

    targetPanel = new JPanel();
    targetPanel.setLayout(new BoxLayout(targetPanel, BoxLayout.X_AXIS));
    targetPanel.setBackground(Color.WHITE);
    
    linesCanvas = new MappingsPanel();
    linesCanvas.setBackground(Color.WHITE);
    linesCanvas.setLayout(null);
    linesCanvas.setPreferredSize(new Dimension(800, 50));
    linesCanvas.setOpaque(true);

    propertiesPanel = new JPanel();
    propertiesPanel.setLayout(new BoxLayout(propertiesPanel, BoxLayout.Y_AXIS));
    propertiesPane = new JScrollPane(propertiesPanel);
    propertiesPanel.add(new JLabel("Actions"));
    propertiesPanel.add(Box.createGlue());
    propertiesPane.setVisible(false);

    waPanel.add(sourcePanel, BorderLayout.NORTH);
    waPanel.add(targetPanel, BorderLayout.SOUTH);
    waPanel.add(linesCanvas, BorderLayout.CENTER);
    JPanel splitPane = new JPanel(new BorderLayout());
    JPanel waParentPanel = new JPanel(new BorderLayout());
    waScrollPane = new JScrollPane(waPanel);
    //waScrollPane.setPreferredSize(new Dimension(800, 200));

    waParentPanel.add(waScrollPane, BorderLayout.CENTER);
    splitPane.add(waParentPanel, BorderLayout.CENTER);
    splitPane.add(propertiesPane, BorderLayout.EAST);
    mainPanel.add(splitPane);

    this.setLayout(new BorderLayout());
    this.add(mainPanel, BorderLayout.CENTER);
    color = getColor(null);
    splitPane.revalidate();
    splitPane.updateUI();
    waPanel.setVisible(false);

    ResourceData myResourceData = (ResourceData)Gate.getCreoleRegister().get(
            this.getClass().getName());
    URL creoleXml = myResourceData.getXmlFileUrl();
    URL alignmentHomeURL = null;
    File actionsConfFile = null;
    try {
      alignmentHomeURL = new URL(creoleXml, ".");

      // loading the default actions config file.
      actionsConfFile = new File(new File(new File(alignmentHomeURL.toURI()),
              "resources"), ACTIONS_CONFIG_FILE);
    }
    catch(MalformedURLException mue) {
      throw new GateRuntimeException(mue);
    }
    catch(URISyntaxException use) {
      throw new GateRuntimeException(use);
    }

    readAction(new ResetAction());
    alignAction = new AlignAction();
    readAction(alignAction);
    removeAlignmentAction = new RemoveAlignmentAction();
    readAction(removeAlignmentAction);
    readActions(actionsConfFile);
  
  }

  /**
   * populates the annotation set combobox
   * 
   * @param documentID
   * @param boxToPopulate
   */
  private void populateAS(String documentID, JComboBox boxToPopulate) {
    Document doc = document.getDocument(documentID);
    Map<String, AnnotationSet> annotSets = doc.getNamedAnnotationSets();
    if(annotSets == null) {
      annotSets = new HashMap<String, AnnotationSet>();
    }

    HashSet<String> setNames = new HashSet<String>(annotSets.keySet());
    setNames.add("<null>");
    DefaultComboBoxModel dcbm = new DefaultComboBoxModel(setNames
            .toArray(new String[0]));
    boxToPopulate.setModel(dcbm);
    if(!setNames.isEmpty()) {
      boxToPopulate.setSelectedIndex(0);
    }
  }

  /**
   * populates the documentIds combobox
   * 
   * @param documentID
   * @param boxToPopulate
   */
  private void populateDocumentIds(JComboBox boxToPopulate, String[] documentIds) {
    if(documentIds == null) documentIds = new String[0];
    DefaultComboBoxModel dcbm = new DefaultComboBoxModel(documentIds);
    boxToPopulate.setModel(dcbm);
    if(documentIds.length == 0) {
      boxToPopulate.setEnabled(false);
    }
  }

  /**
   * populates the documentIds combobox
   * 
   * @param documentID
   * @param boxToPopulate
   */
  private void populateAlignmentFeatureNames() {
    this.document
            .getAlignmentInformation(AlignmentFactory.ALIGNMENT_FEATURE_NAME);
    Set<String> alignmentFeatureNames = this.document
            .getAllAlignmentFeatureNames();
    DefaultComboBoxModel dcbm = new DefaultComboBoxModel(alignmentFeatureNames
            .toArray(new String[0]));
    Object selectedItem = this.alignmentFeatureNames.getSelectedItem();
    this.alignmentFeatureNames.setModel(dcbm);
    if(selectedItem != null) {
      this.alignmentFeatureNames.setSelectedItem(selectedItem);
    }

    selectedItem = this.alignmentFeatureNames.getSelectedItem();
    alignment = this.document.getAlignmentInformation((String)selectedItem);
    if(alignment != null) {
      alignment.removeAlignmentListener(thisInstance);
      alignment.addAlignmentListener(thisInstance);
      refresh();
    }
  }

  /**
   * populates the annotation set combobox
   * 
   * @param documentID
   * @param boxToPopulate
   */
  private void populateParentOfUnitOfAlignment(String documentID,
          JComboBox boxToPopulate) {
    Document doc = document.getDocument(documentID);
    String asName = null;
    if(boxToPopulate == sourceParentOfUnitOfAlignment) {
      asName = (String)sourceASName.getSelectedItem();
    }
    else {
      asName = (String)targetASName.getSelectedItem();
    }

    AnnotationSet srcAnnotSet = asName.equals("<null>")
            ? doc.getAnnotations()
            : doc.getAnnotations(asName);
    Set<String> annotTypes = srcAnnotSet.getAllTypes();
    if(annotTypes == null) {
      annotTypes = new HashSet<String>();
    }

    DefaultComboBoxModel dcbm = new DefaultComboBoxModel(annotTypes
            .toArray(new String[0]));
    boxToPopulate.setModel(dcbm);
    if(!annotTypes.isEmpty()) {
      if(dcbm.getIndexOf("Sentence") >= 0) {
        boxToPopulate.setSelectedIndex(dcbm.getIndexOf("Sentence"));
      }
      else {
        boxToPopulate.setSelectedIndex(0);
      }
    }
  }

  /**
   * populates the annotation set combobox
   * 
   * @param documentID
   * @param boxToPopulate
   */
  private void populateUnitOfAlignment(String documentID,
          JComboBox boxToPopulate) {
    Document doc = document.getDocument(documentID);
    String asName = null;
    if(boxToPopulate == sourceUnitOfAlignment) {
      asName = (String)sourceASName.getSelectedItem();
    }
    else {
      asName = (String)targetASName.getSelectedItem();
    }

    AnnotationSet srcAnnotSet = asName.equals("<null>")
            ? doc.getAnnotations()
            : doc.getAnnotations(asName);
    Set<String> annotTypes = srcAnnotSet.getAllTypes();
    if(annotTypes == null) {
      annotTypes = new HashSet<String>();
    }

    DefaultComboBoxModel dcbm = new DefaultComboBoxModel(annotTypes
            .toArray(new String[0]));
    boxToPopulate.setModel(dcbm);
    if(annotTypes.size() > 0) {
      if(dcbm.getIndexOf("Token") >= 0) {
        boxToPopulate.setSelectedIndex(dcbm.getIndexOf("Token"));
      }
      else {
        boxToPopulate.setSelectedIndex(0);
      }

    }
  }

  /*
   * (non-Javadoc)
   * 
   * @see gate.VisualResource#setTarget(java.lang.Object)
   */
  public void setTarget(Object target) {
    this.document = (CompoundDocument)target;
    thisInstance = this;
    List<String> documentIDs = new ArrayList<String>(this.document
            .getDocumentIDs());
    initGui();
    populateDocumentIds(sourceDocumentId, documentIDs.toArray(new String[0]));
    if(documentIDs.size() > 0) {
      sourceDocumentId.setSelectedIndex(0);
    }

    populateDocumentIds(targetDocumentId, documentIDs.toArray(new String[0]));
    if(documentIDs.size() > 1) {
      targetDocumentId.setSelectedIndex(1);
    }
    else if(documentIDs.size() > 0) {
      targetDocumentId.setSelectedIndex(0);
    }

    populateAlignmentFeatureNames();
  }

  /**
   * This method clears up the latest annotation selection
   */
  public void clearLatestAnnotationsSelection() {

    if(sourceLatestAnnotationsSelection != null
            && !sourceLatestAnnotationsSelection.isEmpty()) {

      for(Annotation annotation : sourceLatestAnnotationsSelection) {
        AnnotationHighlight ah = sourceHighlights.get(annotation);
        ah.setHighlighted(false, Color.WHITE);
      }
      sourceLatestAnnotationsSelection.clear();

    }

    if(targetLatestAnnotationsSelection != null
            && !targetLatestAnnotationsSelection.isEmpty()) {

      for(Annotation annotation : targetLatestAnnotationsSelection) {
        AnnotationHighlight ah = targetHighlights.get(annotation);
        ah.setHighlighted(false, Color.WHITE);
      }
      targetLatestAnnotationsSelection.clear();
    }

  }

  /**
   * Executes the given action. It uses the pair that is being currently
   * shown to collect the alignment information which is then used as
   * parameters to call the provided action.
   * 
   * @param aa
   */
  protected void executeAction(AlignmentAction aa) {

    // obtaining source and target documents
    Document srcDocument = document.getDocument(selectedSourceID);
    Document tgtDocument = document.getDocument(selectedTargetID);

    // obtaining selected annotations
    Set<Annotation> srcSelectedAnnots = new HashSet<Annotation>(
            sourceLatestAnnotationsSelection);
    Set<Annotation> tgtSelectedAnnots = new HashSet<Annotation>(
            targetLatestAnnotationsSelection);

    if(currentAnnotationHightlight != null) {

      Set<Annotation> alignedAnnots = alignment
              .getAlignedAnnotations(currentAnnotationHightlight.annotation);
      if(alignedAnnots == null) alignedAnnots = new HashSet<Annotation>();
      alignedAnnots.add(currentAnnotationHightlight.annotation);

      for(Annotation annot : alignedAnnots) {
        Document tempDoc = alignment.getDocument(annot);
        if(tempDoc == srcDocument) {
          srcSelectedAnnots.add(annot);
        }
        else if(tempDoc == tgtDocument) {
          tgtSelectedAnnots.add(annot);
        }
      }
    }

    try {
      color = Color.WHITE;
      aa.execute(this, this.document, srcDocument,
              getSourceAnnotationSetName(), srcSelectedAnnots, tgtDocument,
              getTargetAnnotationSetName(), tgtSelectedAnnots,
              currentAnnotationHightlight.annotation);

      if(aa == alignAction) {
        for(AlignmentAction a : allActions) {
          if(a.invokeWithAlignAction()) {
            JCheckBox cb = actionsCBMap.get(a);
            if((cb != null && cb.isSelected()) || cb == null)
              a
                      .execute(this, this.document, srcDocument,
                              getSourceAnnotationSetName(), srcSelectedAnnots,
                              tgtDocument, getTargetAnnotationSetName(),
                              tgtSelectedAnnots,
                              currentAnnotationHightlight.annotation);

          }

        }
      }
      else if(aa == removeAlignmentAction) {
        for(AlignmentAction a : allActions) {
          if(a.invokeWithRemoveAction()) {
            JCheckBox cb = actionsCBMap.get(a);
            if((cb != null && cb.isSelected()) || cb == null)

              a
                      .execute(this, this.document, srcDocument,
                              getSourceAnnotationSetName(), srcSelectedAnnots,
                              tgtDocument, getTargetAnnotationSetName(),
                              tgtSelectedAnnots,
                              currentAnnotationHightlight.annotation);
          }
        }
      }
    }
    catch(AlignmentException ae) {
      throw new GateRuntimeException(ae);
    }
  }

  /**
   * Get the alignment feature name
   * 
   * @return
   */
  public String getAlignmentFeatureName() {
    return this.alignmentFeatureNames.getSelectedItem().toString();
  }

  /**
   * Using user selections, this method obtains pairs and shows the
   * first one in the GUI
   */
  public void populate() {
    if(!populate.isSelected()) {
      if(!disableUserSelections) {
        paramPanel.remove(populateButtonsPanel);
        paramPanel.add(sourceDocPanel);
        paramPanel.add(targetDocPanel);
        paramPanel.add(iteratingPanel);
        paramPanel.add(populateButtonsPanel);
        paramPanel.revalidate();
        next.setEnabled(false);
        previous.setEnabled(false);
        showLinks.setEnabled(false);
        waPanel.setVisible(false);
      }
    }
    else {
      if(!disableUserSelections) {
        paramPanel.remove(sourceDocPanel);
        paramPanel.remove(targetDocPanel);
        paramPanel.remove(iteratingPanel);
        paramPanel.revalidate();
        next.setEnabled(true);
        previous.setEnabled(true);
        showLinks.setEnabled(true);
        waPanel.setVisible(true);
      }

      try {
        if(sourceUnitOfAlignment.getSelectedItem() == null) return;
        if(targetUnitOfAlignment.getSelectedItem() == null) return;
        if(sourceParentOfUnitOfAlignment.getSelectedItem() == null) return;
        if(targetParentOfUnitOfAlignment.getSelectedItem() == null) return;

        AlignmentFactory af = new AlignmentFactory(document, selectedSourceID, selectedTargetID, sourceASName.getSelectedItem()
                .toString(), targetASName.getSelectedItem().toString(),
                sourceUnitOfAlignment.getSelectedItem().toString(),
                targetUnitOfAlignment.getSelectedItem().toString(),
                sourceParentOfUnitOfAlignment.getSelectedItem().toString(),
                targetParentOfUnitOfAlignment.getSelectedItem().toString(),
                iteratingMethodTF.getText().trim());

        sourcePanel.setBorder(BorderFactory.createTitledBorder(selectedSourceID));
        targetPanel.setBorder(BorderFactory.createTitledBorder(selectedTargetID));

        // if there were no errors
        alignFactory = af;
        nextAction();
      }
      catch(Exception e) {
        e.printStackTrace();
      }
    }

  }

  /**
   * handles vairous actions, such as populate, next, previous etc.
   */
  public void actionPerformed(ActionEvent ae) {
    if(ae.getSource() == populate) {
      populate();
    }
    else if(ae.getSource() == next) {
      if(alignFactory != null) {
        int answer = JOptionPane.showConfirmDialog(mainPanel,
                "Is alignment complete for this pair?");
        if(answer == JOptionPane.YES_OPTION) {
          callFinishedAlignmentActions();
        }
        else if(answer != JOptionPane.NO_OPTION) {
          return;
        }
      }
      nextAction();
    }
    else if(ae.getSource() == previous) {
      if(alignFactory != null) {
        int answer = JOptionPane.showConfirmDialog(mainPanel,
                "Is alignment complete for this pair?");
        if(answer == JOptionPane.YES_OPTION) {
          callFinishedAlignmentActions();
        }
        else if(answer != JOptionPane.NO_OPTION) {
          return;
        }
      }
      previousAction();
    }
  }

  /**
   * if user says that the pair is completely aligned, this method is
   * invoked, which retrieves a list of FinishedAlignmentActions and
   * calls them one by one.
   */
  private void callFinishedAlignmentActions() {

    Pair pair = alignFactory.current();
    Set<Annotation> srcAnnotations = null;
    Set<Annotation> tgtAnnotations = null;
    Document srcDocument = null;
    Document tgtDocument = null;

    tgtDocument = document.getDocument(pair.getTargetDocumentId());
    srcDocument = document.getDocument(pair.getSourceDocumentId());

    srcAnnotations = alignFactory.getUnderlyingAnnotations(pair
            .getSrcAnnotation(), pair.getSourceDocumentId(),
            sourceUnitOfAlignment.getSelectedItem().toString());
    tgtAnnotations = alignFactory.getUnderlyingAnnotations(pair
            .getTgtAnnotation(), pair.getTargetDocumentId(),
            targetUnitOfAlignment.getSelectedItem().toString());

    for(FinishedAlignmentAction faa : finishedAlignmentActions) {
      try {
        faa.execute(this, document, srcDocument, (String)sourceASName
                .getSelectedItem(), srcAnnotations, tgtDocument,
                (String)targetASName.getSelectedItem(), tgtAnnotations);
      }
      catch(AlignmentException ae) {
        throw new GateRuntimeException(ae);
      }
    }

  }

  /**
   * obtains the next available pair to show it in the editor
   */
  private void nextAction() {
    if(alignFactory != null && alignFactory.hasNext()) {

      Pair next = alignFactory.next();
      Document srcDocument = document.getDocument(next.getSourceDocumentId());
      Document tgtDocument = document.getDocument(next.getTargetDocumentId());

      // before showing, lets execute preDisplayActions
      for(PreDisplayAction pda : preDisplayActions) {
        try {
          pda.execute(this, document, srcDocument,
                  getSourceAnnotationSetName(), next.getSrcAnnotation(),
                  tgtDocument, getTargetAnnotationSetName(), next
                          .getTgtAnnotation());
        }
        catch(AlignmentException ae) {
          ae.printStackTrace();
        }
      }
      updateGUI(next);
    }
    else {
      JOptionPane.showMessageDialog(mainPanel, "Reached End of the Document");
    }
  }

  /**
   * simply refreshes the gui
   */
  private void refresh() {
    if(alignFactory != null && alignFactory.current() != null) {
      updateGUI(alignFactory.current());
    }
  }

  /**
   * This method updates the GUI.
   * 
   * @param docIDsAndAnnots
   */
  private void updateGUI(Pair pair) {
    // before refreshing, we remove all the highlights
    clearLatestAnnotationsSelection();
    sourcePanel.removeAll();
    sourcePanel.updateUI();

    targetPanel.removeAll();
    targetPanel.updateUI();

    linesCanvas.removeAllEdges();
    linesCanvas.repaint();

    HashMap<String, Annotation> docIDsAndAnnots = new HashMap<String, Annotation>();
    docIDsAndAnnots.put(pair.getSourceDocumentId(), pair.getSrcAnnotation());
    docIDsAndAnnots.put(pair.getTargetDocumentId(), pair.getTgtAnnotation());

    Annotation srcSentence = null;
    Annotation tgtSentence = null;
    String srcDocID = null;
    String tgtDocID = null;

    for(String docId : docIDsAndAnnots.keySet()) {
      JPanel panelToUse = sourcePanel;
      boolean isSourceDocument = true;
      if(docId.equals(targetDocumentId.getSelectedItem().toString())) {
        panelToUse = targetPanel;
        isSourceDocument = false;
      }

      // sentence annotation
      Annotation annot = docIDsAndAnnots.get(docId);
      if(isSourceDocument) {
        srcSentence = annot;
        srcDocID = docId;
      }
      else {
        tgtSentence = annot;
        tgtDocID = docId;
      }

      // we need to highlight the unit type
      AnnotationSet underlyingUnitAnnotationsSet = alignFactory
              .getUnderlyingAnnotations(annot, docId, isSourceDocument
                      ? sourceUnitOfAlignment.getSelectedItem().toString()
                      : targetUnitOfAlignment.getSelectedItem().toString());
      // if there are not underlying annotations, just return
      if(underlyingUnitAnnotationsSet == null) {
        return;
      }

      ArrayList<Annotation> units = new ArrayList<Annotation>(
              underlyingUnitAnnotationsSet);
      Collections.sort(units, new gate.util.OffsetComparator());

      // for each underlying unit of alignment, we create a default
      // annotation highlight.
      HashMap<Annotation, AnnotationHighlight> annotationHighlightsMap = new HashMap<Annotation, AnnotationHighlight>();
      for(Annotation underlyingUnitAnnotation : units) {
        String text = alignFactory.getText(underlyingUnitAnnotation, docId);
        AnnotationHighlight ah = new AnnotationHighlight(text, Color.WHITE,
                underlyingUnitAnnotation, isSourceDocument);
        annotationHighlightsMap.put(underlyingUnitAnnotation, ah);
        panelToUse.add(ah);
        if(showLinks.isSelected()) {
          panelToUse.add(Box.createRigidArea(new Dimension(5, 0)));
        } else {
          panelToUse.add(Box.createRigidArea(new Dimension(0, 10)));
        }
      }

      if(isSourceDocument) {
        this.sourceHighlights = annotationHighlightsMap;
      }
      else {
        this.targetHighlights = annotationHighlightsMap;
      }
      panelToUse.revalidate();
      panelToUse.updateUI();
    }

    // now we need to highlight the aligned annotations if there are any
    Set<Annotation> setOfAlignedAnnotations = alignment.getAlignedAnnotations();

    // we keep record of which annotations are already highlighted in
    // order to not highlight them again
    Set<Annotation> highlightedAnnotations = new HashSet<Annotation>();

    // one annotation at a time
    for(Annotation srcAnnotation : setOfAlignedAnnotations) {

      // if already highlighted, don't do it again
      if(highlightedAnnotations.contains(srcAnnotation)) continue;

      // if the annotation doesn't belong to one of the source
      // or target annotations, just skip it
      if(!sourceHighlights.containsKey(srcAnnotation)
              && !targetHighlights.containsKey(srcAnnotation)) {
        continue;
      }

      // find out the language/id of the document
      String docId = alignment.getDocument(srcAnnotation).getName();

      JPanel pane = null;
      boolean isSrcDocument = false;

      if(docId.equals(selectedSourceID)) {
        pane = sourcePanel;
        isSrcDocument = true;
      }
      else if(docId.equals(selectedTargetID)) {
        pane = targetPanel;
        isSrcDocument = false;
      }

      if(pane == null) continue;

      Set<Annotation> sourceAnnots = new HashSet<Annotation>();
      Set<Annotation> targetAnnots = new HashSet<Annotation>();

      if(isSrcDocument) {
        targetAnnots = alignment.getAlignedAnnotations(srcAnnotation);
        for(Annotation tgtAnnot : targetAnnots) {
          Set<Annotation> setOfAnnots = alignment
                  .getAlignedAnnotations(tgtAnnot);
          for(Annotation sAnnot : setOfAnnots) {
            Set<Annotation> setOfTargetAnnots = alignment
                    .getAlignedAnnotations(sAnnot);
            if(setOfTargetAnnots.size() == targetAnnots.size()) {
              if(setOfTargetAnnots.containsAll(targetAnnots)) {
                sourceAnnots.add(sAnnot);
              }
            }
          }
        }
      }
      else {
        sourceAnnots = alignment.getAlignedAnnotations(srcAnnotation);
        for(Annotation srcAnnot : sourceAnnots) {
          Set<Annotation> setOfAnnots = alignment
                  .getAlignedAnnotations(srcAnnot);
          for(Annotation tAnnot : setOfAnnots) {
            Set<Annotation> setOfSourceAnnots = alignment
                    .getAlignedAnnotations(tAnnot);
            if(setOfSourceAnnots.size() == sourceAnnots.size()) {
              if(setOfSourceAnnots.containsAll(sourceAnnots)) {
                targetAnnots.add(tAnnot);
              }
            }
          }
        }
      }

      Color newColor = getColor(null);
      boolean firstTime = true;

      Set<Annotation> toRemove = new HashSet<Annotation>();
      for(Annotation tgtAnnot : targetAnnots) {
        AnnotationHighlight ah = targetHighlights.get(tgtAnnot);
        if(ah == null) {
          toRemove.add(tgtAnnot);
        }
      }

      targetAnnots.removeAll(toRemove);

      for(Annotation srcAnnot : sourceAnnots) {
        AnnotationHighlight sAh = sourceHighlights.get(srcAnnot);
        sAh.setHighlighted(true, newColor);

        for(Annotation tgtAnnot : targetAnnots) {

          AnnotationHighlight ah = targetHighlights.get(tgtAnnot);

          if(firstTime) {
            ah.setHighlighted(true, newColor);
          }

          Edge edge = new Edge();
          edge.srcAH = sAh;
          edge.tgtAH = ah;
          linesCanvas.addEdge(edge);
          linesCanvas.repaint();
        }
        firstTime = false;
      }
    }
  }

  /**
   * does the cleaning up job to free up memory occupied by this editor
   */
  public void cleanup() {
    for(AlignmentAction a : allActions) {
      a.cleanup();
    }
    
    for(PreDisplayAction pda : preDisplayActions) {
      pda.cleanup();
    }

    for(FinishedAlignmentAction faa : finishedAlignmentActions) {
      faa.cleanup();
    }
    
    for(DataPublisherAction dpa : dataPublisherActions) {
      dpa.cleanup();
    }
  }

  /**
   * obtains the pair previous to the current one and shows it into the
   * alignment editor.
   */
  private void previousAction() {
    if(alignFactory != null && alignFactory.hasPrevious()) {
      updateGUI(alignFactory.previous());
    }
    else {
      JOptionPane.showMessageDialog(mainPanel, "Reached Start of the Document");
    }
  }

  /**
   * indicates end of the process
   */
  public void processFinished() {
    this.document.setCurrentDocument("null");
  }

  public void progressChanged(int prgress) {
  }

  /**
   * Internal class - it represents an alignment unit.
   * 
   * @author niraj
   * 
   */
  protected class AnnotationHighlight extends JLabel {

    /**
     * indicates if the annotation is highlighted or not
     */
    boolean highlighted = false;

    /**
     * if the current highlight belongs to the source document
     */
    boolean sourceDocument = false;

    /**
     * color of the highlight
     */
    Color colorToUse = Color.WHITE;

    /**
     * annotation it refers to
     */
    Annotation annotation;

    /**
     * constructor
     * 
     * @param text - the underlying text of the annotation
     * @param color - color of the highlight
     * @param annot - annotation the current highlight refers to
     * @param sourceDocument - if the current annotation belongs to the
     *          source document
     */
    public AnnotationHighlight(String text, Color color, Annotation annot,
            boolean sourceDocument) {
      super((showLinks.isSelected() ? "" : "<html>")+text+(showLinks.isSelected() ? "" : "</html>"));
      this.setOpaque(true);
      this.annotation = annot;
      this.sourceDocument = sourceDocument;
      this.colorToUse = color;
      setBackground(this.colorToUse);
      this.addMouseListener(new MouseActionListener());
      setFont(new Font(getFont().getName(), Font.PLAIN, TEXT_SIZE));
    }

    /**
     * sets the annotation highlighted/dehighlighted
     * 
     * @param val
     * @param color
     */
    public void setHighlighted(boolean val, Color color) {
      this.highlighted = val;
      this.colorToUse = color;
      this.setBackground(color);
      this.updateUI();
    }

    public boolean isHighlighted() {
      return this.highlighted;
    }

    public void setHighlightColor(Color color) {
      this.colorToUse = color;
      this.setBackground(color);
      this.updateUI();
    }

    public Color getHighlightColor() {
      return this.colorToUse;
    }

    public Dimension getPreferredSize() {
      Dimension superPreferred = super.getPreferredSize();
      if(!showLinks.isSelected())
        return new Dimension((int)Math.min(230, superPreferred.getWidth()),
                (int)superPreferred.getHeight());
      else
        return superPreferred;
    }
    
    /**
     * Implements various mouse events. E.g. what should happen when
     * someone clicks on an unhighlighted annotation etc.
     * 
     * @author niraj
     * 
     */
    protected class MouseActionListener extends MouseInputAdapter {

      public void mouseClicked(MouseEvent me) {
        mouseExited(me);
        AnnotationHighlight ah = (AnnotationHighlight)me.getSource();
        Point pt = me.getPoint();
        currentAnnotationHightlight = ah;

        if(SwingUtilities.isRightMouseButton(me)) {

          if(alignment.isAnnotationAligned(ah.annotation)) {
            // lets clear the latest selection
            clearLatestAnnotationsSelection();
          }

          // we should show the option menu here
          JPopupMenu optionsMenu = new JPopupMenu();
          optionsMenu.setOpaque(true);
          optionsMenu.add(new JLabel("Options"));
          optionsMenu.addSeparator();
          for(JMenuItem item : actions.keySet()) {
            AlignmentAction aa = actions.get(item);
            if(alignment.isAnnotationAligned(ah.annotation)) {
              if(aa.invokeForAlignedAnnotation()) {
                optionsMenu.add(item);
              }
            }
            else if(ah.highlighted) {
              if(aa.invokeForHighlightedUnalignedAnnotation()) {
                optionsMenu.add(item);
              }
            }
            else {
              if(aa.invokeForUnhighlightedUnalignedAnnotation()) {
                optionsMenu.add(item);
              }
            }
          }

          optionsMenu.show(ah, (int)pt.getX(), (int)pt.getY());
          optionsMenu.setVisible(true);
          return;
        }

        // was this annotation highlighted?
        // if yes, remove the highlight
        if(ah.highlighted) {

          // we need to check if the ah is aligned
          // if so, we should prompt user to first reset the
          // alignment
          if(alignment.isAnnotationAligned(ah.annotation)) {
            JOptionPane.showMessageDialog(gate.gui.MainFrame.getInstance(),
                    "To remove this annotation from the current"
                            + " aligment, please use the 'Remove Alignment'"
                            + " from the options menu on right click");
            return;
          }

          // the annotation is not aligned but recently highlighted
          // so remove the highlight
          ah.setHighlighted(false, Color.WHITE);

          if(ah.isSourceDocument()) {
            if(sourceLatestAnnotationsSelection == null) {
              sourceLatestAnnotationsSelection = new ArrayList<Annotation>();
            }

            sourceLatestAnnotationsSelection.remove(ah.annotation);
          }
          else {
            if(targetLatestAnnotationsSelection == null) {
              targetLatestAnnotationsSelection = new ArrayList<Annotation>();
            }

            targetLatestAnnotationsSelection.remove(ah.annotation);
          }
        }
        else {
          if(color == Color.WHITE) color = getColor(null);
          ah.setHighlighted(true, color);
          if(ah.isSourceDocument()) {
            if(sourceLatestAnnotationsSelection == null) {
              sourceLatestAnnotationsSelection = new ArrayList<Annotation>();
            }

            if(!sourceLatestAnnotationsSelection.contains(ah.annotation))
              sourceLatestAnnotationsSelection.add(ah.annotation);
          }
          else {
            if(targetLatestAnnotationsSelection == null) {
              targetLatestAnnotationsSelection = new ArrayList<Annotation>();
            }
            if(!targetLatestAnnotationsSelection.contains(ah.annotation))
              targetLatestAnnotationsSelection.add(ah.annotation);
          }
        }

      }

      JPopupMenu menu = new JPopupMenu();

      FeaturesModel model = new FeaturesModel();

      JTable featuresTable = new JTable(model);

      Timer timer = new Timer();

      TimerTask task;

      public void mouseEntered(final MouseEvent me) {
        final AnnotationHighlight ah = (AnnotationHighlight)me.getSource();
        model.setAnnotation(ah.annotation);
        task = new TimerTask() {
          public void run() {
            menu.add(featuresTable);
            menu.show(ah, me.getX(), me.getY() + 10);
            menu.revalidate();
            menu.updateUI();
          }
        };
        timer.schedule(task, 2000);
      }

      public void mouseExited(MouseEvent me) {
        if(task != null) {
          task.cancel();
        }
        if(menu != null && menu.isVisible()) {
          menu.setVisible(false);
        }
      }
    }

    public boolean isSourceDocument() {
      return sourceDocument;
    }

  }

  /**
   * Introduces transparency to the given color. If c is null, a new
   * random color is generated using the color generater class.
   * 
   * @param c
   * @return
   */
  private Color getColor(Color c) {
    float components[] = null;
    if(c == null)
      components = colorGenerator.getNextColor().getComponents(null);
    else components = c.getComponents(null);

    Color colour = new Color(components[0], components[1], components[2], 0.5f);
    int rgb = colour.getRGB();
    int alpha = colour.getAlpha();
    int rgba = rgb | (alpha << 24);
    colour = new Color(rgba, true);
    return colour;
  }

  /**
   * listens to the annotationsAligned event and updates the GUI
   * accordingly.
   */
  public void annotationsAligned(Annotation srcAnnotation, String srcAS,
          Document srcDocument, Annotation tgtAnnotation, String tgtAS,
          Document tgtDocument) {

    if(srcAnnotation == null || tgtAnnotation == null || srcDocument == null
            || tgtDocument == null) {
      System.err.println("One of the src/tgt annotation/document is null");
      return;
    }

    if(!srcDocument.getName().equals(getSourceDocumentId())) {
      return;
    }
    if(!tgtDocument.getName().equals(getTargetDocumentId())) {
      return;
    }

    AnnotationHighlight srcAH = sourceHighlights.get(srcAnnotation);
    AnnotationHighlight tgtAH = targetHighlights.get(tgtAnnotation);
    if(srcAH == null || tgtAH == null) return;

    // if one of the two is already aligned
    Color toUse = null;
    if(srcAH.highlighted)
      toUse = srcAH.colorToUse;
    else if(tgtAH.highlighted)
      toUse = tgtAH.colorToUse;
    else toUse = getColor(null);
    if(!srcAH.highlighted) {
      srcAH.setHighlighted(true, toUse);
    }

    if(!tgtAH.highlighted) {
      tgtAH.setHighlighted(true, toUse);
    }
    refresh();
  }

  /**
   * listens to the annotationsUnAligned event and updates the GUI
   * accordingly.
   */

  public void annotationsUnaligned(Annotation srcAnnotation, String srcAS,
          Document srcDocument, Annotation tgtAnnotation, String tgtAS,
          Document tgtDocument) {

    if(srcAnnotation == null || tgtAnnotation == null || srcDocument == null
            || tgtDocument == null) {
      System.err.println("One of the src/tgt annotation/document is null");
      return;
    }

    if(!srcDocument.getName().equals(selectedSourceID)) return;

    if(!tgtDocument.getName().equals(selectedTargetID)) return;

    AnnotationHighlight srcAH = sourceHighlights.get(srcAnnotation);
    AnnotationHighlight tgtAH = targetHighlights.get(tgtAnnotation);
    if(srcAH == null || tgtAH == null) return;

    if(srcAH.highlighted) {
      srcAH.setHighlighted(false, Color.WHITE);
    }

    if(tgtAH.highlighted) {
      tgtAH.setHighlighted(false, Color.WHITE);
    }
    refresh();
  }

  /**
   * This method reads the given action and decides whether it should be
   * added to the properties panel or not. It also adds it to the
   * appropriate local data structure in order to invoke them when
   * appropriate.
   * 
   * @param action
   */

  private void readAction(AlignmentAction action) {

    // indicates if this action should be added to the menu
    boolean addToMenu = true;

    if(action.invokeWithAlignAction()) {
      allActions.add(action);
      addToMenu = false;
    }

    if(action.invokeWithRemoveAction()) {
      if(!allActions.contains(action)) {
        allActions.add(action);
      }
      addToMenu = false;
    }

    String caption = action.getCaption();
    Icon icon = action.getIcon();

    if(addToMenu) {

      final JMenuItem menuItem;
      if(icon != null) {
        menuItem = new JMenuItem(caption, icon);
        JMenuItem actionItem = actionsMenuItemByCaption.get(action
                .getIconPath());
        if(actionItem != null) {
          actions.remove(actionItem);
          actionsMenuItemByCaption.remove(action.getIconPath());
        }
        actionsMenuItemByCaption.put(action.getIconPath(), menuItem);
      }
      else {
        menuItem = new JMenuItem(caption);
        JMenuItem actionItem = actionsMenuItemByCaption.get(caption);
        if(actionItem != null) {
          actions.remove(actionItem);
          actionsMenuItemByCaption.remove(caption);
        }
        actionsMenuItemByCaption.put(caption, menuItem);
      }
      if(menuItem != null) {
        menuItem.setToolTipText(action.getToolTip());
        actions.put(menuItem, action);
        menuItem.addActionListener(new ActionListener() {
          public void actionPerformed(ActionEvent ae) {
            executeAction(actions.get(menuItem));
          }
        });
      }
    }
  }

  /**
   * Reads different actions from the actions configuration file.
   * 
   * @param actionsConfFile
   */
  private void readActions(File actionsConfFile) {
    System.out.println("Actions configuration file: "+actionsConfFile.getAbsolutePath());
    
    if(actionsConfFile != null && actionsConfFile.exists()) {
      try {
        BufferedReader br = new BufferedReader(new FileReader(actionsConfFile));
        String line = br.readLine();
        String cName = "";
        while(line != null) {
          // each line will have a class name
          try {
            if(line.trim().startsWith("#") || line.trim().length() == 0) {
              continue;
            }

            int index = line.indexOf(",");
            cName = index < 0 ? line.trim() : line.substring(0, index);
            line = index < 0 ? "" : line.substring(index + 1);

            Class actionClass = Class.forName(cName, true, Gate
                    .getClassLoader());

            Object action = actionClass.newInstance();
            String[] args = line.split("[,]");
            String parentPath = actionsConfFile.getParentFile().getAbsolutePath();
            if(!parentPath.endsWith("/")) {
              parentPath += "/";
            }
            
            for(int i=0;i<args.length;i++) {
              args[i] = args[i].replaceAll("(\\$relpath\\$)", parentPath);
            }
            
            if(action instanceof AlignmentAction) {
              loadAlignmentAction((AlignmentAction)action, args);
            }

            if(action instanceof PreDisplayAction) {
              loadPreDisplayAction((PreDisplayAction)action, args);
            }

            if(action instanceof FinishedAlignmentAction) {
              loadFinishedAlignmentAction((FinishedAlignmentAction)action, args);
            }

            if(action instanceof DataPublisherAction) {
              loadDataPublisherAction((DataPublisherAction)action, args);
            }

          }
          catch(ClassNotFoundException cnfe) {
            System.err.println("class " + cName + " not found!");
            continue;
          }
          catch(IllegalAccessException ilae) {
            System.err.println("class " + cName
                    + " threw the illegal access exception!");
            continue;
          }
          catch(InstantiationException ie) {
            System.err.println("class " + cName + " could not be instantiated");
            continue;
          }
          finally {
            line = br.readLine();
          }
        }
      }
      catch(IOException ioe) {
        throw new GateRuntimeException(ioe);
      }
    }
  }

  private void loadAlignmentAction(AlignmentAction aa, String[] args) {
    try {
      aa.init(args);
    }
    catch(AlignmentActionInitializationException aaie) {
      throw new GateRuntimeException(aaie);
    }

    readAction(aa);
    if(aa.invokeWithAlignAction() || aa.invokeWithRemoveAction()) {
      String title = aa.getCaption();
      if(title == null || title.trim().length() == 0) return;
      PropertyActionCB pab = new PropertyActionCB(title, false, aa);
      pab.setToolTipText(aa.getToolTip());
      actionsCBMap.put(aa, pab);
      int count = propertiesPanel.getComponentCount();
      propertiesPanel.add(pab, count - 1);
      propertiesPane.setVisible(true);
      propertiesPane.validate();
      propertiesPane.updateUI();
      waPanel.validate();
      waPanel.updateUI();
    }
  }

  /**
   * load a finished alignment action
   * 
   * @param faa
   * @param args
   */
  private void loadFinishedAlignmentAction(FinishedAlignmentAction faa,
          String[] args) {
    try {
      faa.init(args);
      finishedAlignmentActions.add(faa);
    }
    catch(AlignmentActionInitializationException aaie) {
      throw new GateRuntimeException(aaie);
    }
  }

  /**
   * load a data publishers action
   * 
   * @param faa
   * @param args
   */
  private void loadDataPublisherAction(final DataPublisherAction dpa,
          String[] args) {
    try {
      dpa.init(args);
      dataPublisherActions.add(dpa);
      SwingUtilities.invokeLater(new Runnable() {
        public void run() {
          DefaultDataModel ddm = new DefaultDataModel(dpa);
          tableTabbedPane.add(dpa.getTableTitle(), new XJTable(
                  ddm));
          dpa.setDataModel(ddm);
        }
      });
    }
    catch(AlignmentActionInitializationException aaie) {
      throw new GateRuntimeException(aaie);
    }
  }

  /**
   * loads a pre-display action.
   * 
   * @param pda
   * @param args
   */
  private void loadPreDisplayAction(PreDisplayAction pda, String[] args) {
    try {
      pda.init(args);
      preDisplayActions.add(pda);
    }
    catch(AlignmentActionInitializationException aaie) {
      throw new GateRuntimeException(aaie);
    }
  }

  /**
   * internal class to represent link between the source and the target
   * alignment unit.
   * 
   * @author gate
   * 
   */
  private class Edge {
    AnnotationHighlight srcAH;

    AnnotationHighlight tgtAH;
  }

  /**
   * If selected, the appropriate AlignmentAction is obtained from the
   * local data-structure and invoked.
   * 
   * @author gate
   * 
   */
  private class PropertyActionCB extends JCheckBox {
    /**
     * Which action to call if the check box is selected
     */
    AlignmentAction aa;

    /**
     * GUI component, that allows selecting, deselecting the action
     */
    JCheckBox thisInstance;

    /**
     * caption
     */
    String key;

    /**
     * Constructor
     * 
     * @param propKey - caption
     * @param value - selected or deselected
     * @param action - to be called if selected
     */
    public PropertyActionCB(String propKey, boolean value,
            AlignmentAction action) {
      super(propKey);
      setSelected(value);
      this.aa = action;
      thisInstance = this;
      key = propKey;
    }
  }

  /**
   * canvas that shows lines for every link present in the current pair
   * 
   * @author gate
   * 
   */
  private class MappingsPanel extends JPanel {

    /**
     * edges to paint
     */
    private Set<Edge> edges = new HashSet<Edge>();

    /**
     * constructor
     */
    public MappingsPanel() {
      // do nothing
      setOpaque(true);
      setBackground(Color.WHITE);
    }

    /**
     * clears the local cache
     */
    public void removeAllEdges() {
      edges.clear();
    }

    /**
     * adds a new edge to the panel
     * 
     * @param edge
     */
    public void addEdge(Edge edge) {
      if(edge != null) edges.add(edge);
    }

    /**
     * draws edges
     */
    public void paintComponent(Graphics g) {
      super.paintComponent(g);
      Graphics2D g2d = (Graphics2D)g;
      g2d.setBackground(Color.WHITE);
      g2d.clearRect(0, 0, this.getWidth(), this.getHeight());

      // but only if showLinks is selected
      if(showLinks.isSelected()) {
        for(Edge e : edges) {
          int x = (int)(e.srcAH.getBounds().x + (double)((double)e.srcAH
                  .getBounds().width / 2));
          int y = 0;
          int x1 = (int)(e.tgtAH.getBounds().x + (double)((double)e.tgtAH
                  .getBounds().width / 2));
          int y1 = this.getBounds().height;
          Line2D line = new Line2D.Double(new Point((int)x, (int)y), new Point(
                  (int)x1, (int)y1));
          Stroke stroke = new BasicStroke(2.0f);
          g2d.setStroke(stroke);
          Color c = g2d.getColor();
          g2d.setColor(e.srcAH.getBackground());
          g2d.draw(line);
          g2d.setColor(c);
        }
      }
    }
  }

  /**
   * represents annotation features. This is used for displaying
   * features of the annotation currently being focused by the mouse
   * pointer.
   * 
   * @author gate
   * 
   */
  public class FeaturesModel extends DefaultTableModel {

    // annotation whoes features need to be displayed
    Annotation toShow;

    /**
     * keys
     */
    ArrayList<String> features;

    /**
     * values
     */
    ArrayList<String> values;

    /**
     * constructor
     */
    public FeaturesModel() {
      super(new String[] {"Feature", "Value"}, 0);
    }

    /**
     * sets the annotation whoes features need to be shown
     * 
     * @param annot
     */
    public void setAnnotation(Annotation annot) {
      features = new ArrayList<String>();
      values = new ArrayList<String>();
      for(Object key : annot.getFeatures().keySet()) {
        features.add(key.toString());
        values.add(annot.getFeatures().get(key).toString());
      }
      super.fireTableDataChanged();
    }

    public Class getColumnClass(int column) {
      return String.class;
    }

    public int getRowCount() {
      return values == null ? 0 : values.size();
    }

    public int getColumnCount() {
      return 2;
    }

    public String getColumnName(int column) {
      switch(column) {
        case 0:
          return "Feature";
        default:
          return "Value";
      }
    }

    public Object getValueAt(int row, int column) {
      switch(column) {
        case 0:
          return features.get(row);
        default:
          return values.get(row);
      }
    }

  }

  /**
   * sets the alignment feature name that should be used to store
   * alignment information
   * 
   * @param alignmentFeatureName
   */
  public void setAlignmentFeatureName(String alignmentFeatureName) {
    document.getAlignmentInformation(alignmentFeatureName);
    alignmentFeatureNames.setSelectedItem(alignmentFeatureName);
    if(!alignmentFeatureNames.getSelectedItem().equals(alignmentFeatureName)) {
      alignmentFeatureNames.setEditable(true);
      alignmentFeatureNames.addItem(alignmentFeatureNames);
      alignmentFeatureNames.setSelectedItem(alignmentFeatureName);
      alignmentFeatureNames.setEditable(false);
    }
  }

  // setter methods
  public void setSourceDocumentId(String docId) {
    sourceDocumentId.setSelectedItem(docId);
    selectedSourceID = docId;
  }

  public void setTargetDocumentId(String docId) {
    targetDocumentId.setSelectedItem(docId);
    selectedTargetID = docId;
  }

  public void setSourceAnnotationSetName(String annotSet) {
    if(annotSet == null) {
      sourceASName.setSelectedItem("<null>");
    }
    else {
      sourceASName.setSelectedItem(annotSet);
    }
  }

  public void setTargetAnnotationSetName(String annotSet) {
    if(annotSet == null) {
      targetASName.setSelectedItem("<null>");
    }
    else {
      targetASName.setSelectedItem(annotSet);
    }
  }

  public void setSourceUnitOfAlignment(String unit) {
    sourceUnitOfAlignment.setSelectedItem(unit);
  }

  public void setTargetUnitOfAlignment(String unit) {
    targetUnitOfAlignment.setSelectedItem(unit);
  }

  public void setSourceParentOfUnitOfAlignment(String unit) {
    sourceParentOfUnitOfAlignment.setSelectedItem(unit);
  }

  public void setTargetParentOfUnitOfAlignment(String unit) {
    targetParentOfUnitOfAlignment.setSelectedItem(unit);
  }

  public void disableUserSelections(boolean disableUserSelections) {
    this.disableUserSelections = disableUserSelections;
    if(!this.disableUserSelections) {
      populate.setEnabled(false);
      loadActions.setEnabled(false);
      saveDocument.setEnabled(false);
      sourceUnitOfAlignment.setEnabled(false);
      targetUnitOfAlignment.setEnabled(false);
      sourceParentOfUnitOfAlignment.setEnabled(false);
      targetParentOfUnitOfAlignment.setEnabled(false);
      sourceASName.setEnabled(false);
      targetASName.setEnabled(false);
      sourceDocumentId.setEnabled(false);
      targetDocumentId.setEnabled(false);
      alignmentFeatureNames.setEnabled(false);
    }
    else {
      populate.setEnabled(true);
      loadActions.setEnabled(true);
      saveDocument.setEnabled(true);
      alignmentFeatureNames.setEnabled(true);
      populate();
    }
  }

  boolean disableUserSelections = false;

  /**
   * Saves the compound document in a single XML document.
   * 
   * @author gate
   * 
   */
  public class SaveAsASingleXML extends AbstractAction {

    private static final long serialVersionUID = -1377052643002026640L;

    public SaveAsASingleXML() {
      super("Save");
    }

    public void actionPerformed(ActionEvent ae) {
      CompoundDocument cd = (CompoundDocument)document;

      JFileChooser fileChooser = MainFrame.getFileChooser();
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

  // getter methods
  public String getSourceParentOfUnitOfAlignment() {
    return sourceParentOfUnitOfAlignment.getSelectedItem().toString();
  }

  public String getTargetParentOfUnitOfAlignment() {
    return targetParentOfUnitOfAlignment.getSelectedItem().toString();
  }

  public String getSourceUnitOfAlignment() {
    return sourceUnitOfAlignment.getSelectedItem().toString();
  }

  public String getTargetUnitOfAlignment() {
    return targetUnitOfAlignment.getSelectedItem().toString();
  }

  public String getSourceDocumentId() {
    return selectedSourceID;
  }

  public String getTargetDocumentId() {
    return selectedTargetID;
  }

  public String getSourceAnnotationSetName() {
    String as = sourceASName.getSelectedItem().toString();
    return as.equals("<null>") ? null : as;
  }

  public String getTargetAnnotationSetName() {
    String as = targetASName.getSelectedItem().toString();
    return as.equals("<null>") ? null : as;
  }

}
