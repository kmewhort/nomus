/*
 *  OClassImpl.java
 *
 *  Niraj Aswani, 09/March/07
 *
 *  $Id: OClassImpl.java 12549 2010-04-26 13:52:40Z ian_roberts $
 */
package gate.creole.ontology.owlim;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;
import gate.creole.ontology.OClass;
import gate.creole.ontology.OConstants;
import gate.creole.ontology.Ontology;
import gate.creole.ontology.URI;
import gate.creole.ontology.OConstants.Closure;
import gate.util.ClosableIterator;

/**
 * Implementation of the OClass interface
 * 
 * @author niraj
 * 
 */
public class OClassImpl extends OResourceImpl implements OClass {
  /**
   * Constructor
   * 
   * @param aURI
   * @param ontology
   * @param repositoryID
   * @param owlimPort
   */
  public OClassImpl(URI aURI, Ontology ontology, String repositoryID,
          OWLIM owlimPort) {
    super(aURI, ontology, repositoryID, owlimPort);
  }

  /*
   * (non-Javadoc)
   * 
   * @see
   * gate.creole.ontology.OClass#addSubClass(gate.creole.ontology.OClass
   * )
   */
  public void addSubClass(OClass subClass) {
    // lets first check if the current class is a subclass of the
    // subClass. If so,
    // we don't allow this.
    if(this == subClass) {
      Utils
              .warning("addSubClass(subClass) : The super and sub classes are same.");
      return;
    }

    if(this.isSubClassOf(subClass, OConstants.TRANSITIVE_CLOSURE)) {
      Utils.warning(subClass.getURI().toString() + " is a super class of "
              + this.getURI().toString());
      return;
    }

    if(this.isSuperClassOf(subClass, OConstants.DIRECT_CLOSURE)) {
      Utils.warning(subClass.getURI().toString()
              + " is already a sub class of " + this.getURI().toString());
      return;
    }

    owlim.addSubClass(this.repositoryID, this.uri.toString(), subClass.getURI()
            .toString());
    ontology.fireResourceRelationChanged(this, subClass,
            OConstants.SUB_CLASS_ADDED_EVENT);
  }

  /*
   * (non-Javadoc)
   * 
   * @see
   * gate.creole.ontology.OClass#removeSubClass(gate.creole.ontology
   * .OClass)
   */
  public void removeSubClass(OClass subClass) {

    if(this == subClass) {
      Utils
              .warning("addSubClass(subClass) : The super and sub classes are same.");
      return;
    }

    if(!subClass.isSubClassOf(this, OConstants.DIRECT_CLOSURE)) {
      Utils.warning(subClass.getURI().toString()
              + " is not a direct subclass of " + this.getURI().toString());
      return;
    }

    owlim.removeSubClass(this.repositoryID, this.uri.toString(), subClass
            .getURI().toString());
    ontology.fireResourceRelationChanged(this, subClass,
            OConstants.SUB_CLASS_REMOVED_EVENT);
  }

  /*
   * (non-Javadoc)
   * 
   * @see gate.creole.ontology.OClass#getSubClasses(byte)
   */
  public Set<OClass> getSubClasses(byte closure) {
    ResourceInfo[] subClasses = owlim.getSubClasses(this.repositoryID, this.uri
            .toString(), closure);
    Set<OClass> oClasses = new HashSet<OClass>();
    for(int i = 0; i < subClasses.length; i++) {
      oClasses
              .add(Utils.createOClass(this.repositoryID, this.ontology,
                      this.owlim, subClasses[i].getUri(), subClasses[i]
                              .getClassType()));
    }
    return oClasses;
  }

  public Set<OClass> getSubClasses(Closure closure) {
    byte bclosure = closure == Closure.DIRECT_CLOSURE
            ? OConstants.DIRECT_CLOSURE
            : OConstants.TRANSITIVE_CLOSURE;
    return getSubClasses(bclosure);
  }

  public ClosableIterator<OClass> getSubClassesIterator(Closure closure) {
    byte bclosure = closure == Closure.DIRECT_CLOSURE
            ? OConstants.DIRECT_CLOSURE
            : OConstants.TRANSITIVE_CLOSURE;
    return new ResourceIterator<OClass>(getSubClasses(bclosure));
  }

  /*
   * (non-Javadoc)
   * 
   * @see gate.creole.ontology.OClass#getSuperClasses(byte)
   */
  public Set<OClass> getSuperClasses(byte closure) {
    ResourceInfo[] superClasses = owlim.getSuperClasses(this.repositoryID,
            this.uri.toString(), closure);
    Set<OClass> oClasses = new HashSet<OClass>();
    for(int i = 0; i < superClasses.length; i++) {
      oClasses.add(Utils.createOClass(this.repositoryID, this.ontology,
              this.owlim, superClasses[i].getUri(), superClasses[i]
                      .getClassType()));
    }
    return oClasses;
  }

  public Set<OClass> getSuperClasses(Closure closure) {
    byte bclosure = closure == Closure.DIRECT_CLOSURE
            ? OConstants.DIRECT_CLOSURE
            : OConstants.TRANSITIVE_CLOSURE;
    return getSuperClasses(bclosure);
  }

  /*
   * (non-Javadoc)
   * 
   * @see
   * gate.creole.ontology.OClass#isSuperClassOf(gate.creole.ontology
   * .OClass, byte)
   */
  public boolean isSuperClassOf(OClass aClass, byte closure) {
    return owlim.isSuperClassOf(this.repositoryID, this.uri.toString(), aClass
            .getURI().toString(), closure);
  }

  public boolean isSuperClassOf(OClass aClass, Closure closure) {
    byte bclosure = closure == Closure.DIRECT_CLOSURE
    ? OConstants.DIRECT_CLOSURE
    : OConstants.TRANSITIVE_CLOSURE;
    return isSuperClassOf(aClass, bclosure);
  }

  /*
   * (non-Javadoc)
   * 
   * @see
   * gate.creole.ontology.OClass#isSubClassOf(gate.creole.ontology.OClass
   * , byte)
   */
  public boolean isSubClassOf(OClass aClass, byte closure) {
    return owlim.isSubClassOf(this.repositoryID, aClass.getURI().toString(),
            this.uri.toString(), closure);
  }

  public boolean isSubClassOf(OClass aClass, Closure closure) {
    byte bclosure = closure == Closure.DIRECT_CLOSURE
    ? OConstants.DIRECT_CLOSURE
    : OConstants.TRANSITIVE_CLOSURE;
    return isSubClassOf(aClass, bclosure);
  }

  /*
   * (non-Javadoc)
   * 
   * @see gate.creole.ontology.OClass#isTopClass()
   */
  public boolean isTopClass() {
    return owlim.isTopClass(this.repositoryID, this.uri.toString());
  }

  /*
   * (non-Javadoc)
   * 
   * @see
   * gate.creole.ontology.OClass#setSameClassAs(gate.creole.ontology
   * .OClass)
   */
  public void setEquivalentClassAs(OClass theClass) {
    // lets first check if the current class is a subclass of the
    // subClass. If so,
    // we don't allow this.
    if(this == theClass) {
      Utils
              .warning("setEquivalentClassAs(theClass) : Both the source and the argument classes refer to the same instance of class");
      return;
    }

    owlim.setEquivalentClassAs(this.repositoryID, this.uri.toString(), theClass
            .getURI().toString());
    ontology.fireResourceRelationChanged(this, theClass,
            OConstants.EQUIVALENT_CLASS_EVENT);
  }

  /*
   * (non-Javadoc)
   * 
   * @see gate.creole.ontology.OClass#getSameClasses()
   */
  public Set<OClass> getEquivalentClasses() {
    ResourceInfo[] eqClasses = owlim.getEquivalentClasses(this.repositoryID,
            this.uri.toString());
    Set<OClass> oClasses = new HashSet<OClass>();
    for(int i = 0; i < eqClasses.length; i++) {
      oClasses.add(Utils.createOClass(this.repositoryID, this.ontology,
              this.owlim, eqClasses[i].getUri(), eqClasses[i].getClassType()));
    }
    return oClasses;
  }

  /*
   * (non-Javadoc)
   * 
   * @see
   * gate.creole.ontology.OClass#isSameClassAs(gate.creole.ontology.
   * OClass)
   */
  public boolean isEquivalentClassAs(OClass aClass) {
    return owlim.isEquivalentClassAs(this.repositoryID, this.uri.toString(),
            aClass.getURI().toString());
  }

  /**
   * Gets the super classes, and returns them in an array list where on
   * each index there is a collection of the super classes at distance -
   * the index.
   */
  public ArrayList<Set<OClass>> getSuperClassesVSDistance() {
    ArrayList<Set<OClass>> result = new ArrayList<Set<OClass>>();
    Set<OClass> set;
    int level = 0;
    OClass c;
    Set<OClass> levelSet = new HashSet<OClass>();
    levelSet.add(this);
    boolean rollon = (0 < owlim.getSuperClasses(this.repositoryID, this.uri
            .toString(), OConstants.DIRECT_CLOSURE).length);
    while(rollon) {
      set = new HashSet<OClass>();
      Iterator<OClass> li = levelSet.iterator();
      while(li.hasNext()) {
        c = li.next();
        set.addAll(c.getSuperClasses(OConstants.DIRECT_CLOSURE));
      }
      if(0 < set.size()) {
        result.add(level++, set);
      }
      levelSet = set;
      rollon = 0 < levelSet.size();
    }
    return result;
  }

  /**
   * Gets the sub classes, and returns them in an array list where on
   * each index there is a collection of the sub classes at distance -
   * the index.
   */
  public ArrayList<Set<OClass>> getSubClassesVsDistance() {
    ArrayList<Set<OClass>> result = new ArrayList<Set<OClass>>();
    Set<OClass> set;
    int level = 0;
    OClass c;
    Set<OClass> levelSet = new HashSet<OClass>();
    levelSet.add(this);
    boolean rollon = (0 < owlim.getSubClasses(this.repositoryID, this.uri
            .toString(), OConstants.DIRECT_CLOSURE).length);
    while(rollon) {
      set = new HashSet<OClass>();
      Iterator<OClass> li = levelSet.iterator();
      while(li.hasNext()) {
        c = li.next();
        set.addAll(c.getSubClasses(OConstants.DIRECT_CLOSURE));
      }
      if(0 < set.size()) {
        result.add(level++, set);
      }
      levelSet = set;
      rollon = 0 < levelSet.size();
    }
    return result;
  }
}
