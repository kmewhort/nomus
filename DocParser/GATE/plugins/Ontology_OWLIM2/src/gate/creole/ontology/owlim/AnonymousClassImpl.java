/*
 *  AnnonymousClassImpl.java
 *
 *  Niraj Aswani, 09/March/07
 *
 *  $Id: AnonymousClassImpl.java 11598 2009-10-13 13:44:17Z johann_p $
 */
package gate.creole.ontology.owlim;

import gate.creole.ontology.AnonymousClass;
import gate.creole.ontology.Ontology;
import gate.creole.ontology.URI;

/**
 * Implementation of the AnonymousClass
 * @author niraj
 */
public class AnonymousClassImpl extends OClassImpl implements AnonymousClass {
  /**
   * Constructor
   * @param aURI
   * @param ontology
   * @param repositoryID
   * @param owlimPort
   */
  public AnonymousClassImpl(URI aURI, Ontology ontology, String repositoryID,
          OWLIM owlimPort) {
    super(aURI, ontology, repositoryID, owlimPort);
  }
}
