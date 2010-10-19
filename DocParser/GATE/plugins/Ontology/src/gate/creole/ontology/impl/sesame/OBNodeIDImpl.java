/*
 *  Copyright (c) 1995-2010, The University of Sheffield. See the file
 *  COPYRIGHT.txt in the software or at http://gate.ac.uk/gate/COPYRIGHT.txt
 *
 *  This file is part of GATE (see http://gate.ac.uk/), and is free
 *  software, licenced under the GNU Library General Public License,
 *  Version 2, June 1991 (in the distribution as file licence.html,
 *  and also available at http://gate.ac.uk/gate/licence.html).
 *
 *  Johann Petrak 2009-08-13
 *
 *  $Id: OBNodeIDImpl.java 12006 2009-12-01 17:24:28Z thomas_heitz $
 */

package gate.creole.ontology.impl.sesame;

import gate.creole.ontology.OBNodeID;
import gate.creole.ontology.OURI;

/**
 *
 * @author johann
 */
public class OBNodeIDImpl extends ONodeIDImpl implements OBNodeID {
  public OBNodeIDImpl(String uri) {
    super(uri,true);
  }
}
