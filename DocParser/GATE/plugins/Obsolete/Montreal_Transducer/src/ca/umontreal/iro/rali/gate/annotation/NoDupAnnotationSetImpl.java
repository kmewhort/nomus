/*
 *  NoDupAnnotationSetImpl.java
 *
 *  Copyright (c) 2004, Université de Montréal.
 *
 *  This file uses GATE libraries (see http://gate.ac.uk), and is free
 *  software, licenced under the GNU Library General Public License,
 *  Version 2, June 1991 (in the distribution as file licence.html,
 *  and also available at http://gate.ac.uk/gate/licence.html).
 *
 *  Luc Plamondon, 16/03/2004.
 *
 *  $Id$
 */
package ca.umontreal.iro.rali.gate.annotation;

import gate.*;
import gate.annotation.*;
import gate.util.*;
import java.util.*;

/**
 * A special kind of AnnotationSet that checks whether an identical annotation
 * (same type, features, start offset and end offset) already exists before
 * adding an annotation to the set, so that no duplicate annotation is created.
 * 
 * This behaviour can be disabled by setting authoriseDuplicates to false.
 * 
 * When constructing a new object from another AnnotationSet object, duplicates
 * are copied as is. Only additions made afterwards are subject to duplicate
 * control.
 */
public class NoDupAnnotationSetImpl extends gate.annotation.AnnotationSetImpl {
  private Boolean authoriseDuplicates = Boolean.TRUE;

  /** Construction from Document. */
  public NoDupAnnotationSetImpl(Document doc) {
    super(doc);
    authoriseDuplicates = Boolean.FALSE;
  } // construction from document

  /** Construction from Document and name. */
  public NoDupAnnotationSetImpl(Document doc, String name) {
    super(doc, name);
    authoriseDuplicates = Boolean.FALSE;
  } // construction from document and name

  /**
   * Construction from AnnotationSet. Do not check for duplicate annotations:
   * copy them as is.
   */
  public NoDupAnnotationSetImpl(AnnotationSet c) throws ClassCastException {
    super(c);
    authoriseDuplicates = Boolean.FALSE;
  }

  /** Add an existing annotation. Returns true when the set is modified. */
  public boolean add(Annotation a) throws ClassCastException {
    if(authoriseDuplicates.booleanValue()) {
      return super.add(a);
    } else {
      AnnotationSet subset = getStrict(a.getStartNode().getOffset(), a
              .getEndNode().getOffset());
      if(subset != null) {
        AnnotationSet subsubset = subset.get(a.getType(), a.getFeatures());
        if(subsubset != null && !subsubset.isEmpty()) {
          // Identical annotation already exists; don't add for real
          // ... but before, workaround a bug in AnnotationSetImpl.remove
          // that leaves previously removed annots in annotsByOffsetIndex
          // and that removes them only from annotsById
          for(Iterator iter = subsubset.iterator(); iter.hasNext();) {
            if(get(((Annotation)iter.next()).getId()) != null) { return false; }
          }
        }
      }
      return super.add(a);
    }
  } // add(a)

  /**
   * Get the value of authoriseDuplicates.
   * 
   * @return value of authoriseDuplicates.
   */
  public Boolean getAuthoriseDuplicates() {
    return authoriseDuplicates;
  }

  /**
   * Set the value of authoriseDuplicates.
   * 
   * @param authoriseDuplicates
   *          value of authoriseDuplicates.
   */
  public void setAuthoriseDuplicates(Boolean authorization) {
    authoriseDuplicates = authorization;
  }
}
