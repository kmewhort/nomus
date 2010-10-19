/*
* AnnotationsByOffset.java
* Copyright:    Copyright (c) 2005, Ontotext Lab.
* Company:      Ontotext Lab.
* Krasimir Angelov 12/2005 */

package com.ontotext.gate.japec;

import java.util.*;

public class AnnotationsByOffset implements Comparable
{
  long offset;
  List annotations;

  public AnnotationsByOffset(long offset)
  {
    this.offset      = offset;
    this.annotations = new ArrayList();
  }

  public int compareTo(Object obj) {
    if (obj instanceof AnnotationsByOffset) {
      if (obj == this) return 0;

      AnnotationsByOffset other = (AnnotationsByOffset) obj;
      if (offset < other.offset) return -1;
      else if(offset > other.offset) return 1;
      else return 0;
    } else
        throw new ClassCastException(
                "Attempt to compare a AnnotationsByOffset object to an object " +
                "of type " + obj.getClass()+"!");
  }
}
