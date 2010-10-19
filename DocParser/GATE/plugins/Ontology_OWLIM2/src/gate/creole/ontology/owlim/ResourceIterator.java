package gate.creole.ontology.owlim;

import java.util.Iterator;
import java.util.Set;

import gate.creole.ontology.OResource;
import gate.util.ClosableIterator;

public class ResourceIterator<T extends OResource> implements ClosableIterator<T> {

  Iterator<T> iterator;
  
  public ResourceIterator(Set<T> set) {
    this.iterator = set.iterator();
  }

  public void close() {
    iterator = null;
    // do nothing
  }

  public boolean hasNext() {
    return iterator.hasNext();
  }

  public T next() {
    return iterator.next();
  }

  public void remove() {
    iterator.remove();
    
  }

}