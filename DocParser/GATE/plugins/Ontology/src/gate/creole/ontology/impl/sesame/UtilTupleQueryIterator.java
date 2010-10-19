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
 *  $Id: UtilTupleQueryIterator.java 12602 2010-05-06 14:28:51Z ian_roberts $
 */
package gate.creole.ontology.impl.sesame;

import gate.creole.ontology.GateOntologyException;
import gate.creole.ontology.LiteralOrONodeID;
import gate.creole.ontology.Literal;
import gate.creole.ontology.ONodeID;
import gate.creole.ontology.OConstants;
import gate.creole.ontology.OntologyTupleQuery;
import gate.creole.ontology.impl.LiteralOrONodeIDImpl;
import java.util.List;
import java.util.Vector;
import org.apache.log4j.Logger;
import org.openrdf.model.BNode;
import org.openrdf.model.URI;
import org.openrdf.model.Value;
import org.openrdf.query.BindingSet;
import org.openrdf.query.QueryEvaluationException;
import org.openrdf.query.QueryLanguage;
import org.openrdf.query.TupleQuery;
import org.openrdf.query.TupleQueryResult;
import org.openrdf.repository.RepositoryConnection;

/**
 * A class representing a Sesame Query. This class makes it easy to
 * iterate over query results, reuse a query with different variable
 * bindings, and provides special methods for queries where only one
 * column is retrieved.
 * <p>
 * The following steps should be carried out when using this class:
 * <ol>
 * <li>Create an object and pass on the repository connection, query string,
 * and query language constant
 * <li>Set any variables for the query using {@link #setBinding} one or more times
 * <li>Evaluate the query using {@link evaluate}. This is optional, if not done,
 * it will be done implicitly in the first call to {@link #hasNext}.
 * <li>Retrieve query results one by one using one of the next... methods
 * <li>If no more results are needed the query MUST BE CLOSED using the
 * {@link #close} function to free the query resources. However, if all
 * results have been returned and {@link #hasNext()} has returned false,
 * the query has been closed automatically. It is allowed to call close
 * on a closed query.
 * <li>To reuse the query, repeat from step 2
 * </ol>
 *
 * @author Johann Petrak
 */
 public class UtilTupleQueryIterator implements OntologyTupleQuery {

    private RepositoryConnection mRepositoryConnection;
    private String mQuery;
    private TupleQueryResult mResult;
    private TupleQuery mTupleQuery;
    private Vector<String> mVarnames;
    private QueryLanguage mLang = QueryLanguage.SERQL;
    private boolean mIsClosed = true;
    private boolean mIsPrepared = false;
    private boolean mIsEvaluated = false;
    private Logger logger;

    public UtilTupleQueryIterator(RepositoryConnection repositoryConnection,
        String query, OConstants.QueryLanguage queryLanguage) {
      if(queryLanguage == OConstants.QueryLanguage.SPARQL) {
        mLang = QueryLanguage.SPARQL;
      } else if(queryLanguage == OConstants.QueryLanguage.SERQL) {
        mLang = QueryLanguage.SERQL;
      } else {
        throw new GateOntologyException("Query language must be SPARQL or SERQL, got "+queryLanguage);
      }
      //System.out.println("Created tuple query object: "+query);
      logger = Logger.getLogger(this.getClass().getName());
      mRepositoryConnection = repositoryConnection;
      logger.debug("Creating query: " + query);
      mQuery = query;
      prepare();

    }

    public void prepare() {
      try {
        mTupleQuery = mRepositoryConnection.prepareTupleQuery(mLang, mQuery);
      } catch (Exception e) {
        throw new GateOntologyException("Cannot prepare tuple query: "+mQuery, e);
      }
      mIsPrepared = true;
    }

    /**
     *
     * @param name
     * @param value
     */
    public void setBinding(String name, Value value) {
      mTupleQuery.setBinding(name, value);
    }
    public void setBinding(String name, LiteralOrONodeID value) {
      mTupleQuery.setBinding(name, UtilConvert.toSesameValue(value));
    }
    public void setBinding(String name, Literal value) {
      mTupleQuery.setBinding(name, UtilConvert.toSesameLiteral(value));
    }
    public void setBinding(String name, ONodeID value) {
      mTupleQuery.setBinding(name, UtilConvert.toSesameValue(value));
    }

    /**
     * 
     */
    public void evaluate() {
      close();
      if (!mIsPrepared) {
        prepare();
      }
      try {
          mResult = mTupleQuery.evaluate();
        mIsClosed = false;
        List<String> varnamesList = mResult.getBindingNames();
        mVarnames = new Vector<String>(varnamesList);
      } catch (Exception e) {
        throw new GateOntologyException("Cannot evaluate queyr: "+mQuery, e);
      }
      mIsEvaluated = true;
    }

    public boolean hasNext() {
      if (!mIsPrepared) {
        prepare();
      }
      if (!mIsEvaluated) {
        evaluate();
      }
      try {
        boolean hasnext = mResult.hasNext();
        if(!hasnext) {
          close();
        }
        return hasnext;
      } catch (QueryEvaluationException e) {
        throw new GateOntologyException("Error checking for next result of query", e);
      }
    }

    public String nextFirstAsString() {
      String ret = nextFirstAsValue().stringValue();
      //logger.debug("QS: "+ret);
      return ret;
    }

    public LiteralOrONodeID nextFirst() {
      Value v = nextFirstAsValue();
      if(v instanceof BNode) {
        return new LiteralOrONodeIDImpl(new OBNodeIDImpl(v.stringValue()));
      } else if(v instanceof org.openrdf.model.Literal) {
        return new LiteralOrONodeIDImpl(
            UtilConvert.toGateLiteral((org.openrdf.model.Literal)v));
      } else if(v instanceof org.openrdf.model.URI) {
        URI u = (URI)v;
        // TODO: check if we want toString or stringValue() here
        return new LiteralOrONodeIDImpl(new OURIImpl(u.stringValue()));
      }
      return null;
    }


    public Value nextFirstAsValue() {
      if (mResult == null) {
        throw new GateOntologyException("No prepared query available");
      }
      Value ret;
      try {
        BindingSet bindingSet = mResult.next();
        ret = bindingSet.getValue(mVarnames.get(0));
      } catch (QueryEvaluationException e) {
        throw new GateOntologyException("Could not get next query result", e);
      }
      //logger.debug("QV: "+ret);
      return ret;
    }

    public Vector<Value> nextAsValue() {
      if(!hasNext()) {
        throw new GateOntologyException("No more query results but next was called");
      }
      Vector<Value> result = new Vector<Value>();
      try {
        BindingSet bindingSet = mResult.next();
        for (String bindingName : mVarnames) {
          Value value = bindingSet.getValue(bindingName);
          result.add(value);
        }
      } catch (QueryEvaluationException e) {
        throw new GateOntologyException("Could not get next query result", e);
      }
      logger.debug("Qvec: " + result);
      return result;

    }


   public Vector<String> nextAsString() {
     if (!hasNext()) {
       throw new GateOntologyException("No more query results but next was called");
     }
     Vector<String> result = new Vector<String>();
     try {
       BindingSet bindingSet = mResult.next();
       for (String bindingName : mVarnames) {
         Value value = bindingSet.getValue(bindingName);
         String val = value.stringValue();
         result.add(val);
       }
     } catch (QueryEvaluationException e) {
       throw new GateOntologyException("Could not get next query result", e);
     }
     return result;
   }

    public Vector<LiteralOrONodeID> next() {
      if(!hasNext()) {
        throw new GateOntologyException("No more query results but next was called");
      }
      Vector<LiteralOrONodeID> result = new Vector<LiteralOrONodeID>();

      try { 
        BindingSet bindingSet = mResult.next();
        for (String bindingName : mVarnames) {
          Value value = bindingSet.getValue(bindingName);
          if(value instanceof BNode) {
            result.add(new LiteralOrONodeIDImpl(new OBNodeIDImpl(value.stringValue())));
          } else if(value instanceof org.openrdf.model.Literal) {
             result.add(new LiteralOrONodeIDImpl(
                 UtilConvert.toGateLiteral((org.openrdf.model.Literal)value)));
          } else if(value instanceof org.openrdf.model.URI) {
             URI u = (URI)value;
             result.add(new LiteralOrONodeIDImpl(new OURIImpl(u.stringValue())));
          }
        }
      } catch (QueryEvaluationException e) {
        throw new GateOntologyException("Could not get next query result", e);
      }
      logger.debug("Qvec: " + result);
      return result;

    }



    public void close() {
      try {
        if (!mIsClosed) {
          mResult.close();
          mIsEvaluated = false;
          mIsClosed = true;
        }
      } catch (Exception e) {
        throw new GateOntologyException("Error closing query " + mQuery, e);
      }
    }

    public void remove() {
      throw new UnsupportedOperationException("remove method not supported");
    }
  }

