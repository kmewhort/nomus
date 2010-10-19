// Subclass of Lucene Similarity which disables field length normalization
// (otherwise the short CaseName and Citation fields vastly outweight the full text fields)
//
// Note: apache-solr-*.jar and lucene-*.jar need to be in classpath
// To use:
//  1. create a /lib directory in solr home and place the JAR in it
//  2. Add <similarity class="NomusSolrPlugins.NomusSimilarity"/> to the conf/schema.xml

package NomusSolrPlugins;

import org.apache.lucene.search.DefaultSimilarity;
import org.apache.solr.util.plugin.MapInitializedPlugin;
import java.util.Map;

public class NomusSimilarity extends DefaultSimilarity implements MapInitializedPlugin
{
  public void init( Map<String,String> args )
  {}

  // overide lengthNorm
  public float lengthNorm(String fieldName, int numTerms)
  {
	// original returns the inverse of sqrt(numTerms)
    //return (float)(1.0 / Math.sqrt(numTerms));

	// instead, return 1.0
	return (float)1.0;
  }
}
