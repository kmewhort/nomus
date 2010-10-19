package gate.yahoo;

import com.yahoo.search.*;
import java.io.IOException;

/**
 * Java API to perform a web search. This code is taken from YAHOO website,
 * which describes how to use YAHOO engine.
 * 
 * @author Ryan Kennedy
 */
public class YahooSearch {
  private String applicationID;

  private SearchClient client;

  private String format = ALL;

  public static final String ALL = "all";

  public static final String HTML = "html";

  public static final String MSWORD = "msword";

  public static final String PDF = "pdf";

  public static final String PPT = "ppt";

  public static final String RSS = "rss";

  public static final String TXT = "txt";

  public static final String XLS = "xls";

  public YahooSearch(String applicationID) {
    this.applicationID = applicationID;
    init();
  }

  public void init() {
    client = new SearchClient(this.applicationID);
  }

  public WebSearchResult[] search(String query, int limit)
          throws SearchException, IOException {
    WebSearchRequest request = new WebSearchRequest(query);
    request.setResults(limit);
    WebSearchResults results = client.webSearch(request);
    if(results == null || results.listResults() == null
            || results.listResults().length == 0) { return null; }
    return results.listResults();
  }

  public String getApplicationID() {
    return applicationID;
  }

  public void setApplicationID(String applicationID) {
    this.applicationID = applicationID;
  }

  public String getFormat() {
    return format;
  }

  public void setFormat(String format) {
    if(!format.equals(HTML) && !format.equals(MSWORD) && !format.equals(PDF)
            && !format.equals(PPT) && !format.equals(RSS)
            && !format.equals(TXT) && !format.equals(XLS)) {
      this.format = ALL;
    } else {
      this.format = format;
    }
  }
}