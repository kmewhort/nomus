import java.io.IOException;
import java.io.PrintWriter;
import java.io.FileWriter;
import java.io.FileReader;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileOutputStream;
import java.io.BufferedOutputStream;
import java.io.OutputStreamWriter;
import java.io.StringReader;
import java.lang.Thread;
import java.io.OutputStream;
import java.io.InputStream;
import java.io.StringReader;
import java.io.StringWriter;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.Iterator;
import java.util.List;
import java.util.Arrays;
import java.util.Hashtable;
import java.util.UUID;
import java.util.Calendar;
import java.util.Map;
import java.net.URL;
import java.net.URLDecoder;
import java.net.MalformedURLException;
import java.util.Collection;
import java.lang.ClassCastException;

import javax.servlet.ServletConfig;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;


import org.apache.commons.io.FilenameUtils;
import org.apache.commons.io.FileUtils;
import org.apache.commons.io.IOUtils;

import org.jdom.Element;
import org.jdom.input.SAXBuilder;
import org.jdom.output.XMLOutputter;
import org.jdom.JDOMException;


import gate.Document;
import gate.Corpus;
import gate.CorpusController;
import gate.ProcessingResource;
import gate.AnnotationSet;
import gate.Annotation;
import gate.Gate;
import gate.Factory;
import gate.util.*;
import gate.util.persistence.PersistenceManager;
import gate.corpora.DocumentXmlUtils;
import gate.FeatureMap;
import gate.corpora.RepositioningInfo;
import gate.GateConstants;
import gate.creole.SerialAnalyserController;

import org.w3c.tidy.Tidy;


/**
 * Servlet implementation class DocParser
 */
public class DocParser extends HttpServlet
{			
	// debugging on/off
	private boolean debug = false;
	
	// status and data of each user request being processed,
	// keyed by user id
	private Hashtable requests = new Hashtable();
	
	// representation of a spidering request
	static class Request {
		public String userId;
		public SerialAnalyserController gateApplication;		};
  	
    /**
     * Default constructor. 
     */
   public DocParser()
   {
   }

	public void init(ServletConfig config) throws ServletException
	{
		super.init(config);

		try
		{
			// initialise GATE - this must be done before calling any GATE APIs
		   Gate.init();
		   
		   // load ANNIE (to load the JAPE transducer plugin)
		   Gate.getCreoleRegister().registerDirectories(
		   	new URL(Gate.getGateHome().toURL(), "plugins/ANNIE"));
			
		}
		catch(Exception e)
		{
			System.out.println("Error initializing: " +e.toString());
		}
	}

	/**
	 * @see HttpServlet#doGet(HttpServletRequest request, HttpServletResponse response)
	 */
	protected void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException
	{
		doPost(request, response);
	}

	/**
	 * @see HttpServlet#doPost(HttpServletRequest request, HttpServletResponse response)
	 */
	protected void doPost(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException
	{
		// For all responses, prevent cached respone from being used
		response.setHeader("Cache-Control","no-cache"); //HTTP 1.1
		response.setHeader("Pragma","no-cache"); //HTTP 1.0x
		response.setDateHeader ("Expires", 0); //prevent caching at the proxy server
			
		//
		// Get any posted input data
		//
		
		// read in any posted data
		byte[] postBuf = IOUtils.toByteArray(request.getReader());
		
		// Get the request type
		String requestType = getParameter(request, "req", null);
		if(requestType == null)
		{
			response.getWriter().write("<Error>No request type (req) parameter specified or unknown request type.</Error>\n");
			return;
		}
		
		// whether to output on server-side debug messages
		String debugStr = getParameter(request, "debug", null);
		if(debugStr != null)
		{
			if(debugStr.equals("true"))
				debug = true;
			else
				debug = false;
		}
		
			
		// userid 
		String userId = getParameter(request, "user", null);
		if(userId == null)
		{
			if(debug)
				System.out.print("Error: No user id specified.\r\n");
				
			response.getWriter().write("<Error>No user id specified.</Error>");
			return;
		}
		//
		// Call handlers based on request type
		//
		try
		{
			if(requestType.equals("clearApplications"))
			{
				handleClearApplications(request, response, userId);
			}
			else if(requestType.equals("addApplication"))
			{
				handleAddApplication(request, response, userId);
			}
			else if(requestType.equals("run"))
			{
				handleRun(request, response, userId, postBuf);
			}
			else if(requestType.equals("parserExists"))
			{
				handleParserExists(request, response, userId);
			}
			else
			{
				response.getWriter().write("<Error>No request type (req) parameter specified or unknown request type.</Error>\n");
			}
		}
		catch(IOException e)
		{
			requests.remove(userId);
			if(debug)
   			System.out.println("Error: IO Exception");
			response.getWriter().write("<Error>IO Exception sending commands: " + e.toString() + "</Error>\n");
			return;
		}
		catch(GateException e)
		{
			requests.remove(userId);
			response.getWriter().write("<Error>GATE Exception: " + e.toString() + "</Error>\n");
			if(debug)
			{
				System.out.println("Error: GATE Exception - " + e.toString());
				e.printStackTrace();
			}
			return;
		}
		catch(Exception e)
		{
			requests.remove(userId);
			response.getWriter().write("<Error>Exception: " + e.toString() + "</Error>\n");
			if(debug)
			{
				System.out.println("Error: Exception");
				e.printStackTrace();
			}
			return;
		}
	}
	
	/************************************************************
     * Instructions-handling methods
     ***********************************************************/

	// handle request to add an application  
   protected void handleAddApplication(HttpServletRequest request, HttpServletResponse response, String userId)
   throws ServletException, IOException, GateException
   {
   	// create a new request if necessary
   	Request thisRequest = (Request)requests.get(userId);
   	if(thisRequest == null)
   	{
   		thisRequest = new Request();
   		thisRequest.userId = userId;
   		thisRequest.gateApplication = (SerialAnalyserController)Factory.createResource(
            "gate.creole.SerialAnalyserController"); 
   		requests.put(userId, thisRequest);
   	}
   	
   	// get the GATE file url
   	String urlString = getParameter(request, "fileUrl", null);
   	if(urlString == null)
   	{
   		if(debug)
   			System.out.println("Error: No URL specified");
   		response.getWriter().write("<Error>No URL specified.</Error>");
			return;
   	}
   	urlString = URLDecoder.decode(urlString);
   	
   	URL url = null;
   	try
   	{
   		url = new URL(urlString);
		} catch(MalformedURLException e)
		{
			if(debug)
   			System.out.println("Error: Invalid URL");
   			
			response.getWriter().write("<Error>Invalid URL.</Error>");
			return;
		}
      	
   	// get the GATE file type
   	String fileType = getParameter(request, "fileType", null);
   	if(fileType == null)
   	{
   		if(debug)
   			System.out.println("Error: No file type specified");
   		response.getWriter().write("<Error>No file type specified.</Error>");
			return;
   	}
   			
   	if(debug)
   		System.out.println("Adding file " + url.toString()
   			+ "(" + url.toString() +"), file type " + fileType + ".");
   			
   	// take action based on the file type
   	if(fileType.toLowerCase().equals("gate_jape"))
   	{
         // load the JAPE transducer 
         FeatureMap params = Factory.newFeatureMap();
         params.put("encoding", "UTF-8");
         params.put("grammarURL", url);
         params.put("inputASName", "Tags");
         params.put("outputASName", "XMLOutput");
         ProcessingResource pr =
       	  (ProcessingResource) Factory.createResource(
       	  "gate.creole.Transducer", params);
       	   
			// add the jape transducer
         thisRequest.gateApplication.add(pr);

   	}
   	else if(fileType.toLowerCase().equals("gate_gapp"))
   	{
   		// create a controller for the new application
   		CorpusController appController =
   			(CorpusController)PersistenceManager.loadObjectFromUrl(url);
   			
   		// add each processing resource to the original application
   		Collection appPrs = appController.getPRs();
			Iterator appIter = appPrs.iterator();
			while(appIter.hasNext())
       		thisRequest.gateApplication.add((ProcessingResource)appIter.next());
       		
       	Factory.deleteResource(appController);   		 		
   	}
   	else
   	{
   		response.getWriter().write("<Error>Unknown filename extension.</Error>");
			return;
   	}
   	   
      response.getWriter().write("<Success />");
   }
   
	// handle request to clear applications  
   protected void handleClearApplications(HttpServletRequest request, HttpServletResponse response, String userId)
   throws ServletException, IOException
   {
   	Request thisRequest = (Request)requests.get(userId);
   	if(thisRequest == null)
   	{
   		return;
   	}
   	
   	// delete the GATE application
   	Factory.deleteResource(thisRequest.gateApplication);

   	// delete the request
   	requests.remove(userId);
   	
   }
   
	// handle request to run GATE applications on a document   
   protected void handleRun(HttpServletRequest request, HttpServletResponse response,
   	String userId, byte[] postBuf)
   throws ServletException, IOException, GateException
   {
		// prepare output for text
		response.setContentType("text/plain; charset=UTF-8");
		PrintWriter writer = response.getWriter();
		
		// check if there's an outstanding request for this user
		Request thisRequest = (Request)requests.get(userId);
		if(thisRequest == null) 
		{
			if(debug)
				System.out.print("Error: No applications have been created.x\r\n");
				
			writer.write("<Error>No applications have been created.</Error>");
			return;
		}
			
		// Check that a file was posted
		String postBufStr = new String(postBuf);
		if(postBufStr.length() < 1)
		{
			if(debug)	
				System.out.print("Input Error: No file posted\r\n");
				
			writer.write("<Error>No file posted (or empty file).</Error>\n");
			return;
		}
			
		// Create a Corpus for the document use
		Corpus gateCorpus = Factory.newCorpus("BatchProcessApp Corpus");
		thisRequest.gateApplication.setCorpus(gateCorpus);
			
		// Jtidy the input html
		StringWriter inputHtml = new StringWriter();
		Tidy tidy = new Tidy();
		tidy.setXHTML(true); 
		tidy.parse(new StringReader(new String(postBuf)), inputHtml);
		
	   // create the GATE document, containing the POST content
	   FeatureMap params = Factory.newFeatureMap();
	   params.put("mimeType", new String("text/html"));
	   params.put("markupAware", new Boolean(true));
		params.put("preserveOriginalContent", new Boolean(true));
      params.put("collectRepositioningInfo", new Boolean(true));
      params.put(Document.DOCUMENT_STRING_CONTENT_PARAMETER_NAME, inputHtml.toString());
		Document doc = (Document)Factory.createResource("gate.corpora.DocumentImpl", params);
  
	   // put the document in the corpus
	   gateCorpus.add(doc);
	      
	   // run the application
	   thisRequest.gateApplication.execute();

		// for the XML output, add each document feature
		Element xmlRoot = new Element("GateXMLTags");
		for(Map.Entry<Object, Object> feature : doc.getFeatures().entrySet())
		{
			// skip unneeded information
			String key = (String)feature.getKey();
			if(key.equals("gate.SourceURL") ||
			   key.equals("Original_document_content_on_load"))
				continue;
				
			Element featureElem = new Element(key);
 
 			String value = null;
 			try
 			{
				value = (String)feature.getValue();
			}
			catch(ClassCastException e)
			{
				// if feature cannot be cast to String, use toString
				// for keywords
				if(key.equals("keywords"))
					value = feature.getValue().toString();
			}
			if(value != null && !value.equals(""))
			{
				featureElem.setText(value);
				xmlRoot.addContent(featureElem);
			}
		}
		
		// add each annotation
		AnnotationSet xmlAnnots = doc.getAnnotations("XMLOutput");
		Iterator<Annotation> annotIter = xmlAnnots.iterator();
		while(annotIter.hasNext())
		{
			Annotation annot = annotIter.next();
			Element annotElem = new Element(annot.getType());
			
			// add each feature as an attribute
			for(Map.Entry<Object, Object> feature : annot.getFeatures().entrySet())
			{
				String value = (String)feature.getValue();
				if(value != null && !value.equals(""))
				{
					annotElem.setAttribute(
						(String)feature.getKey(), (String)feature.getValue());
				}
			}
			xmlRoot.addContent(annotElem);
		}
				
		// write out the XML
		org.jdom.Document outputDoc = new org.jdom.Document(xmlRoot);
      XMLOutputter serializer = new XMLOutputter();
      serializer.setFormat(
      	serializer.getFormat().setEncoding("UTF-8"));
      serializer.output(outputDoc, writer);
		
		// get the HTML
		AnnotationSet htmlAnnots = doc.getAnnotations("HTMLOutput");
		String html = doc.toXml(htmlAnnots, true).replaceAll("\\s+gateId\\=\"\\d+\"", "");
		
		// JTidy the HTML and write it out
		writer.write("<GateHTML>");
		tidy.parse(new StringReader(html), writer);
		writer.write(html);		
		writer.write("</GateHTML>");
		
		// delete the document and corpus
	   Factory.deleteResource(doc);
		Factory.deleteResource(gateCorpus);
	   gateCorpus.clear();
   }
   
 	// handle query as to whether parser has been created for the given
 	// user id  
   protected void handleParserExists(HttpServletRequest request, HttpServletResponse response, String userId)
   throws ServletException, IOException
   {
   	Request thisRequest = (Request)requests.get(userId);
   	if(thisRequest == null)
   	{
   		response.getWriter().write("false");
   	}
   	else
   	{
   		response.getWriter().write("true");
   	}
   }
   
   /************************************************************
     * Misc helper functions
     ***********************************************************/
     
   // gets the value of a parameter (returns null or the default if param
   // does not exist)
   static private String getParameter(HttpServletRequest request, String paramName,
   	String defaultVal)
   {
		// Get the request type, parsing the query string directly (as Jetty does not
		// allow combined reading post data and parameters)
		String query = "&" + request.getQueryString();

		if(query == null)
		{
			if(defaultVal != null)
				return defaultVal;
			else
				return null;
		}

		Matcher paramMatcher =
			Pattern.compile("\\&" + paramName +"=([^\\&]+)").matcher(query);
		if(!paramMatcher.find())
		{
			if(defaultVal != null)
				return defaultVal;
			else
				return null;
		}
		return paramMatcher.group(1);
	}

}