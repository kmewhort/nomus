import java.io.IOException;
import java.io.PrintWriter;
import java.io.FileWriter;
import java.io.FileReader;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileOutputStream;
import java.io.BufferedOutputStream;
import java.io.OutputStreamWriter;
import java.lang.Thread;
import java.io.OutputStream;
import java.io.InputStream;
import java.io.StringReader;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.Iterator;
import java.util.List;
import java.util.Arrays;
import java.util.HashMap;
import java.util.UUID;
import java.util.Calendar;
import java.net.URL;
import java.net.URLDecoder;

import javax.servlet.ServletConfig;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.jdom.Document;
import org.jdom.Element;
import org.jdom.input.SAXBuilder;
import org.jdom.output.XMLOutputter;
import org.jdom.JDOMException;

import com.google.gson.Gson;

import org.apache.commons.io.FilenameUtils;
import org.apache.commons.io.FileUtils;
import org.apache.commons.io.IOUtils;
import org.apache.commons.codec.binary.Base64;
import org.apache.commons.codec.binary.Hex;
import org.apache.commons.codec.DecoderException;

import org.apache.commons.lang.StringEscapeUtils;
/**
 * Servlet implementation class SiteSpider
 */
public class SiteSpider extends HttpServlet
{			
	// debugging on/off
	private boolean debug = false;
	
	// status and data of each user request being processed,
	// keyed by user id
	private HashMap requests = new HashMap();
	
	// representation of a spidering request
	static class Request {
		public String userId;
		public int maxResults = -1;
		public int filesDownloaded = 0;
		public String currentXMLInstruction = "";
		public String nextJsCommand = null;
		public String commandResponse = null;
		};
		
	// representation of retrieved links
	static class RetrievedLink
	{
		public String text;
		public String location;
	}
	static class GrabLinkResult
	{
  		public String location;
  		public int numlinks;
  		public RetrievedLink links[];
  	}
  	
    /**
     * Default constructor. 
     */
    public SiteSpider() {
    }

	public void init(ServletConfig config) throws ServletException
	{
		super.init(config);
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
		response.setHeader("Pragma","no-cache"); //HTTP 1.0
		response.setDateHeader ("Expires", 0); //prevent caching at the proxy server
			
		//
		// Get any posted input data
		//
		
		// read in any posted data
		byte[] postBuf = IOUtils.toByteArray(request.getReader());
		
		if(debug &&
		   request.getQueryString() != null &&
		   request.getQueryString().indexOf("getJS") < 0 &&
		   request.getQueryString().indexOf("status") < 0)
		{
			System.out.print("POST/GET request received; Query string=\""
				+ request.getQueryString());
			if(postBuf != null)
				System.out.print("\"; Posted data = " + postBuf.length + "bytes\r\n");
			else
				System.out.print(".\r\n");
		}
		
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
		
		//
		// Call handlers based on request type
		//

		if(requestType.equals("getJS"))
		{
			// request for a javascript command, coming from the browser
			handleBrowserPluginCommandRequest(request, response, postBuf);
		}
		else if(requestType.equals("response"))
		{	
			handleBrowserPluginTextResponse(request, response, postBuf);
		}
		else if(requestType.equals("run"))
		{
			handleRunInstructions(request, response, postBuf);
		}
		else if(requestType.equals("status"))
		{
			handleGetStatus(request, response, postBuf);
		}
		else
		{
			response.getWriter().write("<Error>No request type (req) parameter specified or unknown request type.</Error>\n");
		}
	}
	
	/************************************************************
     * Instructions-handling methods
     ***********************************************************/

	// handle request to run a set of instructions   
   protected void handleRunInstructions(HttpServletRequest request, HttpServletResponse response, byte[] postBuf)
   throws ServletException, IOException
   {
			// prepare output for text
			response.setContentType("text/plain; charset=UTF-8");
			PrintWriter writer = response.getWriter();
			
			// callbacks (where the client gets commands by proxy, sends
			// files, and checks for file uniqueness)
			String commandRequestUrl = URLDecoder.decode(getParameter(request, "commandRequestUrl", null));
			String fileUploadUrl = URLDecoder.decode(getParameter(request, "fileUploadUrl", null));
			String fileUniqueCheckUrl = URLDecoder.decode(getParameter(request, "fileUniqueCheckUrl", null));
			
			// dates to spider
			String yearParam = getParameter(request, "year", null);
			String monthParam = getParameter(request, "month", null);
			
			// max file to download
			String maxResultsParam = getParameter(request, "maxFiles", null);
			
			// userid 
			String userId = getParameter(request, "user", null);
			if(userId == null)
			{
				if(debug)
					System.out.print("Error: No user id specified.\r\n");
				
				writer.write("<Error>No user id specified.</Error>");
				return;
			}
			
			// check if there's an outstanding request for this user
			if(requests.get(userId) != null) 
			{
				if(debug)
					System.out.print("Error: Request is already being processed.\r\n");
				
				writer.write("<Error>A request is already being processed.</Error>");
				return;
			}
			
			// Check that instructions were posted
			String postBufStr = new String(postBuf);
			if(postBufStr.length() < 1)
			{
				if(debug)	
					System.out.print("Input Error: No instructions posted\r\n");
					
				writer.write("<Error>No instructions posted.</Error>\n");
				return;
			}
			
			// Check that a year was specified
			if(yearParam == null)
			{
				if(debug)	
					System.out.print("Input Error: No year specified\r\n");
				writer.write("<Error>'year' parameter must be specified.</Error>\n");
				return;
			}
			int year = Integer.parseInt(yearParam);
			
			// create the request
			Request userRequest = new Request();
			userRequest.userId = userId;
			if(maxResultsParam != null)
				userRequest.maxResults = Integer.parseInt(maxResultsParam);
			requests.put(userId, userRequest);
			
			try
			{
				if(debug)	
					System.out.print("Parsing XML input\r\n");
					
				// create JDOM document from the instructions
				Document doc = (new SAXBuilder()).build(new StringReader(postBufStr.trim()));
				Element root = doc.getRootElement();
			
				// find the instructions for this year
				Element instructionsForThisYear = null;
				List instructionSets = root.getChildren("Instructions");
				for(Iterator<Element> i = instructionSets.iterator();
					i.hasNext() && instructionsForThisYear == null;)
				{
					Element next = i.next();
					
					String fromYearStr = null;
					if(next.getAttribute("fromYear") != null)
					{
						fromYearStr = next.getAttribute("fromYear").getValue();
					}
					
					String toYearStr = null;
					if(next.getAttribute("toYear") != null)
					{
						toYearStr = next.getAttribute("toYear").getValue();
					}
				
					// if each bound is not specified or matches, use this
					// this instruction set				
					if((fromYearStr == null || Integer.parseInt(fromYearStr) <= year) &&
						(toYearStr == null || Integer.parseInt(toYearStr) >= year))
					{
						instructionsForThisYear = next;
					}
				}
			
				if(instructionsForThisYear == null)
				{
					if(debug)	
						System.out.print("Input Error: No instructions found for specified year\r\n");
					writer.write("<Error>No instruction sets match the specified year.</Error>\n");
					requests.remove(userId);
					return;
				}
					
				if(debug)	
					System.out.print("Executing instruction set.\r\n");
			
				executeInstructionSet(userId,
					commandRequestUrl, fileUniqueCheckUrl, fileUploadUrl,
					instructionsForThisYear,
					(new Integer(year)).toString(), monthParam, null, null, null);
	
				if(debug)
					System.out.print("All instructions executed.\r\nGenerating XML output.\r\n");
					
				requests.remove(userId);
				writer.write("<Success></Success>");
			}
			catch(JDOMException e)
			{
				requests.remove(userId);
				writer.write("<Error>Unable to parse XML input: " + e.toString() + "</Error>\n");
				return;
			}
			catch(IOException e)
			{
				requests.remove(userId);
				writer.write("<Error>IO Exception sending commands: " + e.toString() + "</Error>\n");
				return;
			}
			catch(Exception e)
			{
				requests.remove(userId);
				writer.write("<Error>Exception: " + e.toString() + "</Error>\n");
				if(debug)
				{
					e.printStackTrace(writer);
				}
				return;
			}
			
			return;
   }
   
	// execute an instruction set
	private void executeInstructionSet(
		String userId,
		String responseUrl, String uniqueCheckUrl, String uploadUrl,
		Element root, String year, String month,
		String link, String linkText, String uniqueId)
		throws IOException
	{
		// stop executing if max results reached
		Request userRequest = (Request)requests.get(userId);
		if(userRequest == null || 
			(userRequest.maxResults > 0 &&
			 userRequest.filesDownloaded >= userRequest.maxResults))
		{
			 return;
		}
	
					
		List<Element> instructions = root.getChildren();
		
		for(Element instruction : instructions )
		{
			String instructAction = parseVariables(
				instruction.getName(), year, month, link, linkText);
			String instructTarget = parseVariables(
				instruction.getTextTrim(), year, month, link, linkText);
				
			if(debug)
				System.out.print("Executing instruction: " + instructAction + "\r\n");
				
			userRequest.currentXMLInstruction = instructAction;
			if(instructAction.equals("open-url"))
			{
				// convert any forward slashes to back slashes
				String targetUrl = instructTarget.replace('\\', '/');
				
				// if the url is actually javascript, run it directly
				Matcher jsMatcher = Pattern.compile("javascript\\:(.+)").matcher(targetUrl);
				if(jsMatcher.find())
				{
					sendCommand(userId, "*",
						jsMatcher.group(1) + ";",
						true, responseUrl);
				}
				// else, open the url
				else
				{
					sendCommand(userId, "*",
						"location.href = '" + targetUrl + "';",
						true, responseUrl);
				}
				try
				{
					Thread.sleep(1000);
				}
				catch(java.lang.InterruptedException e)
				{
					return;
				}
			}
			else if(instructAction.equals("select-option"))
			{
				sendCommand(userId, instructTarget,
					"$('" + instructTarget +"').parent().get(0).selectedIndex = $('" + instructTarget + "').get(0).index;",
					false, responseUrl);
			}
			else if(instructAction.equals("set-input"))
			{
				// get the value
				String value = parseVariables(
					instruction.getAttribute("value").getValue(), year, month, link, linkText);
					
				sendCommand(userId, instructTarget,
					"$('" + instructTarget +"').get(0).value = '" + value +"';",
					false, responseUrl);
			}
			else if(instructAction.equals("check-radio"))
			{
				sendCommand(userId, instructTarget,
					"$('" + instructTarget +"').get(0).checked = true; ",
					false, responseUrl);
			}
			else if(instructAction.equals("click"))
			{
				sendCommand(userId, instructTarget,
					"$('" + instructTarget + "').trigger('click');",
					true, responseUrl);
			}
			else if(instructAction.equals("follow-link"))
			{
				sendCommand(userId, instructTarget,
					"location.href = $('" + instructTarget +"').attr('href');",
					true, responseUrl);
				try
				{
					Thread.sleep(1000);
				}
				catch(java.lang.InterruptedException e)
				{
				}
			}
			else if(instructAction.equals("wait"))
			{
				try
				{
					Thread.sleep(Integer.parseInt(instructTarget) * 1000);
				}
				catch(java.lang.InterruptedException e)
				{
				}
			}
			else if(instructAction.equals("custom-js"))
			{
				sendCommand(userId, "*",
					instructTarget,
					true, responseUrl);
			}
			else if(instructAction.equals("foreach-link"))
			{
				// get the link nodes filter
				String linkNodes = parseVariables(
					instruction.getAttribute("filter").getValue(), year, month, link, linkText);
					
				// get the linktext node 
				String linkTextNode = null;
				if(instruction.getAttribute("text") != null)
				{
					linkTextNode = parseVariables(
						instruction.getAttribute("text").getValue(), year, month, link, linkText);
				}
					
				// get matching links
				RetrievedLink[] matchingLinks =
					execGetLinks(userId, linkNodes, linkTextNode, responseUrl, false);
				
				// for each link
				for(RetrievedLink thisLink : matchingLinks)
				{
					String linkTextForRecurse = thisLink.text;
					if(linkTextNode == null)
						linkTextForRecurse = linkText;
						
					// recurse on the subnodes
					executeInstructionSet(userId,
						responseUrl, uniqueCheckUrl, uploadUrl,
						instruction, year, month, thisLink.location, linkTextForRecurse, uniqueId);
						
				}
			}
			else if(instructAction.equals("foreach-month"))
			{
				int firstMonth = 1;
				int lastMonth = 12;
				
				// current time
				Calendar now = Calendar.getInstance();
				
				// if month is already set, only use that month
				if(month != null && !month.equals(""))
				{
					firstMonth = Integer.parseInt(month);
					lastMonth = firstMonth;
				}
				
				// if yeal i- nte current year, only iterate to current month
				else if(Integer.parseInt(year) == now.get(Calendar.YEAR))
					lastMonth = now.get(Calendar.MONTH) + 1;
					
				// for each month
				for(int i = firstMonth; i <= lastMonth; i++)
				{
					// recurse on the subnodes
					executeInstructionSet(userId,
						responseUrl, uniqueCheckUrl, uploadUrl,
						instruction, year, (new Integer(i)).toString(), link, linkText, uniqueId);
				} 
			}
			else if(instructAction.equals("while-link"))
			{
				// get the link nodes filter
				String linkNodes = parseVariables(
					instruction.getAttribute("filter").getValue(), year, month, link, linkText);
					
				// get matching links
				RetrievedLink[] matchingLinks =
					execGetLinks(userId, linkNodes, null, responseUrl, false);

				// while there is a matching link
				while(matchingLinks != null && matchingLinks.length > 0)
				{
					// stop executing if max results reached
					if(userRequest == null || 
						(userRequest.maxResults > 0 &&
			 			userRequest.filesDownloaded >= userRequest.maxResults))
					{
			 			break;
					}
					
					// execute the subnodes, using the location of this link
					executeInstructionSet(userId,
						responseUrl, uniqueCheckUrl, uploadUrl,
						instruction, year, month, matchingLinks[0].location, linkText, uniqueId);
					
					if(debug)
						System.out.println("Checking for matching link to determine whether to continue while loop");
					matchingLinks =
						execGetLinks(userId, linkNodes, null, responseUrl, false);
				}
			}
			else if(instructAction.equals("if-uniqueid"))
			{
				// set the unique id
				if(instruction.getAttribute("uniqueId") == null)
				{
					if(debug)
						System.out.print("No unique id attribute - unique id check is false.\r\n");
					uniqueId = "";
				}
				else
				{
					uniqueId = parseVariables(
						instruction.getAttribute("uniqueId").getValue(), year, month, link, linkText);
									
					// if there is a filter, run the regexp on the uniqueid
					if(instruction.getAttribute("uniqueIdFilter") != null)
					{					
						String regExp = parseVariables(
							instruction.getAttribute("uniqueIdFilter").getValue(), year, month, link, linkText);
						String regExpGroupStr = parseVariables(
							instruction.getAttribute("uniqueIdFilterGroups").getValue(), year, month, link, linkText);
						String[] regExpGroups =  regExpGroupStr.split(",");					
											
						Matcher uniqueIdMatcher =
							Pattern.compile(regExp,Pattern.DOTALL).matcher(uniqueId);
						if(uniqueIdMatcher.find())
						{
							uniqueId = "";
							for(String group : regExpGroups)
							{
								uniqueId += uniqueIdMatcher.group(Integer.parseInt(group));
							}
							if(debug)
								System.out.println("uniqueIdFilter filter matched, id = " + uniqueId + "\r\n");
						}
						else
						{
							if(debug)
								System.out.println("uniqueIdFilter did not match - using full uniqueId of: " + uniqueId + "\r\n");
						}
					}
				
					if(debug)
						System.out.println("Checking unique id of: " + uniqueId + "\r\n");
				
					// send a command to check whether the id is unique
					String jsCommand = "var req = new XMLHttpRequest(); ";  
					jsCommand += "req.open('POST', '" + uniqueCheckUrl +"', false);";  
	  				jsCommand += "req.send('" + StringEscapeUtils.escapeJavaScript(uniqueId) + "');";		
					jsCommand += "response.uniqueCheckResult = req.responseText; ";
					String result =
						sendCommand(userId, "*",
						jsCommand,
						false, responseUrl);
				
					if(debug)
						System.out.println("uniqueId check response: " + result + "\r\n");
	
					// recurse on subnodes if unique
					Matcher isUniqueMatcher =
							Pattern.compile("true").matcher(result);
					if(isUniqueMatcher.find())
					{
						// recurse on the subnodes
						executeInstructionSet(userId,
							responseUrl, uniqueCheckUrl, uploadUrl,
							instruction, year, month, link, linkText, uniqueId);
					}
				}
			}	
			else if(instructAction.equals("download-url"))
			{			
				// get the forced extension (if any)
				String forcedExtension = null;
				if(instruction.getAttribute("forceExtension") != null)
				{
					forcedExtension = parseVariables(
						instruction.getAttribute("forceExtension").getValue(), year, month, link, linkText);
				}
					
				// check that a unique id has been set
				if(uniqueId == null)
				{
					if(debug)
						System.out.print("No unique id set, continuing.\r\n");
					return;
				}
				
				// download the file using the browser
				if(debug)
					System.out.print("Dowloading file " + instructTarget +"\r\n");

				downloadFile(userId, instructTarget, responseUrl, uploadUrl,
						uniqueId, linkText, forcedExtension);
				userRequest.filesDownloaded++;
	

			}
		}// end for each instruction
	}
	
	private RetrievedLink[] execGetLinks(String userId,
		String instructTarget, String parentTextNode, String responseUrl, 
		boolean downloadFiles)
		throws IOException
	{
		if(parentTextNode == null)
			parentTextNode = "closest(\"*\")";
				
		String jsCommand = "response.location = location.href; ";
		jsCommand += "var linkNodes = $('" + instructTarget +"'); ";
		jsCommand += "response.numlinks = linkNodes.length; ";
		jsCommand += "response.links = new Array(); ";
		jsCommand += "for(var i = 0; i < linkNodes.length; i++){ ";
		jsCommand += "response.links.push({'text': linkNodes.eq(i)." + parentTextNode + ".html(), 'location': linkNodes.eq(i).closest('a').attr('href')}); ";	
		jsCommand += "} ";
		String result =
			sendCommand(userId, "*",
			jsCommand,
			false, responseUrl);
		//System.out.println(jsCommand);
		if(debug)
			System.out.print("JsonResponse:" + result + "\r\r");
			
		// parse result
  		Gson gson = new Gson();
  		GrabLinkResult resultObj =
  			gson.fromJson(result, GrabLinkResult.class);
  			
  		if(debug)
			System.out.print((new Integer(resultObj.links.length)).toString()
			+ " links received\r\n");
			 						
		// if we know the base url, prepend it as necessary
		if(resultObj.location.matches("http\\:\\/\\/"))
		{
			String base = resultObj.location.substring(
				0, resultObj.location.lastIndexOf("/")+1);
						
			for(RetrievedLink link : resultObj.links )
			{
				// if we need to add on the base
				if(!link.location.matches("http\\:\\/\\/"))
				{
					// if the link starts with a slash, only add the root
					// of the base
					if(link.location.startsWith("/"))
					{
						// if base url has more than the server
						Matcher baseUrlMatcher =
							Pattern.compile("(http\\:\\/\\/.+?)\\/")
							.matcher(base);
						if(baseUrlMatcher.find())
						{
							link.location =
								baseUrlMatcher.group(1) + link.location;
						}
						// else, just prepend the whole server-only url
						else
						{
							link.location = base + link.location;
						}
					}
					// else, if the link starts with neither a http nor
					// a slash, add whole current location
					else
					{
						link.location = base + link.location;
					}
				}
			}// end for each link
		}// end if base url known
		
		return resultObj.links;
	}
	
	private String parseVariables(String string, String year, String month, String link, String linkText)
	{
		if(month != null)
		{
			string = string.replaceAll("\\#MONTH1-12\\#", month);
			
			int intMonth = Integer.parseInt(month);
			if(intMonth <=9)
				string = string.replaceAll("\\#MONTH01-12\\#", "0" + month);
			else
				string = string.replaceAll("\\#MONTH01-12\\#", month);
				
			string = string.replaceAll("\\#MONTH0-11\\#",
				(new Integer(intMonth - 1)).toString());
		}
		if(year != null)
		{
			string = string.replaceAll("\\#YEAR\\#", year);
			string = string.replaceAll("\\#YEAR-TWO\\#", year.substring(2));
		}
		if(link != null)
		{
			string = string.replaceAll("\\#LINK\\#", Matcher.quoteReplacement(link));
		}
		if(linkText != null)
		{
			string = string.replaceAll("\\#LINKTEXT\\#", Matcher.quoteReplacement(linkText));
		}
		return string;
	}
	
	/************************************************************
     * Methods for handling a status request
     ***********************************************************/
   protected void handleGetStatus(HttpServletRequest request, HttpServletResponse response, byte[] postBuf)
   throws ServletException, IOException
   {
		// prepare output for text
		response.setContentType("text/plain; charset=UTF-8");
		PrintWriter writer = response.getWriter();
			
		// callbacks (where the client gets commands by proxy, sends
		// files, and checks for file uniqueness)
		String userId = getParameter(request, "user", null);
			
		// no user id specified
		if(userId == null)
		{
			writer.write("Error: No user id specified");
			return;
		}
		
		// no outstanding request for user
		Request userRequest = (Request)requests.get(userId);
		if(userRequest == null)
		{
			writer.write("Ready!");
			return;
		}
			
		writer.write("Files downloaded: " + userRequest.filesDownloaded + "; ");
		writer.write("Current command: " + userRequest.currentXMLInstruction + " ");
		return;
	}
		
		
		
	
	/************************************************************
     * Methods for handling interactions with the browser plugin
     ***********************************************************/
     
	// handle a request for a command from the browser plugin
   protected void handleBrowserPluginCommandRequest(HttpServletRequest request, HttpServletResponse response, byte[] postBuf)
   throws ServletException, IOException
   {
			// prepare output for text
			response.setContentType("text/plain; charset=UTF-8");
			PrintWriter writer = response.getWriter();
			
			String userId = getParameter(request, "user", null);
			if(userId == null || userId == "")
				return;
			
			// get command (and reset it)
			String command = getNextJsCommand(userId);
			
			// if there is no command, return nothing
			if(command == null || command == "")
				return;

			if(debug)	
				System.out.print("command sent.\r\n");
			
			writer.write(command);
			return;

   }
   
   // handle text repsonse from the browser plugin
   protected void handleBrowserPluginTextResponse(HttpServletRequest request, HttpServletResponse response, byte[] postBuf)
   throws ServletException, IOException
   {
     		if(debug)	
				System.out.print("Command response received from browser with character encoding of "
				+ request.getCharacterEncoding() + "\r\n");
				
			// prepare output for text
			response.setContentType("text/plain; charset=UTF-8");
			PrintWriter writer = response.getWriter();
			
			String userId = getParameter(request, "user", null);
			if(userId == null || userId == "")
				return;
				
			String encoding = request.getCharacterEncoding();
			if(encoding == null)
				encoding = "UTF-8";
			//  the posted response
			setResponse(userId, new String(postBuf, encoding));
			
			String ack = "{\"response\": \"AckReceived\"}";
			writer.write(ack);
			return;
	}
	
	private void downloadFile(String userId, String link, String responseUrl, String uploadUrl,
		String uniqueId, String linkText, String forcedExtension)
		throws IOException
	{
		// replace any backslashes in the link with forward slashes
		String linkUrl = link.replace('\\', '/');
		
		// get the file as binary, synchronously
		String jsCommand = "var req = new XMLHttpRequest(); ";  
		jsCommand += "req.open('GET', '" + linkUrl +"', false); ";  
		jsCommand += "req.overrideMimeType('text/plain; charset=x-user-defined'); ";  
		jsCommand += "req.send(null); ";
      
      // encode as a hex string 
  		jsCommand += "var data = req.responseText; ";
		jsCommand += "var hexDigits = '0123456789ABCDEF'; ";
		jsCommand += "var encodedData = '', di = 0, ei=0; ";
		jsCommand += "do{ ";
		jsCommand += "encodedData += hexDigits[ (data.charCodeAt(di) >> 4) & 15]; ";
		jsCommand += "encodedData += hexDigits[ data.charCodeAt(di++) & 15 ]; ";
		jsCommand += "}while (di < data.length); ";

		// set the unique id, linkText, and forcedExtension
		jsCommand += "var response = {}; ";
		jsCommand += "response.uniqueId = " + (new Gson()).toJson(uniqueId) + "; ";
		jsCommand += "response.link = " + (new Gson()).toJson(link) + "; ";
		
		if(linkText != null && linkText != "")
		{
			jsCommand += "response.linkText = " + (new Gson()).toJson(linkText) + "; ";
		}
		else
		{
			if(debug)
				System.out.print("ERROR: No linktext associated with downloaded file.\r\n");	
		}
		if(forcedExtension != null)
		{
			jsCommand += "response.forcedExtension = "
				+ (new Gson()).toJson(forcedExtension) + "; ";
		}
		
		jsCommand += "response.encodedFile = encodedData; ";
		jsCommand += "response = JSON.stringify(response); ";
			
		// send to the callback
		jsCommand += "var req2 = new XMLHttpRequest(); ";  
		jsCommand += "req2.open('POST', '" + uploadUrl + "', false); ";
		jsCommand += "req2.send(response); ";
		
      // launch comman
		sendCommand(userId, "*",
					jsCommand,
					false, responseUrl);
																													
		System.out.print("Command sent & executed.");
	}
	
	// send command
	// - returns response upon success, or null upon error
	private String sendCommand(String userId,
		String destSelector, String command, boolean ackFirst, String responseUrl)
		throws java.io.IOException
	{			
		// try this instruction up to three times
		String response = null;
		for(int commandTry = 0; commandTry < 3 && response == null; commandTry++)
		{	
			if(debug)
				System.out.print("Sending js command to browser, try "
				+ (commandTry+1) + " of 3.\r\n");
			
			StringBuffer commandBuffer = new StringBuffer(1024);
			
			// load jquery into the command string
			BufferedReader reader = new BufferedReader(
				new FileReader(getServletContext().getRealPath("/WEB-INF/jquery-1.4.2.min.js")));
			char[] buf = new char[1024];
			int numRead=0;
			while((numRead=reader.read(buf)) != -1)
			{
				commandBuffer.append(buf, 0, numRead);
			}
			reader.close();
			
			// load json into the command string
			reader = new BufferedReader(
				new FileReader(getServletContext().getRealPath("/WEB-INF/json2.js")));
			numRead=0;
			while((numRead=reader.read(buf)) != -1)
			{
				commandBuffer.append(buf, 0, numRead);
			}
			reader.close();
			
			// load bas64 encoder
			reader = new BufferedReader(
				new FileReader(getServletContext().getRealPath("/WEB-INF/base64.js")));
			numRead=0;
			while((numRead=reader.read(buf)) != -1)
			{
				commandBuffer.append(buf, 0, numRead);
			}
			reader.close();
			
			// load the js command template
			reader = new BufferedReader(
				new FileReader(getServletContext().getRealPath("/WEB-INF/commandTemplate.js")));
			numRead=0;
			while((numRead=reader.read(buf)) != -1)
			{
				commandBuffer.append(buf, 0, numRead);
			}
			reader.close();
			String jsTemplate = commandBuffer.toString();
			
			// insert the destination selector 
			jsTemplate = jsTemplate.replaceAll("\\#DESTSELECTOR\\#",
				Matcher.quoteReplacement(destSelector));
			
			// whether to acknowledge before executing command
			if(ackFirst)
				jsTemplate = jsTemplate.replaceAll("\\#ACKFIRST\\#", "true");
			else
				jsTemplate = jsTemplate.replaceAll("\\#ACKFIRST\\#", "false");
				
			// set respones url
			jsTemplate = jsTemplate.replaceAll("\\#RESPURL\\#", responseUrl + "&req=response");
			
			// set the command
			jsTemplate = jsTemplate.replaceAll("\\#COMMAND\\#",
				Matcher.quoteReplacement(command));
				
			// buffer the command for reading by the firefox plugin
			setNextJsCommand(userId, jsTemplate);
			
			// wait up to one minute for the response
			for(int responseTry = 0; responseTry < 60 && response == null; responseTry++)
			{
				response = getResponse(userId);
				try
				{
					Thread.sleep(1000);
				}
				catch(java.lang.InterruptedException e)
				{
					return null;
				}
			}
		}
		setNextJsCommand(userId, null);
		return response;
	}
	
	// handle access control requests
	protected void doOptions(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException
	{
		if(debug)
			System.out.print("OPTIONS request received. Allowing all cross-origin requests.\r\n");

		// just allow all cross-origin requests
		response.setHeader("Access-Control-Allow-Origin", "*");
		response.setHeader("Access-Control-Allow-Methods", "GET,POST");
		response.setHeader("Access-Control-Allow-Headers", "");
		return;
	}

	//
	// Get/Set next javascript command
	//
	synchronized private String getNextJsCommand(String userId)
	{
		Request userRequest = (Request)requests.get(userId);
		if(userRequest == null)
		{
			return "";
		}
		String command = userRequest.nextJsCommand;
		userRequest.nextJsCommand = null;
		return command;
	}
	synchronized private void setNextJsCommand(String userId, String command)
	{
		Request userRequest = (Request)requests.get(userId);
		if(userRequest != null)
		{
			userRequest.nextJsCommand = command;
			//System.out.println("Command: " + command);
		}
	}
	
	//
	// Get/Set response and response
	//
	synchronized private String getResponse(String userId)
	{
		Request userRequest = (Request)requests.get(userId);
		if(userRequest == null)
		{
			return "";
		}
		String response = userRequest.commandResponse;
		userRequest.commandResponse = null;

		return response;
	}
	synchronized private void setResponse(String userId, String response)
	{
		Request userRequest = (Request)requests.get(userId);
		if(userRequest != null)
		{
			userRequest.commandResponse = response;
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
 
