import org.artofsolving.jodconverter.office.OfficeManager;
import org.artofsolving.jodconverter.office.OfficeConnectionProtocol;
import org.artofsolving.jodconverter.office.DefaultOfficeManagerConfiguration;
import org.artofsolving.jodconverter.OfficeDocumentConverter;
import org.artofsolving.jodconverter.document.DefaultDocumentFormatRegistry;
import org.artofsolving.jodconverter.document.DocumentFormat;
import org.artofsolving.jodconverter.document.DocumentFormatRegistry;
import org.artofsolving.jodconverter.StandardConversionTask;
import org.artofsolving.jodconverter.document.DocumentFamily;
import org.apache.commons.io.FilenameUtils;
import org.apache.commons.io.FileUtils;
import org.apache.commons.io.IOUtils;


import javax.servlet.ServletConfig;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.commons.fileupload.servlet.*;
import org.apache.commons.fileupload.disk.*; 
import org.apache.commons.fileupload.FileItem;
import org.apache.commons.fileupload.FileUploadException;

import javax.servlet.ServletConfig;
import javax.servlet.ServletException;
import java.io.IOException;
import java.io.PrintWriter;
import java.io.FileWriter;
import java.io.FileReader;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileOutputStream;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.Iterator;
import java.util.List;
import java.util.HashMap;
import java.util.Map;
import java.util.UUID;
import java.util.Random;
 
public class DocConverter extends HttpServlet
{
   // jodconverter OfficeManager singleton
   private OfficeManager officeManager = null;
   
   // get the jodconverter
   public OfficeManager getOfficeManager()
   {
	// if the office manager is null, hot-start it
	if(officeManager == null)
		startOfficeManager();
	return officeManager;
   }

   // init: start jodconverter office manager
   public void init(ServletConfig config) throws ServletException
   {
		startOfficeManager();
		super.init(config);
   }
   
   public void destroy()
   {
		officeManager.stop();
		officeManager = null;
		super.destroy();
   }
   
	protected void doPost(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException
	{
	try
	{
		// For all responses, prevent cached respone from being used
		response.setHeader("Cache-Control","no-cache"); //HTTP 	1.1
		response.setHeader("Pragma","no-cache"); //HTTP 1.0
		response.setDateHeader ("Expires", 0); //prevent caching at the proxy server

		//
		// Get the input parameters
		//
		
		// to parameter - parse the query string directly (as Jetty does not
		// allow combined reading post data and parameters)
		String query = "&" + request.getQueryString();
		if(query == null)
		{
			query = new String("");
		}
		
		String convertTo = null;
		Matcher convertToMatcher =
			Pattern.compile("[^\\&]to=([^\\&]+)").matcher(query);
		if(convertToMatcher.find())
		{
			convertTo = convertToMatcher.group(1);
		}
		
		
		// if it's a direct upload (not multipart)
		byte[] input = null;
		String inFilename = "Unspecified";
		String inMimeType = null;
		if(request.getContentType().indexOf("multipart") == -1)
		{
			// get the data directly
			input = IOUtils.toByteArray(request.getInputStream());
			inMimeType = request.getContentType();
		}
		// otherwise, parse the parts
		else
		{
			// retrieve the file with apache fileupload (max upload ~= 30Mb);
			DiskFileItemFactory factory = new DiskFileItemFactory();
      	ServletFileUpload upload = new ServletFileUpload(factory);
      	upload.setSizeMax(30000000);
      	List fileItems;
			try
			{
				fileItems = upload.parseRequest(request);
			}
			catch(FileUploadException e)
			{
				System.out.println(e.toString());
				response.getWriter().write("<Error>Error parsing upload</Error>");
				return;
			}
      
			// iterate through the parts/files
			// TODO: handle multiple files
     		Iterator iter = fileItems.iterator();
			while(iter.hasNext())
			{
				// check for a to parameter
				FileItem item = (FileItem) iter.next();
      		if (item.isFormField())
      		{
      			if(convertTo == null && item.getFieldName().equals("to"))
						convertTo = item.getString();
				}
				// get any file data
				else
				{
					input = item.get();
					inFilename = item.getName();
					inMimeType = item.getContentType();
				}
			}
		}
		
		// check that one of the two forms of input was found
		if(input == null)
		{
			response.getWriter().write("<Error>No 'to' parameter specified (output file type)</Error>");
			return;
		}

		// check that a to parameter was specified
		if(convertTo == null)
		{			
			response.getWriter().write("<Error>No 'to' parameter specified (output file type)</Error>");
			return;
		}

      		
		// get the file info	
		String fileBase = FilenameUtils.getBaseName(inFilename);
		String fileExtension = FilenameUtils.getExtension(inFilename);	
    		
   	// determine the input format from the extension or,
   	// failing that, the mime type
		DocumentFormatRegistry formatRegistry = new DefaultDocumentFormatRegistry();
		DocumentFormat inputFormat;
		if(!fileExtension.equals(""))
		{
			// htm -> html
			if(fileExtension.equals("htm"))
			{
				fileExtension = "html";
			}
			inputFormat = formatRegistry.getFormatByExtension(fileExtension);
		}
		// otherwise, try to get it from the mime type
		else
		{
			inputFormat = formatRegistry.getFormatByMediaType(inMimeType);
		}
		if(inputFormat == null)
		{
			response.getWriter().write("<Error>Unknown input file type</Error>");
			return;
		}
			
		// determine the output format from the specified extension
		DocumentFormat outputFormat = formatRegistry.getFormatByExtension(convertTo);
		if(outputFormat == null)
		{
			response.getWriter().write("<Error>Unknown output format</Error>");
			return;
		}
	    
		// create a temporary file for the input
    	File tempDir = (File) getServletContext().
      		getAttribute( "javax.servlet.context.tempdir" );
	   File tempInFile = File.createTempFile("NDC", "." + inputFormat.getExtension(), tempDir);
	   FileUtils.writeByteArrayToFile(tempInFile, input);
	   	    	
	   // if the input is HTML, need to preparse before sending to jodconverter
		if(inputFormat.getExtension().equals("html"))
		{
			preFormatHtmlInput(tempInFile);
		}
		
		// if the input is PDF, jodconverter cannot handle so need to
		// pre-parse to HTML using popplerutils
		if(inputFormat.getExtension().equals("pdf"))
		{
			if(!preFormatPDFInput(tempInFile))
			{
				response.getWriter().write("<Error>Unable to pre-format PDF file</Error>");
				return;
			}
			inputFormat = formatRegistry.getFormatByExtension("html");
			
			// rename the input file to have the extension, otherwise
			// OO gets confused
			File newFilename = new File(
				FilenameUtils.removeExtension(tempInFile.getAbsolutePath()) +
				".html");
			FileUtils.moveFile(tempInFile, newFilename);
			tempInFile = newFilename;
				
		}
			
		// if the input format is the same as the output format,
		// the output is the same as the input
		byte[] output;
		if(inputFormat.getExtension().equals(
				outputFormat.getExtension()))
		{
			output = FileUtils.readFileToByteArray(tempInFile);
		} 
		
		// else, convert with jodconverter
		else
		{
			// create a temporary file for the output
			File tempOutFile = File.createTempFile("NDC", "." + outputFormat.getExtension(), tempDir);
	    	
    		// get the singleton office manager
	 		OfficeManager officeManager = getOfficeManager();

			// convert (manually perform conversion steps so the 
			// rather  than use OfficeDocumentConverter so the input
			// type can be specified)

			Map<String,Object> loadProperties = new HashMap<String,Object>();
			loadProperties.put("Hidden", true);
			loadProperties.put("ReadOnly", true);
			StandardConversionTask conversionTask =
				new StandardConversionTask(tempInFile, tempOutFile, outputFormat);
			conversionTask.setDefaultLoadProperties(loadProperties);
			conversionTask.setInputFormat(inputFormat);
			officeManager.execute(conversionTask);

			// get the output
			output = FileUtils.readFileToByteArray(tempOutFile);
			
			// delete the temporary output file
			tempOutFile.delete();
		}
		
		// write out the output
		response.setHeader("Content-type",  outputFormat.getMediaType());
		response.setHeader("Content-Disposition",
			"attachment; filename=\"" + fileBase + "." +
			outputFormat.getExtension() + "\"");
		response.setHeader("Content-Length", (new Long(output.length)).toString());
			
		response.getOutputStream().write(output);
			
		// delete the temp inFile
		tempInFile.delete();
	}
	catch(Exception e)
	{
		response.getWriter().write("<Error>Exception: ");
		e.printStackTrace(response.getWriter());
		response.getWriter().write("</Error>");
	}
	}
	
	// pre-parsing on HTML inputs (strip comments and links)
	private void preFormatHtmlInput(File inputFile)
		throws IOException
	{
		String input = FileUtils.readFileToString(inputFile);
		
		// reload in correct charset
		String charset = null;
		Pattern p = Pattern.compile("text/html;\\s+charset=([^\\s\"]+)[\\s*\"]", Pattern.MULTILINE);
		Matcher m = p.matcher(input);
		if(m.find())
		{
			charset = m.group(1);
			input = FileUtils.readFileToString(inputFile, charset);
		}

		// strip comments
		input = input.replaceAll("/<\\!--.*?-->/", "");
		
		// strip out all external links (everything other than # hrefs)
		input = input.replaceAll(
			"(?i)<\\s*a[^>]*?href=[\"\'][^#][^\"\']*?[\"\'].*?>(.*?)<\\s*\\/a\\s*>",
			"$1");
			
		FileUtils.writeStringToFile(inputFile, input, charset);
	}
	
	// pre-parse PDFs by first converting them to HTML
	// (jodconverter cannot convert to HTMLS)
	private boolean preFormatPDFInput(File inputFile)
		throws IOException
	{

		// page magnification
		final float magnification = (float)2.0;
		
		// create a temp directory
		File tempDir = (File) getServletContext().
        		getAttribute( "javax.servlet.context.tempdir" );	   
	   File pdfDir = new File(tempDir, UUID.randomUUID().toString());
	   pdfDir.mkdir();
	   				
		// execute Poppler-utils pdftohtml
		String popplerUtilsCommand = "pdftohtml -c " + inputFile.getAbsolutePath() +
			" -zoom 2.5 -enc UTF-8 " + pdfDir.getAbsolutePath() +
			"/splithtml.html";
		
		Process process = Runtime.getRuntime().exec(
			popplerUtilsCommand);
		try
		{
      	process.waitFor();
      }
      catch(InterruptedException e)
      {
      	return false;
      }

		// retrieve the output and error streams
		String execOutput = IOUtils.toString(process.getInputStream());
		String execErr = IOUtils.toString(process.getInputStream());
		
		// TODO: parse output and error streams for errors
		
		// post-parse the html generated by poppler utils
		
		String charSetUTF = 
			"<meta http-equiv=\"Content-Type\" content=\"text/html; charset=UTF-8\" />";
		String style = "<html><head>" + charSetUTF + "<style><!-- ";
		String body = "<body>";
		
		// for each page
		int pageNum = 1;
		File nextPage = new File(pdfDir.getAbsolutePath() +
			"/splithtml-" + pageNum + ".html");
		while(nextPage.exists())
		{
			String page = FileUtils.readFileToString(nextPage, "UTF-8");
						
			// get the content (first to last div within the body)
			String content = "";
			Matcher withinBodyMatcher =
				Pattern.compile("<BODY.*?(<DIV.*)<\\/DIV>\\s*<\\/BODY>",
				Pattern.CASE_INSENSITIVE | Pattern.DOTALL)
				.matcher(page);
			if(withinBodyMatcher.find())
			{
				content = withinBodyMatcher.group(1);
			}
			
					
			// get the style and rename the style elements to avoid
			// conflicts between pages
			Matcher styleMatcher =
				Pattern.compile("<STYLE.*?>\\s*<!--\\s*(.*?)\\s*-->\\s*<\\/STYLE>",
				Pattern.CASE_INSENSITIVE | Pattern.DOTALL)
				.matcher(page);
			if(styleMatcher.find())
			{
				String styleHTML = styleMatcher.group(1);

				// for each style element
				Matcher styleTagMatcher =
					Pattern.compile("\\.([\\d\\w]+?)(\\{.*?\\})",
					Pattern.CASE_INSENSITIVE | Pattern.DOTALL)
					.matcher(styleHTML);
				while(styleTagMatcher.find())
				{
					String styleId = styleTagMatcher.group(1);
					String styleVal = styleTagMatcher.group(2);
					
					// add the page number to the style id to make
					// it unique for the document
					String newStyleId = styleId + "p" + Integer.toString(pageNum);
					
					// add the style to the overall document style
					style += " ." + newStyleId + styleVal + " ";
						
					// update the style references in the content
					content = content.replaceAll(
						"class=\\\"" + styleId + "\\\"",
						"class=\\\"" + newStyleId + "\\\"");
				}
			}
			// get the image element width and height 
			// (from, eg <IMG width="612" height="792")
			int imageHeight = 740;
			int imageWidth = 612;
			Matcher dimMatcher =
					Pattern.compile("<IMG\\s*width=\\\"([^\\\">]*?)\\\"\\s*height=\\\"([^\\\">]*?)",
						Pattern.CASE_INSENSITIVE | Pattern.DOTALL)
					.matcher(content);
			if(dimMatcher.find())
			{
				imageHeight = Integer.parseInt(dimMatcher.group(1));
				imageWidth = Integer.parseInt(dimMatcher.group(2));
			}
			int width = imageWidth;
			int height = imageHeight;
			
			// find the top and bottom of the ACTUAL text
			int top = 0;
			int bottom = -1;
			Matcher topPosMatcher =
					Pattern.compile("position:absolute;top:(\\d+);",
						Pattern.CASE_INSENSITIVE | Pattern.DOTALL)
					.matcher(content);
			if(topPosMatcher.find())
			{
				top = Integer.parseInt(topPosMatcher.group(1)) - 15;
				if(top < 0)
					top = 0;
				bottom = Integer.parseInt(topPosMatcher.group(1)) + 30;
			}
						
			while(topPosMatcher.find())
			{
				bottom = Integer.parseInt(topPosMatcher.group(1)) + 30;
			}
			
			// TODO: need to count the number of breaks from the last top to the
			// end and adjust bottom accordingly 
			bottom += 40;
			
			// preferably, use the position of the bottom-most element for the height
			if(bottom >= 0)
				height = bottom - top;				
		
			// expand positions to the desired magnification and shift the page up
 			Matcher posMatcher =
 				Pattern.compile("(position:absolute;top:)(\\d+)(;left:)(\\d+)",
 					Pattern.CASE_INSENSITIVE | Pattern.DOTALL)
 				.matcher(content);
 			StringBuffer posAdjustedContent = new StringBuffer();
			while (posMatcher.find())
			{
				int elementTop = Integer.parseInt(posMatcher.group(2));
				elementTop = (new Float(magnification * (elementTop-top))).intValue();
				int elementLeft =  Integer.parseInt(posMatcher.group(4));
				elementLeft = (new Float(magnification * elementLeft)).intValue();
				
     			posMatcher.appendReplacement(posAdjustedContent,
     				posMatcher.group(1) +
     				Integer.toString(elementTop) +
     				posMatcher.group(3) +
     				Integer.toString(elementLeft));
			}
 			posMatcher.appendTail(posAdjustedContent);
 			content = posAdjustedContent.toString();
 			
 
			// expand page height and width
			height *= magnification;
			width *= magnification;
			
			// append the content of this page to the body
			body += "<div><div id=\"page" + Integer.toString(pageNum) +
				"\" style=\"position:relative;height:" + Integer.toString(height) +
				";\">";
			body += content;
			body += "</div></div>";
			
			// next page
			pageNum++;
			nextPage = new File(pdfDir.getAbsolutePath() +
				"/splithtml-" + pageNum + ".html");

		}//end for each page
		
		// expand font sizes to the desired magnification
 		Matcher fontMatcher =
 				Pattern.compile("(font-size:)(\\d+)(px)",
 					Pattern.CASE_INSENSITIVE | Pattern.DOTALL)
 				.matcher(style);
 		StringBuffer sizeAdjustedStyle = new StringBuffer();
		while (fontMatcher.find())
		{
     		fontMatcher.appendReplacement(sizeAdjustedStyle,
     			fontMatcher.group(1) +
     			Integer.toString(
     				(new Float(magnification * Integer.parseInt(fontMatcher.group(2)) - 1)).intValue()) +
     			fontMatcher.group(3));
		}
 		fontMatcher.appendTail(sizeAdjustedStyle);
 		style = sizeAdjustedStyle.toString();
 		
 		// expand line heights to the desired magnification
 		Matcher lineheightMatcher =
 				Pattern.compile("(line-height:)(\\d+)(px)",
 					Pattern.CASE_INSENSITIVE | Pattern.DOTALL)
 				.matcher(style);
 		sizeAdjustedStyle = new StringBuffer();
		while (lineheightMatcher.find())
		{
     		lineheightMatcher.appendReplacement(sizeAdjustedStyle,
     			lineheightMatcher.group(1) +
     			Integer.toString(
     				(new Float(magnification * Integer.parseInt(lineheightMatcher.group(2)) - 1)).intValue()) +
     			lineheightMatcher.group(3));
		}
 		lineheightMatcher.appendTail(sizeAdjustedStyle);
 		style = sizeAdjustedStyle.toString();				
		
		
		// if there"s nothing in the body, return an error
		if(body.equals("<body>"))
		{
			return false;
		}
			
		// end tags
		style += " --></style></head>";
		body += "</body></html>";
		
		// strip out all external links (everything other than # hrefs)
		body = body.replaceAll(
			"(?is)<\\s*a[^>]*?href=[\"\'][^#][^\"\']*?[\"\'].*?>(.*?)<\\s*\\/a\\s*>",
			"$1");
			
			
		// delete the temporary poppler output dir
		FileUtils.deleteDirectory(pdfDir);
		
		FileUtils.writeStringToFile(inputFile, style + body, "UTF-8");

		return true;
	}

	 
   private void startOfficeManager()
   {
		if(officeManager == null)
		{
			// unique pipename of letters only
			String pipeName = new String("NDC");
			Random random = new Random();
			for(int i = 0; i < 8; i++)
				pipeName += (char)(random.nextInt('Z'-'A'+1)+'A');
			
			officeManager = new DefaultOfficeManagerConfiguration()
			.setOfficeHome(System.getProperty("OPENOFFICEHOME"))
			.setConnectionProtocol(OfficeConnectionProtocol.PIPE)
			.setPipeNames(pipeName)
			.setTaskExecutionTimeout(60000L)
			.buildOfficeManager();
			officeManager.start();
		}
   }
   
   

}

