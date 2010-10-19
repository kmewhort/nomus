var siteSpiderInitialized = false;
var siteSpiderClientReqCount = 0;
var siteSpiderEnabled = false;
var waits = new Array();
var callbackLocation = null;

// initialize function
var siteSpiderInit = function()
{
	// if already initialized, do nothing
	if(siteSpiderInitialized)
		return;
	siteSpiderInitialized = true;	
	
	// keep track of last go to detect a freeze
	var lastGo = null;

	// occasionally the waitForCommand drops out, so setup a background interval
	// to check for this every 30s
	function checkStillRunning()
	{
		if(lastGo == null || (new Date()).getTime() - lastGo > 3000)
		{
			if(siteSpiderEnabled)
				go();
		}
		else
		{
			//console.log('Still running.');
		}
	}
	setInterval(checkStillRunning, 3000);

	// wait for next command
	function waitForCommand()
	{
		// clear all current timeouts
		while(waits.length > 0)
		{
			clearTimeout(waits.pop());
		}
		
		if(siteSpiderEnabled)
			waits.push(setTimeout(go, 1000)); //1500
	}

	// wait for jquery to load and then run commands server-side waiting on us
	function go()
	{
		if(!siteSpiderEnabled)
			return;
			
		// set the timestamp of this go
		lastGo = (new Date()).getTime();
		
		// wait if there's no content document
		if(!content.document)
		{
			waitForCommand();
			return;
		}
		
		// wait if the website in the user window has
		// not given the go ahead with a callback url
		var callbackUrlContainer = content.document.getElementById("nomusSiteSpiderCallback");
		if(!callbackLocation)
		{
			if(!callbackUrlContainer.childNodes[0].data)
			{
				waitForCommand();
				return;
			}
			callbackLocation = callbackUrlContainer.childNodes[0].data;
		}
		
		// if there is a callbackUrlContainer, we're on the
		// spider launch page -- notify that plugin is ready
		if(callbackUrlContainer)
		{
			if(!content.document.getElementById("nomusSiteSpiderPluginReady"))
			{
				var jqE = content.document.createElement("div");
				jqE.id = "nomusSiteSpiderPluginReady";
				jqE.style.display = "none";
				content.document.getElementsByTagName("body")[0].appendChild(jqE);
			}
		}
		
		// wait if jQuery is not ready
		if(typeof($) == "undefined" || !$)
		{
			waitForCommand();
			return;
		}
		
		// get our bottom window
		var iFrame = document.getElementById("sitespider-frame");
		// run whatever command the server wants us to run
		var jqE = iFrame.contentWindow.document.createElement("script");
		jqE.src = callbackLocation + '&req=getJS&seq=' + siteSpiderClientReqCount++;
		jqE.type="text/javascript";
		iFrame.contentWindow.document.getElementsByTagName("head")[0].appendChild(jqE);

		// wait for the next command
		waitForCommand();
	}
	go();
}
window.addEventListener("load", function(e) { setTimeout(siteSpiderInit, 1000); }, false);

// toggle panel open/closed
var togglePanel = function()
{
	var panel = window.document.getElementById('sitespider-vbox');
   var splitter = window.document.getElementById('sitespider-splitter');
   if(panel.collapsed == false)
   {
   	panel.collapsed = true;
      splitter.collapsed = true;
      siteSpiderEnabled = false;

      // clear any waits
      if(waits)
      {
      	while(waits.length > 0)
			{
				clearTimeout(waits.pop());
			}
		}

      // retract the plugin ready notice
      if(content.document)
      {
      	var readyNotice = content.document.getElementById("nomusSiteSpiderPluginReady");
      	if(readyNotice)
      	{
				readyNotice.parentNode.removeChild(readyNotice);
			}
		}
   }
   else
   {
   	panel.collapsed = false;
      splitter.collapsed = false;
      siteSpiderEnabled = true;
      
      // create the plugin ready notice
      if(content.document)
      {
      	var readyNotice = content.document.getElementById("nomusSiteSpiderPluginReady");
      	if(!readyNotice)
      	{
				content.document.getElementById("nomusSiteSpiderPluginReady")
				jqE.id = "nomusSiteSpiderPluginReady";
				jqE.style.display = "none";
				content.document.getElementsByTagName("body")[0].appendChild(jqE);
			}
		}
   }
}
