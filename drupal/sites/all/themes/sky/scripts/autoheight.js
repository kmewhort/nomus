// on page load, set the main window title to the title of the document in the iframe
var launchfixIframeHeight = function()
{
	fixIframeHeight();
}

var fixIframeHeight = function()
{
	var height = $('iframe').contents().get(0).height;
		
	// for IE
	if(height==undefined)
	{
	   height = $('iframe').contents().get(0).body.scrollHeight;
	}
	
	if(height != 0)
	{	
		$("iframe").eq(0).css("height", height);
	}
	else
	{
		setTimeout('fixIframeHeight()', 1000);
	}
}

$(document).ready(function()
{
	$("iframe").load(launchfixIframeHeight);
});
