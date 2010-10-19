// on page load, set the main window title to the title of the document in the iframe
var fixIframeHeight = function()
{
	var height = $('iframe').contents().get(0).height;
		
	// for IE
	if(height==undefined)
	{
	   height = $('iframe').contents().get(0).body.scrollHeight;
	}
		
	$("iframe").css("height", height);
		
}

$(document).ready(function()
{
	$("iframe").load(fixIframeHeight);
});
