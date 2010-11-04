// on page load, set the main window title to the title of the document in the iframe
var launchfixIframeHeight = function()
{
	fixIframeHeight();
}

var fixIframeHeight = function()
{
	var height = $(".quicktabs_tabpage:not('div.quicktabs-hide') iframe").contents().get(0).height;
		
	// for IE
	if(height==undefined)
	{
	   height = $(".quicktabs_tabpage:not('div.quicktabs-hide') iframe").contents().get(0).body.scrollHeight;
	}
	
	if(height != 0)
	{	
		$(".quicktabs_tabpage:not('div.quicktabs-hide') iframe").eq(0).css("height", height+350);
	}
	else
	{
		setTimeout('fixIframeHeight()', 1000);
	}
}

$(document).ready(function()
{
	$(".quicktabs_tabpage:not('div.quicktabs-hide') iframe").load(launchfixIframeHeight);
	
	// resize on any tab change
	$('ul.quicktabs_tabs li a').click(function(){ setTimeout('fixIframeHeight()', 1000); });
});
