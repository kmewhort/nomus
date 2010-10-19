// if this is the destination document

var siteSpiderCommand = function()
{
	// run the command once jquery has loaded, and the destination
	// selector shows up on the page
	if(typeof($) == "undefined" || !$ || $('#DESTSELECTOR#').size() == 0)
	{
		setTimeout(siteSpiderCommand, 250);
		return;
	}

		// response
		var response = {};
		
		// if no ack is needed before execution, execute command now
		// (response will be populated)
		if(!#ACKFIRST#)
		{
			#COMMAND#
		}
		
		// stringify response
		response = JSON.stringify(response);

		// enconding of the current page
		var encoding = document.characterSet;
		//var contenttype = 'application/x-www-form-urlencoded';
		var contenttype = 'text/plain';
		if(encoding)
			contenttype += ';charset=' + encoding;
		
		// send response
		$.ajax({type: 'POST', url: '#RESPURL#', data: response, dataType: 'json',
			contentType: contenttype,
			success: function(data)
			{
				// if the command was only to be run after acknowledgment, execute now
				if(#ACKFIRST#)
				{
					#COMMAND#
				}
			}});
}
siteSpiderCommand();