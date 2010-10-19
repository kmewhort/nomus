// if the nid is not set in the Drupal settings, check if
// if its set as an url argument
$(document).ready(function()
{
	if(!Drupal.settings.nomus_saveprint.nid)
	{
		locNid = Drupal.nomus_getqueryparameter('nid');
		if(locNid){
			Drupal.settings.nomus_saveprint.nid = locNid;
		}
	}
});

// open the save dialog
Drupal.nomus_save_open = function()
{
	// open the dialog
	Drupal.modalFrame.open(
	{
		url: Drupal.settings.basePath + "nomus/save?nid=" +
			Drupal.settings.nomus_saveprint.nid,
		width: 100, height: 140,
		autofit: false,
		onSubmit: function(save_url)
		{
			Drupal.nomus_save(save_url);
		}
	});
	return false;
};

// function to send the actual save request
Drupal.nomus_save = function(save_url)
{
	if(save_url)
	{
		// show wait icon
		//Drupal.nomus_showwait();
		//parent.document.body.style.cursor = 'wait';
			
		// send request
		location.href = save_url;
			
		// hide wait
		//Drupal.nomus_hidewait();
	}
}

// register open-dialog button
$(document).ready(function()
{
	$('.save-link').click(Drupal.nomus_save_open);
	if(Drupal.settings.nomus_saveprint.autorun == 'save' ||
	   Drupal.nomus_getqueryparameter('autorun') == 'save')
	{
		Drupal.nomus_save_open();
	}
});


// open the e-mail dialog
Drupal.nomus_email_open = function()
{
	// open the dialog
	Drupal.modalFrame.open(
	{
		url: Drupal.settings.basePath + "nomus/email?nid=" +
			Drupal.settings.nomus_saveprint.nid,
		width: 500, height: 400,
		autofit: false
	});
	return false;
};

// register the open-dialog button
$(document).ready(function()
{
	$('.email-link').click(Drupal.nomus_email_open);
	
	if(Drupal.settings.nomus_saveprint.autorun == 'email' ||
	   Drupal.nomus_getqueryparameter('autorun') == 'email')
	{
		Drupal.nomus_email_open();
	}
});

// print
Drupal.nomus_print = function()
{
	// if there's an iframe, just print that frame
	if($('iframe').length > 0)
	{
		// only print active tabs
		$('iframe').each(function()
		{
			if($(this).parents('.quicktabs_tabpage').first()
				.css('display') != 'none')
			{
				// set the iframe title to that of the main window
				// (to make the filename nice)
				$(this).contents().get(0).title = document.title;
				
				// print
				window.frames[$(this).attr('name')].focus();
				window.frames[$(this).attr('name')].print();
			}
		});
	}
	else
	{
		window.print();
	}
}		
// register the print button
$(document).ready(function()
{
	$('.print-link').click(Drupal.nomus_print);
	
	if(Drupal.settings.nomus_saveprint.autorun == 'print' ||
		Drupal.nomus_getqueryparameter('autorun') == 'print')
	{
		Drupal.nomus_print();
	}
});
		
// show the wait icon to the left of the buttons
Drupal.nomus_showwait = function()
{
	// find the visible dialog
	$('.ui-dialog').each(function()
	{
		if($(this).css('display') == 'block')
		{
			// if the button does not exist, create it
			var waitIcon = $(this).find('.ui-dialog-buttonpane .waiticon');
			if(waitIcon.size() == 0)
			{
				waitIcon = $('<div class=\"waiticon\"></div>').appendTo($(this).find('.ui-dialog-buttonpane'));
			}
	
			// make visible
			waitIcon.css('visibility', 'visible');
		}
	});
}

Drupal.nomus_hidewait = function()
{
	// find the visible dialog
	$('.ui-dialog').each(function()
	{
		if($(this).css('display') == 'block')
		{
			// set the wait icon to hidden
			var waitIcon = $(this).find('.ui-dialog-buttonpane .waiticon');
			waitIcon.css('visibility', 'hidden');
		}
	});
}

// Retrieves a query parameter from the location bar
Drupal.nomus_getqueryparameter = function(attr)
{
    var matcher = new RegExp("(.*" + attr + "=)([^&]*)","i");
    var matches = matcher.exec(location.href);
    if(!matches)
            return null;
    return matches[2];
}