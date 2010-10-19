var updateDocumentLoaderStatus = function()
{
	// get the status
	var status = '';
	if($('#nomusSiteSpiderPluginReady').size() == 0)
	{
		$('#statusbox').empty()
			.append('Firefox plugin not ready; please click the Nomus icon in the bottom-left of Firefox (or <a href='
				+ Drupal.settings.basePath + 'sites/all/modules/nomus/sitespider_firefoxplugin/SiteSpiderPlugin.xpi>click here</a> to install the plugin)').css('background-color', 'yellow');
	}
	else
	{
		$.get(Drupal.settings.basePath + 'nomus/documentloader/status',
			function(data)
			{
				if(data != '')
				{
					$('#statusbox').empty().append(data);
					if(data.match(/error/i))
					{
						$('#statusbox').css('background-color', 'yellow');
					}
					else
					{
						$('#statusbox').css('background-color', '#90EE90');	
					}
				}
			});
			
		// if autoload and not yet started, start running
		if(Drupal.settings.nomus_documentloader.autostart &&
			!Drupal.settings.nomus_documentloader.started){
			Drupal.settings.nomus_documentloader.started = true;
			$('#nomus-documentloader-toolbox-form #edit-submit').trigger('mousedown');
		}
	}
	
	// run again in a second
	setTimeout(updateDocumentLoaderStatus, 1500);
}
$(document).ready(updateDocumentLoaderStatus);