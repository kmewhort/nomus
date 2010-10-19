// act on the iframe
lookup = function(index, type, pinpointStart, pinpointEnd)
{
	top.location.href = top.Drupal.settings.basePath + 'nomus/lookup?index='
		+ index;
}