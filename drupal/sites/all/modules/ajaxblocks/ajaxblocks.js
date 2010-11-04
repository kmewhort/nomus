// $Id: ajaxblocks.js,v 1.1.2.1 2010/09/20 21:08:13 maximpodorov Exp $

/**
 * @file
 * Loads content of blocks via AJAX just after page loading, updates Drupal.settings, reattaches behaviors.
 */

if (Drupal.jsEnabled) $(document).ready(function () {
  if (typeof Drupal.settings.ajaxblocks == 'undefined') return;
  $('.blocks-ajax-content').each(function()
  {
  	 var id =$(this).attr('id');
  	 id = id.substring(6, id.length-13);
  	 data = Drupal.settings.ajaxblocks;
  	 data = data.replace(/blocks=.*?&/, "blocks=" + id + "&");
  	 data += "&getparams=" + Drupal.settings.ajaxblocks_getparams;
  $.ajax({
    url: Drupal.settings.basePath + "ajaxblocks",
    type: "GET",
    dataType: "json",
    data: data,
    cache: false,
    success: function (data) {
      // Replaces the placeholder divs by the actual block contents returned by the AJAX call,
      // executes the extra JavaScript code and attach behaviours if the apply to the blocks.
      Drupal.freezeHeight();
      for (key in data) {
        var wrapper = $('#block-' + key + '-ajax-content');
        var context = $(data[key]['content']).insertBefore(wrapper);
        wrapper.remove();
        if (data[key]['ajaxblocks_settings']) $.extend(true, Drupal.settings, data[key]['ajaxblocks_settings']);
        Drupal.attachBehaviors(context);
      }
      Drupal.unfreezeHeight();
      
      // lead the scollbar
      $(context).parent('.content').jScrollPane({showArrows:false,scrollbarWidth:4, scrollbarMargin:2});
      
    }
  });
  });
});
