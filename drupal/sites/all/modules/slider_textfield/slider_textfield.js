$(function() {
	$('.slider_callout').hide();
	window.calloutVisible = false;

  $.each(Drupal.settings.slider_settings, function(elem_id, settings) {

    settings.start = function(e, ui) {
      $(this).find('.slider_callout').fadeIn('fast', function() { window.calloutVisible = true;});
      if (settings.callbacks && settings.callbacks.start) {
        try {
          eval(settings.callbacks.start)(e, ui);
        }
        catch (err) {
        };
      };
    }; 

    settings.stop = function(e, ui) {
      if (window.calloutVisible == false) {
        $(this).find('.slider_callout').fadeIn('fast', function() { window.calloutVisible = true;});
        $(this).find('.slider_callout').css('left', ui.handle.css('left')).text(Math.round(ui.value));
      }
			$(this).find('.slider_callout').fadeOut('fast', function() { window.calloutVisible = false; });

      $('#' + elem_id).val(ui.value);

      if (settings.callbacks && settings.callbacks.stop) {
        try {
          eval(settings.callbacks.stop)(e, ui);
        }
        catch (err) {
        };
      };
    };

    settings.slide = function(e, ui) {
      $('.slider_callout').css('left', $(ui.handle).css('left')).text(Math.round(ui.value));
      if (settings.callbacks && settings.callbacks.slide) {
        try {
          eval(settings.callbacks.slide)(e, ui);
        }
        catch (err) {
        };
      };
    }; 

    settings.handle = '.slider_handle';
    settings.steps = settings.max - settings.min;

    var slider_wrapper = $('<div />').addClass('slider_wrapper');
    var slider_bar = $('<div />').addClass('slider_bar').attr({'id':'slider_bar_' + elem_id}).appendTo(slider_wrapper);
    
    slider_bar.append(
      $('<div />').addClass('left')
    ).append(
      $('<div />').addClass('right')
    ).append(
      $('<div />').addClass('slider_callout')
    ).append(
      $('<div />').addClass('slider_handle')
    );
  
    $('#' + elem_id).after(slider_wrapper);

    slider_wrapper.siblings('.field-prefix').prependTo(slider_wrapper);
    slider_wrapper.siblings('.field-suffix').appendTo(slider_wrapper);

    $('.large_label, .small_label').show();

    slider_bar.parents('.fieldset-wrapper').addClass('slider');
    slider_bar.width(parseInt(settings.width)-10).slider(settings);
  });
});