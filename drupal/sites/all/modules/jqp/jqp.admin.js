Drupal.behaviors.libraryCheckInput = function (context) {
  $('input.form-autocomplete:not(.libraryCheckInput)', context).addClass('libraryCheckInput').bind('keyup blur', function() {
    libraryCheckFile($(this));
  });
};

/**
 * Checks if the current input is a valid filename.
 */
function libraryCheckFile(input) {
  var basepath = Drupal.settings.basePath;
  var input, value, type, status_image, status, extention;

  value = input.val();
  status_image = input.parents('tr').find('img');
  type = input.parents('tr').find('td:eq(2)').text();
  status_image.attr('src', basepath + Drupal.settings.jqp_module_path + '/loading.gif');

  $.ajax({
    url:basepath + 'jqp_ajax_check_file',
    type:'post',
    dataType:'json',
    data:{path: value, type:type},
    success:function (json) {
      status = (json.result) ? 'ok' : 'warning';

      if (json.result == true) {
        extention = value.substr(value.lastIndexOf('.') + 1, value.length);
        input.parents('tr').find('.form-select').val((extention == 'js' ? 'scripts' : 'stylesheets'));
      };
      input.parents('tr').removeClass('warning ok').addClass(status);
      status_image.attr('src', basepath + 'misc/watchdog-' + status + '.png');
    }
  });
}