<?php
// $Id: vud_comment.views.inc,v 1.1.2.1 2010/06/29 05:35:38 marvil07 Exp $

/**
 * Implementation of hook_views_data_alter().
 */
function vud_comment_views_data_alter(&$data) {
  $data['comments']['vud_comment_widget'] = array(
    'group' => t('Vote Up/Down'),
    'title' => t('Comment Widget'),
    'help' => t('Provide a comment widget for the Vote up/Down module.'),
    'field' => array(
      'handler' => 'vud_comment_handler_field_widget',
    ),
  );
}

/**
 * Implementation of hook_views_handlers().
 */
function vud_comment_views_handlers() {
  return array(
    'info' => array(
      'path' => drupal_get_path('module', 'vud_comment') . '/views'
    ),
    'handlers' => array(
      'vud_comment_handler_field_widget' => array(
        'parent' => 'views_handler_field',
      ),
    ),
  );
}
