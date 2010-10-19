<?php
// $Id: vud_term.views.inc,v 1.1.2.1 2010/06/29 05:35:39 marvil07 Exp $

/**
 * Implementation of hook_views_data_alter().
 */
function vud_term_views_data_alter(&$data) {
  $data['node']['vud_term_widget'] = array(
    'group' => t('Vote Up/Down'),
    'title' => t('Term Widget'),
    'help' => t('Provide a term widget for the Vote up/Down module.'),
    'field' => array(
      'handler' => 'vud_term_handler_field_widget',
    ),
  );
}

/**
 * Implementation of hook_views_handlers().
 */
function vud_term_views_handlers() {
  return array(
    'info' => array(
      'path' => drupal_get_path('module', 'vud_term') . '/views'
    ),
    'handlers' => array(
      'vud_term_handler_field_widget' => array(
        'parent' => 'views_handler_field',
      ),
    ),
  );
}
