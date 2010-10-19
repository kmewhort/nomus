<?php

/**
 * Provides an list of the configured sort groups.
 *
 */
function apachesolr_custom_sort_apachesolr_sort_group_overview() {

  $settings_list = variable_get('sort_field_settings', array());

  $header = array(t('Group name'), t('Fields'), t('Path'), array('data' => t('Operations'), 'colspan' => '3'));
  $rows = array();

  foreach ($settings_list as $key => $value) {

    $row = array();

    $row[] = l($value->name, 'admin/settings/apachesolr_sort_group/'. $value->name .'/edit');

    $fields = '';
    foreach ($value->fields as $solr_field => $sort_settings) {
      $fields .= $solr_field .'<br/>';
    }
    $row[] = $fields;

    $row[] = $value->visibility['pages'];
    $row[] = l('edit', 'admin/settings/apachesolr_sort_group/'. $value->name .'/edit');
    $row[] = l('delete', 'admin/settings/apachesolr_sort_group/'. $value->name .'/delete');

    $rows[] = $row;
  }

  $output = theme('table', $header, $rows);
  return $output;
}

/**
 * Provides a form to configure a sort group.
 *
 * @param mixed $form_state
 * @param mixed $group_settings
 */
function apachesolr_custom_sort_apachesolr_sort_group_form($form_state, $group_settings=NULL) {

  $form = array();

  if (isset($form_state['sort-settings'])) {
    $group_settings = $form_state['sort-settings'];
  }
  elseif ($group_settings==NULL) {
    // if nothing in state, and no group_settings then a group setting is been added
    $group_settings->fields[''] = '';
  }

  // the "tree" setting ensures that the nested elements in the form get submitted correctly. Without this
  // flag the elements are flattened and only the first sort is submitted.
  $form['#tree'] = TRUE;
  $form['#cache'] = TRUE;

  $form['group-name'] = array(
                      '#type' => 'textfield',
                      '#title' => t('Group name'),
                      '#description' => t('Identifies this specific group of sort fields and paths.'),
                      '#default_value' => $group_settings->name,
                    );

  $form['sort-group-fields'] = array(
    '#type' => 'fieldset',
    '#title' => t('Selected fields for sorting'),
    '#collapsible' => TRUE,
  );

  $form['sort-group-fields']['add_sort'] = array(
    '#type' => 'submit',
    '#value' => t('Add Sort Field'),
    '#attributes' => array('title' => t('Click here to add field.')),
    '#submit' => array('apachesolr_custom_sort_apachesolr_field_add_submit'),
    '#ahah' => array(
      'path' => 'apachesolr_sorting/ahah/callback',
      'wrapper' => 'sort-field',
      'effect' => 'fade',
    ),
  );

  $form['sort-group-fields']['sort-field'] = array(
    '#prefix' => '<div id="sort-field">',
    '#suffix' => '</div>',
    '#theme' => 'apachesolr_sort_groups',
  );

  $form['sort-group-fields']['sort-field'] += _sort_form($group_settings);

  $form['visibility-settings'] = array(
    '#type' => 'fieldset',
    '#title' => t('Visibility'),
    '#collapsible' => TRUE,
  );

  $options = array(t('Show on every page except the listed pages.'), t('Show on only the listed pages.'));
  $description = t("Enter one page per line as Drupal paths. The '*' character is a wildcard. Example paths are %blog for the blog page and %blog-wildcard for every personal blog. %front is the front page.", array('%blog' => 'blog', '%blog-wildcard' => 'blog/*', '%front' => '<front>'));

  $form['visibility-settings']['block_settings']['visibility'] = array(
    '#type' => 'radios',
    '#title' => t('Show block on specific pages'),
    '#options' => $options,
    '#default_value' => isset($group_settings->visibility['type']) ? $group_settings->visibility['type'] : 0,
  );
  $form['visibility-settings']['block_settings']['pages'] = array(
    '#type' => 'textarea',
    '#title' => t('Pages'),
    '#default_value' => $group_settings->visibility['pages'],
    '#description' => $description,
  );

  $form['submit'] = array(
    '#type' => 'submit',
    '#value' => t('Save'),
  );

  return $form;
}

/**
 * Validates the main form submit
 *
 * @param mixed $form
 * @param mixed $form_state
 */
function apachesolr_custom_sort_apachesolr_sort_group_form_validate($form, &$form_state) {

  // We don't want it to validate when adding/removing fields
  if ($form_state['clicked_button']['#id'] == 'edit-submit') {

    // convert state to settings (makes it a bit easier to traverse them)
    $settings = _form_state_to_settings($form_state);

    if (empty($settings->name)) {
      form_set_error('group-name', t('A group name is required.'));
    }
    else {
      $keys = array_keys($settings->fields);

      for ($delta = 0; $delta < count($keys); $delta++) {
        $key = $keys[$delta];
        if (empty($settings->fields[$key]['display_value'])) {
          form_set_error('sort-group-fields][sort-field]['. $delta .'][\'name\']', t("Name is required"));
        }
      }
    }
  }
}

/**
 * Outputs the ahah field list table to support drag and drop re-ordering.
 *
 * @param mixed $form
 * @return mixed
 */
function theme_apachesolr_sort_groups($form) {

  drupal_add_tabledrag('sort-group-fields', 'order', 'group', 'sort-groups-weight');

  $rows = array();

  foreach (element_children($form) as $key) {

    $form[$key]['weight']['#attributes']['class'] = 'sort-groups-weight';

    $row = array(
      'data' => array(
        array('data' => drupal_render($form[$key]['name']), 'class' => 'sort-group-name'),
        array('data' => drupal_render($form[$key]['weight']), 'class' => 'sort-group-weight'),
        array('data' => drupal_render($form[$key]['field_name']), 'class' => 'field-name'),
        array('data' => drupal_render($form[$key]['sort_order']), 'class' => 'field-sort'),
        array('data' => drupal_render($form[$key]['exposed']), 'class' => 'field-exposed'),
        array('data' => drupal_render($form[$key]['remove']), 'class' => 'field-delete'),
      ),
      'class' => 'draggable',
    );

    if (isset($form[$key]['#attributes'])) {
      $row = array_merge($row, $form[$key]['#attributes']);
    }
    $rows[] = $row;
  }

  $output = theme('table', NULL, $rows, array('id' => 'sort-group-fields'));

  return $output;
}

/**
 * Ahah handler for adding a new field to a sort group.
 *
 * @param mixed $form
 * @param stdClass $form_state
 */
function apachesolr_custom_sort_apachesolr_field_add_submit($form, &$form_state) {

  $settings = _form_state_to_settings($form_state);

  // add a new empty field to the group settings
  $settings->fields[''] = '';

  unset($form_state['submit_handlers']);
  form_execute_handlers('submit', $form, $form_state);

  // put the updated settings into the $form_state, this will get processed in
  // apachesolr_custom_sort_apachesolr_sort_group_form
  $form_state['sort-settings'] = $settings;
  $form_state['rebuild'] = TRUE;
}


/**
 * Handler to remove a specific field from a sort group.
 *
 * @param mixed $form
 * @param mixed $form_state
 */
function apachesolr_custom_sort_apachesolr_field_remove_submit($form, &$form_state) {

  // works out which row in the table the button clicked belongs to
  $button_id = $form_state['clicked_button']['#value'];
  $delta = substr($button_id, strpos($button_id, '_') + 1);

  $settings = _form_state_to_settings($form_state);

  // remove the field from the group settings
  $keys = array_keys($settings->fields);
  $key = $keys[$delta];
  unset($settings->fields[$key]);

  unset($form_state['submit_handlers']);
  form_execute_handlers('submit', $form, $form_state);

  // put the updated settings into the $form_state, this will get processed in
  // apachesolr_custom_sort_apachesolr_sort_group_form
  $form_state['sort-settings'] = $settings;
  $form_state['rebuild'] = TRUE;
}

/**
 * Confirmation form for deleting a sort group.
 *
 * @param mixed $form_state
 * @param mixed $group_settings
 * @return OA_Admin_UI_Component_Form
 */
function apachesolr_custom_sort_apachesolr_sort_group_delete_form(&$form_state, $group_settings) {

  $form['group_name'] = array('#type' => 'hidden', '#value' => $group_settings->name);

  return confirm_form($form, t('Are you sure you want to delete the sort group %name?', array('%name' => $group_settings->name)), 'admin/settings/apachesolr_sort_groups', '', t('Delete'), t('Cancel'));
}

/**
 * Submit handler for deleting a sort group.
 *
 * @param mixed $form
 * @param mixed $form_state
 */
function apachesolr_custom_sort_apachesolr_sort_group_delete_form_submit($form, &$form_state) {

  apachesolr_custom_sort_apachesolr_sort_group_delete($form_state['values']['group_name']);

  drupal_set_message(t('The sort group %name has been removed.', array('%name' => $form_state['values']['group_name'])));

  cache_clear_all();

  $form_state['redirect'] = 'admin/settings/apachesolr_sort_groups';
};

/**
 * Saves the specific settings
 *
 * @param mixed $form
 * @param mixed $form_state
 */
function apachesolr_custom_sort_apachesolr_sort_group_form_submit($form, &$form_state) {

  // Don't save changes in response to the "Add sort field" button being clicked.
  // This logic is taken from quicktabs so presume it's correct...
  if ($form_state['clicked_button']['#id'] == 'edit-submit') {

    $settings_list = variable_get('sort_field_settings', array());

    $settings = _form_state_to_settings($form_state);

    $settings_list[$settings->name] = $settings;

    variable_set('sort_field_settings', $settings_list);

    drupal_goto('admin/settings/apachesolr_sort_groups');
  }

  return TRUE;
}

/**
 * Converts the $form_state to a usable settings object.
 *
 * @param mixed $form_state
 * @return stdClass
 */
function _form_state_to_settings($form_state) {

  $settings = new stdClass();
  $settings->name = $form_state['values']['group-name'];
  $settings->visibility = array();
  $settings->visibility['type'] = $form_state['values']['visibility-settings']['block_settings']['visibility'];
  $settings->visibility['pages'] = $form_state['values']['visibility-settings']['block_settings']['pages'];
  $settings->fields = array();

  $weights = array();

  // work out the correct submitted sort order using field weight
  foreach ($form_state['values']['sort-group-fields']['sort-field'] as $key => $value ) {
    $weights[$value['weight']] = $key;
  }

  ksort($weights);

  foreach ($weights as $key => $value) {

    $display_value = $form_state['values']['sort-group-fields']['sort-field'][$value]['name'];
    $sort_order = $form_state['values']['sort-group-fields']['sort-field'][$value]['sort_order'];
    $field_name = $form_state['values']['sort-group-fields']['sort-field'][$value]['field_name'];
    $exposed = $form_state['values']['sort-group-fields']['sort-field'][$value]['exposed'];

    $settings->fields[$field_name] = array('display_value' => $display_value, 'sort' => $sort_order, 'weight' => $key, 'exposed' => $exposed);
  }

  return $settings;
}

/**
 * Ahah callback handler.
 *
 */
function apachesolr_custom_sort_apachesolr_sort_group_ahah() {

  $form_state = array('storage' => NULL, 'submitted' => FALSE);
  $form_build_id = $_POST['form_build_id'];
  $form = form_get_cache($form_build_id, $form_state);
  $args = $form['#parameters'];
  $form_id = array_shift($args);
  $form['#post'] = $_POST;
  $form['#redirect'] = FALSE;
  $form['#programmed'] = FALSE;
  $form_state['post'] = $_POST;
  drupal_process_form($form_id, $form, $form_state);
  $form = drupal_rebuild_form($form_id, $form_state, $args, $form_build_id);
  $sorts = $form['sort-group-fields']['sort-field'];
  unset($sorts['#prefix'], $sorts['#suffix']); // Prevent duplicate wrappers.
  drupal_json(array(
    'status'   => TRUE,
    'data'     => theme('status_messages') . drupal_render($sorts),
  ));
}

/**
 * Builds the sort form. Either creates a new, empty form or if ahah is been used
 * persists the currently configured sorts and adds a new one to the available list.
 *
 * @param mixed $form_state
 */
function _sort_form($group_settings) {

  $form = array();

  $delta = 0;

  foreach ($group_settings->fields as $key => $value) {
    $form[$delta] = _get_sort_widget($delta, $value['display_value'], $key, $value['sort'], $value['exposed']);
    $delta++;
  }

  return $form;
}

/**
 * Returns the sort field widget as a set of FAPI elements.
 *
 * @param mixed $delta
 *   Index of the sort field in the group
 * @param mixed $display_name
 *   Name that should be displayed on the field.
 * @param mixed $field_name
 *   Name of the SOLR field
 * @param mixed $initial_sort_order
 *   Initial sort order for the field (either 'asc' or 'desc')
 * @param mixed exposed
 *   Whether the sort should be exposed to the user.
 */
function _get_sort_widget($delta, $display_name='', $field_name='', $initial_sort_order_order='asc', $exposed=TRUE) {

  $solr = apachesolr_get_solr();
  $data = $solr->getLuke(2);

  $detailed = (array)$data->fields;
  foreach ( $detailed as $key => $options ) {
    $fields[$key] = $key;
  }

  // hard code the score field - this doesn't exist in the SOLR schema but we want it to be available to configure
  // it for a sort group
  $fields['score'] = 'score';

  ksort($fields, SORT_STRING);

  $widget = array();

  $widget['weight'] = array(
                        '#type' => 'weight',
                        '#default_value' => $delta,
                      );

  $widget['name'] = array(
                      '#type' => 'textfield',
                      '#title' => t('Name'),
                      '#description' => t('User friendly name (as it will be displayed to the user).'),
                      '#default_value' => $display_name,
                      '#size' => 20,
                    );

  $widget['field_name'] = array(
                              '#type' => 'select',
                              '#title' => t('Field name'),
                              '#description' => t('Select the field as it is mapped in the Apache Solr index.'),
                              '#default_value' => $field_name,
                              '#options' => $fields,
                            );

  $widget['sort_order'] = array(
                            '#type' => 'select',
                            '#title' => t('Initial sort order'),
                            '#description' => t('Sort ascending or descending'),
                            '#default_value' => $initial_sort_order_order,
                            '#options' => array('asc' => 'Ascending', 'desc' => 'Descending'),
                          );

  $widget['exposed'] = array(
                            '#type' => 'checkbox',
                            '#title' => t('Expose'),
                            '#description' => t('Expose in the sort block'),
                            '#default_value' => $exposed,
                          );

  $widget['remove'] = array(
                          '#type' => 'submit',
                          '#value' => 'remove_'. $delta,
                          '#attributes' => array('title' => t('Deletes the field from the group.')),
                          '#submit' => array('apachesolr_custom_sort_apachesolr_field_remove_submit'),
                          '#ahah' => array(
                            'path' => 'apachesolr_sorting/ahah/callback',
                            'wrapper' => 'sort-field',
                            'effect' => 'fade',
                          ),
                      );

  return $widget;
}