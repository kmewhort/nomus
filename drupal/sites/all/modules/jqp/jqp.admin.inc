<?php
// $Id: jqp.admin.inc,v 1.6 2010/02/17 07:40:42 skilip Exp $

/**
 * @file
 *
 * This file includes all functionality for the libraries configuration pages
 */

/**
 * Page callback for admin/build/jqp.
 *
 * Lists all installed javascript libraries.
 */
function jqp_status_page() {
  $libraries = jqp_rebuild_cache();

  if (!empty($libraries)) {

    // Set the table header.
    $header = array(t('Name'), t('Description'), t('Versions'), t('Operations'), t('Status'));

    foreach ($libraries as $name => $library) {

      // Reset the status class to 'ok'.
      $status = 'ok';

      // Reset all arrays.
      $versions = $links = $warnings = array();

      // Get all versions and check wheter all files exists
      foreach (array('scripts', 'stylesheets') as $type) {

        // Proceed only if there are files added to this type
        if (!empty($library->info[$type])) {
          foreach ($library->info[$type] as $v => $files) {
            unset($files['changed'], $files['base']);

            $versions[$v] = $v == 0 ? 'default' : $v;
            $links[$v] = l(t('configure'), "admin/build/jqp/$name/$v");

            // Check if all files for this version exist
            foreach ($files as $file) {
              if (!file_exists($file)) {
                // The file does not exist.
                // Set the status of this version to 'warning' and stop checking.
                $warnings[$v] = TRUE;
                $status = 'warning';
                continue;
              }
            }
          }
        }
      }

      // Reset the $first which is used to check if we're in the first tr of a library
      $first = TRUE;
      foreach (array_keys($versions) as $v) {
        $library_name = '';
        if ($first) {
          if ($library->info['project_url']) {
            $library_name = l($library->info['name'], $library->info['project_url'], array('attributes' => array('title' => t('Click here to go to %library\'s project page.', array('%library' => $library->info['name'])))));
          }
          else {
            $library_name = $library->info['name'];
          }
        }

        // Create the unique table row
        $rows[$name . $v] = array(
          'data' => array(
            ($library_name ? "<strong>$library_name</strong>" : ''),
            $first ? $library->info['description'] : '',
            $versions[$v],
            $links[$v],
            theme('image', "misc/watchdog-". ($warnings[$v] ? 'warning' : 'ok') .".png"),
          ),
          'class' => ($first ? 'first ' : '') . $status,
        );
        // The first table row for this library is created.
        // Set $first to FALSE for reference.
        $first = FALSE;
      }

      $rows[$name . $v]['class'] .= ' last';
    }

    // Add a bit of custom css to overwrite the default 'odd' and 'even' behaviour.
    drupal_add_css(drupal_get_path('module', 'jqp') .'/jqp.admin.css');

    // Create the table
    $output = theme('table', $header, $rows, array('class' => 'libraries_table multirow'));
  }
  else {
    // Nothing to show.
    $output = "<p>". t('No javascript libraries installed yet!') ."</p>";
  }

  return $output;
}

/**
 * Builds the form which is used to edit a single version of a library
 */
function jqp_library_edit(&$form_state, $library, $version = 0, $op = NULL) {

  $types = array('scripts', 'stylesheets');
  $added_types = array();

  // Used to create an array for form fields which needs to be rendered by the theme function
  $form['files'] = array('#type' => 'value', "#value" => array());

  $index = 0;

  $form['library'] = array('#type' => 'value', "#value" => $library);
  $form['version'] = array('#type' => 'value', "#value" => $version);

  $form['submit'] = array('#type' => 'submit', '#value' => t('Submit'));

  // If operation is 'add' we want to display empty fields.
  // We overwrite $library with an empty placeholder.
  if ($op == 'add') {
    $form['add'] = array('#type' => 'value', '#value' => TRUE);
    _jqp_form_elements($form, $index, array('version' => $version));
    return $form;
  }

  foreach ($types as $type) {
    if (!empty($library->info[$type][$version])) {
      $added_types[] = $type;

      foreach ($library->info[$type][$version] as $key => $path) {
        if (!in_array($key, array('changed', 'base'))) {
          $values = array(
            'name' => $key,
            'path' => $path,
            'version' => $version,
            'type' => $type,
            'operations' => array(
              l(t('remove'), "admin/build/jqp/$library->name/$version/remove_file", array('query' => "type=$type&file=". urlencode($key))),
            )
          );
          _jqp_form_elements($form, $index, $values);
          $index++;
        }
      }
    }
  }

  // If this is not the default version (0), and there are files types available which aren't added yet,
  // we add it here and disable it.
  if ($version !== 0) {
    foreach ($types as $type) {
      if (!in_array($type, $added_types) && isset($library->info[$type][0])) {

        foreach ($library->info[$type][0] as $key => $path) {
          if ($key !== 'changed') {
            _jqp_form_elements($form, -($index), array('name' => $key, 'path' => $path, 'version' => 0));
            $index++;
          }
        }
      }
    }
  }

  $form['reset'] = array('#type' => 'submit', '#value' => t('Reset'));

  return $form;
}

/**
 * Helper function for dynamically creating the form elements required by a single file.
 */
function _jqp_form_elements(&$form, $index, $values = array()) {

  $form["file-$index-name"] = array(
    '#title' => t('Name'),
    '#type' => 'textfield',
    '#default_value' => $values['name'],
    '#size' => 10,
    '#required' => !($index < 0),
    '#disabled' => ($index < 0),
  );
  $form["file-$index-path"] = array(
    '#title' => t('Path'),
    '#type' => 'textfield',
    '#size' => FALSE,
    '#default_value' => $values['path'],
    '#autocomplete_path' => 'jqp_autocomplete',
    '#required' => !($index < 0),
    '#disabled' => ($index < 0),
  );
  $form["file-$index-type"] = array(
    '#title' => t('Type'),
    '#type' => 'select',
    '#default_value' => $values['type'],
    '#options' => array('scripts' => t('Javascript'), 'stylesheets' => t('Stylesheet')),
    '#required' => !($index < 0),
    '#disabled' => ($index < 0),
  );
  $form["file-$index-operations"] = array(
    '#value' => (!empty($values['operations']) ? join($values['operations']) : ''),
  );
  $form["file-$index-status"] = array(
    '#type' => 'value',
    '#value' => file_exists($values['path']),
  );
  $form["file-$index-version"] = array(
    '#type' => 'value',
    '#value' => $values['version'],
  );
  $form['files']['#value'][$index] = $values['name'];
}

/**
 * Submit handler for jqp_library_edit().
 */
function jqp_library_edit_submit($form, &$form_state) {

  $library = $form_state['values']["library"];
  $version = $form_state['values']["version"];

  if ($form_state['values']['op'] == t('Reset')) {
    $form_state['redirect'] = "admin/build/jqp/$library->name/$version/reset";
    return;
  }

  foreach (array_keys($form_state['values']['files']) as $index) {
    if ($index >= 0) {
      $name = $form_state['values']["file-$index-name"];
      $path = $form_state['values']["file-$index-path"];
      $type = $form_state['values']["file-$index-type"];

      $library->info[$type][$version][$name] = $path;
      $library->info[$type][$version]['changed'] = TRUE;
    }
  }

  db_query("UPDATE {system} SET info = '%s' WHERE type = '%s' AND name = '%s'", serialize($library->info), 'javascript library', $library->name);
  jqp_rebuild_cache();
  drupal_set_message(t('The configuration options have been saved!'));
  $form_state['redirect'] = "admin/build/jqp";

  if ($form_state['values']['add']) {
    $form_state['redirect'] .= "/$library->name/$version";
  }
}

/**
 * Theme function for the jqp_library_edit form.
 * Creates a table in which each tr contans all form elements for a single file.
 */
function theme_jqp_library_edit($form) {

  if (!empty($form['files']['#value'])) {
    // Set the table header.
    $header = array(t('Filename'), t('Path'), t('Type'), t('Version'), t('Operations'), t('Status'));

    ksort($form['files']['#value']);

    foreach ($form['files']['#value'] as $index => $file) {
      $version = $form["file-$index-version"]['#value'];
      if ($new = ($form['add']['#value'])) {
        unset($form["file-$index-name"]['#title']);
      }
      else {
        $form["file-$index-name"]['#type'] = 'hidden';
        $name = $form["file-$index-name"]['#value'];
        $form["file-$index-type"]['#type'] = 'hidden';
        $type = $form["file-$index-type"]['#value'];
      }

      unset($form["file-$index-path"]['#title'], $form["file-$index-type"]['#title']);
      if ($form['version']['#value'] !== $version) {
        $version = l(($version == 0 ? t('default') : $version), "admin/build/jqp/". $form['library']['#value']->name ."/$version");
      }
      else {
        $version = ($version == 0 ? t('default') : $version);
      }

      $status = ($form["file-$index-status"]['#value'] ? 'ok' : 'warning');
      $rows[] = array(
        'data' => array(
          $new ? drupal_render($form["file-$index-name"]) : $name,
          drupal_render($form["file-$index-path"]),
          $new ? drupal_render($form["file-$index-type"]) : $form["file-$index-type"]['#options'][$type],
          $version,
          drupal_render($form["file-$index-operations"]),
          theme('image', "misc/watchdog-$status.png", $status, $status, NULL, FALSE),
        ),
        'class' => $status,
      );
    }

    $path = drupal_get_path('module', 'jqp');
    drupal_add_css("$path/jqp.admin.css");
    drupal_add_js("$path/jqp.admin.js");
    drupal_add_js(array('jqp_module_path' => $path), 'setting');

    return theme('table', $header, $rows, array('class' => 'libraries_table')) . drupal_render($form);
  }
}

/**
 * Wrapper function for all actions which needs confirmation before execution.
 */
function jqp_confirm_form(&$form_state, $library = NULL, $version = 0) {

  $form['library'] = array('#type' => 'value', '#value' => $library);
  $form['version'] = array('#type' => 'value', '#value' => $version);
  $form['file'] = array('#type' => 'value', '#value' => $_GET['file']);
  $form['type'] = array('#type' => 'value', '#value' => $_GET['type']);

  if (isset($library, $version, $_GET['file'], $_GET['type'])) {
    return confirm_form($form, t('Are you sure you want to remove the file %item from %library', array('%item' => $_GET['file'], '%library' => $library->info['name'])), "admin/build/jqp/$library->name/$version", t('Removal of this file only applies to version !version of this library.', array('!version' => $version)), t('Remove'));
  }
  elseif (isset($library, $version)) {
    return confirm_form($form, t('Are you sure you want to reset the item %item to its default values?', array('%item' => $library->info['name'])), "admin/build/jqp/$library->name/$version", t('Any customizations will be lost. This action cannot be undone.'), t('Reset'));
  }
  elseif (arg(3) == 'rebuild_confirm') {
    $form = array('reset_all' => array('#type' => 'value', '#value' => TRUE));
    return confirm_form($form, t('Are you sure you want to completely rebuild all javascript libraries?'), "admin/build/jqp", t('Any customizations will be lost. This action cannot be undone.'), t('Reset'));
  }
  else {
    drupal_goto('admin/build/jqp');
  }
}

/**
 * Submit handler for jqp_confirm_form().
 */
function jqp_confirm_form_submit($form, &$form_state) {

  $library = $form_state['values']["library"];
  $version = $form_state['values']["version"];
  $file = $form_state['values']["file"];
  $type = $form_state['values']["type"];
  $op = $form_state['values']['op'];
  $reset_all = $form_state['values']['reset_all'];

  $form_state['redirect'] = "admin/build/jqp";

  switch ($op) {
    case t('Remove'):
      unset($library->info[$type][$version][$file]);
      $message = t('The file has been unattached.');
      break;
    case t('Reset'):
      if (!$reset_all) {
        foreach (array('scripts', 'stylesheets') as $type) {
          if (is_array($library->info[$type])) {
            foreach (array_keys($library->info[$type]) as $key) {
              unset($library->info[$type][$key]['changed']);
            }
          }
        }
        $message = t('The javascript library has been reset to its default settings!');
      }
      break;
  }
  if ($reset_all) {
    $message = t('All javascript libraries has been reset to their default settings!');
  }
  else {
    db_query("UPDATE {system} SET info = '%s' WHERE filename = '%s'", serialize($library->info), $library->filename);
    $form_state['redirect'] .= "/$library->name/$version";
  }

  if ($op == t('Reset')) {
    jqp_rebuild_cache($reset_all);
  }

  drupal_set_message($message);
}

/**
 * AJAX callback function for Drupal.behaviors.libraryCheckInput().
 *
 * Validates the path suggested in the input form field from jqp_library_edit().
 *
 * @see jqp.admin.js
 * @see jqp_library_edit()
 */
function _jqp_ajax_check_file() {
  $p = (object)$_POST;
  if (isset($p->path) && user_access('administer javascript libraries')) {

    $result['result'] = (isset($p->path) && file_exists($p->path) && is_file($p->path));

    if ($result['result'] && isset($p->type)) {
      $type = ($p->type == t('Javascript') ? '.js' : ($p->type == t('Stylesheet') ? '.css' : FALSE));

      if ($type) {
        $result['result'] = (strrpos($p->path, "$type") == (drupal_strlen($p->path) - drupal_strlen($type)));
      }
    }
    print drupal_to_js($result);
  }
}

/**
 * AJAX response function for #autocomplete form elements.
 *
 * Returns all .css and .js file paths matching the search string.
 */
function _jqp_autocomplete() {
  $matches = array();
  $args = func_get_args();
  if (!empty($args)) {
    $string = join('/', $args);
    foreach (jqp_scan_dir() as $file) {
      if (strstr($file->filename, $string)) {
        $matches[$file->filename] = $file->filename;

        // stop if the matches exceed 10 elements
        if (count($matches) >= 20) {
          break;
        }
      }
    }
  }
  drupal_json($matches);
}

/**
 *
 */
function jqp_stop_message($name) {
  $stopped_libraries = variable_get('jqp_stop_message_libraries', array());
  if (!in_array($name, $stopped_libraries)) {
    $stopped_libraries[] = $name;
  }
  variable_set('jqp_stop_message_libraries', $stopped_libraries);
  drupal_goto($_GET['destination']);
}


/**
 * Rebuilds the javascript libraries cache.
 *
 * It registers both all libraries defined in library info files,
 * as well as libraries defined in hook_jqp.
 *
 * @param $reset
 *   If true, all javascript libraries are completely reindexed.
 *   Otherwise only libraries which havent been costumized by administrators are reindexed.
 */
function jqp_rebuild_cache($reset = FALSE) {

  cache_clear_all('jqp', 'cache');

  // Set defaults for library info
  $defaults = array(
    'description' => '',
    'version' => NULL,
  );

  // The following code is merely copied from drupal_system_listing.
  // Added the file_directory to be a possible location for the library directory.
  $profile = variable_get('install_profile', 'default');
  $directory = 'libraries';
  $config = conf_path();
  $file_dir = file_directory_path();
  $libraries = array();

  $searchdir = array(
    "$directory",
    "sites/all/$directory",
    "profiles/$profile/$directory",
    "$config/$directory",
    "$file_dir/$directory",
  );

  // Get current list of libraries registerd by dot-info files
  foreach ($searchdir as $dir) {
    if (file_exists($dir)) {
      $libraries = array_merge($libraries, file_scan_directory($dir, '\.info$', array('.', '..', 'CVS'), 0, TRUE, 'name', 0));
    }
  }

  // Get all libraries registerd by modules
  $module_libraries = module_invoke_all('jqp');
  if (is_array($module_libraries)) {
    foreach ($module_libraries as $module_library_name => $module_library) {
      $libraries[$module_library_name] = (object) ($module_library + array('type' => 'javascript library'));
    }
  }

  // Extract current files from database.
  system_get_files_database($libraries, 'javascript library');

  ksort($libraries);

  foreach ($libraries as $name => $library) {
    $versions_changed = array();

    // Look for the info file.
    $library->info = drupal_parse_info_file(dirname($library->filename) .'/'. $library->name .'.info');

    // If the library doesn't provide info, we're dealing with an module registered library.
    // We'll create the info manually.
    if (empty($library->info)) {
      $info = clone $library;
      unset($info->type, $info->info, $libraries[$name]->scripts, $libraries[$name]->styles);
      $libraries[$name]->info = (array) $info;
      $libraries[$name]->name = $name;
      $libraries[$name]->filename = "$name-module-jqp";
    }

    // Merge in defaults and save.
    $libraries[$name]->info = $library->info + $defaults;

    // Used to merge saved data with loaded data
    $old_library = jqp_library_load($name);
    $libraries[$name]->is_new = (empty($old_library));

    // Loop trough each file type
    foreach (array('stylesheets', 'scripts') as $type) {

      // Continue if no data is found for this file type
      if (empty($libraries[$name]->info[$type])) {
        continue;
      }

      $files = array();

      // Get the data of each file type per version
      foreach ($libraries[$name]->info[$type] as $version => $data) {
        // Check whether this version have been madified and saved before
        if (isset($old_library->info[$type][$version]['changed']) && $reset === FALSE) {
          $versions_changed[] = $version;
        }

        foreach ($data as $file_name) {
          $dirname = file_exists($libraries[$name]->filename) ? dirname($libraries[$name]->filename) : '';
          $files[$version][$file_name] = $dirname ? "$dirname/$file_name" : $file_name;

          // Let's store the basepath of the dot-info file
          $library->info['base'] = ($dirname ? $dirname : NULL);
        }
      }

      $libraries[$name]->info[$type] = $files;
    }

    if (!empty($versions_changed)) {
      foreach ($versions_changed as $version_changed) {
        if (isset($libraries[$name]->info['scripts'][$version_changed])) {
          $libraries[$name]->info['scripts'][$version_changed] = $old_library->info['scripts'][$version_changed];
        }
        if (isset($libraries[$name]->info['stylesheets'][$version_changed])) {
          $libraries[$name]->info['stylesheets'][$version_changed] = $old_library->info['stylesheets'][$version_changed];
        }
      }
    }

    // Invoke hook_system_info_alter() to give installed modules a chance to
    // modify the data in the .info files if necessary.
    drupal_alter('system_info', $libraries[$name]->info, $libraries[$name]);

    // Get all versions and check wheter all files exists
    $library->status = 1;
    foreach (array('scripts', 'stylesheets') as $type) {

      // Proceed only if there are files added to this type
      if (!empty($libraries[$name]->info[$type])) {
        foreach ($libraries[$name]->info[$type] as $v => $files) {
          unset($files['changed'], $files['base']);

          $versions[$v] = $v == 0 ? 'default' : $v;
          $links[$v] = l(t('configure'), "admin/build/jqp/$name/$v");

          // Check if all files for this version exist
          foreach ($files as $file) {
            if (!file_exists($file)) {
              // The file does not exist.
              // Set the status of this version to 0 and stop checking.
              $library->status = 0;
              continue;
            }
          }
        }
      }
    }

    // Update the contents of the system table
    if (!$library->is_new || (isset($library->old_filename) && $library->old_filename != $library->filename)) {
      db_query("UPDATE {system} SET info = '%s', name = '%s', filename = '%s', bootstrap = %d, status = %d WHERE filename = '%s'", serialize($libraries[$name]->info), $library->name, $library->filename, $bootstrap, $library->status, $library->old_filename);
    }
    else {
      // This is a new library.
      db_query("INSERT INTO {system} (name, info, type, filename, status, throttle, bootstrap) VALUES ('%s', '%s', '%s', '%s', %d, %d, %d)", $name, serialize($libraries[$name]->info), 'javascript library', $libraries[$name]->filename, $library->status, 0, $bootstrap);
    }
  }
  return $libraries;
}

/**
 * Fetches all cached libraries from the system table
 */
function jqp_list($refresh = FALSE) {
  static $list;

  if ($refresh) {
    $list = array();
  }

  if (empty($list)) {
    $list = array();
    $libraries = array();
    $result = db_query("SELECT * FROM {system} WHERE type = '%s'", 'javascript library');

    while ($library = db_fetch_object($result)) {
      $library->info = unserialize($library->info);
      $libraries[] = $library;

      foreach (array('stylesheets', 'scripts') as $type) {
        if (empty($library->info[$type])) {
          continue;
        }

        foreach ($library->info[$type] as $version => $data) {
          foreach ($data as $file_name => $path) {
            if (file_exists($path)) {
              $library->{$type}[$version][$file_name] = $path;
            }
          }
        }
      }
      $list[$library->name] = $library;
    }
  }
  return $list;
}

/**
 * Scan the 'libraries', 'modules' and 'themes' directories for .js and .css files
 *
 * @return an array where key is file name and value is file path
 *
 *   Example:
 *     array('sites/all/modules/cck/content.js' => (object)array(
 *       'filename' => 'sites/all/modules/cck/content.js',
 *       'basename' => 'content.js',
 *       'name' => 'content',
 *     );
 *
 *  LIKE MODULES, javascript libraries should have unique file names
 *
 * @see _jqp_autocomplete()
 */
function jqp_scan_dir() {
  static $files;
  if (!isset($files)) {
    if ($cache = cache_get('jqp')) {
      $files = $cache->data;
    }
    else {
      $profile = variable_get('install_profile', 'default');
      $config = conf_path();
      $file_dir = file_directory_path();
      $files = array();

      foreach (array('modules', 'libraries', 'themes') as $directory) {
        $searchdir[] = "$directory";
        $searchdir[] = "sites/all/$directory";
        $searchdir[] = "profiles/$profile/$directory";
        $searchdir[] = "$config/$directory";
        $searchdir[] = "$file_dir/$directory";
      }

      foreach ($searchdir as $dir) {
        if (file_exists($dir)) {
          $files = array_merge($files, file_scan_directory($dir, '(\.js$|\.css$)', array('.', '..', 'CVS'), 0, TRUE, 'filename', 0));
        }
      }

      cache_set('jqp', $files);
    }
  }
  return $files;
}
