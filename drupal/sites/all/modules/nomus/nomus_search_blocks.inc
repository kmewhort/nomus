<?php
function nomus_block($op = 'list', $delta = 0, $edit = array()) {
  global $user;
  switch ($op) {
    case 'list':
      // create a block for each default and user search template
      if($user->uid) {
			$dbresult = db_query("SELECT nid, title FROM {node} WHERE (uid = 1 OR uid = " . $user->uid .") AND type = 'search_template' ORDER BY nid");
			while($row = db_fetch_array($dbresult)) {
				$blocks[$row['nid']] = array(
					'info' => t('Search result block for search template "') . $row['title'] . '"',
					'cache' => BLOCK_CACHE_PER_USER,
					'status' => 1,
					'region' => 'contenttop',
					'weight' => 100,
				);
			}
		}
      return $blocks;

    case 'view':
      // enable jquery ui autocomplete
  		jquery_ui_add(array('ui.widget', 'ui.mouse', 'ui.selectmenu', 'ui.position', 'ui.autocomplete'));
  		drupal_add_js(drupal_get_path('module', 'nomus')
  				. '/searchblocks.js');
  		
  		// set search settings for this block in js
  		$params = array();
  		nomus_search_template_search_params($delta, $params);
  		$searchUrl =  url(drupal_get_path('module', 'nomus')
  				. '/quicksearch.php', array('absolute' => TRUE, 'language' => ''));
  		drupal_add_js(array('nomus_search_block_' . $delta => array(
  			'params' => $params, 'url' => $searchUrl)), 'setting');
  		
  		// get the search template title
		$result = db_query('SELECT title FROM {node} WHERE nid = %d' ,$delta);
		$title = db_result($result);

		// return a placeholder
		$block = array('subject' => t($title),
          'content' => '<div><ul></ul></div>');
      return $block;
      break;

    case 'configure':
      break;

    case 'save':
      break;
  }
}
