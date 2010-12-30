<?php
function nomus_block($op = 'list', $delta = 0, $edit = array()) {
  global $user;
  switch ($op) {
    case 'list':
      // create blocks for parallel searches
      $maxBlocks = variable_get('nomus_search_max_blocks', '8');
      for($i = 1; $i <= $maxBlocks; $i++){ 
      	$blocks[$i] = array(
					'info' => t('Search block ') . $i . '/' . $maxBlocks,
					'cache' => BLOCK_CACHE_PER_USER,
					'status' => 1,
					'region' => 'contenttop',
					'weight' => 100,
				);
		}
      
		// add a block to create a new search template
		$blocks['new'] = array(
			'info' => t('Create new search template'),
			'cache' => BLOCK_CACHE_PER_ROLE,
			'status' => 1,
			'region' => 'contenttop',
			'weight' => 300,
			);
      return $blocks;

    case 'view':
    	// if this is the new block, simply return a link to create a new node
    	if($delta == 'new'){
    		// enable modal dialogs
			modalframe_parent_js();
	
			$newUrl = base_path() . 'node/add/search-template';
    		$block = array('subject' => ' ',
    			'content' => "<ul><li><a class='modal-link' href='$newUrl'>New search template...</a></li></ul>",
    			);
    		return $block;
    	}
    	
    	// get the default and user search templates
    	$blockInfo = null;
		$dbresult = db_query("SELECT nid, title, uid FROM {node} WHERE (uid = 1 OR uid = " . $user->uid .") AND type = 'search_template' ORDER BY nid");
		$maxBlocks = variable_get('nomus_search_max_blocks', '8');
		for($i = 1; $i <= $delta; $i++){
			// if there're no more rows, all templates have been displayed
			if(!($blockInfo = db_fetch_array($dbresult))){
				return array();
			}
		}
    	
      // enable jquery ui autocomplete
  		jquery_ui_add(array('ui.widget', 'ui.mouse', 'ui.selectmenu', 'ui.position', 'ui.autocomplete'));
  		drupal_add_js(drupal_get_path('module', 'nomus')
  				. '/searchblocks.js');
  		
  		// set search settings for this block in js
  		$params = array();
  		nomus_search_template_search_params($blockInfo['nid'], $params);
  		$searchUrl =  url(drupal_get_path('module', 'nomus')
  				. '/quicksearch.php', array('absolute' => TRUE, 'language' => ''));
  		$settings = array(
  				'params' => $params,
  				'url' => $searchUrl,
  				'template' => $blockInfo['nid']);
  		drupal_add_js(array('nomus_search_block_' . $delta =>
  			$settings),'setting');
  			
		// return a placeholder
		$block = array('subject' => t($blockInfo['title']),
          'content' => '<div><ul></ul></div>');
      if($blockInfo['uid'] == $user->uid){
  			$block['edit_link'] = url('node/' . $blockInfo['nid'] . '/edit', array('absolute' => TRUE));
  		}
      return $block;
      break;

    case 'configure':
      break;

    case 'save':
      break;
  }
}
