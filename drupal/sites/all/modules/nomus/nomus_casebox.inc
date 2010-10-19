<?php
// Add 'Scan document for citations' link in bookmarks menu
//
// IMPORTANT NOTE: the following command has to be added to
// to bookmarks_block() (Bookmarks was also significantly modified
// to strip out unused content and rename field names to be more
// intuitive):
//  drupal_alter('bookmarks_block', $block);
function nomus_bookmarks_block_alter(&$block){
	$block['content'] .= '<div class="bookmark_commands">'
		. '<div>' . l('Scan a document for citations', 'node/add/casebox-upload') . '</div>'
		. '<div>' . l('Bulk Save/E-mail', 'bookmarks/save_email') . '</div>'
	 	. '</div>';
}


// add a submit handler to the casebox-upload CCK type
function nomus_casebox_form_alter(&$form, $form_state, $form_id){
	if($form_id == 'casebox_upload_node_form'){
  		$form['#submit'][] = 'nomus_casebox_form_submit';
  		$form['buttons']['submit']['#value'] = t('Scan (this may take some time)');
  		unset($form['buttons']['preview']);
	}
}

// parse citations from uploaded document
function nomus_casebox_form_submit($form, &$form_state) {
	global $user;
	//$filepath = $form['field_casebox_upload'][0]['#value']['filepath']);
	
	// cast the form state into a fake node
	$fakeNode = (object) $form_state['values'];
	
	// document source to use for parsing
	$dbresult = db_query("SELECT nid FROM {node} WHERE title = 'Casebox Upload Document Source' AND type = 'document_source'");
	$docSourceNid = db_result($dbresult);
	
	// parse
	nomus_documentconverter_convert($fakeNode, 'field_casebox_upload');
	nomus_documentparser_parse($fakeNode, $docSourceNid);
	nomus_documentcitetables_generate($fakeNode, false);
	
	// copy pertinent info from the fake node back into the form for the values
	// to be saved
	$values = get_object_vars($fakeNode);
	$form_state['values']['field_converted_file'] = $values['field_converted_file'];
	$form_state['values']['field_parsed_content_tags'] = $values['field_parsed_content_tags'];
	$form_state['values']['field_parsed_html'] = $values['field_parsed_html'];
	$form_state['values']['field_citestable_cites'] = $values['field_citestable_cites'];
	
	// create bookmark category
	$bookmarkCategory['menu_name'] = 'bookmarks-' . $user->uid;
	$bookmarkCategory['link_title'] = $fakeNode->field_casebox_upload[0]['filename'];
	$bookmarkCategory['expanded'] = true;
	$bookmarkCategory['link_path'] = 'bookmarks/mine';
	$bookmarkCotegary['module'] = 'bookmarks';
	$categoryId = menu_link_save($bookmarkCategory);
	
	// create bookmarks for any cases in Nomus' db
	$bookmarkAddCount = 0;
	if($fakeNode->field_citestable_cites[0]['value']) {
		$citesTable = unserialize($fakeNode->field_citestable_cites[0]['value']);
		foreach($citesTable as $citesRow){
			// don't bookmark placeholders
			$nid = $citesRow[2]['data-nid'];
			$dbresult = db_query("SELECT type FROM {node} WHERE nid = '" . $nid . "'"); 
			if(db_result($dbresult) != 'placeholder'){
			
				// save the bookmark
				$bookmark = array();
				$bookmark['menu_name'] = 'bookmarks-' . $user->uid;
				$bookmark['link_title'] = strip_tags($citesRow[0]['data']);
				$bookmark['link_path'] = 'node/' . $nid;
				$bookmark['plid'] = $categoryId;
				$bookmark['module'] = 'bookmarks';
				$mlid = menu_link_save($bookmark);
				$bookmarkAddCount++;
				
				// update menu_node
    			menu_node_save($nid, $mlid);
			}
		}
	}
	drupal_set_message("$bookmarkAddCount documents cited have been added to your bookmarks (these citations are listed in bold below).");
}
?>