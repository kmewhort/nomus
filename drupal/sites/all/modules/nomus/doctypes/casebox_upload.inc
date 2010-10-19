<?php
/**
 * Hook render implementation
 */
function casebox_upload_render(&$node) {
}

function casebox_upload_view(&$node, $teaser, $page){
	$node->content['body']['#value'] = 
		casebox_upload_citetable($node);
	
	// remove the "has been updated message"
	if($_SESSION['messages']['status']) {
		foreach($_SESSION['messages']['status'] as $ind => $message) {
			if(preg_match('/' . t('has been updated') . '/is',
				$message)){
					unset($_SESSION['messages']['status'][$ind]);
			}
		}
	}
}

// generate tabs
function casebox_upload_citetable(&$node) {
 	// create the cites table
	$header = array(
		array(data => t('Referenced Document')),
		array(data => t('Cited as')),
		array(data => t('Context'), 'class' => 'unsortable'),
	);
	if($node->field_citestable_cites[0]['value'])
		$rows = unserialize($node->field_citestable_cites[0]['value']);	
	
	// form for selecting cites to add to bookmarks
	$tableHtml = theme_table($header, $rows,
		array('class' => 'sortable', 'id' => 'sortable_citestable'));
   	
   return $tableHtml;
}

?>