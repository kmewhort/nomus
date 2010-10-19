<?php

function placeholder_view(&$node, $teaser, $page){
		
	$node->content['body']['#value'] = 
		   nomus_saveprint_links($node->nid) .
			placeholder_tabs($node);
	$node->content['group_metadata']['#weight'] = -100;
}

// generate tabs
function placeholder_tabs(&$node) {

	$tabs = array();
		
	// only tab is for the note-up links
   if($node->field_citestable_citedby[0]['value']) {
 		// create the note-up table
		$header = array(
			array(data => t('Referencing Document')),
			array(data => t('Cited as')),
			array(data => t('Context'), 'class' => 'unsortable'),
		);
		$rows = unserialize($node->field_citestable_citedby[0]['value']);	
	
		$tableHtml = theme_table($header, $rows,
			array('class' => 'sortable', 'id' => 'sortable_citestable'));
   	$newTab = array(
   		'type' => 'freetext',
      	'title' => t('Note-up') . " (" . count($rows) . ")",
      	'text' => $tableHtml);
      $tabs['noteuptable'] = $newTab;
   }
   
	$quicktabs['qtid'] = 'case-tabs';
	$quicktabs['tabs'] = $tabs;
	$quicktabs['style'] = 'Arrows';
	$quicktabs['ajax'] = FALSE;
	$quicktabs['default_tab'] = 'default';
	return theme('quicktabs', $quicktabs);
}

?>