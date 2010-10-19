<?php
/**
 * Hook render implementation
 */
function case_render(&$node) {
	// combine the casename and citation into the header
	if(!empty($node->field_meta_casename[0]['value']) &&
	   !empty($node->field_meta_citation[0]['value'])) {
		$node->field_heading[0]['value'] =
			trim($node->field_meta_casename[0]['value'], " ,") . ', ' .
			trim($node->field_meta_citation[0]['value'], " ,");
	}
	else {
		$node->field_heading[0]['value'] = null;
	}
		
	// set the body to the HTML returned by GATE
	$node->body = file_get_contents(
		$node->field_parsed_html[0]['filepath']);
}

function case_view(&$node, $teaser, $page){
		$node->content['body']['#value'] = 
			nomus_saveprint_links($node->nid) .
			flag_create_link('abuse_node', $node->nid) .
			case_tabs($node);
		$node->content['group_metadata']['#weight'] = -100;
}

// generate tabs
function case_tabs(&$node) {

	$tabs = array();
		
	// create a tab to the content of each language
	$curTab = null;
	$translations = translation_node_get_translations($node->tnid);
   $languages = language_list();
   foreach ($languages as $langcode => $language) {
   	$newTab = array(
        	 'type' => 'freetext',
          'title' => $language->native,);
   	
   	if($node->language == $langcode){
   		$newTab['text'] = '<div id="doc"><iframe name="dociframe_' . $langcode . '" id="dociframe" frameborder="0" scrolling="no" src="' .
				url('nomus/raw', array('query' => 'nodeId=' . $node->nid)) .
				'"></iframe></div>';
        	$curTab = $newTab;
      }
      else if (isset($translations[$langcode])) {
      	$newTab['text'] = '<div id="doc"><iframe name="dociframe_' . $langcode . '" id="dociframe" frameborder="0" scrolling="no" src="' .
				url('nomus/raw', array('query' => 'nodeId=' . $translations[$langcode]->nid)) .
				'"></iframe></div>';
        	$tabs["node_translation_$langcode"] = $newTab;
      }
      else {
      	// TODO: google translate link
      }
   }
   
   // create a tab for the cites table
   if($node->field_citestable_cites[0]['value']) {
 		// create the cites table
		$header = array(
			array(data => t('Referenced Document')),
			array(data => t('Cited as')),
			array(data => t('Context'), 'class' => 'unsortable'),
		);
		$rows = unserialize($node->field_citestable_cites[0]['value']);	
	
		$tableHtml = theme_table($header, $rows,
			array('class' => 'sortable', 'id' => 'sortable_citestable'));
   	
   	$newTab = array(
  			'type' => 'freetext',
     		'title' => t('References') . " (" . count($rows) . ")",
     		'text' => $tableHtml);
     	$tabs['citestable'] = $newTab;
   }
   
   // tab for the note-up table
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
   
   $tabs = array_merge(array('default' => $curTab), $tabs);

	$quicktabs['qtid'] = 'case-tabs';
	$quicktabs['tabs'] = $tabs;
	$quicktabs['style'] = 'Arrows';
	$quicktabs['ajax'] = FALSE;
	$quicktabs['default_tab'] = 'default';
	return theme('quicktabs', $quicktabs);
}

?>