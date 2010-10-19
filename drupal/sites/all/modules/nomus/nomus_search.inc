<?php
/*
 * Implementation of hook_apachesolr_cck_fields_alter
 */
function nomus_apachesolr_cck_fields_alter(&$mappings) {
	// remove the defaults
	foreach($mappings as $key => $val)
		unset($mappings[$key]);
	
	$mappings['per-field']['field_meta_casename'] =
		array(
      	'indexing_callback' => 'apachesolr_cck_text_indexing_callback',
      	'display_callback' => 'apachesolr_cck_text_field_callback',
      	'index_type' => 'string',);
	$mappings['per-field']['field_meta_court'] =
		array(
      	'indexing_callback' => 'apachesolr_cck_text_indexing_callback',
      	'display_callback' => 'apachesolr_cck_text_field_callback',
      	'index_type' => 'string',);
	$mappings['per-field']['field_meta_jurisdiction'] =
		array(
      	'indexing_callback' => 'apachesolr_cck_text_indexing_callback',
      	'display_callback' => 'apachesolr_cck_text_field_callback',
      	'index_type' => 'string',);
		
	// date
	$mappings['per-field']['field_meta_date'] =
		array(
      	'indexing_callback' => 'apachesolr_date_date_field_indexing_callback',
      	// Trie-Range date types.
      	'index_type' => 'tdate',
      	'facet_block_callback' => 'apachesolr_date_date_facet_block',
      	'display_callback'     => 'apachesolr_date_display_callback',
  		);
		
   // textfield => citation
   $mappings['per-field']['field_meta_citation'] =
		array(
      	'indexing_callback' => 'nomus_citation_indexing_callback',
      	'display_callback' => '',
      	'index_type' => 'string',);
      	
	// node reference field => citation
	$mappings['per-field']['field_meta_citedas'] =
		array(
      	'indexing_callback' => 'nomus_citation_indexing_callback',
      	'display_callback' => '',
      	'index_type' => 'string',);
      	
	// cite table fields (serialized arrays)
	$mappings['per-field']['field_citestable_cites'] =
		array(
      	'indexing_callback' => 'nomus_citetable_indexing_callback',
      	'display_callback' => '',
      	'index_type' => 'text',);
	$mappings['per-field']['field_citestable_citedby'] =
		array(
      	'indexing_callback' => 'nomus_citetable_indexing_callback',
      	'display_callback' => '',
      	'index_type' => 'text',);
}


// parallel citations from node references
function nomus_citation_indexing_callback($node, $field_name, $cck_info) {

  $fields = array();
  // if node has this field
  if (isset($node->$field_name)) {

	 // prefix index with [c]itation and
	 // whether [s]ingle / [m]ultiple
	 $index_key = 'c';
	 $index_key .= $cck_info['multiple'] ? 'm_' : 's_';
    $index_key .= $cck_info['name'];
    foreach ($node->$field_name as $field) {
    	if($field['nid']){
      	$fields[] = array(
        		'key' => $index_key,
        		'value' => $field['safe']['title'],
      	); }
      else if($field['value']) {
      	$fields[] = array(
        		'key' => $index_key,
        		'value' => $field['value'],
      	); 
      }
    }
  }
  return $fields;
}


// cite table
function nomus_citetable_indexing_callback($node, $field_name, $cck_info) {

  $fields = array();
  // if node has this field
  if (isset($node->$field_name)) {

	  // set to multi-valued text field 
	 $index_key = 'tm_' . $cck_info['name'];
    foreach ($node->$field_name as $field) {
    	// for each row, add each set of sentences
    	$rows = unserialize($field['value']);
    	if($rows) {
    		foreach($rows as $row) {
    			if($row[2]['data'] != null && $row[2]['data'] != '') {
      			$fields[] = array(
        				'key' => $index_key,
        				'value' => $row[2]['data'],
        				'multiple' => 'true' /* added support for single->multiple adding in apachesolr_node_to_document */ 
      			);
      		}
      	}
      }
    }
  }
  return $fields;
}

// only index cases and legislation
function nomus_apachesolr_node_exclude($node) {

	// exclude all but cases and legislation
	if($node->type != 'case' &&
	   $node->type != 'legislation') {
		return TRUE;
	}
	
	// exclude translations (the translated text is added
	// to the primary node when it is indexed)
	if($node->tnid != 0 && $node->tnid != $node->nid) {
		return TRUE;
	}
	
	// exclude unpublished documents
	if($node->status < 1){
		return TRUE;
	}
	return FALSE;
}

// pre-index modification
function nomus_apachesolr_update_index(&$document, $node) {
	// add each language body
	unset($document->body);
		
	// body and heading of primary language
	$body_fieldname = 'body_' . $node->language;
	$heading_fieldname = 'ss_heading_'  . $node->language;
	$rawNode = node_load($node->nid);
	$document->$body_fieldname = $rawNode->body;
	$document->$heading_fieldname = $rawNode->field_heading[0]['value'];
	
	// body of translations	
	$translations = translation_node_get_translations($node->tnid);
	if(count($translations) > 0) {
		foreach($translations as $lang => $tr) {
			$body_fieldname = 'body_' . $lang;
			$heading_fieldname = 'ss_heading_'  . $lang;
			$trnode = node_load($tr->nid);
			$document->$body_fieldname = $trnode->body;
			$document->$heading_fieldname = $trnode->field_heading[0]['value'];
		}
	}
}

// add template selection to search box
function nomus_search_form_alter(&$form, &$form_state, $form_id) {
	global $user;
	if($form_id == 'search_block_form') {
	
		// need to use jquery selectable (http://jquery-ui.googlecode.com/svn/branches/labs/selectmenu/)
		// so as to have edit/delete buttons
		modalframe_parent_js();
		jquery_ui_add(array('ui.widget', 'ui.mouse', 'ui.draggable', 'ui.selectmenu'));
  		drupal_add_js(drupal_get_path('module', 'nomus')
  			.'/searchtemplate_select.js');
		
		// get default templates
		$options = array();
		$dbresult = db_query("SELECT nid, title FROM {node} WHERE uid = '1' AND type = 'search_template' ORDER BY nid");
		while($row = db_fetch_array($dbresult)) {
			// default templates may be cloned, but not edited
			$options[$row['nid']] = t($row['title']) . " clone" . $row['nid']; 
		}

		// get user templates		
		if($user->uid) {
			$dbresult = db_query("SELECT nid, title FROM {node} WHERE uid = '" . $user->uid ."' AND type = 'search_template' ORDER BY nid");
			while($row = db_fetch_array($dbresult)) {
				$options[$row['nid']] = t($row['title'])  . " edit" . $row['nid'] . " delete" . $row['nid'];
			}
		}
		
		// add new template button
		$options['new'] = 'New...';
			
		// get session templates
		if($_SESSION['nomus_session_templates']) {
			// bail for security if the session var contains anything other than numbers
			// seperated by commas
			if(preg_match('/[^,\d]/', $_SESSION['nomus_session_templates']))
				$_SESSION['nomus_session_templates'] = '';
				drupal_access_denied();
				return;
			
			$dbresult = db_query("SELECT nid, title FROM {node} WHERE uid = '0' AND type = 'search_template' AND nid IN '(" . $_SESSION['nomus_session_templates'] . ") ORDERY BY nid'");
			while($row = db_fetch_array($dbresult)) {
				$options[$row['nid']] = t($row['title'])  . " edit" . $row['nid'] . " delete" . $row['nid'];
			}
		}
		
		$form['template'] = array(
  			'#type' => 'select',
  			'#title' => t('Search template'),
  			'#default_value' => t($templates[0]['nid']),
  			'#options' => $options,
  			'#weight' => 1,
  		);
  		$form['submit']['#weight'] = 2;
  		
  		// set defaults
  		if($_SESSION['nomus_search_q']){
  			$form['search_block_form']['#default_value'] =
  				$_SESSION['nomus_search_q']; 
  		}
  		if($_SESSION['nomus_search_template']){
  			$form['template']['#default_value'] =
  				$_SESSION['nomus_search_template']; 
  		}
  		
  		// add our own handler
  		$form['#submit'][] = 'nomus_search_form_submit';
  		

	}
}

function nomus_search_form_submit($form, &$form_state) {
	// set the session variables for the query and template
	// (so they can be displayed as defaults on the next page)
	$_SESSION['nomus_search_q'] = $form_state['values']['search_block_form'];
	$_SESSION['nomus_search_template'] = $form_state['values']['template'];
	
	// add the template id to the redirect path
	$form_state['redirect'] = array($form_state['redirect'],
		'template=' . $form_state['values']['template']);
}

// prepare query
function nomus_apachesolr_prepare_query(&$query, &$params, $caller) {

  // set additional sort options
  $query->set_available_sort('sort_ss_cck_field_meta_casename', array(
       'title' => t('Title'),
       'default' => 'asc',
      ));
  $query->set_available_sort('tds_cck_field_meta_date', array(
       'title' => t('Date'),
       'default' => 'desc',
      ));
   
  // get the template
  $templateNode = node_load($_GET['template']);  
  
  // field list to return
  $params['fl'] .= ',ss_heading_en';
  $params['fl'] .= ',ss_heading_fr';
  $params['fl'] .= ',cs_cck_field_meta_citation';
  $params['fl'] .= ',ss_cck_field_meta_court';
  $params['fl'] .= ',ss_cck_field_meta_jurisdiction';
  $params['fl'] .= ',tds_cck_field_meta_date';
  $params['fl'] .= ',tnid';
  
  // regular query fields
  if($templateNode->field_search_qf[0]['value'])
  	$params['qf'] = $templateNode->field_search_qf[0]['value'];
  else
  	$params['qf'] = ' ';

	// phrase fields
  if($templateNode->field_search_pf[0]['value'])
  	$params['pf'] = $templateNode->field_search_pf[0]['value'];
  else
  	$params['pf'] = ' ';
  	  
  // optional/boost fields
  if($templateNode->field_search_of[0]['value'])
  	$params['of'] = $templateNode->field_search_of[0]['value'];
  else
  	$params['of'] = ' ';
  
  // filters
  if($templateNode->field_search_fq[0]['value'])
  	$params['fq']['template_filter'] = array($templateNode->field_search_fq[0]['value']);
   
	// sort
	if(!$params['sort'] || $params['sort'] == ''){
		if($params['sort'] = $template->field_search_sort[0]['value'])
			$params['sort'] = $template->field_search_sort[0]['value'];
	}
	
	// remove the apachesolr plugin's boost function
	unset($params['bf']);
}

// alter individual result
function nomus_apachesolr_search_result_alter(&$doc) {
	global $language;
	
	// try to use the title field corresponding to the
	// current language
	$titlefield = 'ss_heading_' . $language->language;
	$field = $doc->getField($titlefield);
	if ($field && $field != '') {
       $doc->title = $field['value'];
   }
   else {
   	foreach(language_list() as $lang => $langinfo){
   		$titlefield = 'ss_heading_' . $lang;
   		$field = $doc->getField($titlefield);
   		if($field && $field != '') {
   			$doc->title = $field['value'];
				break;   		
   		}
   	}
   }
   $doc->title = preg_replace('/\&amp;\#039;/', ',', $doc->title);
   
   // try to set the nid to the current language
   $translations = translation_node_get_translations($doc->tnid);
   if(isset($translations[$language->language])) {
   	$doc->nid = $translations[$language->language]->nid;
  	}
}

// alter overall results
function nomus_apachesolr_process_results (&$results) {
	// add numbering to the results
	$pageNum = 0;
	if(isset($_GET['page']))
		$pageNum = $_GET['page']; 	
	$resultsPerPage = variable_get('apachesolr_rows', 10);
	$first = $pageNum * $resultsPerPage; 
		
	foreach($results as $i => &$val) {
		$val['resultnum'] = $first + $i + 1;
	}
}

// alter snippets
// IMPORTANT NOTE: The following line needs to be added to
// apachesolr_search_process_response(), in apache_search.module:
//   drupal_alter('apachesolr_search_snippet', $snippet, $response, $doc);
function nomus_apachesolr_search_snippet_alter(&$snippet, $response, $doc) {
	global $language;
	$snippet = '';
	$hl_fl = 'body_' . $language->language;
	if (isset($response->highlighting->{$doc->id}->$hl_fl)) {
        $snippet = theme('apachesolr_search_snippets', $doc, $response->highlighting->{$doc->id}->$hl_fl);
   }
   else {
   	foreach(language_list() as $lang => $langinfo){
   		$hl_fl = 'body_' . $lang;
   		if(isset($response->highlighting->{$doc->id}->$hl_fl)) {
   			$snippet =
   				theme('apachesolr_search_snippets', $doc, $response->highlighting->{$doc->id}->$hl_fl);
				break;   		
   		}
   	}
   }

   // if snippet was found, clean it
 	if($snippet == '' && isset($doc->teaser)) {
   	$snippet = theme('apachesolr_search_snippets', $doc, array(truncate_utf8($doc->teaser, 256, TRUE)));
   }
}

// search page without a second search form
function apachesolr_search_search_page(&$content) {

	return theme('search_results', $content, 'apachesolr_search');
}


?>