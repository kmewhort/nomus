<?php

/**
 * Bulk render
 */
function nomus_bulk_render($nids){
	foreach($nids as $nid){
		$node = node_load($nid, false, true);
		$node = nomus_documentrenderer_render($node);
		node_save($node);
	}
}

/**
 * Bulk generate new itation
 */
function nomus_bulk_generate_citation($nids){
	foreach($nids as $nid){
		$node = node_load($nid, false, true);
		
		if(!empty($node->field_meta_date[0]['value'])){
	   	$node->field_meta_citation[0]['value'] =
				nomus_documentrenderer_generate_citation(
					$node->type, $node->field_raw_group[0]['value'],
					$node->field_meta_date[0]['value']);	
			$node->field_meta_index[0]['value'] = preg_replace('/\s/', '', $node->field_meta_citation[0]['value']);
			$node->title = $node->field_meta_index[0]['value'];
			
			node_save($node);
		}
	}
}

/**
 * Render - collect and format output
 */
function nomus_documentrenderer_render($node) {
	global $user;
//	_pathauto_include();
	
	// document source node
	$docsourceNode = node_load((int) $node->field_documentsource_ref[0]['nid']);
	
	// linktext parser XML
	$linktextXML = null;
	if($node->field_parsed_linktext_tags &&
		$node->field_parsed_linktext_tags[0]['value'] != '') {
		$linktextXML = new SimpleXMLElement(
			$node->field_parsed_linktext_tags[0]['value']);
	}
	
	// fulltext parser XML
	$fulltextXML = null;
	if($node->field_parsed_content_tags &&
		$node->field_parsed_content_tags[0]['value'] != '') {
		$fulltextXML = new SimpleXMLElement(
			$node->field_parsed_content_tags[0]['value']);
	}
	
	// citations within the document (need to cross-reference these
	// to other documents)
	$cites = array();
	
	// for each meta field
	foreach($node as $field => $val) {
		if(substr($field, 0, 11) == 'field_meta_' || $field == 'field_index') {
						
			$key = substr($field, 11);
			
			// whether field is multi-valued
			$result = db_query("SELECT multiple FROM {content_node_field} WHERE field_name = '$field'");
			$row = db_fetch_array($result);
			$isMulti = $row['multiple'];
			
			$fieldValue = array();
			
			// if field is in document source static values
			if(isset($docsourceNode->field_docparser_static_data)) {
			
				// for each static value
				foreach($docsourceNode->field_docparser_static_data as $fieldData) {
					$staticField = explode(',', $fieldData['value'], 2);
					if($key == $staticField[0]) {

						if($key == 'cites')
							$cites[] = $staticField[1];
						else
							$fieldValue[] = array(
								'value' => (string) $staticField[1]);	
						if(!$isMulti)
							break;				
					}
				}
			}
				
			// if field is in linktext tags
			if(($isMulti || count($fieldValue) == 0) &&
				isset($linktextXML->$key)) {
				
				// for each value in the XML
				foreach($linktextXML->$key as $val){
					// if the field is a cite, just add it to the array
					// to be cross-referenced
					if($key == 'cites')
						$cites[] = $val;
					else
					{
						if(isset($val['value']))
							$fieldValue[] = array(
								'value' => (string) $val['value']);
						else
							$fieldValue[] = array(
								'value' => (string) $val);
					}
						
					// if field is not multivalued, stop
					if(!$isMulti)
						break;
				}
			}
			
			// if field is in fulltext tags
			if(($isMulti || count($fieldValue) == 0) &&
			   isset($fulltextXML->$key)) {
			   
				// for each value in the XML
				foreach($fulltextXML->$key as $val){
					// if the field is a cite, just add it to the array
					// to be cross-referenced
					if($key == 'cites')
						$cites[] = $val;
					else
					{
						if(isset($val['value']))
							$fieldValue[] = array(
								'value' => (string) $val['value']);
						else
							$fieldValue[] = array(
								'value' => (string) $val);
					}
						
					// if field is not multivalued, stop
					if(!$isMulti)
						break;
				}
			}
			
			if(!empty($fieldValue))
				$node->$field = $fieldValue;
		}
	}
	
	// generate citation if necessary
	if(empty($node->field_meta_citation[0]['value']) &&
	   $docsourceNode->field_generate_citations[0]['value'] &&
	   !empty($node->field_meta_date[0]['value']) &&
	   !empty($node->field_meta_casename[0]['value'])){
	   	$node->field_meta_citation[0]['value']
	 			 = nomus_documentrenderer_generate_citation(
					$node->type, $node->field_raw_group[0]['value'],
					$node->field_meta_date[0]['value']);
			$node->field_meta_index[0]['value'] = preg_replace('/\s/', '', $node->field_meta_citation[0]['value']);
			$node->title = $node->field_meta_index[0]['value']; 	
	}
	
	// set the title
	if($node->field_meta_index[0]['value'] &&
		$node->field_meta_index[0]['value'] != '') {
		$node->title = $node->field_meta_index[0]['value'];
	}
	
	// set the language to the language specified in the metadata
	if($node->field_meta_language[0]['value'] &&
		$node->field_meta_language[0]['value'] != '') {
		$node->language = $node->field_meta_language[0]['value'];
	}
	
	// delete existing terms for this node
	db_query('DELETE FROM {term_node} WHERE nid = %d', $node->nid);
	$node->vid = $node->nid;
	
	// set the taxonomy terms
	$taxterms = array();
	foreach($fulltextXML->Lookup as $val){
		$vid = (string) $val['vid'];
		$tid = (string) $val['tid'];
		if(empty($taxterms[$vid]) || !in_array($tid, $taxterms[$vid])){
			$taxterms[$vid][] = $tid;
		}
	}
	
	// for each vocabulary
	foreach($taxterms as $vid => $tids){
		// for each term
		foreach($tids as $tid){
			db_query('INSERT INTO {term_node} (nid, vid, tid) VALUES (%d, %d, %d)',
				$node->nid, $node->vid, $tid);
		}
	}
	$node->taxonomy = taxonomy_node_get_terms($node, 'tid', true);
	
	// check whether this node already exists in a different
	// language
	if($node->language && $node->field_meta_altindex)
	{
		$primaryNodeId = null;
		foreach($node->field_meta_altindex as $otherLangIndex) {
			$altIndex = $otherLangIndex['value'];
			$type = $node->type;
			if($altIndex != '') {
				$result = db_query("SELECT nid, tnid, language FROM {node} WHERE title = '$altIndex' AND type = '$type'");
				while($row = db_fetch_array($result)) {
					if($row['language'] != $node->language &&
					   ($row['tnid'] == 0 ||
					    ($row['nid'] == $row['tnid']))){
						$primaryNodeId = array(
							'nid' => $row['nid'],
							'tnid' => $row['tnid']);
						break;
					}
				}
				if($primaryNodeId)
					break;
			}
		}
		
		// if there is a primary node in a different language, tie
		// this node to the same language set
		if($primaryNodeId){
			if($primaryNodeId['tnid'] != 0){
				$node->tnid = $primaryNodeId['tnid'];
			}
			else {
				$node->tnid = $primaryNodeId['nid'];
				
				// need to turn the primary node into a language set node
				db_query("UPDATE {node} SET tnid = nid WHERE nid = '"
					. $primaryNodeId['nid'] . "'");
			}
		}
	}
	
	// cross reference the cites
	$node = nomus_documentrenderer_crossreference($node, $cites);
	
	// set the input format to Raw HTML
	$filterFormats = filter_formats();
	foreach($filterFormats as $format) {
		if($format->name == 'Raw HTML') {
			$node->format = $format->format;
			break;
		}
	}
	
	
	// if a document with this index exists
	$thisNid = $node->nid;
	$index = $node->title;
	$result = db_query("SELECT nid, type, tnid FROM {node} WHERE title = '$index' AND nid != '$thisNid'");
	while($row = db_fetch_array($result)){
		
		// if the document is a redirect, follow it
		$redirectedFrom = null;
		if($row['type'] == 'redirect') {
			$redirectedFrom = $row['nid'];
			$redirectNode = node_load($row['nid'], false, true);
			
			// if the redirect has been orphaned, delete it
			if(!$redirectNode->field_redirect_to[0]['nid']) {
				node_delete($row['nid']);
				continue;
			}
			$sourceNid = $redirectNode->field_redirect_to[0]['nid'];
			
			$result = db_query("SELECT nid, type, tnid FROM {node} WHERE nid = '$sourceNid'");
			$row = db_fetch_array($result);
		}
		
		// if the document is an alternative language, continue
		if($row['tnid'] != 0 && $row['tnid'] == $node->tnid)
			continue;
		
		// if the document is a placeholder
		if($row['type'] == 'placeholder') {
			// save the current node
			node_save($node);
			
			// merge the placeholder into the current node
			nomus_documentrenderer_transfer_placeholder($row['nid'],
				$node->nid);
				
			// reload the node
			$node = node_load($node->nid, false, true);
		}
		
		// else, if document is an actual node,
		// flag a duplicate error on the node
		else {
			$node->field_error[0]['value'] .= 'Error: Document with this index already exists (nid ' . $row['nid'];
			if($redirectedFrom != null)
				$node->field_error[0]['value'] .= ', redirected from ' . $redirectedFrom;
				$node->field_error[0]['value'] .= ').'; 
		}
	}
	
	// update the path
	//$placeholders = pathauto_get_placeholders('node', $node);
   //$src = "node/$node->nid";
   //$alias = pathauto_create_alias('node', 'update', $placeholders, $src, $node->nid, $node->type, $node->language);
   if($node->field_raw_group[0]['value'] && $node->field_raw_group[0]['value'] != '' &&
      $node->field_meta_index[0]['value'] && $node->field_meta_index[0]['value'] != '') {
   	$node->path = $node->type . '/' . $node->field_raw_group[0]['value'] . '/' . $node->field_meta_index[0]['value'];
   }
	
	// call any doctype render hooks
	$hookname = $node->type . '_render';
	if(function_exists($hookname)) {
		$hookname($node);
	}
	
	// if there's no heading or  no citation, set as unpublished
	if(empty($node->field_heading[0]['value']) ||
		empty($node->field_meta_index[0]['value'])) {
		$node->status = 0;
	}
	else {
		$node->status = 1;
	}
	
	return $node;
}

function nomus_documentrenderer_crossreference($node, $citeXMLElements ) {
		
	$primaryNode = $node;
	
	// only add cross references to the primary node in any
	// set of translations
	if($node->tnid != 0 && $node->nid != $node->tnid) {
		$primaryNode = node_load($node->tnid, false, true);
	}

	// for each cite
	foreach($citeXMLElements as $cite){
		
		// create redirect and placeholder nodes tying together all parallel
		// citations
		$citeNid = nomus_documentrenderer_create_redirects($cite);
		if($citeNid == -1)
			continue;
			
		// set the cite, if if it doesn't already exist
		// to the nid
		$nidReferenced = false;
		if($primaryNode->field_meta_cites) {
			foreach($primaryNode->field_meta_cites as $citeRef) {
				if($citeRef['nid'] == $citeNid) {
					$nidReferenced = true;
					break;
				}
			}
		}
		if(!$nidReferenced)
			$primaryNode->field_meta_cites[]['nid'] = $citeNid;
	}
	
	if($node->tnid != 0 && $node->nid != $node->tnid) {
		node_save($primaryNode);
	}
	
	return $node;
}

		
/**
 * Creates redirects based on parallel citations.
 * Returns the nid of the actual node (or placeholder) referred to
 */
function nomus_documentrenderer_create_redirects($citeXML) {

	// determine whether each parallel stripped citation refers to
	// 1. an existing node, 2. a redirect node, 3. a placeholder
	// node, or 4. a non-existent node
	$existingNodes = array();
	$redirectNodes = array();
	$placeholderNodes = array();
	$nonexistentNodes = array();
		
	$strippedCites = array((string) $citeXML['strippedcitation']);
	$strippedCites = array_merge($strippedCites, 
		explode(";", $citeXML['strippedparcitation']));
	$fullCites = array((string) $citeXML['citation']);
	$fullCites = array_merge($fullCites, 
		explode(";", $citeXML['parallelcitations']));

	foreach($strippedCites as $index => $cite) {
		$cite = trim($cite);
		$cite = trim($cite, ";, ");
		$fullCite = trim($fullCites[$index]);
		$fullCite = trim($fullCites[$index], ";, ");
		$title = null;
		if($citeXML['title'] && $citeXML['title'] != '')
			$title = $citeXML['title'];
			
		if($cite == '')
			continue;
	
	   // query for the index, only pulling out the source nid of any translations
		$result = db_query("SELECT nid, tnid, type FROM {node} WHERE title = '$cite'");
		$row = db_fetch_array($result);
		if(!$row){
			$nonexistentNodes[] = array(
				'cite' => $cite,
				'fullCite'=> $fullCite,
				'title' => $title);
		}
		else {
			$nid = $row['nid'];
			if($row['tnid'] != 0)
				$nid = $row['tnid'];
				
			if($row['type'] == 'placeholder')
				$placeholderNodes[] = array(
					'cite' => $cite,
					'nid' => $nid,
					'fullCite'=> $fullCite,
					'title' => $title);
			else if($row['type'] == 'redirect')
				$redirectNodes[$cite] = array(
					'cite' => $cite,
					'nid' => $nid,
					'fullCite'=> $fullCite,
					'title' => $title);
			else
				$existingNodes[$cite] = array(
					'cite' => $cite,
					'nid' => $nid,
					'fullCite'=> $fullCite,
					'title' => $title);
		}
	}

	/*dpm(array($citeXML->asXML()));
	dpm(array('Existing' => $existingNodes,
		'Placeholder' => $placeholderNodes,
		'Redirects' => $redirectNodes,
		'Nonexistent' => $nonexistentNodes));*/
			
	// follow each redirect node to it's source node
	if($redirectNodes) {
		foreach($redirectNodes as $redirect) {
			$redirectNode = node_load($redirect['nid'], false, true);
			
			// if the redirect has been orphaned, delete it
			if(!$redirectNode->field_redirect_to[0]['nid']) {
				node_delete($redirect['nid']);
				$nonexistentNodes[] = array(
					'cite' => $redirect['cite'],
					'fullCite'=> $redirect['fullCite'],
					'title' => $redirect['title']);
				continue;
			}
			$sourceNid = $redirectNode->field_redirect_to[0]['nid'];
			
			// type of the source node
			$result = db_query("SELECT type FROM {node} WHERE nid = '$sourceNid'");
			$row = db_fetch_array($result);
			$type = $row['type'];
		
			if($type == 'placeholder')
				$placeholderNodes[] = array(
					'cite' => $redirect['cite'],
					'nid' => $sourceNid,
					'fullCite'=> $redirect['fullCite'],
					'title' => $redirect['title']);
			else
				$existingNodes[] = array(
					'cite' => $redirect['cite'],
					'nid' => $sourceNid,
					'fullCite'=> $redirect['fullCite'],
					'title' => $redirect['title']);
		}
	}
	
	// if there's more than one existing node referred to,
	// a parallel reference is erroneous -> bail
	$nids = array();
	if($existingNodes) {
		foreach($existingNodes as $n) {
			$nids[] = $n['nid'];
		}
	}
	$nids = array_unique($nids);
	$numExisting = count($nids);
	if($numExisting > 1) {
		$errMessage = "Conflicting parallel references (nodes " . implode(", ", $nids) . ")."; 
		foreach($nids as $nid) {
			$dupNode = node_load($nid, false, true);
			$dupNode->field_error[0]['value'] .= $errMessage;
			node_save($dupNode);
		}
		return -1;
	}
			
	// if there's exactly one existing node, turn the
	// placeholder nodes into redirects (this will also
	// change the redirects pointing to the placeholder), and
	// turn non-existent nodes into redirects
	if($numExisting == 1){
		if($placeholderNodes) {
			foreach($placeholderNodes as $placeholder) {
				//dpm("Turning placeholders into a redirect to the one existing node");
				nomus_documentrenderer_transfer_placeholder(
					$placeholder['nid'], $nids[0]);
			}
		}
		
		if($nonexistentNodes) {		
			foreach($nonexistentNodes as $nonexist) {
				nomus_documentrenderer_create_redirect_node(
					$nonexist['cite'], $nids[0], $nonexist['title'] . ' ' . $nonexist['fullCite']);
			}
		}
				
		return $nids[0];
	}
	
	// if there's no existing nodes, but at least one
	// placeholder node, merge placeholders by turning all but one
	// into redirects and creating redirects for non-existents
	$nids = array();
	if($placeholderNodes){
		foreach($placeholderNodes as $n) {
			$nids[] = $n['nid'];
		}
	}
	$nids = array_unique($nids);
	$numExisting = count($nids);
	if($numExisting > 0) {
		// merge placeholders
		for($i = 1; $i < $numExisting; $i++) {
			//dpm("Multiple placeholders - merging all into first.");
			nomus_documentrenderer_transfer_placeholder(
				$nids[$i], $nids[0]);
		}
		
		if($nonexistentNodes) {
			foreach($nonexistentNodes as $nonexist) {
				nomus_documentrenderer_create_redirect_node(
					$nonexist['cite'], $nids[0], $nonexist['title'] . ' ' . $nonexist['fullCite']);
			}
		}
		return $nids[0];
	}
	
	// else, if there's no existing and no placeholder nodes,
	// create a placehloder and create redirects for non-existents
	if(count($nonexistentNodes) > 0) {
	
		// create a placeholder
		$nid = nomus_documentrenderer_create_placeholder_node(
			$nonexistentNodes[0]['cite'], $nonexistentNodes[0]['title'] . ' ' . $nonexistentNodes[0]['fullCite']);
	
		// create redirects for other non-existent nodes
		for($i = 1; $i < count($nonexistentNodes); $i++) {
			nomus_documentrenderer_create_redirect_node(
				$nonexistentNodes[$i]['cite'], $nid, $nonexistentNodes[$i]['title'] . ' ' . $nonexistentNode[$i]['fullCite']);
		}
				
		return $nid;
	}
	
	return -1;
}

/**
 * Transfers a placeholder node to an actual node (or another
 * placeholder)
 */
function nomus_documentrenderer_transfer_placeholder(
	$placeholderNid, $targetNid) {

	// get the nodes
	$placeholderNode = node_load($placeholderNid, false, true);
	$targetNode = node_load($targetNid, false, true);
	
	// title/citation
	$title = $placeholderNode->title;
	
	// copy the citedas fields to the target
	if($placeholderNode->field_meta_citedas) {
		foreach($placeholderNode->field_meta_citedas as $val) {
			$refExists = false;
			if($targetNode->field_meta_citedas) {
	   		foreach($targetNode->field_meta_citedas as $existingVal) {
					if($existingVal['nid'] == $val['nid']) {
						$refExists = true;
						break;
					}
				}	   
	   	}
	   		   
	   	if(!$refExists){
				$targetNode->field_meta_citedas[] = $val;
			}
		}
	}
	
	// copy the citedby fields to the target
	if($placeholderNode->field_meta_citedby) {
		foreach($placeholderNode->field_meta_citedby as $val)  {
			$refExists = false;
			if($targetNode->field_meta_citedby) {
	   		foreach($targetNode->field_meta_citedby as $existingVal) {
					if($existingVal['nid'] == $val['nid']) {
						$refExists = true;
						break;
					}
				}	   
	   	}
	   
	   	if(!$refExists)
				$targetNode->field_meta_citedby[] = $val;
		}	
	}
	
	// delete the placeholder
	node_delete($placeholderNid);
	
	// save the target
	node_save($targetNode);
	
	// create a redirect to take the place of the placeholder
	if($placeholderNode->title != $targetNode->title){
		nomus_documentrenderer_create_redirect_node(
			$title, $targetNode->nid, $targetNode->field_heading[0]['value']);
	}
					
}

/**
 * Creates a placeholder node.
 * Returns the nid.
 */
function nomus_documentrenderer_create_placeholder_node(
	$citation, $heading) {
	global $user;

	$node = (object) NULL;
	$node->type = 'placeholder';
	$node->title = $citation;
	$node->field_heading[0]['value'] = $heading;
	
	$node = node_submit($node);
	$node->uid = $user->uid;
	node_save($node);

	return $node->nid;
}

/**
 * Creates a redirect node
 */
function nomus_documentrenderer_create_redirect_node(
	$citation, $targetNid, $heading) {
	global $user;
	
	$node = (object) NULL;
	$node->type = 'redirect';
	$node->title = $citation;
	$node->field_redirect_to[0]['nid'] = $targetNid;
	
	$node = node_submit($node);
	$node->uid = $user->uid;
	if($heading)
		$node->field_heading[0]['value'] = $heading;
	node_save($node);
	
	
	/*$testNode = node_load($targetNid, false, true);
	dpm($node->title . " redirecting to " . $testNode->title);*/
	
	return $node->nid;
}

/**
 * Generates a new citation
 */
function nomus_documentrenderer_generate_citation($contenttype, $group, $date){
	$year = substr($date, 0, 4);
	
	// get all citations for this group/year
	$result = db_query("SELECT field_meta_index_value FROM content_type_$contenttype"
		. " WHERE field_meta_index_value LIKE '${year}Nomus${group}%'");

	// find the maximum existing index number
	$maxExisting = 0;
	while($row = db_fetch_array($result)){
		if(preg_match("/${year}Nomus${group}(\d+)/", $row['field_meta_index_value'], $match)){
			if($maxExisting < $match[1])
				$maxExisting = $match[1];
		}
	}
	
	return $year . ' Nomus' . $group . ' ' . ($maxExisting+1); 
}

?>