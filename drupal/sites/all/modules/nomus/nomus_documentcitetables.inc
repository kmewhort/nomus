<?php

/**d
 * Bulk generate cite tables
 */
function nomus_bulk_citetables($nids){

	// generate all cite tables
	$nidsToNoteup = array();
	foreach($nids as $nid){
		$node = node_load($nid, null, true);
		$nidsCited = nomus_documentcitetables_create_citetable($node);
		$nidsToNoteup = array_unique(array_merge($nidsToNoteup, $nidsCited));
		node_save($node);
	}
	
	// recreate the note-up table for each node cited, as well as
	// these nodes
	$nidsToNoteup = array_unique(array_merge($nidsToNoteup, $nids));
	if(count($nidsToNoteup) > 0) {
		foreach($nidsToNoteup as $nid) {
			$node = node_load($nid, null, true);
			nomus_documentcitetables_create_noteuptable($node);
			node_save($node);
		}
	}
}

/**
 * Generates Cites and Cited by tables
 */
function nomus_documentcitetables_generate(&$node, $updateCitedNodes = true) {

	// cites table	
	$nidsCited = nomus_documentcitetables_create_citetable($node);
	
	// recreate the note-up table for each node cited
	if($updateCitedNodes){
		if(count($nidsCited) > 0) {
			foreach($nidsCited as $nid) {
				$citedNode = node_load($nid, null, true);
				nomus_documentcitetables_create_noteuptable($citedNode);
				node_save($citedNode);
			}
		}
	}
	
	// generate note-up table for this node
	nomus_documentcitetables_create_noteuptable($node);
	return $node;
}

/**
 * Generate Cites table
 * -Returns a list of the nids cited
 */
function nomus_documentcitetables_create_citetable(&$node) {
	$nidsCited = array();
	
	// generate table rows from fulltext parser XML
	$fulltextXML = null;
	$citations = array();
	if($node->field_parsed_content_tags &&
		$node->field_parsed_content_tags[0]['value'] != '') {
		$fulltextXML = new SimpleXMLElement(
			$node->field_parsed_content_tags[0]['value']);
			
		// for each cite
		if($fulltextXML->cites){
			foreach($fulltextXML->cites as $cite) {
				$nextCite = array();
				$nextCite['citation'] = (string) $cite['citation'];
				if($cite['sentence'])
					$nextCite['sentence'] = (string) $cite['sentence'];
				if($cite['paragraph'])
					$nextCite['paragraph'] = (string) $cite['paragraph'];
					
				// index by stripped citation
				$strippedCite = (string) $cite['strippedcitation'];
				if(!isset($citations[$strippedCite]))
					$citations[$strippedCite] = array();
				$citations[$strippedCite][] = $nextCite;
			}
		}
	}
	
	// for each unique citation
	$rows = array();
	foreach($citations as $strippedCite => $nextCite) {
		// lookup any actual document (or a placeholder) for this citation 
		$result = db_query("SELECT nid, tnid, type FROM {node} WHERE title = '$strippedCite'");
		if($dbrow = db_fetch_array($result)) {
			
			// if the node is a redirect, follow it
			$nid = $dbrow['nid'];
			$tnid = $dbrow['tnid'];
			if($dbrow['type'] == 'redirect') {
				$redirectNode = node_load($nid, false, true);
				$nid = $redirectNode->field_redirect_to[0]['nid'];
			}
			$citedNode = node_load($nid, false, true);
				
			// don't use self
			if($nid == $node->nid ||
				($tnid && $node->nid == $tnid) ||
				($node->tnid && $node->tnid == $nid))
				continue;
			
			// row data						
			$citeRow = array();
			
			$header = $citedNode->field_heading[0]['value'];
			if(!$header)
				$header = '<No title>';
			
			$citeRow[] = array('data' =>
				l($header, 'node/' . $nid),
				'class' => $citedNode->type);
			$citeRow[] = array('data' => $nextCite[0]['citation']);
			
			$allSentences = array();
			foreach($nextCite as $cite){
				if($cite['sentence']) {
					$allSentences[] = trim($cite['sentence']);
				}
			}
			$citeRow[] = array('data' =>
				implode('...<br />...', array_unique($allSentences)),
				'data-nid' => $nid);
			
			$rows[] = $citeRow;
			$nidsCited[] = $nid;
		}
	}
	if(count($rows) > 0) {
		$node->field_citestable_cites[0]['value'] = serialize($rows);
	} else {
		if(isset($node->field_citestable_cites[0]['value']))
			unset($node->field_citestable_cites[0]['value']);
	}
	return array_unique($nidsCited);

}

/**
 * Generate Note-up table
 */
function nomus_documentcitetables_create_noteuptable(&$node) {
	$rows = array();
		
	// for each citedby reference
	if($node->field_meta_citedby) {
	
		foreach($node->field_meta_citedby as $val)  {
		
			// don't use self
			if($val['nid'] == $node->nid)
				continue;

			// load the node				
			$referencedNode = node_load($val['nid'], false, true);
			
			// find the cites table row in the referenced node
			// corresponding to this node
			if($referencedNode->field_citestable_cites[0]['value']) {
				$citesTable = unserialize($referencedNode->field_citestable_cites[0]['value']);
				foreach($citesTable as $citesRow){
					if($citesRow[2]['data-nid'] == $node->nid){
						// change the referenced-document column into
						// a referencing-document column
						$citesRow[0]['data'] =
							l($referencedNode->field_heading[0]['value'],
								'node/' . $referencedNode->nid);
						$rows[] = $citesRow;
					}
				}
			}
		}
	}
	
	if(count($rows) > 0) {
		$node->field_citestable_citedby[0]['value'] = serialize($rows);
	} else {
		if(isset($node->field_citestable_citedby[0]['value']))
			unset($node->field_citestable_citedby[0]['value']);
	}

}

?>