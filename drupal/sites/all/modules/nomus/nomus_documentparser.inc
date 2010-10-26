<?php

/**
 * Bulk parse
 */
function nomus_bulk_parse($nids){
	// sort the nodes by document source type (don't
	// hang on to the actual node due for memory considerations)
	$nodes = array();
	foreach($nids as $nid) {
		$node = node_load($nid, false, true);
		if($node->field_documentsource_ref[0]['nid']){
			$type = $node->field_documentsource_ref[0]['nid'];
			if(!isset($nodes[$type]))
				$nodes[$type] = array();
			$nodes[$type][] = $nid;
		}
	}
	// for each node type
	$nodesProcessed = 0;
	foreach($nodes as $nodeType => &$nodeGrouping){
		// initialize the parser for linktext
		if($docParserUserId = 
			nomus_documentparser_linktext_init($nodeType)) {
	
			// for each node	
			foreach($nodeGrouping as $nid){
				$node = node_load($nid, false, true);
				
				// parse
				$node = nomus_documentparser_linktext_run($node, $docParserUserId);
				node_save($node);
			}
		}
	
		// initialize the parser for parsing the fulltext
		if($docParserUserId = nomus_documentparser_fulltext_init($nodeType)) {
		
			// for each node
			foreach($nodeGrouping as $nid){
				$node = node_load($nid, null, true);
				
				$node = nomus_documentparser_fulltext_run($node, $docParserUserId);
				node_save($node);
			}
		}
	}
}


/**
 * Parse - run linktext and the document through
 * the configured GATE documents
 */
function nomus_documentparser_parse($docNode, $documentSourceNid) {
	global $user;
	
	if($docParserUserId = nomus_documentparser_linktext_init($documentSourceNid)) {
		$docNode = nomus_documentparser_linktext_run($docNode, $docParserUserId);
	}
	if($docParserUserId = nomus_documentparser_fulltext_init($documentSourceNid)) {
		$docNode = nomus_documentparser_fulltext_run($docNode, $docParserUserId);
	}
	return $docNode;
}
	
/**
 * Initialize the parser for parsing linktext
 * Returns true if there are linktext parsers to run.
 */
function nomus_documentparser_linktext_init($documentSourceNid, $useCachedDocParser = true) {
	global $user;
	// document parser server
	$server = variable_get('nomus_documentparser_server', '');
	if($server == '') {
		return;
	}
	
	// generate a unique DocParser "user" from the userid and doctype
	// (it's better to cache a new DocParser user for each
	// unique DocParser, otherwise GATE leaks
	// the JAPE Parsers into the JVM each time the parser is
	// initialized)
	$docParserUserId = $user->uid . '-' .
		$documentSourceNid . '-linktext';
	
	// check whether the drupal cache indicates that the parser
	// has been initiatiled (to ensure reinitialization occurs
	// when the drupal cache is cleared)
	$drupalCacheExists = false;
	if (($cache = cache_get("nomus_dp_$docParserUserId")) && $cache->data) {
      $drupalCacheExists = true;
   }

	// get the document source node
	$docSourceNode = node_load($documentSourceNid);

	// initialize
	if($docSourceNode->field_docparser_linktext_gate) {
	
		// query whether the parser exists
		$parserCacheExists = false;
		$url = $server . '?req=parserExists';
		$url .= '&user=' . $docParserUserId;
		$request = new HttpRequest($url);
		$request->setMethod(HTTP_METH_GET);
		$result = $request->send();
		
		// if this parser exists
		if($result->getBody() == 'true') {
			$parserCacheExists = true;
		}
		
		// if both the drupal and docparser caches exist, and using the cache,
		// nothing furthe to do  
		if($useCachedDocParser && $drupalCacheExists && $parserCacheExists){
				return $docParserUserId;
		}
		
		// if the parser cache exists, need to reset it
		if($parserCacheExists){
			// othewise, need to reset this DocParser
			$url = $server . '?req=clearApplications';
			$url .= '&user=' . $docParserUserId;
			$request = new HttpRequest($url);
			$request->setMethod(HTTP_METH_GET);
			$result = $request->send();
		}
	
		// add each GATE document to the document parser
		foreach($docSourceNode->field_docparser_linktext_gate as $gateFileField) {
			$gateFileNid = $gateFileField['nid'];
			if($gateFileNid) {
				$gateFileNode = node_load($gateFileNid);
				$gateFileType = $gateFileNode->type;				
				
	   		// send a request to the server to add the document parser
				$url = $server . '?req=addApplication';
				$url .= '&user=' . $docParserUserId;
				$url .= '&fileUrl=' .
					urlencode(nomus_documentparser_gatedoc_node_url($gateFileNid));
				$url .= '&fileType=' . $gateFileType;
				$url .= '&debug=true';
				
				$request = new HttpRequest($url);
				$request->setMethod(HTTP_METH_GET);
				$result = $request->send();
			}
		}
		
		cache_set("nomus_dp_$docParserUserId", '1');
		return $docParserUserId;
	}
	return false;
}

/**
 * Initialize the parser for parsing fulltext
 * Returns true if there are fulltext parsers to run.
 */
function nomus_documentparser_fulltext_init($documentSourceNid, $useCachedDocParser = true) {
	global $user;
	
	// document parser server
	$server = variable_get('nomus_documentparser_server', '');
	if($server == '') {
		return;
	}
	
	// generate a unique DocParser "user" from the userid and doctype
	$docParserUserId = $user->uid . '-' .
		$documentSourceNid . '-fulltext';
	
	// check whether the drupal cache indicates that the parser
	// has been initiatiled (to ensure reinitialization occurs
	// when the drupal cache is cleared)
	if (($cache = cache_get("nomus_dp_$docParserUserId")) && $cache->data) {
      $drupalCacheExists = true;
   }
		
	// get the document source node
	$docSourceNode = node_load($documentSourceNid);

	// initialize
	if($docSourceNode->field_docparser_fulltext_gate) {
	
		// query whether the parser exists
		$parserCacheExists = false;
		$url = $server . '?req=parserExists';
		$url .= '&user=' . $docParserUserId;
		$request = new HttpRequest($url);
		$request->setMethod(HTTP_METH_GET);
		$result = $request->send();
		
		// if this parser exists
		if($result->getBody() == 'true') {
			$parserCacheExists = true;
		}
		
		// if both the drupal and docparser caches exist, and using the cache,
		// nothing furthe to do  
		if($useCachedDocParser && $drupalCacheExists && $parserCacheExists){
				return $docParserUserId;
		}
		
		// if the parser cache exists, need to reset it
		if($parserCacheExists){
			// othewise, need to reset this DocParser
			$url = $server . '?req=clearApplications';
			$url .= '&user=' . $docParserUserId;
			$request = new HttpRequest($url);
			$request->setMethod(HTTP_METH_GET);
			$result = $request->send();
		}
		
		// add each GATE document to the document parser
		foreach($docSourceNode->field_docparser_fulltext_gate as $gateFileField) {
			$gateFileNid = $gateFileField['nid'];
			if($gateFileNid) {
				$gateFileNode = node_load($gateFileNid);
				$gateFileType = $gateFileNode->type;				
				
	   		// send a request to the server to add the document parser
				$url = $server . '?req=addApplication';
				$url .= '&user=' . $docParserUserId;
				$url .= '&parserid=' . $docSourceNode->nid;
				$url .= '&fileUrl=' .
					urlencode(nomus_documentparser_gatedoc_node_url($gateFileNid));
				$url .= '&fileType=' . $gateFileType;
				$url .= '&debug=true';
				$request = new HttpRequest($url);
				$request->setMethod(HTTP_METH_GET);
				$result = $request->send();
			}
		}
		
		// add each input dictionary to the document parser
		$inputDictionaries = array();
		if(!empty($docSourceNode->field_docparser_dict_input)){
			foreach($docSourceNode->field_docparser_dict_input as $dictInputField) {
				if(!empty($dictInputField['value']))
					$inputDictionaries[] = $dictInputField['value'];
			}
		}
		if(!empty($inputDictionaries)){			
	   	// send a request to the server to add the dictionary
			$url = $server . '?req=addApplication';
			$url .= '&user=' . $docParserUserId;
			$url .= '&fileUrl=' .
				urlencode(nomus_documentparser_gatedoc_vocabularies_url($inputDictionaries));
			$url .= '&fileType=' . 'gate_gaz';
			$url .= '&debug=true';
			
			$request = new HttpRequest($url);
			$request->setMethod(HTTP_METH_GET);
			$result = $request->send();
		}
		
		cache_set("nomus_dp_$docParserUserId", '1'); 		
		return $docParserUserId;
	}
	return false;
}

		
/**
 * Execute the linktext parsers
 */
function nomus_documentparser_linktext_run($docNode, $docParserUserId) {
	global $user;
				
	// document parser server
	$server = variable_get('nomus_documentparser_server', '');
	if($server == '') {
		return;
	}
	
	// convert linktext to utf-8
	$linktext = utf8_decode($docNode->field_raw_link_text[0]['value']);
	
	// parse the linktext
	$url = $server . '?req=run';
	$url .= '&user=' . $docParserUserId;
	$request = new HttpRequest($url);
	$request->setMethod(HTTP_METH_POST);
	$opts = array("Content-Type"=>"text/html; charset=UTF-8", "Accept"=>"text/plain");
	$request->addHeaders($opts);
	$request->setBody($linktext);
	$result = $request->send();
	$linkTextOutput = $result->getBody();

	// save the linktext xml data
	$xmlTagsStart = strpos($linkTextOutput, "<GateXMLTags>");
	$xmlTagsEnd = strpos($linkTextOutput, "</GateXMLTags>");
	if(!($xmlTagsStart === false) && !($xmlTagsEnd === false)) {
		$docNode->field_parsed_linktext_tags[0]['value'] =
		   '<xml>' .
			substr($linkTextOutput, $xmlTagsStart+13,
			($xmlTagsEnd-$xmlTagsStart)-13) .
			'</xml>';
	}
	return $docNode;
}

/**
 * Execute the linktext parsers
 */
function nomus_documentparser_fulltext_run($docNode, $docParserUserId) {
	global $user;
	
	// document parser server
	$server = variable_get('nomus_documentparser_server', '');
	if($server == '') {
		return;
	}
		
	// get the content
	$contentOutput = '';
	if($docNode->field_converted_file) {
		$htmlFilename = 
			$docNode->field_converted_file[0]['filepath'];
			
		// convert text from utf-8
		$text = utf8_decode(file_get_contents($htmlFilename));
				
		// send the request
		$url = $server . '?req=run';
		$url .= '&user=' . $docParserUserId;
		$url .= '&parserid=' . $docNode->field_documentsource_ref[0]['nid'];
		$request = new HttpRequest($url);
		$request->setMethod(HTTP_METH_POST);
		$opts = array("Content-Type"=>"text/html", "Accept"=>"text/plain");
		$request->addHeaders($opts);
		$request->setBody($text);
		$result = $request->send();
		$contentOutput = $result->getBody();			
	}
		
	// if there's already a parsed HTML file, delete it
	if($docNode->field_parsed_html[0]) {
		$docNode->field_parsed_html = array();
		node_save($docNode);
	}
			
	// save the content xml data
	$xmlTagsStart = strpos($contentOutput, "<GateXMLTags>");
	$xmlTagsEnd = strpos($contentOutput, "</GateXMLTags>");
	if(!($xmlTagsStart === false) && !($xmlTagsEnd === false)) {

		$docNode->field_parsed_content_tags[0]['value'] =
		   '<xml>' .
			substr($contentOutput, $xmlTagsStart+13,
			($xmlTagsEnd-$xmlTagsStart)-13) .
			'</xml>';
	}
		
	// save the content html data
	$xmlTagsStart = strpos($contentOutput, "<GateHTML>");
	$xmlTagsEnd = strrpos($contentOutput, "</GateHTML>");
	if(!($xmlTagsStart === false) && !($xmlTagsEnd === false)) {

		$output = 
			substr($contentOutput, $xmlTagsStart+10,
			($xmlTagsEnd-$xmlTagsStart)-10);

		// keep the same filename, but with '_parsed'
		$pathInfo = pathinfo($docNode->field_raw_file[0]['filepath']);
		$tempFilename = $pathInfo['filename'] . '_parsed.html';
		$targetDir = $pathInfo['dirname'];		
		$tempFile = file_directory_temp() . '/' . $tempFilename;
		$fh = fopen($tempFile, "w");
		fwrite($fh, $output);
		fclose($fh);
			
		// save to the file field
		$docNode->field_parsed_html[0] = field_file_save_file(
  			$tempFile, array(), $targetDir);
	}
	return $docNode;

}

/**
 * Strips authentication tokens from urls, and translates
 * plugin directory urls to the local server
 *
 */
function nomus_documentparser_translate_gate_urls(&$node) {
	// strip tokens
	$node->body = preg_replace('/(<urlString>[^<]+token\=)[^<]*(<)/s', '$1$2', $node->body);
	
	// fix plugin directories
	$node->body = preg_replace('/(<urlString>)[^<]+\/plugins\/([^<]*<)/s', '$1$gatehome$plugins/$2', $node->body);
}

/**
 * Generates an external URL to retrieve a raw GATE document,
 * along with a user authentication token
 *
 */
function nomus_documentparser_gatedoc_node_url($gateDocNodeId) {
	return url('nomus/documentparser/gatedoc', array('absolute' => TRUE))
	   			. '?nodeId=' . $gateDocNodeId 
	   			. '&token=' . tokenauth_get_token();
}

/**
 * Generates an external URL to retrieve one or more vocabularies as a 
 * GATE gazetteer
 *
 */
function nomus_documentparser_gatedoc_vocabularies_url($dictionaryIds) {
	return url('nomus/documentparser/gatedoc', array('absolute' => TRUE))
	   			. '?dictIds=' . implode('-', $dictionaryIds)
	   			. '&token=' . tokenauth_get_token();
}


/**
 * Menu callback to retrieve GATE documents
 */
function nomus_documentparser_gatedoc() {
	$dictIds = $_GET['dictIds'];
	$nodeId = $_GET['nodeId'];
	
	// return dictionary ids as gazetteers
	if(!empty($dictIds)){
		$output = '';
	   // if this is a def file request, generate the definition
	   // list of the multiple gazetteer lists (one for each dictionary
	   if(!isset($_GET['dict'])) {
	   	$output = '';
	   	foreach(explode('-', $dictIds) as $dictId) {
	   	  	$filename = "gatedoc?dictIds=$dictIds&dict=$dictId&token=" .
	   	  		tokenauth_get_token();
	   	  	$vocab = db_fetch_array(db_query('SELECT name FROM {vocabulary} WHERE vid = %d', $dictId));
	   	  	$types = substr($val['value'], 0,
	   	  		strpos($val['value'], "\n"));
	   		$output .= $filename . ":" . 'taxonomy' . ":" . $vocab['name'] . "\n";
	   	} 
	   }
	   // else, output the particular gazetteer list requested
	   else {
	   	$vocabId = $_GET['dict'];
	   	$result = db_query('SELECT tid, name FROM {term_data} WHERE vid = %d', $vocabId);
	   	while($row = db_fetch_array($result)){
	   		$output .= $row['name'] .
	   			'|term=' . $row['name'] .
	   			'|tid=' . $row['tid'] .
	   			'|vid=' . $vocabId .
	   			"\n";
	   	}
	   }
	   print $output;
	}
	
	// return GATE files for GATE nodes
	else if(!empty($nodeId)){
		$gateDoc = node_load($nodeId);
	
		// only retrieve GATE document types
		if($gateDoc && preg_match('/^gate_/', $gateDoc->type)) {
			$output = $gateDoc->body;
		
	   	// special case for gazetteers
	   	if($gateDoc->type == 'gate_gaz') {
	   		// if this is a def file request, generate the definition
	   		// list of the gazetteer lists
	   		if(!isset($_GET['lst'])) {
	   			$output = '';
	   	   	foreach($gateDoc->field_gate_gazetteer as $key => $val) {
	   	   		$filename = "gatedoc?nodeId=$nodeId&lst=$key&token=" .
	   	   			tokenauth_get_token();
	   	   		$types = substr($val['value'], 0,
	   	   			strpos($val['value'], "\n"));
	   				$output .= $filename . ":" . $types . "\n";
	   			} 
	   		}
	   		// else, output the particular gazetteer list requested
	   		else {
	   			$lstId = $_GET['lst'];
	   			$gazField = $gateDoc->field_gate_gazetteer[$lstId]['value'];
	   			$output = substr($gazField, strpos($gazField, "\n")+1);
	   		}
	   	}
	
			// special case for applications
			if($gateDoc->type == 'gate_gapp') {
				// replace the token document root with the document root of this server, and
				// token for this user
		   	$output = preg_replace('/<urlString>http.*?nomus\/documentparser\/gatedoc\?(.*?token\=).*?<\/urlString>/s',
		   		'<urlString>' . url('nomus/documentparser/gatedoc', array('absolute' => TRUE)) . 
		   		'?${1}' . tokenauth_get_token() . '</urlString>', $output);
		   		/* $output = preg_replace('/<urlString>http.*?documentparser\/gatedoc(.*?token\=).*?<\/urlString>/s',
		   		'a', $output);*/

			}
			print $output;	
		}
	}
}
?>