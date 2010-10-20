<?php

/**
 * Bulk convert
 */
function nomus_bulk_convert($nodes){
	// no timeout
	set_time_limit(0);	
	
	foreach($nodes as $nid){
		$node = node_load($nid, null, true);
		$node->field_error[0]['value'] = '';
		$node = nomus_documentconverter_convert($node);
		node_save($node);
	}
}

/**
 * Converts any field_raw_file field in a node into HTML, which is
 * placed in the body
 */
function nomus_documentconverter_convert($node,
	$rawFileFieldName = 'field_raw_file'){
	ini_set('pcre.backtrack_limit', 1000000);
	global $user;
	
	$rawFileField = $node->$rawFileFieldName;

	// if there's no field_raw_file field, nothing to do
	if(!$rawFileField || !count($rawFileField)) {
		return $node;
		}
		
	$server = variable_get('nomus_documentconverter_server', '');
	if($server == '') {
		return $node;
	}
	
		
   // construct request to convert the file to HTML
	$ch = curl_init();
	curl_setopt($ch, CURLOPT_URL, $server);
	curl_setopt($ch, CURLOPT_RETURNTRANSFER, 1);
	curl_setopt($ch, CURLOPT_POST, 1);
	curl_setopt($ch, CURLOPT_POSTFIELDS,
		array('filename'=> '@' . $rawFileField[0]['filepath'],
		      'to' => 'html'));
	$html = curl_exec($ch);
	curl_close($ch);

	if($html)
	{
		// apply any regexp replacements
		$docsourceNode = node_load((int) $node->field_documentsource_ref[0]['nid'], null, true);
		if($docsourceNode->field_docconverter_regexp) {
			foreach($docsourceNode->field_docconverter_regexp as $regexpField) {
				$regexp = $regexpField['value'];
				
				if(empty($regex))
					continue;
			
				// split out the regexp, replacement string, and flags
				$regexpElems = preg_split('/([^\x5c])\//', trim($regexp), -1, PREG_SPLIT_DELIM_CAPTURE);
				if(count($regexpElems) < 2)
					continue;
				$find = $regexpElems[0] . $regexpElems[1]; 
				$replace = '';
				if(count($regexpElems) >= 4)
					$replace = $regexpElems[2] . $regexpElems[3];
				
				$flags = '';
				if(count($regexpElems) >= 5)
					$flags = $regexpElems[4];
				else if(count($regexpElems) == 3
					&& substr($regexpElems[2], 0, 1) == '/')
					$flags = substr($regexpElems[2], 1);	
			
				// execute the regexp
				$html = preg_replace($find . '/' . $flags,
					$replace, $html);
				
			}
		}
		
		// keep the same filename, but with '_converted'
		$pathInfo = pathinfo($rawFileField[0]['filepath']);
		$tempFilename = $pathInfo['filename'] . '_converted.html';

		$targetDir = $pathInfo['dirname'];
	
		// save the data to a temporary file
		$tempFile = file_directory_temp() . '/' . $tempFilename;
		$fh = fopen($tempFile, "w");
		fwrite($fh, $html);
		fclose($fh);
		
		// if there's already a converted file, delete it
		if($node->field_converted_file[0]) {
			$node->field_converted_file = array();
			node_save($node);
			}
		
		// save tha actual converted file to the file field
		$node->field_converted_file[0] = field_file_save_file(
   		$tempFile, array(), $targetDir);
		
	}
	
	return $node;
		
}

?>