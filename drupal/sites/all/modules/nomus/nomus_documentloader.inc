<?php

/**
 * Bulk load - Current Month
 */
function nomus_bulk_load_currentmonth($nids){
	global $user;
	// no timeout
	set_time_limit(0);	
	
	// store the nids to the user data
	$bulkLoadData['nids'] = $nids;
	$bulkLoadData['year'] = date('Y');
	$bulkLoadData['month'] = date('n');
	$userdata = array('nomus_documentloader_bulkloaddata' => $bulkLoadData);
  	user_save($user, $userdata);
	
	// redirect to the launch page
	drupal_goto('nomus/documentloader/bulkload');
}

/**
 * Bulk load - Current Year
 */
function nomus_bulk_load_currentyear($nids){
	global $user;
	// no timeout
	set_time_limit(0);	
	
	// store the nids to the user data
	$bulkLoadData['nids'] = $nids;
	$bulkLoadData['year'] = date('Y');
	$bulkLoadData['month'] = '';
	$userdata = array('nomus_documentloader_bulkloaddata' => $bulkLoadData);
  	user_save($user, $userdata);
	
	// redirect to the launch page
	drupal_goto('nomus/documentloader/bulkload');
}

/**
 * Bulk load - All Years
 */
function nomus_bulk_load_allyears($nids){
	global $user;
	// no timeout
	set_time_limit(0);	
	
	// store the nids to the user data
	$bulkLoadData['nids'] = $nids;
	$bulkLoadData['year'] = '';
	$bulkLoadData['month'] = '';
	$userdata = array('nomus_documentloader_bulkloaddata' => $bulkLoadData);
  	user_save($user, $userdata);
	
	// redirect to the launch page
	drupal_goto('nomus/documentloader/bulkload');
}

function nomus_documentloader_bulkload_launch(){
	global $user;
	// no timeout
	set_time_limit(0);
	
	// retrieve the nids from the user data
	$bulkLoadData = $user->nomus_documentloader_bulkloaddata;
	return drupal_get_form('nomus_documentloader_toolbox_form',
		$bulkLoadData['nids'], true, $bulkLoadData['year'], $bulkLoadData['month']);
}


/**
 * Document Loader toolbox form
 */
function nomus_documentloader_toolbox_form($form_id, $nids,
	$bulkload = false, $year = '', $month = ''){

	$form = array();
	
	// xml and nids as hidden fields
	foreach($nids as $nid){
		$node = node_load($nid);
		$form['data'][$nid]['documentloaderxml'] = array(
    		'#type' => 'hidden',
   		'#value' => $node->field_docloader_xml[0]['value'],
  		);
  		$form['data'][$nid]['docsourcenid'] = array(
    		'#type' => 'hidden',
   		'#value' => $node->nid,
  		);
  	}
  	
  	// toolbar buttons if this isn't a bulk load

  	$form['toolbar'] = array(
    		'#type' => 'fieldset',
   		'#title' => t('Tools: Document Loader'),
  			);
  		
	$form['toolbar']['settings'] = array(
    		'#type' => 'markup',
   		'#prefix' => '<div class="container-inline">',
   		'#suffix' => '</div>',
  			);
  			
  		
	$form['toolbar']['settings']['month'] = array(
	  		'#type' => 'textfield',
	 		'#title' => t('Month'),
	  		'#default_value' => $month,
	  		'#size' => 2,
	  		'#maxlength' => 2,
	  		'#required' => FALSE,
			);
	$form['toolbar']['settings']['startyear'] = array(
	  		'#type' => 'textfield',
	  		'#title' => t('Start Year'),
	  		'#default_value' => $year,
	  		'#size' => 4,
	  		'#maxlength' => 4,
	  		'#required' => FALSE,
			);
	$form['toolbar']['settings']['endyear'] = array(
	  		'#type' => 'textfield',
	  		'#title' => t('End Year'),
	  		'#default_value' => $year,
	  		'#size' => 4,
	  		'#maxlength' => 4,
	  		'#required' => FALSE,
			);
	$form['toolbar']['settings']['maxfiles'] = array(
	  		'#type' => 'textfield',
	  		'#title' => t('Max files'),
	  		'#default_value' => '',
	  		'#size' => 4,
	  		'#maxlength' => 4,
	  		'#required' => FALSE,
			);
	$form['toolbar']['settings']['debug'] = array(
		  	'#type' => 'checkbox',
		  	'#title' => t('Server-side debug?'),
			);
	
	$form['toolbar']['buttons'] = array(
    		'#type' => 'markup',
   		'#prefix' => '<div class="container-inline">',
   		'#suffix' => '</div>',
  			);
   
   $form['toolbar']['buttons']['submit'] = array(
   	'#type' => 'submit',
   	'#value' => t("Launch"),
   	'#ahah' => array(
     		'path' => 'nomus/documentloader/launch',
     		'wrapper' => 'resultbox',
   	));
   
   $form['toolbar']['buttons']['firefoxplugin_notify'] = array(
  		'#type' => 'markup',
  		'#value' => '<div id="nomusSiteSpiderCallback" style="display: none;">'
  			. url('nomus/documentloader/nextcommand', array('absolute' => TRUE))
  			. '?token=' . tokenauth_get_token() . '</div>',
  		);
    	
   $form['toolbar']['buttons']['status'] = array(
      '#type' => 'markup',
      '#value' => '<div id="statusbox" style="background: red; width: 150px; height: 150px; margin-right: 10px">Server unavailable.</div>',
  		);
  	$form['toolbar']['buttons']['prevresult'] = array(
      '#type' => 'markup',
      '#value' => '<div id="resultbox" style="background: green; width: 150px; height: 150px; margin-right: 10px">First run.</div>',
  		);
  		
  	// autostart for bulk loads
  	drupal_add_js(array('nomus_documentloader' => array('autostart' => $bulkload)), 'setting');
  	drupal_add_js(drupal_get_path('module', 'nomus') . "/documentloader_status.js");
  		
	return $form;
}
	

/**
 * Launch callback. 
 */
function nomus_documentloader_launch() {
	global $user;
	
	// rebuild the form
	$form_state = array('storage' => NULL, 'submitted' => FALSE);
  	$form_build_id = $_POST['form_build_id'];
  	$form = form_get_cache($form_build_id, $form_state);
  	
	$server = variable_get('nomus_documentloader_server', '');
	if($server == '')
	{
		print drupal_json(array('status' => TRUE, 'data' =>
			"Error: Document Loader URL is not configured in Drupal admin panel."));
		return;
	}
		
  	foreach($form['data'] as $nid => $loaderdata) {
  		$inputData = $loaderdata['documentloaderxml']['#value'];
  		
		// set document content type and static data in a user variable
		$staticData = array();
		$xml = new SimpleXMLElement($inputData);
		if(!$xml->StaticData) {
			print drupal_json(array('status' => TRUE, 'data' =>
				"No StaticData set in XML file"));
			return;
		}
		foreach($xml->StaticData[0] as $key => $val) {
			$staticData[$key] = (string)$val[0];
		}
		if(!isset($staticData['ContentType'])) {
			print drupal_json(array('status' => TRUE, 'data' =>
				"No ContentType set in XML file"));
			return;
		}
		if(!isset($staticData['Group'])) {
			print drupal_json(array('status' => TRUE, 'data' =>
				"No Group set in XML file"));
			return;
		}
		if(!$user) {
			print drupal_json(array('status' => TRUE, 'data' =>
				"No user data available"));
			return;
		}
	
		// store data to the user variables
		$staticData['DocsourceNid'] = $nid;
  		$userdata = array('nomus_documentloader_staticdata' => $staticData);
  		user_save($user, $userdata);

		// authentication token (for the firefox plugin to authenticate
		// as this user)
		$authtoken = tokenauth_get_token();

	   // if the start year is not set, start from the earliest year
	   // set in the static XML data
	   $startYear = date("Y");
	   if(empty($_POST['startyear'])){
	   	if($xml->Coverage[0]->StartYear){
				$startYear = (string) $xml->Coverage[0]->StartYear;
			}
	   }
	   else{
	   	$startYear = $_POST['startyear'];
	   }
	   
	   // if the end year is not set, end at the current year
	   $endYear = date("Y");
	   if(!empty($_POST['endyear'])){
	   	$endYear = $_POST['endyear'];
	   }
	   
			   

		for($year = $startYear; $year <= $endYear; $year++){	
   		// create the request to send the document loader XML info to the server,
   		// in order to launch
			$url = $server .
				'?req=run&' .
				'year=' . $year;
			if($_POST['month']) {
				$url .= '&' . 'month=' . $_POST['month'];
			}
			if($_POST['debug']) {
				$url .= '&debug=true';
			}
			if($_POST['maxfiles'] && $_POST['maxfiles'] != '') {
				$url .= '&maxFiles=' . $_POST['maxfiles'];
			}
			$url .= '&user=' . $user->uid;
			$url .= '&commandRequestUrl=' . urlencode(url('nomus/documentloader/nextcommand', array('absolute' => TRUE))
				. "?token=$authtoken");
			$url .= '&fileUploadUrl=' . urlencode(url('nomus/documentloader/fileupload', array('absolute' => TRUE))
				. "?token=$authtoken");
			$url .= '&fileUniqueCheckUrl=' . urlencode(url('nomus/documentloader/fileuniquecheck', array('absolute' => TRUE))
				. "?token=$authtoken");
			$request = new HttpRequest($url);
			$request->setMethod(HTTP_METH_POST);
			$opts = array("Content-Type"=>"text/html", "Accept"=>"text/plain");
			$request->addHeaders($opts);
			$request->setBody($inputData);

			$result = $request->send();
			$outputData = $result->getBody();
		}//end foreach year
	}//end foreach node

	// return the result into the div
	$resultStatus = "Previous result: success";
  	print drupal_json(array('status' => TRUE, 'data' => $resultStatus));
}

/**
 * Callback to get next JS Command callback (used by the browser plugin
 * to follow instructions)
 */
function nomus_documentloader_nextcommand() {
	global $user;
	 
	$server = variable_get('nomus_documentloader_server', '');
	if($server == '')
	{
		return;
	}
	
	// get the raw post data
	$inputData = file_get_contents('php://input');
		
   // create the request to send the servlet
	$url = $server . '?req=' . $_GET['req'];
	$url .= '&user=' . $user->uid;
	$request = new HttpRequest($url);
	$request->setMethod(HTTP_METH_POST);
	$opts = array("Content-Type"=>"text/html", "Accept"=>"text/plain");
	$request->addHeaders($opts);
	$request->setBody($inputData);
	$result = $request->send();
	$outputData = $result->getBody();

	// quick parse of the XML result
	print $outputData;
}

/**
 * Callback to create a node from a posted file (used by the browser plugin to create
 * a new node from the document retrieved)
 */
function nomus_documentloader_fileupload() {
	global $user;
	
	// get the raw post data
	$inputData = json_decode(file_get_contents('php://input'));

	// decode the hex
	$decodedData = preg_replace('/(.{2})/e',
		"chr(hexdec('\\1'))", $inputData->encodedFile);
	
	// save the data to a temporary file
	$tempFile = file_directory_temp() . '/' . uniqid();
	if($inputData->forcedExtension)
		$tempFile .= '.' . $inputData->forcedExtension;
	else
	{
		$ext = pathinfo($inputData->link, PATHINFO_EXTENSION);
		if($ext)
		{
			$tempFile .= '.' . $ext;
		}
	} 
	$fh = fopen($tempFile, "w");
	fwrite($fh, $decodedData);
	fclose($fh);
	chmod($tempFile, 0666);
	
	// get the static data from the user variables
	$staticData = $user->nomus_documentloader_staticdata;
		
	// create the node
	$node = (object) NULL;
	$node->type = $staticData['ContentType'];
	$node->field_raw_group[0]['value'] = $staticData['Group'];
	$node->field_documentsource_ref[0]['nid'] = $staticData['DocsourceNid'];
	$node->status = 0;
	$values = array();
	
	
	// set the static data
	foreach($staticData as $key => $val)
	{
		if($key != 'ContentType' && $key != 'Group'
			&& $key != 'DocsourceNid')
			$values[$key] = $val;
	}
	
	// set the unique id and link text
	$node->title = $inputData->link;
	$node->field_raw_unique_id[0]['value'] = $inputData->uniqueId;
	$node->field_raw_link[0]['value'] = $inputData->link;
	$node->field_raw_link_text[0]['value'] = $inputData->linkText;
	
	
	// set the file
	$dest = file_directory_path() .
   		"/${staticData['ContentType']}/${staticData['Group']}";
   // if there's any .'s or ~'s, bail for security reasons
   if(preg_match('/[\.\~]/', $dest))
   {
   	dpm("Invalid content type or group name");
   	return;
   }
   mkdir($dest, 0733,  true);
   $node->field_raw_file[] = field_file_save_file(
   	$tempFile, array(), $dest);

	// store the node
	$node = node_submit($node);
	$node->status = 0;
	$node->uid = $user->uid;
	node_save($node);

	// queue the node for processing
	job_queue_add('nomus_process', 'Process node ' . $node->nid . ' (convert, parse & render)',
		array($node->nid), '', TRUE, -1);

	// acknowledge
	print "{\"response\": \"FileReceived\"}";
}

function nomus_documentloader_fileuniquecheck() {
	global $user;
	
	// get the raw post data
	$uniqueId = file_get_contents('php://input');

	// query whether unique id exists (for this content type)
	$dbRow = 'content_type_' . $user->nomus_documentloader_staticdata['ContentType'];
	$result = db_query("SELECT COUNT(*) as count FROM $dbRow WHERE field_raw_unique_id_value = '%s'", $uniqueId);
	//dd("SELECT COUNT(*) as count FROM $dbRow WHERE field_raw_unique_id_value = '" . $uniqueId . "'");
	$row = db_fetch_object($result);
	if($row->count == 0)
  		print 'true';
  	else
		print 'false';
}

/**
 * Callback to check the status
 */
function nomus_documentloader_status() {
	global $user;
	$server = variable_get('nomus_documentloader_server', '');
	if($server == '')
	{
		print "Error: Document Loader URL is not configured in Drupal admin panel.";
		return;
	}
		
   // create the request to send the servlet
	$url = $server . '?req=status';
	$url .= '&user=' . $user->uid;
	$request = new HttpRequest($url);
	$request->setMethod(HTTP_METH_GET);
	$opts = array("Content-Type"=>"text/html", "Accept"=>"text/plain");
	$request->addHeaders($opts);
	$result = $request->send();
	$outputData = $result->getBody();

	// quick parse of the XML result
	print htmlspecialchars($outputData);
}

?>