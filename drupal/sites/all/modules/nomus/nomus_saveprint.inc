<?php

// generate save/print links
function nomus_saveprint_links($nid) {
	// enable modal dialogs
	modalframe_parent_js();
	drupal_add_js(drupal_get_path('module', 'nomus') . '/nomus_saveprint.js');
	drupal_add_js(array('nomus_saveprint' => array('nid' =>$nid)), 'setting');
	
	// save link
	$html = '<div class="saveprint-icons">';
	$saveJsOpen = "javascript:Drupal.nomus_save_open($nid)";
	$saveImg = base_path() . drupal_get_path('module', 'nomus') . '/images/save.png';
	$html .= '<div class="saveprint save-link"><img src="' . $saveImg . '"></img></div>'; 
		
	// e-mail link
	$emailJsOpen = "javascript:Drupal.nomus_email_open($nid);";
	$emailImg = base_path() . drupal_get_path('module', 'nomus') . '/images/email.png';
	$html .= '<div class="saveprint email-link"><img src="' . $emailImg . '"></img></div>';
	
	// print link
	$printJsOpen = "javascript:Drupal.nomus_print();";
	$printImg = base_path() . drupal_get_path('module', 'nomus') . '/images/print.png';
	$html .= '<div class="saveprint print-link"><img src="' . $printImg . '"></img></div>';
	$html .= '</div>';
	
	return $html;
}


// bulk save callback
function nomus_bulk_save($nids){
	// open the save dialog
	$currentPath =  base_path() . isset($_GET['q']) ? $_GET['q'] : '';
	drupal_goto($currentPath, 'nid=' . implode('_', $nids) . '&autorun=save');
}

// bulk e-mail callback
function nomus_bulk_email($nids){
	// open the e-mail
	$currentPath =  base_path() . isset($_GET['q']) ? $_GET['q'] : '';
	drupal_goto($currentPath, 'nid=' . implode('_', $nids) . '&autorun=email');
}

// enable modal dialogs in bulk node operations
function nomus_saveprint_form_alter(&$form, $form_state, $form_id){
	if(preg_match('/views_bulk_operations_form_.*' . '/', $form_id)) {
	   modalframe_parent_js();
		drupal_add_js(drupal_get_path('module', 'nomus') . '/nomus_saveprint.js');
		drupal_add_js(array('nomus_saveprint' => array('nid' =>$nid)), 'setting');
	}
}

// form to save a single node
function nomus_save_form() {
	modalframe_child_js();
	
	$nid = $_GET['nid'];
	
	// if there are multiple nids, can only send by zip
	$zipStr = '';
	if(strpos($nid, ',') !== false){
		$zipStr = t('zipped ');
	}
		
	$form['type'] = array(
    	'#type' => 'select',
    	'#title' => t("Type"),
		'#options' => array(
    		'pdf' => t('Adobe PDF ') . '(' .  $zipStr . 'pdf)',
    		'doc' => t('MS Word Document ') . '(' .  $zipStr . 'doc)',
    		'txt' => t('Plain text ') . '(' .  $zipStr . 'txt)',
  			),
    	'#required' => TRUE,
  		);
  
  $form['nid'] = array(
		'#type' => 'hidden',
		'#value' => $nid,
  		);
  		
	$form['submit'] = array(
		'#type' => 'submit',
		'#value' => t('Save'),
  		);
  	return $form;
}


function nomus_save_form_submit($form, &$form_state) {
	$type = $form_state['values']['type'];
	$nid = $form_state['values']['nid'];
	
	// close dialog, passing the save-file url to the close function
	$saveurl = base_path() . "nomus/save_complete?nid=$nid&type=$type";
	modalframe_close_dialog(array($saveurl));
		
}

function nomus_save_complete() {
	$type = $_GET['type'];
	$nids = explode('_', $_GET['nid']);
	
	$outfile = null;
	$outfilename = null;
	if(count($nids) == 1){
	
		$node = node_load($nids[0]);
	   if(!node_access('view', $node)) {
			drupal_access_denied();
			return;
		}
	
		$outfile = nomus_convert($node, $type);
		$outfilename = $node->title . '.' . $type;
	}
	else if(count($nids) > 1){
		$outfile = nomus_zip($nids, $type);
		$outfilename = 'nomus_bulk_dl-' . date('Y-M-d') . '.zip';	
	}
	
	header('Content-type: ' . file_get_mimetype($outfilename));
	header('Content-Disposition: attachment; filename="' . $outfilename . '"');
	print $outfile;
}

// form to email a single node
function nomus_email_form() {
	modalframe_child_js();
		
	$nid = $_GET['nid'];
   $form['email'] = array(
    '#type' => 'textfield',
    '#title' => t("E-mail address"),
    '#size' => 40,
    '#maxlength' => 128,
    '#required' => TRUE,
    '#default_value' => '',
   );
   
   $form['subject'] = array(
    '#type' => 'textfield',
    '#title' => t("Subject"),
    '#size' => 40,
    '#maxlength' => 128,
    '#required' => TRUE,
    '#default_value' => '',
  	);
  	
   $form['message'] = array(
    '#type' => 'textarea',
    '#title' => t("E-mail message"),
    '#cols' => 25,
    '#maxlength' => 2048,
    '#required' => FALSE,
    '#default_value' => '',
  	);
  
	// if there are multiple nids, can only send by zip
	$zipStr = '';
	if(strpos($nid, ',') !== false){
		$zipStr = t('zipped ');
	}
		
	$form['type'] = array(
    	'#type' => 'select',
    	'#title' => t("Type"),
		'#options' => array(
    		'pdf' => t('Adobe PDF ') . '(' .  $zipStr . 'pdf)',
    		'doc' => t('MS Word Document ') . '(' .  $zipStr . 'doc)',
    		'txt' => t('Plain text ') . '(' .  $zipStr . 'txt)',
  			),
    	'#required' => TRUE,
  		);
  
   $form['nid'] = array(
		'#type' => 'hidden',
		'#value' => $nid,
  		);
  		
	$form['submit'] = array(
		'#type' => 'submit',
		'#value' => t('Send'),
  		);
  		
  	return $form;
}

function nomus_email_form_validate($form, &$form_state)  {
  // validate e-mail
  if(!eregi("^[_a-z0-9-]+(\.[_a-z0-9-]+)*@[a-z0-9-]+(\.[a-z0-9-]+)*(\.[a-z]{2,3})$",
  		$form_state['values']['email']))
  	form_set_error('email', t("Invalid e-mail address."));	
}

function nomus_email_form_submit($form, &$form_state) {
	$type = $form_state['values']['type'];
	$nids = explode('_', $form_state['values']['nid']);
	
	$outfile = null;
	$outfilename = null;
	if(count($nids) == 1){
	
		$node = node_load($nids[0]);
		if(!node_access('view', $node)) {
			drupal_access_denied();
			return;
		}
	
		$outfile = nomus_convert($node, $type);
		$outfilename = $node->title . '.' . $type;
	}
	else if(count($nids) > 1){
		$outfile = nomus_zip($nids, $type);
		$outfilename = 'nomus_bulk_dl-' . date('Y-M-d') . '.zip';	
	}
	
	// send the e-mail
	$result = nomus_mail($form_state['values']['email'],
	   'Nomus.ca <noreply@nomus.ca>',
		$form_state['values']['subject'],
		$form_state['values']['message'],
		$outfile, $outfilename);
	modalframe_close_dialog();
}

// generate a document header
function nomus_saveprint_docheader() {
	// TO DO
	
	// adjust formatting:
	// 1. show top and bottom border on Nomus heading table
	// (the div border is not displayed)
	// 2. add a fixed width to the table width
	// 3. add a blank row to the top and bottom (necessary to avoid cut-off of the border)
	$inputData = preg_replace('/(<table frame=")(void")/', "$1hsides\" width=\"620px\"", $inputData, 1); 
	$inputData = preg_replace ('/<table.*?>/', "$0<tr height=\"0px\"><td></td><td></td></tr>", $inputData, 1);
	$inputData = preg_replace ('/<\/table>/', "<tr height=\"0px\"><td></td><td></td></tr>$0", $inputData,1);

	// stip any comments (otherwise they show up in Word)
	$inputData = preg_replace('/<\!--.*?-->/', '', $inputData);
	
}

// converts the node body to the requested format
// -Returns the output file data
function nomus_convert($node, $outputFormat) {
	$server = variable_get('nomus_documentconverter_server', '');
	
	// save the nodebody to a temporary file
	$infilename = file_directory_temp() . '/' . uniqid('nsp_') . '.html';
	$fh = fopen($infilename, "w");
	fwrite($fh, $node->body);
	fclose($fh); 
		
	// convert the file to the requested format
	$ch = curl_init();
	curl_setopt($ch, CURLOPT_URL, $server);
	curl_setopt($ch, CURLOPT_RETURNTRANSFER, 1);
	curl_setopt($ch, CURLOPT_POST, 1);
	curl_setopt($ch, CURLOPT_POSTFIELDS,
		array('filename'=> '@' . $infilename,
		      'to' => $outputFormat)); 
	$outfile = curl_exec($ch);
	curl_close($ch);
	
	return $outfile;
}

// sends an e-mail with an attachment
function nomus_mail($to, $from, $subject, $message,
	$attachment, $filename)
{
	$content = chunk_split(base64_encode($attachment));
	$contentType = file_get_mimetype($filename);
	$uid = md5(date('r', time()));
		
	$header = "From: $from\r\n";
	$header .= "Reply-To: $from\r\n";
	$header .= "MIME-Version: 1.0\r\n";
	$header .= "Content-Type: multipart/mixed; boundary=\"".$uid."\"\r\n\r\n";
	$header .= "This is a multi-part message in MIME format.\r\n";
	$header .= "--".$uid."\r\n";
	$header .= "Content-type:text/plain; charset=iso-8859-1\r\n";
	$header .= "Content-Transfer-Encoding: 7bit\r\n\r\n";
	$header .= $message."\r\n\r\n";
	$header .= "--".$uid."\r\n";
	$header .= "Content-Type: " . $contentType . "; name=\"".$filename."\"\r\n"; // use diff. tyoes here
	$header .= "Content-Transfer-Encoding: base64\r\n";
	$header .= "Content-Disposition: attachment; filename=\"".$filename."\"\r\n\r\n";
	$header .= $content."\r\n\r\n";
	$header .= "--".$uid."--";
	return mail($to, $subject, "", $header . $message);
}

// converts the node body to the requested format
// -Returns the output file data
function nomus_zip($nids, $outputFormat) {

	// create new directory in tmp
	$dir = file_directory_temp() . '/' . uniqid('nzip', true);
	mkdir($dir);

	foreach($nids as $nid) {
		// convert the file
		$node = node_load($nid);
		
		if(!user_access('access bulk saveprint') ||  !node_access('view', $node)){;			
			drupal_access_denied();
			return;
		}

		$outfile = nomus_convert($node, $outputFormat);
		$outfilename = $node->title . '.' . $outputFormat;
		
		// write out the file to the temp dir
		$fh = fopen("$dir/$outfilename", 'w');
		fwrite($fh, $outfile);
		fclose($fh);
	}
	
	// zip the files
	$zipCommand = "cd $dir; zip - *";
	$outputData = `$zipCommand`;
	
	return $outputData;
}
?>