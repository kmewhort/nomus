<?php
//NOTE: for nomus_tags to work, taxonomy_get_terms() (in taxonomy.module) must be
// patched as follows:
// function taxonomy_node_get_terms($node, $key = 'tid', $reset = FALSE) {
//  static $terms;
//  if($reset) {
//  	unset($terms[$node->vid][$key]);
//  }


// form to show existing terms, vote on terms, and add new terms
function nomus_tags_form($form_id, $node){
	$form = array();
   $form['tags'] = array(
		'#type' => 'markup',
   	'#prefix' => '<div class="container-inline-block">',
   	'#suffix' => '</div>',
  		);
	
	$form['tags']['current-tags-wrapper'] = array(
    '#type' => 'markup',
    '#prefix' => '<div class="container-inline" id="currenttags">',
    '#suffix' => '</div>',
    '#weight' => 0);
    
   $form['tags']['current-tags-wrapper']['current-tags'] = array(
    '#type' => 'markup',
    '#value' => nomus_current_tags($node),
    '#weight' => 0);
    
	if(user_access('use vote up/down')) {
   	$form['tags']['addtag'] = array(
    	'#type' => 'markup',
    	'#prefix' => '<div class="container-inline"><div class="container-inline-block add-tag">',
    	'#suffix' => '</div></div>',
    	'#weight' => 0);

   	$form['tags']['addtag']['newtag'] = array(
   	 '#type' => 'textfield',
	    '#size' => 14,
   	 '#maxlength' => 20,
    	'#required' => FALSE,
    	'#weight' => 1);
  
   	$form['tags']['addtag']['addbutton'] = array(
	  	'#type' => 'button',
     	'#value' => t('Add Tag!'),
	      '#ahah' => array(
   	   	'path' => 'nomus/tags/add',
      		'wrapper' => 'currenttags',
      		//'effect' => 'blur',
	      ),
   	   '#weight' => 2);
   }
   else {
   	$form['tags']['noaccess'] = array(
   	 '#type' => 'item',
   	 '#value' => t('Note: You must be logged in to add or vote on tags.'),
    	 '#weight' => 1);
   }
   
   $form['tags']['nid'] = array(
   	'#type' => 'hidden',
   	'#value' => $node->nid);

  return $form;
}


/**
 * Add tag AHAH callback
 */
function nomus_tags_add_callback() {	
	static $terms; // need $terms to reset the taxonomy cache
	
	$nid = $_POST['nid'];
	$newtag = trim($_POST['newtag']);
	
	// get the node
	$node = node_load($nid);
		
	if($newtag != ''){
	
		// use the first vocabulary for this node type
		$vocabs = taxonomy_get_vocabularies($node->type);
		$vid = reset($vocabs)->vid;
	
		// comma seperated list of the new term and existing terms 
		$tags = trim(strtolower($newtag));
		if($node->tags[$vid]) {
			foreach($node->tags[$vid] as $existingTag) {
				$tags .= ',' . $existingTag->name;
			}
		}
	
		// save
		$taxon = array();
		$taxon['tags'][$vid] = $tags;
		taxonomy_node_save($node, $taxon);
		
		// clear the taxonomy cache and retrieve the terms
		$node->taxonomy = taxonomy_node_get_terms($node, 'tid', true);
	}
	
	// redraw the taxonomy
  	//$node = node_load(array("nid" => $node->nid), null, true);
  	$form = nomus_tags_form(null, $node);
 	$output = drupal_render($form['tags']['current-tags-wrapper']['current-tags']);
 	print drupal_json(array('status' => TRUE, 'data' => $output));

}



// html for current tags, with vote up/down for each term
function nomus_current_tags(&$node){
	// configured widget	
	$widget = variable_get('vud_term_widget', 'plain');
	
	// generate a list of the terms
	$renderedTerms = array();
	$order = array();
	if($node->taxonomy){
   	foreach ($node->taxonomy as $term => $tdetails) {
   		$content_id =  $tdetails->tid;
    		$tag = "n$node->nid";
    		$term = $tdetails->name;
    		$order[] = _vud_term_get_row_votes($content_id, $tag);
    		$renderedTerms[] = '<div class="vud vud-widget-updown"><div class="vud-term">' . l($term, "taxonomy/term/" . $content_id) . '</div>' .
    			theme('vud_widget', $content_id, "term", $tag, 'thumbs') . '</div>';
    			
  		}
  	}
  	array_multisort($order, SORT_DESC, $renderedTerms);
  		
  	return implode('', $renderedTerms);
}