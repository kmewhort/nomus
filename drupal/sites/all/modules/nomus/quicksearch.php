<?php
// load the drupal settings.php file to get db settings
require_once("../../../default/settings.php");

//
// get the solr url from drupal
//

// connect to the drupal db
$url = parse_url($db_url);
$connection = @mysql_connect($url['host'], $url['user'], $url['pass'], TRUE, 2);
if (!$connection || !mysql_select_db(substr($url['path'], 1))) {
	print "Error: Cannot connect to database.\n";
	exit(0);
}

// get the solr server info from drupal variables
$solrServer = array(
	'apachesolr_host' => 'localhost',
	'apachesolr_port'=> 8983,
	'apachesolr_path' => '/solr');
foreach($solrServer as $variable => $default){
	$result=mysql_query("SELECT value FROM variable WHERE name = '$variable'");
	if(!$result){
		print "Database error.\n";
		exit(0);
	}
	$row = mysql_fetch_row($result);
   if(!empty($row[0])){
		$solrServer[$variable] = preg_replace('/.*?"([^"]+?)".*/', "$1", $row[0]);
	}
}
$solrUrl = 'http://' . $solrServer['apachesolr_host'] . ':' .
	$solrServer['apachesolr_port'] . $solrServer['apachesolr_path'] . '/standard';
mysql_close($connection);

//
// get the search terms
//
$query = '';
if(!empty($_GET["q"]))
	$query = $_GET["q"];
$escapeChars ='/[\\\+\-\!\(\)\:\^\]\[\{\}\~\*\?]/';
$query = '?q=' . urlencode(preg_replace($escapeChars, "\\\\$0", $query));

// get the qf, pf and of parameters
if(!empty($_GET['qf']))
	$query .= '&qf=' . urlencode($_GET['qf']);
if(!empty($_GET['pf']))
	$query .= '&pf=' . urlencode($_GET['pf']);
if(!empty($_GET['of']))
	$query .= '&of=' . urlencode($_GET['of']);

// get the language
$lang = 'en';
if(!empty($_GET["lang"]))
	$lang = $_GET["lang"];

// perform the search
$output = quickSearch($query, $solrUrl, $lang);
print json_encode($output);

// search on each single field
function quickSearch($query, $solrUrl, $lang){
		$output = array();
		
		// only retrieve the heading
		$query .= '&fl=nid%20ss_heading_en%20ss_heading_fr';
		
		// disable highlighting and faceting
		$query .= '&hl=false&facet=false';
		
		// three rows max
		$query .= '&rows=3';
		
		$ch = curl_init();
		curl_setopt($ch, CURLOPT_URL, $solrUrl . $query);
		curl_setopt($ch, CURLOPT_HEADER, 0);
		curl_setopt($ch, CURLOPT_RETURNTRANSFER, 1);
		$result = curl_exec($ch);
		curl_close($ch);
		$searchResponse = json_decode($result);

		// if the search returned results
		$searchStatus = $searchResponse->responseHeader->status;
		if(!$searchStatus && !empty($searchResponse) >= 1 && !empty($searchResponse->response->docs)){
			// get each search result
			foreach($searchResponse->response->docs as $doc){
				$headingField = 'ss_heading_' . $lang;
				$heading = $doc->$headingField;
				if(empty($heading) && $lang == 'en' && !empty($doc->ss_heading_fr)){
					$heading = $doc->ss_heading_fr;
				}
				if(empty($heading) && $lang == 'fr' && !empty($doc->ss_heading_en)){
					$heading = $doc->ss_heading_en;
				}
				$result = (object) null;
				$result->label = $heading;
				$result->url = 'node/' . $doc->nid;
				$output['results'][] = $result;
			}
			$output['numResults'] = $searchResponse->response->numFound;
		}			
		return $output;
}
?>
