<?php
// load the drupal settings.php file to get db settings
require_once("../../../default/settings.php");
require_once("nomus_api.inc");

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

// get the language
$lang = 'en';
if(!empty($_GET["lang"]))
	$lang = $_GET["lang"];

// perform the search
$output = nomus_api_quick_search($solrUrl, $lang);
print json_encode($output);

?>
