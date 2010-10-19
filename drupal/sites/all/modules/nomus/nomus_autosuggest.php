<?php
# autosuggest - performs an auto suggest search, returning terms as json, based on provided partial query 

# Includes
require_once('globals.php');
require_once('utils.php');

// constants

// max number of case title matches to return
$caseNameLimit = 8;
// total max (where the remainder after case names is filled 
//with textual matches)
$maxLimit = 16;

// get input
$partialQuery = '';
if(isset($_GET["term"]))
	$partialQuery = $_GET["term"];

$outputTerms = array();

// escape lucene special chars
$escapeChars ='/[\\\+\-\!\(\)\:\^\]\[\{\}\~\*\?]/';
$partialQuery = preg_replace($escapeChars, "\\\\$0", $partialQuery);
	
//	
// first, search for up to five matches based on Title
//
$query = $autosuggestUrl . "?wt=json&terms.fl=Title&terms.limit=$caseNameLimit&terms.regex=" . $partialQuery . ".*";
$searchResponse = json_decode(Utils::getUrlContents($query, $selectLogin, true), false);

// if the search returned results
$searchStatus = $searchResponse->responseHeader->status;
if(!$searchStatus && count($searchResponse) >= 1)
{
	// get the terms
	$termList = $searchResponse->terms[1];
	foreach($termList as $term)
	{
		// discard the integer term counts
		if(is_int($term))
			continue;
			
		$outputTerms[] = $term;
	}
}

//
// second, search for matches based on FullTextExact to bring total up to 12
//

// split out the last term
$lastSpaceChar = strripos($partialQuery, " ");
if(!$lastSpaceChar)
{
	$firstTerms = '';
	$lastTerm = $partialQuery;
}
else
{
	$firstTerms = substr($partialQuery, 0, $lastSpaceChar+1);
	$lastTerm = substr($partialQuery, $lastSpaceChar+1);
}

// only search if the last term is greater than or equal to two characters
if(strlen($lastTerm) >= 2)
{

	// search
	$maxResults = $maxLimit-count($outputTerms);
	$query = $autosuggestUrl . "?wt=json&terms.fl=FullTextExact&terms.limit=$maxResults&terms.regex=" .
		$lastTerm . ".*";
		
	$searchResponse = json_decode(Utils::getUrlContents($query, $selectLogin, true), false);

	// if the search returned results
	$searchStatus = $searchResponse->responseHeader->status;
	if(!$searchStatus && count($searchResponse) >= 1)
	{
		// get the terms
		$termList = $searchResponse->terms[1];
		foreach($termList as $resultterm)
		{
			// discard the integer term counts
			if(is_int($resultterm))
				continue;
			$outputTerms[] = $firstTerms . $resultterm;
		}
	}
}

// output the result as json
echo json_encode($outputTerms);

?>
