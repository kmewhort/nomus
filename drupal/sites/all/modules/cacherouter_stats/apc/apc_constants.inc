<?php

////////// READ OPTIONAL CONFIGURATION FILE ////////////
if (file_exists("apc.conf.php")) 
    include("apc.conf.php");
////////////////////////////////////////////////////////

// operation constants
define('OB_HOST_STATS',1);
define('OB_SYS_CACHE',2);
define('OB_USER_CACHE',3);
define('OB_SYS_CACHE_DIR',4);
define('OB_VERSION_CHECK',9);

