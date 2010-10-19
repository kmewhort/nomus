<?php

// $Id: apc.inc,v 1.1.2.1 2010/07/29 14:29:11 fizk Exp $';

/*
  +----------------------------------------------------------------------+
  | APC                                                                  |
  +----------------------------------------------------------------------+
  | Copyright (c) 2006-2008 The PHP Group                                |
  +----------------------------------------------------------------------+
  | This source file is subject to version 3.01 of the PHP license,      |
  | that is bundled with this package in the file LICENSE, and is        |
  | available through the world-wide-web at the following url:           |
  | http://www.php.net/license/3_01.txt                                  |
  | If you did not receive a copy of the PHP license and are unable to   |
  | obtain it through the world-wide-web, please send a note to          |
  | license@php.net so we can mail you a copy immediately.               |
  +----------------------------------------------------------------------+
  | Authors: Ralf Becker <beckerr@php.net>                               |
  |          Rasmus Lerdorf <rasmus@php.net>                             |
  |          Ilia Alshanetsky <ilia@prohost.org>                         |
  +----------------------------------------------------------------------+

   All other licensing and usage conditions are those of the PHP Group.

 */

require_once 'apc_constants.inc';

class apc_gui {

    public function __construct() {
        //defaults('PROXY', 'tcp://127.0.0.1:8080');
        //defaults('DATE_FORMAT', "d.m.Y H:i:s");	// German
        $this->defaults('DATE_FORMAT', 'Y/m/d H:i:s'); 	// US
        $this->defaults('GRAPH_SIZE',200);					// Image size

        // rewrite $PHP_SELF to block XSS attacks
        //
        $this->PHP_SELF= url($_GET['q']);
        $this->time = time();
        $this->host = php_uname('n');
        if($this->host) { $this->host = '('.$this->host.')'; }
        if ($_SERVER['SERVER_ADDR']) {
          $this->host .= ' ('.$_SERVER['SERVER_ADDR'].')';
        }

        // check validity of input variables
        $this->vardom=array(
            'OB'	=> '/^\d+$/',			// operational mode switch
            'CC'	=> '/^[01]$/',			// clear cache requested
            'DU'	=> '/^.*$/',			// Delete User Key
            'SH'	=> '/^[a-z0-9]+$/',		// shared object description

            'IMG'	=> '/^[123]$/',			// image to generate
            'LO'	=> '/^1$/',				// login requested

            'COUNT'	=> '/^\d+$/',			// number of line displayed in list
            'SCOPE'	=> '/^[AD]$/',			// list view scope
            'SORT1'	=> '/^[AHSMCDTZ]$/',	// first sort key
            'SORT2'	=> '/^[DA]$/',			// second sort key
            'AGGR'	=> '/^\d+$/',			// aggregation by dir level
            'SEARCH'	=> '~^[a-zA-Z0-1/_.-]*$~'			// aggregation by dir level
        );

        // default cache mode
        $this->cache_mode='opcode';

        // cache scope
        $this->scope_list=array(
            'A' => 'cache_list',
            'D' => 'deleted_list'
        );

        // handle POST and GET requests
        if (empty($_REQUEST)) {
            if (!empty($_GET) && !empty($_POST)) {
                $_REQUEST = array_merge($_GET, $_POST);
            } else if (!empty($_GET)) {
                $_REQUEST = $_GET;
            } else if (!empty($_POST)) {
                $_REQUEST = $_POST;
            } else {
                $_REQUEST = array();
            }
        }

        // check parameter syntax
        foreach($this->vardom as $var => $dom) {
            if (!isset($_REQUEST[$var])) {
                $this->MYREQUEST[$var]=NULL;
            } else if (!is_array($_REQUEST[$var]) && preg_match($dom.'D',$_REQUEST[$var])) {
                $this->MYREQUEST[$var]=$_REQUEST[$var];
            } else {
                $this->MYREQUEST[$var]=$_REQUEST[$var]=NULL;
            }
        }

        // check parameter sematics
        if (empty($this->MYREQUEST['SCOPE'])) $this->MYREQUEST['SCOPE']="A";
        if (empty($this->MYREQUEST['SORT1'])) $this->MYREQUEST['SORT1']="H";
        if (empty($this->MYREQUEST['SORT2'])) $this->MYREQUEST['SORT2']="D";
        if (empty($this->MYREQUEST['OB']))	$this->MYREQUEST['OB']=OB_HOST_STATS;
        if (!isset($this->MYREQUEST['COUNT'])) $this->MYREQUEST['COUNT']=20;
        if (!isset($this->scope_list[$this->MYREQUEST['SCOPE']])) $this->MYREQUEST['SCOPE']='A';

        $this->MY_SELF=
            "$this->PHP_SELF".
            "?SCOPE=".$this->MYREQUEST['SCOPE'].
            "&SORT1=".$this->MYREQUEST['SORT1'].
            "&SORT2=".$this->MYREQUEST['SORT2'].
            "&COUNT=".$this->MYREQUEST['COUNT'];
        $this->MY_SELF_WO_SORT=
            "$this->PHP_SELF".
            "?SCOPE=".$this->MYREQUEST['SCOPE'].
            "&COUNT=".$this->MYREQUEST['COUNT'];
            
        // select cache mode
        if ($this->MYREQUEST['OB'] == OB_USER_CACHE) {
            $this->cache_mode='user';
        }
        // clear cache
        if (isset($this->MYREQUEST['CC']) && $this->MYREQUEST['CC']) {
            apc_clear_cache($this->cache_mode);
        }

        if (!empty($this->MYREQUEST['DU'])) {
            apc_delete($this->MYREQUEST['DU']);
        }

        if(!function_exists('apc_cache_info') || !($this->cache=@apc_cache_info($this->cache_mode))) {
            echo "No cache info available.  APC does not appear to be running.";
          exit;
        }

        $this->cache_user = apc_cache_info('user', 1);  
        $this->mem = apc_sma_info();
        if(!$this->cache['num_hits']) { 
            $this->cache['num_hits']=1; $this->time++; // Avoid division by 0 errors on a cache clear
        }

        // don't cache this page
        //
        header("Cache-Control: no-store, no-cache, must-revalidate");  // HTTP/1.1
        header("Cache-Control: post-check=0, pre-check=0", false);
        header("Pragma: no-cache");                                    // HTTP/1.0

        if (isset($this->MYREQUEST['IMG']))
        {
            return $this->show_image();
        }
        else if ($_GET['OB'])
        {
            //return $this->full_display();
        }
    }

    // "define if not defined"
    public function defaults($d,$v) {
        if (!defined($d)) define($d,$v); // or just @define(...)
    }

    public function block_sort($array1, $array2)
    {
        if ($array1['offset'] > $array2['offset']) {
            return 1;
        } else {
            return -1;
        }
    }

    public function duration($ts) {
        $years = (int)((($this->time - $ts)/(7*86400))/52.177457);
        $rem = (int)(($this->time-$ts)-($years * 52.177457 * 7 * 86400));
        $weeks = (int)(($rem)/(7*86400));
        $days = (int)(($rem)/86400) - $weeks*7;
        $hours = (int)(($rem)/3600) - $days*24 - $weeks*7*24;
        $mins = (int)(($rem)/60) - $hours*60 - $days*24*60 - $weeks*7*24*60;
        $str = '';
        if($years==1) $str .= "$years year, ";
        if($years>1) $str .= "$years years, ";
        if($weeks==1) $str .= "$weeks week, ";
        if($weeks>1) $str .= "$weeks weeks, ";
        if($days==1) $str .= "$days day,";
        if($days>1) $str .= "$days days,";
        if($hours == 1) $str .= " $hours hour and";
        if($hours>1) $str .= " $hours hours and";
        if($mins == 1) $str .= " 1 minute";
        else $str .= " $mins minutes";
        return $str;
    }

    // create graphics
    //
    public function graphics_avail() {
        return extension_loaded('gd');
    }

    public function fill_arc($im, $centerX, $centerY, $diameter, $start, $end, $color1,$color2,$text='',$placeindex=0) {
        $r=$diameter/2;
        $w=deg2rad((360+$start+($end-$start)/2)%360);

        if (function_exists("imagefilledarc")) {
            // exists only if GD 2.0.1 is avaliable
            imagefilledarc($im, $centerX+1, $centerY+1, $diameter, $diameter, $start, $end, $color1, IMG_ARC_PIE);
            imagefilledarc($im, $centerX, $centerY, $diameter, $diameter, $start, $end, $color2, IMG_ARC_PIE);
            imagefilledarc($im, $centerX, $centerY, $diameter, $diameter, $start, $end, $color1, IMG_ARC_NOFILL|IMG_ARC_EDGED);
        } else {
            imagearc($im, $centerX, $centerY, $diameter, $diameter, $start, $end, $color2);
            imageline($im, $centerX, $centerY, $centerX + cos(deg2rad($start)) * $r, $centerY + sin(deg2rad($start)) * $r, $color2);
            imageline($im, $centerX, $centerY, $centerX + cos(deg2rad($start+1)) * $r, $centerY + sin(deg2rad($start)) * $r, $color2);
            imageline($im, $centerX, $centerY, $centerX + cos(deg2rad($end-1))   * $r, $centerY + sin(deg2rad($end))   * $r, $color2);
            imageline($im, $centerX, $centerY, $centerX + cos(deg2rad($end))   * $r, $centerY + sin(deg2rad($end))   * $r, $color2);
            imagefill($im,$centerX + $r*cos($w)/2, $centerY + $r*sin($w)/2, $color2);
        }
        if ($text) {
            if ($placeindex>0) {
                imageline($im,$centerX + $r*cos($w)/2, $centerY + $r*sin($w)/2,$diameter, $placeindex*12,$color1);
                imagestring($im,4,$diameter, $placeindex*12,$text,$color1);	

            } else {
                imagestring($im,4,$centerX + $r*cos($w)/2, $centerY + $r*sin($w)/2,$text,$color1);
            }
        }
    } 

    public function text_arc($im, $centerX, $centerY, $diameter, $start, $end, $color1,$text,$placeindex=0) {
        $r=$diameter/2;
        $w=deg2rad((360+$start+($end-$start)/2)%360);

        if ($placeindex>0) {
            imageline($im,$centerX + $r*cos($w)/2, $centerY + $r*sin($w)/2,$diameter, $placeindex*12,$color1);
            imagestring($im,4,$diameter, $placeindex*12,$text,$color1);	

        } else {
            imagestring($im,4,$centerX + $r*cos($w)/2, $centerY + $r*sin($w)/2,$text,$color1);
        }
    } 

    public function fill_box($im, $x, $y, $w, $h, $color1, $color2,$text='',$placeindex='') {
        $x1=$x+$w-1;
        $y1=$y+$h-1;

        imagerectangle($im, $x, $y1, $x1+1, $y+1, $this->col_black);
        if($y1>$y) imagefilledrectangle($im, $x, $y, $x1, $y1, $color2);
        else imagefilledrectangle($im, $x, $y1, $x1, $y, $color2);
        imagerectangle($im, $x, $y1, $x1, $y, $color1);
        if ($text) {
            if ($placeindex>0) {

                if ($placeindex<16)
                {
                    $px=5;
                    $py=$placeindex*12+6;
                    imagefilledrectangle($im, $px+90, $py+3, $px+90-4, $py-3, $color2);
                    imageline($im,$x,$y+$h/2,$px+90,$py,$color2);
                    imagestring($im,2,$px,$py-6,$text,$color1);	

                } else {
                    if ($placeindex<31) {
                        $px=$x+40*2;
                        $py=($placeindex-15)*12+6;
                    } else {
                        $px=$x+40*2+100*intval(($placeindex-15)/15);
                        $py=($placeindex%15)*12+6;
                    }
                    imagefilledrectangle($im, $px, $py+3, $px-4, $py-3, $color2);
                    imageline($im,$x+$w,$y+$h/2,$px,$py,$color2);
                    imagestring($im,2,$px+2,$py-6,$text,$color1);	
                }
            } else {
                imagestring($im,4,$x+5,$y1-16,$text,$color1);
            }
        }
    }

    public function show_image() {
        if (!$this->graphics_avail()) {
            exit(0);
        }

        $size = GRAPH_SIZE; // image size
        if ($this->MYREQUEST['IMG']==3)
            $image = imagecreate(2*$size+150, $size+10);
        else
            $image = imagecreate($size+50, $size+10);

        $col_white = imagecolorallocate($image, 0xFF, 0xFF, 0xFF);
        $col_red   = imagecolorallocate($image, 0xD0, 0x60,  0x30);
        $col_green = imagecolorallocate($image, 0x60, 0xF0, 0x60);
        $this->col_black = imagecolorallocate($image,   0,   0,   0);
        imagecolortransparent($image,$col_white);

        switch ($this->MYREQUEST['IMG']) {
        
            case 1:
                $s=$this->mem['num_seg']*$this->mem['seg_size'];
                $a=$this->mem['avail_mem'];
                $x=$y=$size/2;
                $fuzz = 0.000001;

                // This block of code creates the pie chart.  It is a lot more complex than you
                // would expect because we try to visualize any memory fragmentation as well.
                $angle_from = 0;
                $string_placement=array();
                for($i=0; $i<$this->mem['num_seg']; $i++) {	
                    $ptr = 0;
                    $free = $this->mem['block_lists'][$i];
                    uasort($free, array($this, 'block_sort'));
                    foreach($free as $block) {
                        if($block['offset']!=$ptr) {       // Used block
                            $angle_to = $angle_from+($block['offset']-$ptr)/$s;
                            if(($angle_to+$fuzz)>1) $angle_to = 1;
                            if( ($angle_to*360) - ($angle_from*360) >= 1) {
                                $this->fill_arc($image,$x,$y,$size,$angle_from*360,$angle_to*360,$this->col_black,$col_red);
                                if (($angle_to-$angle_from)>0.05) {
                                    array_push($string_placement, array($angle_from,$angle_to));
                                }
                            }
                            $angle_from = $angle_to;
                        }
                        $angle_to = $angle_from+($block['size'])/$s;
                        if(($angle_to+$fuzz)>1) $angle_to = 1;
                        if( ($angle_to*360) - ($angle_from*360) >= 1) {
                            $this->fill_arc($image,$x,$y,$size,$angle_from*360,$angle_to*360,$this->col_black,$col_green);
                            if (($angle_to-$angle_from)>0.05) {
                                array_push($string_placement, array($angle_from,$angle_to));
                            }
                        }
                        $angle_from = $angle_to;
                        $ptr = $block['offset']+$block['size'];
                    }
                    if ($ptr < $this->mem['seg_size']) { // memory at the end 
                        $angle_to = $angle_from + ($this->mem['seg_size'] - $ptr)/$s;
                        if(($angle_to+$fuzz)>1) $angle_to = 1;
                        $this->fill_arc($image,$x,$y,$size,$angle_from*360,$angle_to*360,$this->col_black,$col_red);
                        if (($angle_to-$angle_from)>0.05) {
                            array_push($string_placement, array($angle_from,$angle_to));
                        }
                    }
                }
                foreach ($string_placement as $angle) {
                    $this->text_arc($image,$x,$y,$size,$angle[0]*360,$angle[1]*360,$this->col_black,$this->bsize($s*($angle[1]-$angle[0])));
                }
                break;
                
            case 2: 
                $s=$this->cache['num_hits']+$this->cache['num_misses'];
                $a=$this->cache['num_hits'];
                
                $this->fill_box($image, 30,$size,50,-$a*($size-21)/$s,$this->col_black,$col_green,sprintf("%.1f%%",$this->cache['num_hits']*100/$s));
                $this->fill_box($image,130,$size,50,-max(4,($s-$a)*($size-21)/$s),$this->col_black,$col_red,sprintf("%.1f%%",$this->cache['num_misses']*100/$s));
                break;
                
            case 3:
                $s=$this->mem['num_seg']*$this->mem['seg_size'];
                $a=$this->mem['avail_mem'];
                $x=130;
                $y=1;
                $j=1;

                // This block of code creates the bar chart.  It is a lot more complex than you
                // would expect because we try to visualize any memory fragmentation as well.
                for($i=0; $i<$this->mem['num_seg']; $i++) {	
                    $ptr = 0;
                    $free = $this->mem['block_lists'][$i];
                    uasort($free, array($this, 'block_sort'));
                    foreach($free as $block) {
                        if($block['offset']!=$ptr) {       // Used block
                            $h=(GRAPH_SIZE-5)*($block['offset']-$ptr)/$s;
                            if ($h>0) {
                                                        $j++;
                                if($j<75) $this->fill_box($image,$x,$y,50,$h,$this->col_black,$col_red,$this->bsize($block['offset']-$ptr),$j);
                                                        else $this->fill_box($image,$x,$y,50,$h,$this->col_black,$col_red);
                                                }
                            $y+=$h;
                        }
                        $h=(GRAPH_SIZE-5)*($block['size'])/$s;
                        if ($h>0) {
                                                $j++;
                            if($j<75) $this->fill_box($image,$x,$y,50,$h,$this->col_black,$col_green,$this->bsize($block['size']),$j);
                            else $this->fill_box($image,$x,$y,50,$h,$this->col_black,$col_green);
                                        }
                        $y+=$h;
                        $ptr = $block['offset']+$block['size'];
                    }
                    if ($ptr < $this->mem['seg_size']) { // memory at the end 
                        $h = (GRAPH_SIZE-5) * ($this->mem['seg_size'] - $ptr) / $s;
                        if ($h > 0) {
                            $this->fill_box($image,$x,$y,50,$h,$this->col_black,$col_red,$this->bsize($this->mem['seg_size']-$ptr),$j++);
                        }
                    }
                }
                break;
            case 4: 
                $s=$this->cache['num_hits']+$this->cache['num_misses'];
                $a=$this->cache['num_hits'];
                        
                $this->fill_box($image, 30,$size,50,-$a*($size-21)/$s,$this->col_black,$col_green,sprintf("%.1f%%",$this->cache['num_hits']*100/$s));
                $this->fill_box($image,130,$size,50,-max(4,($s-$a)*($size-21)/$s),$this->col_black,$col_red,sprintf("%.1f%%",$this->cache['num_misses']*100/$s));
                break;
            
            }
        header("Content-type: image/png");
        imagepng($image);
        exit;
    }

    // pretty printer for byte values
    //
    public function bsize($s) {
        foreach (array('','K','M','G') as $i => $k) {
            if ($s < 1024) break;
            $s/=1024;
        }
        return sprintf("%5.1f %sBytes",$s,$k);
    }

    // sortable table header in "scripts for this host" view
    public function sortheader($key,$name,$extra='') {
        if ($this->MYREQUEST['SORT1']==$key) {
            $this->MYREQUEST['SORT2'] = $this->MYREQUEST['SORT2']=='A' ? 'D' : 'A';
        }
        return "<a class=sortable href=\"{$this->MY_SELF_WO_SORT}$extra&SORT1=$key&SORT2=".$this->MYREQUEST['SORT2']."\">$name</a>";

    }

    // create menu entry 
    public function menu_entry($ob,$title) {
        if ($this->MYREQUEST['OB']!=$ob) {
            return "<li><a href=\"{$this->MY_SELF}&OB=$ob\">$title</a></li>";
        } else if (empty($this->MYREQUEST['SH'])) {
            return "<li><span class=active>$title</span></li>";
        } else {
            return "<li><a class=\"child_active\" href=\"{$this->MY_SELF}&OB=$ob\">$title</a></li>";	
        }
    }

    public function get_section($section) {
        switch ($section) {
            // -----------------------------------------------
            // Host Stats
            // -----------------------------------------------
            case OB_HOST_STATS:
                return $this->section_host_stats(); 
                break;

            // -----------------------------------------------
            // User Cache Entries
            // -----------------------------------------------
            case OB_USER_CACHE:
                return $this->section_cache_list(OB_USER_CACHE); 
                break;

            // -----------------------------------------------
            // System Cache Entries		
            // -----------------------------------------------
            case OB_SYS_CACHE:	
                return $this->section_cache_list(OB_SYS_CACHE); 
                break;

            // -----------------------------------------------
            // Per-Directory System Cache Entries
            // -----------------------------------------------
            case OB_SYS_CACHE_DIR:	
                return $this->section_sys_cache_dir();
                break;

            // -----------------------------------------------
            // Version check
            // -----------------------------------------------
            case OB_VERSION_CHECK:
               return $this->section_version_check();
                break;
        }
    }
    
    public function section_host_stats() {
        $output = '';
        $this->mem_size = $this->mem['num_seg']*$this->mem['seg_size'];
        $this->mem_avail= $this->mem['avail_mem'];
        $this->mem_used = $this->mem_size-$this->mem_avail;
        $seg_size = $this->bsize($this->mem['seg_size']);
        $req_rate = sprintf("%.2f",($this->cache['num_hits']+$this->cache['num_misses'])/($this->time-$this->cache['start_time']));
        $hit_rate = sprintf("%.2f",($this->cache['num_hits'])/($this->time-$this->cache['start_time']));
        $miss_rate = sprintf("%.2f",($this->cache['num_misses'])/($this->time-$this->cache['start_time']));
        $insert_rate = sprintf("%.2f",($this->cache['num_inserts'])/($this->time-$this->cache['start_time']));
        $req_rate_user = sprintf("%.2f",($this->cache_user['num_hits']+$this->cache_user['num_misses'])/($this->time-$this->cache_user['start_time']));
        $hit_rate_user = sprintf("%.2f",($this->cache_user['num_hits'])/($this->time-$this->cache_user['start_time']));
        $miss_rate_user = sprintf("%.2f",($this->cache_user['num_misses'])/($this->time-$this->cache_user['start_time']));
        $insert_rate_user = sprintf("%.2f",($this->cache_user['num_inserts'])/($this->time-$this->cache_user['start_time']));
        $apcversion = phpversion('apc');
        $phpversion = phpversion();
        $number_files = $this->cache['num_entries']; 
        $size_files = $this->bsize($this->cache['mem_size']);
        $number_vars = $this->cache_user['num_entries'];
        $size_vars = $this->bsize($this->cache_user['mem_size']);
        $i=0;
        $output .= "<div class=\"info div1\"><h2>General Cache Information</h2>
            <table width=\"100%\" cellspacing=0><tbody>
            <tr class=tr-0><td class=td-0>APC Version</td><td>$apcversion</td></tr>
            <tr class=tr-1><td class=td-0>PHP Version</td><td>$phpversion</td></tr>";

        if(!empty($_SERVER['SERVER_NAME']))
            $output .= "<tr class=tr-0><td class=td-0>APC Host</td><td>{$_SERVER['SERVER_NAME']} $this->host</td></tr>\n";
        if(!empty($_SERVER['SERVER_SOFTWARE']))
            $output .= "<tr class=tr-1><td class=td-0>Server Software</td><td>{$_SERVER['SERVER_SOFTWARE']}</td></tr>\n";

        $output .= "<tr class=tr-0><td class=td-0>Shared Memory</td><td>{$this->mem['num_seg']} Segment(s) with $seg_size <br/> ({$this->cache['memory_type']} memory, {$this->cache['locking_type']} locking) </td></tr>";
        $output .=   '<tr class=tr-1><td class=td-0>Start Time</td><td>' . date(DATE_FORMAT,$this->cache['start_time']) . '</td></tr>';
        $output .=   '<tr class=tr-0><td class=td-0>Uptime</td><td>' . $this->duration($this->cache['start_time']) . '</td></tr>';
        $output .=   '<tr class=tr-1><td class=td-0>File Upload Support</td><td>' . $this->cache['file_upload_progress'] . '</td></tr>';

        $output .= "
            </tbody></table>
            </div>

            <div class=\"info div1\"><h2>File Cache Information</h2>
            <table width=\"100%\" cellspacing=0><tbody>
            <tr class=tr-0><td class=td-0>Cached Files</td><td>$number_files ($size_files)</td></tr>
            <tr class=tr-1><td class=td-0>Hits</td><td>{$this->cache['num_hits']}</td></tr>
            <tr class=tr-0><td class=td-0>Misses</td><td>{$this->cache['num_misses']}</td></tr>
            <tr class=tr-1><td class=td-0>Request Rate (hits, misses)</td><td>$req_rate cache requests/second</td></tr>
            <tr class=tr-0><td class=td-0>Hit Rate</td><td>$hit_rate cache requests/second</td></tr>
            <tr class=tr-1><td class=td-0>Miss Rate</td><td>$miss_rate cache requests/second</td></tr>
            <tr class=tr-0><td class=td-0>Insert Rate</td><td>$insert_rate cache requests/second</td></tr>
            <tr class=tr-1><td class=td-0>Cache full count</td><td>{$this->cache['expunges']}</td></tr>
            </tbody></table>
            </div>

            <div class=\"info div1\"><h2>User Cache Information</h2>
            <table width=\"100%\" cellspacing=0><tbody>
        <tr class=tr-0><td class=td-0>Cached Variables</td><td>$number_vars ($size_vars)</td></tr>
            <tr class=tr-1><td class=td-0>Hits</td><td>{$this->cache_user['num_hits']}</td></tr>
            <tr class=tr-0><td class=td-0>Misses</td><td>{$this->cache_user['num_misses']}</td></tr>
            <tr class=tr-1><td class=td-0>Request Rate (hits, misses)</td><td>$req_rate_user cache requests/second</td></tr>
            <tr class=tr-0><td class=td-0>Hit Rate</td><td>$hit_rate_user cache requests/second</td></tr>
            <tr class=tr-1><td class=td-0>Miss Rate</td><td>$miss_rate_user cache requests/second</td></tr>
            <tr class=tr-0><td class=td-0>Insert Rate</td><td>$insert_rate_user cache requests/second</td></tr>
            <tr class=tr-1><td class=td-0>Cache full count</td><td>{$this->cache_user['expunges']}</td></tr>

            </tbody></table>
            </div>

            <div class=\"info div2\"><h2>Runtime Settings</h2><table width=\"100%\" cellspacing=0><tbody>";

        $j = 0;
        foreach (ini_get_all('apc') as $k => $v) {
            $output .= "<tr class=tr-$j><td class=td-0>" . $k . "</td><td>" . str_replace(',',',<br />',$v['local_value']) . "</td></tr>\n";
            $j = 1 - $j;
        }

        if($this->mem['num_seg']>1 || $this->mem['num_seg']==1 && count($this->mem['block_lists'][0])>1)
            $this->mem_note = "Memory Usage<br /><font size=-2>(multiple slices indicate fragments)</font>";
        else
            $this->mem_note = "Memory Usage";

        $size='width='.(GRAPH_SIZE+50).' height='.(GRAPH_SIZE+10);
        $output .= "
            </tbody></table>
            </div>

            <div class=\"graph div3\"><h2>Host Status Diagrams</h2>
            <table width=\"100%\" cellspacing=0><tbody>
            <tr>
            <td class=td-0>$this->mem_note</td>
            <td class=td-1>Hits &amp; Misses</td>
            </tr>";
        if ($this->graphics_avail()) {
            $output .=
                  '<tr>'.
                  "<td class=td-0><img alt=\"\" $size src=\"$this->PHP_SELF?IMG=1&$this->time\"></td>".
                  "<td class=td-1><img alt=\"\" $size src=\"$this->PHP_SELF?IMG=2&$this->time\"></td></tr>\n";
        } else {
            $output .= 
            '<tr>' .
            '<td class=td-0><span class="green box">&nbsp;</span>Free: ' . $this->bsize($this->mem_avail).sprintf(" (%.1f%%)",$this->mem_avail*100/$this->mem_size) . "</td>\n" . 
            '<td class=td-1><span class="green box">&nbsp;</span>Hits: ' . $this->cache['num_hits'].sprintf(" (%.1f%%)",$this->cache['num_hits']*100/($this->cache['num_hits']+$this->cache['num_misses'])) . "</td>\n" . 
            '</tr>' . 
            '<tr>' . 
            '<td class=td-0><span class="red box">&nbsp;</span>Used: ' . $this->bsize($this->mem_used ).sprintf(" (%.1f%%)",$this->mem_used *100/$this->mem_size) . "</td>\n" . 
            '<td class=td-1><span class="red box">&nbsp;</span>Misses: ' . $this->cache['num_misses'].sprintf(" (%.1f%%)",$this->cache['num_misses']*100/($this->cache['num_hits']+$this->cache['num_misses'])) . "</td>\n";
        }
        $output .= "
            </tr>
            </tbody></table>

            <br/>
            <h2>Detailed Memory Usage and Fragmentation</h2>
            <table width=\"100%\" cellspacing=0><tbody>
            <tr>
            <td class=td-0 colspan=2><br/>";

        // Fragementation: (freeseg - 1) / total_seg
        $nseg = $freeseg = $fragsize = $freetotal = 0;
        for($i=0; $i<$this->mem['num_seg']; $i++) {
            $ptr = 0;
            foreach($this->mem['block_lists'][$i] as $block) {
                if ($block['offset'] != $ptr) {
                    ++$nseg;
                }
                $ptr = $block['offset'] + $block['size'];
                            /* Only consider blocks <5M for the fragmentation % */
                            if($block['size']<(5*1024*1024)) $fragsize+=$block['size'];
                            $freetotal+=$block['size'];
            }
            $freeseg += count($this->mem['block_lists'][$i]);
        }
        
        if ($freeseg > 1) {
            $frag = sprintf("%.2f%% (%s out of %s in %d fragments)", ($fragsize/$freetotal)*100,$this->bsize($fragsize),$this->bsize($freetotal),$freeseg);
        } else {
            $frag = "0%";
        }

        if ($this->graphics_avail()) {
            $size='width='.(2*GRAPH_SIZE+150).' height='.(GRAPH_SIZE+10);
            $output .= "<img alt='' $size src='$this->PHP_SELF?IMG=3&$this->time'>";
        }
        $output .= "</br>Fragmentation: $frag
            </td>
            </tr>";

        if(isset($this->mem['adist'])) {
            foreach($this->mem['adist'] as $i=>$v) {
                $cur = pow(2,$i); $nxt = pow(2,$i+1)-1;
                if($i==0) $range = "1";
                else $range = "$cur - $nxt";
                $output .= "<tr><th align=right>$range</th><td align=right>$v</td></tr>\n";
            }
        }

        $output .= "</tbody></table> </div>";
        return $output;
    }

    public function section_cache_list($type) {
        $output = '';
        if ($type == OB_USER_CACHE) {
                $fieldname='info';
                $fieldheading='User Entry Label';
                $fieldkey='info';
        } else if ($type != OB_SYS_CACHE) {
            return '<h1>Section not found</h1>';
        }

        // -----------------------------------------------
        // Cache Entries		
        // -----------------------------------------------
        if (!isset($fieldname))
        {
            $fieldname='filename';
            $fieldheading='Script Filename';
            $fieldkey = ini_get("apc.stat") ? 'inode': 'filename';
        }

        if (!empty($this->MYREQUEST['SH']))
        {
            $output .= '<div class="info"><table width=\"100%\" cellspacing=0><tbody>
                <tr><th>Attribute</th><th>Value</th></tr>';

            $m=0;
            foreach($this->scope_list as $j => $list) {
                foreach($this->cache[$list] as $i => $entry) {
                    if (md5($entry[$fieldkey])!=$this->MYREQUEST['SH']) continue;
                    foreach($entry as $k => $value) {
                        if ($k == "num_hits") {
                            $value=sprintf("%s (%.2f%%)",$value,$value*100/$this->cache['num_hits']);
                        }
                        if ($k == 'deletion_time') {
                            if(!$entry['deletion_time']) $value = "None";
                        }
                        $output .=
                            "<tr class=tr-$m>" . 
                            "<td class=td-0>" . ucwords(preg_replace("/_/"," ",$k)) . "</td>" . 
                            "<td class=td-last>" . (preg_match("/time/",$k) && $value!='None') ? date(DATE_FORMAT,$value) : $value . "</td>" . 
                            "</tr>";
                        $m=1-$m;
                    }
                    if($fieldkey=='info') {
                        $output .= "<tr class=tr-$m><td class=td-0>Stored Value</td><td class=td-last><pre>";
                        $output = var_export(apc_fetch($entry[$fieldkey]),true);
                        $output .= htmlspecialchars($output);
                        $output .= "</pre></td></tr>\n";
                    }
                    break;
                }
            }

            $output .= " 
                </tbody></table>
                </div>";
            return $output;
        }

        $cols=6;
        $output .= "
            <div class=sorting><form>Scope:
            <input type=hidden name=OB value={$this->MYREQUEST['OB']}>
            <select name=SCOPE>";
        $output .= 
            "<option value=A" . ($this->MYREQUEST['SCOPE']=='A' ? " selected":"") . ">Active</option>" . 
            "<option value=D" . ($this->MYREQUEST['SCOPE']=='D' ? " selected":"") . ">Deleted</option>" . 
            "</select>" . 
            ", Sorting:<select name=SORT1>" . 
            "<option value=H" . ($this->MYREQUEST['SORT1']=='H' ? " selected":"") . ">Hits</option>" . 
            "<option value=Z" . ($this->MYREQUEST['SORT1']=='Z' ? " selected":"") . ">Size</option>" . 
            "<option value=S" . ($this->MYREQUEST['SORT1']=='S' ? " selected":"") . ">$fieldheading</option>" . 
            "<option value=A" . ($this->MYREQUEST['SORT1']=='A' ? " selected":"") . ">Last accessed</option>" . 
            "<option value=M" . ($this->MYREQUEST['SORT1']=='M' ? " selected":"") . ">Last modified</option>" . 
            "<option value=C" . ($this->MYREQUEST['SORT1']=='C' ? " selected":"") . ">Created at</option>" . 
            "<option value=D" . ($this->MYREQUEST['SORT1']=='D' ? " selected":"") . ">Deleted at</option>";

        if($fieldname=='info') 
            $output .= "<option value=D" . ($this->MYREQUEST['SORT1']=='T' ? " selected":"") . ">Timeout</option>";
        $output .= 
            '</select>' . 
            '<select name=SORT2>' . 
            '<option value=D' . ($this->MYREQUEST['SORT2']=='D' ? ' selected':'') . '>DESC</option>' . 
            '<option value=A' . ($this->MYREQUEST['SORT2']=='A' ? ' selected':'') . '>ASC</option>' . 
            '</select>' . 
            '<select name=COUNT onChange="form.submit()">' . 
            '<option value=10 ' . ($this->MYREQUEST['COUNT']=='10' ? ' selected':'') . '>Top 10</option>' . 
            '<option value=20 ' . ($this->MYREQUEST['COUNT']=='20' ? ' selected':'') . '>Top 20</option>' . 
            '<option value=50 ' . ($this->MYREQUEST['COUNT']=='50' ? ' selected':'') . '>Top 50</option>' . 
            '<option value=100' . ($this->MYREQUEST['COUNT']=='100'? ' selected':'') . '>Top 100</option>' . 
            '<option value=150' . ($this->MYREQUEST['COUNT']=='150'? ' selected':'') . '>Top 150</option>' . 
            '<option value=200' . ($this->MYREQUEST['COUNT']=='200'? ' selected':'') . '>Top 200</option>' . 
            '<option value=500' . ($this->MYREQUEST['COUNT']=='500'? ' selected':'') . '>Top 500</option>' . 
            '<option value=0  ' . ($this->MYREQUEST['COUNT']=='0'  ? ' selected':'') . '>All</option>' . 
            '</select>' . 
        '&nbsp; Search: <input name=SEARCH value="' . $this->MYREQUEST['SEARCH'] . '" type=text size=25/>' . 
            '&nbsp;<input type=submit value="GO!">' . 
            '</form></div>';

        if (isset($this->MYREQUEST['SEARCH'])) {
            // Don't use preg_quote because we want the user to be able to specify a
            // regular expression subpattern.
            $this->MYREQUEST['SEARCH'] = '/'.str_replace('/', '\\/', $this->MYREQUEST['SEARCH']).'/i';
            if (preg_match($this->MYREQUEST['SEARCH'], 'test') === false) {
                $output .= '<div class="error">Error: enter a valid regular expression as a search query.</div>';
                return $output;
            }
      }

      $output .=
            '<div class="info"><table width=\"100%\" cellspacing=0><tbody>' . 
            '<tr>' . 
            '<th>' . $this->sortheader('S' , $fieldheading ,   "&OB=".$this->MYREQUEST['OB']) . '</th>' . 
            '<th>' . $this->sortheader('H' , 'Hits' ,          "&OB=".$this->MYREQUEST['OB']) . '</th>' . 
            '<th>' . $this->sortheader('Z' , 'Size' ,          "&OB=".$this->MYREQUEST['OB']) . '</th>' . 
            '<th>' . $this->sortheader('A' , 'Last accessed' , "&OB=".$this->MYREQUEST['OB']) . '</th>' . 
            '<th>' . $this->sortheader('M' , 'Last modified' , "&OB=".$this->MYREQUEST['OB']) . '</th>' . 
            '<th>' . $this->sortheader('C' , 'Created at' ,    "&OB=".$this->MYREQUEST['OB']) . '</th>';

        if($fieldname=='info') {
            $cols+=2;
             $output .= '<th>' . $this->sortheader('T','Timeout',"&OB=".$this->MYREQUEST['OB']) . '</th>';
        }
        $output .= '<th>' . $this->sortheader('D','Deleted at',"&OB=".$this->MYREQUEST['OB']) . '</th></tr>';

        // builds list with alpha numeric sortable keys
        //
        $list = array();
        foreach($this->cache[$this->scope_list[$this->MYREQUEST['SCOPE']]] as $i => $entry) {
            switch($this->MYREQUEST['SORT1']) {
                case 'A': $k=sprintf('%015d-',$entry['access_time']); 	break;
                case 'H': $k=sprintf('%015d-',$entry['num_hits']); 		break;
                case 'Z': $k=sprintf('%015d-',$entry['mem_size']); 		break;
                case 'M': $k=sprintf('%015d-',$entry['mtime']);			break;
                case 'C': $k=sprintf('%015d-',$entry['creation_time']);	break;
                case 'T': $k=sprintf('%015d-',$entry['ttl']);			break;
                case 'D': $k=sprintf('%015d-',$entry['deletion_time']);	break;
                case 'S': $k='';										break;
            }
            $list[$k.$entry[$fieldname]]=$entry;
        }

        if ($list) {
            // sort list
            //
            switch ($this->MYREQUEST['SORT2']) {
                case "A":	krsort($list);	break;
                case "D":	ksort($list);	break;
            }
            
            // output list
            $i=0;
            foreach($list as $k => $entry) {
                if(!$this->MYREQUEST['SEARCH'] || preg_match($this->MYREQUEST['SEARCH'], $entry[$fieldname]) != 0) {  
                    $field_value = htmlentities(strip_tags($entry[$fieldname],''), ENT_QUOTES, 'UTF-8');
                    $output .=
                        '<tr class=tr-' . $i%2 . '>' . 
                        "<td class=td-0><a href=\"{$this->MY_SELF}&OB=" . $this->MYREQUEST['OB'] . "&SH=" . md5($entry[$fieldkey]) . "\">" . $field_value . '</a></td>' . 
                        '<td class="td-n center">' . $entry['num_hits'] . '</td>' . 
                        '<td class="td-n right">' . $entry['mem_size'] . '</td>' . 
                        '<td class="td-n center">' . date(DATE_FORMAT,$entry['access_time']) . '</td>' . 
                        '<td class="td-n center">' . date(DATE_FORMAT,$entry['mtime']) . '</td>' . 
                        '<td class="td-n center">' . date(DATE_FORMAT,$entry['creation_time']) . '</td>';

                    if($fieldname=='info') {
                        if($entry['ttl'])
                            $output .= '<td class="td-n center">'.$entry['ttl'].' seconds</td>';
                        else
                            $output .= '<td class="td-n center">None</td>';
                    }
                    if ($entry['deletion_time']) {
                        $output .= '<td class="td-last center">' .  date(DATE_FORMAT, $entry['deletion_time']) .  '</td>';
                    } else if ($this->MYREQUEST['OB'] == OB_USER_CACHE) {

                        $output .= '<td class="td-last center">';
                        $output .= '[<a href="' . $this->MY_SELF .  '&OB=' .  $this->MYREQUEST['OB'] .  '&DU=' .  urlencode($entry[$fieldkey]) .  '">Delete Now</a>]';
                        $output .= '</td>';
                    } else {
                        $output .= '<td class="td-last center"> &nbsp; </td>';
                    }
                    $output .= '</tr>';
                    $i++;
                    if ($i == $this->MYREQUEST['COUNT'])
                        break;
                }
            }
        } else {
            $output .= '<tr class=tr-0><td class="center" colspan=' .$cols .'><i>No data</i></td></tr>';
        }
        $output .= "</tbody></table>";

        if ($list && $i < count($list)) {
            $output .= "<br><a href=\"{$this->MY_SELF}&OB=" . $this->MYREQUEST['OB'] . "&COUNT=0\"><i>" . (count($list)-$i) . ' more available...</i></a>';
        }
        $output .= "</div>";
        return $output;
    }

    public function section_sys_cache_dir() {
        $output = '';

        $output .= "
            <div class=sorting><form>Scope:
            <input type=hidden name=OB value={$this->MYREQUEST['OB']}>
            <select name=SCOPE>";
        $output .= 
            "<option value=A" . ($this->MYREQUEST['SCOPE']=='A' ? " selected":"") . ">Active</option>" . 
            "<option value=D" . ($this->MYREQUEST['SCOPE']=='D' ? " selected":"") . ">Deleted</option>" . 
            "</select>" . 
            ", Sorting:<select name=SORT1>" . 
            "<option value=H" . ($this->MYREQUEST['SORT1']=='H' ? " selected":"") . ">Total Hits</option>" . 
            "<option value=Z" . ($this->MYREQUEST['SORT1']=='Z' ? " selected":"") . ">Total Size</option>" . 
            "<option value=T" . ($this->MYREQUEST['SORT1']=='T' ? " selected":"") . ">Number of Files</option>" . 
            "<option value=S" . ($this->MYREQUEST['SORT1']=='S' ? " selected":"") . ">Directory Name</option>" . 
            "<option value=A" . ($this->MYREQUEST['SORT1']=='A' ? " selected":"") . ">Avg. Size</option>" . 
            "<option value=C" . ($this->MYREQUEST['SORT1']=='C' ? " selected":"") . ">Avg. Hits</option>" . 
            '</select>' . 
            '<select name=SORT2>' . 
            '<option value=D' . ($this->MYREQUEST['SORT2']=='D' ? ' selected':'') . '>DESC</option>' . 
            '<option value=A' . ($this->MYREQUEST['SORT2']=='A' ? ' selected':'') . '>ASC</option>' . 
            '</select>' . 
            '<select name=COUNT onChange="form.submit()">' . 
            '<option value=10 ' . ($this->MYREQUEST['COUNT']=='10' ? ' selected':'') . '>Top 10</option>' . 
            '<option value=20 ' . ($this->MYREQUEST['COUNT']=='20' ? ' selected':'') . '>Top 20</option>' . 
            '<option value=50 ' . ($this->MYREQUEST['COUNT']=='50' ? ' selected':'') . '>Top 50</option>' . 
            '<option value=100' . ($this->MYREQUEST['COUNT']=='100'? ' selected':'') . '>Top 100</option>' . 
            '<option value=150' . ($this->MYREQUEST['COUNT']=='150'? ' selected':'') . '>Top 150</option>' . 
            '<option value=200' . ($this->MYREQUEST['COUNT']=='200'? ' selected':'') . '>Top 200</option>' . 
            '<option value=500' . ($this->MYREQUEST['COUNT']=='500'? ' selected':'') . '>Top 500</option>' . 
            '<option value=0  ' . ($this->MYREQUEST['COUNT']=='0'  ? ' selected':'') . '>All</option>' . 
            '</select>' . 
            ", Group By Dir Level:<select name=AGGR>" . 
            "<option value='' selected>None</option>";
            for ($i = 1; $i < 10; $i++)
                $output .= "<option value=$i" . ($this->MYREQUEST['AGGR']==$i ? " selected":"") . ">$i</option>";
            $output .= '</select>' . 
            '&nbsp;<input type=submit value="GO!">' . 
            '</form></div>' . 

            '<div class="info"><table width=\"100%\" cellspacing=0><tbody>' . 
            '<tr>' . 
            '<th>' . $this->sortheader('S','Directory Name',	"&OB=".$this->MYREQUEST['OB']) . '</th>' . 
            '<th>' . $this->sortheader('T','Number of Files',"&OB=".$this->MYREQUEST['OB']) . '</th>' . 
            '<th>' . $this->sortheader('H','Total Hits',	"&OB=".$this->MYREQUEST['OB']) . '</th>' . 
            '<th>' . $this->sortheader('Z','Total Size',	"&OB=".$this->MYREQUEST['OB']) . '</th>' . 
            '<th>' . $this->sortheader('C','Avg. Hits',	"&OB=".$this->MYREQUEST['OB']) . '</th>' . 
            '<th>' . $this->sortheader('A','Avg. Size',	"&OB=".$this->MYREQUEST['OB']) . '</th>' . 
            '</tr>';

        // builds list with alpha numeric sortable keys
        //
        $tmp = $list = array();
        foreach($this->cache[$this->scope_list[$this->MYREQUEST['SCOPE']]] as $entry) {
            $n = dirname($entry['filename']);
            if ($this->MYREQUEST['AGGR'] > 0) {
                $n = preg_replace("!^(/?(?:[^/\\\\]+[/\\\\]){".($this->MYREQUEST['AGGR']-1)."}[^/\\\\]*).*!", "$1", $n);
            }
            if (!isset($tmp[$n])) {
                $tmp[$n] = array('hits'=>0,'size'=>0,'ents'=>0);
            }
            $tmp[$n]['hits'] += $entry['num_hits'];
            $tmp[$n]['size'] += $entry['mem_size'];
            ++$tmp[$n]['ents'];
        }

        foreach ($tmp as $k => $v) {
            switch($this->MYREQUEST['SORT1']) {
                case 'A': $kn=sprintf('%015d-',$v['size'] / $v['ents']);break;
                case 'T': $kn=sprintf('%015d-',$v['ents']);		break;
                case 'H': $kn=sprintf('%015d-',$v['hits']);		break;
                case 'Z': $kn=sprintf('%015d-',$v['size']);		break;
                case 'C': $kn=sprintf('%015d-',$v['hits'] / $v['ents']);break;
                case 'S': $kn = $k;					break;
            }
            $list[$kn.$k] = array($k, $v['ents'], $v['hits'], $v['size']);
        }

        if ($list) {
            
            // sort list
            //
            switch ($this->MYREQUEST['SORT2']) {
                case "A":	krsort($list);	break;
                case "D":	ksort($list);	break;
            }
            
            // output list
            $i = 0;
            foreach($list as $entry) {
                $output .=
                    '<tr class=tr-' . ($i%2) . '>' . 
                    "<td class=td-0>" . $entry[0] . '</a></td>' . 
                    '<td class="td-n center">' . $entry[1] . '</td>' . 
                    '<td class="td-n center">' . $entry[2] . '</td>' . 
                    '<td class="td-n center">' . $entry[3] . '</td>' . 
                    '<td class="td-n center">' . round($entry[2] / $entry[1]) . '</td>' . 
                    '<td class="td-n center">' . round($entry[3] / $entry[1]) . '</td>' . 
                    '</tr>';

                if (++$i == $this->MYREQUEST['COUNT']) break;
            }
            
        } else {
            $output .= '<tr class=tr-0><td class="center" colspan=6><i>No data</i></td></tr>';
        }
        $output .= "</tbody></table>";

        if ($list && $i < count($list)) {
            $output .= "<br><a href=\"{$this->MY_SELF}&OB=" . $this->MYREQUEST['OB'] . "&COUNT=0\"><i>" . (count($list)-$i) . ' more available...</i></a>';
        }

        $output .= "</div>";
        return $output;
    }

    public function section_version_check() {
        $output = '';

        $url = parse_url($_SERVER['REQUEST_URI']);
        $path = explode('/', $url['path']);
        $isDrupal = ($path[count($path)-1] == 'full') ? false: true;

        $output .= '
            <div class="info"><h2>APC Version Information</h2>
            <table width=\"100%\" cellspacing=0><tbody>
            <tr>
            <th></th>
            </tr>';

      if (defined('PROXY')) {
        $ctxt = stream_context_create( array( 'http' => array( 'proxy' => PROXY, 'request_fulluri' => True ) ) );
        $rss = @file_get_contents("http://pecl.php.net/feeds/pkg_apc.rss", False, $ctxt);
      } else {
        $rss = @file_get_contents("http://pecl.php.net/feeds/pkg_apc.rss");
      }
        if (!$rss) {
            $output .= '<tr class="td-last center"><td>Unable to fetch version information.</td></tr>';
        } else {
            $apcversion = phpversion('apc');

            preg_match('!<title>APC ([0-9.]+)</title>!', $rss, $match);
            $output .= '<tr class="tr-0 center"><td>';
            if (version_compare($apcversion, $match[1], '>=')) {
                $msg = 'You are running the latest version of APC ('.$apcversion.')';
                $output .= "<div class='ok'>$msg</div>";
                if($isDrupal)
                    drupal_set_message($msg);
                $i = 3;
            } else {
                $msg = 'You are running an older version of APC ('.$apcversion.'), 
                    newer version '.$match[1].' is available at <a href="http://pecl.php.net/package/APC/'.$match[1].'">
                    http://pecl.php.net/package/APC/'.$match[1].'</a>';
                    $output .= "<div class='failed'>$msg</div>";
                $output .= $msg;
                if($isDrupal)
                    drupal_set_message($msg, 'error');
                $i = -1;
            }
            $output .= '</td></tr>';
            $output .= '<tr class="tr-0"><td><h3>Change Log:</h3><br/>';

            preg_match_all('!<(title|description)>([^<]+)</\\1>!', $rss, $match);
            next($match[2]); next($match[2]);

            while (list(,$v) = each($match[2])) {
                list(,$ver) = explode(' ', $v, 2);
                if ($i < 0 && version_compare($apcversion, $ver, '>=')) {
                    break;
                } else if (!$i--) {
                    break;
                }
                $output .= "<b><a href=\"http://pecl.php.net/package/APC/$ver\">".htmlspecialchars($v)."</a></b><br><blockquote>";
                $output .= nl2br(htmlspecialchars(current($match[2])))."</blockquote>";
                next($match[2]);
            }
            $output .= '</td></tr>';
        }
        $output .= '
            </tbody></table>
            </div>';

        return $output;
    }

    public function full_display() {
        // Display HTML
        require_once 'header.inc';

        // Display main Menu
        echo '<ol class=menu>'
            . "<li><a href=\"{$this->MY_SELF}&OB={$this->MYREQUEST['OB']}&SH={$this->MYREQUEST['SH']}\">Refresh Data</a></li>"
            . $this->menu_entry(1,'View Host Stats')
            . $this->menu_entry(2,'System Cache Entries')
            . $this->menu_entry(3,'User Cache Entries')
            . $this->menu_entry(9,'Version Check')
            . "<li><a class='aright' href='{$this->MY_SELF}&CC=1&OB={$this->MYREQUEST['OB']}' onClick='javascipt:return confirm(\"Are you sure?\");'>Clear $this->cache_mode Cache</a></li>"
            . '</ol>'
            . '<div class=content>';

        echo $this->get_section($this->MYREQUEST['OB']);
        echo '</div>';

        require_once 'footer.inc';
    }
}

