<?php
/*
  +----------------------------------------------------------------------+
  | PHP Version 5                                                        |
  +----------------------------------------------------------------------+
  | Copyright (c) 1997-2004 The PHP Group                                |
  +----------------------------------------------------------------------+
  | This source file is subject to version 3.0 of the PHP license,       |
  | that is bundled with this package in the file LICENSE, and is        |
  | available through the world-wide-web at the following url:           |
  | http://www.php.net/license/3_0.txt.                                  |
  | If you did not receive a copy of the PHP license and are unable to   |
  | obtain it through the world-wide-web, please send a note to          |
  | license@php.net so we can mail you a copy immediately.               |
  +----------------------------------------------------------------------+
  | Author:  Harun Yayli <harunyayli at gmail.com>                       |
  +----------------------------------------------------------------------+
*/

class memcache_gui
{
    public function __construct($servers)
    {
        $this->MEMCACHE_SERVERS = $servers;

        $_GET['op'] = !isset($_GET['op'])? '1':$_GET['op'];
        $this->PHP_SELF= url($_GET['q']);
        $this->time = time();
        // sanitize _GET

        foreach($_GET as $key=>$g){
            $_GET[$key]=htmlentities($g);
        }

        // singleout
        // when singleout is set, it only gives details for that server.
        if (isset($_GET['singleout']) && $_GET['singleout']>=0 && $_GET['singleout'] <count($this->MEMCACHE_SERVERS)) {
            $this->MEMCACHE_SERVERS = array($this->MEMCACHE_SERVERS[$_GET['singleout']]);
        }

        // display images
        if (isset($_GET['IMG']))
            $this->display_image();
    }

    ///////////MEMCACHE FUNCTIONS /////////////////////////////////////////////////////////////////////

    public function sendMemcacheCommands($command){
        $this->MEMCACHE_SERVERS;
        $result = array();

        foreach($this->MEMCACHE_SERVERS as $server){
            $strs = explode(':',$server);
            $host = $strs[0];
            $port = $strs[1];
            $result[$server] = $this->sendMemcacheCommand($host,$port,$command);
        }
        return $result;
    }

    public static function sendMemcacheCommand($server,$port,$command){

        $s = @fsockopen($server,$port);
        if (!$s){
            die("Cant connect to:".$server.':'.$port);
        }

        fwrite($s, $command."\r\n");

        $buf='';
        while ((!feof($s))) {
            $buf .= fgets($s, 256);
            if (strpos($buf,"END\r\n")!==false){ // stat says end
                break;
            }
            if (strpos($buf,"DELETED\r\n")!==false || strpos($buf,"NOT_FOUND\r\n")!==false){ // delete says these
                break;
            }
            if (strpos($buf,"OK\r\n")!==false){ // flush_all says ok
                break;
            }
        }
        fclose($s);
        return memcache_gui::parseMemcacheResults($buf);
    }

    public static function parseMemcacheResults($str){
        $res = array();
        $lines = explode("\r\n",$str);
        $cnt = count($lines);
        for($i=0; $i< $cnt; $i++){
            $line = $lines[$i];
            $l = explode(' ',$line,3);
            if (count($l)==3){
                $res[$l[0]][$l[1]]=$l[2];
                if ($l[0]=='VALUE'){ // next line is the value
                    $res[$l[0]][$l[1]] = array();
                    list ($flag,$size)=explode(' ',$l[2]);
                    $res[$l[0]][$l[1]]['stat']=array('flag'=>$flag,'size'=>$size);
                    $res[$l[0]][$l[1]]['value']=$lines[++$i];
                }
            }elseif($line=='DELETED' || $line=='NOT_FOUND' || $line=='OK'){
                return $line;
            }
        }
        return $res;
    }

    public static function dumpCacheSlab($server,$slabId,$limit){
        list($host,$port) = explode(':',$server);
        $resp = memcache_gui::sendMemcacheCommand($host,$port,'stats cachedump '.$slabId.' '.$limit);

       return $resp;

    }

    public static function flushServer($server){
        list($host,$port) = explode(':',$server);
        $resp = memcache_gui::sendMemcacheCommand($host,$port,'flush_all');
        return $resp;
    }

    public function getCacheItems(){
     $items = $this->sendMemcacheCommands('stats items');
     $serverItems = array();
     $totalItems = array();
     foreach ($items as $server=>$itemlist){
        $serverItems[$server] = array();
        $totalItems[$server]=0;
        if (!isset($itemlist['STAT'])){
            continue;
        }

        $iteminfo = $itemlist['STAT'];

        foreach($iteminfo as $keyinfo=>$value){
            if (preg_match('/items\:(\d+?)\:(.+?)$/',$keyinfo,$matches)){
                $serverItems[$server][$matches[1]][$matches[2]] = $value;
                if ($matches[2]=='number'){
                    $totalItems[$server] +=$value;
                }
            }
        }
     }

     return array('items'=>$serverItems,'counts'=>$totalItems);
    }

    public function getMemcacheStats($total=true){
        $resp = $this->sendMemcacheCommands('stats');

        if ($total){
            $res = array();
            foreach($resp as $server=>$r){
                foreach($r['STAT'] as $key=>$row){
                    if (!isset($res[$key])){
                        $res[$key]=null;
                    }
                    switch ($key){
                        case 'pid':
                            $res['pid'][$server]=$row;
                            break;
                        case 'uptime':
                            $res['uptime'][$server]=$row;
                            break;
                        case 'time':
                            $res['time'][$server]=$row;
                            break;
                        case 'version':
                            $res['version'][$server]=$row;
                            break;
                        case 'pointer_size':
                            $res['pointer_size'][$server]=$row;
                            break;
                        case 'rusage_user':
                            $res['rusage_user'][$server]=$row;
                            break;
                        case 'rusage_system':
                            $res['rusage_system'][$server]=$row;
                            break;
                        case 'curr_items':
                            $res['curr_items']+=$row;
                            break;
                        case 'total_items':
                            $res['total_items']+=$row;
                            break;
                        case 'bytes':
                            $res['bytes']+=$row;
                            break;
                        case 'curr_connections':
                            $res['curr_connections']+=$row;
                            break;
                        case 'total_connections':
                            $res['total_connections']+=$row;
                            break;
                        case 'connection_structures':
                            $res['connection_structures']+=$row;
                            break;
                        case 'cmd_get':
                            $res['cmd_get']+=$row;
                            break;
                        case 'cmd_set':
                            $res['cmd_set']+=$row;
                            break;
                        case 'get_hits':
                            $res['get_hits']+=$row;
                            break;
                        case 'get_misses':
                            $res['get_misses']+=$row;
                            break;
                        case 'evictions':
                            $res['evictions']+=$row;
                            break;
                        case 'bytes_read':
                            $res['bytes_read']+=$row;
                            break;
                        case 'bytes_written':
                            $res['bytes_written']+=$row;
                            break;
                        case 'limit_maxbytes':
                            $res['limit_maxbytes']+=$row;
                            break;
                        case 'threads':
                            $res['rusage_system'][$server]=$row;
                            break;
                    }
                }
            }
            return $res;
        }
        return $resp;
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

    public function bsize($s) {
        foreach (array('','K','M','G') as $i => $k) {
            if ($s < 1024) break;
            $s/=1024;
        }
        return sprintf("%5.1f %sBytes",$s,$k);
    }

    // create menu entry
    public function menu_entry($ob,$title) {
        if ($ob==$_GET['op']){
            return "<li><a class=\"child_active\" href=\"{$this->PHP_SELF}&op=$ob\">$title</a></li>";
        }
        return "<li><a class=\"active\" href=\"{$this->PHP_SELF}&op=$ob\">$title</a></li>";
    }

    public function printHeader() {
        include 'header.inc';
    }

    public function printFooter(){
        include 'footer.inc';
    }

    public function printMenu(){
        echo "<ol class=menu>";
        if ($_GET['op']!=4) {
            echo "<li><a href=\"{$this->PHP_SELF}&op={$_GET['op']}\">Refresh Data</a></li>";
        }
        else {
            echo "<li><a href=\"{$this->PHP_SELF}&op=2\">Back</a></li>";
        }
        echo
            $this->menu_entry(1,'View Host Stats'),
            $this->menu_entry(2,'Variables');

        echo ' </ol> <br/>';
    }

    public function display_image() {
        $memcacheStats = $this->getMemcacheStats();
        $memcacheStatsSingle = $this->getMemcacheStats(false);

        if (!$this->graphics_avail()) {
            exit(0);
        }

        $size = GRAPH_SIZE; // image size
        $image = imagecreate($size+50, $size+10);

        $col_white = imagecolorallocate($image, 0xFF, 0xFF, 0xFF);
        $col_red   = imagecolorallocate($image, 0xD0, 0x60,  0x30);
        $col_green = imagecolorallocate($image, 0x60, 0xF0, 0x60);
        $this->col_black = imagecolorallocate($image,   0,   0,   0);

        imagecolortransparent($image,$col_white);

        switch ($_GET['IMG']){
            case 1: // pie chart
                $tsize=$memcacheStats['limit_maxbytes'];
                $avail=$tsize-$memcacheStats['bytes'];
                $x=$y=$size/2;
                $angle_from = 0;
                $fuzz = 0.000001;

                foreach($memcacheStatsSingle as $serv=>$mcs) {
                    $free = $mcs['STAT']['limit_maxbytes']-$mcs['STAT']['bytes'];
                    $used = $mcs['STAT']['bytes'];


                    if ($free>0){
                        // draw free
                        $angle_to = ($free*360)/$tsize;
                        $perc =sprintf("%.2f%%", ($free *100) / $tsize) ;

                        $this->fill_arc($image,$x,$y,$size,$angle_from,$angle_from + $angle_to ,$this->col_black,$col_green,$perc);
                        $angle_from = $angle_from + $angle_to ;
                    }
                    if ($used>0){
                        // draw used
                        $angle_to = ($used*360)/$tsize;
                        $perc =sprintf("%.2f%%", ($used *100) / $tsize) ;
                        $this->fill_arc($image,$x,$y,$size,$angle_from,$angle_from + $angle_to ,$this->col_black,$col_red, '('.$perc.')' );
                        $angle_from = $angle_from+ $angle_to ;
                    }
                }

                break;

            case 2: // hit miss

                $hits = ($memcacheStats['get_hits']==0) ? 1:$memcacheStats['get_hits'];
                $misses = ($memcacheStats['get_misses']==0) ? 1:$memcacheStats['get_misses'];
                $total = $hits + $misses ;

                $this->fill_box($image, 30,$size,50,-$hits*($size-21)/$total,$this->col_black,$col_green,sprintf("%.1f%%",$hits*100/$total));
                $this->fill_box($image,130,$size,50,-max(4,($total-$hits)*($size-21)/$total),$this->col_black,$col_red,sprintf("%.1f%%",$misses*100/$total));
                break;

        }

        header("Content-type: image/png");
        imagepng($image);
        exit;
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

    public function section_host_stats()
    {
        $output = '';

        $phpversion = phpversion();
        $memcacheStats = $this->getMemcacheStats();
        $memcacheStatsSingle = $this->getMemcacheStats(false);

        $mem_size = $memcacheStats['limit_maxbytes'];
        $mem_used = $memcacheStats['bytes'];
        $mem_avail= $mem_size-$mem_used;
        $startTime = time()-array_sum($memcacheStats['uptime']);

        $curr_items = $memcacheStats['curr_items'];
        $total_items = $memcacheStats['total_items'];
        $hits = ($memcacheStats['get_hits']==0) ? 1:$memcacheStats['get_hits'];
        $misses = ($memcacheStats['get_misses']==0) ? 1:$memcacheStats['get_misses'];
        $sets = $memcacheStats['cmd_set'];

        $req_rate = sprintf("%.2f",($hits+$misses)/($this->time-$startTime));
        $hit_rate = sprintf("%.2f",($hits)/($this->time-$startTime));
        $miss_rate = sprintf("%.2f",($misses)/($this->time-$startTime));
        $set_rate = sprintf("%.2f",($sets)/($this->time-$startTime));

        $output .= "<div class=\"info div1\"><h2>General Cache Information</h2>
            <table cellspacing=0><tbody>
            <tr class=tr-1><td class=td-0>PHP Version</td><td>$phpversion</td></tr>";
        $output .= "<tr class=tr-0><td class=td-0>Memcached Host". ((count($this->MEMCACHE_SERVERS)>1) ? 's':'')."</td><td>";
        $i=0;
        if (!isset($_GET['singleout']) && count($this->MEMCACHE_SERVERS)>1){
            foreach($this->MEMCACHE_SERVERS as $server){
                $output .= ($i+1).'. <a href="'.$this->PHP_SELF.'&singleout='.$i++.'">'.$server.'</a><br/>';
            }
        }
        else{
            $output .= '1.'.$this->MEMCACHE_SERVERS[0];
        }
        if (isset($_GET['singleout'])){
            $output .= '<a href="'.$this->PHP_SELF.'">(all servers)</a><br/>';
        }
        $output .= "</td></tr>\n";
        $output .= "<tr class=tr-1><td class=td-0>Total Memcache Cache</td><td>".$this->bsize($memcacheStats['limit_maxbytes'])."</td></tr>\n";

        $output .= '</tbody></table>
            </div>

            <div class="info div1"><h2>Memcache Server Information</h2>';

        foreach($this->MEMCACHE_SERVERS as $server){
            $output .= '<table cellspacing=0><tbody>';
            $output .= '<tr class=tr-1><td class=td-1>'.$server.'</td><td><a href="'.$this->PHP_SELF.'&server='.array_search($server,$this->MEMCACHE_SERVERS).'&op=6">[<b>Flush this server</b>]</a></td></tr>';
            $output .= '<tr class=tr-0><td class=td-0>Start Time</td><td>' . date(DATE_FORMAT,$memcacheStatsSingle[$server]['STAT']['time']-$memcacheStatsSingle[$server]['STAT']['uptime']) . '</td></tr>';
            $output .= '<tr class=tr-1><td class=td-0>Uptime</td><td>' . $this->duration($memcacheStatsSingle[$server]['STAT']['time']-$memcacheStatsSingle[$server]['STAT']['uptime']) . '</td></tr>';
            $output .= '<tr class=tr-0><td class=td-0>Memcached Server Version</td><td>'.$memcacheStatsSingle[$server]['STAT']['version'].'</td></tr>';
            $output .= '<tr class=tr-1><td class=td-0>Used Cache Size</td><td>' . $this->bsize($memcacheStatsSingle[$server]['STAT']['bytes']) . '</td></tr>';
            $output .= '<tr class=tr-0><td class=td-0>Total Cache Size</td><td>' . $this->bsize($memcacheStatsSingle[$server]['STAT']['limit_maxbytes']) . '</td></tr>';
            $output .= '</tbody></table>';
        }
        $output .= '</div>
            <div class="graph div3"><h2>Host Status Diagrams</h2>
            <table cellspacing=0><tbody>';

        $size='width='.(GRAPH_SIZE+50).' height='.(GRAPH_SIZE+10);
        $output .= '<tr>
            <td class=td-0>Cache Usage</td>
            <td class=td-1>Hits &amp; Misses</td>
            </tr>';

        $output .=
            $this->graphics_avail() ?
            '<tr>'.
            "<td class=td-0><img alt=\"\" $size src=\"$this->PHP_SELF&IMG=1&".(isset($_GET['singleout'])? 'singleout='.$_GET['singleout'].'&':'')."$this->time\"></td>".
            "<td class=td-1><img alt=\"\" $size src=\"$this->PHP_SELF&IMG=2&".(isset($_GET['singleout'])? 'singleout='.$_GET['singleout'].'&':'')."$this->time\"></td></tr>\n"
            : "" . 
            '<tr>' . 
            '<td class=td-0><span class="green box">&nbsp;</span>Free: ' . $this->bsize($mem_avail).sprintf(" (%.1f%%)",$mem_avail*100/$mem_size) . "</td>\n" . 
            '<td class=td-1><span class="green box">&nbsp;</span>Hits: ' . $hits.sprintf(" (%.1f%%)",$hits*100/($hits+$misses)) . "</td>\n" . 
            '</tr>' . 
            '<tr>' . 
            '<td class=td-0><span class="red box">&nbsp;</span>Used: ' . $this->bsize($mem_used ).sprintf(" (%.1f%%)",$mem_used *100/$mem_size) . "</td>\n" . 
            '<td class=td-1><span class="red box">&nbsp;</span>Misses: ' . $misses.sprintf(" (%.1f%%)",$misses*100/($hits+$misses)) . "</td>\n";
        $output .= "
            </tr>
            </tbody></table>
            <br/>
            <div class=\"info\"><h2>Cache Information</h2>
            <table cellspacing=0><tbody>
            <tr class=tr-0><td class=td-0>Current Items(total)</td><td>$curr_items ($total_items)</td></tr>
            <tr class=tr-1><td class=td-0>Hits</td><td>{$hits}</td></tr>
            <tr class=tr-0><td class=td-0>Misses</td><td>{$misses}</td></tr>
            <tr class=tr-1><td class=td-0>Request Rate (hits, misses)</td><td>$req_rate cache requests/second</td></tr>
            <tr class=tr-0><td class=td-0>Hit Rate</td><td>$hit_rate cache requests/second</td></tr>
            <tr class=tr-1><td class=td-0>Miss Rate</td><td>$miss_rate cache requests/second</td></tr>
            <tr class=tr-0><td class=td-0>Set Rate</td><td>$set_rate cache requests/second</td></tr>
            </tbody></table>
            </div>";

        return $output;
    }

    public function section_variables() {
        $output = '';

        $m=0;
        $cacheItems= $this->getCacheItems();
        $items = $cacheItems['items'];
        $totals = $cacheItems['counts'];
        $maxDump = MAX_ITEM_DUMP;
        foreach($items as $server => $entries) {

            $output .= "<div class=\"info\"><table cellspacing=0><tbody>
                <tr><th colspan=\"2\">$server</th></tr>
                <tr><th>Slab Id</th><th>Info</th></tr>";

            foreach($entries as $slabId => $slab) {
                $dumpUrl = $this->PHP_SELF.'&op=2&server='.(array_search($server,$this->MEMCACHE_SERVERS)).'&dumpslab='.$slabId;
                $output .=
                    "<tr class=tr-$m>" . 
                    "<td class=td-0><center>" . '<a href="' . $dumpUrl . '">' . $slabId . '</a>' . "</center></td>" . 
                    "<td class=td-last><b>Item count:</b> " . $slab['number'] . '<br/><b>Age:</b>' . $this->duration($this->time-$slab['age']) . '<br/> <b>Evicted:</b>' . ((isset($slab['evicted']) && $slab['evicted']==1)? 'Yes':'No');
                if ((isset($_GET['dumpslab']) && $_GET['dumpslab']==$slabId) &&  (isset($_GET['server']) && $_GET['server']==array_search($server,$this->MEMCACHE_SERVERS))){
                    $output .= "<br/><b>Items: item</b><br/>";
                    $items = $this->dumpCacheSlab($server,$slabId,$slab['number']);
                    // maybe someone likes to do a pagination here :)
                    $i=1;
                    foreach($items['ITEM'] as $itemKey=>$itemInfo){
                        $itemInfo = trim($itemInfo,'[ ]');


                        $output .= '<a href="' . $this->PHP_SELF . '&op=4&server=' . (array_search($server,$this->MEMCACHE_SERVERS)) . '&key=' . base64_encode($itemKey).'">' . $itemKey . '</a>';
                        if ($i++ % 10 == 0) {
                            $output .= '<br/>';
                        }
                        elseif ($i!=$slab['number']+1){
                            $output .= ',';
                        }
                    }
                }

                $output .= "</td></tr>";
                $m=1-$m;
            }
            $output .= '
                </tbody></table>
                </div><hr/>';
        }

        return $output;
    }

    public function section_item_dump()
    {
        if (!isset($_GET['key']) || !isset($_GET['server'])) {
            return "No key set!";
        }

        $output = '';

        // I'm not doing anything to check the validity of the key string.
        // probably an exploit can be written to delete all the files in key=base64_encode("\n\r delete all").
        // somebody has to do a fix to this.
        $theKey = htmlentities(base64_decode($_GET['key']));

        $theserver = $this->MEMCACHE_SERVERS[(int)$_GET['server']];
        list($h,$p) = explode(':',$theserver);
        $r = $this->sendMemcacheCommand($h,$p,'get '.$theKey);
        $output .= '
            <div class="info"><table cellspacing=0><tbody>
            <tr><th>Server<th>Key</th><th>Value</th><th>Delete</th></tr>';
        $output .= "<tr><td class=td-0>" . $theserver . "</td><td class=td-0>" . $theKey . 
             " <br/>flag:" . $r['VALUE'][$theKey]['stat']['flag'] . 
             " <br/>Size:" . $this->bsize($r['VALUE'][$theKey]['stat']['size']) . 
             "</td><td>" . chunk_split($r['VALUE'][$theKey]['value'],40) . "</td>" . 
             '<td><a href="' . $this->PHP_SELF . '&op=5&server=' . (int)$_GET['server'] . '&key=' . base64_encode($theKey) . "\">Delete</a></td>" . "</tr>";
        $output .= '
            </tbody></table>
            </div><hr/>';
    }

    public function section_item_delete() {
        if (!isset($_GET['key']) || !isset($_GET['server'])){
            return "No key set!";
        }

        $theKey = htmlentities(base64_decode($_GET['key']));
        $theserver = $this->MEMCACHE_SERVERS[(int)$_GET['server']];
        list($h,$p) = explode(':',$theserver);
        $r = $this->sendMemcacheCommand($h,$p,'delete '.$theKey);

        return 'Deleting '.$theKey.':'.$r;
    }

    public function section_flush() {
        $theserver = $this->MEMCACHE_SERVERS[(int)$_GET['server']];
        $r = $this->flushServer($theserver);

        return 'Flush  '.$theserver.":".$r;
    }

    public function full_display()
    {
        $this->printHeader();
        $this->printMenu();

        switch ($_GET['op']) {

            case 1: // host stats
                echo $this->section_host_stats();
                break;

            case 2: // variables
                echo $this->section_variables();
                break;

            case 4: //item dump
                echo $this->section_item_dump();
                break;

            case 5: // item delete
                echo $this->section_item_dump();
                break;
            
           case 6: // flush server
                echo $this->section_flush();
                break;
        }
        $this->printFooter();
    }
}

?>
