<?php

require_once 'memcache_constants.inc';

function cacherouter_stats_memcache_admin()
{
    return '<br><br><br>' . l('Full page', 'admin/cacherouter_stats/memcache/full')
           . '<br><br><br>' . drupal_get_form('cacherouter_stats_memcache_clear_cache');
}

function cacherouter_stats_memcache_clear_cache()
{
    global $conf;

    $form = array();
    $form['clear'] = array(
            '#type' => 'fieldset',
            '#title' => t('Clear cache'),
            );
    $form['clear']['types'] = array(
            '#type' => 'checkboxes',
            '#title' => t('Server'),
            '#required' => TRUE,
            '#options' => drupal_map_assoc($conf['cacherouter']['default']['servers']),
            );
    $form['clear']['submit'] = array('#type' => 'submit', '#value' => 'Clear cache');
    return $form;
}

function cacherouter_stats_memcache_clear_cache_submit(&$form, &$form_state)
{
    require_once 'memcache.inc';

    global $conf;
    $servers = $conf['cacherouter']['default']['servers'];

    foreach($form_state['values']['types'] as $type => $value) {
        if ($value !== 0)
        {
            $e = memcache_gui::flushServer($type);
            if ($e === 'OK')
                drupal_set_message('Cleared cache for server ' . $type);
            else
                drupal_set_message('Server ' . $type . ' responded with: ' . $e, 'error');
        }
    }
}

function cacherouter_stats_memcache_display($arg = NULL)
{
    require_once 'memcache.inc';

    global $conf;
    $servers = $conf['cacherouter']['default']['servers'];

    // don't cache this page
    header("Cache-Control: no-store, no-cache, must-revalidate");  // HTTP/1.1
    header("Cache-Control: post-check=0, pre-check=0", false);
    header("Pragma: no-cache");                                    // HTTP/1.0

    if ($arg === 'full') {
        $gui = new memcache_gui($servers);
        $gui->full_display();
        exit;
    }
    switch ($arg) {
        case 'host':
            $gui = new memcache_gui($servers);
            return $gui->section_host_stats();
        case 'variables':
            $gui = new memcache_gui($servers);
            return $gui->section_variables();
    }
}

function cacherouter_stats_memcache_full()
{
    return cacherouter_stats_memcache_display('full');
}

function cacherouter_stats_memcache_host()
{
    return cacherouter_stats_memcache_display('host');
}


function cacherouter_stats_memcache_variables()
{
    return cacherouter_stats_memcache_display('variables');
}

function cacherouter_stats_memcache_servers()
{
    $head = array(
            array('data' => t('Server'), 'sort' => 'asc'),
            array('data' => t('Port')),
            array('data' => 'Operations'),
            );
    $rows = array();

    global $conf;
    $servers = $conf['cacherouter']['default']['servers'];
    foreach($servers as $server)
    {
        list($ip, $port) = explode(":", $server);
        $rows[] = array(
            array('data' => $ip, 'width' => 200),
            array('data' => $port),
            array('data' => '...'),
        );
    }

    $output .= theme('table', $head, $rows);
    return $output;

}

