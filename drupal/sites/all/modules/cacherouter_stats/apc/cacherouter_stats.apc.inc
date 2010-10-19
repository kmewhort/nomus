<?php

require_once 'apc_constants.inc';

function cacherouter_stats_apc_admin()
{
    return '<br><br><br>' . l('Full page', 'admin/cacherouter_stats/apc/full')
           . '<br><br><br>' . drupal_get_form('cacherouter_stats_apc_clear_cache');
}

function cacherouter_stats_apc_clear_cache()
{
    $form = array();
    $form['clear'] = array(
            '#type' => 'fieldset',
            '#title' => t('Clear cache'),
            );
    $form['clear']['types'] = array(
            '#type' => 'checkboxes',
            '#title' => t('Cache type'),
            '#required' => TRUE,
            '#options' => array('cache_user' => 'User', 'cache_system' => 'System', 'cache_opcode' => 'op-code'),
            );
    $form['clear']['submit'] = array('#type' => 'submit', '#value' => 'Clear op-code cache');
    return $form;
}

function cacherouter_stats_apc_clear_cache_submit(&$form, &$form_state)
{
    foreach($form_state['values']['types'] as $type => $value) {
        if ($value !== 0)
        {
            switch ($type) {
                case 'cache_user':
                    apc_clear_cache('user');
                    drupal_set_message('User cache cleared');
                    break;
                case 'cache_system':
                    apc_clear_cache('system');
                    drupal_set_message('System cache cleared');
                    break;
                case 'cache_opcode':
                    apc_clear_cache('opcode');
                    drupal_set_message('Op-code cache cleared');
                    break;
            }
        }
    }
}

function cacherouter_stats_apc_display($arg = NULL)
{
    require_once 'apc.inc';

    if ($arg === 'full') {
        $gui = new apc_gui();
        $gui->full_display();
        exit;
    }
    else if (isset($arg)) {
        $gui = new apc_gui();
        return $gui->get_section($arg);
    }
}

function cacherouter_stats_apc_host()
{
    return cacherouter_stats_apc_display(OB_HOST_STATS);
}

function cacherouter_stats_apc_system()
{
    return cacherouter_stats_apc_display(OB_SYS_CACHE);
}

function cacherouter_stats_apc_user()
{
    return cacherouter_stats_apc_display(OB_USER_CACHE);
}

function cacherouter_stats_apc_version()
{
    return cacherouter_stats_apc_display(OB_VERSION_CHECK);
}

function cacherouter_stats_apc_full()
{
    return cacherouter_stats_apc_display('full');
}

