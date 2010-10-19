<?php
// $Id$

/**
 * @file
 * Outputs contents of nodes
 *
 * @see template_preprocess_node(), preprocess/preprocess-node.inc
 * http://api.drupal.org/api/function/template_preprocess_node/6
 */
drupal_add_css(drupal_get_path('theme', 'sky') .'/node-case.css');
drupal_add_js(drupal_get_path('theme', 'sky') .'/scripts/doclookup.js');
?>
<div<?php print $node_attributes; ?>>
  <div class="content clearfix">
    <?php print $content; ?>
  </div>
</div>