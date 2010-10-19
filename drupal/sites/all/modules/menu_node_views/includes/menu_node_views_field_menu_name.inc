<?php

// $Id: menu_node_views_field_menu_name.inc,v 1.1 2009/06/22 15:35:13 agentken Exp $

/**
 * @file Views field handler for the name of a menu.
 */
class menu_node_views_field_menu_name extends views_handler_field {

  function render($values) {
    static $names;
    if (!isset($names)) {
      $names = menu_get_menus();
    }
    $value = $names[$values->{$this->field_alias}];
    return check_plain($value);
  }

}

