<?php
// $Id: corresponding_node_references.crud.inc,v 1.1 2010/05/07 11:09:27 domidc Exp $

/**
 * @file
 * Include file providing corresponding node reference insert, update, and delete handling.
 */

/**
 * Add any corresponding references on node insertion.
 *
 * @param $home_node the node object catched in the node api on this node a reference is made to the away node
 * @param $home_field the field on the home node used to make the reference
 * @param $away_node_type the type of the away node
 * @param $away_field the name of the field on the away node referencing to the home node
 */
function corresponding_node_references_insert($home_node, $home_field, $away_node_type, $away_field) {
  $old_node = node_load($home_node->nid);

  // Determine the nodereference values after the insert.
  if (isset($home_node->$home_field) && is_array($home_node->$home_field)) {
    foreach ($home_node->$home_field as $reference) {
      if (!empty($reference['nid'])) {
        // Load the referenced node if it is of the specified away type.
        if ($referenced_node = node_load(array('nid' => $reference['nid'], 'type' => $away_node_type))) {
          // Add the new reference.
          $referenced_node->{$away_field}[] = array('nid' => $home_node->nid);
          _corresponding_node_references_update($referenced_node);
        }
      }
    }
  }
}

/**
 * Change corresponding references on node updating.
 *
 * Corresponding changes are made for any references removed or added.
 *
 * @param $home_node the node object catched in the node api on this node a reference is made to the away node
 * @param $home_field the field on the home node used to make the reference
 * @param $away_node_type the type of the away node
 * @param $away_field the name of the field on the away node referencing to the home node
 */
function corresponding_node_references_update($home_node, $home_field, $away_node_type, $away_field) {
  $old_node = node_load($home_node->nid);
  $old = $new = array();

  if ($home_node->$home_field != $old_node->$home_field) {
    // Determine the nodereference values before the update.
    if (isset($old_node->$home_field) && is_array($old_node->$home_field)) {
      foreach ($old_node->$home_field as $reference) {
        if (!empty($reference['nid'])) {
          $old[] = $reference['nid'];
        }
      }
    }
    // Determine the nodereference values after the update.
    if (isset($home_node->$home_field) && is_array($home_node->$home_field)) {
      foreach ($home_node->$home_field as $reference) {
        if (!empty($reference['nid'])) {
          $new[] = $reference['nid'];
        }
      }
    }
    // Handle removed references.
    if ($removed = array_diff($old, $new)) {
      foreach ($removed as $nid) {
        // Load the referenced node if it is of the specified away type.
        if ($referenced_node = node_load(array('nid' => $nid, 'type' => $away_node_type))) {
          if (isset($referenced_node->$away_field) && is_array($referenced_node->$away_field)) {
            // Iterate through the away node's references.
            foreach ($referenced_node->$away_field as $key => $value) {
              // Remove references to the deleted node.
              if ($value['nid'] && $value['nid'] == $home_node->nid) {
                unset($referenced_node->{$away_field}[$key]);
                _corresponding_node_references_update($referenced_node);
                break;
              }
            }
          }
        }
      }
    }
    // Handle added references.
    if ($added = array_diff($new, $old)) {
      foreach ($added as $nid) {
        // Load the referenced node if it is of the specified away type.
        if ($referenced_node = node_load(array('nid' => $nid, 'type' => $away_node_type))) {
          // Detect whether the reference already exists.
          $exists = FALSE;
          if ($referenced_node->$away_field && !empty($referenced_node->$away_field)) {
            foreach ($referenced_node->$away_field as $data) {
              if ($data['nid'] == $home_node->nid) {
                $exists = TRUE;
                break;
              }
            }
          }
          // Add the new reference. Don't create a duplicate.
          if (!$exists) {
            $referenced_node->{$away_field}[] = array('nid' => $home_node->nid);
            _corresponding_node_references_update($referenced_node);
          }
        }
      }
    }
  }
}

/**
 * Remove corresponding references on node deletion.
 *
 * @param $home_node the node object catched in the node api on this node a reference is made to the away node
 * @param $home_field the field on the home node used to make the reference
 * @param $away_node_type the type of the away node
 * @param $away_field the name of the field on the away node referencing to the home node
 */
function corresponding_node_references_delete($home_node, $home_field, $away_node_type, $away_field) {
  // Iterate through the field's references.
  foreach ($home_node->$home_field as $reference) {
    if (!empty($reference['nid'])) {
      // Load the referenced node if it is of the specified away type.
      if ($referenced_node = node_load(array('nid' => $nid, 'type' => $away_node_type))) {
        // Iterate through the away node's references.
        foreach ($referenced_node->$away_field as $key => $value) {
          // Remove references to the deleted node.
          if ($value['nid'] && $value['nid'] == $home_node->nid) {
            unset($referenced_node->{$away_field}[$key]);
            _corresponding_node_references_update($referenced_node);
            break;
          }
        }
      }
    }
  }
}

/**
 * Update field data.
 *
 * @param $node the referenced node to be updated
 */
function _corresponding_node_references_update($node) {
  content_update($node);
}
