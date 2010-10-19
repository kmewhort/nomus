<?php
// $Id: widget.tpl.php,v 1.1.2.12 2010/05/31 03:17:52 marvil07 Exp $

/**
 * @file
 * widget.tpl.php
 *
 * UpDown widget theme for Vote Up/Down
 */
?>
<!-- <div class="vud-widget vud-widget-updown" id="<?php print $id; ?>">-->
<div id="<?php print $id; ?>">
  <div class="vud-widget-points">(<?php print $points; ?>)</div>
  <?php if ($show_links): ?>
    <a href="<?php print $link_up; ?>" class="<?php print $link_class_up; ?>">
      <div class="<?php print $class_up; ?>" title="<?php print t('Vote up!'); ?>"></div>
    </a>
    <a href="<?php print $link_down; ?>" class="<?php print $link_class_down; ?>">
      <div class="<?php print $class_down; ?>" title="<?php print t('Vote down!'); ?>"></div>
    </a>
  <?php endif; ?>
</div>
<!-- </div> -->
