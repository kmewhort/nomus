$(document).ready(function()
{

$('#sidebar-left .block').hover(
 function () {
  $(this).stop().animate({'marginLeft':'150px'},200);
 },
 function () {
  $(this).stop().animate({'marginLeft':'0px'},200);
 });

});