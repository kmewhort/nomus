//$id$
jQuery Eye Candy Module


-------------------------
Description
-------------------------
jQuery Eye Candy is a javascript wrapper module for several jquery plugins that handle appearance functions like rounded corners, drop shadows, and gradients. Using this module enables using these jquery plugins on all pages or selected pages without having to edit any theme files. Usage is not complicated, but some basic jquery familiarity, for specifying the jquery statements required to use the individual plugins, is helpful.


-------------------------
Installation Instructions
-------------------------

PREREQUISITES: 

1. This module requires the jquery_plugin module for D6. Be sure to download and install it first.

2. This module requires one or more of the following jquery plugins (you only need to download the plugins for the functions you wish to use):

     a) ColorBlend plugin: http://plugins.jquery.com/project/colorBlend
     b) CurvyCorners plugin: http://blue-anvil.com/archives/anti-aliased-rounded-corners-with-jquery
     c) DropShadow plugin: http://plugins.jquery.com/project/DropShadow (also requires the Dimensions plugin)
     d) Gradient plugin: http://plugins.jquery.com/project/gradient (also requires the Dimensions plugin)
     e) Reflection plugin: http://plugins.jquery.com/project/reflect
     f) Dimensions plugin: download from http://eyebulb.com/dropshadow/ (not the plugins.jquery.com project page)

INSTALLATION:

1. Download and install the jquery_plugin module from http://drupal.org/project/jquery_plugin.

2. Download the required jquery plugins you wish to you use (see PREREQUISITES above). You don't need to install any you don't plan on using, the module will work without them and indicate their absence on the settings page.

3. Extract the jquery plugin files into the jquery_plugin directory. Use the .min.js (minified) versions of the plugin whenever possible).

4. Rename any plugins not fitting the jquery_plugin naming scheme of "jquery.<plugin-name>.min.js" The "min" indicates the file has been "minified". Some jquery plugins include minified versions, some do not. All that matters for jquery_plugin is that the files are named appropriately. You may also wish to edit the .js files of non-minified plugins to remove any unnecessary comments and/or instructions (to shrink the file size).

5. Navigate to admin/settings/jq_eyecandy. If the plugins are properly installed you should see a message indicating the path where it has been found in the fieldset along with an enabled textarea to type any jquery statements you wish to use to invoke that plugin.

6. See the demo pages at http://demo.joannmelnik.com for examples of how to use the module or visit the plugin.jquery.com project page for a particular plugin for instructions on how to use it.


-------------------------
USAGE
-------------------------

1. Set sitewide options for each plugin as desired in the jQuery Eye Candy Settings page (admin/settings/jq_eyecandy). Only add statements here you wish to be applied to all pages. In general, the jquery statements you enter should be similar to:

     //required for each plugin
     $(document).ready(function() {
     
       //plugin specific statements here
       $('.mytestdiv1').corner();
       
     })

2. You can also use plugins on specific pages by including the code in <script> tags (don't forget you'll have to add the <script> tag for the "Filtered HTML" input format):

     <script type="text/javascript">
       //required for each plugin
       $(document).ready(function() {

         //plugin specific statements here
         $('.mytestdiv1').corner();

       })
    </script>

3. For usage examples and sample code for each of the plugins see the plugin demo pages at http://demo.joannmelnik.com or the project pages of the individual plugins at plugins.jquery.com.

4. For information on using jquery with drupal see:
    
    --Drupal.org / Using Query - http://drupal.org/node/88978
    --Drupal Dojo / jQuery Resurrected - http://drupaldojo.com/node/73
    --Drupal School / Tasty JQuery Modules - http://theartlab.net/drupal-school-tasty-jquery-modules
    
5. For some good info on using jQuery in general see: http://docs.jquery.com/Tutorials


-------------------------
SUPPORT
-------------------------

For support, please submit requests via the project issues queue at: http://drupal.org/project/issues/jq_eyecandy
