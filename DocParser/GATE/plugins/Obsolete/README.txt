Obsolete Plugins
================

This directory contains plugins that are now obsolete but have been retained
for backwards compatibility.  These plugins should not be needed for new
development with GATE, but are available here in case they are required by an
old application.  Note that the obsolete plugins will not appear in GATE's
plugin manager by default - if you need one you will have to load it explicitly
("Add a new CREOLE repository" in the plugin manager, or
Gate.getCreoleRegister().registerDirectories(URL) from the API).

For further details about what is contained in each plugin, see the comments in
their respective creole.xml files.
