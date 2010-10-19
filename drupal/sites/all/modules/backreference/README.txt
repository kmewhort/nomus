// $Id: README.txt,v 1.1 2009/08/20 00:04:36 jcfiala Exp $

BackReference
-------------

Backreference Module provides a nodeapi interface to maintain 1-1 relationships
between all shared instances of a nodereference field. This means that given a field instance
of field_reference1, if you add a reference to NodeBeta to NodeAlpha's field_reference1 and
NodeBeta has an instance of field_reference1, then NodeAlpha will be added to NodeBeta's instance
of field_reference1.

Note that this code grew out of the backreference module that user dopry stowed
  in his sandbox on cvs two years ago.  Although we've made changes since then, we're grateful for
  the foundation he provided.

Using it is as simple as installing the module by putting it in your sites/all/modules directory,
and then going to Administer -> Site building -> Modules to enable it.  Note that this does require
the NodeReference module and thus, the CCK module as well.

Once installed, go in to edit any existing Nodereference field or create a new one, and in addition
to the usual settings, at the bottom you will see a BackReference fieldset with a dropdown which allows
you to choose a NodeReference field.  Whichever field you select, which can include the same field you're
editing, that field will be considered to be linked to the current field.  What that means is that whenever
you save a node with this field, whatever node(s) this field is pointing to will be updated to point back
at this node... and the same with the field you've selected.

This code has been tested with simpletests, which you can try yourself, and has been used in production on
a number of sites.
