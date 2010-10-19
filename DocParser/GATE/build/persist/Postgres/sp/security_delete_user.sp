/*
 *
 *  Copyright (c) 1998-2002, The University of Sheffield.
 *
 *  This file is part of GATE (see http://gate.ac.uk/), and is free
 *  software, licenced under the GNU Library General Public License,
 *  Version 2, June 1991 (in the distribution as file licence.html,
 *  and also available at http://gate.ac.uk/gate/licence.html).
 *
 *  Marin Dimitrov, 21/Mar/2002
 *
 *  $Id: security_delete_user.sp 5582 2004-04-08 10:41:24Z valyt $
 *
 */


CREATE OR REPLACE FUNCTION security_delete_user(int4) RETURNS boolean AS '

   DECLARE
      p_usr_id alias for $1;
      has_documents boolean;

      x_user_owns_resources constant varchar := ''x_user_owns_resources'';

   BEGIN
       /* check for documents
       -- if the user owns documents then fail
       */
       if (security_can_delete_group(p_usr_id) = false) then
          raise exception ''%'', x_user_owns_resources;
       end if;

       /* delete user from t_user_group */
       delete from t_user_group
       where  ugrp_user_id = p_usr_id;
       
       /* unlock LRs locked by user */
       update t_lang_resource
       set    lr_locking_user_id = null
       where  lr_locking_user_id = p_usr_id;
       
       /* delete the user */
       delete from t_user
       where usr_id = p_usr_id;

       /* dummy */
       return true;

   END;
'
LANGUAGE 'plpgsql';
