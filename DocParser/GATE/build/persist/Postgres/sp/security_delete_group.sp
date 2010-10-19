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
 *  $Id: security_delete_group.sp 5582 2004-04-08 10:41:24Z valyt $
 *
 */

CREATE OR REPLACE FUNCTION security_delete_group(int4) RETURNS boolean AS '

   DECLARE
      p_grp_id alias for $1;

      x_group_owns_resources constant varchar := ''x_group_owns_resources'';

   BEGIN
      /* check for documents
       -- if the group ownes documents then fail
      */
      if (security_can_delete_group(p_grp_id) = false) then
          raise exception ''%'', x_group_owns_resources;
      end if;
  
      /* delete group users from t_user_group */
      delete from t_user_group
      where  ugrp_group_id = p_grp_id;

      /* delete group from t_group */
      delete from t_group
      where  grp_id = p_grp_id;
      
      /* dummy */
      return true;
   END;
' LANGUAGE 'plpgsql';
