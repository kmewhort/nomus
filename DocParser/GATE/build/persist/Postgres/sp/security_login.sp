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
 *  $Id: security_login.sp 5582 2004-04-08 10:41:24Z valyt $
 *
 */


CREATE OR REPLACE FUNCTION security_login(varchar,varchar,int4) RETURNS boolean AS '

   DECLARE
      p_usr_name alias for $1;
      p_usr_pass alias for $2;
      p_pref_grp_id alias for $3;

      cnt int4;
      l_usr_id int4;

      x_invalid_user_name_or_pass constant varchar := ''x_invalid_user_name_or_pass'';
      x_invalid_user_group constant varchar := ''x_invalid_user_group'';
      C_ADMIN_USER_ID constant int4 := 0;
      C_ADMIN_GROUP_ID constant int4 := 0;

   BEGIN
      /* find ID
         because of previous step we are sure
         there is such user
      */
      select usr_id
      into   l_usr_id
      from   t_user
      where  usr_login = p_usr_name
             and usr_pass= p_usr_pass;

      if not FOUND then
         raise exception ''%'',x_invalid_user_name_or_pass;
      end if;

      /* valid group? */
      select count(ugrp_id)
      into   cnt
      from   t_user_group
      where  ugrp_group_id = p_pref_grp_id
             and ugrp_user_id = l_usr_id;
       
      if (cnt = 0) then
         raise exception ''%'', x_invalid_user_group;
      end if;

      /* is privileged? */

      if (l_usr_id = C_ADMIN_USER_ID and p_pref_grp_id = C_ADMIN_GROUP_ID) then
         return true;
      else
         return false;
      end if;

   END;
'
LANGUAGE 'plpgsql';
