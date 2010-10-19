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
 *  $Id: security_has_access_to_lr.sp 5582 2004-04-08 10:41:24Z valyt $
 *
 */


CREATE OR REPLACE FUNCTION security_has_access_to_lr(int4,int4,int4,int4) RETURNS boolean AS '

   DECLARE
      p_lr_id alias for $1;
      p_usr_id alias for $2;
      p_grp_id alias for $3;
      p_mode_4 alias for $4;

      cnt int4;
      l_owner_group int4;
      l_owner_user int4;
      l_locking_user int4;
      l_access_mode int4;
      p_mode int2;

      C_READ_ACCESS constant int2 := 0;
      C_WRITE_ACCESS constant int2 := 1;

      C_PERM_WR_GW constant int2 := 1;
      C_PERM_GR_GW constant int2 := 2;
      C_PERM_GR_OW constant int2 := 3;
      C_PERM_OR_OW constant int2 := 4;

      x_invalid_argument constant varchar := ''x_invalid_argument'';
      x_invalid_lr constant varchar := ''x_invalid_lr'';

   BEGIN
      /*downcast params*/
      p_mode = cast(p_mode_4 as int2);
      
      /* preconditions */
      if (p_mode <> C_READ_ACCESS and p_mode <> C_WRITE_ACCESS) then
         raise exception ''%'', x_invalid_argument;
      end if;

      select coalesce(lr_owner_user_id,0),
             coalesce(lr_owner_group_id,0),
             coalesce(lr_locking_user_id,0),
             lr_access_mode
      into   l_owner_user,
             l_owner_group,
             l_locking_user,
             l_access_mode
      from   t_lang_resource
      where  lr_id = p_lr_id;

      if not FOUND then
         raise exception ''%'',x_invalid_lr;
      end if;

      if (p_mode = C_WRITE_ACCESS) then

         /* not locked but check permissions
            write access is granted :
               1a. permissions are USER_WRITE and OWNER_USER == p_usr_id
               1b. permissions are GROUP_WRITE and
                   member_of(p_usr_id,OWNER_GROUP) and
                   OWNER_GROUP == p_grp_id
         */

         /* user is owner, and permisssions are OWNER_WRITE */
         if (l_owner_user = p_usr_id and
                 (l_access_mode = C_PERM_GR_OW or l_access_mode = C_PERM_OR_OW)) then
            /* case 1a */
            return true;
         end if;

         /* user is in owning group */
         if (security_is_member_of_group(p_usr_id,l_owner_group) and
             l_owner_group = p_grp_id and
             (l_access_mode = C_PERM_GR_GW or l_access_mode = C_PERM_WR_GW)) then
            /* -- case 1b */
            return true;
         end if;

         /* fail */
         return false;

       else
          /*
          -- read access request
          -- check read persmissions
          -- read access is granted :
          -- 1a. OWNER_USER == p_usr_id : owner can always read
          -- 1b. permissions are GROUP_READ and member_of(p_usr_id,OWNER_GROUP)
          -- 1c. permissions are WORLD_READ
          */
          if (l_owner_user = p_usr_id) then
             /* -- case 1a */
             return true;
          end if;

          if ((l_access_mode = C_PERM_GR_GW or l_access_mode = C_PERM_GR_OW) and
               security_is_member_of_group(p_usr_id,l_owner_group) and
               l_owner_group = p_grp_id)  then
             /* -- case 1b */
             return true;
          end if;

          if (l_access_mode = C_PERM_WR_GW) then
             /* -- case 1c */
             return true;
          end if;

          /*--fail */
          return false;

       end if;

   END;'
LANGUAGE 'plpgsql';
