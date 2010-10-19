/*
 *
 *  Copyright (c) 1998-2002, The University of Sheffield.
 *
 *  This file is part of GATE (see http://gate.ac.uk/), and is free
 *  software, licenced under the GNU Library General Public License,
 *  Version 2, June 1991 (in the distribution as file licence.html,
 *  and also available at http://gate.ac.uk/gate/licence.html).
 *
 *  Marin Dimitrov, 20/Mar/2002
 *
 *  $Id: security_is_valid_security_data.sp 5582 2004-04-08 10:41:24Z valyt $
 *
 */


CREATE OR REPLACE FUNCTION security_is_valid_security_data(int4,int4,int4) RETURNS boolean AS '

   DECLARE
      p_perm_mode_4 alias for $1;
      p_group_id alias for $2;
      p_user_id alias for $3;

      p_perm_mode int2;
      
      C_PERM_WR_GW constant int2 := 1;
      C_PERM_GR_GW constant int2 := 2;
      C_PERM_GR_OW constant int2 := 3;
      C_PERM_OR_OW constant int2 := 4;

   BEGIN
      /*downcast params*/
      p_perm_mode = cast(p_perm_mode_4 as int2);
      
      if (p_perm_mode = C_PERM_WR_GW or
          p_perm_mode = C_PERM_GR_GW or
          p_perm_mode = C_PERM_GR_OW) then
         /* group write/read access, owner_group_id should ne NOT NULL */
         if (p_group_id is null) then
            return false;
         end if;
      end if;

      if (p_perm_mode = C_PERM_GR_OW or p_perm_mode = C_PERM_OR_OW) then
         /* owner_user_id is mandatory */
         if (p_user_id is null) then
            return false;
         end if;
      end if;

      return true;

   END;
' LANGUAGE 'plpgsql'
WITH (iscachable);
