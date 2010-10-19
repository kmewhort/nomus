/*
 *
 *  Copyright (c) 1998-2002, The University of Sheffield.
 *
 *  This file is part of GATE (see http://gate.ac.uk/), and is free
 *  software, licenced under the GNU Library General Public License,
 *  Version 2, June 1991 (in the distribution as file licence.html,
 *  and also available at http://gate.ac.uk/gate/licence.html).
 *
 *  Marin Dimitrov, 25/Mar/2002
 *
 *  $Id: persist_unlock_lr.sp 5582 2004-04-08 10:41:24Z valyt $
 *
 */


CREATE OR REPLACE FUNCTION persist_unlock_lr(int4,int4) RETURNS boolean AS '
   DECLARE
      p_lr_id     alias for $1;
      p_usr_id    alias for $2;
      l_locking_user_id int4;

      x_invalid_lr constant varchar := ''x_invalid_lr'';
      C_ADMIN_USER_ID constant int2 := 0;

   BEGIN
      /* 1. get the locking user if any */
      select lr_locking_user_id
      into   l_locking_user_id
      from   t_lang_resource
      where  lr_id = p_lr_id;

     if not FOUND then
        raise exception ''%'',x_invalid_lr;
     end if;

     /* is the reource locked by us? */
     if (l_locking_user_id = p_usr_id or
         p_usr_id = C_ADMIN_USER_ID) then

        update t_lang_resource
        set    lr_locking_user_id = null
        where  lr_id = p_lr_id;

     end if;

     /* dummy */
     return true;
  END;
'
LANGUAGE 'plpgsql';
