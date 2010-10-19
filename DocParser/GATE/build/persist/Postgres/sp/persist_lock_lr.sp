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
 *  $Id: persist_lock_lr.sp 5582 2004-04-08 10:41:24Z valyt $
 *
 */


CREATE OR REPLACE FUNCTION persist_lock_lr(int4,int4,int4) RETURNS boolean AS '
   DECLARE
      p_lr_id  alias for $1;
      p_usr_id alias for $2;
      p_grp_id alias for $3;

      l_can_write_lr boolean;
      l_locking_user_id int4;

      C_READ_ACCESS constant int2 := 0;
      C_WRITE_ACCESS constant int2 := 1;

      x_insufficient_privileges constant varchar := ''x_insufficient_privileges'';
      x_invalid_lr constant varchar := ''x_invalid_lr'';

   BEGIN
      /* 1. check if the user has write access to the LR */
      if (false = security_has_access_to_lr(p_lr_id,
                                            p_usr_id,
                                            p_grp_id,
                                            C_WRITE_ACCESS)) then
         raise exception ''%'',x_insufficient_privileges;
      end if;

     /* 2. get the locking user if any */
     select lr_locking_user_id
     into   l_locking_user_id
     from   t_lang_resource
     where  lr_id = p_lr_id;

     if not FOUND then
        raise exception ''%'',x_invalid_lr;
     end if;
    
     if (l_locking_user_id is null) then
        /* 2a resource unlocked - lock it */
        update t_lang_resource
        set    lr_locking_user_id = p_usr_id
        where  lr_id = p_lr_id;
       
        return true;
     else
        /* 2b resource already locked */
        return false;
     end if;

   END;
'
LANGUAGE 'plpgsql';
