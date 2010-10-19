/*
 *
 *  Copyright (c) 1998-2002, The University of Sheffield.
 *
 *  This file is part of GATE (see http://gate.ac.uk/), and is free
 *  software, licenced under the GNU Library General Public License,
 *  Version 2, June 1991 (in the distribution as file licence.html,
 *  and also available at http://gate.ac.uk/gate/licence.html).
 *
 *  Marin Dimitrov, 22/Mar/2002
 *
 *  $Id: persist_update_lr.sp 5582 2004-04-08 10:41:24Z valyt $
 *
 */

CREATE OR REPLACE FUNCTION persist_update_lr(int4,varchar,int4) RETURNS boolean AS '

   DECLARE
      p_lr_id     alias for $1;
      p_lr_name   alias for $2;
      p_lr_parent_id alias for $3;

      cnt int4;
      x_invalid_lr constant varchar := ''x_invalid_lr'';

   BEGIN
      /* 1. is there such LR? */
      select count(LR_ID)
      into   cnt
      from   t_lang_resource
      where  lr_id = p_lr_id;

      if not FOUND then
         raise exception ''%d'',x_invalid_lr;
      end if;
  
      /* 2. update it */
      update t_lang_resource
      set    lr_name = p_lr_name,
             lr_parent_id = p_lr_parent_id
      where  lr_id = p_lr_id;

      /* dummy */
      return true;

   END;'
LANGUAGE 'plpgsql';
