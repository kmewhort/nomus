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
 *  $Id: security_can_delete_group.sp 5582 2004-04-08 10:41:24Z valyt $
 *
 */

CREATE OR REPLACE FUNCTION security_can_delete_group(int4) RETURNS boolean AS '

   DECLARE
      p_grp_id alias for $1;
      cnt int4;

   BEGIN
      /* --if there are resources owned by group then fail */
      select count(lr_owner_group_id)
      into   cnt
      from   t_lang_resource
      where  lr_owner_group_id = p_grp_id;
  
      return (cnt = 0);

   END;
'
LANGUAGE 'plpgsql';
