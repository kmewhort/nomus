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
 *  $Id: security_is_member_of_group.sp 5582 2004-04-08 10:41:24Z valyt $
 *
 */



CREATE OR REPLACE FUNCTION security_is_member_of_group(int4,int4) RETURNS boolean AS '

   DECLARE

      p_user_id alias for $1;
      p_grp_id alias for $2;

      cnt int4;

   BEGIN

    select count(ugrp_id)
    into   cnt
    from   t_user_group
    where  ugrp_user_id = p_user_id
           and ugrp_group_id = p_grp_id;

    return (cnt > 0);

   END;
' LANGUAGE 'plpgsql';
