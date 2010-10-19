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
 *  $Id: security_add_user_to_group.sp 5582 2004-04-08 10:41:24Z valyt $
 *
 */

CREATE OR REPLACE FUNCTION security_add_user_to_group(int4,int4) RETURNS boolean AS '

   DECLARE
      p_group_id alias for $1;
      p_user_id alias for $2;

   BEGIN
       insert into t_user_group(ugrp_id,
                                ugrp_user_id,
                                ugrp_group_id)
       values (nextval(''seq_user_group''),
               p_user_id,
               p_group_id);

       /* dummy */
       return true;
   END;
' LANGUAGE 'plpgsql';
