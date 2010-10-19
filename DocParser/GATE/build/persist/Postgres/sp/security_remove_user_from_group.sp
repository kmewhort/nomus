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
 *  $Id: security_remove_user_from_group.sp 5582 2004-04-08 10:41:24Z valyt $
 *
 */


CREATE OR REPLACE FUNCTION security_remove_user_from_group(int4,int4) RETURNS boolean AS '

   DECLARE
      p_group_id alias for $1;
      p_user_id alias for $2;

   BEGIN
       delete from t_user_group
       where ugrp_user_id = p_user_id
             and ugrp_group_id = p_group_id;

       /* dummy */
       return true;
   END;
' LANGUAGE 'plpgsql';
