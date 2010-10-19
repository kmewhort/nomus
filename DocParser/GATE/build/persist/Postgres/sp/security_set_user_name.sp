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
 *  $Id: security_set_user_name.sp 5582 2004-04-08 10:41:24Z valyt $
 *
 */


CREATE OR REPLACE FUNCTION security_set_user_name(int4,varchar) RETURNS boolean AS '

   DECLARE
      /* in parameters */
      p_user_id alias for $1;
      p_new_name alias for $2;

   BEGIN

      update t_user
      set    usr_login = p_new_name
      where  usr_id = p_user_id;

      return true;

   END;
' LANGUAGE 'plpgsql';
