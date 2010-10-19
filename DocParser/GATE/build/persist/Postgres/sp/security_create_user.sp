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
 *  $Id: security_create_user.sp 5582 2004-04-08 10:41:24Z valyt $
 *
 */

CREATE OR REPLACE FUNCTION security_create_user(varchar,varchar) RETURNS int4 AS '
   DECLARE
      p_usr_name alias for $1;
      p_usr_pass alias for $2;
      cnt int4;

      x_duplicate_user_name constant varchar := ''x_duplicate_user_name'';

   BEGIN
      select count(usr_login)
      into   cnt
      from   t_user
      where  usr_login = p_usr_name;
  
      if (cnt > 0) then
         raise exception ''%'', x_duplicate_user_name;
      end if;
    
      insert into t_user(usr_id,
                         usr_login,
                         usr_pass)
      values(nextval(''seq_user''),
             p_usr_name,
             p_usr_pass);

      /* get ID */
      return currval(''seq_user'');

   END;
'
LANGUAGE 'plpgsql';
