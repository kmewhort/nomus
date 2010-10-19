/*
 * *  Copyright (c) 1998-2002, The University of Sheffield.
 *
 *  This file is part of GATE (see http://gate.ac.uk/), and is free
 *  software, licenced under the GNU Library General Public License,
 *  Version 2, June 1991 (in the distribution as file licence.html,
 *  and also available at http://gate.ac.uk/gate/licence.html).
 *
 *  Marin Dimitrov, 21/Mar/2002
 *
 *  $Id: security_create_group.sp 5582 2004-04-08 10:41:24Z valyt $
 *
 */


CREATE OR REPLACE FUNCTION security_create_group(varchar) RETURNS int4 AS '

   DECLARE
      p_grp_name alias for $1;
      cnt int4;
      x_duplicate_group_name constant varchar := ''x_duplicate_group_name'';

   BEGIN
      select count(grp_name)
      into   cnt
      from   t_group
      where  grp_name = p_grp_name;
  
      if (cnt > 0) then
         raise exception ''%'',x_duplicate_group_name;
      end if;
    
      insert into t_group(grp_id,
                          grp_name)
      values(nextval(''seq_group''),
             p_grp_name);

      /* get ID */

      return currval(''seq_group'');

   END;
' LANGUAGE 'plpgsql';
