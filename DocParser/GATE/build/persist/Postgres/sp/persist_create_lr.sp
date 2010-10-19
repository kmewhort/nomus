/*
 *  persist.bdy
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
 *  $Id: persist_create_lr.sp 5582 2004-04-08 10:41:24Z valyt $
 *
 */


CREATE OR REPLACE FUNCTION persist_create_lr(int4,int4,varchar,varchar,int4,int4) RETURNS int4 AS '

   DECLARE
      /* in parameters */
      p_usr_id alias for $1;
      p_grp_id alias for $2;
      p_lr_type alias for $3;
      p_lr_name alias for $4;
      p_lr_permissions_4 alias for $5;
      p_lr_parent_id alias for $6;

      l_lr_type int4;
      l_lr_id int4;
      p_lr_permissions int2;

      x_incomplete_data constant varchar := ''incomplete data'';
      x_invalid_lr_type constant varchar := ''invalid LR type supplied'';

   BEGIN
     /* downcast the params */
     p_lr_permissions = cast(p_lr_permissions_4 as int2);
   
     /* 1. sanity check */
     if (false = security_is_valid_security_data(p_lr_permissions,p_grp_id,p_usr_id)) then
        raise exception ''error: %'',x_incomplete_data;
     end if;

     /* 3. check if the LR type supplied is valid */
     select lrtp_id
     into   l_lr_type
     from   t_lr_type
     where  lrtp_type = p_lr_type;

     if not FOUND then
        raise exception ''error: %'',x_invalid_lr_type;
     end if;

     /* 2. create a lang_resource record */
     insert into t_lang_resource(lr_id,
                                 lr_type_id,
                                 lr_owner_user_id,
                                 lr_locking_user_id,
                                 lr_owner_group_id,
                                 lr_name,
                                 lr_access_mode,
                                 lr_parent_id)
     values (nextval(''seq_lang_resource''),
            l_lr_type,
            p_usr_id,
            null,
            p_grp_id,
            p_lr_name,
            p_lr_permissions,
            p_lr_parent_id);

     /* get ID */
     select currval(''seq_lang_resource'')
     into l_lr_id;

     return l_lr_id;

   END;
' LANGUAGE 'plpgsql';
