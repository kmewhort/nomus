/*
 *
 *  Copyright (c) 1998-2002, The University of Sheffield.
 *
 *  This file is part of GATE (see http://gate.ac.uk/), and is free
 *  software, licenced under the GNU Library General Public License,
 *  Version 2, June 1991 (in the distribution as file licence.html,
 *  and also available at http://gate.ac.uk/gate/licence.html).
 *
 *  Marin Dimitrov, 28/Mar/2002
 *
 *  $Id: test_create_test_data.sp 5582 2004-04-08 10:41:24Z valyt $
 *
 */

CREATE OR REPLACE FUNCTION test_create_test_data() RETURNS bool AS '

   DECLARE
                          
      GROUP_1_ID constant int4 := 101;
      GROUP_2_ID constant int4 := 102;
    
      USER_1_ID constant int4 := 1;
      USER_2_ID constant int4 := 2;
      USER_3_ID constant int4 := 3;
      USER_4_ID constant int4 := 4;
      USER_5_ID constant int4 := 5;
    
      UG_ID_1 constant int4 := 301;
      UG_ID_2 constant int4 := 302;
      UG_ID_3 constant int4 := 303;
      UG_ID_4 constant int4 := 304;
      UG_ID_5 constant int4 := 305;
      UG_ID_6 constant int4 := 306;
      UG_ID_7 constant int4 := 307;
    
      DOC_1_ID constant int4 := 501;
      DOC_2_ID constant int4 := 502;
      DOC_3_ID constant int4 := 503;
      DOC_4_ID constant int4 := 504;
      DOC_5_ID constant int4 := 505;
    
      LR_TYPE_1_ID constant int4 := 1;
    
      /* ACCESS_read_write */
      ACCESS_WORLD_GROUP constant int4 := 1;
      ACCESS_GROUP_GROUP constant int4 := 2;
      ACCESS_GROUP_OWNER constant int4 := 3;
      ACCESS_OWNER_OWNER constant int4 := 4;

   BEGIN
       /* first remove the previous test data */
       perform test_remove_test_data();
  
       /* create groups */
  
       insert into t_group(grp_id,
                           grp_name)
       values (GROUP_1_ID,''English Language Group'');
  
       insert into t_group(grp_id,
                           grp_name)
       values (GROUP_2_ID,''Suahili Group'');
  
       /* create users */
  
       insert into t_user(usr_id,
                          usr_login,
                          usr_pass)
       values(USER_1_ID,''hamish'',''sesame'');
  
       insert into t_user(usr_id,
                          usr_login,
                          usr_pass)
       values(USER_2_ID,''kalina'',''sesame'');
  
       insert into t_user(usr_id,
                          usr_login,
                          usr_pass)
       values(USER_3_ID,''diana'',''sesame'');
  
       insert into t_user(usr_id,
                          usr_login,
                          usr_pass)
       values(USER_4_ID,''valentin'',''sesame'');
  
       insert into t_user(usr_id,
                          usr_login,
                          usr_pass)
       values(USER_5_ID,''cristian'',''sesame'');
  
       /*  put users in groups */
  
       insert into t_user_group(ugrp_id,
                                ugrp_user_id,
                                ugrp_group_id)
       values(UG_ID_1,USER_1_ID,GROUP_1_ID);
  
       insert into t_user_group(ugrp_id,
                                ugrp_user_id,
                                ugrp_group_id)
       values(UG_ID_2,USER_2_ID,GROUP_1_ID);
  
       insert into t_user_group(ugrp_id,
                                ugrp_user_id,
                                ugrp_group_id)
       values(UG_ID_3,USER_3_ID,GROUP_1_ID);
  
       insert into t_user_group(ugrp_id,
                                ugrp_user_id,
                                ugrp_group_id)
       values(UG_ID_4,USER_4_ID,GROUP_1_ID);
  
  
       insert into t_user_group(ugrp_id,
                                ugrp_user_id,
                                ugrp_group_id)
       values(UG_ID_5,USER_2_ID,GROUP_2_ID);
  
       insert into t_user_group(ugrp_id,
                                ugrp_user_id,
                                ugrp_group_id)
       values(UG_ID_6,USER_3_ID,GROUP_2_ID);
  
       insert into t_user_group(ugrp_id,
                                ugrp_user_id,
                                ugrp_group_id)
       values(UG_ID_7,USER_5_ID,GROUP_2_ID);

      /* dummy */
      return true;

   END;
'
LANGUAGE 'plpgsql';


