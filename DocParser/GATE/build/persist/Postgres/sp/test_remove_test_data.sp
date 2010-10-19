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
 *  $Id: test_remove_test_data.sp 5582 2004-04-08 10:41:24Z valyt $
 *
 */

CREATE or replace FUNCTION test_remove_test_data() RETURNS boolean AS '

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

     /* remove users in groups */
     delete from t_user_group
     where ugrp_id in (UG_ID_1,UG_ID_2,UG_ID_3,UG_ID_4,UG_ID_5,UG_ID_6,UG_ID_7);

    /* delete groups */
     delete from t_group
     where grp_id in (GROUP_1_ID,GROUP_2_ID);

     /* delete users */
     delete from t_user
     where  usr_id in (USER_1_ID,USER_2_ID,USER_3_ID,USER_4_ID,USER_5_ID);

     /* dummy */
     return true;

   END;
'
LANGUAGE 'plpgsql';


