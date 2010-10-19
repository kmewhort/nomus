create or replace package error is

/*
 *  error.pck
 *
 *  Copyright (c) 1998-2001, The University of Sheffield.
 *
 *  This file is part of GATE (see http://gate.ac.uk/), and is free
 *  software, licenced under the GNU Library General Public License,
 *  Version 2, June 1991 (in the distribution as file licence.html,
 *  and also available at http://gate.ac.uk/gate/licence.html).
 *
 *  Marin Dimitrov, 26/Sep/2001
 *
 *  $Id: error.spc 3462 2002-02-28 16:26:47Z marin $
 */
  
  -- Public type declarations 
/*  type <TypeName> is <Datatype>;
  
  -- Public constant declarations
  <ConstantName> constant <Datatype> := <Value>;

  -- Public variable declarations
  <VariableName> <Datatype>;

  -- Public function and procedure declarations
  function <FunctionName>(<Parameter> <Datatype>) return <Datatype>;
*/

  /* exceptions */

  x_duplicate_group_name EXCEPTION;
  PRAGMA EXCEPTION_INIT(x_duplicate_group_name, -20101 );

  x_duplicate_user_name EXCEPTION;
  PRAGMA EXCEPTION_INIT(x_duplicate_user_name, -20102);
  
  x_invalid_user_name EXCEPTION;
  PRAGMA EXCEPTION_INIT(x_invalid_user_name, -20103);

  x_invalid_user_pass EXCEPTION;
  PRAGMA EXCEPTION_INIT(x_invalid_user_pass, -20104);

  x_invalid_user_group EXCEPTION;
  PRAGMA EXCEPTION_INIT(x_invalid_user_group, -20105);

  x_invalid_lr EXCEPTION;
  PRAGMA EXCEPTION_INIT(x_invalid_lr, -20106);

  x_invalid_access_mode EXCEPTION;
  PRAGMA EXCEPTION_INIT(x_invalid_access_mode, -20107);
  
  x_invalid_argument EXCEPTION;
  PRAGMA EXCEPTION_INIT(x_invalid_argument, -20108);

  x_not_implemented EXCEPTION;
  PRAGMA EXCEPTION_INIT(x_not_implemented, -20109);

  x_group_owns_resources EXCEPTION;
  PRAGMA EXCEPTION_INIT(x_group_owns_resources, -20110);

  x_user_owns_resources EXCEPTION;
  PRAGMA EXCEPTION_INIT(x_user_owns_resources, -20111);

  x_incomplete_data EXCEPTION;
  PRAGMA EXCEPTION_INIT(x_incomplete_data, -20112);

  x_invalid_lr_type EXCEPTION;
  PRAGMA EXCEPTION_INIT(x_invalid_lr_type, -20113);

  x_invalid_annotation_type EXCEPTION;
  PRAGMA EXCEPTION_INIT(x_invalid_annotation_type, -20114);

  x_invalid_feature_type EXCEPTION;
  PRAGMA EXCEPTION_INIT(x_invalid_feature_type, -20115);

  x_invalid_content_type EXCEPTION;
  PRAGMA EXCEPTION_INIT(x_invalid_content_type, -20116);

  x_invalid_annotation EXCEPTION;
  PRAGMA EXCEPTION_INIT(x_invalid_annotation, -20117);

  x_insufficient_privileges EXCEPTION;
  PRAGMA EXCEPTION_INIT(x_insufficient_privileges, -20118);

  x_invalid_annotation_set EXCEPTION;
  PRAGMA EXCEPTION_INIT(x_invalid_annotation_set, -20119);
                           
end error;
/
