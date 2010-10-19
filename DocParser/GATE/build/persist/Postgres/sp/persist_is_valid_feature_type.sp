/*
 *
 *  Copyright (c) 1998-2002, The University of Sheffield.
 *
 *  This file is part of GATE (see http://gate.ac.uk/), and is free
 *  software, licenced under the GNU Library General Public License,
 *  Version 2, June 1991 (in the distribution as file licence.html,
 *  and also available at http://gate.ac.uk/gate/licence.html).
 *
 *  Marin Dimitrov, 22/Mar/2002
 *
 *  $Id: persist_is_valid_feature_type.sp 5582 2004-04-08 10:41:24Z valyt $
 *
 */

CREATE OR REPLACE FUNCTION persist_is_valid_feature_type(int4) RETURNS boolean AS '

   DECLARE
      p_type alias for $1;

      C_VALUE_TYPE_NULL       constant int4 := 100;
      C_VALUE_TYPE_INTEGER    constant int4 := 101;
      C_VALUE_TYPE_LONG       constant int4 := 102;
      C_VALUE_TYPE_BOOLEAN    constant int4 := 103;
      C_VALUE_TYPE_STRING     constant int4 := 104;
      C_VALUE_TYPE_BINARY     constant int4 := 105;
      C_VALUE_TYPE_FLOAT      constant int4 := 106;

   BEGIN

     return (p_type in (C_VALUE_TYPE_NULL,
                       C_VALUE_TYPE_INTEGER,
                       C_VALUE_TYPE_LONG,
                       C_VALUE_TYPE_BOOLEAN,
                       C_VALUE_TYPE_STRING,
                       C_VALUE_TYPE_BINARY,
                       C_VALUE_TYPE_FLOAT));

   END;
'
LANGUAGE 'plpgsql'
WITH (iscachable);
