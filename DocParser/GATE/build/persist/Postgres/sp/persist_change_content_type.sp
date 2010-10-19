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
 *  $Id: persist_change_content_type.sp 5582 2004-04-08 10:41:24Z valyt $
 *
 */

CREATE OR REPLACE FUNCTION persist_change_content_type(int4,int4) RETURNS boolean AS '

   DECLARE
      p_cont_id alias for $1;
      p_new_type_4 alias for $2;

      x_invalid_content_type constant varchar := ''x_invalid_content_type'';

      C_CHARACTER_CONTENT  constant int4 := 1;
      C_BINARY_CONTENT     constant int4 := 2;
      C_EMPTY_CONTENT      constant int4 := 3;
      p_new_type int2;

   BEGIN
     /* convert the parameter */
     p_new_type = cast (p_new_type_4 as int2);
     
     if (p_new_type not in (C_CHARACTER_CONTENT,
                            C_BINARY_CONTENT,
                            C_EMPTY_CONTENT)) then
                           
        raise error.x_invalid_content_type;
     end if;
    
     update t_doc_content
     set    dc_content_type = p_new_type
     where  dc_id = p_cont_id;

     /* dummy */
     return true;

   END;
'
LANGUAGE 'plpgsql';
