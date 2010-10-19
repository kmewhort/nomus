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
 *  $Id: persist_create_feature.sp 5582 2004-04-08 10:41:24Z valyt $
 *
 */

CREATE OR REPLACE FUNCTION persist_create_feature(int4,int4,varchar,int4,float8,varchar,bytea,int4) RETURNS int4 AS '

   DECLARE
      p_entity_id           alias for $1;
      p_entity_type_4       alias for $2;
      p_key                 alias for $3;
      p_value_int           alias for $4;
      p_value_float         alias for $5;
      p_value_varchar       alias for $6;
      p_value_binary        alias for $7;
      p_value_type_4        alias for $8;

      l_feature_key_id int4;
      cnt int4;
      p_entity_type int2;
      p_value_type int2;

      x_invalid_feature_type constant varchar := ''x_invalid_feature_type'';

   BEGIN
      /* downcast the input params */
      p_entity_type = cast(p_entity_type_4 as int2);
      p_value_type = cast(p_value_type_4 as int2);
      
      
      if (false = persist_is_valid_feature_type(p_value_type)) then
         raise exception ''%'',x_invalid_feature_type;
      end if;
  
      /* 1. find feature_key id */
      select fk_id
      into   l_feature_key_id
      from   t_feature_key
      where  fk_string = p_key;

      if not FOUND then
         /* 2. if there is no such key then create one and get the id */
         insert into t_feature_key(fk_id,
                                   fk_string)
         values(nextval(''seq_feature_key''),
                p_key);

         l_feature_key_id := currval(''seq_feature_key'');

      end if;

      insert into t_feature(ft_id,
                            ft_entity_id,
                            ft_entity_type,
                            ft_key_id,
                            ft_int_value,
                            ft_float_value,
                            ft_binary_value,
                            ft_character_value,
                            ft_value_type)
      values(nextval(''seq_feature''),
             p_entity_id,
             p_entity_type,
             l_feature_key_id,
             p_value_int,
             p_value_float,
             p_value_binary,
             p_value_varchar,
             p_value_type);

      return currval(''seq_feature'');

   END;
'
LANGUAGE 'plpgsql';
