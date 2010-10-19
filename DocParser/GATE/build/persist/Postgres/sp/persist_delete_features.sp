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
 *  $Id: persist_delete_features.sp 5582 2004-04-08 10:41:24Z valyt $
 *
 */


CREATE OR REPLACE FUNCTION persist_delete_features(int4,int4) RETURNS boolean AS '

   DECLARE
      p_ent_id     alias for $1;
      p_ent_type_4   alias for $2;
      
      p_ent_type int2;

   BEGIN
      /*downcast the params */
      p_ent_type = cast(p_ent_type_4 as int2);
      
      delete from t_feature
      where  ft_entity_id = p_ent_id
      and    ft_entity_type = p_ent_type;

      /* dummy */
      return true;

   END;
'
LANGUAGE 'plpgsql';
