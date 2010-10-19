/*
 *
 *  Copyright (c) 1998-2002, The University of Sheffield.
 *
 *  This file is part of GATE (see http://gate.ac.uk/), and is free
 *  software, licenced under the GNU Library General Public License,
 *  Version 2, June 1991 (in the distribution as file licence.html,
 *  and also available at http://gate.ac.uk/gate/licence.html).
 *
 *  Marin Dimitrov, 25/Mar/2002
 *
 *  $Id: persist_delete_annotation.sp 5582 2004-04-08 10:41:24Z valyt $
 *
 */

CREATE OR REPLACE FUNCTION persist_delete_annotation(int4,int4) RETURNS boolean AS '

   DECLARE
      p_doc_id alias for $1;
      p_ann_local_id alias for $2;

      l_ann_global_id int4;
      x_invalid_annotation constant varchar := ''x_invalid_annotation'';

      C_FEATURE_OWNER_ANNOTATION  constant int4 :=  3;

   BEGIN
      /* 0. get the global ID */
      select ann_global_id
      into   l_ann_global_id
      from   t_annotation
      where  ann_doc_id = p_doc_id
             and ann_local_id = p_ann_local_id;

      if not FOUND then
         raise exception ''%'',x_invalid_annotation;
      end if;

      /* 1. delete fetures */
      delete
      from   t_feature
      where  ft_entity_id = l_ann_global_id
             and ft_entity_type = C_FEATURE_OWNER_ANNOTATION;

      /* 2. delete aset-to-annotation mappings */
      delete
      from  t_as_annotation
      where asann_ann_id = l_ann_global_id;
     
      /* 3. delete annotations */
      delete
      from   t_annotation
      where  ann_global_id = l_ann_global_id;

      /* dummy */
      return true;

   END;
'
LANGUAGE 'plpgsql';
