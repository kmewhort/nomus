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
 *  $Id: persist_delete_document.sp 5582 2004-04-08 10:41:24Z valyt $
 *
 */

CREATE OR REPLACE FUNCTION persist_delete_document(int4) RETURNS boolean AS '

   DECLARE
      p_lr_id alias for $1;
      l_doc_id int4;
      l_content_id int4;

      x_invalid_lr constant varchar := ''x_invalid_lr'';
      C_FEATURE_OWNER_CORPUS constant int4 := 1;
      C_FEATURE_OWNER_DOCUMENT constant int4 := 2;
      C_FEATURE_OWNER_ANNOTATION constant int4 := 3;

   BEGIN
      /* 0. get doc_id */
      select doc_id
      into   l_doc_id
      from   t_document
      where  doc_lr_id = p_lr_id;

      if not FOUND then
         raise exception ''%d'',x_invalid_lr;
      end if;

      /* get content id */
      select doc_content_id
      into   l_content_id
      from   t_document
      where  doc_id = l_doc_id;

      if not FOUND then
         raise exception ''%d'',x_invalid_lr;
      end if;

     /* 1. delete features */
     delete 
     from   t_feature
     where  ft_entity_id = p_lr_id
            and ft_entity_type = C_FEATURE_OWNER_DOCUMENT;

     /* 2. delete annotations */

     /* 2.1. delete annotation features */
     delete 
     from   t_feature
     where  ft_entity_type = C_FEATURE_OWNER_ANNOTATION
            and ft_entity_id in (select ann_global_id
                                 from   t_annotation
                                 where  ann_doc_id = l_doc_id
                                );

     /* 2.2. delete annotation to a-set mappings */
     delete
     from   t_as_annotation
     where  asann_ann_id in (select ann_global_id
                             from   t_annotation
                             where  ann_doc_id = l_doc_id
                             );

     /* 2.3 delete annotations */
     delete
     from   t_annotation
     where  ann_doc_id = l_doc_id;

     /* 3. delete annotation sets */
     delete
     from   t_annot_set
     where  as_doc_id = l_doc_id;

     /* 4. delete nodes */
     delete
     from   t_node
     where  node_doc_id = l_doc_id;

     /* 4.5 delete from corpus (if part of) */
     delete
     from   t_corpus_document
     where  cd_doc_id = l_doc_id;

     /* 5. delete document */
     delete
     from   t_document
     where  doc_id = l_doc_id;

     /* 6. delete document content */
     delete
     from   t_doc_content
     where  dc_id = l_content_id;

     /* 8. delete LR */
     delete
     from   t_lang_resource
     where  lr_id = p_lr_id;

     return true;

   END;
'
LANGUAGE 'plpgsql';
