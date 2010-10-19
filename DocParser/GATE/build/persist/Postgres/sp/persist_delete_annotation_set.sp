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
 *  $Id: persist_delete_annotation_set.sp 5582 2004-04-08 10:41:24Z valyt $
 *
 */


CREATE OR REPLACE FUNCTION persist_delete_annotation_set(int4,varchar) RETURNS boolean AS '
   DECLARE
      p_lr_id        alias for $1;
      p_set_name     alias for $2;
      l_as_id        int4;

      x_invalid_annotation_set constant varchar := ''x_invalid_annotation_set'';

   BEGIN
      /* 1. get aset ID */
      select as_id
      into   l_as_id
      from   t_annot_set aset,
             t_document  doc
      where  aset.as_name = p_set_name
             and aset.as_doc_id = doc.doc_id
             and doc.doc_lr_id = p_lr_id;

      if not FOUND then
         raise exception ''%d'',x_invalid_annotation_set;
      end if;

    
      /* 2. delete mappings */
      delete
      from   t_as_annotation
      where  asann_as_id = l_as_id;
    
      /* 1. delete annotations */
      delete
      from   t_annotation  
      where  exists (select members.asann_id
                     from   t_as_annotation members
                     where  members.asann_ann_id = t_annotation.ann_global_id
                            and members.asann_as_id = l_as_id);
    
      /* 3. delete set itself */
      delete
      from   t_annot_set
      where  as_id = l_as_id;

      /* dummy */
      return true;

   END;
'
LANGUAGE 'plpgsql';
