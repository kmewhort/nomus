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
 *  $Id: persist_create_annotation.sp 5582 2004-04-08 10:41:24Z valyt $
 *
 */

CREATE OR REPLACE FUNCTION persist_create_annotation(int4,int4,int4,int4,int4,int4,int4,varchar) RETURNS int4 AS '

   DECLARE
      p_lr_id alias for $1;
      p_ann_local_id  alias for $2;
      p_as_id  alias for $3;
      p_node_start_lid   alias for $4;
      p_node_start_offset alias for $5;
      p_node_end_lid      alias for $6;
      p_node_end_offset   alias for $7;
      p_ann_type         alias for $8;

      l_doc_id               int4;
      l_start_node_gid       int4;
      l_end_node_gid         int4;
      l_ann_type_id          int4;
      cnt                    int4;
      l_ann_global_id        int4;
      l_test                 boolean;

      x_invalid_lr constant varchar := ''x_invalid_lr'';

   BEGIN
      /* 0. get the DOC_ID */
      select doc_id
      into   l_doc_id
      from   t_document
      where  doc_lr_id = p_lr_id;

      if not FOUND then
         raise exception ''%d'',x_invalid_lr;
      end if;

      /* get the annotation id */
      select ann_global_id
      into   l_ann_global_id
      from   t_annotation
      where  ann_doc_id = l_doc_id
             and ann_local_id = p_ann_local_id;

      if not FOUND then
         /* new annotation, create it
            -- 1. store nodes in DB only if they,re new
            -- (nodes are shared between annotations so the chances 
            -- a node is used by more than one annotation is high)
         */

         /*
            -- 1.1. start node
            -- select the global ID
         */
         select node_global_id
         into   l_start_node_gid
         from   t_node
         where  node_doc_id = l_doc_id
                and node_local_id = p_node_start_lid;

         if not FOUND then
            /* new node, add to DB */
            insert into t_node(node_global_id,
                               node_doc_id,
                               node_local_id,
                               node_offset)
            values (nextval(''seq_node''),
                    l_doc_id,
                    p_node_start_lid,
                    p_node_start_offset);

            /* get ID */
            l_start_node_gid := currval(''seq_node'');

         end if;

         /* 1.2. end node
         -- select the global ID */

         select node_global_id
         into   l_end_node_gid
         from   t_node
         where  node_doc_id = l_doc_id
                and node_local_id = p_node_end_lid;

         if not FOUND then
            /* -- add to DB */
            insert into t_node(node_global_id,
                               node_doc_id,
                               node_local_id,
                               node_offset)
            values (nextval(''seq_node''),
                    l_doc_id,
                    p_node_end_lid,
                    p_node_end_offset);
            /* get ID */
            l_end_node_gid := currval(''seq_node'');

         end if;

         /* 2. store annotation in DB */

         /* 2.1 get the anotation type ID */
         select at_id
         into   l_ann_type_id
         from   t_annotation_type
         where  at_name = p_ann_type;

         if not FOUND then
            /* 2.2 if there is no such type, then create one
                 --oops, new type
                 --add it
            */
            insert into t_annotation_type(at_id,
                                          at_name)
            values (nextval(''seq_annotation_type''),
                    p_ann_type);

            /* get ID */
            l_ann_type_id := currval(''seq_annotation_type'');

         end if;

         /* 2.3 insert annotation */
         insert into t_annotation(ann_global_id,
                                  ann_doc_id,
                                  ann_local_id,
                                  ann_at_id,
                                  ann_startnode_id,
                                  ann_endnode_id)
         values (nextval(''seq_annotation''),
                 l_doc_id,
                 p_ann_local_id,
                 l_ann_type_id,
                 l_start_node_gid,
                 l_end_node_gid);

         l_ann_global_id :=currval(''seq_annotation'');

      end if;

     /* 3. create a annotation-to-aset mapping */
     insert into t_as_annotation(asann_id,
                                 asann_ann_id,
                                 asann_as_id)
     values (nextval(''seq_annotation''),
             l_ann_global_id,
             p_as_id);


     return l_ann_global_id;

   END;
'
LANGUAGE 'plpgsql';
