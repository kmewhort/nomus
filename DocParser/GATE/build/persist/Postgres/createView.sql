/*
 *  DDL script for PostgreSQL 7.2
 *
 *  Copyright (c) 1995-2010, The University of Sheffield. See the file
 *  COPYRIGHT.txt in the software or at http://gate.ac.uk/gate/COPYRIGHT.txt
 *
 *  This file is part of GATE (see http://gate.ac.uk/), and is free
 *  software, licenced under the GNU Library General Public License,
 *  Version 2, June 1991 (in the distribution as file licence.html,
 *  and also available at http://gate.ac.uk/gate/licence.html).
 *
 *  Marin Dimitrov, 18/Mar/2002
 *
 *  $Id: createView.sql 12006 2009-12-01 17:24:28Z thomas_heitz $
 *
 */

drop view v_document;

create view v_document as
   select t_document.doc_content_id,
          t_document.doc_lr_id,
          t_document.doc_id,
          t_document.doc_url,
          t_document.doc_start,
          t_document.doc_end,
          t_document.doc_is_markup_aware,
          t_lang_resource.lr_id,
          t_lang_resource.lr_name,
          t_lr_type.lrtp_type,
          t_lang_resource.lr_parent_id
   from t_document,
        t_lang_resource,
        t_lr_type
   where doc_lr_id = lr_id
         and lr_type_id = lrtp_id;


drop view v_annotation_features;

create view v_annotation_features (ann_local_id,
                                   set_id,
                                   key,
                                   ft_value_type,
                                   ft_int_value,
                                   ft_float_value,
                                   ft_character_value,
                                   ft_binary_value)  as
   select t_annotation.ann_local_id,
          t_as_annotation.asann_as_id,
          t_feature_key.fk_string,
          t_feature.ft_value_type,
          t_feature.ft_int_value,
          t_feature.ft_float_value,
          t_feature.ft_character_value,
          t_feature.ft_binary_value
   from t_as_annotation,
        t_feature,
        t_annotation,
        t_feature_key
   where  ft_entity_id = ann_global_id
          and   ft_entity_type = 3
          and  ann_global_id = asann_ann_id
          and  ft_key_id = fk_id;


drop view v_annotation;

create view v_annotation (ann_local_id,
                          at_name,
                          start_offset,
                          end_offset,
                          asann_as_id)  as
   select t_annotation.ann_local_id,
          t_annotation_type.at_name,
          s_node.node_offset,
          e_node.node_offset,
          t_as_annotation.asann_as_id
   from t_as_annotation,
        t_annotation,
        t_annotation_type,
        t_node s_node,
        t_node e_node
   where asann_ann_id=ann_global_id
         and ann_at_id=at_id
         and ann_startnode_id=s_node.node_global_id
         and ann_endnode_id=e_node.node_global_id;


drop view v_content;

create view v_content as
   select t_doc_content.dc_character_content,
          t_doc_content.dc_binary_content,
          t_doc_content.dc_content_type,
          t_doc_encoding.enc_name,
          t_doc_content.dc_id,
          v_document.doc_id,
          v_document.lr_id
   from t_doc_content,
        t_doc_encoding,
        v_document
   where enc_id = dc_encoding_id
         and doc_content_id = dc_id;


drop view v_lr;

create view v_lr as
   select t_lr_type.lrtp_type,
          t_lang_resource.lr_id,
          t_lang_resource.lr_owner_user_id,
          t_lang_resource.lr_owner_group_id,
          t_lang_resource.lr_locking_user_id,
          t_lang_resource.lr_name,
          t_lang_resource.lr_access_mode,
          t_lang_resource.lr_parent_id
   from t_lr_type,
        t_lang_resource
   where lr_type_id = lrtp_id;


drop view v_annotation_set;

create view v_annotation_set as
   select t_annot_set.as_doc_id,
          t_annot_set.as_id,
          v_document.lr_id,
          t_annot_set.as_name
   from t_annot_set,
        v_document
   where as_doc_id = doc_id;







