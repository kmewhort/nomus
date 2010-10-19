/*
 *  DDL script for Postgres SQL 7.2
 *
 *  Copyright (c) 1995-2010, The University of Sheffield. See the file
 *  COPYRIGHT.txt in the software or at http://gate.ac.uk/gate/COPYRIGHT.txt
 *
 *  This file is part of GATE (see http://gate.ac.uk/), and is free
 *  software, licenced under the GNU Library General Public License,
 *  Version 2, June 1991 (in the distribution as file licence.html,
 *  and also available at http://gate.ac.uk/gate/licence.html).
 *
 *  Marin Dimitrov, 14/Mar/2002
 *
 *  $Id: createIndex.sql 12006 2009-12-01 17:24:28Z thomas_heitz $
 *
 */


CREATE UNIQUE INDEX xt_annot_set_01 ON	t_annot_set
(
	as_doc_id			,
	as_name
)
;


CREATE UNIQUE INDEX xt_annotation_01 on  t_annotation
(
	ann_doc_id			,
	ann_local_id
)
;

CREATE UNIQUE INDEX xt_annotation_type_01 on  t_annotation_type
(
	at_name
)
;

CREATE INDEX xt_as_annotation_01 on  t_as_annotation
(
	asann_as_id
)
;

CREATE INDEX xt_as_annotation_02 on  t_as_annotation
(
	asann_ann_id
)
;

CREATE UNIQUE INDEX xt_doc_encoding_01 on  t_doc_encoding
(
	enc_name
)
;

CREATE UNIQUE INDEX xt_document_01 on  t_document
(
	doc_lr_id
)
;

CREATE INDEX xt_feature_01 on  t_feature
(
	ft_entity_id			,
	ft_entity_type
)
;

CREATE INDEX xt_feature_02 on  t_feature
(
       ft_key_id,
       ft_int_value
)
;

CREATE INDEX xt_feature_03 on  t_feature
(
       ft_key_id,
       ft_character_value
)
;


CREATE UNIQUE INDEX xt_feature_key_01 on  t_feature_key
(
	fk_string
)
;

CREATE UNIQUE INDEX xt_group_01 on  t_group
(
	grp_name
)
;

CREATE INDEX xt_lang_resource_01 on  t_lang_resource
(
	lr_locking_user_id
)
;

CREATE INDEX xt_lang_resource_02 on  t_lang_resource
(
	lr_owner_group_id
)
;

CREATE INDEX xt_lang_resource_03 on  t_lang_resource
(
	lr_owner_user_id
)
;

CREATE UNIQUE INDEX xt_lr_type_01 on  t_lr_type
(
	lrtp_type
)
;

CREATE UNIQUE INDEX xt_node_01 on  t_node
(
	node_doc_id			,
	node_local_id
)
;

CREATE UNIQUE INDEX xt_parameter_01 on	t_parameter
(
	par_key
)
;

CREATE UNIQUE INDEX xt_user_01 on  t_user
(
	usr_login
)
;

CREATE INDEX xt_user_02 on  t_user
(
	usr_pass
)
;

CREATE UNIQUE INDEX xt_user_group_01 on  t_user_group
(
	ugrp_user_id			,
	ugrp_group_id
)
;

CREATE INDEX xt_user_group_02 on  t_user_group
(
	ugrp_group_id
)
;




