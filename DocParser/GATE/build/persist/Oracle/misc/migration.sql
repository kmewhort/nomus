/*
 *  DDL script for Oracle 8.x and Oracle 9.x
 *
 *  Copyright (c) 1995-2010, The University of Sheffield. See the file
 *  COPYRIGHT.txt in the software or at http://gate.ac.uk/gate/COPYRIGHT.txt
 *
 *  This file is part of GATE (see http://gate.ac.uk/), and is free
 *  software, licenced under the GNU Library General Public License,
 *  Version 2, June 1991 (in the distribution as file licence.html,
 *  and also available at http://gate.ac.uk/gate/licence.html).
 *
 *  Marin Dimitrov, 24/Feb/2002
 *
 *  $Id: migration.sql 12006 2009-12-01 17:24:28Z thomas_heitz $
 *
 */

alter table gateadmin.t_annotation move tablespace "GATEDATA01";
alter index gateadmin.xpkt_annotation rebuild tablespace "GATEINDEX01";
alter index gateadmin.xt_annotation_01 rebuild tablespace "GATEINDEX01";

alter table gateadmin.t_annotation_type move tablespace "GATEDATA01";
alter index gateadmin.xpkt_annotation_type rebuild tablespace "GATEINDEX01";
alter index gateadmin.xt_annotation_type_01 rebuild tablespace "GATEINDEX01";

alter table gateadmin.t_annot_set move tablespace "GATEDATA01";
alter index gateadmin.xpkt_annot_set rebuild tablespace "GATEINDEX01";
alter index gateadmin.xt_annot_set_01 rebuild tablespace "GATEINDEX01";

alter table gateadmin.t_as_annotation move tablespace "GATEDATA01";
alter index gateadmin.xpkt_as_annotation rebuild tablespace "GATEINDEX01";
alter index gateadmin.xt_as_annotation_01 rebuild tablespace "GATEINDEX01";
alter index gateadmin.xt_as_annotation_02 rebuild tablespace "GATEINDEX01";

alter table gateadmin.t_corpus move tablespace "GATEDATA01";
alter index gateadmin.xpkt_corpus rebuild tablespace "GATEINDEX01";

alter table gateadmin.t_corpus_document move tablespace "GATEDATA01";
alter index gateadmin.xpkt_corpus_document rebuild tablespace "GATEINDEX01";

alter table gateadmin.t_document move tablespace "GATEDATA01";
alter index gateadmin.xpkt_document rebuild tablespace "GATEINDEX01";
alter index gateadmin.xt_document_01 rebuild tablespace "GATEINDEX01";

alter table gateadmin.t_doc_content
      move tablespace "GATEDATA01"
      lob (DC_CHARACTER_CONTENT,DC_BINARY_CONTENT) store as (tablespace "GATELOB01");
alter index gateadmin.xpkt_doc_content rebuild tablespace "GATEINDEX01";

alter table gateadmin.t_doc_encoding move tablespace "GATEDATA01";
alter index gateadmin.xpkt_doc_encoding rebuild tablespace "GATEINDEX01";
alter index gateadmin.xt_doc_encoding_01 rebuild tablespace "GATEINDEX01";

alter table gateadmin.t_feature_key move tablespace "GATEDATA01";
alter index gateadmin.xpkt_feature_key rebuild tablespace "GATEINDEX01";
--alter index gateadmin.i_feature_key_01 rename to xt_feature_key_01;
alter index gateadmin.xt_feature_key_01 rebuild tablespace "GATEINDEX01";

alter table gateadmin.t_group move tablespace "GATEDATA01";
alter index gateadmin.xpkt_group rebuild tablespace "GATEINDEX01";
alter index gateadmin.xt_group_01 rebuild tablespace "GATEINDEX01";

alter table gateadmin.t_lang_resource move tablespace "GATEDATA01";
alter index gateadmin.xpkt_lang_resource rebuild tablespace "GATEINDEX01";
alter index gateadmin.xt_lang_resource_01 rebuild tablespace "GATEINDEX01";
alter index gateadmin.xt_lang_resource_02 rebuild tablespace "GATEINDEX01";
alter index gateadmin.xt_lang_resource_03 rebuild tablespace "GATEINDEX01";

alter table gateadmin.t_lr_type move tablespace "GATEDATA01";
alter index gateadmin.xpkt_lr_type rebuild tablespace "GATEINDEX01";
alter index gateadmin.xt_lr_type_01 rebuild tablespace "GATEINDEX01";

alter table gateadmin.t_node move tablespace "GATEDATA01";
alter index gateadmin.xpkt_node rebuild tablespace "GATEINDEX01";
alter index gateadmin.xt_node_01 rebuild tablespace "GATEINDEX01";

alter table gateadmin.t_parameter move tablespace "GATEDATA01";
alter index gateadmin.xpkt_parameter rebuild tablespace "GATEINDEX01";
alter index gateadmin.xt_parameter_01 rebuild tablespace "GATEINDEX01";

alter table gateadmin.t_user move tablespace "GATEDATA01";
alter index gateadmin.xpkt_user rebuild tablespace "GATEINDEX01";
alter index gateadmin.xt_user_01 rebuild tablespace "GATEINDEX01";

alter table gateadmin.t_user_group move tablespace "GATEDATA01";
alter index gateadmin.xpkt_user_group rebuild tablespace "GATEINDEX01";
alter index gateadmin.xt_user_group_01 rebuild tablespace "GATEINDEX01";
alter index gateadmin.xt_user_group_02 rebuild tablespace "GATEINDEX01";

alter table gateadmin.t_feature
      move tablespace "GATEDATA01"
      lob (FT_BINARY_VALUE,FT_LONG_CHARACTER_VALUE) store as (tablespace "GATELOB01");
alter index gateadmin.xpkt_feature rebuild tablespace "GATEINDEX01";
alter index gateadmin.xt_feature_01 rebuild tablespace "GATEINDEX01";

alter user GATEADMIN default tablespace "GATEDATA01";
alter user GATEUSER default tablespace "GATEDATA01";
