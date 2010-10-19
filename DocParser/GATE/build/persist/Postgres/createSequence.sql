/*  DDL script for PostgreSQL 7.2
 *
 *  Copyright (c) 1995-2010, The University of Sheffield. See the file
 *  COPYRIGHT.txt in the software or at http://gate.ac.uk/gate/COPYRIGHT.txt
 *
 *  This file is part of GATE (see http://gate.ac.uk/), and is free
 *  software, licenced under the GNU Library General Public License,
 *  Version 2, June 1991 (in the distribution as file licence.html,
 *  and also available at http://gate.ac.uk/gate/licence.html).
 *
 *  Marin Dimitrov, 12/Mar/2002
 *
 *
 *  $Id: createSequence.sql 12006 2009-12-01 17:24:28Z thomas_heitz $
 *
 */

drop sequence  seq_feature ;
create sequence  seq_feature
       start 1
       increment 1
       maxvalue 2147483647
       minvalue 1
       cache 20;

drop sequence  seq_feature_key ;
create sequence  seq_feature_key
       start 1
       increment 1
       maxvalue 2147483647
       minvalue 1
       cache 5;

drop sequence   seq_user ;
create sequence  seq_user
       start 1000
       increment 1
       maxvalue 2147483647
       minvalue 1000
       cache 1;

drop sequence    seq_group ;
create sequence  seq_group
       start 1000
       increment 1
       maxvalue 2147483647
       minvalue 1000
       cache 1;

drop sequence    seq_user_group ;
create sequence  seq_user_group
       start 1000
       increment 1
       maxvalue 2147483647
       minvalue 1000
       cache 1;

drop sequence    seq_doc_encoding ;
create sequence  seq_doc_encoding
       start 1
       increment 1
       maxvalue 2147483647
       minvalue 1
       cache 1;

drop sequence    seq_doc_content ;
create sequence  seq_doc_content
       start 1
       increment 1
       maxvalue 2147483647
       minvalue 1
       cache 5;

drop sequence    seq_lr_type ;
create sequence  seq_lr_type
       start 1
       increment 1
       maxvalue 2147483647
       minvalue 1
       cache 1;

drop sequence    seq_lang_resource ;
create sequence  seq_lang_resource
       start 1
       increment 1
       maxvalue 2147483647
       minvalue 1
       cache 5;

drop sequence    seq_document ;
create sequence  seq_document
       start 1
       increment 1
       maxvalue 2147483647
       minvalue 1
       cache 5;

drop sequence    seq_node ;
create sequence  seq_node
       start 1
       increment 1
       maxvalue 2147483647
       minvalue 1
       cache 20 ;

drop sequence    seq_annotation_type ;
create sequence  seq_annotation_type
       start 1
       increment 1
       maxvalue 2147483647
       minvalue 1
       cache 1;

drop sequence  seq_annotation ;
create sequence  seq_annotation
       start 1
       increment 1
       maxvalue 2147483647
       minvalue 1
       cache 20 ;

drop sequence  seq_annot_set ;
create sequence  seq_annot_set
       start 1
       increment 1
       maxvalue 2147483647
       minvalue 1
       cache 20 ;

drop sequence  seq_as_annotation ;
create sequence  seq_as_annotation
       start 1
       increment 1
       maxvalue 2147483647
       minvalue 1
       cache 20 ;

drop sequence  seq_corpus ;
create sequence  seq_corpus
       start 1
       increment 1
       maxvalue 2147483647
       minvalue 1
       cache 5;


drop sequence  seq_corpus_document ;
create sequence  seq_corpus_document
       start 1
       increment 1
       maxvalue 2147483647
       minvalue 1
       cache 5;

drop sequence  seq_parameter ;
  create sequence  seq_parameter
       start 1
       increment 1
       maxvalue 2147483647
       minvalue 1
       cache 1;
