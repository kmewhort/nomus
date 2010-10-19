/*
 *  grants.sql
 *
 *  Copyright (c) 1995-2010, The University of Sheffield. See the file
 *  COPYRIGHT.txt in the software or at http://gate.ac.uk/gate/COPYRIGHT.txt
 *
 *  This file is part of GATE (see http://gate.ac.uk/), and is free
 *  software, licenced under the GNU Library General Public License,
 *  Version 2, June 1991 (in the distribution as file licence.html,
 *  and also available at http://gate.ac.uk/gate/licence.html).
 *
 *  Marin Dimitrov, 27/Mar/2002
 *
 *  $Id: grants.sql 12006 2009-12-01 17:24:28Z thomas_heitz $
 *
 */




grant select,update on  SEQ_ANNOTATION to group GATE_USER_GROUP;
grant select,update on  SEQ_ANNOTATION_TYPE to group GATE_USER_GROUP;
grant select,update on  SEQ_ANNOT_SET to group GATE_USER_GROUP;
grant select, update on  SEQ_AS_ANNOTATION to group GATE_USER_GROUP;
grant select, update on  SEQ_CORPUS to group GATE_USER_GROUP;
grant select, update on  SEQ_CORPUS_DOCUMENT to group GATE_USER_GROUP;
grant select, update on  SEQ_DOCUMENT to group GATE_USER_GROUP;
grant select, update on  SEQ_DOC_CONTENT to group GATE_USER_GROUP;
grant select, update on  SEQ_DOC_ENCODING to group GATE_USER_GROUP;
grant select, update on  SEQ_FEATURE to group GATE_USER_GROUP;
grant select, update on  SEQ_FEATURE_KEY to group GATE_USER_GROUP;
grant select, update on  SEQ_GROUP to group GATE_USER_GROUP;
grant select, update on  SEQ_LANG_RESOURCE to group GATE_USER_GROUP;
grant select, update on  SEQ_LR_TYPE to group GATE_USER_GROUP;
grant select, update on  SEQ_NODE to group GATE_USER_GROUP;
grant select, update on  SEQ_USER to group GATE_USER_GROUP;
grant select, update on  SEQ_USER_GROUP to group GATE_USER_GROUP;

grant select,insert,update,delete on  T_ANNOTATION to group GATE_USER_GROUP;
grant select,insert,update,delete on  T_ANNOTATION_TYPE to group GATE_USER_GROUP;
grant select,insert,update,delete on  T_ANNOT_SET to group GATE_USER_GROUP;
grant select,insert,update,delete on  T_AS_ANNOTATION to group GATE_USER_GROUP;
grant select,insert,update,delete on  T_CORPUS to group GATE_USER_GROUP;
grant select,insert,update,delete on  T_CORPUS_DOCUMENT to group GATE_USER_GROUP;
grant select,insert,update,delete on  T_DOCUMENT to group GATE_USER_GROUP;
grant select,insert,update,delete on  T_DOC_CONTENT to group GATE_USER_GROUP;
grant select,insert,update,delete on  T_DOC_ENCODING to group GATE_USER_GROUP;
grant select,insert,update,delete on  T_FEATURE to group GATE_USER_GROUP;
grant select,insert,update,delete on  T_FEATURE_KEY to group GATE_USER_GROUP;
grant select,insert,update,delete on  T_GROUP to group GATE_USER_GROUP;
grant select,insert,update,delete on  T_LANG_RESOURCE to group GATE_USER_GROUP;
grant select,insert,update,delete on  T_LR_TYPE to group GATE_USER_GROUP;
grant select,insert,update,delete on  T_NODE to group GATE_USER_GROUP;
grant select,insert,update,delete on  T_USER to group GATE_USER_GROUP;
grant select,insert,update,delete on  T_USER_GROUP to group GATE_USER_GROUP;
grant select,insert,update,delete on  T_PARAMETER to group GATE_USER_GROUP;

grant select on  V_ANNOTATION to group GATE_USER_GROUP;
grant select on  V_ANNOTATION_FEATURES to group GATE_USER_GROUP;
grant select on  V_ANNOTATION_SET to group GATE_USER_GROUP;
grant select on  V_CONTENT to group GATE_USER_GROUP;
grant select on  V_DOCUMENT to group GATE_USER_GROUP;
grant select on  V_LR to group GATE_USER_GROUP;

/*
 grant select on persist_create_document to group GATE_USER_GROUP;
 grant select on persist_create_lr to group GATE_USER_GROUP;
 grant select on persist_delete_corpus to group GATE_USER_GROUP;
 grant select on persist_delete_document to group GATE_USER_GROUP;
 grant select on persist_create_annotation_set to group GATE_USER_GROUP;
 grant select on persist_create_annotation to group GATE_USER_GROUP;
 grant select on persist_create_corpus to group GATE_USER_GROUP;
 grant select on persist_is_valid_feature_type to group GATE_USER_GROUP;
 grant select on persist_create_feature to group GATE_USER_GROUP;
 grant select on persist_change_content_type to group GATE_USER_GROUP;
 grant select on persist_update_lr to group GATE_USER_GROUP;
 grant select on persist_update_document to group GATE_USER_GROUP;
 grant select on persist_delete_features to group GATE_USER_GROUP;
 grant select on persist_delete_annotation to group GATE_USER_GROUP;
 grant select on persist_unlock_lr to group GATE_USER_GROUP;
 grant select on persist_delete_annotation_set to group GATE_USER_GROUP;
 grant select on persist_add_document_to_corpus to group GATE_USER_GROUP;
 grant select on persist_remove_doc_from_corpus to group GATE_USER_GROUP;
 grant select on persist_lock_lr to group GATE_USER_GROUP;
 grant select on persist_update_document_content to group GATE_USER_GROUP;
 
 grant select on security_is_member_of_group to group GATE_USER_GROUP;
 grant select on security_set_group_name to group GATE_USER_GROUP;
 grant select on security_add_user_to_group to group GATE_USER_GROUP;
 grant select on security_remove_user_from_group to group GATE_USER_GROUP;
 grant select on security_set_user_name to group GATE_USER_GROUP;
 grant select on security_is_valid_security_data to group GATE_USER_GROUP;
 grant select on security_set_user_password to group GATE_USER_GROUP;
 grant select on security_create_group to group GATE_USER_GROUP;
 grant select on security_delete_group to group GATE_USER_GROUP;
 grant select on security_create_user to group GATE_USER_GROUP;
 grant select on security_delete_user to group GATE_USER_GROUP;
 grant select on security_login to group GATE_USER_GROUP;
 grant select on security_has_access_to_lr to group GATE_USER_GROUP;
 grant select on security_can_delete_group to group GATE_USER_GROUP;
 grant select on security_can_delete_user to group GATE_USER_GROUP;
*/
