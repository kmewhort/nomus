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
 *  Marin Dimitrov, 01/Oct/2001
 *
 *  $Id: grants.sql 12006 2009-12-01 17:24:28Z thomas_heitz $
 *
 *      DO NOT EDIT !
 *      THIS FILE IS GENERATED FROM THE generateGrants.sql script
 */




grant execute on GATEADMIN.ERROR to GATE_USER_ROLE;                                   
grant execute on GATEADMIN.PERSIST to GATE_USER_ROLE;                                 
grant execute on GATEADMIN.SECURITY to GATE_USER_ROLE;                                

grant select on GATEADMIN.SEQ_ANNOTATION to GATE_USER_ROLE;
grant select on GATEADMIN.SEQ_ANNOTATION_TYPE to GATE_USER_ROLE;
grant select on GATEADMIN.SEQ_ANNOT_SET to GATE_USER_ROLE;
grant select on GATEADMIN.SEQ_AS_ANNOTATION to GATE_USER_ROLE;
grant select on GATEADMIN.SEQ_CORPUS to GATE_USER_ROLE;
grant select on GATEADMIN.SEQ_CORPUS_DOCUMENT to GATE_USER_ROLE;
grant select on GATEADMIN.SEQ_DOCUMENT to GATE_USER_ROLE;
grant select on GATEADMIN.SEQ_DOC_CONTENT to GATE_USER_ROLE;
grant select on GATEADMIN.SEQ_DOC_ENCODING to GATE_USER_ROLE;
grant select on GATEADMIN.SEQ_FEATURE to GATE_USER_ROLE;
grant select on GATEADMIN.SEQ_FEATURE_KEY to GATE_USER_ROLE;
grant select on GATEADMIN.SEQ_GROUP to GATE_USER_ROLE;
grant select on GATEADMIN.SEQ_LANG_RESOURCE to GATE_USER_ROLE;
grant select on GATEADMIN.SEQ_LR_TYPE to GATE_USER_ROLE;
grant select on GATEADMIN.SEQ_NODE to GATE_USER_ROLE;
grant select on GATEADMIN.SEQ_TIMESTAMP to GATE_USER_ROLE;
grant select on GATEADMIN.SEQ_USER to GATE_USER_ROLE;
grant select on GATEADMIN.SEQ_USER_GROUP to GATE_USER_ROLE;

grant select,insert,update,delete on GATEADMIN.T_ANNOTATION to GATE_USER_ROLE;
grant select,insert,update,delete on GATEADMIN.T_ANNOTATION_TYPE to GATE_USER_ROLE;
grant select,insert,update,delete on GATEADMIN.T_ANNOT_SET to GATE_USER_ROLE;
grant select,insert,update,delete on GATEADMIN.T_AS_ANNOTATION to GATE_USER_ROLE;
grant select,insert,update,delete on GATEADMIN.T_CORPUS to GATE_USER_ROLE;
grant select,insert,update,delete on GATEADMIN.T_CORPUS_DOCUMENT to GATE_USER_ROLE;
grant select,insert,update,delete on GATEADMIN.T_DOCUMENT to GATE_USER_ROLE;
grant select,insert,update,delete on GATEADMIN.T_DOC_CONTENT to GATE_USER_ROLE;
grant select,insert,update,delete on GATEADMIN.T_DOC_ENCODING to GATE_USER_ROLE;
grant select,insert,update,delete on GATEADMIN.T_FEATURE to GATE_USER_ROLE;
grant select,insert,update,delete on GATEADMIN.T_FEATURE_KEY to GATE_USER_ROLE;
grant select,insert,update,delete on GATEADMIN.T_GROUP to GATE_USER_ROLE;
grant select,insert,update,delete on GATEADMIN.T_LANG_RESOURCE to GATE_USER_ROLE;
grant select,insert,update,delete on GATEADMIN.T_LR_TYPE to GATE_USER_ROLE;
grant select,insert,update,delete on GATEADMIN.T_NODE to GATE_USER_ROLE;
grant select,insert,update,delete on GATEADMIN.T_USER to GATE_USER_ROLE;              
grant select,insert,update,delete on GATEADMIN.T_USER_GROUP to GATE_USER_ROLE;        
grant select,insert,update,delete on GATEADMIN.T_PARAMETER to GATE_USER_ROLE;        

grant select on GATEADMIN.V_ANNOTATION to GATE_USER_ROLE;
grant select on GATEADMIN.V_ANNOTATION_FEATURES to GATE_USER_ROLE;
grant select on GATEADMIN.V_ANNOTATION_SET to GATE_USER_ROLE;
grant select on GATEADMIN.V_CONTENT to GATE_USER_ROLE;
grant select on GATEADMIN.V_DOCUMENT to GATE_USER_ROLE;
grant select on GATEADMIN.V_LR to GATE_USER_ROLE;
