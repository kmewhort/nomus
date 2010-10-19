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
 *  Marin Dimitrov, 19/Sep/2001
 *
 *  auto generated: Wed Jan 23 16:08:36 2002
 *
 *  $Id: createSequence.sql 12006 2009-12-01 17:24:28Z thomas_heitz $
 *
 */


DROP SEQUENCE SEQ_TIMESTAMP ;
CREATE SEQUENCE SEQ_TIMESTAMP
INCREMENT BY 1
START WITH 1
NOMAXVALUE
NOMINVALUE
NOCYCLE
NOORDER
;
DROP SEQUENCE SEQ_ANNOT_SET;

CREATE SEQUENCE SEQ_ANNOT_SET INCREMENT BY 1 START WITH 1000;





DROP SEQUENCE SEQ_ANNOTATION;

CREATE SEQUENCE SEQ_ANNOTATION INCREMENT BY 1 START WITH 1000 cache 200;





DROP SEQUENCE SEQ_ANNOTATION_TYPE;

CREATE SEQUENCE SEQ_ANNOTATION_TYPE INCREMENT BY 1 START WITH 1000;





DROP SEQUENCE SEQ_AS_ANNOTATION;

CREATE SEQUENCE SEQ_AS_ANNOTATION INCREMENT BY 1 START WITH 1000 cache 200;





DROP SEQUENCE SEQ_CORPUS;

CREATE SEQUENCE SEQ_CORPUS INCREMENT BY 1 START WITH 1000;





DROP SEQUENCE SEQ_CORPUS_DOCUMENT;

CREATE SEQUENCE SEQ_CORPUS_DOCUMENT INCREMENT BY 1 START WITH 1000;





DROP SEQUENCE SEQ_DOC_CONTENT;

CREATE SEQUENCE SEQ_DOC_CONTENT INCREMENT BY 1 START WITH 1000;





DROP SEQUENCE SEQ_DOC_ENCODING;

CREATE SEQUENCE SEQ_DOC_ENCODING INCREMENT BY 1 START WITH 1000;





DROP SEQUENCE SEQ_DOCUMENT;

CREATE SEQUENCE SEQ_DOCUMENT INCREMENT BY 1 START WITH 1000;





DROP SEQUENCE SEQ_FEATURE;

CREATE SEQUENCE SEQ_FEATURE INCREMENT BY 1 START WITH 1000 cache 200;





DROP SEQUENCE SEQ_FEATURE_KEY;

CREATE SEQUENCE SEQ_FEATURE_KEY INCREMENT BY 1 START WITH 1000;





DROP SEQUENCE SEQ_GROUP;

CREATE SEQUENCE SEQ_GROUP INCREMENT BY 1 START WITH 1000;





DROP SEQUENCE SEQ_LANG_RESOURCE;

CREATE SEQUENCE SEQ_LANG_RESOURCE INCREMENT BY 1 START WITH 1000;





DROP SEQUENCE SEQ_LR_TYPE;

CREATE SEQUENCE SEQ_LR_TYPE INCREMENT BY 1 START WITH 1000;





DROP SEQUENCE SEQ_NODE;

CREATE SEQUENCE SEQ_NODE INCREMENT BY 1 START WITH 1000 cache 200;





DROP SEQUENCE SEQ_PARAMETER;

CREATE SEQUENCE SEQ_PARAMETER INCREMENT BY 1 START WITH 1000;





DROP SEQUENCE SEQ_USER;

CREATE SEQUENCE SEQ_USER INCREMENT BY 1 START WITH 1000;





DROP SEQUENCE SEQ_USER_GROUP;

CREATE SEQUENCE SEQ_USER_GROUP INCREMENT BY 1 START WITH 1000;








