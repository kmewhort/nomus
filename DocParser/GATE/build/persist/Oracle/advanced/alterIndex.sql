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
 *  Marin Dimitrov, 31/Jan/2001
 *
 *  $Id: alterIndex.sql 12006 2009-12-01 17:24:28Z thomas_heitz $
 *
 */

ALTER INDEX xpkt_annotation REBUILD 
	TABLESPACE gateindex01
	PCTFREE 0
	REVERSE;

ALTER INDEX xpkt_annotation_type REBUILD 
	TABLESPACE gateindex01
	PCTFREE 0;

ALTER INDEX xpkt_annot_set REBUILD 
	TABLESPACE gateindex01
	PCTFREE 0;

ALTER INDEX xpkt_as_annotation REBUILD 
	TABLESPACE gateindex01
	PCTFREE 0;

ALTER INDEX xpkt_corpus REBUILD 
	TABLESPACE gateindex01
	PCTFREE 0;


ALTER INDEX xpkt_corpus_document REBUILD 
	TABLESPACE gateindex01
	PCTFREE 0;


ALTER INDEX xpkt_document REBUILD 
	TABLESPACE gateindex01
	PCTFREE 0;


ALTER INDEX xpkt_doc_content REBUILD TABLESPACE gateindex01;

ALTER INDEX xpkt_doc_encoding REBUILD 
	TABLESPACE gateindex01
	PCTFREE 0;


ALTER INDEX xpkt_feature REBUILD 
	TABLESPACE gateindex01
	PCTFREE 0;

ALTER INDEX xpkt_feature_key REBUILD 
	TABLESPACE gateindex01
	PCTFREE 0;

ALTER INDEX xpkt_group REBUILD 
	TABLESPACE gateindex01
	PCTFREE 0;

ALTER INDEX xpkt_lang_resource REBUILD 
	TABLESPACE gateindex01
	PCTFREE 0;


ALTER INDEX xpkt_lr_type REBUILD TABLESPACE gateindex01;

ALTER INDEX xpkt_node REBUILD 
	TABLESPACE gateindex01
	PCTFREE 0;

ALTER INDEX xpkt_parameter REBUILD TABLESPACE gateindex01;

ALTER INDEX xpkt_user REBUILD TABLESPACE gateindex01;

ALTER INDEX xpkt_user_group REBUILD TABLESPACE gateindex01;




