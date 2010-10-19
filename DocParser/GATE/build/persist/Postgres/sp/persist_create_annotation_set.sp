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
 *  $Id: persist_create_annotation_set.sp 5582 2004-04-08 10:41:24Z valyt $
 *
 */

CREATE OR REPLACE FUNCTION persist_create_annotation_set(int4,varchar) RETURNS int4 AS '

   DECLARE
      p_lr_id alias for $1;
      p_as_name alias for $2;

      l_doc_id int4;
      x_invalid_lr constant varchar := ''x_invalid_lr'';

   BEGIN
      /* 1. get the DOC_ID */
      select doc_id
      into   l_doc_id
      from   t_document
      where  doc_lr_id = p_lr_id;

      if not FOUND then
         raise exception ''%d'',x_invalid_lr;
      end if;

      /* 2. create an entry for the set */
      insert into t_annot_set(as_id,
                              as_doc_id,
                              as_name)
      values(nextval(''seq_annot_set''),
             l_doc_id,
             p_as_name);

      return currval(''seq_annot_set'');

   END;
'
LANGUAGE 'plpgsql';
