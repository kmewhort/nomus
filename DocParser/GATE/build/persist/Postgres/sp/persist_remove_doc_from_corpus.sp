/*
 *
 *  Copyright (c) 1998-2002, The University of Sheffield.
 *
 *  This file is part of GATE (see http://gate.ac.uk/), and is free
 *  software, licenced under the GNU Library General Public License,
 *  Version 2, June 1991 (in the distribution as file licence.html,
 *  and also available at http://gate.ac.uk/gate/licence.html).
 *
 *  Marin Dimitrov, 25/Mar/2002
 *
 *  $Id: persist_remove_doc_from_corpus.sp 5582 2004-04-08 10:41:24Z valyt $
 *
 */


CREATE OR REPLACE FUNCTION persist_remove_doc_from_corpus(int4,int4) RETURNS boolean AS '
   DECLARE
      p_doc_lrid     alias for $1;
      p_corp_lrid    alias for $2;
      cnt       int4;
      l_corp_id int4;
      l_doc_id  int4;

      x_invalid_lr constant varchar := ''x_invalid_lr'';

   BEGIN
      /* 1. get the doc_id */
      select doc_id
      into   l_doc_id
      from   t_document
      where  doc_lr_id = p_doc_lrid;

      if not FOUND then
         raise exception ''%'',x_invalid_lr;
      end if;
     
      /* 2. get the corpus ID */
      select corp_id
      into   l_corp_id
      from   t_corpus
      where  corp_lr_id = p_corp_lrid;

      if not FOUND then
         raise exception ''%'',x_invalid_lr;
      end if;
     
      /* 3. delete the doc-to-corpus mapping */
      delete
      from   t_corpus_document
      where  cd_corp_id = l_corp_id
             and cd_doc_id = l_doc_id;

      /* dummy */
      return true;

   END;
'
LANGUAGE 'plpgsql';
