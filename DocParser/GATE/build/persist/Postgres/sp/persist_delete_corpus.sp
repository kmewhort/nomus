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
 *  $Id: persist_delete_corpus.sp 5582 2004-04-08 10:41:24Z valyt $
 *
 */

CREATE OR REPLACE FUNCTION persist_delete_corpus(int4) RETURNS boolean AS '

   DECLARE
      p_lr_id alias for $1;
      l_corp_id int4;

      x_invalid_lr constant varchar := ''x_invalid_lr'';

   BEGIN
      /* 0. get corp id */
      select corp_id
      into   l_corp_id
      from   t_corpus
      where  corp_lr_id = p_lr_id;

      if not FOUND then
         raise exception ''%d'', x_invalid_lr;
      end if;

      /* 1. delete from t_corpus_document */
      delete
      from   t_corpus_document
      where  cd_corp_id = l_corp_id;

      /* 2. delete t_corpus */
      delete
      from   t_corpus
      where  corp_id = l_corp_id;

      /* 3. delete from t_lang_resource */
      delete
      from   t_lang_resource
      where  lr_id = p_lr_id;

      /* dummy */
      return true;

   END;
'
LANGUAGE 'plpgsql';
