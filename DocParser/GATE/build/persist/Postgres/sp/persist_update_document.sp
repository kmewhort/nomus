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
 *  $Id: persist_update_document.sp 5582 2004-04-08 10:41:24Z valyt $
 *
 */

CREATE OR REPLACE FUNCTION persist_update_document(int4,varchar,int4,int4,boolean) RETURNS boolean AS '

   DECLARE
      p_lr_id        alias for $1;
      p_url          alias for $2;
      p_start_offset alias for $3;
      p_end_offset   alias for $4;
      p_is_mrk_aware alias for $5;

      cnt int4;
      x_invalid_lr constant varchar := ''x_invalid_lr'';

   BEGIN

      select count(doc_id)
      into   cnt
      from   t_document
      where  doc_lr_id = p_lr_id;
     
      if (cnt = 0) then
         raise exception ''%d'',x_invalid_lr;
      end if;
     
      update t_document
      set    doc_url = p_url,
             doc_start = p_start_offset,
             doc_end = p_end_offset,
             doc_is_markup_aware = p_is_mrk_aware
      where  doc_lr_id = p_lr_id;

      /* dummy */
      return true;

   END;
'
LANGUAGE 'plpgsql';
