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
 *  $Id: persist_update_document_content.sp 5582 2004-04-08 10:41:24Z valyt $
 *
 */

CREATE or REPLACE FUNCTION persist_update_document_content(int4,text) RETURNS boolean AS '

   DECLARE
      p_lr_id alias for $1;
      p_content alias for $2;
      l_dc_id int4;
      x_invalid_lr constant varchar := ''x_invalid_lr'';

   BEGIN

      select doc_content_id
      into   l_dc_id
      from   t_document
      where  doc_lr_id = p_lr_id;

      if not FOUND then
         raise exception ''%'',x_invalid_lr;
      end if;

      update t_doc_content
      set    dc_character_content = p_content
      where  dc_id = l_dc_id;

      /* dummy */
      return true;

   END;
'
LANGUAGE 'plpgsql'
