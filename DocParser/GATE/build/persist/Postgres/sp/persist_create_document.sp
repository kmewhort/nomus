/*
 *  persist.bdy
 *
 *  Copyright (c) 1998-2002, The University of Sheffield.
 *
 *  This file is part of GATE (see http://gate.ac.uk/), and is free
 *  software, licenced under the GNU Library General Public License,
 *  Version 2, June 1991 (in the distribution as file licence.html,
 *  and also available at http://gate.ac.uk/gate/licence.html).
 *
 *  Marin Dimitrov, 20/Mar/2002
 *
 *  $Id: persist_create_document.sp 5582 2004-04-08 10:41:24Z valyt $
 *
 */


CREATE OR REPLACE FUNCTION persist_create_document(int4,varchar,varchar,int4,int4,boolean,int4)
RETURNS int4 AS   '

   DECLARE

      /* in parameters */
      p_lr_id alias for $1;
      p_url alias for $2;
      p_encoding alias for $3;
      p_start_offset alias for $4;
      p_end_offset alias for $5;
      p_is_mrk_aware alias for $6;
      p_corpus_id alias for $7;

      /* local vars */
      l_encoding_id int4;
      l_encoding varchar(16);
      cnt int4;
      l_content_id int4;
      l_doc_id int4;

      C_CHARACTER_CONTENT  constant int4 := 1;
      C_BINARY_CONTENT     constant int4 := 2;
      C_EMPTY_CONTENT      constant int4 := 3;


   BEGIN

      /* -1. if encoding is null, then set it to UTF8 */
      l_encoding := p_encoding;

      if (l_encoding is null) then
         l_encoding := ''--!--'';
      end if;

      /* 0. get encoding ID if any, otherwise create a new
        entry in T_DOC_ENCODING */
      select count(enc_id)
      into   cnt
      from   t_doc_encoding
      where  enc_name = l_encoding;

      if (cnt = 0) then
         /* oops new encoding
         add it */
         insert into t_doc_encoding(enc_id,
                                      enc_name)
         values (nextval(''seq_doc_encoding''),l_encoding);

         /* get the ID */
         select currval(''seq_doc_encoding'')
         into l_encoding_id;

      else

         /* get encoding id */
         select enc_id
         into   l_encoding_id
         from   t_doc_encoding
         where  enc_name = l_encoding;

      end if;

      /* --1. create a document_content entry */
      insert into t_doc_content(dc_id,
                               dc_encoding_id,
                               dc_character_content,
                               dc_binary_content,
                               dc_content_type)
      values(nextval(''seq_doc_content''),
             l_encoding_id,
             null /*empty_clob()*/,
             null /*empty_blob()*/,
             C_EMPTY_CONTENT /*persist.EMPTY_CONTENT*/);

      /* get the ID */
      select currval(''seq_doc_content'')
      into l_content_id;

      /* --2. create a document entry */
      insert into t_document(doc_id,
                             doc_content_id,
                             doc_lr_id,
                             doc_url,
                             doc_start,
                             doc_end,
                             doc_is_markup_aware)
      values(nextval(''seq_document''),
             l_content_id,
             p_lr_id,
             p_url,
             p_start_offset,
             p_end_offset,
             p_is_mrk_aware);
      /* get the ID */
      select currval(''seq_document'')
      into l_doc_id;

      /* --3. if part of a corpus create a corpus_document entry */
      if (p_corpus_id is not null) then
         insert into t_corpus_document(cd_id,
                                       cd_corp_id,
                                       cd_doc_id)
         values (nextval(''seq_document''),
                 p_corpus_id,
                 l_doc_id);
      end if;

      RETURN l_doc_id;

   END;
' LANGUAGE 'plpgsql';
