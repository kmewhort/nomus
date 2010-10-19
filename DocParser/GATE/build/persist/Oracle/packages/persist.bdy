create or replace package body persist is

/*
 *  persist.bdy
 *
 *  Copyright (c) 1998-2001, The University of Sheffield.
 *
 *  This file is part of GATE (see http://gate.ac.uk/), and is free
 *  software, licenced under the GNU Library General Public License,
 *  Version 2, June 1991 (in the distribution as file licence.html,
 *  and also available at http://gate.ac.uk/gate/licence.html).
 *
 *  Marin Dimitrov, 18/Sep/2001
 *
 *  $Id: persist.bdy 3852 2002-04-04 12:52:38Z marin $
 *
 */
 
  ORACLE_TRUE  constant number := 1;
  ORACLE_FALSE constant number := 0;
 
 
  /******************************************************************************************
  *
  *   not used yet                                                                                             
  *   
  */
  procedure get_timestamp(p_timestamp  OUT number)
  is
  
  begin
       select SEQ_TIMESTAMP.nextval
       into p_timestamp
       from dual;
  end;                                                                                                        


  /******************************************************************************************
  *
  *   returns LR name by id                                                                                             
  *   
  */
  procedure get_lr_name(p_lr_id     IN number,
                        p_lr_name   OUT varchar2)
  is
  
  begin
       select lr_name
       into   p_lr_name
       from   t_lang_resource
       where  lr_id = p_lr_id;

  exception
       when NO_DATA_FOUND then
          raise error.x_invalid_lr;

  end;                                          
                                                                
  /******************************************************************************************
  *
  *   removes a copus and all contained components (features)                                                                                             
  *   
  */
  procedure delete_corpus(p_lr_id     IN number)
  is
     l_corp_id number;
  begin       
     --0. get corp id
     select corp_id 
     into   l_corp_id
     from   t_corpus
     where  corp_lr_id = p_lr_id;
     
     --1. delete from t_corpus_document
     delete 
     from   t_corpus_document
     where  cd_corp_id = l_corp_id;

     --2. delete t_corpus
     delete 
     from   t_corpus
     where  corp_id = l_corp_id;

     --3. delete from t_lang_resource
     delete 
     from   t_lang_resource
     where  lr_id = p_lr_id;     
     
     exception
        when NO_DATA_FOUND then
           raise error.x_invalid_lr;
     
  end;                                                                                                        

  /******************************************************************************************
  *
  *   removes a document and all contained components:
  *   - content
  *   - nodes
  *   - annnotations
  *   - features
  *   - annotation sets                                                                                             
  *   
  */
  procedure delete_document(p_lr_id     IN number)
  is
     l_doc_id number;
     l_content_id number;
     
  begin       
     --0. get doc_id
     select doc_id 
     into   l_doc_id
     from   t_document
     where  doc_lr_id = p_lr_id;
     
     -- get content id
     select doc_content_id
     into   l_content_id
     from   t_document
     where  doc_id = l_doc_id;
     
     -- 1. delete features
     delete 
     from   t_feature
     where  ft_entity_id = p_lr_id
            and ft_entity_type = persist.FEATURE_OWNER_DOCUMENT;

     -- 2. delete annotations

     -- 2.1. delete annotations' features
     delete 
     from   t_feature
     where  ft_entity_type = persist.FEATURE_OWNER_ANNOTATION
            and ft_entity_id in (select ann_global_id
                                 from   t_annotation
                                 where  ann_doc_id = l_doc_id
                                );

     --2.2. annotation to aset mappings
     delete 
     from   t_as_annotation
     where  asann_ann_id in (select ann_global_id
                             from   t_annotation
                             where  ann_doc_id = l_doc_id
                             );                                                                    
     --2.3 annotations
     delete
     from   t_annotation
     where  ann_doc_id = l_doc_id;
                      
     -- 3. delete annotation sets
     delete
     from   t_annot_set
     where  as_doc_id = l_doc_id;
     
     -- 4. delete nodes
     delete 
     from   t_node
     where  node_doc_id = l_doc_id;
     
     -- 4.5 delete from corpus if part of
     delete
     from   t_corpus_document
     where  cd_doc_id = l_doc_id;
          
     -- 5. delete document
     delete
     from   t_document
     where  doc_id = l_doc_id;
     
     -- 6. delete document content
     delete 
     from   t_doc_content
     where  dc_id = l_content_id;
     
     -- 8. delete LR
     delete 
     from   t_lang_resource
     where  lr_id = p_lr_id;     
     
     exception
        when NO_DATA_FOUND then
           raise error.x_invalid_lr;
     
  end;                                                                                                        


  /******************************************************************************************
  *
  *   creates a new LR                                                                                             
  *   
  */
  procedure create_lr(p_usr_id           IN number,
                      p_grp_id           IN number,
                      p_lr_type          IN varchar2,
                      p_lr_name          IN varchar2,
                      p_lr_permissions   IN number,
                      p_lr_parent_id     IN number,
                      p_lr_id            OUT number)
  is
    l_lr_type number;
  begin
     
     -- 1. sanity check
     if (false = security.is_valid_security_data(p_lr_permissions,p_grp_id,p_usr_id)) then
        raise error.x_incomplete_data;
     end if;
     
     -- 3. check if the LR type supplied is valid
     select lrtp_id
     into   l_lr_type
     from   t_lr_type
     where  lrtp_type = p_lr_type;
     
     
     -- 2. create a lang_resource record
     insert into t_lang_resource(lr_id,
                                 lr_type_id,
                                 lr_owner_user_id,
                                 lr_locking_user_id,
                                 lr_owner_group_id,
                                 lr_name,
                                 lr_access_mode,
                                 lr_parent_id)
     values (seq_lang_resource.nextval,
            l_lr_type,
            p_usr_id,
            null,
            p_grp_id,
            p_lr_name,
            p_lr_permissions,
            p_lr_parent_id)
     returning lr_id into p_lr_id;           
     
     
     exception
        when NO_DATA_FOUND then
           raise error.x_invalid_lr_type;
           
     
  end;                                                                                                        


  /*******************************************************************************************
  *
  *   creates a new document                                                                                             
  *   
  */  
  procedure create_document(p_lr_id        IN number,
                            p_url          IN varchar2,
                            p_encoding     IN varchar2,
                            p_start_offset IN number,
                            p_end_offset   IN number,
                            p_is_mrk_aware IN number,
                            p_corpus_id    IN number,
                            p_doc_id       OUT number)
  is
     l_encoding_id number;
     l_content_id number;
     l_encoding varchar2(16);
     cnt number;
  begin
  
     -- -1. if encoding is null, then set it to UTF8
     l_encoding := p_encoding;
     if (l_encoding is null) then
        l_encoding := ENCODING_DEFAULT;
     end if;

     --0. get encoding ID if any, otherwise create a new
     -- entry in T_DOC_ENCODING
     select count(enc_id)
     into   cnt
     from   t_doc_encoding
     where  enc_name = l_encoding;         
     
     if (cnt = 0) then
        --oops new encoding
        --add it 
        insert into t_doc_encoding(enc_id,
                                      enc_name)
        values (seq_doc_encoding.nextval,
                   l_encoding)
        returning enc_id into l_encoding_id;
     else
        --get encoding id
        select enc_id
        into   l_encoding_id
        from   t_doc_encoding
        where  enc_name = l_encoding;                
          
     end if;

       
     --1. create a document_content entry
     insert into t_doc_content(dc_id,
                               dc_encoding_id,
                               dc_character_content,
                               dc_binary_content,
                               dc_content_type)
     values(seq_doc_content.nextval,
            l_encoding_id,
            empty_clob(),
            empty_blob(),
            persist.EMPTY_CONTENT)
     returning dc_id into l_content_id;
     
     --2. create a document entry  
     insert into t_document(doc_id,
                            doc_content_id,
                            doc_lr_id,
                            doc_url,
                            doc_start,
                            doc_end,
                            doc_is_markup_aware)
     values(seq_document.nextval,
            l_content_id,
            p_lr_id,
            p_url,
            p_start_offset,
            p_end_offset,
            p_is_mrk_aware)
     returning doc_id into p_doc_id;
                 
     --3. if part of a corpus create a corpus_document entry
     if (p_corpus_id is not null) then
        insert into t_corpus_document(cd_id,
                                      cd_corp_id,
                                      cd_doc_id)
        values (seq_corpus_document.nextval,
                p_corpus_id,
                p_doc_id);                
     end if;     
                                     
  end;                                                                                                        
  

  /******************************************************************************************
  *
  *   creates a new annotation set                                                                                             
  *   
  */  
  procedure create_annotation_set(p_lr_id           IN number,
                                  p_as_name          IN varchar2,
                                  p_as_id            OUT number)
  is
     l_doc_id number;
  begin
  
     -- 1. get the DOC_ID
     select doc_id
     into   l_doc_id
     from   t_document
     where  doc_lr_id = p_lr_id;
  
     -- 2. create an entry for the set
     insert into t_annot_set(as_id,
                             as_doc_id,
                             as_name)
     values(seq_annot_set.nextval,
            l_doc_id,
            p_as_name)
     returning as_id into p_as_id;
     
     exception
     
        when NO_DATA_FOUND then
           raise error.x_invalid_lr;
                                 
  end;
  
  
  /******************************************************************************************
  *
  *   inserts annotation in a-set
  *   if the annotation does not exist then a new one is created                                                                                             
  *   
  */
  procedure create_annotation(p_lr_id           IN number,
                              p_ann_local_id         IN number,  
                              p_as_id            IN number,
                              p_node_start_lid   IN number,                                
                              p_node_start_offset IN number,  
                              p_node_end_lid      IN number,                                                              
                              p_node_end_offset   IN number,  
                              p_ann_type         IN varchar2,
                              p_ann_global_id    OUT number)
  is
     l_doc_id number;
     l_start_node_gid       number;
     l_end_node_gid         number;     
     l_ann_type_id          number;
     cnt                    number;
     l_ann_global_id        number;
     l_test                 boolean;
  begin

     -- 0. get the DOC_ID
     select doc_id
     into   l_doc_id
     from   t_document
     where  doc_lr_id = p_lr_id;
     
     
     begin
        -- get the annotation id
        select ann_global_id
        into   p_ann_global_id
        from   t_annotation
        where  ann_doc_id = l_doc_id
               and ann_local_id = p_ann_local_id;

        exception
           when NO_DATA_FOUND then                        
              begin
                 begin
                    -- new annotation, create it

                    -- 1. store nodes in DB only if they're new
                    -- (nodes are shared between annotations so the chances 
                    -- a node is used by more than one annotation is high)
     
                    -- 1.1. start node
                    -- select the global ID
                    select node_global_id
                    into   l_start_node_gid
                    from   t_node
                    where  node_doc_id = l_doc_id
                           and node_local_id = p_node_start_lid;        
           
                    exception
                       when NO_DATA_FOUND then
                          -- add to DB
                          insert into t_node(node_global_id,
                                             node_doc_id,
                                             node_local_id,
                                             node_offset)
                          values (seq_node.nextval,
                                  l_doc_id,
                                  p_node_start_lid,
                                  p_node_start_offset)
                          returning node_global_id into l_start_node_gid;        
                 end;
                 
                 begin
     
                    -- 1.2. end node     
                    -- select the global ID
                    select node_global_id
                    into   l_end_node_gid
                    from   t_node
                    where  node_doc_id = l_doc_id
                           and node_local_id = p_node_end_lid;        
     
                    exception
                       when NO_DATA_FOUND then
                          -- add to DB
                          insert into t_node(node_global_id,
                                             node_doc_id,
                                             node_local_id,
                                             node_offset)
                          values (seq_node.nextval,
                                 l_doc_id,
                                 p_node_end_lid,
                                 p_node_end_offset)
                          returning node_global_id into l_end_node_gid;        
                 end;
     
     
                 begin
                    -- 2. store annotation in DB     
                    -- 2.1 get the anotation type ID
                    select at_id
                    into   l_ann_type_id
                    from   t_annotation_type
                    where  at_name = p_ann_type;
           
                    exception
                       when NO_DATA_FOUND then
     
                          -- 2.2 if there is no such type, then create one
                          --oops, new type
                          --add it
                          insert into t_annotation_type(at_id,
                                                        at_name)
                          values (seq_annotation_type.nextval,
                                  p_ann_type)
                          returning at_id into l_ann_type_id;
                 end;

                 -- 2.3 insert annotation
                 insert into t_annotation(ann_global_id,
                                          ann_doc_id,
                                          ann_local_id,
                                          ann_at_id,
                                          ann_startnode_id,
                                          ann_endnode_id)
                 values (seq_annotation.nextval,
                        l_doc_id,
                        p_ann_local_id,
                        l_ann_type_id,
                        l_start_node_gid,
                        l_end_node_gid)
                 returning ann_global_id into p_ann_global_id;
     
              end;
     end;
        
     -- 3. create a annotation-to-aset mapping
     insert into t_as_annotation(asann_id,
                                 asann_ann_id,
                                 asann_as_id)
     values (seq_as_annotation.nextval,
             p_ann_global_id,
             p_as_id);
     

     exception     
        when NO_DATA_FOUND then
           raise error.x_invalid_lr;

          
  end;


  /******************************************************************************************
  *
  *   creates a corpus                                                                                             
  *   
  */  
  procedure create_corpus(p_lr_id     IN number,
                          p_corp_id   OUT number)
  is
  begin
     
     insert into t_corpus(corp_id,
                          corp_lr_id)
     values (seq_corpus.nextval,
             p_lr_id)
     returning corp_id into p_corp_id;
     
  end;

  
  /******************************************************************************************
  *
  *   checks if  feature value type is valid                                                                                             
  *   
  */  
  function is_valid_feature_type(p_type          IN number)
     return boolean
     deterministic
  is
  begin
     
     return (p_type in (persist.VALUE_TYPE_NULL,
                       persist.VALUE_TYPE_INTEGER,
                       persist.VALUE_TYPE_LONG,
                       persist.VALUE_TYPE_BOOLEAN,
                       persist.VALUE_TYPE_STRING,
                       persist.VALUE_TYPE_BINARY,
                       persist.VALUE_TYPE_FLOAT));
     
  end;
  
  /******************************************************************************************
  *
  *   creates a new feature                                                                                             
  *   
  */  
  procedure create_feature(p_entity_id           IN number,
                           p_entity_type         IN number,
                           p_key                 IN varchar2,  
                           p_value_number        IN number,                                
                           p_value_varchar       IN varchar2,
                           p_value_type          IN number,
                           p_feat_id             OUT number)                      
  is
     l_feature_key_id number;
     cnt number;
  begin
  

     --0.  
     if (false = is_valid_feature_type(p_value_type)) then
        raise error.x_invalid_feature_type;
     end if;  
  
     -- 1. find feature_key id
     begin
       select fk_id
       into   l_feature_key_id
       from   t_feature_key
       where  fk_string = p_key;
     
       exception
          when NO_DATA_FOUND then
             --2. if there is no such key then create one and get the id
             insert into t_feature_key(fk_id,
                                       fk_string)
             values(seq_feature_key.nextval,
                    p_key)
             returning fk_id into l_feature_key_id;                  
     end;
     
     insert into t_feature(ft_id,
                           ft_entity_id,
                           ft_entity_type,
                           ft_key_id,
                           ft_number_value,
                           ft_binary_value,
                           ft_character_value,
                           ft_long_character_value,
                           ft_value_type)
     values(seq_feature.nextval,
            p_entity_id,
            p_entity_type,
            l_feature_key_id,
            p_value_number,
            empty_blob(),
            p_value_varchar,
            empty_clob(),
            p_value_type)
     returning ft_id into p_feat_id;
     
  end;


  /******************************************************************************************
  *
  *   not used?                                                                                             
  *   
  */  
  procedure change_content_type(p_cont_id        in number,
                                p_new_type       in number)     
  is         
  begin
    
    if (p_new_type not in (persist.CHARACTER_CONTENT,
                           persist.BINARY_CONTENT,
                           persist.EMPTY_CONTENT)) then
                           
       raise error.x_invalid_content_type;
    end if;
    
    update t_doc_content
    set    dc_content_type = p_new_type
    where  dc_id = p_cont_id;
      
  end;

  /******************************************************************************************
  *
  *   updates LR attributes                                                                                             
  *   
  */  
  procedure update_lr  (p_lr_id     IN number,
                        p_lr_name   IN varchar2,
                        p_lr_parent_id IN number)
  is
    cnt number;
  begin
    
    --1. is there such LR?
    select count(LR_ID)
    into   cnt
    from   t_lang_resource
    where  lr_id = p_lr_id;
  
    --2. update it
    update t_lang_resource
    set    lr_name = p_lr_name,
           lr_parent_id = p_lr_parent_id
    where  lr_id = p_lr_id;
    
    

  exception
    when NO_DATA_FOUND then
       raise error.x_invalid_lr;

  end;                                                                                                        

 
  /******************************************************************************************
  *
  *   updates document components/atrtibutes                                                                                             
  *   
  */  
  procedure update_document(p_lr_id        IN number,
                            p_url          IN varchar2,
                            p_start_offset IN number,
                            p_end_offset   IN number,
                            p_is_mrk_aware IN number)
  is
    cnt number;
  begin
     
     -- 1. get the doc_id
     select count(doc_id)
     into   cnt
     from   t_document
     where  doc_lr_id = p_lr_id;
     
     if (cnt = 0) then
        raise error.x_invalid_lr;                              
     end if;
     
     update t_document
     set    doc_url = p_url,
            doc_start = p_start_offset,
            doc_end = p_end_offset,
            doc_is_markup_aware = p_is_mrk_aware
     where  doc_lr_id = p_lr_id;     
  end;

  /******************************************************************************************
  *
  *   removes features associated with some entity (corpus, document, annotation)                                                                                             
  *   
  */  
  procedure delete_features(p_ent_id        IN number,
                            p_ent_type      IN number)
  is
  begin
     delete from t_feature
     where  ft_entity_id = p_ent_id
     and    ft_entity_type = p_ent_type; 
  end;


  /******************************************************************************************
  *
  *   removes annotation (and its features)                                                                                             
  *   
  */  
  procedure delete_annotation(p_doc_id     IN number,
                              p_ann_local_id     IN number)
  is
     l_ann_global_id number;
  begin       
     
     -- 0. get the global ID
     select ann_global_id
     into   l_ann_global_id
     from   t_annotation
     where  ann_doc_id = p_doc_id
            and ann_local_id = p_ann_local_id;
     
     -- 1. delete fetures
     delete 
     from   t_feature
     where  ft_entity_id = l_ann_global_id
            and ft_entity_type = persist.FEATURE_OWNER_ANNOTATION;

     -- 2. delete aset-to-annotation mappings
     delete
     from  t_as_annotation
     where asann_ann_id = l_ann_global_id;
     
     -- 3. delete annotations
     delete 
     from   t_annotation
     where  ann_global_id = l_ann_global_id;
     
     exception
        when NO_DATA_FOUND then
           raise error.x_invalid_annotation;
  end;

  /******************************************************************************************
  *
  *   locks LR, no other sessions may write to it                                                                                             
  *   
  */  
  procedure lock_lr(p_lr_id     IN  number,
                    p_usr_id    IN number,
                    p_grp_id    IN number,
                    p_success   OUT number)
  is
    l_can_write_lr number;
    l_locking_user_id number;
  begin

    --1. check if the user has write access to the LR
    security.has_access_to_lr(p_lr_id,p_usr_id,p_grp_id,security.WRITE_ACCESS,l_can_write_lr);

    if (ORACLE_FALSE = l_can_write_lr) then
       raise error.x_insufficient_privileges;
    end if;
    
    --2. get the locking user if any
    select lr_locking_user_id
    into   l_locking_user_id
    from   t_lang_resource    
    where  lr_id = p_lr_id;
    
    if (l_locking_user_id is null) then
       --2a resource unlocked - lock it
       update t_lang_resource
       set    lr_locking_user_id = p_usr_id
       where  lr_id = p_lr_id;
       
       p_success := ORACLE_TRUE;       
    else
       -- 2b resource already locked
       p_success := ORACLE_FALSE;
    end if;
    
    exception
    
       when NO_DATA_FOUND then
          raise error.x_invalid_lr;
    
  end;


  /*******************************************************************************************
  *
  *   unlocks LR                                                                                             
  *   
  */  
  procedure unlock_lr(p_lr_id     IN  number,
                      p_usr_id    IN number)
  is
    l_locking_user_id number;
  begin
   
    --1. get the locking user if any
    select lr_locking_user_id
    into   l_locking_user_id
    from   t_lang_resource    
    where  lr_id = p_lr_id;
    
    -- is the reource locked by us?
    if (l_locking_user_id = p_usr_id or 
        l_locking_user_id = security.ADMIN_USER_ID) then
        
       update t_lang_resource
       set    lr_locking_user_id = null
       where  lr_id = p_lr_id;          
       
    end if;
    
  
    exception
    
       when NO_DATA_FOUND then
          raise error.x_invalid_lr;
     
  end;  

  /******************************************************************************************
  *
  *   removes a-set and its annotations                                                                                             
  *   
  */  
  procedure delete_annotation_set(p_lr_id        IN number,
                                  p_set_name     IN varchar2)
  is
    l_as_id number;
  begin
    
    --1. get aset ID
    select as_id
    into   l_as_id
    from   t_annot_set aset,
           t_document  doc           
    where  aset.as_name = p_set_name
           and aset.as_doc_id = doc.doc_id
           and doc.doc_lr_id = p_lr_id;
    
    --2. delete mappings
    delete 
    from   t_as_annotation
    where  asann_as_id = l_as_id;
    
    --1. delete annotations
    delete
    from   t_annotation   ann
    where  exists (select members.asann_id
                   from   t_as_annotation members
                   where  members.asann_ann_id = ann.ann_global_id
                          and members.asann_as_id = l_as_id);                          
    
    --3. delete set itself
    delete
    from   t_annot_set
    where  as_id = l_as_id;
    
    exception
    
       when NO_DATA_FOUND then
          raise error.x_invalid_annotation_set;
    
  end;


  /******************************************************************************************
  *
  *   guess                                                                                             
  *   
  */
  procedure add_document_to_corpus(p_doc_lrid     IN  number,
                                   p_corp_lrid    IN number)
  is
    cnt       number;
    l_corp_id number;
    l_doc_id  number;
  begin
  
     --1. get the doc_id
     select doc_id
     into   l_doc_id
     from   t_document
     where  doc_lr_id = p_doc_lrid;
     
     --2. get the corpus ID
     select corp_id
     into   l_corp_id
     from   t_corpus
     where  corp_lr_id = p_corp_lrid;
     
     --3. check if the document is not part of the corpus already
     select count(*)
     into   cnt
     from   t_corpus_document
     where  cd_corp_id = l_corp_id
            and cd_doc_id = l_doc_id;
            
     if (cnt = 0) then
        --4. no such entry, add one
        insert into t_corpus_document(cd_id,
                                      cd_corp_id,
                                      cd_doc_id)
        values (seq_corpus_document.nextval,
                l_corp_id,
                l_doc_id);                
     end if;
     
     exception
        when NO_DATA_FOUND then
           raise error.x_invalid_lr;

  end;

  /******************************************************************************************
  *
  *   processes a bulk load - the varrays passed are decomposed and proper calls                                                                                             
  *   to create_feature() are made
  *   
  */  
  procedure create_feature_bulk(p_entity_ids           IN INT_ARRAY,
                                p_entity_types         IN INT_ARRAY,
                                p_keys                 IN STRING_ARRAY,  
                                p_value_numbers        IN INT_ARRAY,                                
                                p_value_floats         IN INT_ARRAY,                                                                                                
                                p_value_varchars       IN STRING_ARRAY,
                                p_value_types          IN INT_ARRAY,
                                p_count                IN number)
  is
--     l_counter number;
     l_id number;
     l_number_value number;
     l_string_value varchar(4000);
     l_feature_type number;
  begin

     for i in 1 .. p_count loop

        l_feature_type := p_value_types(i);
        l_number_value := null;
        l_string_value := null;

        if (VALUE_TYPE_NULL = l_feature_type) then
           --do nothing
           null;
           
        elsif (l_feature_type = VALUE_TYPE_INTEGER or
               l_feature_type = VALUE_TYPE_LONG or
               l_feature_type = VALUE_TYPE_BOOLEAN) then

           l_number_value := p_value_numbers(i);
           --l_string_value := null;    

        elsif (l_feature_type = VALUE_TYPE_FLOAT) then
        
           l_number_value := p_value_floats(i);
           --l_string_value := null;                          
        
        elsif (l_feature_type = VALUE_TYPE_STRING) then

           --l_number_value := null;
           l_string_value := p_value_varchars(i);                          

        else           
           raise error.x_invalid_feature_type;
                      
        end if;
                 
        create_feature(p_entity_ids(i),
                       p_entity_types(i),
                       p_keys(i),  
                       l_number_value,                                
                       l_string_value,
                       l_feature_type,
                       l_id);
     end loop;

  end;                           

  
  /******************************************************************************************
  *
  *   guess                                                                                             
  *   
  */
  procedure remove_document_from_corpus(p_doc_lrid     IN  number,
                                        p_corp_lrid    IN number)
  is
    cnt       number;
    l_corp_id number;
    l_doc_id  number;
  begin
  
     --1. get the doc_id
     select doc_id
     into   l_doc_id
     from   t_document
     where  doc_lr_id = p_doc_lrid;
     
     --2. get the corpus ID
     select corp_id
     into   l_corp_id
     from   t_corpus
     where  corp_lr_id = p_corp_lrid;
     
     --3. delete the doc-to-corpus mapping
     delete 
     from   t_corpus_document
     where  cd_corp_id = l_corp_id
            and cd_doc_id = l_doc_id;
          
     exception
        when NO_DATA_FOUND then
           raise error.x_invalid_lr;

  end;
                           
    
/*begin
  -- Initialization
  <Statement>; */
end persist;
/
