create or replace package persist is

/*
 *  persist.spc
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
 *  $Id: persist.spc 3852 2002-04-04 12:52:38Z marin $
 *
 */  

  -- dummy encoding, do not use
  ENCODING_DEFAULT constant varchar2(16) := '-!-';

  -- LR class types
  -- these should be sync-ed with the java code  
  DOCUMENT_CLASS constant varchar2(128) := 'gate.corpora.DatabaseDocumentImpl';
  CORPUS_CLASS constant varchar2(128) :=  'gate.corpora.DatabaseCorpusImpl';

  -- entity that owns a feature
  -- should be sync-ed with java sources
  FEATURE_OWNER_CORPUS      constant number :=  1;
  FEATURE_OWNER_DOCUMENT    constant number :=  2;
  FEATURE_OWNER_ANNOTATION  constant number :=  3;
  
  --feature value types
  -- should be sync-ed with java sources  
  VALUE_TYPE_NULL       constant number := 100;   
  VALUE_TYPE_INTEGER    constant number := 101;
  VALUE_TYPE_LONG       constant number := 102;
  VALUE_TYPE_BOOLEAN    constant number := 103;
  VALUE_TYPE_STRING     constant number := 104;
  VALUE_TYPE_BINARY     constant number := 105;
  VALUE_TYPE_FLOAT      constant number := 106;

  --content types
  -- should be sync-ed with java sources    
  CHARACTER_CONTENT     constant number := 1;
  BINARY_CONTENT        constant number := 2;
  EMPTY_CONTENT         constant number := 3;  
    

  procedure get_timestamp(p_timestamp  OUT number);

  
  procedure get_lr_name(p_lr_id     IN number,
                        p_lr_name   OUT varchar2);
  
  procedure update_lr(p_lr_id        IN number,
                      p_lr_name      IN varchar2,
                      p_lr_parent_id IN number);
  
  procedure delete_document(p_lr_id     IN number);
  
  procedure delete_corpus(p_lr_id     IN number);
  
  procedure delete_annotation(p_doc_id     IN number,
                              p_ann_local_id     IN number);  

  procedure create_lr(p_usr_id           IN number,
                      p_grp_id           IN number,
                      p_lr_type          IN varchar2,
                      p_lr_name          IN varchar2,
                      p_lr_permissions   IN number,
                      p_lr_parent_id     IN number,
                      p_lr_id            OUT number);

  
  procedure create_document(p_lr_id        IN number,
                            p_url          IN varchar2,
                            p_encoding     IN varchar2,
                            p_start_offset IN number,
                            p_end_offset   IN number,
                            p_is_mrk_aware IN number,
                            p_corpus_id    IN number,
                            p_doc_id       OUT number);

                            
  procedure create_annotation_set(p_lr_id           IN number,
                                  p_as_name          IN varchar2,
                                  p_as_id            OUT number);


  procedure create_annotation(p_lr_id           IN number,
                              p_ann_local_id     IN number,    
                              p_as_id            IN number,
                              p_node_start_lid   IN number,                                
                              p_node_start_offset IN number,  
                              p_node_end_lid      IN number,                                                              
                              p_node_end_offset   IN number,                                
                              p_ann_type         IN varchar2,
                              p_ann_global_id    OUT number);


  procedure create_corpus(p_lr_id     IN number,
                          p_corp_id   OUT number);
                      

  procedure create_feature(p_entity_id           IN number,
                           p_entity_type         IN number,
                           p_key                 IN varchar2,  
                           p_value_number        IN number,                                
                           p_value_varchar       IN varchar2,
                           p_value_type          IN number,
                           p_feat_id             OUT number);
                      
  procedure create_feature_bulk(p_entity_ids           IN INT_ARRAY,
                                p_entity_types         IN INT_ARRAY,
                                p_keys                 IN STRING_ARRAY,  
                                p_value_numbers        IN INT_ARRAY,                                
                                p_value_floats         IN INT_ARRAY,                                                                
                                p_value_varchars       IN STRING_ARRAY,
                                p_value_types          IN INT_ARRAY,
                                p_count                IN number);

  
  function is_valid_feature_type(p_type          IN number)
     return boolean
     deterministic
;

  
  procedure change_content_type(p_cont_id        in number,
                                p_new_type       in number);     

                                
  procedure update_document(p_lr_id        IN number,
                            p_url          IN varchar2,
                            p_start_offset IN number,
                            p_end_offset   IN number,
                            p_is_mrk_aware IN number);

  procedure delete_features(p_ent_id        IN number,
                            p_ent_type      IN number);


  procedure delete_annotation_set(p_lr_id        IN number,
                                  p_set_name     IN varchar2);

                            
  procedure lock_lr(p_lr_id     IN  number,
                    p_usr_id    IN number,
                    p_grp_id    IN number,
                    p_success   OUT number);

  procedure unlock_lr(p_lr_id     IN  number,
                      p_usr_id    IN number);

  procedure add_document_to_corpus(p_doc_lrid     IN  number,
                                   p_corp_lrid    IN number);

  procedure remove_document_from_corpus(p_doc_lrid     IN  number,
                                        p_corp_lrid    IN number);
                                
end persist;
/
