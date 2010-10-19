CREATE or replace FUNCTION test_remove_all_data() RETURNS boolean AS '

   BEGIN

     /* delete LRs */
 
     /* features */
     delete from t_feature;

    /* feature keys */
     delete from t_feature_key;

     /* annotations/sets */
     delete from t_as_annotation;
     delete from t_annotation;
     delete from t_annot_set;
     delete from t_annotation_type;

     /* delete nodes */
     delete from t_node;

     /* documents/corpuses */
     delete from t_corpus_document;
     delete from t_document;
     delete from t_corpus;

     /* document contents */
     delete from t_doc_content;

     /* finally */
     delete from t_lang_resource;


     /* remove users in groups */
     delete from t_user_group
     where ugrp_id > 0;

     /* delete groups */
     delete from t_group
     where grp_id >0;

     /* delete users */
     delete from t_user
     where usr_id >0;

     /* dummy */
     return true;

   END;
'
LANGUAGE 'plpgsql';
