#convert vocabulary tmp_upd_vocabolary from  dataframe(I) into full_dict collections::dictionary object(O). 
# I: vocabulary made of old entries and new entries according to label,lab_synonyms,content_cont_synonyms. All fields contain one entry, not comma-separated multiple synonyms.
# O: dict with following structures: dict(key:value) = (key= label_*_Lsynonyms_*_content  :  value=cont_synonyms)
convert_df_to_dict <- function (tmp_upd_vocabolary){
  
  tmp <- tmp_upd_vocabolary %>%                                
    group_by(label) %>%
    # creates cumulative string of label synonyms (1)
    # starts to build the key(label_*_labSynonyms), later unified with reference contents extendedkey(label_*_labSynonyms_**_allowed_features) (2,3)
    # groups according to extended_key and cumulative string of content synonyms (4,5)  
    mutate(compact_lab_synlab = stringr::str_c(na.omit(unique(lab_syn)), collapse="|"))  %>%
    unite(process_key, c(label,compact_lab_synlab), sep="_*_", remove=FALSE,na.rm=T) %>%
    unite(extended_key, c(process_key,allowed_features), sep="_**_", remove=TRUE,na.rm=T) %>%
    group_by(extended_key)%>%
    mutate(synfeat2 = stringr::str_c(na.omit(unique(syn_features)), collapse=",")) 
  
  full_dict<-collections::dict(keys=tmp$extended_key, item=tmp$synfeat2)
  
  
}