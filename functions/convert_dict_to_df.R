#convert object collections::dictionary new_OBJdict (I) into a list of two vocabulary dataframes voc_NEWversions in compact and extended versions (compact_new_dict, extended_new_dict) (O). 
# I: newly updated vocabulary as collections_dictionary object with following structure: dict(key:value) = (key= label_*_Lsynonyms_*_content  :  value=cont_synonyms)
# O: voc_NEWversions, list of equivalent updated vocabulary (compact- and extended_versions) with new terms strucutred according to label,lab_synonyms,content_cont_synonyms.
#         compact:  Synonym fields of specific label/content are comma_separated.
#         extendend: All fields contain one entry, with no comma-separated multiple synonyms.

convert_dict_to_df <- function(new_OBJdict){
  # melt the dict object in properly strucutred dataframe and substitutes NA string and "" with NA
  new_dict <- reshape2::melt(new_OBJdict$as_list(), value.name = "syn_features")
  compact_new_dict <- new_dict %>%
    separate(L1, c("label","L2"), sep="\\_\\*\\_",extra="merge") %>%
    separate(L2, c("lab_syn","allowed_features"), sep="\\_\\**\\_",extra="merge") %>% 
    .[,c("label", "lab_syn", "allowed_features", "syn_features")] %>%
    na_if(.,"NA") %>% na_if(.,"") %>% .[order(.$label),] 
  #%>%group_by(label, lab_syn,syn_features) %>% summarize_all(~paste(unique(na.omit(.)), collapse = ','))
  
  extended_new_dict <- compact_new_dict %>% separate_rows(.,lab_syn,sep="\\|")  
  voc_NEWversions <- list(compact_new_dict, extended_new_dict)
  voc_NEWversions
  
  
  
}
