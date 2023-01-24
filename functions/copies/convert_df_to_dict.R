#convert vocabulary as dataframe(I) into dict object collections(O). A list of thre dicts are buildt.
convert_df_to_dict <- function (d_df){
  tmp <- d_df %>% 
    group_by(label) %>%
    summarise(syn_lab = stringr::str_c(unique(Lab_Syn), collapse="|"))
  dict_LAB<-collections::dict(items = tmp$syn_lab, keys = tmp$label)
  
  #dict allowed content- synonyms
  tmp <- d_df %>%
    group_by(allowed_features) %>%
    summarise(syn_cont = stringr::str_c(unique(Syn_Features), collapse="|")) 
  dict_CONT<-collections::dict(items = tmp$syn_cont, keys = tmp$allowed_features)
  
  #dict label_content
  tmp <- d_df %>%
    group_by(label) %>%
    summarise(content = stringr::str_c(unique(allowed_features), collapse="|")) 
  dict_LAB_CONT<-collections::dict(items = tmp$content, keys = tmp$label)
  
  full_dict <- list(dict_LAB, dict_CONT, dict_LAB_CONT)
  full_dict
}