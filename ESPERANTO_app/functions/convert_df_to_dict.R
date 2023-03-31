#' Convert a dataframe structurated similarly to reference vocabulary into a collections::dict object (key:value).  
#'
#' @description 
#' 'convert_df_to_dict' takes as input a dataframe (tmp_upd_vocabolary) where new potential entries, obtained during the curation process, are row-bound to the original reference vocabulary entries. The input is converted into a collections::dict object, where each unique key is linked to a value (key:value).  
#' (tmp_upd_vocabolary) is structured in the reference vocabulary fashion, "reference label", "label synonyms", "reference content", "content synonyms".
#' The uniqueness of the key:value pair is ensured by pasting all label synonyms and reference contents related to the reference label into the key; the content synonyms are stored into the value field. I.e. (key = label_*_lab_syn1|lab_syn2_**_content) : (value = cont_syn1,cont_syn2,cont_syn3).
#' #' @param tmp_upd_vocabolary A dataframe originated by the reference vocabulary and new rows containing the new terms to potentially incorporate into an updated version of the reference vocabulary.
#' @return  A collections::dict object (key:value).
#' @examples
#' \dontrun{
#' library(magrittr) 
#' library(collections)
#' dict_keys <-rbind(
#'               data.frame( label="sex" , lab_syn ="gender", allowed_features=c("M","F"), syn_features =c("Male,male","Female,female")  ) ,
#'               data.frame( label="organism" , lab_syn =NA, allowed_features=c("Homo Sapiens","Mus Musculus"), syn_features =NA)) %>% 
#'               tidyr::separate_rows(.,c(syn_features))
#' new_entries <- rbind(data.frame( label="disease" , lab_syn ="illness", allowed_features="COVID-19", syn_features ="Covid-19"),
#'                   data.frame( label="disease" , lab_syn ="sickness", allowed_features="COVID-19", syn_features ="SARS-CoV-2"),
#'                   data.frame( label="disease" , lab_syn =NA, allowed_features=NA, syn_features =NA),
#'                   data.frame( label="organism" , lab_syn ="being", allowed_features="Mus Musculus", syn_features ="mouse"),
#'                   data.frame( label="organism" , lab_syn =NA, allowed_features="Mustela putorius", syn_features ="Ferrets"),
#'                   data.frame( label="organism" , lab_syn =NA, allowed_features="Mustela putorius", syn_features ="ferret"),
#'                   data.frame( label="sex" , lab_syn =NA, allowed_features="F", syn_features ="femine") ) 
#' tmp_upd_vocabolary <- rbind(dict_keys, new) 
#' dict_collection <- convert_df_to_dict(tmp_upd_vocabolary)
#' dict_collection$as_list()
#' }
#' @keywords internal
#' @export



#convert vocabulary tmp_upd_vocabolary from  dataframe(I) into full_dict collections::dictionary object(O). 
# I: vocabulary made of old entries and new entries according to label,lab_synonyms,content_cont_synonyms. All fields contain one single entry and, not comma-separated multiple synonyms.
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
    mutate(synfeat2 = stringr::str_c(na.omit(unique(syn_features)), collapse="|")) 
  
  full_dict<-collections::dict(keys=tmp$extended_key, item=tmp$synfeat2)
  
  
}