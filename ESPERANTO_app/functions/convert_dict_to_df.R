#' Convert a collections::dict object (key:value) input into a list containing the list of updated vocabularies dataframes.

#' @description
#' 'convert_dict_to_df' takes as input (new_OBJdict) a collections::dict object (key:value), such as an intermediate updated version of the original reference vocabulary, and converts it into a list of two vocabulary dataframes, equivalent in the entries, but structured in a compact and extended version.
#' #' @param new_OBJdict A collections::dict object containing an intermediate updated version of the reference vocabulary, as consequence of the curation process.
#' @return  A list containing two dataframes, such as respectively the compact and the extended version of the reference vocabulary. The compact ([1]) has synonym fields of reference label/content are separated respectively by vertical bar/comma. In the extended version [2], the separator is removed by the label synonym and from the allowed_features fields, duplicating the rest of the row-fields appropriately.
#' @examples
#' \dontrun{
#' library(collections)
#' library(magrittr)
#' library(tidyr)
#' library(dplyr)
#'
#' #new_entries and dict_keys (original_vocabulary vocabulary) are given for comparison
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
#' new_OBJdict <-collections::dict(NULL)
#' new_OBJdict$set("sex_*_gender_**_M", "Male,male")$set("sex_*_gender_**_F", "Female,female")$set("organism_*_being_**_Homo Sapiens", NA)$set("organism_*_being_**_Mus Musculus","mouse")$set("disease_*_illness|sickness_**_COVID-19", "Covid-19,SARS-CoV-2")$set("disease_*_illness|sickness",NA)$set("organism_*_being_**_Mustela putorius","Ferret,ferret")
#' convert_dict_to_df(new_OBJdict)
#' }
#' @keywords internal
#' @export



convert_dict_to_df <- function(new_OBJdict){
  # melt the dict object in properly strucutred dataframe and substitutes NA string and "" with NA
  new_dict <- reshape2::melt(new_OBJdict$as_list(), value.name = "syn_features")
  compact_new_dict <- new_dict %>%
    separate(L1, c("label","L2"), sep="\\_\\*\\_",extra="merge") %>%
    separate(L2, c("lab_syn","allowed_features"), sep="\\_\\**\\_",extra="merge") %>%
    .[,c("label", "lab_syn", "allowed_features", "syn_features")] %>%
    dplyr::na_if(.,"NA") %>% dplyr::na_if(.,"") %>% .[order(.$label),]

  extended_new_dict <- compact_new_dict %>% separate_rows(.,lab_syn,sep="\\|")  %>%
                            separate_rows(.,allowed_features,sep="\\,") %>%   .[!duplicated(.),]
  #remove eventual duplicates and clean from "NA"
  extended_new_dict["syn_features"] <-  (strsplit(extended_new_dict$syn_features, "\\|")) %>%
                                          lapply(., function(k) {paste(unique(k), collapse = "|") }) %>%
                                          as.character(.)
  extended_new_dict <- extended_new_dict %>% dplyr::mutate( syn_features = dplyr::na_if(syn_features,"NA"))
  voc_NEWversions <- list(compact_new_dict, extended_new_dict)
  voc_NEWversions



}
