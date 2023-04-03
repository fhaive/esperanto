#' Finds the unique contents present in the selected column and suggests their potential recoding by means of the terms present in the reference vocabulary.
#'
#' @description
#' 'single_col_recode_finder' cross-compares the input dataframe column cells (column) and a reference vocabulary (dict_keys).
#' The reference vocabulary is a dataframe where each row shows the following fields: "reference label", "label synonyms","reference content", "content synonyms".
#' Each row of the output dataframe, is composed by a unique content of the column (cases), "recoded as" string (text for visualization), suggested recoding (pairs), indicator of the type of recoding (indicator, potential values -1,0,1).
#' (pairs) and (indicator) depend on the cross-comparison between the entry in cases and the vocabulary. If cases is stored as "reference content" in the vocabulary, pairs shows the same value of cases while the indicator assumes the value 0. If cases is stored as "content synonym" in the vocabulary, pairs shows the correspondent "reference content" and 1 as indicator. In the third case, cases is not present in the vocabulary, pairs shows the same value of cases and indicator is -1.
#' @param dict_keys A dataframe to use as reference vocabulary.
#' @param column A dataframe column.
#' @return  A dataframe with potential pairings of reference labels with current dataframe column names.
#' @examples
#' \dontrun{
#' library(magrittr)
#' library(dplyr)
#' library(tidyr)
#' dataset <- data.frame(gender=c("Male", NA,"Female","Unrecorded"),age=c(25,33, 24,40), organism = "Homo Sapiens", nationality = c("Finnish", "English","English","Finnish"))
#' dict_keys <-rbind(
#'               data.frame( label="sex" , lab_syn ="gender", allowed_features=c("M","F"), syn_features =c("Male,male","Female,female")  ) ,
#'               data.frame( label="organism" , lab_syn =NA, allowed_features=c("Homo Sapiens","Mus Musculus"), syn_features =NA)) %>%
#'               tidyr::separate_rows(.,c(syn_features))
#' column <- dataset$gender
#' paired_recodings <- single_col_recode_finder(dict_keys, column)
#' column <- dataset$organism
#' paired_recodings <- single_col_recode_finder(dict_keys, column)
#' }
#' @keywords internal
#' @export

# this function compares the cell content of the dataset with those present in the vocabulary
single_col_recode_finder <- function(dict_keys, column){


  cases <- unique(column)
  pairs <-c()
  cases_and_pairs <- data.frame(cases)
  cases_and_pairs["text"] <- "  recoded as  "


  #recoding: as it is (the cell content is in  allowed_features), recoded as allowed_features (if the content was a reported synonim), recoded with "**TbS__" prefix if the content was not present at all in the vocabulary. It will be later reviewed.
  cases_and_pairs$pairs<-  (lapply(cases, function(x) {
    if(x %in% c("NA", "NULL", "NONE")) { (NA)}
    else if  (is.na(x)) {  (NA)}
    else if(!x %in% dict_keys$allowed_features && x %in% dict_keys$syn_features) {
      na.omit(unique(dict_keys$allowed_features[dict_keys$syn_features==x]))}
    else if(x %in% dict_keys$allowed_features) {na.omit(unique(dict_keys$allowed_features[dict_keys$allowed_features==x])) }
    else{paste0("**TbS__",x)}

  }))
 #indicator is inserted in a forth column. If cases and pairs are identical "0" is set; otherwise "1" if they are different (pairs is the recoded version) and "-1" for "**TbS__" recoding (which will be reviewed later).
cases_and_pairs <- cases_and_pairs %>% dplyr::distinct(.) %>%
                              dplyr::mutate(indicator = ifelse(pairs %in% cases, 0,
                                                        ifelse(grepl("**TbS__", pairs,fixed=TRUE),-1, 1)))
cases_and_pairs$pairs <- sub("**TbS__", "", cases_and_pairs$pairs , fixed=TRUE)

finder<-(cases_and_pairs)
finder
}

