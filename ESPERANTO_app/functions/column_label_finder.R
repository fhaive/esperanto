#' Finds potential candidates to rename column of the dataframe according to a reference vocabulary
#'
#' @description
#' 'column_label_finder' cross-compares a dataframe (single_df) and a reference vocabulary (dict_keys).
#' The reference vocabulary is a dataframe where each row shows the following fields: "reference label", "label synonyms","reference content", "content synonyms".
#' The cross comparison compares the vocabulary synonyms vs the column names of a dataframe.
#' Each row of the output dataframe, is composed by suggested_label (label), matching_synonym (word), matching_original_df_colname (match), label_word_pairing (pairs), direction.
#' @param  dict_keys A dataframe to use as reference vocabulary.
#' @param single_df A dataframe.
#' @return  A dataframe with potential pairing of reference labels with current dataframe column names
#' @examples
#' \dontrun{
#' library(stringr)
#' single_df <- data.frame(gender=c("Male","Female"),age=c(c(25,37),c(24,53)))
#' dict_keys <- data.frame( label="sex", lab_syn ="gender", allowed_features=c("M","F"), syn_features =c("Male,male","Female,female")  ) %>% tidyr::separate_rows(.,syn_features)
#' relabels <- column_label_finder(dict_keys, single_df)
#' relabels
#'
#' }
#' @keywords internal
#' @export


# it finds the column of the dataset named by label synonyms that should be renamed with the allowed label.
column_label_finder <- function(dict_keys, single_df){
  name_col <- colnames(single_df)
  t<-c()
  LABEL_matchedDs_DiSYN <- c()
  tmp_pair <-c()


  #controlla che ci siano match tra dict e dataset label synonim. T contains the datset colname to relabel.
  ngrams <- unique(c(dict_keys$lab_syn))
  patternDICT <- paste(ngrams, collapse = "|")    # only synonyms
  t<-unique(stringr::str_extract(name_col, patternDICT))   # matching colnames with voc. synonyms
  t<-t[!t %in% "NA"]
  t<-t[!is.na(t)]


  len<-length(t)
  if (len==0){
    print("NO MATCH")}
  else{
      #find a vocabulary reference label vs the the synonym got by match dict-dataset

      for (i in 1:len){
          #find the row number of the allowed label LABEL_matchedDs_DiSYN corresponding to the previosuly found label_syn; LABEL_matchedDs_DiSYN is stored to to replace later the synonym part in the dataset name(label_replacement)
          label_row<- which(dict_keys$lab_syn == t[i], arr.ind=TRUE)[1]
          LABEL_matchedDs_DiSYN [i] <- dict_keys$label[label_row]


    tmp_pair[i]<- as.character(paste(single_df %>%
                      dplyr::select(contains(t[i])) %>% colnames(.) , collapse= "|"))

    }


  retrieved <- data.frame(label= LABEL_matchedDs_DiSYN, word= t, match=tmp_pair) %>% tidyr::separate_rows(.,match, sep="\\|")
  retrieved["pairs"] <- paste0(retrieved$label," -> ",retrieved$match)
  retrieved["direction"] <- " -> "
  retrieved
  }
  retrieved

}

