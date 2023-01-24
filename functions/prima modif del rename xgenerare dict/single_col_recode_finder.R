#I: vocabulary keys, content of a single column of the analyzed dataset
#O: is a dataframe (finder) containing in one column the different unique contents of the tested column; in the second column there is the recoded version; there is a third column where values (-1,0,1) are given according to the first and second column content relate (check "indicator" comments).

# this function compares the cell content of the dataset with those present in the vocabulary
single_col_recode_finder <- function(dict_keys, column){

    #check matches between dict allowed feature vs dataset cell content  
  ngrams <- unique(c(dict_keys$allowed_features))
  patternDICT <- paste(ngrams, collapse = "|")
  
  ngrams_syn <- unique(c(dict_keys$Syn_Features))
  patternDICT_syn <- paste(ngrams_syn, collapse = "|")
  
  cases <- ((unique(column)))
  pairs <-c()
  #recoding: as it is (the cell content in an allowed_features), recoded as allowed_features (if the content was a reported synonim), recoded with "**TbS__" prefix if the content was not present at all in the vocabulary. It will be later reviewed.
  # dz<- 
  pairs<-  (lapply(cases, function(x) {
       if(x %in% dict_keys$Syn_Features) {
         unique(dict_keys$allowed_features[dict_keys$Syn_Features==x])[2]}
       else if(x %in% dict_keys$allowed_features) {unique(dict_keys$allowed_features[dict_keys$allowed_features==x])[2] }
       else if(x %in% c("NA", "NULL", "NONE")) {NA}
               else{paste0("**TbS__",x)}
  
         }))
  
  #creates the dataframe where cases and the recoded are paired
cases_and_pairs <- (data.frame(cases))
cases_and_pairs["text"] <- "  recoded as  "
cases_and_pairs$pairs<-pairs
 #indicator is inserted in a forth column. If cases and pairs are identical "0" is set; otherwise "1" if they are different (pairs is the recoded version) and "-1" for "**TbS__" recoding (which will be reviewed later).
cases_and_pairs <- cases_and_pairs %>% 
                              mutate(indicator = ifelse(pairs %in% cases, 0,
                                                        ifelse(grepl("**TbS__", pairs,fixed=TRUE),-1, 1))) 
  

finder<-(cases_and_pairs)   
finder
}


