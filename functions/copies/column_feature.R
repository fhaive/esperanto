#I: vocabulary keys, content of a single column of the analyzed dataset
#O: list of each cell content of the column recoded according to the allowed feature, or left untouched if not present in the vocabulary.

# this function compares the cell content of the dataset with those present in the vocabulary
column_feature2 <- function(dict_keys, column){
  upd_col <- NA
  
  #check matches between dict allowed feature vs dataset cell content  
  ngrams <- unique(c(dict_keys$allowed_features))
  patternDICT <- paste(ngrams, collapse = "|")
  
  ngrams_syn <- unique(c(dict_keys$Syn_Features))
  patternDICT_syn <- paste(ngrams_syn, collapse = "|")
  
  cases <- ((unique(column)))
  
  pairs <-c()
  pairs<-  (lapply(cases, function(x) {
       if(x %in% dict_keys$Syn_Features) {
         unique(dict_keys$allowed_features[dict_keys$Syn_Features==x])[2]}
       else if(x %in% dict_keys$allowed_features) {unique(dict_keys$allowed_features[dict_keys$allowed_features==x])[2] }
       else if(x %in% c("NA", "NULL", "NONE")) {NA}
               else{paste0("**TbS__",x)}
  
         }))
  

 upcol<-((mapvalues(column, from= cases, to=pairs)))

  upcol
}


