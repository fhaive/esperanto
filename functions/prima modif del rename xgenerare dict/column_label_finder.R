#input: vocabulary (labels+features) & dataset
#output: match, such as a dataset each row is a pair made of the dataset colnames and the proper label to rename 

# it finds the column of the dataset named by label synonyms that should be renamed with the allowed label. 
column_label_finder <- function(dict_keys, single_df){
  name_col <- colnames(single_df)
  t<-c()
  LABEL_matchedDs_DiSYN <- c()
  tmp_pair <-c()
  
  
  #controlla che ci siano match tra dict e dataset label synonim. T contains the name of dataset to relabel.
  ngrams <- unique(c(dict_keys$Lab_Syn))
  patternDICT <- paste(ngrams, collapse = "|")
  t<-unique(str_extract(name_col, patternDICT))
  t<-t[!t %in% "NA"]
  t<-t[!is.na(t)]
  
  
  len<-length(t)
  if (len==0){ 
    print("NO MATCH")}
  else{
      #trova la label permessa rispetto al sinonimo trovato nel match dict-mask
    
      for (i in 1:len){
          #find the row number of the allowed label LABEL_matchedDs_DiSYN corresponding to the previosuly found label_syn; LABEL_matchedDs_DiSYN is stored to to replace later the synonym part in the dataset name(label_replacement)
          label_row<- which(dict_keys$Lab_Syn == t[i], arr.ind=TRUE)[1]
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

