 
word_versions_tracker <-function(word, gvariable, sequence, more_than_one_word){
  sequence <- sequence
  
  orig_df <- gvariable$original_phTable[[1]]
  orig_columns <- colnames(orig_df)
  
  
  if((word %in% orig_columns && length(more_than_one_word)  == 0) || (identical(word, more_than_one_word))) {   #||
    sequence <- unique(c(sequence, word))
    return (sequence)
  } 
  else if (word %in% orig_columns && length(more_than_one_word) >0) {
    #more_than_one_word <- unique(more_than_one_word)
    alternative_words <- more_than_one_word[!more_than_one_word %in% word]
    last_of_alternative_moreword <- tail(alternative_words, 1)
    alternative_words <- alternative_words[seq_len(length(alternative_words)-1)] 
    more_than_one_word <- c(word, alternative_words[!alternative_words %in% last_of_alternative_moreword])
    sequence <- c(sequence, last_of_alternative_moreword)
    return (word_versions_tracker(last_of_alternative_moreword, gvariable, sequence,more_than_one_word))
  } 
  
  else {   #word is not in origin cols
    recodeAC_op_df <- unique(gvariable$recod_ops_dataframe[,c(1,2)]) 
    
    list_relab_ops <- gvariable$relab_op_list
    relab_acc_index <-  grep("relab_AC",names(list_relab_ops))
    re_labs <- as.character(purrr::map(list_relab_ops[relab_acc_index],1))
    old_labs <- as.character(purrr::map(list_relab_ops[relab_acc_index],2))
    
    if (word %in% recodeAC_op_df[[1]] | (word %in% re_labs)) {
      
      old_word <- recodeAC_op_df[recodeAC_op_df[[1]] %in% word,"oldlab"] %>% .[. != word] 
      more_than_one_word <- unique(c(more_than_one_word,old_word)) %>% .[!. %in% sequence]
            
      if(word %in% re_labs) {
          first_version <- old_labs[re_labs %in% word]
          sequence <- unique(c(sequence, word))
          more_than_one_word <- unique(c(more_than_one_word,first_version))
          return (word_versions_tracker(first_version, gvariable, sequence,more_than_one_word))
      }
      
      last_of_moreword <- tail(more_than_one_word, 1)
      more_than_one_word <- more_than_one_word[seq_len(length(more_than_one_word)-1)] 
      sequence <- unique(c(sequence, last_of_moreword))
      return (word_versions_tracker(last_of_moreword, gvariable, sequence,more_than_one_word))
      
      
      
      
      
      #sequence <- unique(c(sequence, old_word))
      #return (word_versions_tracker(old_word, gvariable, sequence,more_than_one_word))
      
    }
    else {
      
      last_of_moreword <- tail(more_than_one_word, 1)
      more_than_one_word <- more_than_one_word[seq_len(length(more_than_one_word)-1)] 
      sequence <- unique(c(sequence, last_of_moreword))
      return (word_versions_tracker(last_of_moreword, gvariable, sequence,more_than_one_word))
      
      
    }
    
    
    
  }
    
}