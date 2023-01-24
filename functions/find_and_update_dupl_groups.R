# This function takes a list element representing one previously performed operation (list_former_new_value), the current metadata dataset (current_df), label and content dictionaries storing as key the oldand shorten it to a specificied lenght (nchar), included the ".." placed as placeholder for the ripped part.
#I:   list of the former_new_elements set during previous ops (list_former_new_value)
#     current metadata dataset (current_df)
#     dict containing the recoded labels(dictLAB) and contents (dictCONT) where k:v are former_new_value: newly_recoded_current_value 
#O: return the  updated version - if necessary - of the groups of duplicated found and processed in the previous operations   

find_and_update_dupl_groups <-  function(list_dupl_ops, current_df, dictLAB, dictCONT) {
  kept <- purrr::map(list_dupl_ops,1)
  dupl_selected <- purrr::map(list_dupl_ops,2)
  not_recoded_labels_finder <- kept %in% colnames(current_df)
  if (all(not_recoded_labels_finder)) {    #all column names are updated
    return (list_dupl_ops)#(dupl_selected)
  } else {  #it is not in the current_df so, the value was recoded at some point and thus can be tracked in the dictLAB
    # former_new %in% collections::dictLAB$keys())
    lapply(which(!not_recoded_labels_finder ), function(k) { 
      former_word <- kept[[k]]
      updated_word <- dictLAB$get(former_word)
      list_dupl_ops[[k]][[1]][list_dupl_ops[[k]][[1]] %in% former_word] <<- updated_word
      
      list_dupl_ops[[k]][[2]][list_dupl_ops[[k]][[2]] %in% former_word] <<- updated_word
      #dupl_selected[[k]][dupl_selected[[k]] %in% former_word] <<- updated_word
        
    }
    ) 
    return(find_and_update_dupl_groups(list_dupl_ops, current_df, dictLAB, dictCONT))
  }
}

