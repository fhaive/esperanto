# This function takes a list element representing one previously performed operation (list_former_new_value), the current metadata dataset (current_df), label and content dictionaries storing as key the oldand shorten it to a specificied lenght (nchar), included the ".." placed as placeholder for the ripped part.
#I:   list of the former_new_elements set during previous ops (list_former_new_value)
#     current metadata dataset (current_df)
#     dict containing the recoded labels(dictLAB)  where k:v are former_new_value: newly_recoded_current_value 
#O: return the list of  words processed in the previous operations updated with their recoded versions  

find_and_update_previous_actionsLSTrelab_REJ <-  function(list_relab_ops, current_df, dictLAB) {
 
  relab_rej_index <-  grep("relab_RJ",names(list_relab_ops))
  #candidates <- purrr::map(list_relab_ops[relab_acc_index],1)
  tosubstitute <- purrr::map(list_relab_ops[relab_rej_index],2) 
  #columns_relab_RJ <- as.character(candidates [grepl("relab_RJ",names(candidates))])
  
  #map(list_relab_ops [grepl("relab_AC",names(list_relab_ops))],1)  
  not_recoded_labels_finder   <- tosubstitute %in% colnames(current_df) 
  if (all(not_recoded_labels_finder)) {
    return (list_relab_ops)
  } else {  #it is not in the current_df so, the value was recoded at some point and thus can be tracked in the dictLAB
    # former_new %in% collections::dictLAB$keys())
    
    lapply(relab_rej_index[!not_recoded_labels_finder], function(l) { #browser()
      former_new <- list_relab_ops [[l]][[2]]
      if (dictLAB$has(former_new) ){
        recoded_word <- dictLAB$get(former_new)
        list_relab_ops [[l]][[2]] <<- recoded_word 
      } else { 
        #  if(former_new %in% columns_relab_RJ) { return (return(return(list_relab_ops))) }
        # else {list_relab_ops [[k]] <<-NULL}
        list_relab_ops [[l]] <<-NULL
        
      }
    }
    )
    
    return(find_and_update_previous_actionsLSTrelab_AC(list_relab_ops, current_df, dictLAB))
  }
}
