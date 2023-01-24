

update_ops_list <- function(relab_AC, list_ops) {
  browser()
  list_ops <- (list_ops) %>% .[[1]]
  
  
  relab_AC <- relab_AC #list(relab_AC=  list("NNpatient_id","subject_id.ch1"))    #patient_id    #DA ELIMINARE
  
  
  #current_new_ops_entries
  list_ops_df <- as.data.frame(sapply(list_ops, "[[", 1))
  
  
  #updates values
  list_index <-  1:length(list_ops)
  newlabs <- as.character(list_ops_df[,1]) 
  
  tosubstitute_finder   <- newlabs %in% relab_AC[[1]][[1]]
  if (!all(tosubstitute_finder)| is_empty(tosubstitute_finder)) {
    return (list_ops)
  } else {    #the value located as current value in list-ops is substituted with the former old value in the relab_AC op restored through the undo
    target_index <- list_index[tosubstitute_finder]
    list_ops [[target_index]][[2]] <- relab_AC[[1]][[2]] 
    return (list_ops)
  }
}  