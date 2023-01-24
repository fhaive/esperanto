
update_ops_df <- function(relab_AC, df_ops) {
  browser()
  #current_new_ops_entries
  df_ops_df <- (df_ops) %>% .[[1]]
  
  
  relab_AC <- relab_AC #list(relab_AC=  list("NNpatient_id","subject_id.ch1"))    #patient_id    #DA ELIMINARE
  
  
  
  #df_ops_df <- as.data.frame(sapply(df_ops, "[[", 1))
  
  
  #updates values
  row_index <-  1:nrow(df_ops_df)
  newlabs <- as.character(df_ops_df[,1]) 
  
  tosubstitute_finder   <- newlabs %in% relab_AC[[1]][[1]]
  
  if (all(tosubstitute_finder)| is_empty(tosubstitute_finder)) {
    return (df_ops_df)
  } else {    #the value located as current value in list-ops is substituted with the former old value in the relab_AC op restored through the undo
    target_index <- row_index[tosubstitute_finder]
    df_ops_df [target_index,1] <- relab_AC[[1]][[2]] 
    return (df_ops_df)
  }
}  