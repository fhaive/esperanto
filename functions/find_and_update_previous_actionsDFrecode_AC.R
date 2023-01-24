# This function takes a list element representing one previously performed operation (recodeAC_op_df), the current metadata dataset (current_df), label and content dictionaries storing as key the old value (dictLAB,dictCONT), gvariable (containing the deleted op list and the dict with ops related to deleted columns)
#I:   list of the former_new_elements set during previous ops (recodeAC_op_df)
#     current metadata dataset (current_df)
#     dict containing the recoded labels(dictLAB) and contents (dictCONT) where k:v are former_new_value: newly_recoded_current_value 
#O: return the list of  words processed in the previous operations updated with their recoded versions and stores the ops related to deleted columns (if present)  

find_and_update_previous_actionsDFrecode_AC <-  function(recodeAC_op_df, current_df, dictLAB, gvariable) {
  browser()
  recodeAC_op_df <- unique(recodeAC_op_df)
  orig_colnames <- as.character(gvariable[[2]])
  dict_relab <- gvariable[[3]]
  newlabs <- (recodeAC_op_df[[1]]) 
  
  #updates values
  recode_acc_index <-  1:nrow(recodeAC_op_df)
  newlabs <- (recodeAC_op_df[[1]]) 
  
  
  current_recoded_label_finder   <- newlabs %in% colnames(current_df) 
  if (all(current_recoded_label_finder) | is_empty(current_recoded_label_finder)) {
    return (recodeAC_op_df)
  }  else { #it is not in the current_df so, the value was recoded at some point and thus can be tracked in the dictLAB
    
      lapply(newlabs[!current_recoded_label_finder], function(modified_colname) {  browser()
          former_new <- modified_colname
          if (dictLAB$has(former_new) ){
            recoded_word <- dictLAB$get(former_new)
            recodeAC_op_df[[1]] [recodeAC_op_df[[1]] == former_new] <<- recoded_word 
          } else { 
            
            #if(x[2]) {
            #  recodeAC_op_df[[1]] [recodeAC_op_df[[1]] == former_new]   <<- x[1]
            print("x")
            #}
            limited_selected_subset<-recodeAC_op_df[recodeAC_op_df[1] == former_new,2]
            keys <-as.character(dict_relab$keys()) 
            bool <-keys %in% limited_selected_subset
            if (any(bool)) {     #se una delle voci e' stata fatta inrelab
        #      first_version_original_label <- dict_relab$get(keys[bool])
         #     recodeAC_op_df[[1]] [recodeAC_op_df[[1]] == former_new] <<- first_version_original_label 
          #    return(find_and_update_previous_actionsDFrecode_AC(recodeAC_op_df, current_df, dictLAB,gvariable))
            }
            #else there is no such entry in the df
            #store the entry (it is an operation connected to a deleted column in a dictionary)
            analyzed_row <- recodeAC_op_df[recodeAC_op_df$newlab %in% former_new, ]
            #cleans current df of the lost op related to deleted col
            recodeAC_op_df <<- recodeAC_op_df[!recodeAC_op_df$newlab %in% analyzed_row$newlab,]
          }
      })
    
    return(find_and_update_previous_actionsDFrecode_AC(recodeAC_op_df, current_df, dictLAB,gvariable))
  }
}
