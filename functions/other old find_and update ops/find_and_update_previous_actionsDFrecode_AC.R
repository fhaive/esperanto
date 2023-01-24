# This function takes a list element representing one previously performed operation (recodeAC_op_df), the current metadata dataset (current_df), label and content dictionaries storing as key the old value (dictLAB,dictCONT), gvariable (containing the deleted op list and the dict with ops related to deleted columns)
#I:   list of the former_new_elements set during previous ops (recodeAC_op_df)
#     current metadata dataset (current_df)
#     dict containing the recoded labels(dictLAB) and contents (dictCONT) where k:v are former_new_value: newly_recoded_current_value 
#O: return the list of  words processed in the previous operations updated with their recoded versions and stores the ops related to deleted columns (if present)  

find_and_update_previous_actionsDFrecode_AC <-  function(recodeAC_op_df, current_df, dictLAB, gvariable) {
  #browser()
  recodeAC_op_df <- unique(recodeAC_op_df)
  
  newlabs <- (recodeAC_op_df[[1]]) 
  
  #find the ops belonging to deleted elements
  dict_DELrelated_ops <- gvariable[[2]]
  deleted <- gvariable[[1]]
  del_recAC_ops_archive <- recodeAC_op_df[recodeAC_op_df$newlab %in% NULL,]
  
  #deleted_index <- newlabs %in% deleted
  #numeric_index <- which(deleted_index %in% TRUE)
  #recodeAC_op_df <- recodeAC_op_df[-numeric_index,] 
  
  #check if there is  a subvocabulary with the proper type of ops otherwise create an empty one
  if(!dict_DELrelated_ops$has("recodeAC")){
    browser()
    dict_DELrelated_ops <- dict_DELrelated_ops$set("recodeAC", del_recAC_ops_archive)
    #    recodeRJ_op_list <- recodeRJ_op_list[-numeric_index]
    #    recode_RJ_df <- recode_RJ_df[,-numeric_index]
  }
  
  #updates values
  recode_acc_index <-  1:nrow(recodeAC_op_df)
  newlabs <- (recodeAC_op_df[[1]]) 
  
  
  current_recoded_label_finder   <- newlabs %in% colnames(current_df) 
  if (all(current_recoded_label_finder) | is_empty(current_recoded_label_finder)) {
    return (recodeAC_op_df)
  }  else { #it is not in the current_df so, the value was recoded at some point and thus can be tracked in the dictLAB
    # former_new %in% collections::dictLAB$keys())
    
    
    
    
    lapply(newlabs[!current_recoded_label_finder], function(modified_colname) {  #browser()
      former_new <- modified_colname
      if (dictLAB$has(former_new) ){
        recoded_word <- dictLAB$get(former_new)
        recodeAC_op_df[[1]] [recodeAC_op_df[[1]] == former_new] <<- recoded_word 
      } else { #else there is no such entry in the df
        #store the entry (it is an operation connected to a deleted column in a dictionary)
        analyzed_row <- recodeAC_op_df[recodeAC_op_df$newlab %in% former_new, ]
        if (analyzed_row[1] %in% deleted) {
          tmp_df <- unique(rbind(dict_DELrelated_ops$get("recodeAC"), analyzed_row )) 
          dict_DELrelated_ops <- dict_DELrelated_ops$set("recodeAC", tmp_df)
          
        }
        
       # if (dict_DELrelated_ops$size() !=0) {   #dict_DELrelated_ops$has("recodeAC") vedi se basta fare il rbind con larchive che dovrebbe essere tutti o nel caso che non ci sono valori
          #sempre set cn key: rbind(qui seguente, archive e contentuto del dict precedente - values non si puo usare)
      #    del_recAC_ops_archive <- rbind(del_recAC_ops_archive, dict_DELrelated_ops$values()[[1]]) 
      #  }  
      #  dict_DELrelated_ops <- dict_DELrelated_ops$set("recodeAC",       unique(rbind( del_recAC_ops_archive, analyzed_row)))
        gvariable[[2]]<- dict_DELrelated_ops
        assign("gVars$dict_DELrelated_ops", dict_DELrelated_ops, envir = globalenv())
        #cleans current df of the lost op related to deleted col
        recodeAC_op_df <<- recodeAC_op_df[!recodeAC_op_df$newlab %in% analyzed_row$newlab,]
      }
    }
    )
    
    return(find_and_update_previous_actionsDFrecode_AC(recodeAC_op_df, current_df, dictLAB,gvariable))
  }
}
