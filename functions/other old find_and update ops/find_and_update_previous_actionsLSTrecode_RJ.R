# This function takes a list element representing one previously performed operation (recodeRJ_op_list), the current metadata dataset (current_df), 
#label dictionary storing as key the old label (dictLAB) and the global variable with the deleted columns

#I:   list of the former_new_elements set during previous ops (list_former_new_value)
#     current metadata dataset (current_df)
#     dict containing the recoded labels(dictLAB) and contents (dictCONT) where k:v are former_new_value: newly_recoded_current_value 
#O: return the list of  words processed in the previous operations updated with their recoded versions  

find_and_update_previous_actionsLSTrecode_RJ <-  function(recodeRJ_op_list, current_df, dictLAB, gvariable) {
  browser()
  #recodeRJ_op_list[[5]][[1]][1] <- "pinco"
  
  
  recodeRJ_op_list <- recodeRJ_op_list[!(duplicated(recodeRJ_op_list) & duplicated(names(recodeRJ_op_list)))]
  
  #recodeRJ_op_list <- append(recodeRJ_op_list, list(recode_RJ= list(c("source_name_ch1","vecc",NA,NA))))
  
  recode_RJ_df <- as.data.frame(sapply(recodeRJ_op_list, "[[", 1)[1:2,])  #one recRj op per column
  newlabs <- as.character(recode_RJ_df[1,]) 
  
  #clean the ops from deleted elements
  deleted <- gvariable[[1]]
  dict_DELrelated_ops <- gvariable[[2]] 
  
  deleted_index <- newlabs %in% deleted
  numeric_index <- which(deleted_index %in% TRUE)
  
 # if (dict_DELrelated_ops$size() !=0 & !is_empty(numeric_index)) {
#    del_recAC_ops_archive <- rbind(del_recAC_ops_archive, dict_DELrelated_ops$values()[[1]]) 
#  }  
  if(!is_empty(numeric_index) & !dict_DELrelated_ops$has("recodeRJ")){
   
    dict_DELrelated_ops <- dict_DELrelated_ops$set("recodeRJ", list())
#    recodeRJ_op_list <- recodeRJ_op_list[-numeric_index]
#    recode_RJ_df <- recode_RJ_df[,-numeric_index]
  }
  
  
  
  
  #updates values
  #recode_RJ_df <- recode_RJ_df[,-numeric_index]
  recode_rej_index <-  1:length(recodeRJ_op_list)
  newlabs <- as.character(recode_RJ_df[1,]) 
  
  current_recoded_finder   <- newlabs %in% colnames(current_df) 
  if (all(current_recoded_finder)| is_empty(current_recoded_finder)) {
    return (recodeRJ_op_list)
  }  else { #it is not in the current_df so, the value was recoded at some point and thus can be tracked in the dictLAB
    # former_new %in% collections::dictLAB$keys())
    
    
    
    
    lapply(recode_rej_index[!current_recoded_finder], function(l) {  
      browser()
      former_new <- recodeRJ_op_list [[l]][[1]][2]
      if (dictLAB$has(former_new) ){
        recoded_word <- dictLAB$get(former_new)
        recodeRJ_op_list [[l]][[2]] <<- recoded_word 
      } else { #names(lbis) <- rep("deleted", length(lbis))
        new_el <- recodeRJ_op_list [l][[1]][[1]][1]
        if (new_el %in% deleted) {
          list_tmp <- unique(append(dict_DELrelated_ops$get("recodeRJ"), recodeRJ_op_list [l] )) 
          names(list_tmp) <-        rep("recode_RJ", length(list_tmp))            
          dict_DELrelated_ops <- dict_DELrelated_ops$set("recodeRJ", list_tmp)
          
          
          gvariable[[2]]<- dict_DELrelated_ops
          assign("gVars$dict_DELrelated_ops", dict_DELrelated_ops, envir = globalenv())                                                
        }
        recodeRJ_op_list [l] <<-NULL
      }
    }
    )
    
    return(find_and_update_previous_actionsLSTrecode_RJ(recodeRJ_op_list, current_df, dictLAB,gvariable))
  }
}
