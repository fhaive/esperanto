# This function takes a list element representing one previously performed operation (list_former_new_value), the current metadata dataset (current_df), label and content dictionaries storing as key the oldand shorten it to a specificied lenght (nchar), included the ".." placed as placeholder for the ripped part.
#I:   list of the former_new_elements set during previous ops (list_former_new_value)
#     current metadata dataset (current_df)
#     dict containing the recoded labels(dictLAB) where k:v are former_new_value: newly_recoded_current_value 
#O: return the list of  words processed in the previous operations updated with their recoded versions  

find_and_update_previous_actionsLSTrelab_AC <-  function(list_relab_ops, current_df, dictLAB, gvariable,current_env) {
  browser()
  relab_acc_index <-  grep("relab_AC",names(list_relab_ops))
  candidates <- purrr::map(list_relab_ops[relab_acc_index],1)
  #tosubstitute <- purrr::map(list_relab_ops[relab_acc_index],2) 
  #columns_relab_RJ <- as.character(candidates [grepl("relab_RJ",names(candidates))])
  
  candidate_df <- as.data.frame(sapply(candidates, "[[", 1) )  #new relabelled labels per row 
  newlabs <- as.character(candidate_df[,1]) 
  
  #clean the ops from deleted elements
  deleted <- gvariable[[1]]
  dict_DELrelated_ops <- gvariable[[2]] 
  deleted_index <- newlabs %in% deleted
  numeric_index <- which(deleted_index %in% TRUE)
  #current_env<- new.env()
  
  if(!is_empty(numeric_index) & !dict_DELrelated_ops$has("relabAC")){
    
    dict_DELrelated_ops <- dict_DELrelated_ops$set("relabAC", list())
    #    recodeRJ_op_list <- recodeRJ_op_list[-numeric_index]
    #    recode_RJ_df <- recode_RJ_df[,-numeric_index]
  }
  
  
  
  #map(list_relab_ops [grepl("relab_AC",names(list_relab_ops))],1)  
  not_recoded_labels_finder   <- candidates %in% colnames(current_df) 
  if (all(not_recoded_labels_finder)) {
    return (list_relab_ops)
  } else {  #it is not in the current_df so, the value was recoded at some point and thus can be tracked in the dictLAB
    # former_new %in% collections::dictLAB$keys())
   #current_env <- new.env()
    lapply(relab_acc_index[!not_recoded_labels_finder], function(l) { browser()
      former_new <- list_relab_ops [[l]][[1]]
      
      if (dictLAB$has(former_new) ){
        recoded_word <- dictLAB$get(former_new)
        list_relab_ops [[l]][[1]] <<- recoded_word 
        assign("list_relab_ops1", list_relab_ops, (current_env))    
      } else { 
      #  if(former_new %in% columns_relab_RJ) { return (return(return(list_relab_ops))) }
       # else {list_relab_ops [[k]] <<-NULL}
        browser()
       # current_env<- new.env()     
        new_el <- list_relab_ops [l][[1]][[1]][1]
        if (new_el %in% deleted) {
          list_tmp <- unique(append(dict_DELrelated_ops$get("relabAC"), list_relab_ops [l] )) 
          names(list_tmp) <-        rep("relabAC", length(list_tmp))            
          dict_DELrelated_ops <- dict_DELrelated_ops$set("relabAC", list_tmp)
          gvariable[[2]]<- dict_DELrelated_ops
          assign("gVars$dict_DELrelated_ops", dict_DELrelated_ops, envir = globalenv())
          
        }
        
        browser()
        #current_env$list_relab_ops <- list_relab_ops[-l]
        list_relab_ops <- list_relab_ops[-l]
       #list_relab_ops [l] <- NULL
   #     mget(ls(current_env), envir = current_env)
        assign("list_relab_ops1", list_relab_ops, (current_env))      #current_env$list_relab_ops, (current_env))
       # list_relab_ops <<- list_relab_ops
         }
    }
    )
    browser()
    print(list_relab_ops)
    
    return(find_and_update_previous_actionsLSTrelab_AC(current_env$list_relab_ops1, current_df, dictLAB, gvariable,(current_env)))
  }
}
