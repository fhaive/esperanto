collecter_del_related_opsRELAB_AC <-  function(col_todel, current_df, gvariable) {    # relab_op_list , dict_DELrelated_ops , 
  browser()
  dict_DELrelated_ops <- gvariable$dict_DELrelated_ops
  dictLAB <- gvariable$dict_modifLAB
  #creates all key:value pairs (i.e. key category relab:list of relab_ops  
  if(!dict_DELrelated_ops$has("relabAC")){
    dict_DELrelated_ops <- dict_DELrelated_ops$set("relabAC", list())
  }
  
  current_df<-df
  x<-col_todel
  deleted <- col_todel   #deleted <-input$pick_cols_recoding
  list_relab_ops <- gvariable$relab_op_list
  relab_acc_index <-  grep("relab_AC",names(list_relab_ops))
  newlabs <- as.character(purrr::map(list_relab_ops[relab_acc_index],1))
  
  
  #delted =RECODED   ntissue
  #devi trovare la vecchia    tissue
  #if deleted is not in the relabelled entries, it was recoded and it is in the dictLAB with recoded versions tored old:new
  if (!deleted %in% newlabs)
       
  boo_ind_newlabs<- deleted[deleted %in%  dictLAB$keys()] 
  #check presence of recoded versions in the dictLAB
  if(all( ))
  if(dictLAB$has(deleted)) {
      deleted <- c(col_todel, dictLAB$get(deleted)) 
  }
  found_indices <- newlabs %in%   deleted
  newlabs
  
  boo_ind_dictlab <-  ((dictLAB$keys() %in% newlabs)) 
  boo_ind_newlabs<- newlabs %in%  dictLAB$keys() 
  if (any(boo_ind_newlabs)){ browser()
    updated_version_newlabel <- newlabs
    updated_version_newlabel[boo_ind_newlabs] <- as.character(dictLAB$values()[boo_ind_dictlab])
    #selected_items <-
  } #else { selected_items}
  selected <- unique(c(newlabs,updated_version_newlabel))
  
  numeric_index <- which(newlabs %in% deleted) 
  
  lapply (newlabs, function(l) { browser()
    former_new <- list_relab_ops [[l]][[1]]
    
    if (dictLAB$has(former_new) ){
      recoded_word <- dictLAB$get(former_new)
      list_relab_ops [[l]][[1]] <<- recoded_word 
      assign("list_relab_ops1", list_relab_ops, (current_env))    
    }
  if (!is_empty(numeric_index) ){
  }
  
  deleted_index <- newlabs %in% deleted
  })
  
  if(!is_empty(numeric_index) & !dict_DELrelated_ops$has("relabAC")){
    
    dict_DELrelated_ops <- dict_DELrelated_ops$set("relabAC", list())
  }
  
  #gVars$recode
  #relab_op_list
  #dupl_op_list
  #recodeRJ_op_list
  #recod_ops_dataframe
  
}