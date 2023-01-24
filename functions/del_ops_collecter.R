del_ops_collecter <-function(columns, gvariable){
  
  columns <- columns
  dict_DELrelated_ops <- gvariable$dict_DELrelated_ops
  #creates all key:value pairs (i.e. key category relab:list of relab_ops  
  if(!dict_DELrelated_ops$has("relab_AC")){
    dict_DELrelated_ops <- dict_DELrelated_ops$set("relab_AC", list())
  }
  
  if(!dict_DELrelated_ops$has("dupl_AC")){
    dict_DELrelated_ops <- dict_DELrelated_ops$set("dupl_AC", list())
  }
  
  if(!dict_DELrelated_ops$has("recode_RJ")){
    dict_DELrelated_ops <- dict_DELrelated_ops$set("recode_RJ", list())
  }
  
  
  #relabelling ops
  list_relab_ops <- gvariable$relab_op_list
  ops <- as.data.frame(purrr::map(list_relab_ops,1)) 
  newlabs <- ops[1,]
  indices <- which(newlabs %in% columns & grepl("relab_AC",names(list_relab_ops)) )
  if (!is_empty (indices)){
      ops_to_get <- list_relab_ops[indices]
  
      ops_to_set <- unique(append(dict_DELrelated_ops$pop("relab_AC"), ops_to_get )) 
      names(ops_to_set) <-        rep("relab_AC", length(ops_to_set))            
      dict_DELrelated_ops <- dict_DELrelated_ops$set("relab_AC", ops_to_set)
      list_relab_ops[indices] <- NULL 
  }
  
  
  #duplicate removal ops DA TESTARE
  list_dupl_ops <- gvariable$dupl_op_list 
  newlabs <- as.character(purrr::map(list_dupl_ops,1)) 
  indices <- which(newlabs %in% columns  )
  if (!is_empty (indices)){
      ops_to_get <- list_dupl_ops[indices]
  
      ops_to_set <- unique(append(dict_DELrelated_ops$pop("dupl_AC"), ops_to_get )) 
      names(ops_to_set) <-        rep("dupl_AC", length(ops_to_set))            
      dict_DELrelated_ops <- dict_DELrelated_ops$set("dupl_AC", ops_to_set)
      list_dupl_ops[indices] <- NULL
  }
  
  
  #recodeRJ ops   DA TESTARE
  list_recodeRJ_ops <- gvariable$recodeRJ_op_list 
  newlabs <- as.character(purrr::map(list_recodeRJ_ops,1)) 
  indices <- which(newlabs %in% columns  )
  if (!is_empty (indices)){
      ops_to_get <- list_recodeRJ_ops[indices]
  
      ops_to_set <- unique(append(dict_DELrelated_ops$pop("recode_RJ"), ops_to_get )) 
      names(ops_to_set) <-        rep("recode_RJ", length(ops_to_set))            
      dict_DELrelated_ops <- dict_DELrelated_ops$set("recode_RJ", ops_to_set)
      list_recodeRJ_ops[indices] <- NULL
  }
   
  #recodeAC ops   DA TESTARE
  df_recodeAC_ops <- gvariable$recod_ops_dataframe 
  if(!dict_DELrelated_ops$has("recode_AC")){
    start_row_recoAC <- df_recodeAC_ops[df_recodeAC_ops$newlab %in% NULL,]
    dict_DELrelated_ops <- dict_DELrelated_ops$set("recode_AC", start_row_recoAC)
  }
  newlabs <- unique(df_recodeAC_ops$newlab) 
  #df_recodeAC_ops[df_recodeAC_ops$newlab %in% columns,]
  indices <- which(df_recodeAC_ops$newlab %in% columns )
  if (!is_empty (indices)){
    ops_to_get <- df_recodeAC_ops[indices,]
    ops_to_set <- unique(rbind(dict_DELrelated_ops$pop("recode_AC"), ops_to_get )) 
    dict_DELrelated_ops <- dict_DELrelated_ops$set("recode_AC", (ops_to_set))
    df_recodeAC_ops <- df_recodeAC_ops[-indices,]
  }
  
  return (list(dict_DELrelated_ops,
               list_relab_ops, list_dupl_ops, list_recodeRJ_ops,df_recodeAC_ops))
  
}