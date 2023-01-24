#input: current deleted value , gVars
#output: updated gVars were all the ops including the todelete name were removed

# it finds the column of the dataset named by label synonyms that should be renamed with the allowed label. 
find_and_update_modifications_recRJ <- function(torecodeRJ_item, gVars){
  req(!is_empty(gVars))
  new_torecodeRJ_colnameitem <- as.character(torecodeRJ_item[[1]][1])
  old_torecodeRJ_colnameitem <- as.character(torecodeRJ_item[[1]][2])
  
  
  
  
  #relab section
  relab_ops <- gVars$relab_op_list
  former_new_in_relab <- map(relab_ops,1)
  index_rel <- which(former_new_in_relab %in% old_torecodeRJ_colnameitem, arr.ind=TRUE)
  if (!is_empty(index_rel)) { 
    relab_ops[index_rel] %<>% map2(recoded_new_colnameitem, ~ assign_in(.x, 1, .y)) 
    gVars$relab_op_list <- relab_ops
  }
  
  #dupl section
  dupl_ops <- gVars$dupl_op_list
  former_new_in_dupl <- map(dupl_ops,1)
  index_dupl <- which(former_new_in_dupl %in% old_torecodeRJ_colnameitem, arr.ind=TRUE)
  if (!is_empty(index_dupl)) { 
    dupl_ops[[index_dupl]][1] <- new_torecodeRJ_colnameitem
    dupl_ops[[index_dupl]][[2]] <- str_replace(index_dupl[[index]][[2]], paste0("^",old_torecodeRJ_colnameitem,"$") , new_torecodeRJ_colnameitem)
    gVars$dupl_op_list <- dupl_ops
  }
  
  
  
  #delete part
  delete_ops <- gVars$delete_op_list
  former_new_in_del <- map(delete_ops,1)
  index_del <- which(former_new_in_del %in% todelete, arr.ind=TRUE)
  if (!is_empty(index_recRJ)) { 
    
    gVars$delete_op_list <- delete_ops
  }
  
  #recodeAC part
  recAC_ops <- gVars$recod_ops_dataframe
  former_new_in_recAC <- recAC_ops[[1]]
  index_recAC <- which(former_new_in_recAC %in% todelete, arr.ind=TRUE)
  if (!is_empty(index_recAC)) { 
    recAC_ops <-recAC_ops[-index_recAC,]
    gVars$recod_ops_dataframe <- recAC_ops
  }
  
  
  #vocabulary
  #DA TESTARE
  vocab_all <- gVars$later_voc_upd_dfTEST
  dict_label <- gVars$dkeys$label
  
  already_in_vocab <- vocab_all[,c(2,3)]
  index_vocab <- which(     already_in_vocab[[1]]%in% todelete |
                              already_in_vocab[[2]]  %in% todelete,
                            arr.ind=TRUE)
  if (!is_empty(index_vocab)) { 
    vocab_all <-vocab_all[-index_vocab,]
    gVars$later_voc_upd_df <- vocab_all
  }
  
  
  gVars
}