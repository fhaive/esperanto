#input: current deleted value , gVars
#output: updated gVars were all the ops including the todelete name were removed

# it finds the column of the dataset named by label synonyms that should be renamed with the allowed label. 
find_and_update_modifications_del <- function(todelete_item, gVars){
  req(!is_empty(gVars))
  todelete <- as.character(todelete_item[[1]][1])
  #recoded_old_colnameitem <- as.character(recode_item[[1]][2])
   
  
   #relab section
  relab_ops <- gVars$relab_op_list
  former_new_in_relab <- map(relab_ops,1)
  index_rel <- which(former_new_in_relab %in% todelete, arr.ind=TRUE)
  if (!is_empty(index_rel)) { 
      relab_ops[index_rel] <- NULL
      gVars$relab_op_list <- relab_ops
  }
  
  #dupl section
  dupl_ops <- gVars$dupl_op_list
  former_new_in_dupl <- map(dupl_ops,1)
  index_dupl <- which(former_new_in_dupl %in% todelete, arr.ind=TRUE)
  if (!is_empty(index_dupl)) { 
    dupl_ops[index_dupl] <- NULL
    gVars$dupl_op_list <- dupl_ops
  }
  
  #recodeRJ part
  recRJ_ops <- gVars$recodeRJ_op_list
  former_new_in_recRJ <- map(recRJ_ops,1)
  index_recRJ <- which(former_new_in_recRJ %in% todelete, arr.ind=TRUE)
  if (!is_empty(index_recRJ)) { 
    recRJ_ops[recRJ_ops] <- NULL
    gVars$recodeRJ_op_list <- recRJ_ops
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