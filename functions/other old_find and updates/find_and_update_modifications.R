#input: current recode_AC new values, op list during relabelli, duplicate removal phases (relab_op_list, dupl_op_list)
#output: updated op lists (relab_op_list, dupl_op_list) were the old values are updated with new values of recode_AC

# it finds the column of the dataset named by label synonyms that should be renamed with the allowed label. 
find_and_update_modifications <- function(recode_item, gVars){
  req(!is_empty(gVars))
 
  name_item <- names(recode_item)
  
  if(names(last5list[index]) %in% c("relab_AC", "relab_RJ") ){
      ops <-find_and_update_modifications_relab (recode_item, gVars$relab_op_list)
      gVars$processedL[gVars$processedL== as.character(recode_item[[1]][2]) ] <- as.character(recode_item[[1]][1])
  } else if (names(last5list[index]) %in% "dupl_AC"){
      last5list[[index]][1] <- recoded_new_colnameitem
      last5list[[index]][[2]] <- str_replace(last5list[[index]][[2]], paste0("^",recoded_old_colnameitem,"$") , recoded_new_colnameitem)
      
    } else if (names(last5list[index]) %in% "deleted"){   #controlla
    } else if (names(last5list[index]) %in% "recode_AC"){  #controlla
    } else if (names(last5list[index]) %in% "recode_RJ"){  #controlla
    } else {return(NULL)} #.specials
  
   #relab section
   relab_ops <- relab_op_list
   former_new_in_relab <- map(relab_ops,1)
   index <- which(former_new_in_relab %in% recoded_old_colnameitem, arr.ind=TRUE)
   if (!is_empty(index)) {
     relab_ops[index] %<>% map2(recoded_new_colnameitem, ~ assign_in(.x, 1, .y))
   } else {
     return(relab_ops)
   }
   
 
   ops
  }