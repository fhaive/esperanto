#input: current recode_AC new values, op list during relabelli, duplicate removal phases (relab_op_list, dupl_op_list)
#output: updated op lists (relab_op_list, dupl_op_list) were the old values are updated with new values of recode_AC

# it finds the column of the dataset named by label synonyms that should be renamed with the allowed label. 
find_and_update_modifications_dupl <- function(recode_item, dupl_op_list){
  req( !is_empty(dupl_op_list))
  recoded_new_colnameitem <- as.character(recode_item[[1]][1])
  recoded_old_colnameitem <- as.character(recode_item[[1]][2])
   
  browser()
   
   dupl_ops <- dupl_op_list
   former_new_in_dupl <- map(dupl_ops,1)
   index <- which(former_new_in_dupl %in% recoded_old_colnameitem, arr.ind=TRUE)
   if (!is_empty(index)) {
     dupl_ops[[index]][1] <- recoded_new_colnameitem
     dupl_ops[[index]][[2]] <- str_replace(dupl_ops[[index]][[2]], paste0("^",recoded_old_colnameitem,"$") , recoded_new_colnameitem)
   } else {
     return(dupl_ops)
   }
   
   dupl_ops
  }