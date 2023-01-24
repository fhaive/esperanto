#input: current recode_AC new values, op list during relabelli, duplicate removal phases (relab_op_list, dupl_op_list)
#output: updated op lists (relab_op_list, dupl_op_list) were the old values are updated with new values of recode_AC

# it finds the column of the dataset named by label synonyms that should be renamed with the allowed label. 
find_and_update_modifications_relab <- function(recode_item, relab_op_list){
  req(!is_empty(relab_op_list))
  recoded_new_colnameitem <- as.character(recode_item[[1]][1])
  recoded_old_colnameitem <- as.character(recode_item[[1]][2])
  
   #relab section
   relab_ops <- relab_op_list
   former_new_in_relab <- map(relab_ops,1)
   index <- which(former_new_in_relab %in% recoded_old_colnameitem, arr.ind=TRUE)
   if (!is_empty(index)) {
     relab_ops[index] %<>% map2(recoded_new_colnameitem, ~ assign_in(.x, 1, .y))
   } else {
     return(relab_ops)
   }
   
 
   relab_ops
  }