#input: current recode_AC new values, op list during relabelli, duplicate removal phases (relab_op_list, dupl_op_list)
#output: updated op lists (relab_op_list, dupl_op_list) were the old values are updated with new values of recode_AC

# it finds the column of the dataset named by label synonyms that should be renamed with the allowed label. 
find_and_update_modifications_forlast5 <- function(recode_item, last5list){
  req( !is_empty(last5list))
  recoded_new_colnameitem <- as.character(recode_item[[1]][1])
  recoded_old_colnameitem <- as.character(recode_item[[1]][2])
   
  browser()
   
  last5dict <- dict(last5list)
  
   former_new_in_last5 <- map(last5list,1)
   index <- which(former_new_in_last5 %in% recoded_old_colnameitem, arr.ind=TRUE)
   
   if (!is_empty(index)) {
     #last5list[[index]][1] <- recoded_new_colnameitem
     if(names(last5list[index]) %in% c("relab_AC", "relab_RJ") ){
       last5list[index] %<>% map2(recoded_new_colnameitem, ~ assign_in(.x, 1, .y))
     } else if (names(last5list[index]) %in% "dupl_AC"){
          last5list[[index]][1] <- recoded_new_colnameitem
          last5list[[index]][[2]] <- str_replace(last5list[[index]][[2]], paste0("^",recoded_old_colnameitem,"$") , recoded_new_colnameitem)
       
     } else if (names(last5list[index]) %in% "deleted"){   #controlla
     } else if (names(last5list[index]) %in% "recode_AC"){ 
       print("dqdwq")
       #controlla  lista di riga df
     } else if (names(last5list[index]) %in% "recode_RJ"){  #controlla lista con elmen ogni  riga df
     } else {return(NULL)} #.specials
     
     
     
     
     
     
    
   } else {
     return(last5dict$as_list())
   }
   
   last5dict
  }