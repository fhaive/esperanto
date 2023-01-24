restore_deletion_ops <- function (del_linked_ops_dict, gvariable){
  browser()
  order_pattern <- c("relab_AC", "dupl_AC", "recode_RJ", "recode_AC")
  keys <- del_linked_ops_dict$keys() 
  
  
  unordered_gvariable <- lapply(keys, function(type_op) {browser()
           to_examine <- del_linked_ops_dict$get(type_op) 
           if(!is_empty(to_examine) & inherits(to_examine,"list")){
             #del_linked_ops_dict$as_list()
             gvariable[[type_op]] <- append(gvariable[[type_op]],to_examine )   #%>% setNames(., type_op)   #del_linked_ops_dict$get(type_op)  
        # gvariable
           }
          
           if(inherits(to_examine,"data.frame") && dim(to_examine)[1] != 0 ){
             df_colnames <- colnames(to_examine)
             gvariable[[type_op]] <- rbind(gvariable[[type_op]],to_examine ) %>%   #del_linked_ops_dict$get(type_op)
               setNames(., df_colnames)

             
             
           }
          # unordered_gvariable <- gvariable %>% setNames(., names(gvariable))
      gvariable[[type_op]]
  }) %>% setNames(., names(gvariable))
  browser()
  
  
  ordered <- unordered_gvariable[order_pattern]
  
  ordered
  
}