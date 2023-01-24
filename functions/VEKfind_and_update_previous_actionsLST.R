# This function takes a list element representing one previously performed operation (restore), the current metadata dataset (current_df) label and content dictionaries storing as key the oldand shorten it to a specificied lenght (nchar), included the ".." placed as placeholder for the ripped part.
#I:   list element representing one previously performed operation (restore)
#     current metadata dataset (current_df)
#O: return the shortened version of the input string, where the ripped part is substituted with "..."

find_and_update_previous_actionsLST <- function(restore, current_df, dictLAB, dictCONT) {
  browser()
  former_new <- restore[[1]]
  if (former_new %in% colnames(current_df)) {
    return (restore)
  } else {  #it is not in the current_df so, the value was recoded at some point and thus can be tracked in the dictLAB
        # former_new %in% collections::dictLAB$keys())
        restore[[1]] <- dictLAB$get(former_new)
        return(find_and_update_previous_actionsLST (restore, current_df, dictLAB, dictCONT))
        
       
  }
}
      
      
     