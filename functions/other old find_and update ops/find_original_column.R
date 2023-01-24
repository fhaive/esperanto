# I: the column name of the column wished to restore (col) , the original_dataframe (orig_df), the objects storing recoding ops (gvariable <- recodeAc, recodeRJ)
# the column name could be present in the original_dataframe OR could be recoded before deletion. In that case, it must be searched in the recoding ops
# O: the original colname corresponding to col (the column name to restore)

find_original_column <- function (col, orig_df, gvariables) {
  browser()
  
  recodeRJ_op_list <- gvariables[[1]]
  recod_ops_dataframe <- gvariables[[2]]
  
  tmp_old_cols <- as.data.frame(sapply(recodeRJ_op_list, "[[", 1)[1:2,])
  index_retrievals_recRJ <- which( as.character(tmp_old_cols[1,]) %in% col) 
  retrievals_recRJ <- ifelse(!is_empty(index_retrievals_recRJ),    unique(as.character(tmp_old_cols[2,index_retrievals_recRJ]))  ,NA)
    
    
  retrievals_recAC <- unique(recod_ops_dataframe[recod_ops_dataframe$newlab %in% col,]$oldlab) 
  retrieved_colnames <- unique(c(retrievals_recAC, retrievals_recRJ)) %>% na.omit(.) %>% .[. != col]
  
  orig_colnames <- colnames(orig_df)
  if (retrieved_colnames %in% orig_colnames) {
    orig_column_df <- orig_df[, retrieved_colnames, drop=FALSE]
    return (orig_column_df)
  } else {  #it is not in the current_df so, the value was recoded at some point and thus can be tracked in the dictLAB
    # former_new %in% collections::dictLAB$keys())
    orig_column_df <- NULL
    return(orig_column_df)
  }
}
