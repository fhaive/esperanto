undofunction <- function (torestore){
      #  require(data.table)
     # torestore <- tail (gVars$last5list,1)[[1]]
    #  df <- gVars$phTable[[1]] #actual df
    
        
        
       
      op_names<- names(torestore)
      lapply(op_names, function (k)
      fcase(k == "relab_AC", undofunc_relabAC(torestore),
            k == "relab_RJ", undofunc_relabRJ(torestore),
            k == "dupl_AC", undofunc_duplAC(torestore)
            #,
           # .== "col_DELETE", undofunc_del_COL(torestore),
             
        
       
        
      )#chiu fcase
      )#chius list

      
}



undofunc_relabAC <- function (torestore) {
  #torestore: list(chosen_lab, chosen_match, former df)
  tmp <- torestore[["relab_AC"]]
  df <- gVars$phTable[[1]] #actual df
  data.table::setnames(df, tmp[[1]], tmp[[2]])
  gVars$phTable[[1]] <-df
  gVars$processedL <-  gVars$processedL[gVars$processedL != tmp[[2]]]
}


undofunc_relabRJ <- function (torestore) {
  #torestore: list(chosen_match)
  tmp <- torestore[["relab_RJ"]]
  gVars$processedL <-  gVars$processedL[gVars$processedL != tmp[[1]]]
}


undofunc_duplAC <- function (torestore) {
  #torestore: list(previous_step_df, fullstring)
  tmp <- torestore[["dupl_AC"]]
  gVars$processedD <-  gVars$processedD[gVars$processedD != tmp[[2]]]
  gVars$phTable[[1]] <- tmp[[1]]  #to restore previous step df
}
