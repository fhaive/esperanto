undofunction <- function (last_operations, df){
        
      torestore <- tail (gVars$last5list,1)[[1]]
      df <- gVars$phTable[[1]]
    
        
        
       
      op_names<- names(torestore)
      lapply(op_names,
      fcase(.== "relab_AC", (tmp <- torestore[["relab_AC"]]
                            data.table::setnames(df, tmp[[1]], tmp[[2]])
                            gVars$phTable[[1]] <-df
                            gVars$processedL <-  tmp[[2]]
                            ),
            .== "relabRJ", (tmp <- torestore[["relab_AC"]]
                            gVars$processedL <-  tmp[[1]]
                            )
        
        
        
        
      )#chiu fcase
      )#chius list
      
    #  fcase(
     #   x < 5L, 1L,
    #    x > 5L, 3L
     # )
      
      
      
}