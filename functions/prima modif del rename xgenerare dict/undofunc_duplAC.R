undofunc_duplAC <- function (torestore) {
  #torestore: list(previous_step_df, fullstring)
  tmp <- torestore[["dupl_AC"]]
  gVars$processedD <-  gVars$processedD[gVars$processedD != tmp[[2]]]
  gVars$phTable[[1]] <- tmp[[1]]  #to restore previous step df
}
