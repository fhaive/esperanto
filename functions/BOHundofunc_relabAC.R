#
#
undofunc_relabAC <- function (torestore) {
  #torestore: list(chosen_lab, chosen_match, former df)
  print("//((/&//(/(")
  browser()
  tmp <- torestore[["relab_AC"]]
  df <- gVars$phTable[[1]] #actual df
  data.table::setnames(df, tmp[[1]], tmp[[2]])
  gVars$phTable[[1]] <-df
  gVars$processedL <-  gVars$processedL[gVars$processedL != tmp[[2]]]
}