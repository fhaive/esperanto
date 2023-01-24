undofunc_relabRJ <- function (torestore) {
  #torestore: list(chosen_match)
  tmp <- torestore[["relab_RJ"]]
  gVars$processedL <-  gVars$processedL[gVars$processedL != tmp[[1]]]
}
