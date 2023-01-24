
# Determine color of the font according to the presence or not of the word in a dataset. In particular, it colors the word green if present as allowed content or label, blue f it is a synomym, red if it is absent.
colorwords <- function(word, dataset){
  allowed_d <- unique(unlist(dataset[,c(1,3)]))
  syn_d <- unique(unlist(dataset[,c(2,4)]))
  outTxt <- ' '
  #allowed labels
  if (is.element(word, allowed_d)){
    font <- "green"
  } else if (is.element(word, syn_d)){
    font <- "blue"
  } else {
    font <- "red"
  }
  # Create html
  formatedFont <- sprintf('<b><font color="%s">%s</font></b>',font,word) 
  
  # Append to text to show
  outTxt <- paste(outTxt, formatedFont,collapse=' ')
  
  outTxt
  }