#' Attributes a color to code whether and how the word is present in the reference vocabulary.
#'
#' @description
#' 'colorwords' cross-compares the input string (word) and a reference vocabulary (dict_keys), attributing a specific color in relation to if and how the word is retrieved.
#' The reference vocabulary is a dataframe where each row shows the following fields: "reference label", "label synonyms","reference content", "content synonyms".
#' The output consists of a html string to color the word depending on whether it is present in the vocabulary: "Green" if it is stored as "reference label" or "reference content"; "Blue" if the word is a synonim of label or content; "Red" if it is absent.
#' @param word A string.
#' @param dict_keys A dataframe to use as reference vocabulary.
#' @return  A html string.
#' @examples
#' \dontrun{
#' library(magrittr)
#' dict_keys <-rbind(
#'               data.frame( label="sex" , lab_syn ="gender", allowed_features=c("M","F"), syn_features =c("Male,male","Female,female")  ) ,
#'               data.frame( label="organism" , lab_syn =NA, allowed_features=c("Homo Sapiens","Mus Musculus"), syn_features =NA)) %>%
#'               tidyr::separate_rows(.,c(syn_features))
#' word_test1 <- "English"
#' colorwords(word_test1, dict_keys)
#' word_test2 <- "Mus Musculus"
#' colorwords(word_test2, dict_keys)
#' word_test3 <- "gender"
#' colorwords(word_test3, dict_keys)
#' }
#' @keywords internal
#' @export



# Determine color of the font according to the presence or not of the word in a dataset. In particular, it colors the word green if present as allowed content or label, blue f it is a synomym, red if it is absent.
colorwords <- function(word, dict_keys){
  allowed_d <- unique(unlist(dict_keys[,c(1,3)]))
  syn_d <- unique(unlist(dict_keys[,c(2,4)]))
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
  outTxt <- c(paste(outTxt, formatedFont,collapse=' '))

  outTxt
  }
