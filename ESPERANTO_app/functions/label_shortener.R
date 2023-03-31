#' Shortens a string to a predefined length.  
#'
#' @description 
#' 'label_shortener' takes an excessively long string (label) and shortens it to a specified length (nchar). It also adds ".." as placeholder for the ripped part.
#' The output consists of the shortened label including the "..".  
#' @param label A string.
#' @param nchar An integer to define the shortening threshold.
#' @return  A string with the shortened version of the input string (label).
#' @examples
#' \dontrun{
#' word_test1 <- "English"
#' label_shortener(word_test1, 7)
#' word_test2 <- "nationality"
#' label_shortener(word_test2, 7)
#' word_test3 <- "test"
#' label_shortener(word_test3, 7)
#' word_test4<- "NY"
#' label_shortener(word_test4, 7)
#' }
#' @keywords internal
#' @export


label_shortener <- function(label, n_char) {
 
  
  label <- ifelse(nchar(label) > n_char, 
         {substr(label, 1, n_char) %>% paste0(., "...")},
         label)
  label
#  label <- as.character(label)
#  processed_labels <- sapply (label, function (lbl) {
    
#    if(nchar(lbl) <= n_char ) {
#          lbl
#    } else{    #(nchar(label) > (n_char - 3)) 
#          lbl <- substr(lbl, 1, n_char-10)  %>% paste0(., "...")
#    } 
#    lbl  
    
#  })

  
  
  
  
   # if(nchar(label) <= n_char ) {
#    label
#    } else{    #(nchar(label) > (n_char - 3)) 
#         label <- substr(label, 1, n_char)  %>% paste0(., "...")
       
#    }
#  label
    }