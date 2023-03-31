#' Read all sheets from an excel document
#'
#' @description 
#' 'read_excel_allsheets' returns the content of the sheets of the excel file identified by the filepath in the arguments
#' @param x A filepath to a xls or xlsx file
#' @param y A flag whether convert output to dataframe
#' @return A list or a dataframe
#' @examples
#' \dontrun{
#' readxl::excel_sheets(filename)
#' readxl::excel_sheets(filename, tibble=FALSE)
#' }



read_excel_allsheets <- function(filename, tibble = FALSE) {
  sheets <- readxl::excel_sheets(filename)
  x <- lapply(sheets, function(X){ 
    
    y = readxl::read_excel(filename, sheet = X)
    
  }
  )
  
  if(!tibble) x <- lapply(x, as.data.frame)
  names(x) <- sheets
  return(x)
  
}
 