#' Finds duplicate columns present in a dataset
#'
#' @description
#' 'duplicate_finder' cross-compares the columns of the input dataframe (df) to retrieve (duplicated) columns identical in their content, but eventually different regarding their column names.
#' The cross comparison also deals with a typical situation present in the phenodata, such as when the cell contents of a certain column appears preceded by a tag, i.e. "age: 22".
#' @param df A dataframe
#' @return  A list where each element contains a string vector composed by the names of the identical columns retrieved by the dataset (df).
#' @examples
#' \dontrun{
#' df <- data.frame(gender=c("Male","Female"),age=c(c(25,37),c(24,53)), country= c("Italy","Italy"), age_ch1=c(c("age: 25", "age: 37"),c("age: 24","age: 53")),nations= c("nation: Italy","nation: Italy"),age_ch2=c(c(25,37),c(24,53)) )
#' duplicates <- duplicate_finder(df)
#'
#' }
#' @keywords internal
#' @export


duplicate_finder <- function (df)  {

  #some columns present a pattern like "tag: cell_content", so everything before ": " should be removed
    tmp <- as.data.frame(sapply(df,function(x) gsub("^.*: ","",x)))

    cols_with_duplicates <- colnames(tmp[duplicated(as.list(tmp))])
    # for each column name retrieved, the duplicates are grouped and stored.
    duplicate_groupings <- lapply(cols_with_duplicates, function(column) {
         names(which(apply(tmp, 2, identical, tmp[, column])))
        })
    out <- unique(duplicate_groupings)
    out
}

