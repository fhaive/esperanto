# if retreives all potential duplicates of a dataframes which are returned as a list where each element contains a string vector composed by the names of identical columns.
# INPUT: dataset (df)
# OUTPUT: list (out) where each element contains a string vector composed by the names of identical columns.

duplicate_finder <- function (df)  {  
  
  #some columns present a pattern like "something: data", so everything before ": " should be removed
    tmp <- as.data.frame(sapply(df,function(x) gsub("^.*: ","",x)))
    
    # it retreives the name of the columns with a duplicate
   # cols_with_duplicates <- names(which(duplicated(t(tmp))))
    #cols_with_duplicates<-data.frame(t(tmp) )%>%
     #                                       filter(duplicated(.)) %>% rownames(.)
    cols_with_duplicates <- colnames(tmp[duplicated(as.list(tmp))])
    #for each name, the duplicates are grouped and stored.
    duplicate_groupings <- lapply(cols_with_duplicates, function(column) {
         names(which(apply(tmp, 2, identical, tmp[, column])))
        })
    out <- unique(duplicate_groupings)
    out
}



#dupl<-tmp[duplicated(as.list(tmp))]
