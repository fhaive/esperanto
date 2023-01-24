add_to_dict <- function (old_dict, k,v){
  if(!is.null(old_dict)){
    dic <- collections::dic$set(c(k),v)
  } else {
    
    dic <- collections::dict(setNames(as.list(v),k))
  }
  
  dic
}
  
  