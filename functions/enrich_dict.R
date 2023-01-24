# normal df transofrmed in list of dict objects as input
enrich_dict <- function (olddict, new_entries){
  browser()
  dLAB <- olddict[[1]]
  dCONT <- olddict[[2]]
  dLAB_CONT <- olddict[[3]]
  
  
  #k<-"testkey"
  #d$set(k,append(d$as_list()$k,4342))
  #new_entries[is.na(new_entries)] = ""
  new_entries<-data.frame(new_entries)
  
  rows <- nrow(new_entries)
  for (row in 1:rows){
    lab <- new_entries[row,1]
    s_lab  <- new_entries[row,2]
    #print(c(lab,s_lab))
    con <- new_entries[row,3]
    s_con <- new_entries[row,4]
    
    #str_c(dCONT$get(s_con, ""),s_con,sep="|") lab in stingr 1, lab in 3
    #  dLAB$set(lab, stringr::str_c(dLAB$get(lab,""),s_lab,sep="|"))#append(dLAB$as_list()$lab,s_lab))  
    #  dCONT$set(con, stringr::str_c(dCONT$get(s_con, ""),s_con,sep="|"))#append(dCONT$as_list()$con,s_con))
    #  dLAB_CONT$set(lab, stringr::str_c(dLAB_CONT$get(lab,""),con,sep="|"))# append(dLAB_CONT$as_list()$lab,s_con))
    
    dLAB$set(lab, append(dLAB$as_list()[[lab]],s_lab))  
    dCONT$set(con, append(dCONT$as_list()[[con]],s_con))
    dLAB_CONT$set(lab, append(dLAB_CONT$as_list()[[lab]],con))
    
  }
  
  #dLAB_CONT$as_list()
  #dLAB$as_list()
  #dCONT$as_list()
  full_dict <- list(dLAB, dCONT, dLAB_CONT)
  full_dict
}
