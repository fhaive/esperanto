convert_dict_to_df <- function(list_dict){
  dLAB <- list_dict[[1]]$as_list() %>% purrr::list_modify("NA" = NULL) 
  dCONT <- list_dict[[2]]$as_list() %>% purrr::list_modify("NA" = NULL) %>% lapply(., function(u) u[!duplicated(u)]) %>% lapply(., function(x) if(any(!is.na(x))) {x[!is.na(x)]} else {x} ) 
  dLAB_CONT <- list_dict[[3]]$as_list() %>% purrr::list_modify("NA" = NULL)
  
  #trasnform lists to df and unlist the synonyms part
  vLAB<-lapply(dLAB, function(x) unlist(strsplit(x, split="\\|"))) %>% lapply(., function(u) u[!duplicated(u)]) %>% 
    lapply(., function(x) if(any(!is.na(x))) {x[!is.na(x)]} else {x} )   %>% stack(.)
  
  vCONT <- stack(dCONT)
  colnames(vCONT) <- c("value","ind")    
  vLAB_CONT<-lapply(dLAB_CONT, function(x) unlist(strsplit(x, split="\\|"))) %>% lapply(., function(u) u[!duplicated(u)]) %>% 
    lapply(., function(x) if(any(!is.na(x))) {x[!is.na(x)]} else {x} )  %>% stack(.)
  
  
  
  #merging LAB and LAB_CONT dataframes and later it with content dataframe
  tmpLC <- merge(vLAB_CONT,vLAB, by = "ind", all=TRUE)
  colnames(tmpLC) <- c("label", "ind","Lab_Syn") #c("label", "ind","lab_syn", "ind")
  final_dict <- merge(vCONT,tmpLC, by = "ind",all=TRUE) %>% .[,c("label","Lab_Syn", "ind", "value")] %>%
    plyr::rename(., c("ind"= "allowed_features", "value"= "Syn_Features")) %>% .[order(.$label),]
  
  #different cleanings
  final_dict[final_dict == "NA"] <- NA 
  final_dict[final_dict == ""] <- NA 
  final_dict <- final_dict %>%.[!duplicated(.),] 
  final_dict []<- lapply(final_dict , gsub, pattern="^,", replacement="")  
  final_dict<- final_dict[rowSums(is.na(final_dict)) != ncol(final_dict), ] %>%
    separate_rows(., Syn_Features, sep = "\\|")
  
  final_dict
}
