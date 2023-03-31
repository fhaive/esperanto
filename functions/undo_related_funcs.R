#' According to the specific operation to undo, it handles the related undo-sub_functions.  
#'
#' @description 
#' 'undofunction' takes a list element (torestore) and the list of global variables stored within the app. According to the operation to undo represented in torestore element, undofunction recalls the proper sub-function. 
#' The output consists of the result of the undo operation-specific subfunction, thus the app state before the undo of last operation.
#' @param torestore A named list element. The name identifies the type of operation performed to undo. 'torestore' structure varies according to the specific operation stored. 
#' @param gVars The list of global variables stored by the app.
#' @return  The app state before the performing the operation undone.
#' @keywords internal
#' @export


undofunction <- function (torestore, gVars){ 
  if (!is.null(torestore)){
    gVars <- gVars
    op_names<- names(torestore)
    if (op_names == "relab_AC") {return( undofunc_relabAC(torestore, gVars) ) }
    else if (op_names == "relab_RJ") {return( undofunc_relabRJ(torestore, gVars) ) }
    else if (op_names == "dupl_AC") { return (undofunc_duplAC(torestore, gVars))}
    else if (op_names == "deleted") {return (undofunc_delete(torestore, gVars))}
    else if (op_names == "recode_AC") {return (undofunc_recodeAC(torestore, gVars))}
    else if (op_names == "recode_RJ") {return (undofunc_recodeRJ(torestore, gVars))}
    else if( op_names %in% c("special_agiSPL","special_SPL","special_SPLREG")) {return(undofunc_specials_ALLsplits(torestore, gVars))}                      
    else if (op_names == "special_addEMPTY") {return(undofunc_specials_addEmpty(torestore, gVars))}
    else if (op_names == "multi") {return( undofunc_multi(torestore, gVars))}
    else {return(NULL)}
  }
}


#' Undo the accepted suggested relabelling.  
#'
#' @description 
#' 'undofunc_relabAC' takes a list element (torestore) and the list of global variables (gVars) stored within the app to undo the accepting of the suggested relabelling.   
#' @param torestore A named list element. The name ('relab_AC') identifies the type of operation performed to undo (accepted relabellign suggestion). 'torestore' is a nested one-element list, where the two inner lists  are respectively the suggested and accepted label, and the original dataset label substituted.   
#' @param gVars The list of global variables stored by the app.
#' @return  The updated list of global variables as consequence of undoing the accepted-relabelling operation described in 'torestore'.
#' @keywords internal
#' @export

undofunc_relabAC <- function (torestore, gVars) { 
gVars <- gVars
  tmp <- torestore[["relab_AC"]]
  df <- gVars$phTable[[1]] #actual df
  setnames(df, tmp[[1]], tmp[[2]])
  
  #delivering message to user & update of values
  msg <- paste("Undoing relabelling of", tmp[[2]],"as", tmp[[1]])
  shinyjs::info(msg)
  gVars$undo_message <- msg
  gVars$phTable[[1]] <-df
  gVars$processedL <-  gVars$processedL[gVars$processedL != tmp[[2]]]
  gVars$tmpLa  <- gVars$processedL
  out <- list(gVars)
  out
}


#' Undo the rejected suggested relabelling.  
#'
#' @description 
#' 'undofunc_relabRJ' takes a list element (torestore) and the list of global variables (gVars) stored within the app to undo the rejection of the suggested relabelling.   
#' @param torestore A named list element. The name ('relab_RJ') identifies the type of operation performed to undo (rejected relabelling suggestion). 'torestore' is a nested one-element list, where the two inner lists  are respectively the suggested and rejected label, and the original dataset label.   
#' @param gVars The list of global variables stored by the app.
#' @return  The updated list of global variables as consequence of undoing the rejected relabelling operation described in 'torestore'.
#' @keywords internal
#' @export

undofunc_relabRJ <- function (torestore, gVars) {
  gVars <- gVars
  #torestore: list(chosen_match)
  tmp <- torestore[["relab_RJ"]]
  #delivering message to user & update of values
  msg <- paste("Undoing the rejection of", tmp[[2]], "as potential column to rename")
  shinyjs::info(msg)
  gVars$undo_message <- msg
  gVars$processedL <-  gVars$processedL[gVars$processedL != tmp[[2]]]
  gVars$tmpLr <- gVars$processedL 
  out <- list(gVars)
  out
}


#' Undo the deletion of the duplicated columns.  
#'
#' @description 
#' 'undofunc_duplAC' takes a list element (torestore) and the list of global variables (gVars) stored within the app to undo the deletion of the duplicated columns. 
#' @param torestore A named list element. The name ('dupl_AC') identifies the type of operation performed to undo (deletion of duplicated columns). 'torestore' is a nested one-element list, where the two inner lists  are respectively the data.frame column(s) of the kept column(s), and all the labels of the duplicated columns.   
#' @param gVars The list of global variables stored by the app.
#' @return  The updated list of global variables as consequence of undoing the duplicated column deletion operation described in 'torestore'.
#' @keywords internal
#' @export

undofunc_duplAC <- function (torestore,gVars) {
  gVars <- gVars
  #torestore: list(list(kept col, c(all duplicates with the included)))
  tmp <- torestore[["dupl_AC"]]
  kept <- tmp[[1]]  
  df_dupl <- tmp[[2]]
  deleted <- colnames(df_dupl)[colnames(df_dupl)!= kept]
  
  gVars$phTable[[1]] <- cbind(gVars$phTable[[1]],df_dupl[deleted])  #to restore previous step df
  
 if (is_empty(deleted) & length(kept)>1) {
   tmp_msg <- "All duplicates were kept, so even if the operation is undone, they are still present in the dataset."
   gVars$processedD <-  gVars$processedD[gVars$processedD != kept]
 } else {
   tmp_msg <- paste0("Undoing the deletion of the following duplicates: ", toString(deleted),".")
   gVars$processedD <-  gVars$processedD[gVars$processedD != deleted]
 }
  
  
  #delivering message to user & update of values
  shinyjs::info(tmp_msg)
  gVars$undo_message <- tmp_msg
  
  out <- list(gVars)
  out
}


#' Undo the recoding of the pair column label/content.  
#'
#' @description 
#' 'undofunc_recodeAC' takes a list element (torestore) and the list of global variables (gVars) stored within the app to undo the recoding of the pair column label/content.  
#' @param torestore A named list element. The name ('recode_AC') identifies the type of operation performed to undo (recoding of the column label/content pair). 'torestore' is a nested one-element list, where the inner lists  is a dataframe row made of 5 fields (new recoded label, old original label, new recoded content, old original content, username).   
#' @param gVars The list of global variables stored by the app.
#' @return  a list with respectively (i) the updated list of global variables as consequence of undoing the recoding of the column label/content pair describen in 'torestore' and (ii) a label vector to update the dropdown menu with the column labels still to process as potential choices.
#' @keywords internal
#' @export

undofunc_recodeAC <- function (torestore, gVars) {
  gVars <- gVars
  #torestore: list(list(cmodification)), such as list(new_lab;old_lab; new_cont;old_cont))new_colname));flag=T to restore also the kept column to original state
  tmp <- torestore[["recode_AC"]]
  req(nrow(tmp[[1]])>0)
  op_row <- as.data.frame(ifelse(nrow(tmp[[1]])==1, tmp,list(tail(tmp[[1]],1)) )  )
  current_df <- gVars$phTable[[1]]
  
  names(current_df) [names(current_df) == op_row$newlab] <- op_row$oldlab
  current_df[[op_row$oldlab]][current_df[[op_row$oldlab]] == op_row$newcont] <- op_row$oldcont
  gVars$phTable[[1]]<- current_df
  
  #delivering message to user & update of values
  msg <- paste0("Undoing the recoding of column ", op_row$newlab, " and the content ", op_row$newcont, ". The values were restores to original, respectively ", op_row$oldlab ," and ", op_row$oldcont, ".")
  
  shinyjs::info(msg)
  gVars$undo_message <- msg
  
  
  gVars$list_cols_tmp <-  gVars$list_cols_tmp %>% .[. != op_row$newlab] %>% c(., op_row$oldlab) 
  gVars$processedLIST2REC <-  gVars$processedLIST2REC[gVars$processedLIST2REC != op_row$newlab]
  gVars$processed_cols_RECOD <- !gVars$processed_cols_RECOD %in% op_row$newlab
  target_el <- gVars$processedLIST_content2RECODE$get(op_row$newcont, NULL)
  pasted_new_old_lab <- paste(op_row$newlab, op_row$oldlab,sep="_*_" )
  if(pasted_new_old_lab %in% target_el){
    if (length(target_el) == 1) { gVars$processedLIST_content2RECODE$remove(op_row$newcont, silent=FALSE)
    } else { gVars$processedLIST_content2RECODE$set(op_row$newcont, target_el[target_el != pasted_new_old_lab]) 
        }
  }
    
  #new list of choices to update updateSelectizeInput "pick_cols_recoding"
  t <- gVars$list_cols_tmp [!gVars$list_cols_tmp %in% gVars$processedLIST2REC] %>% .[!. %in%gVars$processed_cols_RECOD] # %>% .[!. %in% gVars$deleted_colname] 
  
  #remove last row of the intermediate vocabulary
  gVars$later_voc_upd_df <- head(gVars$later_voc_upd_df, -1)
  out <- list(gVars,t)
  out
} #chius recode func


#' Undo the skipping of the full recoding of the whole column.  
#'
#' @description 
#' 'undofunc_recodeRJ' takes a list element (torestore) and the list of global variables (gVars) stored within the app to undo the  skipping of the full recoding of the whole column.  
#' @param torestore A named list element. The name ('recode_RJ') identifies the type of operation performed to undo (recoding of the column label and the skip of the recoding of the column content). 'torestore' is a nested one-element list, where the inner lists  are (i) a vector of 5 fields (new recoded label, old original label, NA, NA, and username) and (ii) a vector with the contents that will be stored in the temporary intermediate vocabulary structure. .   
#' @param gVars The list of global variables stored by the app.
#' @return  a list with respectively (i) the updated list of global variables as consequence of undoing the skipping of the full recoding of the whole column described in 'torestore' and (ii) a label vector to update the dropdown menu with the column labels still to process as potential choices.
#' @keywords internal
#' @export

undofunc_recodeRJ <- function (torestore,gVars) {
  gVars <- gVars
  #torestore: list(list(tmodification)), such as list(new_lab;old_lab; NA;NA)
  tmp <- torestore[["recode_RJ"]]
  contents_in_later_voc <- tmp[[2]]
  newcolname <- tmp[[1]][1]
  oldcolname <- tmp[[1]][2]
  names(gVars$phTable[[1]]) [names(gVars$phTable[[1]]) == newcolname] <- oldcolname
  
  #delivering message to user & update of values
  msg <- paste0("Undoing the rejection of the recoded content of column ", newcolname, ". The column label is restored to ", oldcolname ,".")
  shinyjs::info(msg)
  gVars$undo_message <- msg
  
  gVars$list_cols_tmp <-  gVars$list_cols_tmp %>% .[. != newcolname] %>% c(., oldcolname)
  gVars$processedLIST2REC <-  gVars$processedLIST2REC[gVars$processedLIST2REC != newcolname]
  
  #new list of choices to update updateSelectizeInput "pick_cols_recoding"
  t1 <- gVars$list_cols_tmp [!gVars$list_cols_tmp %in% gVars$processedLIST2REC] %>% .[!. %in% gVars$deleted_colname] %>%
    .[!. %in%gVars$processed_cols_RECOD]
  
  #remove last row of the intermediate vocabulary
  last_stored_label <- tail(gVars$later_voc_upd_df,1)$mainL
  candidates_df <- gVars$later_voc_upd_df %>% .[!(.$mainL %in% last_stored_label & .$mainC %in% contents_in_later_voc) ,]
  gVars$later_voc_upd_df <- candidates_df
  out <- list(gVars,t1)
  out
} #chius recode func


#' Undo the deletion of a column.  
#'
#' @description 
#' 'undofunc_delete' takes a list element (torestore) and the list of global variables (gVars) stored within the app to undo the deletion of a column.  
#' @param torestore A named list element. The name ('deleted') identifies the type of operation performed to undo (deletion of a column). 'torestore' is a nested one-element list, where the inner lists  are (i) the original label of the deleted column and (ii) the deleted column.   
#' @param gVars The list of global variables stored by the app.
#' @return  a list with respectively (i) the updated list of global variables as consequence of undoing the deletion of the column described in 'torestore' and (ii) a label vector to update the dropdown menu with the column labels still to process as potential choices.
#' @keywords internal
#' @export

undofunc_delete <- function (torestore, gVars) {
  gVars <- gVars
  tmpcolname <- torestore[["deleted"]][[1]]
  deletion_setup <- gVars$delete_op_list[[which(map(gVars$delete_op_list,1)%in% tmpcolname )]]
  tmp_recovered_col <-  deletion_setup[[2]]
  
  current_df <- gVars$phTable[[1]]
  df <- cbind(current_df, tmp_recovered_col) 
  
  #delivering message to user & update of values
  msg <- paste0("Undoing deletion of ", tmpcolname ,". The column content is restored to the state at the time of its deletion.")
  shinyjs::info(msg)
  gVars$undo_message <- msg
  gVars$phTable[[1]] <-df 
  
  gVars$deleted_colname <-  gVars$deleted_colname[gVars$deleted_colname != c(tmpcolname)]
  #new list of choices to update updateSelectizeInput "pick_cols_recoding"
  tdel <- c(gVars$list_cols_tmp, tmpcolname)
  
  out <- list(gVars,tdel)
  out
}  


#' Undo the splitting of a column.  
#'
#' @description 
#' 'undofunc_specials_ALLsplits' takes a list element (torestore) and the list of global variables (gVars) stored within the app to undo the splitting of a column.
#' In the app, there are three ways to split a column (based on Agilent standard, by means of a separator or a regular expression).  
#' @param torestore A named list element. The names ('special_agiSPL', 'special_SPL', 'special_SPLREG') identify the type of the performed splitting to undo. 'torestore' is a nested one-element list, where the inner lists  are (i) the original label of the column to split and (ii) the label of the columns obtained thought the splitting.   
#' @param gVars The list of global variables stored by the app.
#' @return  a list with respectively (i) the updated list of global variables as consequence of undoing the column splitting described in 'torestore' and (ii) a label vector to update the dropdown menu with the column labels still to process as potential choices.
#' @keywords internal
#' @export

undofunc_specials_ALLsplits <- function (torestore, gVars) {
  #torestore[["special_agiSPL"]], [[1]]: processed colname, [[2]]: newly splitted column names
  tmp <- torestore[[1]] 
  old_whole_colname <- tmp[[1]][1]
  new_splitted_colname <- tmp[[2]]
  current_df <- gVars$phTable[[1]]
  if (!old_whole_colname %in% colnames(current_df)){
    original_in_old_df <- gVars$original_phTable[[1]]
    current_df <- cbind(current_df,original_in_old_df)
  }
  #remove newly splitted cols
  current_df[new_splitted_colname]<-NULL
  new_splitted_string <- toString(new_splitted_colname)
  
  #delivering message to user & update of values
  msg <- paste0("Undoing the splitting of column '", old_whole_colname, "'. The obtained resulting columns (", new_splitted_string, ") were deleted.")
  shinyjs::info(msg)
  gVars$undo_message <- msg
  gVars$phTable[[1]] <- current_df
  
  #new list of choices to update updateSelectizeInput "pick_cols_recoding"
  tminus_splitted <- gVars$list_cols_tmp <- gVars$list_cols_tmp[!gVars$list_cols_tmp %in% new_splitted_colname]
  
  out <- list(gVars,tminus_splitted)
  out
} 


#' Undo the addition of empty column(s).  
#'
#' @description 
#' 'undofunc_specials_addEmpty' takes a list element (torestore) and the list of global variables (gVars) stored within the app to undo the addition of empty column(s).
#' @param torestore A named list element. The name ('special_addEMPTY') identifies the type of operation performed to undo (addition of empty column(s)). 'torestore' is a nested one-element list, where the inner list contains the label name(s) of the newly added empty column(s).   
#' @param gVars The list of global variables stored by the app.
#' @return  a list with respectively (i) the updated list of global variables as consequence of undoing the deletion of the column described in 'torestore' and (ii) a label vector to update the dropdown menu with the column labels still to process as potential choices.
#' @keywords internal
#' @export

undofunc_specials_addEmpty <- function (torestore, gVars) {
  #torestore: list(list(added col colnames)) [[1]]: names of the newly added columns 
  tmp <- torestore[["special_addEMPTY"]]
  new_added_colname <- tmp[[1]]
  current_df <- gVars$phTable[[1]]
  #remove newly added cols
  if (any(new_added_colname %in% colnames(current_df))){
    toremove <- new_added_colname[new_added_colname %in% colnames(current_df)]
    current_df <- current_df[, !names(current_df) %in% toremove]
  }
  
  #delivering message to user & update of values
  msg <- paste0("Removing the added empty column(s) named as'", toString(toremove), "'.")
  shinyjs::info(msg)
  gVars$undo_message <- msg
  
  gVars$phTable[[1]] <- current_df
  #new list of choices to update updateSelectizeInput "pick_cols_recoding"
  tminus_added <- gVars$list_cols_tmp <- gVars$list_cols_tmp[!gVars$list_cols_tmp %in% new_added_colname]
  
  out <- list(gVars,tminus_added)
  out
}


#' Undo the classification of the entries (issue/accept) during multiple dataset integration.  
#'
#' @description 
#' 'undofunc_multi' takes a list element (torestore) and the list of global variables (gVars) stored within the app to undo the classification of the entries present in multiple dataset during their integration.
#' Multiple curated datasets are automatically integrated together through merging. Different column labels and contents must be evaluated by the user and can be classified as an 'issue' (to revise later) or as 'accept' (if it is consistent with the reference vocabulary entries).     
#' @param torestore A named list element. The name ('multi') identifies the type of operation performed to undo (classification of the entries during multiple dataset integration. 'torestore' is a nested one-element list, where the inner lists are (i) the column selected, made of its label and unique contents; (ii) the analysis group the column evaluated belongs to ('Safe', 'Slow check', 'Fast check'); (iii) how the entry was classified ('issue' or 'accept').
#' @param gVars The list of global variables stored by the app.
#' @return  a list with respectively (i) the updated list of global variables as consequence of undoing the classification of the entries present in multiple dataset integration as described in 'torestore' and (ii) a logical indicator that when is TRUE updates the dropdown menu presenting the analysis categories grouping the different columns ('Safe', 'Slow check', 'Fast check').
#' @keywords internal
#' @export

undofunc_multi <- function (torestore, gVars) {
  gVars <- gVars
  #torestore: list(list(column_torestore, group from the seelcted column belong to, how it was classified))
  tmp <- torestore[["multi"]]
  column_torestore <- data.frame(tmp[[1]])
  group_type <- tmp[[2]]  #("Safe", "Fast check", "Slow check")
  decision <- tmp[[3]]    #accept, issue 
  to_update <- FALSE
  
  if (group_type %in% "Slow check"){ #undo_elem
    tmptable<- gVars$multi_slow_check()
    tmptable <- cbind(tmptable, column_torestore)
    gVars$multi_slow_check(tmptable)
    if(!"Slow check" %in% gVars$grouping_multi_tables && dim(gVars$multi_slow_check())[2] != 0) { 
      gVars$grouping_multi_tables <- c(gVars$grouping_multi_tables, "Slow check") %>% .[order(match(., c("Safe", "Fast check", "Slow check")))]
      to_update <-TRUE} 
  } else if (group_type %in% "Fast check")  {  
    tmptable<- gVars$multi_fast_check()
    tmptable <- cbind(tmptable, column_torestore)
    gVars$multi_fast_check(tmptable)
    if(!"Fast check" %in% gVars$grouping_multi_tables && dim(gVars$multi_fast_check())[2] != 0) {
      gVars$grouping_multi_tables <- c(gVars$grouping_multi_tables, "Fast check") %>% .[order(match(., c("Safe", "Fast check", "Slow check")))]
      to_update <-TRUE
    }
  } else   {     #Safe
    tmptable<- gVars$multi_safe_check()
    tmptable <- cbind(tmptable, column_torestore)
    gVars$multi_safe_check(tmptable) 
    if(!"Safe" %in% gVars$grouping_multi_tables && dim(gVars$multi_safe_check())[2] != 0) {
      gVars$grouping_multi_tables <- c("Safe", gVars$grouping_multi_tables) %>% .[order(match(., c("Safe", "Fast check", "Slow check")))]
      to_update <-TRUE} 
  } 
  
  #remove of the column name to undo from the list of processed and classified columns; 
  #if "issue" is selected, it removes also the number of issues to re-evaluate
  if(decision %in% "accept") {
    gVars$multi_accept_colnames <- gVars$multi_accept_colnames[gVars$multi_accept_colnames != colnames(column_torestore)]
    gVars$multi_ACmsg[length(gVars$multi_ACmsg)]  <- NULL
  } else {
    gVars$multi_issue_colnames <- gVars$multi_issue_colnames[gVars$multi_issue_colnames != colnames(column_torestore)]
    gVars$save_issue_blocks_list[length(gVars$save_issue_blocks_list )]  <- NULL
    gVars$multi_ISSmsg[length(gVars$multi_ISSmsg)] <- NULL
  }
  
  #delivering message to user & update of values
  msg <- paste0("Undoing the classification of the integrated multiple dataframe column: ", colnames(column_torestore),".")
  
  shinyjs::info(msg)
  gVars$undo_message <- msg
  out <- list(gVars, to_update)
  out
}
