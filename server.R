options(java.parameters = c("-XX:+UseConcMarkSweepGC", "-Xmx8192m"))
srcDir <- dirname(getSrcDirectory(function(x){x})) #Get source directory

library(magrittr)
library(dplyr)
library(drc)
library(data.table)
library(zeallot)
library(collections)
library(reshape2)
library(tibble)
library(shinyWidgets)
library(stringr)
library(readxl)
library(DT)
library(xlsx)
library(tidyverse)

library(shinycssloaders)
library(neo4jshell)    #uksesp

#library(plotly)
library(shiny)
library(shinyjs)
library(shinyBS)
library(shinyalert)


#Get source directory
source("functions/read_excel_allsheets.R")
source("functions/column_label_finder.R")
source("functions/duplicate_finder.R")
source("functions/column_toRecode.R")
source("functions/single_col_recode_finder.R")
source("functions/colorwords.R")
source("functions/add_to_dict.R")
source("functions/convert_df_to_dict.R")
source("functions/convert_dict_to_df.R")
source("functions/enrich_dict.R")
source("functions/label_shortener.R")
source("functions/find_and_update_previous_actionsLSTrelab_AC.R")
source("functions/find_and_update_previous_actionsLSTrelab_REJ.R")
source("functions/find_and_update_dupl_groups.R")
source("functions/find_original_column.R")
source("functions/find_and_update_previous_actionsLSTrecode_RJ.R")
source("functions/find_and_update_previous_actionsDFrecode_AC.R")
#source("functions/collecter_del_related_opsRELAB_AC.R")
source("functions/word_versions_tracker.R")
source("functions/del_ops_collecter.R")
source("functions/restore_deletion_ops.R")
source("functions/update_ops_list.R")
source("functions/update_ops_df.R")

source("functions/find_and_update_modifications_forlast5.R")
source("functions/find_and_update_modifications_relab.R")
source("functions/find_and_update_modifications_dupl.R")




# this instruction increase the maximum size of the file that can be uploaded in a shiny application
options(shiny.maxRequestSize=300*1024^2) 

celTable <- NULL
phTable <- NULL
extractedList <- NULL

gxTable <- NULL
gxCorMat <- NULL
tmpMethod <- NULL



#Function to hide bsCollapsePanel
hideBSCollapsePanel <- function(session, panel.name)
{
  session$sendCustomMessage(type='hideBSCollapsePanelMessage', message=list(name=panel.name))
}


#Function to get names of columns with datatype character
factorize_cols <- function(phTable, idx){
  for(i in idx){
    phTable[,i] <- factor(phTable[,i])
  }
  return(phTable)
}



enableBookmarking(store="url")

server<-(function(input, output, session) {

  # Global variable list
  gVars <- shiny::reactiveValues( 
    phTable=NULL,             # setting phenodata matrix
    original_phTable = NULL,  # setting original unmodified phenodata matrix
    working_status=NULL,     #setting the working mode,F for multiple dataframe, T for singles
    GLP_status=NULL,         #setting the abilitation of GLP mode, F if GLP active, T otherwise
    current_df_file_name=NULL, #Stores the name of the file currently processed
    dict_modifLAB=collections::dict(NULL),
    dict_modifCONT=collections::dict(NULL),
    label_first_version=collections::dict(NULL),   #in case of undoing recoding involving the original column names, stores those values before undo 
    dict_relab=collections::dict(NULL),
    UKS_status=FALSE,
    username=NULL,         # current user name
    all_curators=NULL,     #list of all users who had a role during curation
    
    phTab_progress=NULL,
    sampleColID = FALSE,      # setting sample column number
   # doseColID = NULL,         # setting dose column number
    TPColID=NULL,             # setting TP column number
    inputGx=NULL,             # setting gene expression matrix
    compact_voc=NULL,         # storing compact version to display of the vocabulary
    dkeys=NULL,               # setting a more accessible vocabulary
    vocabulary_entries=NULL,  # stores all entries of the vocabulary
    always=NULL,              # stores the columns always present in every dataset
    label=NULL,
    processedL=NULL,               # setting the colnames renamed or rejected in relation to an allowed label
    tmpLr = NULL,             #stores a temporary modified copy of the colnames  rejected in relation to an allowed label
    tmpLa = NULL,             #stores a temporary modified copy of the colnames renamed  in relation to an allowed label
    tmpdfL=NULL,       #stores temporary modified df with colnames renamed 
    relab_op_list=NULL,       # setting the list to contain all operations related to relabelling
    relabelling_ops_dataframe=NULL, #sotres all the steps of the relabelling phase as datframe
    dupl_decisions=NULL,     #stores the decision about duplicates, pos=1 the kept, following positions with deleted col names
    dupl_op_list=NULL,       # setting the list to contain all operations related to duplicate removal
    dupl_ops_dataframe=NULL, #sotres all the steps of the duplicate removal phase as datframe
    
    
    glp_comment_rep =NULL, 
    
    processedD=NULL,        #setting the potential column duplicates
    content_NA = "NA", #stores NA as string
    tmpD = NULL,             #stores a temporary modified copy without duplicated columns of the dataset
    selected_cols=NULL, 
    potential_choices=NULL,     #stores names of identical columns among which the user should chose
    all_identical_cols=NULL, #stores all the different groups of identical columns
    deleted_colname = NULL,  #stores the name of the column completely deleted
    delete_op_list =NULL,
    dupl_op_list=NULL,       # setting the list to contain all operations related to duplicate deletion
    tmp_pt_value =NULL,        #temporarly sotres the pt text 
    
    recod_op_list=NULL,       # setting the list to contain all operations related to duplicate removal
    recod_ops_dataframe=NULL, #sotres all the steps of the duplicate removal phase as datframe
    subset_df_recode_ops=NULL,   #stores the recoding ops of the chosen type (recoded, or rejected recoding of the content)
    recodeRJ_op_list=NULL, # stores all the column that were relabelled but whose content recoding was fully rejected
    dict_DELrelated_ops=collections::dict(NULL), #stores the recodedAC operations related to deleted columns
    originalcontent=NULL, #stores the original content value during recoding
    
    processedR= NULL,      #storing the name of the columns recoded   
    clean_recoding_pairs=NULL, #stroes temporary the column content to remove from recoding pair options
    map_recoding= NULL,    #stores the content and its recoded version 
    map_for_plot=NULL,   #stores values for plotting in the recoding section
    processedLIST_content2RECODE = NULL,  #storing the proposals for content recoding for each column   
    processedLIST2REC=NULL,
    modification_synthesis =NULL,
    tmp_stored_df=NULL,           #tmp storage for the current df with modifications
    #toStore = data.frame(t(rep(NA,4))),
    #stored_for_adding=NULL,
    #upd_vocabulary = NULL,
    last5list = NULL,
    undo_message=NULL ,    #message to store in report after undoing some ops
    action_to_undo=NULL,   #stores the action to undo in relabelling in total undo section
    list_cols_tmp=NULL,
    recap_dupl = NULL,     #stores the recap (dupl kept, dupl deleted, dupl renamed) for every selected action in undo section
    pt_glp_list =list(),    #stores any pt/glp information for each step
    orig_lab=NULL, #stores label for vocabolary section
    orig_cont=NULL, #stroes content for vocabulary section 
    later_voc_upd_df=NULL, #stores the new items to check again before adding definitively to the vocabulary
    later_voc_upd_row_tmp=NULL, #stores temporarily the last op
    safe=NULL,                 #sub-group of the safe prestored new words to check (are the already present in the vocabulary)
    outsider=NULL,          #sub-group of the fast check" prestored new words to check (are both allowed words completely new )
    enrichments =NULL,    #sub-group of prestored new words to check with a bit more attentions (different combinations of synonyms and allowed)
    dt_temporary_cases_voc=NULL, #temporary store f the selected pre-stored (safe/outsider/enrichemnts)temporary cases for the vocabulary update
    empty_prestored_voc_tables=c("Safe","Fast check","To overview"), #stores which group safe/check/overview was cpompleted processed -no more terms inside 
    voc_issues_report=list(), #stores the suggested vocabulary entries and their type of storing as report inc ase something is wrong and requires a second step curation
    voc_updating_report=list(), #stores the suggested vocabulary entries that are saved in a new updated version of the vocabulary
    recap_elaborated_voc_rows=NULL, # stores the processed rows of the going-to-be vocabualry entries, to restore properly the session if needed
    selected_row_index=NULL, #stores the row index of the selected row processed to evaluate entries for acceptance in the vocabulary
    voc_glp_comments_list=NULL,   #stores the comment for each operation taken in processing the entries from the curation for acceptance in the vocabulary   #REMOVE
    flag_REMOVE_WIDGETS= NULL,
    separatorChar=NULL, #stores the type of separator
    sel_col_split=NULL, #stores the selected column to split
    dt_split=NULL,   #stores the newly splitted columns
    special_op_list=NULL,    #setting the list to contain all operations related to special operations
    
    agi_splitted_new_cols=NULL, #stores the columns created by splitting AGILENT filneame.txt string
    AGIsplit_msg=NULL,      #stores the message for pt procedural track in agilent
    addEmpty_msg=NULL,      #stores the message for pt procedural track in adding empty columns
    
    multidf_list=NULL, #sores a list containing  multiple datsets
    multidf=NULL, #stores the join of multiple dataframes
    grouping_multi_tables=c("Safe","Fast check","To overview"),    #entries for a dropdown
    multi_safe=NULL,      # once retrieved from joining the multiple dataframes, it stores entries where colname and column content are in the vocabulary 
    multi_fast_check=NULL,      # once retrieved from joining the multiple dataframes, it stores entries of columns of the "always" category
    multi_slow_check=NULL,      # once retrieved from joining the multiple dataframes, it stores entries where one or more entries are not present in the vocabulary 
    multi_ACmsg=NULL,               #stores messages of multidf merging accept
    multi_accept_colnames=NULL,    #stores accepted colnames from multidf merging
    multi_ISSmsg=NULL,               #stores messages of multidf issue report
    multi_issue_colnames=NULL,    #stores issue colnames from multidf merging
    multi_col_decisions=NULL,    #stores the decision above each column 
    col_issue_cases=NULL,       #stores the entries of the selected column that make the column an "issue" needing a second round of curation
    multi_flag=NULL,             #stores the type of operation performed to allocate the GLP comment in the right list
    
    
    plot_tmp=NULL, #stores values for plot data
    
 
  )
  
  gVars$sepChoices <- c("Tab", ",", ";", "Space","", "Other")
  gVars$quoteChoices <- c(NA, "SINGLE", "DOUBLE")
  gVars$pvAdjChoices <- c("Holm"="holm", "Hochberg"="hochberg", "Hommel"="hommel", "Bonferroni"="bonferroni", "Benjamini & Hochberg"="BH", "Benjamini & Yekutieli"="BY", "False Detection Rate"="fdr", "None"="none")
  gVars$normChoices <- c("Between Arrays"="BA", "Quantile"="quantile", "Variance Stabilizing"="vsn", "Cyclic Loess"="cl")
  gVars$baChoices <- c("None"="none", "Scale"="scale", "Quantile"="quantile", "Cyclic Loess"="cyclicloess")
  
  gVars$AllAvailableModels =  mNames_all = c("LL.2","LL.3","LL.3u","LL.4","LL.5",
                                             "W1.2","W1.3","W1.4","W2.2","W2.3","W2.4",
                                             "BC.4","BC.5",
                                             "LL2.2","LL2.3","LL2.4","LL2.5",
                                             "AR.2","AR.3",
                                             "MM.2","MM.3","Linear", "Quadratic", "Cubic",
                                             "Power2","Power3","Power4","Exponential",
                                             "Hill05","Hill1","Hill2","Hill3","Hill4","Hill5")
  
  
  
  
  
   

 
  gVars$flag_REMOVE_WIDGETS <- TRUE
  
  #session starting: user identifier name
  shinyWidgets::inputSweetAlert(
    session = session,
    inputId = "enter_username",
    input="text",
    title = "Identification",
    btn_labels = "Save",   
    btn_colors = "#0392cf",
    text = "User, please identify",
    type = "info",
    allowOutsideClick = FALSE,
    placeholder = "User identification",
    inputValidator =I(
      "function(value){
        if (value=='') {
          return 'You need to identify for the curation report!';
        }
      }"
    )
  )

    
  
  observeEvent(input$enter_username,{
    req(!is.null(input$enter_username))
    req(nzchar(input$enter_username))  
   
    #stores username
    if (!is.null(gVars$username)){
      gVars$all_curators <- c(gVars$all_curators, input$enter_username)
    }
    gVars$username <- input$enter_username
    #new/load old session
    shinyWidgets::confirmSweetAlert(
      session = session,
      inputId = "session_type",
      title = "Operating session",
      btn_labels = c("New","Load"),   #F/T
      btn_colors = c("#0392cf","#0392cf"),
      text = "Would you like to run a new curation or restore a previous state?",
      type = "info"
    )
  })
  
  
  
  observeEvent(input$session_type,{
  
  if(input$session_type == FALSE){ #new session
      gVars$pt_glp_list <- c(gVars$pt_glp_list, paste("USER - Current session was carried over by:",input$enter_username))
      #mode selection choice by selecting single/multiple(s) phenodata
      shinyWidgets::confirmSweetAlert(
        session = session,
        inputId = "working_mode",
        title = "Type of phenodata",
        btn_labels = c("Multiple","Single"),   #F/T
        btn_colors = c("#0392cf","#0392cf"),
        text = "What kind of phenodata will you process in this session?",
        type = "info"
      )
      shinyBS::updateCollapse(session, "bsSidebar1", open="LOAD PHENODATA")
  } else {
     showModal( restoresession())
       
   }
  })  
  
  
  #alternative trigger for loading stored session  
  observeEvent(input$dorestore_session,{
    showModal( restoresession())
    shinyBS::updateCollapse(session, "bsSidebar0_5", close="SESSION MANAGEMENT")
    
  })
  
  
  
  
  restoresession <- function() {
    modalDialog(
      tags$head(tags$style(
        type = 'text/css',
     " .modal {
        text-align: center;
        padding: 0!important;
      }
      .modal:before {
        content: '';
        display: inline-block;
        height: 100%;
        vertical-align: middle;
        margin-right: -4px;
      }
      .modal-dialog {
        display: inline-block;
        text-align: left;
        vertical-align: middle;
      }"
     )), 
     title= HTML("Restore previous session"),
      fluidRow(column(5, 
                      tags$head(tags$style(HTML(".bigger_label label {font-size:11pt;}"))),
                      div( 
                        fileInput("load_sessionfile", label=("Load session file"),multiple=FALSE),class="bigger_label") 
      )),
      br(),
      fluidRow(
        column(1, align="left",shinyBS::bsButton("import_session", label="Load", style="info", icon=icon("hand-o-right")))
      ),
      footer =  modalButton("Cancel")
      
    )
  }
  
    
  
  
 observeEvent(input$working_mode, {
   req(!is.null(input$working_mode))
   gVars$working_status <- input$working_mode
   if(input$working_mode) {   #T such as Single mode
     shinyBS::updateButton(session, "import_pheno_submit", label="Import Single PhenoData", icon=icon("hand-point-right"),disabled = FALSE)
     shinyBS::updateButton(session, "relabelling_block_button", disabled = FALSE)
     shinyBS::updateButton(session, "dupl_removal_block_button", disabled = FALSE)
     shinyBS::updateButton(session, "recoding_button", disabled = FALSE)
     gVars$pt_glp_list <- c(gVars$pt_glp_list, "MODE - Current session takes SINGLE dataset as input.")
     
  } else { #F, such as multiple mode
     shinyBS::updateButton(session, "import_pheno_submit", label="Import Multiple PhenoData", icon=icon("hand-point-right"), disabled = FALSE)
     shinyBS::updateButton(session, "relabelling_block_button", disabled=TRUE)
     shinyBS::updateButton(session, "dupl_removal_block_button", disabled=TRUE)
     shinyBS::updateButton(session, "recoding_button", disabled=TRUE)
     gVars$pt_glp_list <- c(gVars$pt_glp_list, "MODE - Current session takes MULTIPLE datasets as input.")
     
  }
    
   })
  
  
 output$disabledTab <- renderText({"<b><i><big><big>This tab is not available in the current working mode</big></big></b></i>"}) 
 output$disabledTab_multi <- renderText({"<b><i><big><big>This tab is not available in the current working mode</big></big></b></i>"}) 
 
  
  
 
 #save current session
 observeEvent(input$dosave_session, {
   req(input$dosave_session)
   #arrange new file name
   current_df_file_name <- gVars$current_df_file_name %>% 
                                     sub('.[^.]*$', '', .) %>%
                                          paste(., collapse = "_")
   
   newfile = paste0(format(Sys.time(), "%Y-%m-%d_%H-%M-%S"), "_",
                    current_df_file_name, "_current_session.RData")
     
   #glp/pt comment
   gVars$pt_glp_list <- c(gVars$pt_glp_list, paste("SAVED the current session: as",newfile))
   save_session <- gVars

   save(save_session, file=newfile)#file = file_path)
   Save_done <- showNotification(paste("Message:", "The session has been saved"), duration = NULL)
   
   shinyBS::updateCollapse(session, "bsSidebar0_5", close="SESSION MANAGEMENT")
 })
 
 
 #Load data saved session
 observeEvent(input$import_session, {
   req(!is.null(input$load_sessionfile))
   
   load(input$load_sessionfile$name)
   
   previous_session_gVars <<- save_session
   #overwriting of gVars
   lapply(names(previous_session_gVars), function(k) {
     gVars[[k]] <- previous_session_gVars[[k]]
   })  
   Load_done <- showNotification(paste("Message:", "The session has been restored"), duration = NULL)
  
   
   # fix sidebar buttons
   if(!gVars$GLP_status) { 
     shinyBS::updateButton(session, "GLP_inactivation_mode", label= " GLP Mode Enabled",  style="success", icon=icon("check-circle"))#, disabled = !input$GLP_inactivation_mode)
     shinyBS::updateCollapse(session, "bsSidebar1", close="STRUCTURE HOMOGENIZATION", style=list("GLP MODE"="success"))
   }
   if(!is.null(gVars$phTable[[1]]) |!is.null(gVars$multidf) ){
     shinyBS::updateButton(session, "import_pheno_submit", style="success", icon=icon("check-circle"))
     shinyBS::updateCollapse(session, "bsSidebar1", close="LOAD VOCABULARY", style=list("LOAD PHENODATA"="success"))
   }
   
   if(!is.null(gVars$dkeys)){   #vocab
     shinyBS::updateButton(session, "import_expr_submit", style="success", icon=icon("check-circle"))
     shinyBS::updateCollapse(session, "bsSidebar1", close="GLP MODE", style=list("LOAD VOCABULARY"="success"))
   }
   
   if(!gVars$working_status) {   #F such as Multiple mode
     shinyBS::updateButton(session, "relabelling_block_button", disabled = TRUE)
     shinyBS::updateButton(session, "dupl_removal_block_button", disabled = TRUE)
     shinyBS::updateButton(session, "recoding_button", disabled = TRUE)
    
   }
    #updates the choices in multiple section
   updateSelectizeInput( 
     session,
     inputId="multijoin_outcomes",
     choices=gVars$grouping_multi_tables,
     selected = character(0),
     options = list(
       placeholder = 'Please select an option below',
       onInitialize = I('function() { this.setValue(""); }')),
     server=TRUE)
   
   
   
  #creates a condition for showing/hide some of the widgets in single/multiple phenodata scenario
   output$working_mode_scenario <- reactive(gVars$working_status)
   outputOptions(output, "working_mode_scenario", suspendWhenHidden=FALSE)
   
   #new comment to pt/GLP
   timing <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")
   gVars$pt_glp_list <- c(gVars$pt_glp_list, paste("RESTORED the previous session at ",timing))
   gVars$pt_glp_list <- c(gVars$pt_glp_list, paste("USER - From now, the restored session is carried over by:",input$enter_username))
   removeModal()
   
   
 })
 
 
 
 # launch modal to reset the app to starting point
 observeEvent(input$launch_reset_modal,{
   showModal( reset_session_modal())
   shinyBS::updateCollapse(session, "bsSidebar0_5", close="SESSION MANAGEMENT")
 })
 
 
 
 reset_session_modal <- function() {
   modalDialog(
     tags$head(tags$style(
       type = 'text/css',
       " .modal {
        text-align: center;
        padding: 0!important;
      }
      .modal:before {
        content: '';
        display: inline-block;
        height: 100%;
        vertical-align: middle;
        margin-right: -4px;
      }
      .modal-dialog {
        display: inline-block;
        text-align: left;
        vertical-align: middle;
      }"
     )), 
     title= HTML("Reset current session"),
     fluidRow(column(12, align="center",
                     h4(tags$b("Are you sure you want to reset the current session? \n")) ,h5("Previous progress will be lost.")
     )),
     fluidRow(
       column(12, align="center",shinyBS::bsButton("reset_session", label="Reset", style="danger", icon=icon("hand-o-right")))
     ),
     footer =  modalButton("Cancel")
     
   )
 }
 
 
 observeEvent(input$reset_session,{
   #reset gVars to NULL
   lapply(names(gVars), function(k) {
     gVars[[k]] <- NULL
     gVars$dict_DELrelated_ops = collections::dict(NULL)
     gVars$dict_modifLAB =collections::dict(NULL)
     gVars$dict_modifCONT = collections::dict(NULL)
     
     gVars$pt_glp_list =list()
     gVars$voc_updating_report =list()
     gVars$voc_issues_report = list()
     
     empty_prestored_voc_tables=c("Safe","Fast check","To overview")
   })
   
   
   # fix sidebar buttons
     shinyBS::updateButton(session, "GLP_inactivation_mode", label= " GLP Mode Disabled",  style="danger", icon=icon("check-circle"))#, disabled = !input$GLP_inactivation_mode)
     shinyBS::updateCollapse(session, "bsSidebar1", close="STRUCTURE HOMOGENIZATION", style=list("GLP MODE"="warning"))
   
     shinyBS::updateButton(session, "import_pheno_submit", style="danger", icon=icon("check-circle"))
     shinyBS::updateCollapse(session, "bsSidebar1", close="LOAD VOCABULARY", style=list("LOAD PHENODATA"="warning"))
   
     shinyBS::updateButton(session, "import_expr_submit", style="danger", icon=icon("check-circle"))
     shinyBS::updateCollapse(session, "bsSidebar1", close="GLP MODE", style=list("LOAD VOCABULARY"="warning"))
   
   removeModal()
   
  Sys.sleep(0.5)
  #session starting: user identifier name
  shinyWidgets::inputSweetAlert(
    session = session,
    inputId = "enter_username",
    input="text",
    title = "Identification",
    btn_labels = "Save",   
    btn_colors = "#0392cf",
    text = "User, please identify",
    type = "info",
    placeholder = "User identification",
    inputValidator =I(
      "function(value){
        if (value=='') {
          return 'You need to identify for the curation report!';
        }
      }"
    )
  )
 })
 

 
 
 
 
 
  
 
 
 
 
 
  
  
  observeEvent(input$upload_voc_submit, {
    gxFile <- input$gx
    if (is.null(gxFile))
      return(NULL)
    
    gx = read_excel_allsheets(filename = gxFile$datapath,tibble = FALSE)  
    
    
    gVars$inputGx <- gx
    # compact version of the vocabulary to display
    gVars$compact_voc <- gx[[1]]
    
    # loading of allowed labels and related synonyms.
     dict_keys <- data.frame(gx[[1]]) %>% 
                     separate_rows(., lab_syn, sep=",") %>% 
                     separate_rows(., syn_features, sep=",") %>% .[order(.$label),]
     gVars$dkeys <- dict_keys
    
    
    shinyBS::toggleModal(session, "importGxModal", toggle="close")
    shinyBS::updateButton(session, "import_expr_submit", style="success", icon=icon("check-circle"))
    shinyBS::updateCollapse(session, "bsSidebar1", open="GLP MODE", style=list("LOAD VOCABULARY"="success","STRUCTURE HOMOGENIZATION"="warning"))
    shiny::updateTabsetPanel(session, "display",selected = "gExpTab")
  })

  

  observeEvent(input$GLP_inactivation_mode, ({
    
    if (!input$GLP_inactivation_mode){     #GLP mode active
      if(length(gVars$pt_glp_list)!=0){   #if pt contains already something
        shinyjs::info("The activation of GLP mode will delete operation listed in the procedural track.")
        gVars$pt_glp_list <- list()             #ensure emptiness of procedural track (if previously some steps were taken)
        #  return(NULL)
      }
      shinyBS::updateButton(session, "GLP_inactivation_mode", label= " GLP Mode Enabled",  style="success", icon=icon("check-circle"))#, disabled = !input$GLP_inactivation_mode)
      shinyBS::updateCollapse(session, "bsSidebar1", open="STRUCTURE HOMOGENIZATION", style=list("GLP MODE"="success","STRUCTURE HOMOGENIZATION"="warning"))
      gVars$GLP_status <- FALSE
    }
    else {
      shinyBS::updateButton(session, "GLP_inactivation_mode", label=" GLP Mode Disabled",  style="danger", icon=icon("exclamation-circle"))
      shinyBS::updateCollapse(session, "bsSidebar1", close="STRUCTURE HOMOGENIZATION", style=list("GLP MODE"="warning","STRUCTURE HOMOGENIZATION"="warning"))
      gVars$GLP_status <- TRUE
    }    
    
  }))
  
  
  
  gVars$gxTable <- shiny::reactive({
    if (is.null(gVars$inputGx) || is.null(gVars$dgxTable))
      return(NULL)
    
    gx <- gVars$inputGx
    gx <- gx[rownames(gVars$dgxTable),]
    return(gx)
  })
  
  gVars$inputDgx <- eventReactive(input$load_dgx_submit, {
    if(is.null(input$dgx))
      return(NULL)
    
    dgxFile <- input$dgx
    sepS <- input$sepS
    sepT <- input$sepT
    sepChar=NULL
    if(sepS=="OTHER"){
      sepChar <- sepT
    }else{
      if(sepS=="TAB"){
        sepChar="\t"
      }else if(sepS=="SPACE"){
        sepChar=" "
      }else{
        sepChar=sepS
      }
    }
    
    quote <- input$quote
    if(is.na(quote) || quote=="NA"){
      quote <- ""
    }else if(quote=="SINGLE"){
      quote <- "'"
    }else if(quote=="DOUBLE"){
      quote <- '"'
    }
    
    rowNames <- NULL
    con <- file(dgxFile$datapath, "r", blocking=FALSE)
    fileLines <- readLines(con)
    fileLines <- gsub("\t\t", "\tNA\t", fileLines)
    fileLines <- gsub("\t$", "\tNA", fileLines)
    close(con)
    colNumByRowDist <- table(sapply(fileLines, function(x) {length(strsplit(x, sepChar)[[1]])}, USE.NAMES=FALSE))
    if(length(colNumByRowDist) > 1){
      fileLines2 <- fileLines[-1]
      colNumByRowDist2 <- table(sapply(fileLines2, function(x) {length(strsplit(x, sepChar)[[1]])}, USE.NAMES=FALSE))
      if(length(colNumByRowDist2) > 1){
        shinyjs::info(paste0("Separating character '", sepChar, "', results in inconsistent number of columns!\n\nPlease check the input file format and select the correct separating character!"))
        gVars$dgxLoaded <- NULL
        return(NULL)
      }
      rowNames <- 1
    }
    
    gVars$dgxLoaded <- 1
    gVars$dgxFileName <- dgxFile$name
    dgxTable <- read.csv(dgxFile$datapath,header=TRUE, sep=sepChar, stringsAsFactors=FALSE, quote=quote, as.is=TRUE, strip.white=TRUE, check.names=FALSE)
    
    return(dgxTable)
  })
  
  gVars$dgxColChoices <- reactive({
    if(is.null(gVars$dgxLoaded))
      return(c("NA"))
    
    choicesVec <- seq(1,ncol(gVars$inputDgx()))
    choicesNames <- paste0("Column ", choicesVec)
    names(choicesVec) <- choicesNames
    return(choicesVec)
  })
  
  output$dgxDT <- DT::renderDataTable({
    if(is.null(gVars$inputDgx))
      return(NULL)
    
    dgxTable <- gVars$inputDgx()
    colnames(dgxTable) <- paste0(colnames(dgxTable), " [", c(1:ncol(dgxTable)), "]")
    DT::datatable(dgxTable, filter="none", 
                  options = list(
                    ch = list(regex=TRUE, caseInsensitive=FALSE), 
                    scrollX=TRUE, 
                    pageLength=2,
                    lengthMenu=c(1,2,3),
                    ordering=FALSE
                  )
    )
  },server=TRUE)
  
  
  #stores an old version original of the dataframe
  observeEvent(input$load_pheno_submit, {
    if(is.null(input$fPheno))
      return(NULL)
    
    phFile <- input$fPheno
    gVars$current_df_file_name <- input$fPheno$name
    phTable = read_excel_allsheets(filename = phFile$datapath,tibble = FALSE)
    
    names(phTable[[1]]) <- gsub(x = names(phTable[[1]]), pattern = "\\:", replacement = "\\.") #####
    
    gVars$original_phTable = phTable
    
    })
  
  
  #upload an excell file with N sheets such as the number of experiments that we are considering
  
  gVars$inputPh <- eventReactive( input$load_pheno_submit, {
    if(is.null(input$fPheno))
        return(NULL)
    
    phFile <- input$fPheno
    phTable = read_excel_allsheets(filename = phFile$datapath,tibble = FALSE)
    
    names(phTable[[1]]) <- gsub(x = names(phTable[[1]]), pattern = "\\:", replacement = "\\.") #####
    
     phTable = phTable
    
      gVars$phLoaded <- 1
    
      return(phTable)
    })
  
#  gVars$inputPh <- reactiveValues()
  
  #  observeEvent(input$load_pheno_submit, {
  #        if(is.null(input$fPheno))
  #        return(NULL)
    
  #      phFile <- input$fPheno
  #    phTable = read_excel_allsheets(filename = phFile$datapath,tibble = FALSE)
    
  #      names(phTable[[1]]) <- gsub(x = names(phTable[[1]]), pattern = "\\:", replacement = "\\.") #####
    
  #     phTable = phTable
    
  #        gVars$phLoaded <- 1
    
  #      gVars$inputPh = phTable
  #   })
  
  
  
  
  gVars$phColTypes <- reactive({
    if(is.null(gVars$inputPh()))
      return(NULL)
    
    phTable <- gVars$inputPh()[[1]]
    colNames <- list("unch"=NULL,"usu"=NULL)
    
    #phTable <- gVars$inputPh()[[1]]
    #colNames <- list("c"=NULL,"n"=NULL)
    #coltypes <- unlist(lapply(phTable, class))
    #coltypes.charOnly.idx <- which(coltypes=="character")
    #coltypes.nonChar.idx <- which(!coltypes=="character")
    #coltypes.charOnly.len <- length(coltypes.charOnly.idx)
    #coltypes.nonChar.len <- length(coltypes.nonChar.idx)
    #if(coltypes.charOnly.len>0){
    #  colNames[["c"]] <- colnames(phTable)[coltypes.charOnly.idx]
    #}
    #if(coltypes.nonChar.len>0){
    #  colNames[["n"]] <- colnames(phTable)[coltypes.nonChar.idx]
    #}
    
    always <- scan("./files/always.txt", character(), quote = "", skip=1)
    gVars$always <- always
    #alw2<-source("files/always.txt", character(), quote = "", skip=1)
    
    colnamesdf <- colnames(phTable)
    colnamesdf_always <- which(colnamesdf %in% always)
    colnamesdf_unchecked <- which(!colnamesdf %in% always)
    
    colNames[["unch"]] <- colnames(phTable)[colnamesdf_unchecked]
    colNames[["usu"]] <- colnames(phTable)[colnamesdf_always]
    
    return(colNames)
  })
  
  
  
  output$phRowsText <- renderText({
    req(!is.null(gVars$inputPh))
    if(is.null(gVars$inputPh())){
      nRow <- "NA"
    }else{
      nRow <- nrow(gVars$inputPh()[[1]])
    }
    return(paste0("Samples: ", nRow))
  })
  
  output$phColsText <- renderText({
    req(!is.null(gVars$inputPh))
    if(is.null(gVars$inputPh())){
      nCol <- "NA"
    }else{
      nCol <- ncol(gVars$inputPh()[[1]])
    }
    return(paste0("Variables: ", nCol))
  })
  
  
  gVars$phColChoices <- reactive({
    if(is.null(gVars$phLoaded))
      return(c("NA"))
    phenoList = gVars$inputPh()
    choicesVec <- seq(1,ncol(phenoList[[1]]))
    choicesNames <- paste0("Variable ", choicesVec)
    names(choicesVec) <- choicesNames
    return(choicesVec)
  })
  
  output$phenoDT <- DT::renderDataTable({
    # if(is.null(gVars$phTable))
    #   return(NULL)
    
    
    phenoList = gVars$inputPh()
    phTable <- phenoList[[1]]
    colnames(phTable) <- paste0(colnames(phTable), " [", c(1:ncol(phTable)), "]")
    DT::datatable(phTable, filter="none",
                  options = list(
                    ch = list(regex=TRUE, caseInsensitive=FALSE),
                    scrollX=TRUE,
                    pageLength=2,
                    lengthMenu=c(1,2,3),
                    ordering=F
                  )
    )
  },server=TRUE)
  
  output$phenoTypesRH <- rhandsontable::renderRHandsontable({
    shiny::validate(
      need(!is.null(gVars$inputPh()), "No phenodata file!")
    )
    phenoList = gVars$inputPh()
    phTable <- phenoList[[1]]
    
    colNames <- paste0(colnames(phTable), " [", c(1:ncol(phTable)), "]")
    colTypesList <- gVars$phColTypes()
    colClass <- unlist(lapply(phTable, class))
    
    colTypesDF <- data.frame(Variable=colNames, Type="-", Class=colClass, t(head(phTable)), stringsAsFactors=FALSE)
    colnames(colTypesDF)[c(4:ncol(colTypesDF))] <- paste0("Sample", c(1:(ncol(colTypesDF)-3)))
    if(!is.null(colTypesList[["unch"]])){
      cIdx <- which(colnames(phTable) %in% colTypesList[["unch"]])
      if(length(cIdx>0)){
        colTypesDF[cIdx,"Type"] <- "unchecked"
      }
    }
    if(!is.null(colTypesList[["usu"]])){
      nIdx <- which(colnames(phTable) %in% colTypesList[["usu"]])
      if(length(nIdx>0)){
        colTypesDF[nIdx,"Type"] <- "common"  
      }
    }
    rhandsontable::rhandsontable(colTypesDF, rowHeaders=NULL, readOnly=TRUE, contextMenu=FALSE) %>%
      hot_col("Type", readOnly=FALSE, type="dropdown", source=c("unchecked","common"),
              renderer = "function (instance, td, row, col, prop, value, cellProperties) {
              Handsontable.renderers.NumericRenderer.apply(this, arguments);
              if (value == 'unchecked') {
              td.style.background = 'Salmon';
              } else if (value == 'common') {
              td.style.background = 'lightgreen';
              }
              }"
      )
  })
  
  
  ############ STRUCTURE HOMOGENIZATION section
  output$pt_text <- renderText({"<b>Procedural Track</b>"})
  output$pt_label <- renderText({"<i><small>Waiting for action!</small></i>"})
  
  ##################### Re-labelling part
  # fnCOL_LAB_FINDER:stores results of FuNction column_label_finder
  data <- reactiveValues(fnCOL_LAB_FINDER = NULL)
  
  #it elaborates phenodata table to couple proper label from the vocabulary by using allowed label and related label synonyms
  output$pick_col_labelling <- renderUI({
    shiny::validate(
      need(!is.null(gVars$phTable), "No Phenodata File Provided!")
    )
    
    shiny::validate(
      need(!is.null(gVars$inputGx), "No Vocabulary File Provided!")
    )
    
    df= gVars$phTable[[1]]
    dkeys = gVars$dkeys
    procesL <- gVars$processedL
    fnCOL_LAB_FINDER = column_label_finder(dkeys,df)
    data$fnCOL_LAB_FINDER = data.frame(fnCOL_LAB_FINDER)
    
    
    a <- data$fnCOL_LAB_FINDER$label
    b <- data$fnCOL_LAB_FINDER$direction
    c <- data$fnCOL_LAB_FINDER$match
    tmp <- data.frame(a,b,c) %>% .[!.$c %in% procesL,] %>%
      rowwise() %>% 
      mutate(dropdownText=HTML(paste0("<span class='col1'>", a, 
                                      "</span><span class='col2'>", b,  
                                      "</span><span class='col3'>",c, "</span>"))) %>%  ungroup()
    
    
    # .bs-select-all{display=none;}
    
    pickerInput(
      inputId = "pick_col_labelling",
      label = "Select the column(s) of the dataset you wish to display (Vocabulary label -> Dataset column name):",
      choices = tmp$c,
      choicesOpt=list(content=tmp$dropdownText),
      options = list(`actions-box` = TRUE,
                     `selected-text-format` = paste0("count > ", length(colnames(data$fnCOL_LAB_FINDER)) - 1),
                     `count-selected-text` = "All pairs",
                     liveSearch = TRUE,
                     liveSearchPlaceholder = TRUE),   # build buttons for collective selection
      multiple = TRUE)
  })
  
  
  
  
  
  # as consequence of selecting a specific pair, it shows the column of the dataset of the selected pair (label,colname) and identified by colname
  output$content1 <- renderDataTable({
    table1 <- gVars$phTable[[1]]        
    table1<-data.frame(table1[,req(input$pick_col_labelling)])
    colnames(table1) <-input$pick_col_labelling
    table1
  })
  
  
  
  # as consequence of selecting a specific pair, it shows the correspondent label in the upper left box
  observeEvent(input$pick_col_labelling,{
    temp <- (data$fnCOL_LAB_FINDER)
    lab <-temp$label[temp$match %in% input$pick_col_labelling]
    matchCN <- temp$match[temp$match %in% input$pick_col_labelling]
    gVars$label <- lab
    gVars$match <- matchCN
    #gVars$processedL <- c(gVars$processedL,matchCN)
    output$candidate_text <- renderText({"Label candidate"})
    output$candidate_label <- renderText({lab})
    output$pt_label <- renderText({"..."})
    
    #stores the current candidate label
    data$current_col_candidate_label = lab
  })
  
  
  
  
  #button reject in relabelling section  
  observeEvent(input$reject_lab_candidate,{
    shiny::validate(
      need(!is.null(gVars$phTable), "No Phenodata File Provided!")
    )
    
    shiny::validate(
      need(!is.null(gVars$inputGx), "No Vocabulary File Provided!")
    )
    
    req(input$pick_col_labelling)
    # Only when one colname is selected, it deletes the picked colname (chosen_match) from the picked pairs table obtained (fnCOL_LAB_FINDER)
    chosen_match <- gVars$match 
    chosen_lab <- gVars$label  #kept only for pt update
    
    len <- length(chosen_match)
    if(len>1){
      shinyjs::info("The labels you are going to reject are more than one!\n\nPlease run this step once per label!")
      return(NULL)
    }
    else{
      #storing stage for undostep
      relab_RJ <- list(chosen_lab,chosen_match)
      gVars$relab_op_list <- c(gVars$relab_op_list, (list(relab_RJ =relab_RJ)))
      gVars$last5list <- c(gVars$last5list, (list(relab_RJ =relab_RJ)))
      
      
      #perform rejection action
      #gVars$processedL <- c(gVars$processedL,chosen_match)
      gVars$tmpLr <- chosen_match
      temp <- data$fnCOL_LAB_FINDER
      #    data$fnCOL_LAB_FINDER <- temp[!temp$match %in% chosen_match,]  
    }
    
    #cleaning of text box
    # output$candidate_label <- renderText({" "})
    #update buttons
    shinyBS::updateButton(session, "reject_lab_candidate", style="success", icon=icon("hand-point-right"))
    #enable("glp_pt_button_relab")
    disable("accept_lab_candidate")
    disable("reject_lab_candidate")
    disable("undo_relab_button")
    enable("glp_pt_button_relab")
    disable("pick_col_labelling")
    
    
    #updates the PT box with what was rejected
    Value <- paste0("'",chosen_lab, "' is rejected as relabelling candidate for '",chosen_match,"'.")
    gVars$pt_glp_list <- c(gVars$pt_glp_list, Value)
    #updateTextAreaInput(session, inputId="PT_relab_input", "Procedural track", value = Value)
    output$pt_label <- renderText({Value})
  })
  
  
  #by pressing add/glp button, it reupdates all the buttons and perform the relabelling/or not
  observeEvent(input$glp_pt_button_relab, {
    
    
    
    
    if(!input$GLP_inactivation_mode){    #such as GLPmode activated
      showModal(dataModal())
      disable("undo_relab_button")
      
      
      
      
      
      
      
      #output$pt_label <- renderText({"<i><small>Waiting for action!</small></i>"})
      #output$candidate_label <- renderText({" "})
    } else { # only procedural track report
      # re-update buttons 
      shinyBS::updateButton(session, "accept_lab_candidate", style="info", icon=icon("hand-point-right"))
      shinyBS::updateButton(session, "reject_lab_candidate", style="info", icon=icon("hand-point-right"))
      enable("accept_lab_candidate")
      enable("reject_lab_candidate")
      enable("undo_relab_button")
      enable("pick_col_labelling")
      
      
      #re-update boxes
      #updateTextAreaInput(session, inputId="PT_relab_input", "Procedural track", value = "...")
      #cleaning of text box with processed column name And procedural track
      output$pt_label <- renderText({"<i><small>Waiting for action!</small></i>"})
      output$candidate_label <- renderText({" "})
      #it effectively performs the acceptance of  relabelling(tmpLa) or the rejection (tmpLr)
      gVars$processedL <- unique(c(gVars$processedL,gVars$tmpLr ))
      
      if (req(input$accept_lab_candidate)){
        gVars$processedL <- c(gVars$processedL,gVars$tmpLa) 
        gVars$phTable[[1]] <- gVars$tmpdfL
      }
    }
    
  }) 
  
  
  
  
  
  # Return a modal dialog window with the possibility to input data. If 'failed' is TRUE (no written justifications in the inputbox), the user is informed about it.
  dataModal <- function(failed = FALSE) {
    modalDialog(
      textAreaInput(inputId = "glp_comments_box", label = "GLP comments", width = "100%", rows=4,
                    placeholder = 'Please justify your last operation.'),
      shinyBS::bsButton("glp_store_comment", label="Add", style="info", icon=icon("hand-point-right")),
      
      if (failed){
        div(br(),tags$b("In GLP mode, you must justify your decisions. Please do."), style = "color: red;")},
      
     
      
      
      footer =  modalButton("Cancel")
      
    )
  }
  
  
  
  # When Add button(glp_store_comment) is pressed, it tests if the textareainput box is not empty. 
  # If successful, remove the modal, otherwise it shows another modal with a failure message.
  observeEvent(input$glp_store_comment, {
    # Check that box for glp comments is filled
    if (input$glp_comments_box != "") {
      tabulated_comment <- paste0("\t","COMMENT:",input$glp_comments_box)
      list_elem <- list(tabulated_comment)
      gVars$pt_glp_list <- c(gVars$pt_glp_list, list_elem)
      removeModal()
      
      
    } else {
      showModal(dataModal(failed = TRUE))
    }
    
  })
  
  
  
  #update of all buttons and perform the cleaning if needed of data boxes i.e. the selected column
  observeEvent(input$glp_store_comment, {
    req(input$glp_comments_box != "")
    
    
    if(req(input$pick_col_labelling!="")){ #&& data$current_col_candidate_label != " ")){  
      # re-update buttons 
      shinyBS::updateButton(session, "accept_lab_candidate", style="info", icon=icon("hand-point-right"))
      shinyBS::updateButton(session, "reject_lab_candidate", style="info", icon=icon("hand-point-right"))
      enable("accept_lab_candidate")
      enable("reject_lab_candidate")
      enable("undo_relab_button")
      disable("glp_pt_button_relab")
      
      #re-update boxes
      #updateTextAreaInput(session, inputId="PT_relab_input", "Procedural track", value = "...")
      #cleaning of text box with processed column name And procedural track
      output$pt_label <- renderText({"<i><small>Waiting for action!</small></i>"})
      output$candidate_label <- renderText({" "})
      #it effectively performs the acceptance of  relabelling(tmpLa) or the rejection (tmpLr)
      gVars$processedL <- unique(c(gVars$processedL,gVars$tmpLr ))
      
      if (req(input$accept_lab_candidate)){
        gVars$processedL <- c(gVars$processedL,gVars$tmpLa) 
        gVars$phTable[[1]] <- gVars$tmpdfL
      }
      
    }
    
    
    
    
    
  })
  
  
  
  
  
  
  
  
  # button accept in relabelling section
  observeEvent(input$accept_lab_candidate,{
    shiny::validate(
      need(!is.null(gVars$phTable), "No Phenodata File Provided!")
    )
    
    shiny::validate(
      need(!is.null(gVars$inputGx), "No Vocabulary File Provided!")
    )
    
    shiny::validate(
      need(!is.null(gVars$label), "No Label Selected!")
    )
    
    
    req(input$pick_col_labelling)
    #substitute the colname (chosen_match) with the chosen label (chosen_lab) only if the selected colname is one at the time
    df <- gVars$phTable[[1]]
    names_col <- colnames(df)
    chosen_lab <- gVars$label
    chosen_match <- gVars$match
    
    
    
    len <- length(chosen_match)
    if(len>1){
      shinyjs::info("The labels you are going to substitute are more than one!\n\nPlease run this step once per label!")
      return(NULL)
    }
    else{
      gVars$tmpLa <- c(chosen_match,chosen_lab)    
      
      #storing for undostep
      relab_AC <- list( chosen_lab,chosen_match) 
      if(chosen_match %in% colnames(gVars$original_phTable[[1]])){
          gVars$dict_relab$set(chosen_lab, chosen_match)}
      gVars$relab_op_list <- c(gVars$relab_op_list, (list(relab_AC=relab_AC)))
      gVars$last5list <- c(gVars$last5list, (list(relab_AC=relab_AC)))
      }
    
    df_tmp<-df
    setnames(df_tmp, chosen_match, chosen_lab)
    gVars$tmpdfL <- df_tmp
    #gVars$phTable[[1]] <- gVars$tmpdfL
    
    #output$candidate_label <- renderText({" "})
    #updates the different buttons
    shinyBS::updateButton(session, "accept_lab_candidate", style="success", icon=icon("hand-point-right"))
    disable("accept_lab_candidate")
    disable("reject_lab_candidate")
    disable("undo_relab_button")
    enable("glp_pt_button_relab")
    disable("pick_col_labelling")
    
    
    #updates the PT box with what was done and accepted
    # if (is.null(relab_op_list)){}
    Value <- paste0("'",chosen_lab, "' relabels '",chosen_match,"'.")
    gVars$pt_glp_list <- c(gVars$pt_glp_list, Value)
    #updateTextAreaInput(session, inputId="PT_relab_input", "Procedural track", value = Value)
    output$pt_label <- renderText({Value})
  })
  
  
  
  
  
  #activate/deactivate GLP/add button according GLP mode abilitation 
  observe({
    if (!input$GLP_inactivation_mode) {    #GLP enabled
      gVars$pt_glp_list <- NULL             #ensure emptiness of procedural track (if previously some steps were taken)
      shinyBS::updateButton(session, "glp_pt_button_relab", label=" GLP", style="info", icon=icon("hand-point-right"))
      shinyBS::addTooltip(session,"glp_pt_button_relab", "Launch a graphical window, to enrich the procedural track info with comments according to GLP regulations.", placement="bottom")
      # shinyjs::show("glp_button_relab")
      #  shinyjs::hide("add_pt_relab")
    } else {
      shinyBS::updateButton(session, "glp_pt_button_relab", label=" Add", style="info", icon=icon("hand-point-right"))
      shinyBS::removeTooltip(session,"glp_pt_button_relab")
      #  shinyjs::show("add_pt_relab")
      #  shinyjs::hide("glp_button_relab")
    }
  })
  
  
  
  
  
  
  
  # undo buttons  
  observeEvent(input$undo_relab_button | input$undo_dupl_button | input$undo_del_button | input$undo_save_button |  
                 input$undo_agil_button | input$undo_norm_regex_split_button | input$undo_addEmpty  | input$undo_multi_button,{     
    req(!is.na(gVars$pt_glp_list))#stores undo
    #  gVars$pt_glp_list <- c(gVars$pt_glp_list, paste("UNDO:", gVars$undo_message))
    if (!is.null(gVars$last5list)){      
      if (!is_empty(gVars$last5list)){ 
        torestore <- tail (gVars$last5list,1)[1]
        #when action is undone, the last element of the list (storing that action) is deleted
        gVars$last5list = gVars$last5list[0:(length(gVars$last5list)-1)]
        undofunction(torestore)
        gVars$pt_glp_list <- c(gVars$pt_glp_list, paste("UNDO:", gVars$undo_message))
        
        name_op <-names(torestore)
        if(name_op %in% c("relab_AC","relab_RJ")) {gVars$relab_op_list[[ which(gVars$relab_op_list %in% torestore)]] <-NULL}
        else if(name_op %in% "dupl_AC") {gVars$dupl_op_list[[ which(gVars$dupl_op_list %in% torestore)]]<-NULL}
        else if(name_op %in% "deleted"){gVars$delete_op_list[[ which(gVars$delete_op_list %in% torestore)]] <-NULL}
        else if(name_op %in% "recode_RJ"){gVars$recodeRJ_op_list[[ which(gVars$recodeRJ_op_list %in% torestore)]] <-NULL}
        else if(name_op %in% "recode_AC"){gVars$recod_ops_dataframe <- head(gVars$recod_ops_dataframe, -1)}
        else if(name_op %in% c("special_agiSPL","special_SPL","special_SPLREG")){gVars$special_op_list[[ which(gVars$special_op_list %in% torestore)]] <-NULL}
        else if(name_op %in% "special_addEMPTY"){gVars$special_op_list[[ which(gVars$special_op_list %in% torestore)]] <-NULL}
        else {return(NULL)}
        #else if
        # gVars$dupl_op_list[[ which(gVars$dupl_op_list %in% torestore)]]<-NULL}
        #  else if(} special_op_list (ma con special_SPL e special_SPLREG
        
        #if GLPmode activated, stores the message and open the GLPcomment modal window
        req(!input$GLP_inactivation_mode)
        showModal(dataModal(failed = FALSE))
      }
      else { shinyjs::info("It is not possible to undo more operations") }
    }
    
    
    
    
  })
  
  
  undofunction <- function (torestore){
    
    if (!is.null(torestore)){
      op_names<- names(torestore)
      if (op_names == "relab_AC") {undofunc_relabAC(torestore)  }
      else if (op_names == "relab_RJ") {undofunc_relabRJ(torestore)  }
      else if (op_names == "dupl_AC") {undofunc_duplAC(torestore)}
      else if (op_names == "deleted") {undofunc_delete(torestore)}
      else if (op_names == "recode_AC") {undofunc_recodeAC(torestore)}
      else if (op_names == "recode_RJ") {undofunc_recodeRJ(torestore)}
      else if( op_names %in% c("special_agiSPL","special_SPL","special_SPLREG")) {undofunc_specials_ALLsplits(torestore)}                       #aggiungi anche split normale e con regex
      else if (op_names == "special_addEMPTY") {undofunc_specials_addEmpty(torestore)}
      else if (op_names == "multi") {undofunc_multi(torestore)}
      else {return(NULL)}
    }
  }
  
  
  
  observeEvent(input$undo_del_button | input$undo_save_button ,{
    gVars$flag_REMOVE_WIDGETS <- FALSE
  })
  
  
  undofunc_relabAC <- function (torestore) {
    #torestore: list(chosen_lab, chosen_match)
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
  }
  
  
  undofunc_relabRJ <- function (torestore) {
    #torestore: list(chosen_match)
    tmp <- torestore[["relab_RJ"]]
    #delivering message to user & update of values
    msg <- paste("Undoing the rejection of", tmp[[2]], "as potential column to rename")
    shinyjs::info(msg)
    gVars$undo_message <- msg
    gVars$processedL <-  gVars$processedL[gVars$processedL != tmp[[2]]]
    gVars$tmpLr <- gVars$processedL 
  }
  
  
  undofunc_duplAC <- function (torestore,flag=FALSE) {
    #torestore: list(list(kept col, c(all duplicates with the included)));flag=T to restore also the kept column to original state
    tmp <- torestore[["dupl_AC"]]
    kept <- tmp[[1]]  
    df_dupl <- dplyr::select(gVars$original_phTable[[1]],tmp[[2]])
    deleted <- colnames(df_dupl)[colnames(df_dupl)!= kept]
  
    if(flag == FALSE){
      gVars$phTable[[1]] <- cbind(gVars$phTable[[1]],df_dupl[deleted])  #to restore previous step df
    } else {   #flag=T- wish to restore column to its primitive state
      gVars$phTable[[1]] <- gVars$phTable[[1]] %>% .[,!names(.) %in% kept] %>% cbind(., df_dupl) 
      msg_appendix <- paste("Column", kept, "was fully restored to original state.")
    
      }
    
    
    #delivering message to user & update of values
    tmp_msg <- paste0("Undoing the deletion of the following duplicates: ", toString(deleted),".")
    msg <- ifelse(flag==FALSE, 
                  tmp_msg , paste(tmp_msg,"\n",msg_appendix)
                        )
    shinyjs::info(msg)
    gVars$undo_message <- msg
    gVars$processedD <-  gVars$processedD[gVars$processedD != deleted]
    
    
    
    
  }
  
  
  undofunc_recodeAC <- function (torestore,flag=FALSE) {
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
    ###msg se voglio resotre a colonna di fabbrica, ma devo anche triggerare il search dei valori nelle ops.
    shinyjs::info(msg)
    gVars$undo_message <- msg
    
    
    gVars$list_cols_tmp <-  gVars$list_cols_tmp %>% .[. != op_row$newlab] %>% c(., op_row$oldlab) 
    gVars$processedLIST2REC <-  gVars$processedLIST2REC[gVars$processedLIST2REC != op_row$newlab]
    gVars$processed_cols_RECOD <- !gVars$processed_cols_RECOD %in% op_row$newlab
    
    t <- gVars$list_cols_tmp [!gVars$list_cols_tmp %in% gVars$processedLIST2REC] %>% .[!. %in%gVars$processed_cols_RECOD] # %>% .[!. %in% gVars$deleted_colname] 
    updateSelectizeInput(session,
                         inputId = "pick_cols_recoding",
                         #label = "Select the column to recode you wish to display:",
                         choices = t,
                         selected = character(0),
                         server=TRUE)
    
    #remove last row of the intermediate vocabulary
    gVars$later_voc_upd_df <- head(gVars$later_voc_upd_df, -1)
    
  } #chius recode func
  
  
  
  undofunc_recodeRJ <- function (torestore,flag=FALSE) {
    #torestore: list(list(tmodification)), such as list(new_lab;old_lab; NA;NA)
    tmp <- torestore[["recode_RJ"]]
    newcolname <- tmp[[1]][1]
    oldcolname <- tmp[[1]][2]
    names(gVars$phTable[[1]]) [names(gVars$phTable[[1]]) == newcolname] <- oldcolname
    
    #delivering message to user & update of values
    msg <- paste0("Undoing the rejection of the recoded content of column ", newcolname, ". The column label is restored to ", oldcolname ,".")
    shinyjs::info(msg)
    gVars$undo_message <- msg
    
    gVars$list_cols_tmp <-  gVars$list_cols_tmp %>% .[. != newcolname] %>% c(., oldcolname)
    gVars$processedLIST2REC <-  gVars$processedLIST2REC[gVars$processedLIST2REC != newcolname]
    t1 <- gVars$list_cols_tmp [!gVars$list_cols_tmp %in% gVars$processedLIST2REC] %>% .[!. %in% gVars$deleted_colname] %>%
      .[!. %in%gVars$processed_cols_RECOD]
    updateSelectizeInput(session,
                         inputId = "pick_cols_recoding",
                         #label = "Select the column to recode you wish to display:",
                         choices = t1,
                         selected = 2,
                         server=TRUE)
  
    
    #remove last row of the intermediate vocabulary
    gVars$later_voc_upd_df <- head(gVars$later_voc_upd_df, -1)
      
  } #chius recode func
  
  
  undofunc_delete <- function (torestore) {
    tmpcolname <- torestore[["deleted"]][[1]]
    deletion_setup <- gVars$delete_op_list[[which(map(gVars$delete_op_list,1)%in% tmpcolname )]]
    tmp_recovered_col <-  deletion_setup[[2]]
    
    current_df <- gVars$phTable[[1]]
    df <- cbind(current_df, tmp_recovered_col)#cbind(gVars$original_phTable[[1]], orig_column_df)
    
    #delivering message to user & update of values
    msg <- paste0("Undoing deletion of ", tmpcolname ,". The column content is restored to the state at the time of its deletion.")
    shinyjs::info(msg)
    gVars$undo_message <- msg
    gVars$phTable[[1]] <-df 
    
    
    
    
    
    #toremove <- ifelse (!identical(tmpcolname,tmp_recovered_col) )
    gVars$deleted_colname <-  gVars$deleted_colname[gVars$deleted_colname != c(tmpcolname)]
  } #chius undo_delete
  
  
  
  
  undofunc_specials_ALLsplits <- function (torestore) {
    browser()
    tmp <- torestore[[1]] #torestore[["special_agiSPL"]]
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
    
  } 
  
  
  
  undofunc_specials_addEmpty <- function (torestore) {
    #torestore: list(list(added col colnames))
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
  }
  
  
  
  undofunc_multi <- function (torestore) {
    #torestore: tmptable<- gVars$multi_slow_check()
    tmp <- torestore[["multi"]]
    column_torestore <- tmp[[1]]
    group_type <- tmp[[2]]  #("Safe", "Fast check", "To overview")
    decision <- tmp[[3]]    #accept, issue
    
    
    if (group_type %in% "To overview"){ #undo_elem
      tmptable<- gVars$multi_slow_check()
      tmptable <- cbind(tmptable, column_torestore)
      gVars$multi_slow_check(tmptable)
    } else if (group_type %in% "Fast check")  {  
      tmptable<- gVars$multi_fast_check()
      tmptable <- cbind(tmptable, column_torestore)
      gVars$multi_fast_check(tmptable)
    } else   {     #Safe
      tmptable<- gVars$multi_safe_check()
      tmptable <- cbind(tmptable, column_torestore)
      gVars$multi_safe_check(tmptable)
    } 
     
    #remove of the column name to undo from the list of processed and classified columns
    if(decision %in% "accept") {
      gVars$multi_accept_colnames <- gVars$multi_accept_colnames[gVars$multi_accept_colnames != colnames(column_torestore)]
    } else {
      gVars$multi_issue_colnames <- gVars$multi_issue_colnames[gVars$multi_issue_colnames != colnames(column_torestore)]
    }
    
    #delivering message to user & update of values
    msg <- paste0("Undoing the classification of the integrated multiple dataframe column: ", colnames(column_torestore),".")
    
    shinyjs::info(msg)
    gVars$undo_message <- msg
    
    
    
    
  }
  
  
  
  
  
  # activate/deactivate GLP/add button according GLP mode abilitation
  observe({
    if (!input$GLP_inactivation_mode) {    #GLP enabled
      shinyBS::updateButton(session, "glp_pt_button_dupl", label=" GLP", style="info", icon=icon("hand-point-right"))
      shinyBS::addTooltip(session,"glp_pt_button_dupl", "Launch a graphical window, to enrich the procedural track info with comments according to GLP regulations.", placement="bottom")
      #  shinyjs::show("glp_button_dupl")
      #  shinyjs::hide("add_pt_dupl")
    } else {
      shinyBS::updateButton(session, "glp_pt_button_dupl", label=" Add", style="info", icon=icon("hand-point-right"))
      shinyBS::removeTooltip(session,"glp_pt_button_dupl")
      #  shinyjs::show("add_pt_dupl")
      # shinyjs::hide("glp_button_dupl") 
    }
  })
  
  
  
  
  
  
  ##################### Duplicate removal part
  output$pt_text_dupl <- renderText({"<b>Procedural Track</b>"})
  output$pt_dupl <- renderText({"<i><small>Waiting for action!</small></i>"})
  
  # fnDUPL_FINDER: stores results of FuNction duplicate_finder
  data <- reactiveValues(fnDUPL_FINDER = NULL, test=NULL)
  
  #it elaborates phenodata table to retrieve potential column duplicates 
  output$pick_col_duplicates <- renderUI({
    
    df= gVars$phTable[[1]]
    
    procesD <- gVars$processedD
    fnDUPL_FINDER = duplicate_finder(df)
    data$fnDUPL_FINDER = (fnDUPL_FINDER)
    
    
    k1<- data$fnDUPL_FINDER
    
    identicals <- (plyr::ldply(k1, rbind)) %>% rbind("",.)
    identicals[is.na(identicals)] <- ""
    identicals[["whole_str"]] <- apply(identicals, 1,function(x) paste (x,collapse=" -- "))
    identicals[["whole_str"]][1] <- ""
    a<-identicals$whole_str
    
    tmp1 <-data.frame(a) %>% filter(., !(a %in% procesD)) %>%  
      rowwise() %>% 
      mutate(dropdownText=HTML(paste0("<span class='col1'>", a,  "</span>"))) %>%  ungroup()
    
    #for_choice<-c("",tmp1$a)
    pickerInput(
      
      inputId = "pick_col_duplicate",
      label = "Select the group of identical columns:",
      choices = tmp1$a,
      selected = NULL,
      choicesOpt=list(content=c(tmp1$dropdownText)),
      options = list(`actions-box` = TRUE,
                     `selected-text-format` = paste0("count > ", length(colnames(data$fnDUPL_FINDER)) - 1),
                     `count-selected-text` = "All pairs",
                     liveSearch = TRUE,
                     liveSearchPlaceholder = TRUE),   # build buttons for collective selection
      multiple = FALSE)
  })
  
  
  
  
  
  
  # as consequence of selecting a specific group of identical columns, their contents are shown; the group name is splitted to store the specific colnames to match in order to identify which needed to be shown. 
  output$content_dupl_remov <- renderDataTable({
    req(input$pick_col_duplicate)
    dupl_cols = unlist(strsplit(as.character(input$pick_col_duplicate), split = " -- "))   
    table1 <- gVars$phTable[[1]]
    #  table1<- table1 %>% .[ , !(names(.) %in% toexclude)] %>% dplyr::select(.,selected_cols)
    table1<-dplyr::select(table1,dupl_cols)
    colnames(table1) <-dupl_cols
    gVars$potential_choices = dupl_cols
    table1
    
  })
  
  
  
  # as consequence of selecting a specific pair, it shows the correspondent label in the upper left box
  observeEvent(input$pick_col_duplicate,{
    output$pt_dupl <- renderText({"..."})
    #radiobuttons are created dinamically here in the server and given to UI.
    output$radioDUPL <- renderUI({
      potential_choices  <- gVars$potential_choices  
      
      options <- c(potential_choices, "None")  #it creates the option of radiobuttons; "none" to delete all columns of the group 
      # The options are dynamically generated on the server
      radioButtons("reply", "Select the column to keep", options, selected = character(0))
    })
  })
  
  
  #as the aradio button options are selected the PT is updated
  observeEvent(input$reply,{
    df <- gVars$phTable[[1]]
    choice <- as.character((input$reply))
    potential_choices <- gVars$potential_choices 
    toexclude <- potential_choices[!choice == potential_choices]
    gVars$dupl_decisions <- c(choice, toexclude)
    
    
    
    req(!is_empty(choice) )
    #updates the PT box with what was done and accepted to do
    if (choice == "None"){
      toexcl_2string <- paste0(" '", paste0(toexclude, collapse= "', '"),"' ")
      Value <- paste0("All duplicated columns (", toexcl_2string, ") are deleted.")
    }
    else {
      if(length(toexclude)==1) {
        indicator <- c("duplicate", "is") 
        toexcl_2string <- paste0(" '", toexclude, "' ")}
      else {
        indicator <- c("duplicates","are") 
        toexcl_2string <- paste0(" '", paste0(toexclude, collapse= "', '"),"' ")}
      Value <- paste0("The column '",choice, "' is kept, while its ", indicator[1], toexcl_2string, indicator[2]," deleted.")
    } 
    #updateTextAreaInput(session, inputId="PT_dupl_input", "Procedural track", value = Value)
    output$pt_dupl <- renderText({Value})
    gVars$tmp_pt_value <- Value
    
    
  })
  
  
  
  
  
  
  observeEvent(      input$submit,{
    
    choice <- as.character((input$reply))
    if(is_empty(choice)){
      shinyjs::info("Please select one option!")
      return(NULL)
    }
    
    #updates buttons
    disable("reply")
    disable("undo_dupl_button")
    enable("glp_pt_button_dupl")
    
    #what was done is stored
    Value <- gVars$tmp_pt_value 
    gVars$pt_glp_list <- c(gVars$pt_glp_list, Value)
    
    
    
    
    
    
    
    
    
    shinyBS::updateButton(session, "submit", style="success", icon=icon("hand-point-right"))
    
    
    
    
    
    
    
    
    disable("submit")
    
  })
  
  
  
  
  observeEvent(input$glp_pt_button_dupl,{#double_reactors_duplicate(),{
    #it stores the decision about each duplicate group: dupl_decis are (kept, deleteted colnames)
    dupl_decis <- gVars$dupl_decisions
    df <- gVars$phTable[[1]]
    req(input$reply)
    
    
    # metto qui l upd del df e non aggiorno
    potential_choices <- gVars$potential_choices
    choice<- dupl_decis[1]
    toexclude <- potential_choices[!choice == potential_choices]
    gVars$tmpdf <- df[,!(names(df) %in% toexclude)]
    gVars$selected_cols <- choice
    shinyjs::enable("submit")
    gVars$phTable[[1]] <- gVars$tmpdf
    #updateTextAreaInput(session, inputId="PT_dupl_input", "Procedural track", value = "...")
    shinyBS::updateButton(session, "submit", style="info", icon=icon("hand-point-right"))
    shinyBS::updateButton(session, "glp_pt_button_dupl", style="info", icon=icon("hand-point-right"))
    
    #storing for undostep
    dupl_AC <- list( choice,potential_choices )
    gVars$dupl_op_list <- c(gVars$dupl_op_list, (list(dupl_AC=dupl_AC)))
    gVars$last5list <- c(gVars$last5list, (list(dupl_AC=dupl_AC)))
    
    
    
    
    if(!input$GLP_inactivation_mode){    #such as GLPmode activated
      showModal(dataModal())
      disable("undo_dupl_button")
      
      
      
    } else { # only procedural track report
      # re-update buttons 
      shinyBS::updateButton(session, "submit", style="info", icon=icon("hand-point-right"))
      enable("submit")
      enable("undo_dupl_button")
      
    }
    gVars$potential_choices <- NULL  
  }) 
  
  
  
  ########################## end duplicate part
  
  
  
  
  
  #variable to store later the updated content
  upd_content <<- c()
  
  
  
  ###########START: recoding content part
  # fnSINGLE_COL_RECODER: stores results of FuNction column_toRecode
  data <- reactiveValues(fnCOLUMNS_to_RECODE = NULL)
  previous_cols_list <- NULL
  
  #it elaborates phenodata table to retrieve a first group of columns to recode
  observeEvent(c(input$recoding_button, input$glp_pt_button_rec),{ #input$save_recoding),{  #rex123
    req(!is.null(gVars$dkeys) , !is.null(gVars$phTable))
    df <- gVars$phTable[[1]]
    dkeys = gVars$dkeys
    alw <- gVars$always
    procesLIST2REC <- gVars$processedLIST2REC 
    proces_cols_RECOD <- gVars$processed_cols_RECOD
    
    # fnCOLUMNS_to_RECODE diventa df_2recode
    df_2recode = df[,!colnames(df) %in% alw]
    data$fnCOLUMNS_to_RECODE = data.frame(df_2recode)
    
    
    
    # it ONLY shows the list of those clumns that are going to be recorded and it creates a dropdown menu
    a <- colnames(df_2recode)
    
    
    tmp <- data.frame(a) %>% filter(.,!(a %in% procesLIST2REC)) %>% filter(.,!(a %in% proces_cols_RECOD)) %>%
      filter(.,!(a %in% gVars$deleted_colname))    %>% rowwise()
    
    gVars$list_cols_tmp <- tmp$a
    updateSelectizeInput(session,
                         inputId = "pick_cols_recoding",
                         #label = "Select the column to recode you wish to display:",
                         choices = tmp$a,
                         selected = current_selection(),
                         server=TRUE)
  })
  
  current_selection <- reactiveVal(NULL)
  observeEvent(input$pick_cols_recoding, {
    current_selection(input$pick_cols_recoding)
  })
  
  
  # as consequence of selecting a specific column name from the "column to recode" dropdown menu, it shows the column content of that column.
  output$contentLIST_COLS <- renderDataTable({
    req(input$pick_cols_recoding)
    if(gVars$flag_REMOVE_WIDGETS){     
      table1 <- gVars$phTable[[1]]        
      table1<-data.frame(table1[,req(input$pick_cols_recoding)])
      colnames(table1) <-input$pick_cols_recoding
      table1
    }
  })
  
  
  
  
  data <- reactiveValues(fnSING_COL_RECODE_FINDER = NULL)
  later_stored <<- c()
  
  # as consequence of selecting a specific column name from the "column to recode" dropdown menu, it shows the correspondent column name in the TextInput box above on the left(recoded_lab_value. There, it could be modified and saved again into the dataset. Just below, the unique cell contents of the column and their recoded version are shown. 
  #recode_synthesis <- eventReactive(input$pick_cols_recoding,{
  #  df = gVars$phTable[[1]]
  #  dkeys = gVars$dkeys
  #  chosen_rec_label <- input$pick_cols_recoding
  #  fnSING_COL_RECODE_FINDER = single_col_recode_finder(dkeys,df[[chosen_rec_label]])
  #  list(colname = chosen_rec_label, uniquecontents = fnSING_COL_RECODE_FINDER)
  #  })
  
  
  
  
  observeEvent(input$pick_cols_recoding, {
    gVars$flag_REMOVE_WIDGETS <<-TRUE
  })
  
  
  # as consequence of selecting a specific column name from the "column to recode" dropdown menu, it shows the correspondent column name in the TextInput box above on the left(recoded_lab_value. There, it could be modified and saved again into the dataset. Just below, the unique cell contents of the column and their recoded version are shown. 
  #creates a double input to observe and react to remove the elaborated and stored element from the list of recoded contents
  double_reactors <- reactive({
    paste(input$pick_cols_recoding)#,  input$glp_pt_button_rec)# input$save_recoding)  #rex123
  })
  
  recode_synthesis <- eventReactive( double_reactors(), {
    req(input$pick_cols_recoding)
    df = gVars$phTable[[1]]
    dkeys = gVars$dkeys
    chosen_rec_label <- ifelse(!is.null(input$recoded_label),
                               ifelse(identical(input$pick_cols_recoding,input$recoded_label), input$recoded_label,  input$pick_cols_recoding ),
                               input$pick_cols_recoding)
   
    if(gVars$flag_REMOVE_WIDGETS){ 
      gVars$orig_lab <- reactiveVal(chosen_rec_label) #input$pick_cols_recoding)
      fnSING_COL_RECODE_FINDER = single_col_recode_finder(dkeys,df[[chosen_rec_label]])
      upd_fnSING_COL_RECODE_FINDER <- isolate({filter(fnSING_COL_RECODE_FINDER, !fnSING_COL_RECODE_FINDER$cases %in% gVars$processedLIST_content2RECODE )})
      if (nrow(upd_fnSING_COL_RECODE_FINDER) ==0) {#chosen_rec_label = ""
        gVars$processed_cols_RECOD <- c( gVars$processed_cols_RECOD, chosen_rec_label)
        t <- gVars$list_cols_tmp [!gVars$list_cols_tmp %in% gVars$processed_cols_RECOD] #%>% .[!. %in% gVars$deleted_colname]
        updateSelectizeInput(session,
                             inputId = "pick_cols_recoding",
                             # label = "Select the column to recode you wish to display:",
                             choices = t,
                             server=TRUE,
                             selected = 2)
        
      }
      list(colname = chosen_rec_label, uniquecontents = upd_fnSING_COL_RECODE_FINDER)
    }
  })
  
  
  output$recoded_lab_value <-  renderUI({
    req(input$pick_cols_recoding)
    if(gVars$flag_REMOVE_WIDGETS){ 
      textInput(inputId="recoded_label", "Recoded column name", value = recode_synthesis()$colname, width = NULL, placeholder = NULL) 
      
    }
    
  })
  
  
  
  
  observeEvent(input$pick_cols_recoding, {
    req(input$pick_cols_recoding)
    df = gVars$phTable[[1]]
    dkeys = gVars$dkeys
    procesLIST_content2RECODE <- gVars$processedLIST_content2RECODE 
    
    data$fnSING_COL_RECODE_FINDER = data.frame(recode_synthesis()$uniquecontents)
    
    
    d <- data$fnSING_COL_RECODE_FINDER$cases
    e <- data$fnSING_COL_RECODE_FINDER$text
    f <- unlist(data$fnSING_COL_RECODE_FINDER$pairs)
    g <- data$fnSING_COL_RECODE_FINDER$indicator
    gVars$map_recoding <- isolate({data.frame(d,e,f,g)  %>% filter(., !(g ==0)) }) 
    gVars$map_for_plot <- gVars$map_recoding #data.frame(d,e,f,g) 
    gVars$test <- data.frame(d,e,f,g) 
    
    
    tmp <- data.frame(d,e,f,g) %>% #filter(., (!f %in% procesLIST_content2RECODE)) %>% filter(., (!d %in% upd_content)) %>% #%>% filter(., (!d %in% later_stored)) %>% #  .[!is.na(.$f),] %>% ###########################
    rowwise() %>% 
      mutate(dropdownText=HTML(paste0("<span class='col4'>", d, 
                                      "</span><span class='col5'>", e,  
                                      "</span><span class='col6'>",f, "</span>"))) %>%  ungroup()
    
    
    updatePickerInput( session,
      inputId = "pick_recoding_content_pairs",
      label = "Select the content of the column with its recoded version:",
      choices = tmp$f,   #tmp$f,
      choicesOpt=list(content=tmp$dropdownText),
      options = list(`actions-box` = TRUE,
                     `selected-text-format` = paste0("count > ", length(colnames(data$fnCOL_LAB_FINDER)) - 1),
                     `count-selected-text` = "All pairs",
                     liveSearch = TRUE,
                     liveSearchPlaceholder = TRUE)   # build buttons for collective selection
      )
    
    
  })
  
  
  
  observeEvent(input$step_submit2,{
    
    write.xlsx(gVars$test, "map_reco_cell.xlsx", row.names=FALSE)
    
    mrf<- (gVars$test)
    df<- (gVars$phTable[[1]])
    browser()
   
    mrf_sub <- filter(mrf, g!=0)
    write.xlsx(mrf_sub, "newcells.xlsx", row.names=FALSE)
    newfile<-data.frame()
    #sostituisce i content
    apply(mrf_sub, 1, function (mrfsub_new_field) { 
      
      df$cell_line_name[df$cell_line_name == mrfsub_new_field[1]] <<- mrfsub_new_field[3]
      
    })
    cou <- data.frame(cbind(df=df$cell_line_name,df1=df1$cell_line_name))
    length(which(cou$df == cou$df1))
    cou[(which(cou$df != cou$df1)),]
    subset(cou, cou$df == cou$df1)
    write.xlsx(df, "final_df_cells.xlsx", row.names=FALSE)
    
  })
  
  
  
  
  
  observeEvent(c(input$glp_pt_button_rec,input$step_submit) ,{   #re123
    
    req(!is.null(gVars$map_recoding))#, input$pick_recoding_content_pairs)
    if (!is.null(input$pick_recoding_content_pairs)){
        chosen_rec_orig_content <-  gVars$map_recoding[gVars$map_recoding$f %in% input$pick_recoding_content_pairs,]$d
    #gVars$map_recoding[gVars$map_recoding$f %in% input$pick_recoding_content_pairs,]$d
    #ifelse(!is_empty(gVars$map_recoding), 
    #   gVars$map_recoding[gVars$map_recoding$f %in% input$pick_recoding_content_pairs,]$d, NA)
    #  gVars$orig_cont <- reactiveVal(chosen_rec_orig_content)
    #  gVars$originalcontent <- chosen_rec_orig_content
    
        chosen_rec_content <-  input$pick_recoding_content_pairs # ifelse(input$pick_recoding_content_pairs=="", "",input$pick_recoding_content_pairs  ) 
    #  if(chosen_rec_content %in% "NA") {  chosen_rec_content <- NA }
    
   # gVars$originalcontent <- (chosen_rec_content)        #Re123
        gVars$orig_cont <- reactiveVal(chosen_rec_orig_content)
    }
    
    
  })
  
  
  
  
  
  
  output$recoded_content_value <- renderUI({
    req( input$pick_cols_recoding, !is.null(gVars$phTable))
    
    chosen_rec_content <-  input$pick_recoding_content_pairs  
    gVars$originalcontent <- (chosen_rec_content)        
    
   msg <- c()
   if(input$pick_recoding_content_pairs=="") {msg <-"Leave blank for NA"}#, "")
    textInput(inputId="recoded_content", "Recoded unique content", value = chosen_rec_content, width = NULL, placeholder = msg) 
  
  })
  
  
  

  
  
  

  
  
  
  observeEvent(input$pick_recoding_content_pairs,{
  output$barplot <- renderPlot({ 
    req(input$pick_cols_recoding )
    
    if(gVars$flag_REMOVE_WIDGETS){ 
      dt <- (gVars$phTable[[1]][,input$pick_cols_recoding]) %>%  
        table(.) %>%  data.frame(unclass(.))  
      
      colnames(dt) <- c("id","Frequency")
      dt <- dt[c("id","Frequency")]
      row.names(dt) <- NULL
      
      req(input$pick_recoding_content_pairs)
      oldc<- dplyr::filter(gVars$map_for_plot, f %in% input$pick_recoding_content_pairs )$d
      dt$highlight <- ifelse(dt$id == oldc,  1,  0)
      
      ggplot((dt),
             aes(id, Frequency, fill=highlight)) +
        geom_bar(stat = "identity")+
        theme(axis.title.x = element_blank(), 
              axis.text.x = element_text(angle = 60, hjust = 1),
              legend.position="none")+ scale_x_discrete(labels = function(x) label_shortener(x, 22))
    }  
  })
  })
  

  
  
  # in the newly opened page
  # pressing button Next to the deletion selection
  observeEvent(      input$step_submit_del,{
    req(input$pick_cols_recoding)
    df <- data.frame(gVars$phTable[[1]])
    choice <- as.character((input$step_choice))
    
    
    if(is_empty(choice)){
      shinyjs::info("Please select the next step!")
      return()
    }
    
    else {    #deletion of selected column
      shinyjs::info("The selected column is being deleted!")
      
      chosen_col_step<- c(input$recoded_label,input$pick_cols_recoding)
      gVars$deleted_colname <- c(gVars$deleted_colname,input$pick_cols_recoding)
      deleted_col_tmp <- data.frame(df[,(names(df) %in% chosen_col_step)]) %>% setNames(.,input$pick_cols_recoding)
      df <- df[,!(names(df) %in% chosen_col_step)]
      gVars$phTable[[1]] <-df
      t3 <- gVars$list_cols_tmp [!gVars$list_cols_tmp %in% gVars$deleted_colname] %>% .[!. %in%gVars$processedLIST2REC] %>%
        .[!. %in%gVars$processed_cols_RECOD]   %>% .[!. %in%gVars$deleted_colname ]  
      updateSelectizeInput(session,
                           inputId = "pick_cols_recoding",
                           #label = "Select the column to recode you wish to display:",
                           choices = t3,
                           selected = 2,
                           server=TRUE
      )
      
      
      
      #storing for undostep
      deleted <- list( input$pick_cols_recoding,deleted_col_tmp)
      
      gVars$delete_op_list <- c(gVars$delete_op_list, (list(deleted=deleted))) 
      gVars$last5list <- c(gVars$last5list, list(deleted=deleted))   
      
      
      
      # once delted was selected, need to write procedural track and glp comment if needed
      output$pt_recod_del<- renderText({"<b>Procedural Track</b>"})
      del_op_message_reactive <- reactiveValues( msg=paste(  (input$pick_cols_recoding)  ,"was deleted.") )
      #Sys.sleep(3)
      output$pt_recod_del_msg <- renderText({ del_op_message_reactive$msg}) #paste(  deleted  ,"was deleted.")})
      shinyBS::updateButton(session, "glp_pt_button_rec_del", disabled=FALSE)
      
      gVars$pt_glp_list <- c(gVars$pt_glp_list, del_op_message_reactive$msg)
      
      
      
      
      
      
      
      #updatebutton
      shinyBS::updateButton(session, "undo_del_button", disabled=FALSE)
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
    }
    
    
  })
  
  
  
  observeEvent(input$glp_pt_button_rec_del, {
    
    shinyBS::updateButton(session, "glp_pt_button_rec_del", disabled=TRUE)
    updateRadioButtons(session, "step_choice", choices= c("Delete" = 1, "Modify and/or Save" = 2, "Specials" =3)  , selected = character(0), inline=TRUE)
    shinyBS::updateButton(session, "undo_del_button", disabled=FALSE)
    output$pt_recod_del_msg <- renderText({"<i><small>Waiting for action!</small></i>"})
    
    
    if(!input$GLP_inactivation_mode){    #such as GLPmode activated
      showModal(dataModal())
      
      #observeEvent(input$glp_store_comment, {
      #  toggleModal(session, "modalstep", "close")  })
      
      #output$candidate_label <- renderText({" "})
    } 
    
    
  })
  
  
  
  
  
  # pressing button Next to the deletion selection
  observeEvent(      input$step_submit,{
    #req(input$step_submit,input$step_choice)   ##########*###
    df <- data.frame(gVars$phTable[[1]])
    choice <- as.character((input$step_choice))
    
    
    if(is_empty(choice)){
      shinyjs::info("Please select the next step!")
      return()
    }
    
    
    else { #(choice == "Modify and/or Save") #it shows newly edited label and content 
      d<-gVars$dkeys
      output$new_label_text <- renderText({"Edited label:"})
      output$edited_label <- renderText({colorwords(isolate({input$recoded_label}),d)})
      
      output$new_content_text <- renderText({"Edited content:"})
      output$edited_content <- renderText({colorwords(isolate({input$recoded_content}),d)})
      #    updateRadioButtons(session, "step_choice", selected = character(0))
      
    }#CHIUSURA ELSE
    
  })
  
  
  
  #buttons and actions
  #button reject in recoding section  
  
  observeEvent(input$reject_full_recoding,{
    req(input$pick_cols_recoding)
    shiny::validate(
      need(!is.null(gVars$phTable), "No Phenodata File Provided!")
    )
    
    shiny::validate(
      
      need(!is.null(gVars$inputGx), "No Vocabulary File Provided!")
    )
    # When reject is pressed, it keeps the picked column (keep_original_column) and rejects the full recoding version of its content by deleting the colname from the dropdown menu (the one with column names on the left of the app-page)
    newlabel <- input$recoded_label
    oldlabel <- input$pick_cols_recoding
    gVars$phTable[[1]]<-setnames((gVars$phTable[[1]]), oldlabel, newlabel)
    gVars$tmp_stored_df <-gVars$phTable[[1]]
    
    colname_to_keep <- newlabel
    
    #cleaning the list to show in selectizeInput
    gVars$processedLIST2REC <- c(gVars$processedLIST2REC,colname_to_keep)
    gVars$list_cols_tmp <- gVars$list_cols_tmp[gVars$list_cols_tmp != oldlabel]
    t1 <- gVars$list_cols_tmp [!gVars$list_cols_tmp %in% gVars$processedLIST2REC] %>% .[!. %in% gVars$deleted_colname] %>%
      .[!. %in%gVars$processed_cols_RECOD]
    updateSelectizeInput(session,
                         inputId = "pick_cols_recoding",
                         #label = "Select the column to recode you wish to display:",
                         choices = t1,
                         selected = 2,
                         server=TRUE
    )
    
    
    t_modified <- c(newlabel, oldlabel, NA,NA, gVars$username)
    gVars$modification_synthesis <- rbind(gVars$modification_synthesis, t_modified)
    
    #gVars$dict_modif <- add_to_dict(gVars$dict_modif, t_modified[1], t_modified[2])  
    #recoding in the vocabulary for updating undos
    if (!identical(newlabel, oldlabel)) {gVars$dict_modifLAB$set(t_modified[2], t_modified[1])}
    #storing for undostep
    recode_RJ <- list( t_modified)#list( t_modified, gVars$phTable[[1]][,t_modified[1]] )
    gVars$recodeRJ_op_list <- append(gVars$recodeRJ_op_list, list(recode_RJ=recode_RJ))
    
    gVars$last5list <- c(gVars$last5list, (list(recode_RJ=recode_RJ)))
    
    #update button
    shinyBS::updateButton(session, "save_recoding",  disabled=TRUE)
    shinyBS::updateButton(session,"reject_full_recoding", style="success", disabled = TRUE) 
    
    shinyBS::updateButton(session,"store_button", disabled = FALSE) 
    output$pt_recod <- renderText({"..."})
    
    
    
    
  })
  
  
  
  # button accept in recoding section
  observeEvent(input$save_recoding,{    #input$save_recoding,{   #rex123
    shiny::validate(
      need(!is.null(gVars$phTable), "No Phenodata File Provided!")
    )
    
    shiny::validate(
      need(!is.null(gVars$inputGx), "No Vocabulary File Provided!")
    )
    #COLNAME:
    # check if colname is modified and only if yes, it saves the new version (input$recoded_label) deleting the old(input$pick_cols_recoding)
    df <- gVars$phTable[[1]]
    cmodification <- rep(NA,5) 
   # old_content <-NULL
    req(input$recoded_label)
     
    
    if(!identical(input$recoded_label,input$pick_cols_recoding)){   
      #stores the new/old labels 
      new_colname <- input$recoded_label
      
      gVars$original_label <- input$pick_cols_recoding
      original_oldcolname <<- input$pick_cols_recoding
      setnames(df, original_oldcolname, new_colname)
      #flag to remove widgets based on renamed cols
      gVars$flag_REMOVE_WIDGETS <<- FALSE
    } else{ 
      new_colname <- input$pick_cols_recoding
    #  gVars$original_label <- gVars$recod_ops_dataframe %>% 
    #    .[.$newlab==input$pick_cols_recoding,] %>%
    #    .[.$oldlab!=input$pick_cols_recoding, ] %>% .$oldlab
    #  if (is.null(gVars$original_label)){ gVars$original_label <- new_colname}
      gVars$original_label <- input$pick_cols_recoding
      original_oldcolname <<- gVars$original_label
      gVars$flag_REMOVE_WIDGETS <<- TRUE
      
      
    }
    #NO gVars$processedLIST2REC <- c(gVars$processedLIST2REC,new_colname)  #.123
    
    #CONTENTS
#    old_content <- gVars$originalcontent #gVars$orig_cont()
    
    #substitute the old contents with the new recoded version
    #req(input$recoded_content)
    
    originalcontent <-  ifelse(gVars$originalcontent != "", gVars$originalcontent, NA)
    
    mr_tmp <-  data.frame(gVars$map_recoding)
    if (!input$recoded_content %in% originalcontent) {
      old_content <- mr_tmp[mr_tmp$f %in% originalcontent,]$d     #filter(as.data.frame(mr_tmp), (f == originalcontent))$d
      mr_tmp["f"][mr_tmp["f"] == originalcontent] <- input$recoded_content 
      gVars$map_recoding <- data.frame(mr_tmp)
    } else {      old_content <- filter(mr_tmp, (f == input$recoded_content))$d}
    
    
    #substitution in the df
    new_content <- input$recoded_content  
    df[new_colname][df[new_colname] == old_content] <- new_content 
    
    #stores temporarely this value to clean the recoding pairs later
    gVars$clean_recoding_pairs <- new_content
    #prepare the new voices to update the vocabulary
    cmodification[c(1:5)] <- c(new_colname, original_oldcolname, new_content, old_content,gVars$username)
    gVars$recod_ops_dataframe <-  as.data.frame(rbind(gVars$recod_ops_dataframe , cmodification)) %>% 
      setNames(., c("newlab","oldlab","newcont","oldcont")) %>%
          `rownames<-`(NULL) 
    #recoding in the vocabulary for updating undos
    if (!identical(new_colname, original_oldcolname))   {gVars$dict_modifLAB$set(cmodification[2], cmodification[1])}
    if (!identical(new_content, old_content)) {gVars$dict_modifCONT$set(cmodification[4], cmodification[3])}
    
    
    
    gVars$modification_synthesis <- rbind(gVars$modification_synthesis, cmodification)
    
   
    
    
    
    # storing the operation for the undo last5step and general undo
    #storing for undostep
    recode_AC <- list(gVars$recod_ops_dataframe )
    recode_AC_forlast5 <- list(tail(recode_AC[[1]],1))
    
    gVars$recode_list <- list(recode_AC=recode_AC)
    gVars$last5list <- append(gVars$last5list, (list(recode_AC=recode_AC_forlast5)))
    
    
    #update button
    shinyBS::updateButton(session, "save_recoding",   style="success", disabled=TRUE)
    shinyBS::updateButton(session,"reject_full_recoding", disabled = TRUE) 
    
    shinyBS::updateButton(session,"store_button", disabled = FALSE) 
    output$pt_recod <- renderText({"..."})
    
    
    gVars$tmp_stored_df <-df
  })
  
  
  observeEvent(input$glp_pt_button_rec,{
    req(!is.null(gVars$tmp_stored_df))
    gVars$phTable[[1]]<-gVars$tmp_stored_df
    
    #storing of the newly recoded content
    gVars$processedLIST_content2RECODE = c(gVars$processedLIST_content2RECODE,gVars$clean_recoding_pairs)
    
    
    #once eliminated all recoding proposals in recoding dropdown menu, it deletes the proposed column name on related dropdown menu  (on the left)
    if(is.null(input$pick_recoding_content_pairs)){
      new_colname <- input$label_TOclean_FORvocabulary
      gVars$processedLIST2REC <- c(gVars$processedLIST2REC,new_colname)  #.123
    }
    
  })
  
  
  output$original_label <-  renderUI({
    req(!is_empty(gVars$orig_lab()))
    textInput(inputId="label_TOclean_FORvocabulary", "Potential label-entry", value = gVars$orig_lab()) #gVars$original_label) 
  })
  
  output$original_content <- renderUI({
    req(!is.null(gVars$phTable[[1]]),!is.null(gVars$dkeys))
    req(!is_empty(gVars$orig_lab())   )#wew,!is_empty(gVars$orig_cont()))
    textInput(inputId="content_TOclean_FORvocabulary", "Potential content-entry", value =  (gVars$orig_cont()))# isol)#gVars$orig_cont())#(gVars$originalcontent)) 
    })
  
  
  
  observeEvent(input$recoded_label, {
    req(input$pick_cols_recoding, input$recoded_label)
    reclab<- input$recoded_label
    dkeys = gVars$dkeys
    h <- dkeys$label
    i <- dkeys$lab_syn
    
    tmp <- rbind("", unique(data.frame(h,i)))  #data.frame(.)     #%>% distinct(.)
     
    if(is.element(reclab, unlist(tmp) )){   
      tmp<- tmp %>% filter_all(any_vars(. == reclab))
    } else {tmp<- tmp}
    
    tmp<- tmp %>% 
      rowwise() %>% 
      mutate(dropdownText=HTML(paste0("<span class='col7'>", h, 
                                      "</span><span class='col8'>",i, "</span>"))) %>%  ungroup()
    
    
    updatePickerInput(session,
      inputId = "pick_to_check_dict_labels",
      label = "Labels and synonyms",
      choices = tmp$h,
      choicesOpt=list(content=tmp$dropdownText),
      options = list(
                     liveSearch = TRUE,
                     liveSearchPlaceholder = "Search entry"))
  })
  
  
  
  output$pick_to_check_dict_contents <- renderUI({
    req(input$pick_cols_recoding)
    req(input$pick_to_check_dict_labels)
    dkeys = gVars$dkeys
    i <- dkeys$label
    k <- dkeys$allowed_features
    j <- dkeys$syn_features
    
    tmp <- rbind("",unique(data.frame(i,k,j))) %>% filter(., i == input$pick_to_check_dict_labels)
    rec_cont<-input$recoded_content
    
    if (is.element(rec_cont, tmp)){
      tmp<- tmp %>% filter_all(any_vars(. == rec_cont))
    }
    
    
    tmp<- tmp %>% 
      rowwise() %>% 
      mutate(dropdownText=HTML(paste0("<span class='col9'>", i, 
                                      "</span><span class='col10'>",k, 
                                      "</span><span class='col11'>",j,"</span>"))) %>%  ungroup()
    
    
    pickerInput(
      inputId = "pick_to_check_dict_contents",
      label = "Contents and synonyms",
      choices = tmp$k,
      choicesOpt=list(content=tmp$dropdownText),
      options = list(`actions-box` = TRUE,
                     `selected-text-format` = paste0("count > ", length(colnames(tmp)) - 1),
                     `count-selected-text` = "All pairs",
                     liveSearch = TRUE,
                     liveSearchPlaceholder = "Search entry"),   # build buttons for collective selection
      multiple = FALSE)
    
    
  })
  
  
  # it allows to select the option to store newly edited label and content into the vocabulary
  output$radiostoreLAB <- renderUI({ 
    radioButtons("store_labels", label = ("Labels"),
                 choices = list("as Label Synonym" = 1, "Do not store"=2), 
                 selected = character(0))
  })
  
  
  output$radiostoreCONT <- renderUI({
    radioButtons("store_contents", label = ("Contents"),
                 choices = list("as Content Synonym" = 3, "Do not store"=4),
                 selected = character(0))
  })
  
  
  
  
  
  
  
  
  
  
  
  
  

  
  anti_join_if_possible = function(d1, d2) {
    if (0 %in% dim(d1)) {
      message("Warning: First data frame is empty")
      return(d2)
    } else if (0 %in% dim(d2)) {
      message("Warning: Second data frame is empty")
      return(d1)
    } else{
      return( anti_join(d1,d2 ))
    }
  }
  
  
  
  
  gVars$later_voc_upd_df <- readxl::read_xlsx("./Book1.xlsx")
  
  
  
  #takes what in storage and updates the vocab - IT WAS WORKING BEFORE DIVIDING SECTION FOR UPDATING THE VOCAB
  observeEvent( input$update_pre_VOCstorage_button,{   #input$update_voc_button ,,{   quello gisuto: input$outcomes_dictXcandidates
    #  req(input$outcomes_dictXcandidates)   DA RIMETTERE MA VA SISTEMATO SOTTO
    req(nrow(gVars$later_voc_upd_df)!=0)
    candidate_df <- gVars$later_voc_upd_df[!duplicated(gVars$later_voc_upd_df),]
  
    dict <- gVars$dkeys  
    uniqDictLab <- unique(dict$label)  
    choices_options <- c("Safe","Fast check","To overview")
    
    #safe words (allowed label and content already present in the vocabulary and wished to be saved as main label/content (1,3) or not saving the content(5))
   
    tmp_pre_safe <- candidate_df %>% .[which(.$mainL %in% uniqDictLab),] 
    
    tmp_pre_safe_wCONTENT <- lapply((tmp_pre_safe$mainL), function(k) {
      tmp_dict <- unique((dict[dict$label %in% k,]$allowed_features)) 
      y<-filter (tmp_pre_safe[tmp_pre_safe$mainL %in% k,], mainC %in% tmp_dict | is.na(mainC)) 
      y
    }) %>% rbindlist(.) %>% unique(.)  
      
    
    # this side variable takes care of rows where label and/or content are in common with dict, but they may show NAs in other cells (potetnially absent in the vocabulary)  
    #all NA except label in dict
    side_safe_allNA <- tmp_pre_safe %>% filter_at(vars(newL,mainC,newC), all_vars(is.na(.)))             #all Na except label
    
    #in reference to the dict, only common label and content and all NA
    side_safe_partialNA <- lapply(tmp_pre_safe_wCONTENT$mainL, function(k) { 
      tmp_dict <- unique((dict[dict$label %in% k,])) 
      y<-filter (tmp_pre_safe_wCONTENT[tmp_pre_safe_wCONTENT$mainL %in% k,], newC %in% tmp_dict$syn_features | is.na(newC), newL %in% tmp_dict$lab_syn | is.na(newL)) 
      y
    }) %>% rbindlist(.) %>% unique(.)
    
    #only new matching values
    side_safe_with_values <-  lapply(tmp_pre_safe$mainL, function(k) { 
          tmp_dict <- unique((dict[dict$label %in% k,])) 
          y<-filter (tmp_pre_safe[tmp_pre_safe$mainL %in% k,], newL %in% tmp_dict$lab_syn, mainC %in% tmp_dict$allowed_features, newC %in% tmp_dict$syn_features ) 
          y
        }) %>% rbindlist(.) %>% unique(.)
    
    
    
     safe <- rbind(side_safe_with_values, side_safe_allNA, side_safe_partialNA ) %>% distinct()  #%>%
    if (nrow(safe)==0) {choices_options <- choices_options[!choices_options %in% "Safe"]}
    
    
    
    
    #outsider words (new label and content are completely new - so no synonyms are present)
     pre_outsider <- candidate_df %>% anti_join(., safe) %>% filter( !mainL %in% uniqDictLab)
    outsider1 <-  lapply(pre_outsider$mainL, function(k) { 
      tmp_dict <- unique((dict[dict$label %in% k,])) 
      
      y<-filter (pre_outsider[pre_outsider$mainL %in% k,], !newL %in% na.omit(tmp_dict$lab_syn), !mainC %in% na.omit(tmp_dict$allowed_features), !newC %in% na.omit(tmp_dict$syn_features )) 
      y
    }) %>% rbindlist(.) %>% unique(.)
      
      
      
    outsider2 <-  lapply(pre_outsider$mainL, function(k) { 
      tmp_dict <- unique((dict[dict$label %in% k,])) 
      y<-filter (pre_outsider[pre_outsider$mainL %in% k,], !newL %in% na.omit(tmp_dict$lab_syn), is.na(mainC), is.na(newC)) 
      y
    }) %>% rbindlist(.) %>% unique(.)
      
      
    outsider <- rbind(outsider1, outsider2) %>% unique()
    if (nrow(outsider)==0) {choices_options <- choices_options[!choices_options %in% "Fast check"]}
    
    
    
    
    enrichments <-  anti_join_if_possible(candidate_df,safe) %>% 
      anti_join_if_possible(., outsider) 
    if (nrow(enrichments)==0) {choices_options <- choices_options[!choices_options %in% "To overview"]}
    
 
    updateSelectizeInput(session,
                         inputId = "outcomes_dictXcandidates",   
                         choices= choices_options, 
                         selected= character(0),
                         options = list(
                           placeholder = 'Please select an option below',
                           onInitialize = I('function() { this.setValue(""); }')),
                         server=TRUE)
    
    gVars$empty_prestored_voc_tables <-  choices_options
    gVars$safe <- reactiveVal(safe)
    gVars$outsider <- reactiveVal(outsider)
    gVars$enrichments <- reactiveVal(enrichments)
    gVars$potential_new_entries_report <-dplyr::bind_rows(safe,outsider,enrichments) %>% 
      rowid_to_column(.,"ID") %>%
      add_column(., Acceptor=NA, .after=ncol(.)) %>% 
      add_column(., Decision=NA, .after=ncol(.)) %>%
      setNames(.,c("ID","Label_type_of_storing","Label","Label_synonym","Content_type_of_storing","Content","Content_synonym","Curator","Arbiter","Decision"))  %>%
      .[,c(1:7,10,8:9)]
    
    
    
    if(!is.null(gVars$recap_elaborated_voc_rows)){
      gVars$potential_new_entries_report <- rbind(gVars$recap_elaborated_voc_rows, gVars$potential_new_entries_report) %>%
                         .[order(.$ID),]
      gVars$potential_new_entries_report$ID <- 1:length(gVars$potential_new_entries_report$ID)
    }
    
  })
  
  
  
  
  
  
  
  trigerg <- reactive({
    paste(input$outcomes_dictXcandidates, input$update_voc_button, input$update_To_review, input$update_To_discard)
  })
  
  
  
  observeEvent(trigerg() ,{
      req(input$outcomes_dictXcandidates)
    req(!is.null(gVars$dkeys))
    if (input$outcomes_dictXcandidates == "To overview"){
      table_toshow <- gVars$enrichments()
    } else if (input$outcomes_dictXcandidates == "Fast check"){
      table_toshow <- gVars$outsider()
    } else { # safe items
      table_toshow <- gVars$safe()
    }
    
   # req(plyr::empty(table_toshow))
    table_toshow %>% data.frame(.) %>%
             setNames(., c("Label type of storing", "Main dictionary label","Original Label","Content type of storing", "Main dictionary content","Original content","Curator"))
     
    #to build ghost cols to color words according to their presence in the vocabulary
    dict <- gVars$dkeys
    
    table_ncols <-ncol(table_toshow)-3      # number of cols of interest
    vect_table_cols <- c(2,3,5,6)      #position of the cols to show
    ghost_cols <- (table_ncols *2): (table_ncols*2+3)           #cols later to hide, but needed to store boolean of the presence entry_voc in dt
    extra_ghost_cols <- c(1,4,7,ghost_cols)                     #cols to hide later
    
    ttable_toshow_redux <- data.frame(table_toshow) %>% .[,vect_table_cols]   %>% t(.)  %>% data.frame(row.names=NULL) 
    
    tmp_lst <- as.list(ttable_toshow_redux)
    hit_terms <- lapply(seq_along(tmp_lst), function(k){#browser()
      d <- dict[dict$label == tmp_lst[[k]][1],] 
      list(c(tmp_lst[[k]][1] %in% d$label              ,                         
             tmp_lst[[k]][2]%in% d$lab_syn  ,            
             tmp_lst[[k]][3]%in% d$allowed_features  ,
             tmp_lst[[k]][4] %in% d$syn_features
      ))
    }) 
    hit_terms <-  t(bind_cols(hit_terms))# rbindlist(.)
    
    if(is_empty(hit_terms)){
      #remove the outcome
      print("dsfefe")
      gVars$empty_prestored_voc_tables <- gVars$empty_prestored_voc_tables[!gVars$empty_prestored_voc_tables %in% input$outcomes_dictXcandidates]
      # return()
    }
    if (is_empty(hit_terms)){#(is.null(dim(hit_terms)) & !is_empty(hit_terms)){
      hit_terms <- NULL#as.list(hit_terms)
    }
    table_toshow <- cbind(table_toshow,hit_terms) %>% data.frame(., check.names=FALSE, row.names = NULL)

    req(input$outcomes_dictXcandidates)
    input_tab <- table_toshow %>% dplyr::rename("Term"=2,"Term Synonyms"=3,"Instance"=5,"Instance Synonym"=6)

    output$dt_toshow <- DT::renderDataTable(
      DT::datatable(
        input_tab,  selection=list(mode="single", target ="row"),
        options=list(columnDefs=list(list(visible=FALSE, targets=extra_ghost_cols)))
      )  %>%
        formatStyle(columns=vect_table_cols,
                    valueColumns = ghost_cols,
                    color= styleEqual(c("0","1"),
                                      c("#DC3545","#04AA6D"))
        )
    )
    
    
    
    gVars$dt_temporary_cases_voc <- data.frame(table_toshow)
    
    
  })
  

  
  
  
  #retrieved the correspondent entry from the dictionary
  observeEvent(input$dt_toshow_rows_selected,{   #dt_toshow_cell_clicked
    
    shinyBS::updateButton(session, "update_To_review", disabled = FALSE)
    shinyBS::updateButton(session, "update_To_discard", disabled = FALSE)
    shinyBS::updateButton(session, "update_voc_button", disabled = FALSE)   

    dict <- gVars$dkeys
    selected_fromDT <- as.data.frame(gVars$dt_temporary_cases_voc[input$dt_toshow_rows_selected,])
    
    label_search_key <- as.character(ifelse(is.na(selected_fromDT$mainL), selected_fromDT$newL,selected_fromDT$mainL))
    
    retrieved_from_dict_row <- dict%>% .[.$label == label_search_key,] %>%
      group_by(label)%>%
      summarise_all(funs(paste(unique(.), collapse=","))) %>%
      setNames(.,c("Main Label","Label synonym","Main content", "Content synonym"))
    
    output$retrieved_from_dict <- DT::renderDataTable(
      retrieved_from_dict_row, selection="none",options =list(dom="t"))
  })
  
  
  # to provide condition for conditional panel showinfg dt_toshow
  output$for_retrieved_voc_entry <- reactive(is.null(input$dt_toshow_rows_selected))
  outputOptions(output, "for_retrieved_voc_entry", suspendWhenHidden = FALSE)
  
  
  #stores whether the entry was accepted in the final report file.
  observeEvent(input$update_voc_button,{
    shinyBS::updateButton(session, "update_To_review", disabled = TRUE)
    shinyBS::updateButton(session, "update_To_discard", disabled = TRUE)
    shinyBS::updateButton(session, "update_voc_button", style="success", icon=icon("hand-o-right"))
    
    full_df_entries <- gVars$potential_new_entries_report %>% data.frame(., row.names = NULL)
    new_entries <- gVars$dt_temporary_cases_voc[input$dt_toshow_rows_selected,-c(1,4,7,8:11)] #input$dt_toshow_rows_selected
    tmp_for_removal_new_entry <- gVars$dt_temporary_cases_voc[input$dt_toshow_rows_selected,-c(8:11)] 
    
    new_entries<-new_entries%>% 
      setNames(.,c("Label","Label_synonym","Content","Content_synonym")) 
    
    
    #retrieve the corresponding row index of the row containing all entries in the main entry table
    row_index <- which(Reduce(`&`, Map(`%in%`, full_df_entries[,c(3,4,6,7)], new_entries[1,])))
      
    accepted_how <- ifelse(input$outcomes_dictXcandidates == "Safe", "Accepted", "Newly Accepted")
    full_df_entries$Decision[row_index] <- accepted_how
    full_df_entries$Arbiter[row_index] <- gVars$username
    gVars$potential_new_entries_report <- full_df_entries
    
    #updates values, respectively for saving and restore session and for cleaning the original entries to process 
    gVars$recap_elaborated_voc_rows <- full_df_entries[!is.na(full_df_entries$Decision),]
    gVars$later_voc_upd_df  <- tmp_for_removal_new_entry %>% setNames(., colnames(gVars$later_voc_upd_df )) %>%
                    anti_join_if_possible(gVars$later_voc_upd_df ,.)
    
    #if GLPmode activated, stores the message and open the GLPcomment modal window
    req(!input$GLP_inactivation_mode)
    showModal(dataModal_VOC_comments(failed = FALSE))
  })
  
  
  
  
  #stores whether the entry will be sent to final issue report file .
  observeEvent(input$update_To_review,{
    shinyBS::updateButton(session, "update_voc_button", disabled = TRUE)   
    shinyBS::updateButton(session, "update_To_discard", disabled = TRUE)
    shinyBS::updateButton(session, "update_To_review", style="success", icon=icon("hand-o-right"))
    full_df_entries <- gVars$potential_new_entries_report
    new_entries <- gVars$dt_temporary_cases_voc[input$dt_toshow_rows_selected,-c(1,4,7,8:11)] #input$dt_toshow_rows_selected
    tmp_for_removal_new_entry <- gVars$dt_temporary_cases_voc[input$dt_toshow_rows_selected,-c(8:11)] 
    
    new_entries<-new_entries%>% 
      setNames(.,c("Label","Label_synonym","Content","Content_synonym"))
    
    
    #retrieve the corresponding row index of the row containing all entries in the main entry table
    row_index <- which(Reduce(`&`, Map(`%in%`, full_df_entries[,c(3,4,6,7)], new_entries[1,])))
           #full_df_entries[apply(full_df_entries[,c(3,4,6,7)], 1, function(r) setequal(r , new_entries[1,])),]$ID
    gVars$selected_row_index <- row_index
    
    full_df_entries$Decision[row_index] <- "Issue"
    full_df_entries$Arbiter[row_index] <- gVars$username
    gVars$potential_new_entries_report <- full_df_entries
    
    #updates values, respectively for saving and restore session and for cleaning the original entries to process 
    gVars$recap_elaborated_voc_rows <- full_df_entries[!is.na(full_df_entries$Decision),]
    gVars$later_voc_upd_df  <- tmp_for_removal_new_entry %>% setNames(., colnames(gVars$later_voc_upd_df )) %>%
      anti_join_if_possible(gVars$later_voc_upd_df ,.)
    
    #if GLPmode activated, stores the message and open the GLPcomment modal window
    req(!input$GLP_inactivation_mode)
    showModal(dataModal_VOC_comments(failed = FALSE))
    
  })
  

  #stores whether the entry will be marked as discarded in final  report file .
  observeEvent(input$update_To_discard,{
    shinyBS::updateButton(session, "update_voc_button", disabled = TRUE)   
    shinyBS::updateButton(session, "update_To_review", disabled = TRUE)
    shinyBS::updateButton(session, "update_To_discard", style="success", icon=icon("hand-o-right"))
    full_df_entries <- gVars$potential_new_entries_report
    new_entries <- gVars$dt_temporary_cases_voc[input$dt_toshow_rows_selected,-c(1,4,7,8:11)] #input$dt_toshow_rows_selected
    tmp_for_removal_new_entry <- gVars$dt_temporary_cases_voc[input$dt_toshow_rows_selected,-c(8:11)] 
    
    new_entries<-new_entries%>% 
      # mutate(newlabel = ifelse(!is.na(.$'Main dictionary label'),.$'Main dictionary label',.$'Current Label'),
      #         newlabsyn=ifelse(!is.na(.$'Main dictionary label'),.$'Current Label',.$'Main dictionary label') ) %>% 
      #  mutate(newcont = ifelse(!is.na(.$'Main dictionary content'),.$'Main dictionary content',.$'Current content'),
      #         newcontsyn=ifelse(!is.na(.$'Main dictionary content'),.$'Current content',.$'Main dictionary content') ) %>% data.frame(.) %>%.[,-c(1:4)] %>% 
      setNames(.,c("Label","Label_synonym","Content","Content_synonym"))
    
    
    #retrieve the corresponding row index of the row containing all entries in the main entry table
    row_index <- which(Reduce(`&`, Map(`%in%`, full_df_entries[,c(3,4,6,7)], new_entries[1,])))
              #full_df_entries[apply(full_df_entries[,c(3,4,6,7)], 1, function(r) setequal(r , new_entries[1,])),]$ID
    gVars$selected_row_index <- row_index
    
    full_df_entries$Decision[row_index] <- "Discard"
    full_df_entries$Arbiter[row_index] <- gVars$username
    
    gVars$potential_new_entries_report <- full_df_entries
    
    
    #updates values, respectively for saving and restore session and for cleaning the original entries to process 
    gVars$recap_elaborated_voc_rows <- full_df_entries[!is.na(full_df_entries$Decision),]
    gVars$later_voc_upd_df  <- tmp_for_removal_new_entry %>% setNames(., colnames(gVars$later_voc_upd_df )) %>%
      anti_join_if_possible(gVars$later_voc_upd_df ,.)
    
    
    #if GLPmode activated, stores the message and open the GLPcomment modal window
    req(!input$GLP_inactivation_mode)
    showModal(dataModal_VOC_comments(failed = FALSE))
    
  })
  
  
   
  
  
  # Return a modal dialog window with the possibility to input data. If 'failed' is TRUE (no written justifications in the inputbox), the user is informed about it.
  dataModal_VOC_comments <- function(failed = FALSE) {
    modalDialog(
      textAreaInput(inputId = "glp_comments_box", label = "GLP comments", width = "100%", rows=4,
                    placeholder = 'Please justify your last operation.'),
      shinyBS::bsButton("glp_store_commentVOC", label="Add", style="info", icon=icon("hand-point-right")),
      
      if (failed){
        div(br(),tags$b("In GLP mode, you must justify your decisions. Please do."), style = "color: red;")},
      
      footer =  modalButton("Cancel")
      
    )
  }
  
  
  
  # When Add button(glp_store_comment) is pressed, it tests if the textareainput box is not empty. 
  # If successful, remove the modal, otherwise it shows another modal with a failure message.
  observeEvent(input$glp_store_commentVOC, {
    row_index <- gVars$selected_row_index
    # Check that box for glp comments is filled
    if (input$glp_comments_box != "") {
      tabulated_comment <- paste0("\t","COMMENT:",input$glp_comments_box)
      list_element <- list(tabulated_comment, row_index)
      gVars$pt_glp_list <- c(gVars$pt_glp_list, list_element)
      removeModal()
      
      
    } else {
      showModal(dataModal_VOC_comments(failed = TRUE))
    }
    
  })
  
  
  
  # to provide condition for conditional panel showinfg dt_toshow
  output$left_outcomes <- reactive(input$outcomes_dictXcandidates %in% gVars$empty_prestored_voc_tables)
  outputOptions(output, "left_outcomes", suspendWhenHidden = FALSE)
  
  
  
  react_trigger_UPDvoc_and_issue <- reactive({
    paste(input$update_voc_button,input$update_To_review, input$update_To_discard)
  })
  
  
  #cleans the datatable as the rows are checked(both if accepted, or sent to issue)
  observeEvent(react_trigger_UPDvoc_and_issue(),{
    req(input$outcomes_dictXcandidates, input$dt_toshow_rows_selected)
    req(!is.null(gVars$dkeys))
 
    
    
    if (input$outcomes_dictXcandidates == "To overview"){
      tmptable<- gVars$enrichments()
      tmptable <- tmptable[-input$dt_toshow_rows_selected,]
      tmp_nrow <- nrow(tmptable)
      gVars$enrichments(tmptable)
    } else if (input$outcomes_dictXcandidates == "Fast check"){
      tmptable<- gVars$outsider()
      tmptable <- tmptable[-input$dt_toshow_rows_selected,]
      tmp_nrow<- nrow(tmptable)
      gVars$outsider(tmptable)
    } else { # safe items
      tmptable<-gVars$safe()
      tmptable<-tmptable[-input$dt_toshow_rows_selected,]
      tmp_nrow<- nrow(tmptable)
      gVars$safe(tmptable)
      
    }
    
   
    
    #updates the buttons
    shinyBS::updateButton(session, "update_voc_button", style="info")   
    shinyBS::updateButton(session, "update_To_review", style="info")
    shinyBS::updateButton(session, "update_To_discard", style="info", icon=icon("hand-o-right"))
     
    
    #cleans the selectizeinput from safe/to chekc/to overview once they are empty (=all entries were processed)
    if (tmp_nrow == 0){
      gVars$empty_prestored_voc_tables <- gVars$empty_prestored_voc_tables[!gVars$empty_prestored_voc_tables %in% input$outcomes_dictXcandidates]
      
      
      if (length(gVars$empty_prestored_voc_tables) != 0){
        updateSelectizeInput(session,
                             inputId="outcomes_dictXcandidates",
                             label="Potential outcome:",
                             choices=gVars$empty_prestored_voc_tables,#c("Safe","Fast check","To overview"),
                             selected=character(0),
                             options = list(
                               placeholder = 'Please select an option below',
                               onInitialize = I('function() { this.setValue(""); }')),
                             server=TRUE)
        
        
      } else{
        updateSelectizeInput(session,
                             inputId="outcomes_dictXcandidates",
                             label="Potential outcome:",
                             choices=NULL,
                             options = list(
                               placeholder = 'No more entries to process',
                               onInitialize = I('function() { this.setValue(""); }')),
                             server=TRUE)
        
      }
      
    }
  })
  
  
  
  
  
  observeEvent(input$generate_dict_updated_version,{      
    shinyBS::updateButton(session, "update_voc_button", label= " Upd",  style="success", icon=icon("check-circle")) 
    #  current structure of dt_temporary_cases_voc (1*11):
    # ID,"Label type of storing"   "Current Label" , "Main dictionary label",   "Content type of storing", "Current content",   "Main dictionary content",  "Status","Decision"  
    curr_dict <- gVars$dkeys   
    new_entries <- dplyr::filter(gVars$potential_new_entries_report, Decision=="Newly Accepted")[,c(3,4,6,7)] %>% 
                      setNames(., c("label","lab_syn", "allowed_features", "syn_features"))
    
    
    tmp_upd_vocabolary <- rbind(curr_dict,new_entries) %>% group_by(label, lab_syn,syn_features) %>% summarize_all(~paste(unique(na.omit(.)), collapse = ','))
    #I:current vocabulary, O: convert to an object collections::dict  (key:value) enriched by new terms
    new_OBJdict <- convert_df_to_dict(tmp_upd_vocabolary) 
    
    # I:enriched new object collections::dict    O: list of (compact_dict_to_display, extended_dict) 
    tmp_convert_dict_to_df <- convert_dict_to_df(new_OBJdict)
    gVars$compact_voc <- tmp_convert_dict_to_df[[1]]
    gVars$dkeys <- tmp_convert_dict_to_df[[2]]
     
  })#chus obs upd_voc
  
  
  
  
  output$pt_text_recod <- renderText({"<b>Procedural Track</b>"})
  output$pt_recod <- renderText({"<i><small>Waiting for action!</small></i>"})
  
  
  
  
   
  
  #YES VOCABULARY ENRICHMENT  
  #vocabulary and storage pattern sections
  pt_STOR_PATTreactors <- reactive({
    paste(input$ON_vocabulary_panel,input$save_recoding, input$reject_full_recoding ,input$store_labels, input$store_contents ) 
  })  #input$ON_vocabulary_panel
  
  
  
  
  
  
  
  
  
  
  
  sub_paragraph_storage_pattern <- eventReactive( pt_STOR_PATTreactors(), {
    req(input$recoded_label)
    ed_s <- paste0(paste0(" Edited Label: ",input$recoded_label,"\n"), 
                  paste0("Edited Content: ", input$recoded_content)) #tail(gVars$recod_ops_dataframe,n=1)[3])) #input$recoded_content))
    req(input$store_labels , input$store_contents )  
    
    #c(type_lab (ALLowed/syn), newlab, ALLrif_lab se syn, type_cont (ALLowed/syn), newcont, ALLrif_cont se syn)
    later_voc_upd_row <- data.frame(t(c(input$store_labels, input$recoded_label, input$label_TOclean_FORvocabulary, 
                                        input$store_contents, input$recoded_content, input$content_TOclean_FORvocabulary))) %>%
      setNames(.,c("typeL","mainL","newL","typeC","mainC","newC")) %>% mutate_all(na_if, "")

    
    req(!is.null(input$label_TOclean_FORvocabulary) & nzchar(input$label_TOclean_FORvocabulary))
    if (input$store_labels== 1){      # old label as synonym of the edited label
      if (input$store_contents ==3){
          s <- paste0(paste0 ("\t","'",input$label_TOclean_FORvocabulary, "' is temporarily stored as a 'Label Synonym' of the edited label. \n \t"),
                     paste0 ("'",input$content_TOclean_FORvocabulary, "' is temporarily stored as a 'Content Synonym' of the edited content.")) 
      }
      
      else if (input$store_contents ==4){        #  do not store contents
          s <- paste0(paste0 ("\t","'",input$label_TOclean_FORvocabulary, "' is temporarily stored as a 'Label Synonym' of the edited label. \n \t"),
                     paste0("The edited content is stored, while the original ('",input$content_TOclean_FORvocabulary ,"') was discarded."))
          later_voc_upd_row[,6] <- NA 
      } else {    # selected =5, active only when full column recoding was rejected
          ed_s <- paste0(paste0(" Edited Label: ",input$recoded_label,"\n"), 
                      paste0("Edited Content: the whole column was left unmodified.2"))
          s <- paste("\t","'",input$label_TOclean_FORvocabulary, "' is temporarily stored as a 'Label Synonym' of the edited label. \n \t") 
          later_voc_upd_row[,5:6] <- NA  
      }
      
    }
    
    
    
    
    
    
    else if (input$store_labels==2){       #do not store original label  
      if (input$store_contents ==3){
        s <-  paste0(paste0("\t","The edited label is stored, while the original ('",input$label_TOclean_FORvocabulary ,"') was discarded. \n \t"),
                   paste0("'",input$content_TOclean_FORvocabulary, "' is temporarily stored as a 'Content Synonym' of the edited content.") )
        later_voc_upd_row[,3] <- NA
      }
      
      else if (input$store_contents ==4){
        s <- paste0(paste0("\t","The edited label is stored, while the original ('",input$label_TOclean_FORvocabulary ,"') was discarded. \n \t"),
              paste0("The edited content is stored, while the original ('",input$content_TOclean_FORvocabulary ,"') was discarded."))
        later_voc_upd_row[,c(3,6)] <- NA
      } else { # selected =5, active only when full column recoding was rejected
        ed_s <- paste0(paste0(" Edited Label: ",input$recoded_label,"\n"), 
                      paste0("Edited Content: the whole column was left unmodified.1"))
        s <- paste0("\t","The edited label is stored, while the original ('",input$label_TOclean_FORvocabulary ,"') was discarded.")
        later_voc_upd_row[,c(3,5,6)] <- NA
      }
      
      
      
    }
     
    
    #simplify intermediate dict in case of identical values
    if (identical(later_voc_upd_row[2],later_voc_upd_row[3])){ later_voc_upd_row[3] <-NA}
    if (!is.na(later_voc_upd_row[5]) && identical(later_voc_upd_row[5],later_voc_upd_row[6])){ later_voc_upd_row[6] <-NA}
    
    
    gVars$later_voc_upd_row_tmp <- later_voc_upd_row
    #clean the row if needed: if 
    
    final_s <- paste(ed_s,"\n",s)
    final_s
    
    
    
    
    
  })
  
  
  
  observeEvent(input$reject_full_recoding, { 
    output$radiostoreCONT <- renderUI({
      radioButtons("store_contents", label = ("Contents"),
                   choices = list("Reject full column recoding"=5),#, "Reject whole column recoding" = 6), 
                   selected = 5)
    })
    })
    

  
  
  
  
  
  #updates the proedural track with what was done, (YES vocabulary enrichment in recoding part)
  observeEvent( pt_STOR_PATTreactors() ,{
    
    Value <-  sub_paragraph_storage_pattern()
    
    #prepares msg for pt box 
    Value <- Value %>% gsub(pattern = "\n", replacement = "<br/>", .)  %>%
      gsub(pattern = "\t", replacement = "&nbsp &nbsp", .) 
   # gVars$pt_glp_list <- c(gVars$pt_glp_list, Value)
    output$pt_recod <- renderText({Value})
    
  })
  
  observeEvent (input$store_button ,{ 
    Value <-  sub_paragraph_storage_pattern()
    
    #prepares msg for pt box 
    Value <- Value %>% gsub(pattern = "\n", replacement = "<br/>", .)  %>%
      gsub(pattern = "\t", replacement = "&nbsp &nbsp", .) 
      gVars$pt_glp_list <- c(gVars$pt_glp_list, Value)
    
    #updates buttons
    shinyBS::updateButton(session, "store_button", style="success", disabled=TRUE)
    shinyBS::updateButton(session,"undo_save_button", disabled = TRUE)
    shinyBS::updateButton(session,"undo_upd_button", disabled = FALSE)
    
    shinyBS::updateButton(session,"glp_pt_button_rec", disabled = FALSE)
    #gVars$modification_synthesis <- NULL

  })
  
  observeEvent(input$glp_pt_button_rec,{
    req(!is_empty( gVars$later_voc_upd_row_tmp),!is.null( gVars$later_voc_upd_row_tmp))
    #stores the ops
    gVars$later_voc_upd_df <- rbind(gVars$later_voc_upd_df, gVars$later_voc_upd_row_tmp)
  })
  
  
  
  
  #Activation/deactivation of all buttons and menus in recoding page
  # modification of buttons add/GLP according to the selection of GLP mode activation in sidebar
  observe({
    if (!input$GLP_inactivation_mode) {    #GLP enabled
      shinyBS::updateButton(session, "glp_pt_button_del", label=" GLP", style="info", icon=icon("hand-point-right"))
      shinyBS::addTooltip(session,"glp_pt_button_del", "Launch a graphical window, to enrich the procedural track info with comments according to GLP regulations.", placement="bottom")
      
      shinyBS::updateButton(session, "glp_pt_button_rec", label=" GLP", style="info", icon=icon("hand-point-right"))
      shinyBS::addTooltip(session,"glp_pt_button_rec", "Launch a graphical window, to enrich the procedural track info with comments according to GLP regulations.", placement="bottom")
      
    } else {
      shinyBS::updateButton(session, "glp_pt_button_del", label=" Add", style="info", icon=icon("hand-point-right"))
      shinyBS::removeTooltip(session,"glp_pt_button_del")
      
      shinyBS::updateButton(session, "glp_pt_button_rec", label=" Add", style="info", icon=icon("hand-point-right"))
      shinyBS::removeTooltip(session,"glp_pt_button_rec")
      
    }
  })
  
  
  
  #if rejected the full column recoding, it disables content radiooptions
  observeEvent(input$reject_full_recoding,{   
    shinyjs::disable(selector = "[type=radio][value =3 ]")
#    shinyjs::disable(selector = "[type=radio][value =4]")
    shinyjs::runjs("$('[type=radio][value =3]').parent().parent().addClass('disabled').css('opacity', 0.4)")
 #   shinyjs::runjs("$('[type=radio][value =4]').parent().parent().addClass('disabled').css('opacity', 0.4)")
   
  })

  
  
  
  
  
  observeEvent(input$glp_pt_button_rec, {
    
    disable("undo_upd_button")
    
    
    if(!input$GLP_inactivation_mode){    #such as GLPmode activated
      showModal(dataModal())
      
      observeEvent(input$glp_store_comment, {
        updateRadioButtons(session, "step_choice",    choices = c("Delete" = 1, "Modify and/or Save" = 2, "Specials" =3)  , selected = character(0), inline=TRUE)
        toggleModal(session, "modalstep", "close")
      })
      
      #output$candidate_label <- renderText({" "})
      
    } 
    
    
    else{
      #reupdate buttons and menus
      shinyBS::updateButton(session, "save_recoding", style="info", disabled=FALSE)
      shinyBS::updateButton(session, "reject_full_recoding", style="info", disabled=FALSE)
      shinyBS::updateButton(session, "undo_save_button", style="info", disabled=FALSE)
      
      output$radiostoreLAB <- renderUI({ 
        radioButtons("store_labels", label = ("Labels"),
                     choices = list("as Label Synonym" = 1, "Do not store"=2),#, "Do not store" = NA), 
                     selected = character(0))
      })
      
      
      output$radiostoreCONT <- renderUI({
        radioButtons("store_contents", label = ("Contents"),
                     choices = list("as Content Synonym" = 3, "Do not store"=4),#, "Reject whole column recoding" = 6), 
                     selected = character(0))
      })
      
      
      shinyBS::updateButton(session, "store_button", style="info", disabled=TRUE)
      shinyBS::updateButton(session, "undo_upd_button", style="info", disabled=TRUE)
      
      
      updateRadioButtons(session, "step_choice",   choices= c("Delete" = 1, "Modify and/or Save" = 2, "Specials" =3) , selected = character(0), inline=TRUE)
      
      toggleModal(session, "modalstep", "close")
    }
    output$pt_recod <- renderText({"<i><small>Waiting for action!</small></i>"})
    
  }) 
  
  
  
  
  
  
  
  
  
  ##### section special ops
  
  
  
  
  
  #AGILENT
  output$pt_text_AGIsplit <- renderText({"<b>Procedural Track</b>"})
  output$pt_AGIsplit <- renderText({"<i><small>Waiting for action!</small></i>"})
  
  
  
  output$num_new_colsAGIL <- renderPrint({ input$new_colsAGIL })
  output$colname_to_showAGIL <- renderText(paste("Current column: <b>", input$pick_cols_recoding, "</b>"))
  output$col_to_showAGIL <-  DT::renderDataTable({
    table1 <- gVars$phTable[[1]] 
    table1<-data.frame(table1[,req(input$pick_cols_recoding)])
    colnames(table1) <-input$pick_cols_recoding
    DT::datatable( table1, selection="none",options =list( scrollX=TRUE,dom="t"))
    
})
    
    
    
  
  
  observeEvent(input$test_agil_button,{
    req(input$pick_cols_recoding)
    current_df <- gVars$phTable[[1]]
    tmp_selected_col <- current_df[,input$pick_cols_recoding]
    
    gsm = sapply(tmp_selected_col, function(x) substr(x,0,regexpr('_', x)[1]-1))
    slide = sapply(tmp_selected_col, function(x) substr(x,regexpr('_', x)[1] + 1, regexpr('_S',x)[1] - 1))
    array = sapply(tmp_selected_col, function(x) substr(x,regexpr('_[1-4]_', x)[1]+1, regexpr('_[1-4]\\.txt',x)[1]+1))
    gVars$agi_splitted_new_cols <- data.frame(gsm=gsm, slide=slide, array=array)
    
    updateButton(session,  "save_agil_button",disabled = FALSE)
    updateButton(session,  "test_agil_button",style="success",disabled = FALSE)
    updateButton(session,  "reset_agitest_table",disabled = FALSE)
    
    gVars$AGIsplit_msg <- paste0("Column '",input$pick_cols_recoding, "' was splitted into 'gsm', 'slide' and 'array'. ")
    output$pt_AGIsplit <- renderText({"..."})
  })
  
  
  
  
  output$table_AGI_splitted_cols <- renderDataTable({
    req(input$pick_cols_recoding)
    req(!is_empty(gVars$agi_splitted_new_cols))
    cols_toshow <- as.data.frame(gVars$agi_splitted_new_cols)
    DT::datatable(
      cols_toshow, selection="none",options =list(scrollX=TRUE,dom="t"))
    
  })
  
  
  
  #reset test table
  observeEvent(input$reset_agitest_table,{
    output$table_AGI_splitted_cols <-NULL
    updateButton(session,  "reset_agitest_table",disabled = TRUE)
    updateButton(session,  "save_agil_button", style="info", disabled = TRUE)
    updateButton(session,  "test_agil_button",style="info",disabled = FALSE)
    
  })
  
  
  
  
  observeEvent(input$save_agil_button,{
    req(input$pick_cols_recoding)
  gVars$phTable[[1]] <- cbind(gVars$phTable[[1]],gVars$agi_splitted_new_cols)
  
  #storing stage for undostep
  special_agiSPL <- list( input$pick_cols_recoding,colnames(gVars$agi_splitted_new_cols) )
  gVars$special_op_list <- c(gVars$special_op_list, (list(special_agiSPL =special_agiSPL)))
  gVars$last5list <- c(gVars$last5list, (list(special_agiSPL =special_agiSPL)))
  len2 <- length(gVars$last5list)    #check to maintain a 5 actions potential undo
  if (len2>=6)     {gVars$last5list[1] <- NULL}  
  
  #stores colnames processed - meglio di no perche la elabora dopo con le altre 
  #gVars$processedLIST2REC <- c(gVars$processedLIST2REC, input$pick_cols_recoding)
  
  
  value <- gVars$AGIsplit_msg
      updateButton(session,  "glp_pt_button_AGIsplit",disabled = FALSE)
      updateButton(session,  "save_agil_button", style="success", disabled = TRUE)
      updateButton(session,  "test_agil_button",style="info",disabled = TRUE)
      updateButton(session,  "reset_agitest_table",disabled = TRUE)
      updateButton(session,  "undo_agil_button",disabled = FALSE)
  
  output$pt_AGIsplit <- renderText({value})
              
  })
  
  
  
  observeEvent(input$glp_pt_button_AGIsplit,{
   
    gVars$pt_glp_list <- c(gVars$pt_glp_list, gVars$AGIsplit_msg) 
    
    updateButton(session,  "glp_pt_button_split",disabled = TRUE)
    updateButton(session,  "test_agil_button",disabled = FALSE)
    updateButton(session,  "save_agil_button", style="info", disabled = TRUE)
    updateButton(session,  "glp_pt_button_AGIsplit", style="info", disabled = TRUE)
    
    output$pt_AGIsplit <- renderText({"<i><small>Waiting for action!</small></i>"})
    updateRadioButtons(session, "step_choice",    choices= c("Delete" = 1, "Modify and/or Save" = 2, "Specials" =3) , selected = character(0), inline=TRUE)
  
    
    if(!input$GLP_inactivation_mode){    #such as GLPmode activated
      showModal(dataModal())
    }
  })
  
  
  
  observeEvent(input$undo_agil_button,{
    updateButton(session,  "undo_agil_button",disabled = TRUE)
    #cleaning of the tables
    output$col_to_showAGIL<-NULL
    output$table_AGI_splitted_cols<-NULL
    
    shinyBS::toggleModal(session, "SpecialOPmodal", toggle="close")
  })
  
  
  
  
  
  
  
  
  
  
  # ADDING EMPTY COLUMNS
  #output$ <- numericInput(inputId = "new_cols", label= HTML("Column(s) to add"), value=0))),
  
  output$pt_text_addEmpty <- renderText({"<b>Procedural Track</b>"})
  output$pt_addEmpty <- renderText({"<i><small>Waiting for action!</small></i>"})
  
  
  
  observe({
    if(input$new_cols != 0 | input$new_cols != "" | is.na(input$new_cols)){
      output$pt_addEmpty <- renderText({"..."})
    }  
  })
  
  
  
  
  
  # add the cols
  observeEvent(input$add_Fempty_cols_button,{
    req(!is.null(gVars$phTable))
    req(!is.null(input$names_added_cols))
    req(!is_empty(gVars$phTable))
    req(input$new_cols > 0)
    req(any(nzchar(input$names_added_cols)))
    
    new_names <- unlist(strsplit(input$names_added_cols,","))
    
    #checks if new colnames are equal to new columns
    tmp_len_str <-length(new_names)
    if(tmp_len_str != input$new_cols){
        shinyjs::info("The number of newly added column(s) and entered column names do not match. Please check!")
        return(NULL)
      }
    
    #adds the cols  
    new_names <- unlist(strsplit(input$names_added_cols,","))
    current_df <- gVars$phTable[[1]]
    col_cur_df <- colnames(current_df)
    if(any(new_names %in% col_cur_df)){
      tmpname <- toString(new_names[which(new_names %in%col_cur_df )])
      shinyjs::info(paste("Please enter other column name(s). Column(s)",tmpname ,"is already present!"))
      return(NULL)
    }
    current_df[,new_names] <- NA
    gVars$phTable[[1]] <- current_df
    
    
    #storing stage for undostep
    special_addEMPTY <- list(new_names )
    gVars$special_op_list <- c(gVars$special_op_list, (list(special_addEMPTY =special_addEMPTY)))
    gVars$last5list <- c(gVars$last5list, (list(special_addEMPTY =special_addEMPTY)))
    len2 <- length(gVars$last5list)    #check to maintain a 5 actions potential undo
    if (len2>=6)     {gVars$last5list[1] <- NULL}  
    
    #for pt message
    gVars$addEmpty_msg <- paste(input$new_cols, "new column(s) was/were added and named", toString(new_names),".")
    output$pt_addEmpty <- renderText({gVars$addEmpty_msg})
    
    updateButton(session,  "add_Fempty_cols_button", style="success",disabled = TRUE)
    updateButton(session,  "glp_pt_button_addEmpty",disabled = FALSE)
  })
  
  
  
  observeEvent(input$glp_pt_button_addEmpty,{
    gVars$pt_glp_list <- c(gVars$pt_glp_list, gVars$addEmpty_msg) 

    output$pt_AGIsplit <- renderText({"<i><small>Waiting for action!</small></i>"})
    updateNumericInput(session,inputId = "new_cols", label= HTML("Column(s) to add"), min=0, value=0) 
    updateTextInput(session, inputId = "names_added_cols","Name new column(s)", value="")
    updateButton(session,  "add_Fempty_cols_button", style="info",disabled = FALSE)
    updateButton(session,  "undo_addEmpty", disabled = FALSE)
    updateRadioButtons(session, "step_choice",    choices = c("Delete" = 1, "Modify and/or Save" = 2, "Specials" =3) , selected = character(0), inline=TRUE)
    output$pt_addEmpty <- renderText({"<i><small>Waiting for action!</small></i>"})
    
    if(!input$GLP_inactivation_mode){    #such as GLPmode activated
      showModal(dataModal())
    }
  })
  
  
  observeEvent(input$undo_addEmpty,{
      updateButton(session,  "undo_addEmpty",disabled = TRUE)
      updateButton(session,  "add_Fempty_cols_button", style="info",disabled = FALSE)
      shinyBS::toggleModal(session, "SpecialOPmodal", toggle="close")
  })
  
  
  
  
  
  
  
  
  
  # SPLITTING FUNCTION
  output$value <- renderPrint({ input$type_of_splitter })
  
  
  observe({
    if (input$type_of_separator != ""){
      updateButton(session,  "test_separ",disabled = FALSE)
    }
  })
  
  
  #section splitting column #separator part
  observe({
    if(input$type_of_separator=="Other"){ 
      gVars$separatorChar <- input$other_sep
    
    }
  })
  
  observeEvent(input$type_of_separator, {
    gVars$separatorChar <- NULL
    if(input$type_of_separator=="Tab"){
      gVars$separatorChar <- "\t"
    } else if(input$type_of_separator=="Space"){
      gVars$separatorChar <- " "
    } else{
      gVars$separatorChar <- input$type_of_separator
    }
    
  })
  
  
  
  
  output$test_colname_to_showSPLIT <- renderText("Newly splitted columns")
  
  observeEvent(input$test_separ,{
    updateButton(session,  "reset_test_table",disabled = FALSE)
    updateButton(session,  "run_separ",disabled = FALSE)
    updateTextAreaInput(session, inputId="PT_store_split", "Procedural track", value = "...")
    
    output$test_splitted_cols <- renderDataTable({
      req(input$type_of_separator,gVars$sel_col_split)
      col_to_split <- gVars$sel_col_split
      test_table <- data.frame(gVars$phTable[[1]] [,col_to_split])
      colnames(test_table) <-col_to_split
      sep <- gVars$separatorChar
      test_table <- splitstackshape::cSplit(test_table, col_to_split, sep=sep)
      
      if(input$name_splitted_cols ==TRUE){
        new_colnames <- unlist(strsplit(input$names_split_cols, ","))
        if (length(new_colnames) == ncol(test_table)){
          colnames(test_table)<- new_colnames
        } else {
          shinyjs::info("The number of newly splitted columns and entered column names do not match. Please check!")
        }
      }
      
      gVars$dt_split = test_table 
      DT::datatable(
        test_table, selection="none",options =list(scrollX=TRUE))
      
    })
    
  })
  
  
  #reset test table
  observeEvent(input$reset_test_table,{
    output$test_splitted_cols <-NULL
    updateButton(session,  "reset_test_table",disabled = TRUE)
    
    
    updateButton(session,  "run_separ", style="info", disabled = TRUE)
    updateButton(session,  "test_separ",style="info",disabled=FALSE)
    
    
    
  })
  
  
  
  observeEvent(input$run_separ,{
    req(!is_empty(gVars$dt_split))
    updateButton(session,  "reset_test_table",disabled = TRUE)
    updateButton(session,  "run_separ",style="success", icon=icon("check-circle")) 
    updateButton(session,  "test_separ",disabled = TRUE)
    #gVars$dt_split
    gVars$phTable[[1]]<- cbind(gVars$phTable[[1]],gVars$dt_split)
    
    #storing stage for undostep
    special_SPL <- list(input$pick_cols_recoding, colnames(gVars$dt_split))
    gVars$special_op_list <- c(gVars$special_op_list, (list(special_SPL =special_SPL)))
    gVars$last5list <- c(gVars$last5list, (list(special_SPL =special_SPL)))
    len2 <- length(gVars$last5list)    #check to maintain a 5 actions potential undo
    if (len2>=6)     {gVars$last5list[1] <- NULL}  
  })
  
  
  
  #enable/disable buttons and remove shown splitted columns if the "other" separator box is cleaned
  observe({
    if(input$other_sep==""){
      output$test_splitted_cols <-NULL
      updateButton(session,  "test_separ",disabled = TRUE)
    } else{
      updateButton(session,  "test_separ",disabled = FALSE)
    }
  })
  
  
  
  #enable/disable buttons test/separate if the "name splitted columns" checkbox(name_splitted_cols) is selected
  observe({   
    if(input$name_splitted_cols==FALSE){    #no will to have the new columns renamed
      output$names_split_cols <-NULL
      updateButton(session,  "test_separ",disabled = TRUE)
      updateButton(session,  "run_separ",disabled = TRUE)
    } else{
      updateButton(session,  "test_separ",disabled = FALSE)
      updateButton(session,  "run_separ",disabled = FALSE)
    }
  })
  
  
  
  
  
  #section manual selection of another specific column clicked/unclicked
  observe({
    if(input$other_col_split == TRUE){
      df <- gVars$phTable[[1]] 
      #e come se restasse attivo other_col_split cioe se ho manual selection
      if(input$select_col_df_SPLIT == ""){
        updateSelectizeInput(session,inputId = "select_col_df_SPLIT",   choices=colnames(df), selected = character(0), #multiple = FALSE,
                             options = list(
                               placeholder= "columns from loaded dataframe", onInitialize = I('function() { this.setValue(""); }')),
                             server=TRUE)
      }
      gVars$sel_col_split <- input$select_col_df_SPLIT  
    } else{
      gVars$sel_col_split <- input$pick_cols_recoding
    }
    
    
    # section original column 
    output$colname_to_showSPLIT <- renderText(paste("Current column: <b>", gVars$sel_col_split, "</b>"))
    output$col_to_showSPLIT <- renderDataTable({
      table1 <- gVars$phTable[[1]]        
      table1<-data.frame(table1[,req(gVars$sel_col_split)])
      colnames(table1) <-gVars$sel_col_split
      table1
      DT::datatable(
        table1, selection="none",options =list(scrollX=TRUE))
    })
    
  })
  
  
  #if manual is ticked/unticked - resets the values downflow
  observeEvent(input$other_col_split,{
    
    updateRadioGroupButtons( session,
                             inputId = "type_of_splitter",
                             label = "Splitting method",
                             choices = c("Separator", "Regular Expression"),
                             selected=character(0),
                             status = "radioGROUPclass", 
                             checkIcon = list(
                               yes = icon("check-square"),
                               no = icon("square-o")
                             ))
    
    gVars$sel_col_split <-NULL   #remove the selected shown column table
  })
  
  
  ##storing in procedural track - splitting column section
  sentence1 <- reactive({
    input$run_separ
    input$run_separ_regex
    req(!is_empty(gVars$sel_col_split))
    if(!is.null(gVars$sel_col_split)){
      num_spl_cols <- ncol(gVars$dt_split)
      return( paste0("Column '",gVars$sel_col_split, "' is splitted into ", num_spl_cols, " columns" ))
      
    } else return(NULL)
    
    
  })
  
  Value_split <- reactive({
    input$run_separ
    input$run_separ_regex
    req(input$type_of_splitter)
    if(input$type_of_splitter == "Regular Expression"){
      pt_step <- paste0(sentence1(), " named '", paste(input$names_split_cols_regex,collapse=","),"'" )
      divider <- input$regex_splitting
      sentence2 <- paste0("Splitting was performed via ", "'",divider,"' ", tolower(input$type_of_splitter),"." )  
    } else{  #normal separator
      pt_step <- paste0(sentence1(), " named '", paste( colnames(gVars$dt_split),collapse=","),"'" )
      divider <- ifelse(input$type_of_separator== "Other", input$other_sep,input$type_of_separator)
      sentence2 <- paste0("Splitting was performed via ", "'",divider,"' ", tolower(input$type_of_splitter),"." )  
    }
    return(paste0(pt_step,".","\n",sentence2))
    # get(Value_split)
    
    
  })
  
  
  
  
  pt_SPLIT_MESreactors <- reactive({
    paste(input$run_separ,
          input$run_separ_regex)})
  
  
  #updates the procedural track with what have been done
  # observeEvent(input$run_separ,{   
  observeEvent (pt_SPLIT_MESreactors(),{
    Value <- Value_split()
    updateTextAreaInput(session, inputId="PT_store_split", "Procedural track", value = Value)
    updateButton(session,  "glp_pt_button_split",disabled = FALSE)
    
    
  })    
  
  
  observeEvent(input$glp_pt_button_split,{
    req(input$type_of_splitter == "Separator")
    gVars$pt_glp_list <- c(gVars$pt_glp_list, input$PT_store_split) 
    updateCheckboxInput(session, "other_col_split", "Select manually another specific column", value = T)
    updateCheckboxInput(session, "other_col_split", "Select manually another specific column", value = FALSE)
    
    updateButton(session,  "glp_pt_button_split",disabled = TRUE)
    updateButton(session,  "reset_test_table",disabled = TRUE)
    updateButton(session,  "run_separ",disabled = TRUE,style="info", icon=icon("check-circle")) 
    updateButton(session,  "test_separ",disabled = TRUE)
    updateRadioButtons(session, "step_choice",    choices = c("Delete" = 1, "Modify and/or Save" = 2, "Specials" =3) , selected = character(0), inline=TRUE)
    
    output$test_splitted_cols <-NULL
    updateTextAreaInput(session, inputId="PT_store_split", "Procedural track", value = "Waiting for action!")
    updateButton(session,  "undo_norm_regex_split_button",disabled = FALSE)
    
    if(!input$GLP_inactivation_mode){    #such as GLPmode activated
      showModal(dataModal())
    }
  })
  
  
  
  #undo for both regex and normal splitting part
  observeEvent(input$undo_agil_button,{
    updateButton(session,  "undo_norm_regex_split_button",disabled = TRUE)
    
    #ckleaning of the tables
    output$test_splitted_cols <-NULL
    output$col_to_showSPLIT <-NULL
    
    shinyBS::toggleModal(session, "SpecialOPmodal", toggle="close")
  })
  
  
  
  #REGEXP
  observeEvent(input$test_separ_regex,{
    updateButton(session,  "reset_test_table_regex",disabled = FALSE)
    updateButton(session,  "run_separ_regex",disabled = FALSE)
    updateTextAreaInput(session, inputId="PT_store_split", "Procedural track", value = "...")
    
    output$test_splitted_cols <- renderDataTable({
      req(!is_empty(input$regex_splitting))
      col_to_split <- gVars$sel_col_split
      test_table <- data.frame(gVars$phTable[[1]] [,col_to_split])
      colnames(test_table) <-col_to_split
      newcolnames <- unlist(strsplit(input$names_split_cols_regex, ","))
      sep <- input$regex_splitting
      
      test_table <- tidyr::extract(test_table, col_to_split, into=newcolnames, sep)
      
      
      gVars$dt_split <- test_table 
      DT::datatable(
        test_table, selection="none",options =list(scrollX=TRUE))
      
    })
    
  })
  
  
  
  #reset test table
  observeEvent(input$reset_test_table_regex,{
    output$test_splitted_cols <-NULL
    updateButton(session,  "reset_test_table_regex",disabled = TRUE)
    
    
    updateButton(session,  "run_separ_regex", style="info", disabled = TRUE)
    updateButton(session,  "test_separ_regex",style="info",disabled=FALSE)
  })
  
  
  
  
  observeEvent(input$run_separ_regex,{
    req(!is_empty(gVars$dt_split))
    updateButton(session,  "reset_test_table_regex",disabled = TRUE)
    updateButton(session,  "run_separ_regex",style="success", icon=icon("check-circle")) 
    updateButton(session,  "test_separ_regex",disabled = TRUE)
    gVars$phTable[[1]]<- cbind(gVars$phTable[[1]],gVars$dt_split)
    
    #storing stage for undostep
    special_SPLREG <- list(input$pick_cols_recoding,colnames(gVars$dt_split))
    gVars$special_op_list <- c(gVars$special_op_list, (list(special_SPLREG =special_SPLREG)))
    gVars$last5list <- c(gVars$last5list, (list(special_SPLREG =special_SPLREG)))
    len2 <- length(gVars$last5list)    #check to maintain a 5 actions potential undo
    if (len2>=6)     {gVars$last5list[1] <- NULL}  
  })
  
  
  
  
  #enable/disable buttons and remove shown splitted columns if the "other" separator box is cleaned
  observe({
    if(input$other_sep==""){
      output$test_splitted_cols <-NULL
      updateButton(session,  "test_separ_regex",disabled = TRUE)
    } else{
      updateButton(session,  "test_separ_regex",disabled = FALSE)
    }
  })
  
  
  
  #enable/disable buttons test/separate if the "name splitted columns" checkbox(name_splitted_cols) is selected
  observe({   
    
    if(input$names_split_cols_regex==""){    #no  new columns named
      output$names_split_cols_regex <-NULL
      updateButton(session,  "test_separ_regex",disabled = TRUE)
      updateButton(session,  "run_separ_regex",disabled = TRUE)
    } else{
      updateButton(session,  "test_separ_regex",disabled = FALSE)
      updateButton(session,  "run_separ_regex",disabled = FALSE)
    }
  })
  
  
  
  
  
  
  
  
  
  
  
  #section manual selection of another specific column clicked/unclicked
  observe({
    if(input$other_col_split == TRUE){
      df <- gVars$phTable[[1]] 
      if(input$select_col_df_SPLIT == ""){
        updateSelectizeInput(session,inputId = "select_col_df_SPLIT",   choices=colnames(df), selected = character(0), #multiple = FALSE,
                             options = list(
                               placeholder= "columns from loaded dataframe", onInitialize = I('function() { this.setValue(""); }')),
                             server=TRUE)
      }
      gVars$sel_col_split <- input$select_col_df_SPLIT  
    } else{
      gVars$sel_col_split <- input$pick_cols_recoding
    }
    
    
    # section original column 
    output$colname_to_showSPLIT <- renderText(paste("Current column: <b>", gVars$sel_col_split, "</b>"))
    output$col_to_showSPLIT <- renderDataTable({
      table1 <- gVars$phTable[[1]]        
      table1<-data.frame(table1[,req(gVars$sel_col_split)])
      colnames(table1) <-gVars$sel_col_split
      
      DT::datatable(
        table1, selection="none",options =list(scrollX=TRUE))
    })
    
  })
  
  
  
  
  observeEvent(input$glp_pt_button_split,{
    req(input$type_of_splitter == "Regular Expression")
    gVars$pt_glp_list <- c(gVars$pt_glp_list, input$PT_store_split) 
    updateCheckboxInput(session, "other_col_split", "Select manually another specific column", value = TRUE)
    updateCheckboxInput(session, "other_col_split", "Select manually another specific column", value = FALSE)
    
    updateButton(session,  "glp_pt_button_split",disabled = TRUE)
    updateButton(session,  "reset_test_table_regex",disabled = TRUE)
    updateButton(session,  "run_separ_regex",disabled = TRUE,style="info", icon=icon("check-circle")) 
    updateButton(session,  "test_separ_regex",disabled = TRUE)
    output$test_splitted_cols <-NULL
    updateRadioButtons(session, "step_choice",    choices = c("Delete" = 1, "Modify and/or Save" = 2, "Specials" =3)  , selected = character(0), inline=TRUE)
    updateTextAreaInput(session, inputId="PT_store_split", "Procedural track", value = "Waiting for action!")
    
    if(!input$GLP_inactivation_mode){    #such as GLPmode activated
      showModal(dataModal())
    }
  })
  
  
  observe({
    if (!input$GLP_inactivation_mode) {    #GLP enabled
      shinyBS::updateButton(session, "glp_pt_button_split", label=" GLP", style="info", icon=icon("hand-point-right"))
      shinyBS::addTooltip(session,"glp_pt_button_split", "Launch a graphical window, to enrich the procedural track info with comments according to GLP regulations.", placement="bottom")
    } else {
      shinyBS::updateButton(session, "glp_pt_button_split", label=" Add", style="info", icon=icon("hand-point-right"))
      shinyBS::removeTooltip(session,"glp_pt_button_split") 
    }
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ###TABITEM, section donwloads phenodata, vocabulary and GLP
  
  output$upd_phenodf <- renderDT(
    gVars$phTable[[1]] , options = list(scrollX = TRUE))
  
  
  
  #enable upload button if PT_GLPTab is selected and there are ops to show.
  observe({
    req(input$display=="PT_GLPTab")
    if(length(gVars$pt_glp_list) != 0){ 
      enable("update_glp_button")
    } else  {    #empty list of ops/comment 
      disable("update_glp_button")
    }
  })
  
  
  
  
  
  
  
  observeEvent(input$update_glp_button, {
    
    tmpcolname<- ifelse (!input$GLP_inactivation_mode, "operation_and_comment", "only_operation") 
    report_pt_glp <- gVars$pt_glp_list 
    if (!"list" %in% class(report_pt_glp)){
      gVars$pt_glp_list <- as.list(report_pt_glp)
    }
    gVars$glp_comment_rep <- as.data.frame(report_pt_glp) %>% do.call(rbind.data.frame, .)  %>% 
      setNames(.,tmpcolname ) %>%    cbind(nstep=row.names(.), .)
    
  })
  
  output$glp_report <- renderDT(
    gVars$glp_comment_rep  ,escape=F, options = list(scrollX = TRUE))
  
  
  #activates button only if the table is updated to the last performed operation
  output$activate_download <- renderUI({
    req(!is.null(gVars$pt_glp_list), !is.null(gVars$glp_comment_rep))
    if(nrow(gVars$glp_comment_rep) == length(gVars$pt_glp_list)) {
                    downloadButton("download_Upd_GLP", "Download report")
    }
  })
  
  
  
  
  
  # download all single reports
  # update vocabulary section
  observe({
    req(!is.null(gVars$potential_new_entries_report))
    issue <- gVars$potential_new_entries_report[gVars$potential_new_entries_report$Decision %in% c("Issue"),]
    discard <- gVars$potential_new_entries_report[gVars$potential_new_entries_report$Decision %in% c("Discard"),]
    accept <- gVars$potential_new_entries_report[gVars$potential_new_entries_report$Decision %in% c("Accepted", "Newly Accepted"),]
    if (dim(issue)[1] == 0) { shinyjs::disable("download_to_review_issue")} else {shinyjs::enable("download_to_review_issue")}
    if (dim(discard)[1] == 0) { shinyjs::disable("download_to_review_discard")} else {shinyjs::enable("download_to_review_discard")}
    if (dim(accept)[1] == 0) { 
            shinyjs::disable("download_to_review_accepted")
            shinyjs::disable("download_Upd_Vocabulary")
    } else {
        shinyjs::enable("download_to_review_accepted")
        shinyjs::enable("download_Upd_Vocabulary")
      }
    
  })
  
  #issue
  output$download_to_review_issue <- downloadHandler(
    filename = function() {
      file = paste0("issue_toreview_vocabulary_v",
                    format(Sys.time(), "%Y-%m-%d_%H-%M-%S"),
                    ".xlsx", 
                    sep = "")
    },
    content = function(file) {
      shiny::validate(
        need(!is.null(gVars$potential_new_entries_report), "No new entries evaluated Provided!")
      )
      
      shinyjs::html(id="loadingText", "Selecting Issue Vocabulary entries")
      shinyjs::show(id="loading-content")
      on.exit({
        shinyjs::hide(id="loading-content", anim=TRUE, animType="fade")    
      })
      
      for_report <- gVars$potential_new_entries_report
      issue_file <- data.frame(for_report) %>% .[.$Decision %in% c("Issue"),]
      
      write.xlsx(issue_file, file, row.names=FALSE)
      print("Issue list stored!")
      
    }
  )
  
  
  
  #discard
  output$download_to_review_discard <- downloadHandler(
    filename = function() {
      file = paste0("discard_toreview_vocabulary_v",
                    format(Sys.time(), "%Y-%m-%d_%H-%M-%S"),
                    ".xlsx", 
                    sep = "")
    },
    content = function(file) {
      shiny::validate(
        need(!is.null(gVars$potential_new_entries_report), "No new entries evaluated Provided!")
      )
      
      shinyjs::html(id="loadingText", "Selecting Discarded Vocabulary entries")
      shinyjs::show(id="loading-content")
      on.exit({
        shinyjs::hide(id="loading-content", anim=TRUE, animType="fade")    
      })
      
      for_report <- gVars$potential_new_entries_report
      discard_file <- data.frame(for_report) %>% .[.$Decision %in% c("Discard"),]
      
      write.xlsx(discard_file, file, row.names=FALSE)
      print("Discard list stored!")
      
    }
  )
  
  
  #accepted
  output$download_to_review_accepted <- downloadHandler(
    filename = function() {
      file = paste0("accepted_toreview_vocabulary_v",
                    format(Sys.time(), "%Y-%m-%d_%H-%M-%S"),
                    ".xlsx", 
                    sep = "")
    },
    content = function(file) {
      shiny::validate(
        need(!is.null(gVars$potential_new_entries_report), "No new entries evaluated Provided!")
      )
      
      shinyjs::html(id="loadingText", "Selecting Accepted Vocabulary entries")
      shinyjs::show(id="loading-content")
      on.exit({
        shinyjs::hide(id="loading-content", anim=TRUE, animType="fade")    
      })
      
      for_report <- gVars$potential_new_entries_report
      accepted_file <- data.frame(for_report) %>% .[.$Decision %in% c("Accepted", "Newly Accepted"),]
      
      write.xlsx(accepted_file, file, row.names=FALSE)
      print("Accepted list stored!")
      
    }
  )
  
  
  #updated vocabulary
  output$download_Upd_Vocabulary <- downloadHandler(
    filename = function() {
      file = paste0("updated_vocabulary_v",
                    format(Sys.time(), "%Y-%m-%d_%H-%M-%S"),
                    ".xlsx", 
                    sep = "")
    },
    content = function(file) {
      shiny::validate(
        need(!is.null(gVars$dkeys), "No Vocabulary File Provided!")
      )
      
      shinyjs::html(id="loadingText", "Updating Vocabulary")
      shinyjs::show(id="loading-content")
      on.exit({
        shinyjs::hide(id="loading-content", anim=TRUE, animType="fade")    
      })
      
      
      upd_voc <- data.frame(gVars$dkeys)
      
      write.xlsx(upd_voc,file, row.names=FALSE)
      print("Vocabulary updated!")
      
    }
  )
  
  
  
  
  
  #phenodata download
  observeEvent(input$update_df_button,{ 
    output$upd_phenodf <- renderDT(
      gVars$phTable[[1]] , options = list(scrollX = TRUE))
  })
  
  
  output$download_Upd_Phdata <- downloadHandler(
    filename = function() {
      file = paste0("updated_dataset_v",
                    format(Sys.time(), "%Y-%m-%d_%H-%M-%S"),
                    ".xlsx", 
                    sep = "")
    },
    content = function(file) {
      shiny::validate(
        need(!is.null(gVars$phTable), "No Phenodata File Provided!")
      )
      
      shinyjs::html(id="loadingText", "Updating dataset")
      shinyjs::show(id="loading-content")
      on.exit({ 
        shinyjs::hide(id="loading-content", anim=TRUE, animType="fade")    
      })
      
      
      upd_df <- gVars$phTable[[1]]
      row.names(upd_df) <-NULL
      
      write.xlsx(upd_df,file, row.names=FALSE)
      print("Dataset updated!")
      
    }
  )
  
  
  
  #GLP download 
  output$download_Upd_GLP <- downloadHandler(
    filename = function() {     
      
      
      current_df_file_name <- gVars$current_df_file_name %>% 
        sub('.[^.]*$', '', .) %>%
        paste(., collapse = "_")
      
      
      file = paste0("pt_GLPreport_",current_df_file_name, "_v",
                    format(Sys.time(), "%Y-%m-%d_%H-%M-%S"),
                    ".xlsx", 
                    sep = "")
    },
    content = function(file) {
      shiny::validate(
        need(!is.null(gVars$glp_comment_rep), "No procedural steps taken!")
      )
      
      shinyjs::html(id="loadingText", "Updating Procedural Report")
      shinyjs::show(id="loading-content")
      on.exit({
        shinyjs::hide(id="loading-content", anim=TRUE, animType="fade")    
      })
      
      for_report <- gVars$glp_comment_rep
      
      write.xlsx(for_report, file, row.names=FALSE)
      print("procedural report stored!")
      
    }
  )
  
   
  
  
  
  #multiple integration section
  #issue
  output$download_multi_issue <- downloadHandler(
    filename = function() {     
      
      
      current_df_file_name <- gVars$current_df_file_name %>% 
        sub('.[^.]*$', '', .) %>%
        paste(., collapse = "_")
      
      
      file = paste0("issue_multiple_integration",current_df_file_name, "_v",
                    format(Sys.time(), "%Y-%m-%d_%H-%M-%S"),
                    ".html", 
                    sep = "")
    },
    content = function(file) {
      shiny::validate(
        need(!is.null(gVars$multi_ISSmsg), "No entries classified as issues!")
      )
      
      shinyjs::html(id="loadingText", "Updating Issue Entries Report")
      shinyjs::show(id="loading-content")
      on.exit({
        shinyjs::hide(id="loading-content", anim=TRUE, animType="fade")    
      })
      
      tempReport <- paste0("./report/report_multi_issue.Rmd") 
      file.copy("report.Rmd", tempReport, overwrite = TRUE)
      
      # Set up parameters to pass to Rmd document
      params <- list(gVars=gVars, input=input, srcDir=srcDir)
       
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
      
      
      
      
     print("Issue entries report for multiple integration stored!")
      
    }
  )
  
  
  #consistent
  output$download_multi_consistent <- downloadHandler(
    filename = function() {     
      
      
      current_df_file_name <- gVars$current_df_file_name %>% 
        sub('.[^.]*$', '', .) %>%
        paste(., collapse = "_")
      
      
      file = paste0("consistent_multiple_integration",current_df_file_name, "_v",
                    format(Sys.time(), "%Y-%m-%d_%H-%M-%S"),
                    ".html", 
                    sep = "")
    },
    content = function(file) {
      shiny::validate(
        need(!is.null(gVars$multi_ACmsg), "No entries classified as consistent!")
      )
      
      shinyjs::html(id="loadingText", "Updating Consistent Entries Report")
      shinyjs::show(id="loading-content")
      on.exit({
        shinyjs::hide(id="loading-content", anim=TRUE, animType="fade")    
      })
      
      tempReport <- paste0("./report/report_multi_consistent.Rmd") 
      file.copy("report.Rmd", tempReport, overwrite = TRUE)
      
      # Set up parameters to pass to Rmd document
      params <- list(gVars=gVars, input=input, srcDir=srcDir)
      
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
      
      print("consistent entries report for multiple integration stored!")
      
    }
  )
  
  
  
  
  
  
  
  
  
  
  #modification overview section  
  total_modification_reactive <- reactive(  length(gVars$relab_op_list)+length(gVars$dupl_op_list) )  ####va aggiunta modif rec,del e reject full colum
  output$total_modifications <- renderText({paste0("A total of ", 
                                                   strong(total_modification_reactive()), " modifications were performed on this dataset.")
  })
  
  #relabelling phase part   
  #total relabelling(rejections included)
  total_relab_steps_reactive <- reactive(  (gVars$relab_op_list))
  output$total_relab_steps <- renderText({paste0("Total steps: ", 
                                                 strong(length(total_relab_steps_reactive()))) #map(mylist, ~.x$power)
  })
  #total relabelling (without rejections)
  output$only_relab_steps <- renderText({paste0("Only Relabelling steps: ", 
                                                strong(length(
                                                  total_relab_steps_reactive() [which(names(total_relab_steps_reactive())%in%("relab_AC"))] )))
  })
  
  output$only_relabRJ_steps <- renderText({paste0("Only Relabelling Rejection steps: ", 
                                                  strong(length(
                                                    total_relab_steps_reactive() [which(names(total_relab_steps_reactive())%in%("relab_RJ"))] )))
  })
  
  
  
  observeEvent(input$pick_relab_modif, {
    shinyjs::enable("undo_relab_ops") 
  })
  
  observeEvent(list(input$pick_relab_modif,total_relab_steps_reactive()),{
    req(input$pick_relab_modif,total_relab_steps_reactive())
    current_df <- gVars$phTable[[1]] 
    dictLAB <- gVars$dict_modifLAB
  
    if (input$pick_relab_modif == "Accepting relabelling proposal"){
      gvariable <-  c(list(as.character(sapply(gVars$delete_op_list, "[[", 1))),gVars$dict_DELrelated_ops)
      current_env <- new.env()
      updates_total_relab_steps <- find_and_update_previous_actionsLSTrelab_AC(gVars$relab_op_list, current_df, dictLAB,gvariable, current_env)
      gVars$relab_op_list <- updates_total_relab_steps
      
    } else{   #such as rejections of relabelling suggestions
      updates_total_relab_steps <- find_and_update_previous_actionsLSTrelab_REJ(gVars$relab_op_list, current_df, dictLAB)
      gVars$relab_op_list <- updates_total_relab_steps 
    }
    
    
    
    #total_relab_steps_reactive <- total_relab_steps_reactive(updates_total_relab_steps)
    candidates <- purrr::map(updates_total_relab_steps,1) 
    tosubstitute <- purrr::map(updates_total_relab_steps,2)
    #candidates <- purrr::map(total_relab_steps_reactive(),1) %>% find_and_update_previous_actionsLST(.,current_df, dictLAB)
    
    #tosubstitute <- purrr::map(total_relab_steps_reactive(),2)
    relab_ops_df <- data.frame(cbind(candidates,tosubstitute) ) %>% add_column(., tidyr::unite(., united,candidates,tosubstitute,sep=" --> "))
    gVars$relabelling_ops_dataframe <- relab_ops_df
        
    if (input$pick_relab_modif == "Accepting relabelling proposal"){
      for_selection <- relab_ops_df [grepl("relab_AC",rownames(relab_ops_df)),]
      
      
    } else{   #such as rejections of relabelling suggestions
      for_selection <- relab_ops_df [grepl("relab_RJ",rownames(relab_ops_df)),]
    }
    
    updateSelectizeInput(session,
                         inputId = "relab_ops_done",
                         choices = for_selection$united,
                         #multiple=FALSE,
                         selected=character(0),
                         options = list(
                           placeholder = 'Please select an option below',
                           onInitialize = I('function() { this.setValue(""); }')),
                         server=TRUE)
    
     
  })
  
  
  
  
  observeEvent(input$checkbox_show_relab_cols, {#list(input$checkbox_show_relab_cols, input$pick_relab_modif, input$relab_ops_done), {
    req(input$pick_relab_modif)
    req(input$relab_ops_done) 
    req(!is_empty(input$relab_ops_done))
    if (input$relab_ops_done == "" | input$pick_relab_modif == "") {
      return (paired=NULL)
      }else{
    req(input$pick_relab_modif)
    req(input$relab_ops_done) 
    req(!is_empty(input$relab_ops_done))
   # if (input$relab_ops_done == "") {return (paired=NULL)
    #  }else{
          labels_processed <- gVars$relabelling_ops_dataframe [gVars$relabelling_ops_dataframe$united==input$relab_ops_done,] 
          
          lab_new<-as.character(labels_processed[,1])
          lab_old <-as.character(labels_processed[,2])
          
          if(input$pick_relab_modif == 'Accepting relabelling proposal')
              {#it will show the table either the relabelling suggestion was aaccepted or rejected
                col_new <- gVars$phTable[[1]] [[lab_new]]
                col_old <- gVars$original_phTable[[1]][[lab_old]]
                paired <- data.frame(cbind(col_new,col_old)) %>% setNames(.,c(lab_new,lab_old))
                
                cn<-colnames(paired)
                sketch_for_relab = htmltools::withTags(table(
                  class = "display",
                  thead(
                    tr(
                      th(colspan = 1, "Current Label"),
                      th(colspan = 1, "Old Label to Restore") 
                    ),
                    tr(
                      lapply(cn,th)
                      
                    )
                  )
                ))
                
           } else {#rejection proposal
            paired <- data.frame( gVars$original_phTable[[1]][[lab_old]]) %>% setNames(.,lab_old)
            
            cn<-colnames(paired)
            sketch_for_relab = htmltools::withTags(table(
              class = "display",
              thead(
                tr(
                  th(colspan = 1, "Current Old Label",class="dt-center") 
                ),
                tr(
                  lapply(cn,th)
                  
                )
              )
            ))
           }                   
      }      
          output$operated_relab_cols_content <- DT::renderDataTable(
            DT::datatable( as.matrix(paired), container=sketch_for_relab, options= list(scrollX=TRUE, autowidth=T)
            ))
    
    
  })
  
  
  
  observeEvent(input$undo_relab_ops, {
    req(input$pick_relab_modif)
    req(!is_empty(input$relab_ops_done))
    
    labels_processed <- gVars$relabelling_ops_dataframe [gVars$relabelling_ops_dataframe$united==input$relab_ops_done,] 
  
    lab_new<-as.character(labels_processed[,1])
    lab_old <-as.character(labels_processed[,2])
    
    
    if (input$pick_relab_modif == "Accepting relabelling proposal"){
          relab_AC <- list(relab_AC=  list(lab_new, lab_old))
          
          undofunc_relabAC((relab_AC))
          gvariable <- c(list(relab=gVars$relab_op_list), list(dupl= gVars$dupl_op_list),list(rec_RJ= gVars$recodeRJ_op_list), list(rec_AC = gVars$recod_ops_dataframe))
          x<-update_ops_df(relab_AC, gvariable[4])
          gVars$recod_ops_dataframe <-x 
          
          tmp_item <- relab_AC
          gVars$pt_glp_list <- c(gVars$pt_glp_list, paste("UNDO:", gVars$undo_message))
          
    } else {   #rejection proposal
        relab_RJ <- list(relab_RJ=  list(lab_new, lab_old))
        undofunc_relabRJ((relab_RJ))
        tmp_item <- relab_RJ
        gVars$pt_glp_list <- c(gVars$pt_glp_list, paste("UNDO:", gVars$undo_message))
    }    
    
    
    if (!input$GLP_inactivation_mode) {    #GLP enabled
      req(!input$GLP_inactivation_mode)
      showModal(dataModal(failed = FALSE))
    }
    
    
     
     #check if the removed ops is in last5 stored ops and eventually remove it
      retrieved_index <- which(gVars$last5list %in% tmp_item )  
      if(!is_empty(retrieved_index) && retrieved_index != 0){gVars$last5list[[retrieved_index]]<-NULL}

     
        retrieved_index2 <- which(gVars$relab_op_list %in% tmp_item )
        if(!is_empty(retrieved_index2) && retrieved_index2 != 0){gVars$relab_op_list[[retrieved_index2]] <-NULL}
    
    
    shinyjs::disable("undo_relab_ops")
    updateCheckboxInput(session, "checkbox_show_relab_cols", value = FALSE)
    updateSelectizeInput(session,
                         inputId = "pick_relab_modif",
                         label="Type of modification:",
                         choices=c("Accepting relabelling proposal","Rejecting relabelling proposal"),
                         #multiple=FALSE,
                         selected=character(0),
                         options = list(
                           placeholder = 'Please select an option below',
                           onInitialize = I('function() { this.setValue(""); }')),
                         server=TRUE)
      
    
    
    
        
    })
  
  
   
  
  
  
  
  
  
  
  
  
  
  
        
  
  
  
  
  
  
  
  
  #duplicate removal phase part   
  #total duplicate removals 
  total_dupl_steps_reactive <- reactive(  (gVars$dupl_op_list))
  output$total_dupl_steps <- renderText({paste0("Total steps: ", 
                                                strong(length(total_dupl_steps_reactive()))) #map(mylist, ~.x$power)
  })
  
  
  
 
  
  
  observeEvent(total_dupl_steps_reactive(),{
    req(!is.null(total_dupl_steps_reactive()))
    dictLAB <- gVars$dict_modifLAB
    current_df <- gVars$phTable[[1]]
    update_total_dupl_steps <- find_and_update_dupl_groups(total_dupl_steps_reactive(), current_df, dictLAB)
    kept <- purrr::map(update_total_dupl_steps,1) 
    dupl_selected <- purrr::map(update_total_dupl_steps,2) %>% lapply(., function(k) paste(k, collapse=", "))
    
    dupl_ops_df <- dplyr::bind_rows(kept,dupl_selected) %>% t(.) %>% data.frame(., row.names = NULL) 
    if (dim(dupl_ops_df)[1]!= 0) {dupl_ops_df <- setNames(dupl_ops_df, c("kept","dupl_selected"))}
    
    gVars$duplicates_ops_dataframe <- dupl_ops_df
    
    updateSelectizeInput(session,
                         inputId = "dupl_ops_done",
                   label="Performed modification:",
                   choices = (dupl_ops_df$dupl_selected),
                  # multiple=FALSE,
                   selected=character(0),
                   options = list(
                     placeholder = 'Please select an option below',
                     onInitialize = I('function() { this.setValue(""); }')),
                  server=TRUE)
  })
  

  
  
  observeEvent(input$dupl_ops_done,{      #moddom list(input$dupl_ops_done,total_dupl_steps_reactive())
    req(input$dupl_ops_done)
    selected_group <- input$dupl_ops_done   #chosen group of duplicate in dropdown
    correspondent_kept <-  dplyr::filter(gVars$duplicates_ops_dataframe, dupl_selected == selected_group)$kept
    tmp_df <-  dplyr::filter(gVars$duplicates_ops_dataframe, dupl_selected == selected_group)
    correspondent_deleted <- unlist(strsplit(selected_group, ", ")) %>% .[!. %in% correspondent_kept]
    gVars$recap_dupl <- list(correspondent_kept,correspondent_deleted)
    
  })
  
  
  observeEvent(input$dupl_ops_done, {
    shinyjs::enable("undo_dupl_ops") 
  })
  
  
  
  
  
    
  output$dupl_kept_text <- renderText({"Duplicate kept"})
  output$kept <-renderText({
    req(input$dupl_ops_done)
    gVars$recap_dupl[[1]] })
  
  
  
  output$dupl_deleted_text <- renderText({"Duplicate deleted"})
  output$deleted <-renderText({
    req(input$dupl_ops_done)
    gVars$recap_dupl[[2]] })
  
  
  
  
  
  observeEvent(input$checkbox_show_dupl_cols, {#list(input$checkbox_show_relab_cols, input$pick_relab_modif, input$relab_ops_done), {
    req(input$dupl_ops_done) 
    if (input$dupl_ops_done == "") {
      return (paired=NULL)
    }else{
      req(input$dupl_ops_done) 
      req(!is_empty(input$dupl_ops_done))
      
      kept <- gVars$recap_dupl[[1]]
      deleted <- gVars$recap_dupl[[2]]
      
      
      paired <- data.frame(cbind(gVars$phTable[[1]][,kept],gVars$original_phTable[[1]][,deleted])) %>% 
                    setNames(.,c(kept,deleted))

        cn<-colnames(paired)
        ndel <- length(deleted)
        sketch_for_dupl = htmltools::withTags(table(
          class = "display",
          thead(
            tr(
              th(colspan = 1, "Current Column"),
              th(colspan = ndel, "Old Duplicate(s) to Restore",class=("dt-center")) 
            ),
            tr(
              lapply(cn,th)
              
            )
          )
        ))
        
      
    }      
    output$operated_dupl_cols_content <- DT::renderDataTable(
      DT::datatable( as.matrix(paired), container=sketch_for_dupl, options= list(scrollX=TRUE, autowidth=T)
      ))
    
    
  })
  
  
  
  
  
  
  observeEvent(input$undo_dupl_ops, {
    req(input$undo_dupl_ops)
    req(!is_empty(input$undo_dupl_ops))
    if (input$dupl_ops_done == "") {
      return ()
    }else{
    
      chosen_dupl <- gVars$recap_dupl[[1]]
      all_selected_dupl_lab <- c(gVars$recap_dupl[[1]],gVars$recap_dupl[[2]])
      
      
      
    dupl_AC <- list(dupl_AC=  list(chosen_dupl, all_selected_dupl_lab))
      undofunc_duplAC(dupl_AC, flag=FALSE)           #change flag is we put the option for restoring to stage
      tmp_item <- dupl_AC
    
    gVars$pt_glp_list <- c(gVars$pt_glp_list, paste("UNDO:", gVars$undo_message))
    
    if (!input$GLP_inactivation_mode) {    #GLP enabled
      req(!input$GLP_inactivation_mode)
      showModal(dataModal(failed = FALSE))
    }
    
   
      
    #check if the removed ops is in last5 stored ops and eventually remove it
    
    retrieved_index <- which(sapply(map(gVars$last5list,2), setequal, tmp_item[[1]][[2]] ) ) 
    if (length(retrieved_index)>1){return(NULL)}
    if(!is_empty(retrieved_index) && retrieved_index != 0){gVars$last5list[retrieved_index]<-NULL}
    
    
    retrieved_index2 <- as.numeric(which(sapply(map(gVars$dupl_op_list,2), setequal, tmp_item[[1]][[2]] ) ) )
    if(!is_empty(retrieved_index2) && retrieved_index2 != 0){gVars$dupl_op_list[retrieved_index2] <-NULL}
    
    
    shinyjs::disable("undo_dupl_ops")
    updateCheckboxInput(session, "input$checkbox_show_dupl_cols", value = FALSE)
    updateSelectizeInput(session,
                         inputId = "dupl_ops_done",
                        # choices=c("Accepting relabelling proposal","Rejecting relabelling proposal"),
                         #multiple=FALSE,
                         selected=NULL,
                         options = list(
                           placeholder = 'Please select an option below',
                           onInitialize = I('function() { this.setValue(""); }')),
                         server=TRUE)
    }
  })

  
  #recoding (accept) phase part   
  #total recoding(rejections of full content recoding included)
  
  
  total_only_recodAC_reactive <- reactive( 
    gVars$recod_ops_dataframe #%>% .[!(.$newcont %in% NA) | (!.$oldcont %in% NA),], 
    
  )
  
  
  total_only_recodRJ_reactive <- reactive( gVars$recodeRJ_op_list)
  
  total_only_deleted_reactive <- reactive( gVars$delete_op_list)
  
  
  
  
  output$total_recode_steps <- renderText({paste0("Total Recoding steps: ", 
                                                  strong(
                                                    nrow(total_only_recodAC_reactive())+length(total_only_recodRJ_reactive())+length(total_only_deleted_reactive())    
                                                  ))   
  })
  #total recoding (without rejections)
  output$only_recodAC_steps <- renderText({paste0("Only Full Recoding steps: ", 
                                                  strong(
                                                    nrow(total_only_recodAC_reactive() )  ))
    
    
  })
  
  
  
  #total recoding (with content rejections)
  output$only_recodRJ_steps <- renderText({paste0("Only full content recoding rejected steps: ", 
                                                  strong( length(
                                                    total_only_recodRJ_reactive()  )))
  })
  # total_relab_steps_reactive() [which(names(total_relab_steps_reactive())%in%("relab_AC"))] )))
  
  
  #total deleted columns (without any kind of modification)
  output$only_delete_steps <- renderText({paste0("Simple deletion steps: ", 
                                                 strong( length(
                                                   total_only_deleted_reactive()  ))) 
  })
  
  
  observe({
    if (input$pick_type_recode_modif == "Recoded column" && all(c(input$recode_ops_done_oldcont,input$recode_ops_done_newcont) != "") ){
      shinyBS::updateButton(session, "undo_recode_ops", label= "Undo", disabled = FALSE)
    } else if (input$pick_type_recode_modif == "Recoded but rejecting full recoding" && all(c(input$recode_ops_done_oldlab, input$recode_ops_done_newlab) != "") ) {
      shinyBS::updateButton(session, "undo_recode_ops", label= "Undo", disabled = FALSE)
    } else if (input$pick_type_recode_modif == "Deleted unmodified column" && (input$recode_ops_done_delete  != "") ) {
      shinyBS::updateButton(session, "undo_recode_ops", label= "Undo", disabled = FALSE)
    } 
    else {shinyBS::updateButton(session, "undo_recode_ops", label= "Undo", disabled = TRUE)}
  })
  
 
  
  observeEvent(list(input$pick_type_recode_modif, input$undo_recode_ops), {   
    # moddom forse uno dei 2 reactive, se capisci a cosa serve o quello totale, ce in relab
    #req(123)
   # req(nzchar(input$pick_type_recode_modif))
     req(input$pick_type_recode_modif)
    gVars$subset_df_recode_ops <- NULL 
    tmpsubset<-(rep(NA,4))
    current_df <- gVars$phTable[[1]]
    dictLAB <- gVars$dict_modifLAB
    dictCONT <- gVars$dict_modifCONT
    if (input$pick_type_recode_modif == "Deleted unmodified column"){
      
      tmpsubset <- map(total_only_deleted_reactive(),1) %>% unlist(.) 
      
      
      req(total_only_deleted_reactive())
      
      
      for_selectionDEL <- map(total_only_deleted_reactive(),1) %>% unlist(.) %>% as.vector() %>% c("",.)#%>%  setNames(., "deleted_cols") #unique(.) %>% 
      updateSelectizeInput(session,
                           inputId = "recode_ops_done_delete",
                           choices = for_selectionDEL,
                           #multiple=FALSE,
                           selected=character(0),
                           options = list(
                             placeholder = 'Please select an option below',
                             onInitialize = I('function() { this.setValue(""); }')),
                           server=TRUE)
      
      
      
      
      
      
    } else if (input$pick_type_recode_modif == "Recoded column"){   #such as recodings
      
      
      x<-gVars$KEGG_MAT 
      
      
      gvariable <-  c(list(as.character(sapply(gVars$delete_op_list, "[[", 1))), list(colnames(gVars$original_phTable[[1]])),gVars$dict_relab)
      updates_total_recodeACC_steps <- find_and_update_previous_actionsDFrecode_AC(gVars$recod_ops_dataframe, current_df, dictLAB,gvariable) 
      gVars$recod_ops_dataframe <- updates_total_recodeACC_steps
      
      
      tmpsubset <- total_only_recodAC_reactive()
    } else {   # columns whose recoding content was rejected
      
         if(!is.null(total_only_recodRJ_reactive())){
           gvariable <-  c(list(as.character(sapply(gVars$delete_op_list, "[[", 1))),gVars$dict_DELrelated_ops)
           #gvariable <-  c(list(gVars$delete_op_list),gVars$dict_DELrelated_ops)
           updates_total_recodeRJ_steps <- find_and_update_previous_actionsLSTrecode_RJ(gVars$recodeRJ_op_list, current_df, dictLAB,gvariable)
           gVars$recodeRJ_op_list <- updates_total_recodeRJ_steps
           
                
           tmpsubset<-t(rbindlist(list((purrr::map(total_only_recodRJ_reactive(),1) )) )) %>% 
            `rownames<-`(NULL) %>% data.frame(.) %>% 
            setNames(. , c("newlab","oldlab","newcont","oldcont")) %>% data.frame(.)
         } else {
             tmpsubset <-data.frame(t(tmpsubset)) %>% setNames(. , c("newlab","oldlab","newcont","oldcont")) %>% data.frame(.)
           }
    }
    
    gVars$subset_df_recode_ops <- reactive(tmpsubset)
  #  shinyBS::updateButton(session, "undo_recode_ops", label= "Undo", disabled=TRUE) 
    
    
  })
  
  
  
  
  
  observeEvent( input$undo_recode_ops,{   #list(input$recode_ops_done_delete, input$undo_recode_ops)
    req(input$pick_type_recode_modif == "Deleted unmodified column") 
    req(input$recode_ops_done_delete)
    
    #colname_to_restore <- input$recode_ops_done_delete
    deleted <- list(deleted=list( input$recode_ops_done_delete))
    undofunc_delete(deleted) 
    gVars$pt_glp_list <- c(gVars$pt_glp_list, paste("UNDO:", gVars$undo_message))
    #check if the removed ops is in last5 stored ops and eventually remove it
    retrieved_index <- which(gVars$last5list %in% deleted )  
    if(!is_empty(retrieved_index) && retrieved_index != 0){gVars$last5list[[ retrieved_index]]<-NULL}
    
    #delete from general op list
    retrieved_index2 <- which(gVars$delete_op_list %in% deleted )   
    if(!is_empty(retrieved_index2) && retrieved_index2 != 0){gVars$delete_op_list[[ retrieved_index2]]<-NULL}
    
    updateSelectizeInput(session,
                         inputId="pick_type_recode_modif",
                         label="Type of modification:",
                         choices=c("","Deleted unmodified column","Recoded column", "Recoded but rejecting full recoding"),
                         
                         selected=character(0),
                         options = list(
                           placeholder = 'Please select an option below',
                           onInitialize = I('function() { this.setValue(""); }')),
                         server=TRUE)
    
    
    
  })
  
  
  observeEvent( input$undo_recode_ops,{                     #list(input$recode_ops_done, input$undo_recode_ops)
    req(input$pick_type_recode_modif == "Recoded but rejecting full recoding") 
    req(input$recode_ops_done_newlab, input$recode_ops_done_oldlab)
    recode_RJ <- list(recode_RJ=list( c(input$recode_ops_done_newlab, input$recode_ops_done_oldlab,NA,NA)   ))
    undofunc_recodeRJ(recode_RJ) 
    gVars$pt_glp_list <- c(gVars$pt_glp_list, paste("UNDO:", gVars$undo_message))
    #check if the removed ops is in last5 stored ops and eventually remove it
    retrieved_index <- which(gVars$last5list %in% recode_RJ )  
    if(retrieved_index != 0){gVars$last5list[[ retrieved_index]]<-NULL}
    
    retrieved_index2 <- which(gVars$recodeRJ_op_list %in% recode_RJ )
    if(retrieved_index2 != 0){gVars$recodeRJ_op_list[[ retrieved_index2]]<-NULL}
    
    
    
    updateSelectizeInput(session,
                         inputId="pick_type_recode_modif",
                         label="Type of modification:",
                         choices=c("","Deleted unmodified column","Recoded column", "Recoded but rejecting full recoding"),
                         
                         selected=character(0),
                         options = list(
                           placeholder = 'Please select an option below',
                           onInitialize = I('function() { this.setValue(""); }')),
                         server=TRUE)
    
    
  })
  
  
  
  observeEvent( input$undo_recode_ops,{                         #list(input$recode_ops_done, input$undo_recode_ops)
    req(input$pick_type_recode_modif == "Recoded column") 
    req(input$recode_ops_done_newlab, input$recode_ops_done_oldlab,input$recode_ops_done_newcont,input$recode_ops_done_oldcont)
    #recode_AC <- list(gVars$recod_ops_dataframe )
    tmp_df <- data.frame(input$recode_ops_done_newlab, input$recode_ops_done_oldlab,input$recode_ops_done_newcont,input$recode_ops_done_oldcont)  %>%
      setNames(., c("newlab","oldlab","newcont","oldcont"))
    recode_AC <- list(recode_AC=list( tmp_df ))
    #check if the removed ops is in last5 stored ops and eventually remove it
    last5df<- map(gVars$last5list[1],1)[[1]]
    retrieved_index <- which(Reduce("&",Map("==",last5df, data.frame(recode_AC))))
    if(!is_empty(retrieved_index) && retrieved_index!=0){
      gVars$last5list[1]$recode_AC[[1]]<-  map(gVars$last5list[1],1)[[1]] [-retrieved_index,] 
    }
    #remove from the df of all recode_AC ops
    gVars$recod_ops_dataframe <- anti_join_if_possible(gVars$recod_ops_dataframe,tmp_df)
    
    undofunc_recodeAC(recode_AC) 
    #gVars$label_first_version$set(tmp_df$oldlab, c(tmp_df$oldlab, tmp_df$oldlab %in% colnames(gVars$original_phTable[[1]])) )
   # x<- c(tmp_df$oldlab, tmp_df$oldlab %in% colnames(gVars$original_phTable[[1]])) 
    
    gVars$pt_glp_list <- c(gVars$pt_glp_list, paste("UNDO:", gVars$undo_message))
    
    updateSelectizeInput(session,
                         inputId="pick_type_recode_modif",
                         label="Type of modification:",
                         choices=c("","Deleted unmodified column","Recoded column", "Recoded but rejecting full recoding"),
                         
                         selected=character(0),
                         options = list(
                           placeholder = 'Please select an option below',
                           onInitialize = I('function() { this.setValue(""); }')),
                         server=TRUE)
    
    
    updateSelectizeInput(session,
                         inputId = "recode_ops_done_oldlab",
                         #choices = for_selectionOL$oldlab, 
                         #multiple=FALSE,
                         selected=character(0),
                         options = list(
                           placeholder = 'Please select an option below',
                           onInitialize = I('function() { this.setValue(""); }')),
                         server=TRUE)
    
  
      updateSelectizeInput(session,
                        inputId= "recode_ops_done_newcont",
                        label="New content:",
                        selected=character(0),
                        options = list(
                          placeholder = 'Please select an option below',
                          onInitialize = I('function() { this.setValue(""); }')),
                        server=TRUE)
      
            updateSelectizeInput(session,
                          inputId= "recode_ops_done_oldcont",
                          label="Correspondent old content:",
                          selected=character(0),
                          options = list(
                            placeholder = 'Please select an option below',
                            onInitialize = I('function() { this.setValue(""); }')),
                          server=TRUE)
                      
         #12334   shinyBS::updateButton(session, "undo_recode_ops", label= "Undo") 
    
    
    #   else if(name_op %in% "deleted"){gVars$delete_op_list[[ which(gVars$delete_op_list %in% torestore)]] <-NULL}
    
  })
  
  
  
  observeEvent(list(input$pick_type_recode_modif,total_only_recodAC_reactive(), total_only_recodRJ_reactive()),{
    req(input$pick_type_recode_modif =="Recoded column" | input$pick_type_recode_modif =="Recoded but rejecting full recoding")
    for_selectionNL <- gVars$subset_df_recode_ops() %>% .[!duplicated(.),] # %>% data.frame
              # #%>% setNames(., c(newlab, oldlab, newcont, oldcont))#o distinct e valuta segg
    #o distinct e valuta segg
    
    
    updateSelectizeInput(session,
                         inputId = "recode_ops_done_newlab",
                         choices = for_selectionNL$newlab,#[[1]],
                         #multiple=FALSE,
                         
                         selected=NULL,
                         options = list(
                           placeholder = 'Please select an option below',
                           onInitialize = I('function() { this.setValue(""); }')),
                         server=TRUE)
    
  #  updateSelectizeInput(session,
  #                       inputId = "recode_ops_done_oldlab",
  #                       #choices = for_selectionOL[[2]],   #$oldlab, 
    #                       #multiple=FALSE,
    #                       selected=NULL,
    #                       options = list(
    #                       placeholder = 'Please select an option below',
    #                       onInitialize = I('function() { this.setValue(""); }')),
    #                     server=TRUE)
    
  })
  
  
  
  
  observeEvent(input$recode_ops_done_newlab,{
    req(nzchar(input$recode_ops_done_newlab))
    req(input$pick_type_recode_modif =="Recoded column" | input$pick_type_recode_modif =="Recoded but rejecting full recoding")
    data_ops <- gVars$subset_df_recode_ops ()
    for_selectionOL <- distinct(data_ops[data_ops$newlab == input$recode_ops_done_newlab,])
    updateSelectizeInput(session,
                         inputId = "recode_ops_done_oldlab",
                         choices = for_selectionOL$oldlab, 
                         #multiple=FALSE,
                         selected=NULL,
                         options = list(
                           placeholder = 'Please select an option below',
                           onInitialize = I('function() { this.setValue(""); }')),
                         server=TRUE)
    
    
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ### blocco new old cont - run only for fully recoding
  
  observeEvent(c(input$recode_ops_done_newlab,input$recode_ops_done_oldlab),{
    req(input$pick_type_recode_modif == "Recoded column")
    
    req(input$recode_ops_done_newlab, input$recode_ops_done_oldlab)
    data_ops <- gVars$subset_df_recode_ops()
    for_selectionALLC <- data_ops[data_ops$newlab == input$recode_ops_done_newlab & data_ops$oldlab == input$recode_ops_done_oldlab,] 
    
    updateSelectizeInput(session,
                         inputId = "recode_ops_done_newcont",
                         choices = for_selectionALLC$newcont, 
                         #multiple=FALSE,
                         selected=character(0),
                         options = list(
                           placeholder = 'Please select an option below',
                           onInitialize = I('function() { this.setValue(""); }')),
                         server=TRUE)
    
    
    updateSelectizeInput(session,
                         inputId = "recode_ops_done_oldcont",
                         choices = (for_selectionALLC$oldcont), 
                         #multiple=FALSE,
                         selected=character(0),
                         options = list(
                           placeholder = 'Please select an option below',
                           onInitialize = I('function() { this.setValue(""); }')),
                         server=TRUE)
    
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  #PLOTTING TOOL
  #checkboxGroupInput
  
  # populates the choices of abscissa box
  observeEvent(input$checkboxSINGLE_MULTIphdata, {
    req(input$checkboxSINGLE_MULTIphdata)
    if (input$checkboxSINGLE_MULTIphdata=="single"){   #loaded only single phenodata
      df <- gVars$phTable[[1]]
    } else{
      df <-gVars$multidf
    }
    gVars$plot_tmp <- colnames(df) 
    updateSelectizeInput(session,
                         inputId = "multi_colnamesX",
                         #label = "Select the column to recode you wish to display:",
                         choices = colnames(df),
                         selected = 1,
                         server=TRUE)
    
    
  })
  
  observe({
    x_class_to_show <-ifelse((input$multi_colnamesX)!="", paste("Data format: ", class(gVars$phTable[[1]][[input$multi_colnamesX]]) ), "Data format: -") 
    output$class_x <- renderText(HTML("<font size=-1.5>","<em>",x_class_to_show,"</em>","</font>"))
  })
  
  # populates the choices of ordinate box
  observeEvent(input$multi_colnamesX, {
    to_chose <- gVars$plot_tmp
    updateSelectizeInput(session,
                         inputId = "multi_colnamesY",
                         choices = c("",to_chose[!to_chose %in% input$multi_colnamesX]), 
                         selected = 1,
                         server=TRUE)
    
  })
  
  
  observe({
    y_class_to_show <-ifelse((input$multi_colnamesY)!="", paste("Data format: ", class(gVars$phTable[[1]][[input$multi_colnamesY]]) ), "Data format: -") 
    output$class_y <- renderText(HTML("<font size=-1.5>","<em>",y_class_to_show,"</em>","</font>"))
  })
  
  
  
  # populates the choices of condition box
  observeEvent(list(input$multi_colnamesX,input$multi_colnamesY), {
    req(input$multi_colnamesX,input$multi_colnamesY)
    to_chose <- gVars$plot_tmp
    updateSelectizeInput(session,
                         inputId = "multi_colnamesCOND",
                         choices = to_chose[!to_chose %in% c(input$multi_colnamesX ,input$multi_colnamesY)],
                         selected = 1,
                         server=TRUE)
    
  })
  
  observe({
    cond_class_to_show <-ifelse((input$multi_colnamesCOND)!="", paste("Data format: ", class(gVars$phTable[[1]][[input$multi_colnamesCOND]]) ), "Data format: -") 
    output$class_cond <- renderText(HTML("<font size=-1.5>","<em>",cond_class_to_show,"</em>","</font>"))
  })
  
  
  
  # Return a modal dialog window with the possibility to input data. If 'failed' is TRUE (no written justifications in the inputbox), the user is informed about it.
  dataModal_convert <- function(failed = FALSE,idcss = "" ) {
    modalDialog(
      tags$head(tags$style(
        type = 'text/css',
        '#test .modal-dialog  { width: fit-content !important; }'
      )),
      
      fluidPage(
        tags$strong(HTML("Convert column format")),
        fluidRow(
          box(  
            tags$label(em("X-axis")),
            div(style = "margin-top: -10px"),
            checkboxInput(inputId = "x_conversion", label = input$multi_colnamesX, value=FALSE ),
            div(style = "margin-top: -20px"),
            HTML("<font size=-5>", "&nbsp","&nbsp","&nbsp","&nbsp","&nbsp" ,"Current format: ", class(gVars$phTable[[1]][[input$multi_colnamesX]]),"</font>"),
            div(style = "margin-top: 12px"),
            
            
            tags$label(em("Y-axis")),
            div(style = "margin-top: -10px" ),
            checkboxInput(inputId = "y_conversion", label = input$multi_colnamesY, value=FALSE ),
            div(style = "margin-top: -20px"),
            HTML("<font size=-5>", "&nbsp","&nbsp","&nbsp","&nbsp","&nbsp" ,"Current format: ", class(gVars$phTable[[1]][[input$multi_colnamesY]]),"</font>"),
            div(style = "margin-top: 12px"),
            
            tags$label(em("Condition-axis")),
            div(style = "margin-top: -10px"),
            checkboxInput(inputId = "cond_conversion", label = input$multi_colnamesCOND, value=FALSE ),
            div(style = "margin-top: -20px"),
            HTML("<font size=-5>", "&nbsp","&nbsp", "&nbsp","&nbsp","&nbsp" ,"Current format: ", class(gVars$phTable[[1]][[input$multi_colnamesCOND]]),"</font>"),
            div(style = "margin-top: 12px"),
            
            br(),
            radioButtons(inputId = "conversion_options", label = "Convert to:", choices=list("Numeric","Character","Factor"), selected=character(0) ,inline=TRUE),
            
            br(),
            
            shinyBS::bsButton("convert_button", label="Convert", style="info", icon=icon("hand-point-right"))
          ),    
          
          box(fluidPage(
            shinyBS::bsButton("donotshowcol", label="Show listed columns", type = "toggle", value = TRUE, style="danger", icon=icon("exclamation-circle")),
            fluidRow(br()), 
            fluidRow(DT::DTOutput("converting_cols"))
          ))
          
          
        ), #fluidorw prima del primobox
        
        
      ))
  }
  
  
  
  
  
  observeEvent(input$convert_formats_button, {
    showModal(dataModal_convert(idcss = "test"))
 
  })
  
  
  observeEvent(input$donotshowcol, { 
    if (!input$donotshowcol){     #it shows columns
      if (input$checkboxSINGLE_MULTIphdata=="single"){   #loaded only single phenodata
        df <- gVars$phTable[[1]]
      } else{
        df <-gVars$multidf
      }
      to_convert <- c(input$x_conversion,input$y_conversion,input$cond_conversion)
      selected_cols <- c(input$multi_colnamesX,input$multi_colnamesY,input$multi_colnamesCOND)
      sel_cols_toConvert <-selected_cols[to_convert, drop=FALSE]
      output$converting_cols <- renderDT(data.frame(df[,sel_cols_toConvert]),options = list(scrollX = TRUE))
      shinyBS::updateButton(session, "donotshowcol", label= " Showing columns",  style="success", icon=icon("check-circle")) 
      
    }
    else {
      shinyBS::updateButton(session, "GLP_inactivation_mode", label=" GLP Mode Disabled",  style="danger", icon=icon("exclamation-circle"))
      shinyBS::updateCollapse(session, "bsSidebar1", close="STRUCTURE HOMOGENIZATION", style=list("GLP MODE"="warning","STRUCTURE HOMOGENIZATION"="warning"))
     
    }
    
  })
  
  
  
  
  
  
  observeEvent(input$plot_multipic,{
   # req(input$input$checkboxBAR_TILEplot)
    
    output$multiplot <- plotly::renderPlotly({ 
      req(input$checkboxSINGLE_MULTIphdata)
      
      if (input$checkboxSINGLE_MULTIphdata=="single"){   #loaded only single phenodata
        df <- gVars$phTable[[1]]
      } else{
        df <-gVars$multidf
      }
      
      #need of a df copy
      dfc <- df
      #renaming of chosen column to avoid issues in ggplot with weird chrs
      colnames(dfc)[which(names(dfc) == input$multi_colnamesX)] <- "x_var"
      colnames(dfc)[which(names(dfc) == input$multi_colnamesY)] <- "y_var"
      colnames(dfc)[which(names(dfc) == input$multi_colnamesCOND)] <- "cond_var"
      x_var <- dfc$x_var
      y_var <- dfc$y_var
      cond_var <- dfc$cond_var
      
      
      #fill parameter in barplot is set equal to the condition as default
      fill_var <- cond_var
      #1:stacked; 2grouped;3no condition is selected. In "no condition" the important is that position_var is not null, so for default is set to "dodge.."
      position_var <- ifelse (input$type_plot_relation == "stacked", "stack",  "dodge")    
      
      #if no condition is selected fill_var needs to be se to x axis value, 
      if (input$type_plot_relation == "nocond") {
        fill_var <- x_var
        
      }
      
      # values shown as percent
      if (input$type_value_barplot == TRUE) {
        position_var <- "fill"
      }
      
      
      
      
      
      #change position of the legend or remove it
      leg_pos <- ifelse(input$remove_legend == FALSE, "right", "none")
      
      
      #setting of axis and legend labelling
      xlab <- input$x_axis_label
      ylab <- input$y_axis_label
      #if no condition is set, the title of the legend will not be a condition but x axis var
      legend_lab <- ifelse(input$type_plot_relation == "nocond",input$x_axis_label,input$cond_legend_label)
      
      
      
      
      
      # plotting function
      if(input$checkboxBAR_TILEplot == "barplot") {
        pic<- ggplot(df, aes(fill=fill_var, y=y_var, x=x_var)) + 
          geom_bar(position=position_var, stat="identity")+
          xlab(xlab)+ ylab(ylab)+ 
          labs(fill=legend_lab)+
          theme(legend.position = leg_pos, #axis.title.x = element_blank(), 
                axis.text.x = element_text(angle = 60, hjust = 1))+ scale_x_discrete(labels = function(x) label_shortener(x, 22))  
      } else {    #tileplot
        pic<- ggplot(df, aes(fill=fill_var, y=y_var, x=x_var)) + 
                geom_tile()+
                xlab(xlab)+ ylab(ylab)+ 
                labs(fill=legend_lab)+  
                theme(legend.position = leg_pos, #axis.title.x = element_blank(), 
                      axis.text.x = element_text(angle = 60, hjust = 1))+ scale_x_discrete(labels = function(x) label_shortener(x, 22)) 
        }
               
    ggplotly(pic) 
     })
  })
  
  
  #it updates the label and legend labelling
  observe({
    updateTextInput(session, "x_axis_label",  value = input$multi_colnamesX)
    updateTextInput(session, "y_axis_label",  value = input$multi_colnamesY)
    updateTextInput(session, "cond_legend_label",  value = input$multi_colnamesCOND)
  })
  
  
  #if "no condition" is selected in "type of barplot", the boxes for the choice of condition and the one for labelling the legend are disabled
  observe({
    if (req(input$type_plot_relation) == "nocond") { 
      #disable the boxes for the choice of condition and the one for labelling the legend 
      shinyjs::disable("cond_legend_label")
      
    } else{
      #enables the boxes
      shinyjs::enable("cond_legend_label")
    }
    
  })
  
  
  
  #if condition box to plot is empty, it disables the option in radiobutton different from "no condition"
  observe({
    
    if(
      input$multi_colnamesCOND ==""){
      #disable the other radiobuttons options
      shinyjs::disable(selector = "[type=radio][value =stacked ]")
      shinyjs::runjs("$('[type=radio][value =stacked]').parent().addClass('disabled').css('opacity', 0.4)")
      shinyjs::disable(selector = "[type=radio][value =dodge ]")
      shinyjs::runjs("$('[type=radio][value =dodge]').parent().addClass('disabled').css('opacity', 0.4)")
    } else{
      #enables the other options in radiobutton
      shinyjs::enable(selector = "[type=radio][value =stacked ]")
      shinyjs::runjs("$('[type=radio][value =stacked]').parent().addClass('enabled').css('opacity', 1)")
      shinyjs::enable(selector = "[type=radio][value =dodge ]")
      shinyjs::runjs("$('[type=radio][value =dodge]').parent().addClass('enabled').css('opacity', 1)")
    }
    
  })
  
  observe({
    req(!is.null(input$session_type), !is.null(input$working_mode))
    print(input$session_type)
    if(
      input$working_mode == TRUE){ # single
      #keep "single" of "type of phenodata" enabled and it disables "multiple" radiobutton option
      shinyjs::enable(selector = "[type=radio][value =single ]")
      shinyjs::runjs("$('[type=radio][value =single]').parent().addClass('enabled').css('opacity', 1)")
      shinyjs::disable(selector = "[type=radio][value =multiple ]")
      shinyjs::runjs("$('[type=radio][value =multiple]').parent().addClass('disabled').css('opacity', 0.4)")
    } else{   # multiple
      #enables "the"multiple" in "type of phenodata" radiobutton, and disables "single"
      shinyjs::enable(selector = "[type=radio][value =multiple ]")
      shinyjs::runjs("$('[type=radio][value =stacked]').parent().addClass('enabled').css('opacity', 1)")
      shinyjs::disable(selector = "[type=radio][value =single ]")
      shinyjs::runjs("$('[type=radio][value =single]').parent().addClass('disabled').css('opacity', 0.4)")
    }
    
    
    
  })
  
  #if radiobutton selected as "allowed..."(value for lable is 1, for content is 3) it disables the related dropdown menu
  #  observe({     #for labels
  #    if (req(input$store_labels) == 1){   shinyjs::disable("pick_to_check_dict_labels")  }
  #    else {   shinyjs::enable("pick_to_check_dict_labels")  }
  #  })
  
  
  
  
  #disable and re enabling of components in plotting panel
  observe({
    #removal legend 
    if(input$remove_legend == TRUE){
      shinyjs::disable("cond_legend_label")
      shinyjs::disable("legend_position")
      
    } else{
      shinyjs::enable("cond_legend_label")
      shinyjs::enable("legend_position")
    }
    
  })
  
  
 
  
  output$save_multipic <- downloadHandler(
    filename = function() {
      file = paste("multiplot",".png",sep="")
    },
    content = function(file) {
      
        shinyjs::html(id="plotText", "Saving plot")
        shinyjs::show(id="loading-content")
        on.exit({ 
          shinyjs::hide(id="loading-content", anim=TRUE, animType="fade")    
        })
        
        
  
      
      
   #   shiny::validate(
  #      need(!is.null(gVars$phTable), "No Phenodata File Provided!")
   #   )
      #ggsave(file,plot=input$multiplot)
    #  tiff("test.tiff", units="in", width=5, height=5, res=300)
      #file.copy(fn_downloadname(), file, overwrite=T)
    #  png(file)
     # print(myplot())
    #  dev.off()
    }
  #contentType = "image/png"
    
  )
  
  
  
  
  
  
  
  
  
  
  
  #multiintegration tool
  
  observeEvent(input$upload_multi_pheno_submit, {
    multi_fPhenoFile <- input$multi_fPheno
    gVars$current_df_file_name <- input$multi_fPheno$name
    if (is.null(multi_fPhenoFile))
      return(NULL)
    
    
    #import always - contains typical colnames in metadata which are not of interest in curation
    always <- scan("./files/always.txt", character(), quote = "", skip=1)
    gVars$always <- always
    
    output$multifiles <- DT::renderDT({
      DT::datatable(input$multi_fPheno[,c(1,2)], selection=c("single"), options=list(scrollX=TRUE,scrollY=TRUE)) 
    })
    
    
    all_files <- reactive({
      req(input$multi_fPheno)
      purrr::map(input$multi_fPheno$datapath, read_excel_allsheets) %>% purrr::map(.,1) %>%
        purrr::set_names(input$multi_fPheno$name) %>%
        #add column file_name with correspondent file name
        mapply(cbind, ., "mainfile_name"=input$multi_fPheno$name, SIMPLIFY=F)
    })
     
    output$selected_df_from_multi <- DT::renderDataTable({
      req(input$multi_fPheno)
      req(input$multifiles_rows_selected)
      all_files()[[
        input$multi_fPheno$name [[input$multifiles_rows_selected]]
      ]]
    },options=list(scrollX=TRUE,scrollY=TRUE))
    
    gVars$multidf_list <- all_files()
    
    shinyBS::updateButton(session, "upload_multi_pheno_submit",  style="success", icon=icon("hand-o-right"))
    
  })
  
  
  
  observeEvent(input$go_to_multi_tool_integ_button,{
    req(length(gVars$multidf_list)!=0)
    gVars$multidf <- as_tibble(do.call(plyr::rbind.fill,gVars$multidf_list)) %>% dplyr::select(mainfile_name, everything())
      
    #updates tabpanel and sidebar
    updateTabsetPanel(session, "display", selected="multiple_integration_tool_page")
    shinyBS::updateButton(session, "upload_multi_pheno_submit" ,style="info", icon=icon("hand-o-right"))
    
    shinyBS::toggleModal(session, "importPhenoModal", toggle="close")
    shinyBS::updateButton(session, "import_pheno_submit", style="success", icon=icon("check-circle"))
    shinyBS::updateCollapse(session, "bsSidebar1", open="LOAD VOCABULARY", style=list("LOAD PHENODATA"="success","LOAD VOCABULARY"="warning"))
  })
  
  
  output$multidf_table <- DT::renderDataTable({
    shiny::validate(
      need(!is.null(gVars$multidf), "No Multiple Phenodata Files Provided!")
    )
    if(is.null(gVars$multidf))
      return(NULL)
    
    phTable <- gVars$multidf  
    DT::datatable(phTable, filter="none",
                  options = list(
                    search = list(regex=TRUE, caseInsensitive=FALSE),
                    scrollX=TRUE,
                    ordering=F
                  )
    )
  },server=TRUE)
  
 
  
  
  #close the multi upload page and merge the multiple datasets
  observeEvent(c(input$go_to_multi_tool_integ_button,input$upload_voc_submit),{
    req(length(gVars$multidf_list)!=0 &!is.null(gVars$dkeys))
    
    #creates the sub_df safe/fast check/overview
    dict <- gVars$dkeys
    multidf <- gVars$multidf %>% .[,!names(.) %in% "mainfile_name"]
    always <- gVars$always
    
    
    pre_safe_check <- multidf %>%  .[,colnames(.) %in% dict$label]
    cn <- colnames(pre_safe_check)
    # retrieve logical indices to find the real_safe_colnames
    real_safe_colnames_index <- cn %>% 
      data.frame(labnames=.) %>% 
      apply(.,1, function(k) lapply(as.list(pre_safe_check[[k]]), function(j) 
        all_of(unique(j)) %in%  unique(dict$allowed_features[dict$label==k]))) %>%
      sapply(., function(z) all(z==T))
    safe_check <- pre_safe_check[,real_safe_colnames_index]  
    
    
    fast_check <- multidf %>%
      dplyr::select(any_of(always),-intersect(always,colnames(safe_check))) 
    
    
    slow_check <-  multidf %>%
      dplyr::select(-c(colnames(safe_check), colnames(fast_check)))
    
    
    
    #prepare the name for the menu update of the possible outcomes
    dfnames<-c("safe_check","fast_check","slow_check")
    gVars$grouping_multi_tables <- gVars$grouping_multi_tables[sapply(dfnames, function(x) ncol(get(x)) != 0)]
    updateSelectizeInput( 
      session,
      inputId="multijoin_outcomes",
      choices=gVars$grouping_multi_tables,
      selected = character(0),
      options = list(
        placeholder = 'Please select an option below',
        onInitialize = I('function() { this.setValue(""); }')),
      server=TRUE)
    
    
    #updates and creates reactive values
    gVars$multi_safe_check <- reactiveVal(safe_check)
    gVars$multi_fast_check <- reactiveVal(fast_check)
    gVars$multi_slow_check <- reactiveVal(slow_check)
    
    
    
  })
  
  
  
  
  
  
  
  
  trig_column_table <- reactive({
    paste(input$multijoin_outcomes, input$glp_pt_button_multi)#input$accept_multi_button, input$issue_multi_button )
  })
  
  
  
  
  observeEvent(trig_column_table(),{
    req(input$multijoin_outcomes)
    
    if (input$multijoin_outcomes == "To overview"){
      dt_column_toshow <- colnames(gVars$multi_slow_check())
    } else if (input$multijoin_outcomes == "Fast check"){
      dt_column_toshow <- colnames(gVars$multi_fast_check())
    } else { # safe items
      dt_column_toshow <- colnames(gVars$multi_safe_check())
    }
    
    
    
    col_df <- data.frame(column_name = dt_column_toshow) 
    vect_table_cols <- 1
    ghost_cols <- 2:2 
    labels <- unique(gVars$dkeys$label)
    
    hits_label <- apply(col_df, 1, function(k)  k %in% labels)
    col_df[,ghost_cols] <- hits_label
    
    
    output$columns_multi_selected_group <- DT::renderDataTable(
      DT::datatable(
        col_df,  selection=list(mode="single", target ="cell"), 
        options=list(columnDefs=list(list(visible=FALSE, targets=c(0,ghost_cols)),list(className="dt-center", targets="_all")))  
      )  %>%
        formatStyle(columns=vect_table_cols,
                    valueColumns = ghost_cols,
                    color= styleEqual(c("0","1"),
                                      c("#DC3545","#04AA6D")) 
                    
        ))
    
  })
  
  
  
  
  
  
  
  
  
  
  
  
  observeEvent(input$columns_multi_selected_group_cell_clicked,{
    
    shinyBS::updateButton(session, "issue_multi_button", disabled = TRUE)
    shinyBS::updateButton(session, "accept_multi_button", disabled = TRUE)   
    
    
    multidf <- gVars$multidf
    
    req(length(input$columns_multi_selected_group_cell_clicked)!=0) 
    
    
    if (input$multijoin_outcomes == "To overview"){
      dt_multi_toshow <- gVars$multi_slow_check()[[input$columns_multi_selected_group_cell_clicked$value]] %>% tibble(.) %>% setNames(., input$columns_multi_selected_group_cell_clicked$value)
    } else if (input$multijoin_outcomes == "Fast check"){
      dt_multi_toshow <- gVars$multi_fast_check()[[input$columns_multi_selected_group_cell_clicked$value]]%>% tibble(.) %>% setNames(., input$columns_multi_selected_group_cell_clicked$value)
    } else { # safe items
      dt_multi_toshow <- gVars$multi_safe_check()[[input$columns_multi_selected_group_cell_clicked$value]]%>% tibble(.) %>% setNames(.,input$columns_multi_selected_group_cell_clicked$value)
    }
    
    
    
    #to build ghost cols to color words according to their presence in the vocabulary or are completely numeric
    ref_contents <- unique( gVars$dkeys[gVars$dkeys$label == input$columns_multi_selected_group_cell_clicked$value,]$allowed_features )
    dict <- gVars$dkeys
    
    table_ncols <-ncol(dt_multi_toshow)
    vect_table_cols <- 1:table_ncols
    ghost_cols <- (table_ncols+1): (table_ncols*2)           #cols later to hide, but needed to store boolean of the presence entry_voc in dt
    
    
    # suggested terms present in the vocabulary
    #hit_terms_multi <-  (apply( (dt_multi_toshow[,vect_table_cols]),2,"%in%", ref_entries))
    hit_terms_multi <-  apply( (dt_multi_toshow[,vect_table_cols]),2,"%in%", ref_contents)
    
    if(is_empty(hit_terms_multi)){
      shinyjs::info("No other column names are available, change the potential outcome!")
      #remove the outcome
      req(input$multijoin_outcomes)
      gVars$grouping_multi_tables <- gVars$grouping_multi_tables[!gVars$grouping_multi_tables %in% input$multijoin_outcomes]
      return()
    }
    if(is.null(dim(hit_terms_multi)) & !is_empty(hit_terms_multi) ){
      hit_terms_multi <- as.list(hit_terms_multi)
    }
     
    
    dt_multi_toshow[,ghost_cols] <- (hit_terms_multi)
    gVars$col_issue_cases <- dt_multi_toshow %>% cbind(mainfile_name=gVars$multidf$mainfile_name,.)  %>% .[.[[3]] == FALSE,1:2] %>% unique(.)
    
    req(input$multijoin_outcomes)
    output$dt_toshow_multi_selected_group <- DT::renderDataTable(
      DT::datatable(
        unique(dt_multi_toshow),  selection=list(mode="single", target ="column"),
        options=list(scrollX=TRUE,
                     columnDefs=list(
                       
                       list(className="dt-center", targets="_all"),
                       list(targets=c(0,ghost_cols), visible=FALSE)
                     ))
      )  %>%
        formatStyle(columns=vect_table_cols,
                    valueColumns = ghost_cols,
                    color= styleEqual(c("0","1"),
                                      c("#DC3545","#04AA6D"))
        )
      
    )
    
  })
  
  
  
  
  
  observeEvent(input$dt_toshow_multi_selected_group_columns_selected,{
    shinyBS::updateButton(session, "issue_multi_button", disabled = FALSE)
    shinyBS::updateButton(session, "accept_multi_button", disabled = FALSE) 
  })
  
  
  
  
  
  
  #cleans the datatable as the columns are checked(both if accepted, or sent to issue)
  observeEvent(input$glp_pt_button_multi,{#react_trigger_multi_Acc_and_Issue(),{
    shinyBS::updateButton(session, "issue_multi_button", disabled = TRUE)
    shinyBS::updateButton(session, "accept_multi_button", disabled = TRUE) 
    
    req(input$multijoin_outcomes, input$dt_toshow_multi_selected_group_columns_selected)
    req(!is.null(gVars$dkeys))
    req(input$columns_multi_selected_group_cell_clicked$value)
    if (input$multijoin_outcomes == "To overview"){
      tmptable<- gVars$multi_slow_check()
      #stores for undo
      undo_element <- list(tmptable[,input$columns_multi_selected_group_cell_clicked$value], input$multijoin_outcomes, gVars$multi_flag)
      gVars$last5list <- c(gVars$last5list, (list(multi = undo_element)))
      tmptable <- tmptable[,!colnames(tmptable) %in% input$columns_multi_selected_group_cell_clicked$value ]
      tmp_col <- ncol(tmptable)
      gVars$multi_slow_check(tmptable)
    } else if (input$multijoin_outcomes == "Fast check"){
      tmptable<- gVars$multi_fast_check()
      #stores for undo
      undo_element <- list(tmptable[,input$columns_multi_selected_group_cell_clicked$value], input$multijoin_outcomes, gVars$multi_flag)
      gVars$last5list <- c(gVars$last5list, (list(multi = undo_element)))
      tmptable <- tmptable[,!colnames(tmptable) %in% input$columns_multi_selected_group_cell_clicked$value ]
      tmp_col<- ncol(tmptable)
      gVars$multi_fast_check(tmptable)
    } else { # safe items
      tmptable<-gVars$multi_safe_check()
      #stores for undo
      undo_element <- list(tmptable[,input$columns_multi_selected_group_cell_clicked$value], input$multijoin_outcomes, gVars$multi_flag)
      gVars$last5list <- c(gVars$last5list, (list(multi = undo_element)))
      tmptable<-tmptable[, !colnames(tmptable) %in% input$columns_multi_selected_group_cell_clicked$value ]
      tmp_col<- ncol(tmptable)
      gVars$multi_safe_check(tmptable)
      
    }
    
    
    
    #cleans the selectizeinput from safe/to chekc/to overview once they are empty (=all entries were processed)
    if (tmp_col == 0){
      gVars$grouping_multi_tables <- gVars$grouping_multi_tables[!gVars$grouping_multi_tables %in% input$multijoin_outcomes]
      
      
      if (length(gVars$grouping_multi_tables) != 0){
        updateSelectizeInput(session,
                             inputId="multijoin_outcomes",
                             label="Potential outcome:",
                             choices=gVars$grouping_multi_tables,#c("Safe","Fast check","To overview"),
                             selected=character(0),
                             options = list(
                               placeholder = 'Please select an option below',
                               onInitialize = I('function() { this.setValue(""); }')),
                             server=TRUE)
        
        
      } else{
        updateSelectizeInput(session,
                             inputId="multijoin_outcomes",
                             label="Potential outcome:",
                             choices=NULL,
                             options = list(
                               placeholder = 'No more entries to process',
                               onInitialize = I('function() { this.setValue(""); }')),
                             server=TRUE)
        
        
        #  output$columns_multi_selected_group <- DT::renderDataTable(NULL) 
      }
      
    }
    
  })
  
  
  #        paste(input$accept_multi_button,input$issue_multi_button)   
  
  
  output$pt_text_MULTI <- renderText({"<b>Procedural Track</b>"})
  output$pt_MULTI <- renderText({"<i><small>Waiting for action!</small></i>"})
  
  
  #stores whether the entry was accepted in the final report file.
  observeEvent(input$accept_multi_button,{
    shinyBS::updateButton(session, "glp_pt_button_multi", disabled = FALSE)
    shinyBS::updateButton(session, "issue_multi_button", disabled = TRUE)
    shinyBS::updateButton(session, "accept_multi_button", style="success", icon=icon("hand-o-right"),disabled = TRUE)
    
    req(input$columns_multi_selected_group_cell_clicked$value)
    gVars$multi_accept_colnames <- c(gVars$multi_accept_colnames, input$columns_multi_selected_group_cell_clicked$value)
    tmp_msg <- paste(tags$b(input$columns_multi_selected_group_cell_clicked$value), "is accepted." )
    gVars$multi_ACmsg <- append (gVars$multi_ACmsg, list(tmp_msg))
    output$pt_MULTI <- renderText({tmp_msg})
    
    #in pt track report
    gVars$pt_glp_list <- c(gVars$pt_glp_list, tmp_msg)
    gVars$multi_flag <- "accept"
  })
  
  
  
  #stores whether the entry was added to issue report.
  observeEvent(input$issue_multi_button,{
    shinyBS::updateButton(session, "glp_pt_button_multi", disabled = FALSE)
    shinyBS::updateButton(session, "accept_multi_button", disabled = TRUE)
    shinyBS::updateButton(session, "issue_multi_button", style="success", icon=icon("hand-o-right"),disabled = TRUE)
    
    req(input$columns_multi_selected_group_cell_clicked$value)
    gVars$multi_issue_colnames <- c(gVars$multi_issue_colnames, input$columns_multi_selected_group_cell_clicked$value)
    tmp_msg <- paste(tags$b(input$columns_multi_selected_group_cell_clicked$value), "needs further modification. (ISSUE)")
     
    #identify entries which need a further round of modification to store in issue report message
    unique_mainfiles <- unique(gVars$col_issue_cases$mainfile_name)
    issue_entries <- lapply(unique_mainfiles, function(file) {
                  entries <- pull(gVars$col_issue_cases[gVars$col_issue_cases$mainfile_name==file,],input$columns_multi_selected_group_cell_clicked$value) 
                  if(all(is.na(entries))) {x<-NULL
                  } else {
                        entries <- entries %>% paste(., collapse="|")
                      #  if (!is.null(entries) | length(entries!= 0)){
                          y<- paste(  "File Name: ",tags$b(file)," :<br/>")
                          x<-paste0(y, "&#x09;",entries,"<br/>")
                        }
                   
     x
    })
    
    issue_entries <- issue_entries %>% discard(is.null)
    full_tmp_msg <- paste(tmp_msg,"<pre>",toString(issue_entries))
    full_tmp_msg <- full_tmp_msg %>%  
      gsub(pattern = "<br/>,", replacement = "<br/>", .) %>% 
      gsub(pattern = "\\|", replacement = "<br/> &#x09;", .)  
    
    gVars$multi_ISSmsg <- append (gVars$multi_ISSmsg, list(full_tmp_msg))
    
    
    output$pt_MULTI <- renderText({tmp_msg})
    
    gVars$multi_flag <- "issue" 
    #in pt track report
    gVars$pt_glp_list <- c(gVars$pt_glp_list, full_tmp_msg)
  })
  
  
  
  observeEvent(input$glp_pt_button_multi,{
    shinyBS::updateButton(session, "glp_pt_button_multi", disabled = TRUE)
    shinyBS::updateButton(session, "issue_multi_button", style="info", disabled = TRUE)
    shinyBS::updateButton(session, "accept_multi_button", style="info", disabled = TRUE)
    
    output$pt_MULTI <- renderText({"<i><small>Waiting for action!</small></i>"})
    
    #reset the table
    output$dt_toshow_multi_selected_group <- DT::renderDataTable(NULL) 
    
    #if GLPmode activated, stores the message and open the GLPcomment modal window
    req(!input$GLP_inactivation_mode)
    showModal(dataModal_MULTI_comments(failed = FALSE))
    
    
  })
  
  
  
  
  # Return a modal dialog window with the possibility to input data. If 'failed' is TRUE (no written justifications in the inputbox), the user is informed about it.
  dataModal_MULTI_comments <- function(failed = FALSE) {
    modalDialog(
      textAreaInput(inputId = "glp_comments_box", label = "GLP comments", width = "100%", rows=4,
                    placeholder = 'Please justify your last operation.'),
      shinyBS::bsButton("glp_store_commentMULTI", label="Add", style="info", icon=icon("hand-point-right")),
      
      if (failed){
        div(br(),tags$b("In GLP mode, you must justify your decisions. Please do."), style = "color: red;")},
      
      footer =  NULL#modalButton("Cancel")
      
    )
  }
  
  
  
  # When Add button(glp_store_comment) is pressed, it tests if the textareainput box is not empty. 
  # If successful, remove the modal, otherwise it shows another modal with a failure message.
  observeEvent(input$glp_store_commentMULTI, {
    # Check that box for glp comments is filled
    if (input$glp_comments_box != "") {
      tabulated_comment <- paste0("\t","COMMENT:",input$glp_comments_box)
      list_element <- list(tabulated_comment)
      
      if (gVars$multi_flag=="accept"){
        gVars$multi_ACmsg <- c(gVars$multi_ACmsg, list_element)
      }
      else{
        gVars$multi_ISSmsg <- c(gVars$multi_ISSmsg, list_element)
      }
      
      #adds the comment to the complete procedural track
      gVars$pt_glp_list <- c(gVars$pt_glp_list, list_elem)
      
      removeModal()
      
      
    } else {
      showModal(dataModal_MULTI_comments(failed = TRUE))
    }
    
  }) 
  
  
  
  
 
  
  
  
   
 
  
  output$downloadFunmapponeInput <- downloadHandler(
    
    filename = function() {
      paste("input_funmappone.xlsx", sep = "")
    },
    content = function(file) {
      
      if(is.null(gVars$GList) || is.null(gVars$pheno)){
        print('no input')
        
        return(NULL)
      }
      
      shinyjs::html(id="loadingText", "Saving tables")
      shinyjs::show(id="loading-content")
      on.exit({
        print("inside on exit")
        shinyjs::hide(id="loading-content", anim=TRUE, animType="fade")    
      })
      
      write.xlsx(gVars$GList[[1]], file, sheetName = names(gVars$GList)[1], row.names = FALSE) 
      
      if(length(gVars$GList)>1){
        for(i in 2:length(gVars$GList)){
          write.xlsx(gVars$GList[[i]], file, sheetName = names(gVars$GList)[i], append = TRUE, row.names = FALSE) 
        }
      }
      
      write.xlsx(gVars$pheno, file, sheetName = "Groups", append = TRUE, row.names = FALSE) 
      
      
    }
  )
  
  
 
   
  
  output$downloadAnovaData <- downloadHandler(
    filename = function() {
      paste("anova_table.xlsx", sep = "")
    },
    content = function(file) {
      if(length(gVars$PValMat)==0){ 
        print("No anova table to save!")
        return(NULL)
      }
      
      shinyjs::html(id="loadingText", "Saving tables")
      shinyjs::show(id="loading-content")
      on.exit({
        print("inside on exit")
        shinyjs::hide(id="loading-content", anim=TRUE, animType="fade")    
      })
      
      print("I'm saving anova tables")
      
      firstCall = TRUE
      
      for(exp in names(gVars$PValMat)){
        print(exp)
        times = names(gVars$PValMat[[exp]])
        for(tp in times){
          print(tp)
          if(firstCall){
            if(nrow(gVars$PValMat[[exp]][[tp]])>0){
              write.xlsx(gVars$PValMat[[exp]][[tp]], file, sheetName = paste(exp,tp,sep = "_"), append = FALSE)
              xlcFreeMemory()
              firstCall = FALSE
              print("first")
            }
          }else{
            if(nrow(gVars$PValMat[[exp]][[tp]])>0){
              write.xlsx(gVars$PValMat[[exp]][[tp]], file, sheetName = paste(exp,tp,sep = "_"), append = TRUE)
              xlcFreeMemory()
              print("appending")
            }
          }
          
        }
      }
      
      
      print("Anova table stored!")
    }
  )
  
  output$downloadBMDData <- downloadHandler(
    filename = function() {
      paste("bmd_table.xlsx", sep = "")
    },
    content = function(file) {
      if(length(gVars$MQ_BMD)==0){ 
        print("No bmd table to save!")
        return(NULL)
      }
      shinyjs::html(id="loadingText", "Saving tables")
      shinyjs::show(id="loading-content")
      on.exit({
        print("inside on exit")
        shinyjs::hide(id="loading-content", anim=TRUE, animType="fade")    
      })
      
      print("I'm saving bmd tables")
      
      if(is.null(gVars$MQ_BMD_filtered)){
        BMDDataList = gVars$MQ_BMD
        
      }else{
        BMDDataList = gVars$MQ_BMD_filtered
        
      }
      
      firstCall = TRUE
      
      for(exp in names(BMDDataList)){
        print(exp)
        times = names(BMDDataList[[exp]])
        for(tp in times){
          print(tp)
          if(firstCall){
            if(nrow(BMDDataList[[exp]][[tp]][[1]])>0){
              write.xlsx(BMDDataList[[exp]][[tp]][[1]], file, sheetName = paste(exp,tp,sep = "_"), append = FALSE)
              xlcFreeMemory()
              firstCall = FALSE
              print("first")
            }
          }else{
            if(nrow(BMDDataList[[exp]][[tp]][[1]])>0){
              write.xlsx(BMDDataList[[exp]][[tp]][[1]], file, sheetName = paste(exp,tp,sep = "_"), append = TRUE)
              xlcFreeMemory()
              print("appending")
            }
          }
          
        }
      }
      
      
      print("BMD table stored!")
    }
  )
  
  # output$downloadBMDData <- downloadHandler(
  #   # if(is.null(gVars$BMD_tab)){
  #   #   return(NULL)
  #   # }
  #   
  #   filename = function() {
  #     paste("bmd_table.csv", sep = "")
  #   },
  #   content = function(file) {
  #     write.csv(gVars$BMD_tab, file, row.names = FALSE)
  #   }
  # )
  
  output$Anova_table <- DT::renderDataTable({
    if(is.null(gVars$PValMat))
      return(NULL)
    
    if(is.null(input$time_point_id_visual))
      return(NULL)
    
    print("TP da analizzare:")
    print(input$time_point_id_visual)
    
    Anova_tab <- gVars$PValMat[[input$experiment_anova]][[input$time_point_id_visual]]
    print(head(Anova_tab))
    print(head(Anova_tab))
    
    print(head(Anova_tab))
    gVars$Anova_tab=Anova_tab
    DT::datatable(Anova_tab, filter="top",
                  options = list(
                    search = list(regex=TRUE, caseInsensitive=FALSE),
                    scrollX=TRUE,
                    ordering=T,
                    rowCallback = JS('
                                                  function(nRow, aData, iDisplayIndex, iDisplayIndexFull) {
                                                  // Bold and green cells for conditions
                                                  if (parseFloat(aData[2]) <=0.05)
                                                  $("td:eq(3)", nRow).css("font-weight", "bold");
                                                  }')
                  )
    )
  },server=TRUE) #,sanitize.text.function=function(x){x}
  
  
  # output$AnovaVenn = renderPlot({
  #   if(is.null(gVars$PValMat))
  #     return(NULL)
  #   
  #   print(gVars$var_genes)
  #   gplots::venn(gVars$var_genes,show.plot = TRUE)
  # })
  
  output$anovaPlot <- renderPlot({
    if(is.null(gVars$PValMat))
      return(NULL)
    
    if(length(gVars$PValMat)==0)
      return(NULL)
    
    
    #Anova_tab <-gVars$PValMat[[1]]
    #Anova_tab <- gVars$PValMat[[input$time_point_id_visual]]
    Anova_tab <- gVars$PValMat[[input$experiment_anova]][[input$time_point_id_visual]]
    
    print("Nomi time points: ")
    print(names(gVars$PValMat))
    
    x <-  c(sum(as.numeric(Anova_tab[,2])<=0.05), sum(as.numeric(Anova_tab[,2])>0.05))
    labels <-  c("p.val <= 0.05","p.val>0.05")
    
    piepercent<- round(100*x/sum(x), 1)
    
    # Plot the chart.
    pie(x, labels = piepercent, main = "Filtering Result",col = rainbow(length(x)))
    legend("topright", c("Significant Genes","Non Significant Genes"), cex = 0.8,
           fill = rainbow(length(x)))
    
  })
  
  
  output$BMDVenn <- renderPlot({
    if(is.null(gVars$MQ_BMD_filtered)){ 
      print("Null BMD")
      return(NULL)
    }
    
    listFV = list()
    for(i in 1:length(gVars$MQ_BMD_filtered)){
      listFV[[names(gVars$MQ_BMD_filtered)[i]]] = gVars$MQ_BMD_filtered[[i]]$BMDValues_filtered[,1]
    }
    venn(listFV) 
  }) #server = TRUE
  
  output$meanBMD_timepoint = renderPlotly({
    if(is.null(gVars$EnrichDatList)){ 
      print("Null enrichment")
      return(NULL)
    }
    
    PD = c()
    
    ER <- gVars$EnrichDatList # result of enrichment
    Hier = gVars$hierarchy
    
    for(i in 1:length(ER)){
      print(i)
      
      ERi = ER[[i]]
      if(nrow(ERi)==0) next
      
      print(ERi)
      elemn = names(ER)[i]
      exper = strsplit(x = elemn, split = "_")[[1]][1]
      timep = strsplit(x = elemn, split = "_")[[1]][2]
      
      if(is.null(gVars$MQ_BMD_filtered)){
        BMDFilMat = gVars$MQ_BMD[[exper]][[as.character(timep)]]$BMDValues #filtered BMD genes
        
      }else{
        BMDFilMat = gVars$MQ_BMD_filtered[[exper]][[as.character(timep)]]$BMDValues_filtered #filtered BMD genes
        
      }
      
      if(!is.null(BMDFilMat) & nrow(ERi)>0){
        
        for(npat in 1:nrow(ER[[i]])){
          gi = unlist(strsplit(x = as.character(ERi[npat,2]),split = ","))
          
          goID = ERi[npat,"annID"]
          PatName = ERi[npat,"Description"]
          PatPval = ERi[npat,"pValueAdj"]
          NGenes = length(gi)
          # PatNGenes = ERi[npat,"gID"]
          # PatSize = ERi[npat,]
          
          idx = which(tolower(BMDFilMat[,1]) %in% tolower(gi))
          BMD = as.numeric(BMDFilMat[idx,"Single column analysis"])
          names(BMD) = BMDFilMat[idx,"Gene"]
          bmd = mean(BMD)
          
          levidx = which(Hier[,"ID"] %in% goID)
          levName = as.character(Hier[levidx[1],1])
          
          #PD = rbind(PD, cbind(names(gVars$MQ_BMD_filtered)[i],PatName,PatPval, bmd, NGenes, levName))
          PD = rbind(PD, cbind(exper, timep, PatName,PatPval, bmd, NGenes, levName))
          
        }
      }
    }
    
    colnames(PD) = c("Experiment","TimePoint","FunCategory", "Adj.pval","MeanBMD","NGenes", "LevName")
    PD = as.data.frame(PD)
    PD$TimePoint = factor(PD$TimePoint, level=sort(as.numeric(as.vector(unique(PD$TimePoint)))))
    PD$Adj.pval = as.numeric(as.vector(PD$Adj.pval))
    PD$MeanBMD = as.numeric(as.vector(PD$MeanBMD))
    PD$logPval = -log(PD$Adj.pval)
    PD$logPval = -log(PD$Adj.pval)
    PD$NGenes = as.numeric(as.vector(PD$NGenes))
    gVars$MeanBMDTimePoint = PD
    ggp = ggplot(data=PD, aes(MeanBMD, fill=TimePoint)) + 
      geom_histogram() + facet_grid(. ~ Experiment)
    
    ggplotly(ggp)
    
  })
  
  
  output$pathway_bubble = renderPlotly({
    if(is.null(gVars$EnrichDatList)){ 
      print("Null enrichment")
      return(NULL)
    }
    
    PD = c()
    
    ER <- gVars$EnrichDatList # result of enrichment
    Hier = gVars$hierarchy
    
    for(i in 1:length(ER)){
      print(i)
      
      ERi = ER[[i]]
      if(nrow(ERi)==0) next
      
      print(ERi)
      elemn = names(ER)[i]
      exper = strsplit(x = elemn, split = "_")[[1]][1]
      timep = strsplit(x = elemn, split = "_")[[1]][2]
      
      if(is.null(gVars$MQ_BMD_filtered)){
        BMDFilMat = gVars$MQ_BMD[[exper]][[as.character(timep)]]$BMDValues #filtered BMD genes
      }else{
        BMDFilMat = gVars$MQ_BMD_filtered[[exper]][[as.character(timep)]]$BMDValues_filtered #filtered BMD genes
      }
      
      if(!is.null(BMDFilMat) & nrow(ERi)>0){
        
        for(npat in 1:nrow(ER[[i]])){
          gi = unlist(strsplit(x = as.character(ERi[npat,2]),split = ","))
          
          goID = ERi[npat,"annID"]
          PatName = ERi[npat,"Description"]
          PatPval = ERi[npat,"pValueAdj"]
          NGenes = length(gi)
          
          idx = which(tolower(BMDFilMat[,1])%in% tolower(gi))
          BMD = as.numeric(BMDFilMat[idx,"Single column analysis"])
          names(BMD) = BMDFilMat[idx,"Gene"]
          bmd = mean(BMD)
          
          levidx = which(Hier[,"ID"] %in% goID)
          levName = as.character(Hier[levidx[1],1])
          
          PD = rbind(PD, cbind(exper, timep,PatName,PatPval, bmd, NGenes, levName))
        }
      }
    }
    
    colnames(PD) = c("Experiment","TimePoint","FunCategory", "Adj.pval","MeanBMD","NGenes", "LevName")
    PD = as.data.frame(PD)
    PD$TimePoint = factor(PD$TimePoint, level=sort(as.numeric(as.vector(unique(PD$TimePoint)))))
    PD$Adj.pval = as.numeric(as.vector(PD$Adj.pval))
    PD$MeanBMD = as.numeric(as.vector(PD$MeanBMD))
    PD$logPval = -log(PD$Adj.pval)
    PD$logPval = -log(PD$Adj.pval)
    PD$NGenes = as.numeric(as.vector(PD$NGenes))
    gVars$MeanBMDTimePoint = PD
    
    p <- PD %>%
      ggplot(aes(MeanBMD,logPval, label = FunCategory, color = LevName, size = NGenes)) + # color=FunCategory
      geom_point() +
      scale_x_log10() +
      theme_bw() + labs(x = "Mean BMD", y = "-log(P.Value)") +
      facet_grid(Experiment ~ TimePoint)
    
    ggplotly(p)
  })
  
  
  
  output$path_bmd_dist = renderPlotly({
    # shiny::validate(
    #   need(is.null(input$BMD_table_rows_selected), "Select at least one row of the matrix!")
    # )
    
    if(length(input$PAT_table_rows_selected) == 0){
      return(NULL)
    }
    
    selectedrowindex = input$PAT_table_rows_selected[length(input$PAT_table_rows_selected)]
    selectedrowindex = as.numeric(selectedrowindex)
    
    ER = gVars$EnrichDatList[[input$time_point_id_visualPat]]
    PatName = ER[selectedrowindex,"annID"]
    PatGenes = ER[selectedrowindex,"gID"]
    
    exp_tp = unlist(strsplit(input$time_point_id_visualPat,"_"))
    
    if(is.null(gVars$MQ_BMD_filtered)){
      BMDFilMat = gVars$MQ_BMD[[exp_tp[1]]][[exp_tp[2]]]$BMDValues
      
    }else{
      BMDFilMat = gVars$MQ_BMD_filtered[[exp_tp[1]]][[exp_tp[2]]]$BMDValues_filtered
      
    }
    
    gi = unlist(strsplit(x = ER[input$PAT_table_rows_selected,2],split = ","))
    idx = which(tolower(BMDFilMat[,1])%in% tolower(gi))
    BMD = as.numeric(BMDFilMat[idx,"Single column analysis"])
    BMDL = as.numeric(BMDFilMat[idx,"BMDL"])
    BMDU = as.numeric(as.vector(BMDFilMat[idx,"BMDU"]))
    
    names(BMD) = BMDFilMat[idx,"Gene"]
    names(BMDL) = names(BMDU) = names(BMD)
    
    
    BMD = data.frame(gene = names(BMD), bmd = BMD, bmdl = BMDL, bmdu = BMDU)
    BMD = BMD[order(BMD$bmd),]
    BMD$gene = factor(x = BMD$gene, levels = BMD$gene)
    
    
    p = ggplot(data=BMD, aes(x=gene, y=bmd, group=1, label1 = bmdl, label2 = bmdu)) +
      geom_line()+
      geom_point() +
      geom_ribbon(aes(ymin=BMD$bmdl, ymax=BMD$bmdu), linetype=2, alpha=0.1) +
      labs(y = "BMDL - BMD - BMDU", x = "Gene")
    
    gVars$BMD_dist_in_path = p
    ggplotly(p)
    
  })
  
  output$PAT_table = DT::renderDataTable({
    # if(is.null(gVars$MQ_BMD_filtered)){ 
    #   print("Null BMD")
    #   return(NULL)
    # }
    if((input$time_point_id_visualPat %in% names(gVars$EnrichDatList))==FALSE){
      print("No enrichment for this TP")
      return(NULL)
    }
    
    PATWAY_tab <- gVars$EnrichDatList[[input$time_point_id_visualPat]]
    PATWAY_tab = PATWAY_tab[,c(1,3,4,5)]
    gVars$PATWAY_tab=PATWAY_tab
    DT::datatable(PATWAY_tab, filter="top",
                  options = list(
                    search = list(regex=TRUE, caseInsensitive=FALSE),
                    scrollX=TRUE,
                    ordering=T
                  )
    )
  })
  
  
  
  output$BMD_BMDL = renderPlotly({
    if(is.null(gVars$MQ_BMD)){ 
      print("Null BMD")
      return(NULL)
    }
    
    print(":")
    if(is.null(gVars$MQ_BMD_filtered)){
      BMD_tab <- gVars$MQ_BMD[[input$experiment_bmd]]#[[input$time_point_id_visual2]]$BMDValues_filtered
    }else{
      BMD_tab <- gVars$MQ_BMD_filtered[[input$experiment_bmd]]#[[input$time_point_id_visual2]]$BMDValues_filtered
    }    
    #BMD_tab <- gVars$MQ_BMD_filtered#[[input$time_point_id_visual2]]$BMDValues_filtered
    #save(BMD_tab, file="BMD_table.RData")
    
    DF = c()
    for(i in 1:length(BMD_tab)){ # for each time point
      print(BMD_tab[[i]][[1]])
      if(!is.null(BMD_tab[[i]][[1]])){
        if(nrow(BMD_tab[[i]][[1]])>=1){
          DF = rbind(DF, cbind(names(BMD_tab)[i],
                               BMD_tab[[i]][[1]][,"Single column analysis"],
                               BMD_tab[[i]][[1]][,"BMDL"],
                               as.character(BMD_tab[[i]][[1]][,"MOD_NAME"])))
        }
      }
    }
    colnames(DF) = c("TimePoint", "Single column analysis","BMDL","MOD_NAME")
    print(head(DF))
    DF = as.data.frame(DF)
    
    DF$TimePoint = factor(DF$TimePoint, level=sort(as.numeric(as.vector(unique(DF$TimePoint)))))
    DF$MOD_NAME = as.factor( DF$MOD_NAME)
    DF$BMD = as.numeric(as.vector(DF$BMD))
    DF$BMDL = as.numeric(as.vector(DF$BMDL))
    
    #save(DF, file = "../BMD_BMDL.RData")
    
    p = ggplot(DF, aes(x=BMDL, y=BMD, color = MOD_NAME)) +
      geom_point(shape=1) +  facet_grid(. ~ TimePoint)
    
    gVars$BMD_BMDL = p
    ggplotly(p)
    
  })
  
  output$BMD_dist_models = renderPlot({
    if(is.null(gVars$MQ_BMD)){ 
      print("Null BMD")
      return(NULL)
    }
    
    print(":")
    if(is.null(gVars$MQ_BMD_filtered)){
      BMD_tab <- gVars$MQ_BMD[[input$experiment_bmd]]#[[input$time_point_id_visual2]]$BMDValues_filtered
    }else{
      BMD_tab <- gVars$MQ_BMD_filtered[[input$experiment_bmd]]#[[input$time_point_id_visual2]]$BMDValues_filtered
    }
    
    DF = c()
    for(i in 1:length(BMD_tab)){ # for each time point
      print(BMD_tab[[i]][[1]])
      if(!is.null(BMD_tab[[i]][[1]])){
        if(nrow((BMD_tab[[i]][[1]]))>=1){
          xx = cbind(names(BMD_tab)[i],as.character(BMD_tab[[i]][[1]][,"MOD_NAME"]))
          print(xx)
          DF = rbind(DF, xx)
        }
        
      }
    }
    colnames(DF) = c("TimePoint", "Model")
    DF = as.data.frame(DF)
    DF$TimePoint = factor(DF$TimePoint, level=sort(as.numeric(as.vector(unique(DF$TimePoint)))))
    DF = melt(table(DF$TimePoint, DF$Model))
    colnames(DF) = c("TimePoint", "Model", "Freq")
    DF$TimePoint = factor(DF$TimePoint, level=sort(as.numeric(as.vector(unique(DF$TimePoint)))))
    
    mm = unique(DF$TimePoint)
    for(i in mm){
      iid = which(DF$TimePoint %in% i)
      DF$Freq[iid] = DF$Freq[iid]/sum(DF$Freq[iid]) * 100
    }
    
    #save(DF, file = "../mod_hist.RData")
    
    print(head(DF))
    p=ggplot(data=DF, aes(x = "", y = Freq, fill = Model)) + geom_bar(width = 1, stat = "identity") + 
      coord_polar("y", start=0)    +  facet_grid(. ~ TimePoint)
    #geom_histogram()+  facet_grid(. ~ TimePoint)
    gVars$BMD_dist_models = p
    p
  })
  
  output$BMD_BMDL_BMDU_by_model = renderPlotly({
    if(is.null(gVars$MQ_BMD)){ 
      print("Null BMD")
      return(NULL)
    }
    
    
    print(":")
    if(is.null(gVars$MQ_BMD_filtered)){
      BMD_tab <- gVars$MQ_BMD[[input$experiment_bmd]]#[[input$time_point_id_visual2]]$BMDValues_filtered
    }else{
      BMD_tab <- gVars$MQ_BMD_filtered[[input$experiment_bmd]]#[[input$time_point_id_visual2]]$BMDValues_filtered
    }
    
    DF = c()
    for(i in 1:length(BMD_tab)){ # for each time point
      # print(BMD_tab[[i]]$BMDValues_filtered)
      if(!is.null(BMD_tab[[i]][[1]])){
        if(nrow(BMD_tab[[i]][[1]])>=1){
          
          mi = rbind(cbind(names(BMD_tab)[i],BMD_tab[[i]][[1]][,"Single column analysis"],   as.character(BMD_tab[[i]][[1]][,"MOD_NAME"]),"Single column analysis"),
                     cbind(names(BMD_tab)[i],BMD_tab[[i]][[1]][,"BMDL"],  as.character(BMD_tab[[i]][[1]][,"MOD_NAME"]),"BMDL"),
                     cbind(names(BMD_tab)[i],BMD_tab[[i]][[1]][,"BMDU"],  as.character(BMD_tab[[i]][[1]][,"MOD_NAME"]), "BMDU"))
          
          DF = rbind(DF, mi)
        }
      }
    }
    colnames(DF) = c("TimePoint", "Value","Model","ValueType")
    print(head(DF))
    DF = as.data.frame(DF)
    
    DF$TimePoint = factor(DF$TimePoint, level=sort(as.numeric(as.vector(unique(DF$TimePoint)))))
    DF$ValueType = factor(DF$ValueType, level=c("BMDL","Single column analysis","BMDU"))
    
    DF$Value = as.numeric(as.vector(DF$Value))
    
    p = ggplot(DF, aes(x=Model, y=Value, fill = ValueType)) + geom_boxplot(width=0.5) +facet_grid(. ~ TimePoint)
    ggplotly(p) %>%layout(boxmode = "group")
    
    gVars$BMD_BMDL_BMDU_by_model = p
  })
  
  output$BMD_dist_TP = renderPlotly({
    if(is.null(gVars$MQ_BMD)){ 
      print("Null BMD")
      return(NULL)
    }
    
    print(":")
    if(is.null(gVars$MQ_BMD_filtered)){
      BMD_tab <- gVars$MQ_BMD[[input$experiment_bmd]]#[[input$time_point_id_visual2]]$BMDValues_filtered
    }else{
      BMD_tab <- gVars$MQ_BMD_filtered[[input$experiment_bmd]]#[[input$time_point_id_visual2]]$BMDValues_filtered
    }
    
    DF = c()
    for(i in 1:length(BMD_tab)){
      print(BMD_tab[[i]][[1]])
      
      if(!is.null(BMD_tab[[i]][[1]])){
        if(nrow(BMD_tab[[i]][[1]])>=1){
          xx = cbind(names(BMD_tab)[i],BMD_tab[[i]][[1]][,"Single column analysis"])
          print(xx)
          DF = rbind(DF, xx)
        }
      }
    }
    
    print(head(DF))
    colnames(DF) = c("TimePoint", "Single column analysis")
    DF = as.data.frame(DF)
    DF$TimePoint = factor(DF$TimePoint, level=sort(as.numeric(as.vector(unique(DF$TimePoint)))))
    DF$BMD = as.numeric(as.vector(DF$BMD))
    # toRem = union(which(DF$BMD< as.numeric(input$bmd_th_min)), which(DF$BMD > as.numeric(input$bmd_th_max)))
    # 
    # if(length(toRem)>0){
    #   DF = DF[-toRem,]
    # }
    
    DF$BMD = as.numeric(as.vector(DF$BMD))
    #save(DF, file = "../BMD_hist.RData")
    print("in DF --------------------->>>>>>>>>>>>>>>>>>>>>")
    print(head(DF))
    
    p = ggplot(data=DF, aes(x = BMD)) + 
      geom_histogram(aes(y = ..density..)) + 
      geom_density(alpha = .2, fill="#FF6655") +
      facet_grid(. ~ TimePoint)
    
    gVars$BMD_dist_TP = p
    ggplotly(p)
  })
  
  output$BMD_pval_fitting = renderPlotly({
    if(is.null(gVars$MQ_BMD)){ 
      print("Null BMD")
      return(NULL)
    }
    
    print(":")
    if(is.null(gVars$MQ_BMD_filtered)){
      BMD_tab <- gVars$MQ_BMD[[input$experiment_bmd]]#[[input$time_point_id_visual2]]$BMDValues_filtered
    }else{
      BMD_tab <- gVars$MQ_BMD_filtered[[input$experiment_bmd]]#[[input$time_point_id_visual2]]$BMDValues_filtered
    }    
    
    DF = c()
    for(i in 1:length(BMD_tab)){
      if(!is.null(BMD_tab[[i]][[1]])){
        if(nrow(BMD_tab[[i]][[1]])>=1){
          DF = rbind(DF, cbind(names(BMD_tab)[i],BMD_tab[[i]][[1]][,"LOFPVal"]))
        }
      }
    }
    colnames(DF) = c("TimePoint", "LOFPVal")
    DF = as.data.frame(DF)
    DF$TimePoint = factor(DF$TimePoint, level=sort(as.numeric(as.vector(unique(DF$TimePoint)))))
    DF$LOFPVal = as.numeric(as.vector(DF$LOFPVal))
    
    p = ggplot(data=DF, aes(x = LOFPVal)) + 
      geom_histogram(aes(y = ..density..)) + 
      geom_density(alpha = .2, fill="#FF6655") +
      facet_grid(. ~ TimePoint)
    
    gVars$BMD_pval_fitting = p
    
    ggplotly(p)
  })
  
  output$NGTime = renderPlotly({
    if(is.null(gVars$MQ_BMD)){ 
      print("Null BMD")
      return(NULL)
    }
    
    # BMD_tab <- gVars$MQ_BMD_filtered[[input$experiment_bmd]]
    print(":")
    if(is.null(gVars$MQ_BMD_filtered)){
      BMD_tab <- gVars$MQ_BMD[[input$experiment_bmd]]#[[input$time_point_id_visual2]]$BMDValues_filtered
    }else{
      BMD_tab <- gVars$MQ_BMD_filtered[[input$experiment_bmd]]#[[input$time_point_id_visual2]]$BMDValues_filtered
    }    
    
    
    GL = list()
    NG = c()
    for(i in 1:length(BMD_tab)){
      BMD_tab2 <- BMD_tab[[i]][[1]]
      if(!is.null(BMD_tab2) & nrow(BMD_tab2)>0){
        GL[[names(BMD_tab)[i]]] =  BMD_tab2[,"Gene"]
        NG = rbind(NG, c(names(BMD_tab)[i], nrow(BMD_tab2)))
      }else{
        NG = rbind(NG, c(names(BMD_tab)[i], 0))
      }
    }
    
    colnames(NG) = c("TimePoint","NGenes")
    NG = as.data.frame(NG)
    NG$NGenes = as.numeric(as.vector(NG$NGenes))
    NG$TimePoint = factor(as.character(NG$TimePoint),levels = sort(as.numeric(as.vector(NG$TimePoint))))
    
    p<-ggplot(data=NG, aes(x=TimePoint, y=NGenes)) +
      geom_bar(stat="identity")
    
    ggplotly(p)
    gVars$NGTime = p
  })
  
  
  output$gene_bmd_plot = renderPlotly({
    if(is.null(gVars$MQ_BMD)){ return(NULL) }
    if(is.null(input$geneClust)) {return(NULL)}
    if(is.null(input$intersectionName)) {return(NULL)}
    
    print("gene_bmd_plot")
    print(":")
    if(is.null(gVars$MQ_BMD_filtered)){
      BMD_tab <- gVars$MQ_BMD[[input$experiment_bmd]]#[[input$time_point_id_visual2]]$BMDValues_filtered
    }else{
      BMD_tab <- gVars$MQ_BMD_filtered[[input$experiment_bmd]]#[[input$time_point_id_visual2]]$BMDValues_filtered
    } 
    if(length(BMD_tab)>1){
      
      c(GL, ItemsList) %<-%  venn_diagram_bmd_genes_across_time_point(BMD_tab)
      print("xxx")
      c(XX, hls, BMD_gene) %<-% create_gene_bmd_dataframe_and_cluster_genes_by_bmd(bmd_list=BMD_tab,
                                                                                   ItemsList=ItemsList,
                                                                                   hmethod=input$geneClustMeth, 
                                                                                   nclust=as.numeric(input$geneClust),
                                                                                   intersectionName=input$intersectionName)
      
      if(unique(XX$Cluster==0)){
        p = ggplot(data=XX, aes(x=BMD, y=Gene, color = BMD)) +
          geom_point() 
      }else{
        p = ggplot(data=XX, aes(x=TimePoint, y=BMD, group=Gene, color = Gene)) +
          geom_line(linetype = "dashed")+
          geom_point() + facet_grid(~Cluster)
      }
      
      gVars$gene_bmd_plot = p
      ggplotly(p)
    }
  })
  
  
  output$nclustvenn <- renderUI({
    if(is.null(gVars$MQ_BMD)){ return(NULL) }
    print("nclustvenn")
    
    print(":")
    if(is.null(gVars$MQ_BMD_filtered)){
      BMD_tab <- gVars$MQ_BMD[[input$experiment_bmd]]#[[input$time_point_id_visual2]]$BMDValues_filtered
    }else{
      BMD_tab <- gVars$MQ_BMD_filtered[[input$experiment_bmd]]#[[input$time_point_id_visual2]]$BMDValues_filtered
    }     
    
    if(length(BMD_tab)>1){
      c(GL, ItemsList) %<-%  venn_diagram_bmd_genes_across_time_point(BMD_tab)
      
      if(is.null(input$input$intersectionName)){
        ng = length(ItemsList[[length(ItemsList)]])
      }else{
        ng = length(ItemsList[[input$input$intersectionName]])
      }
      #
      selectInput(inputId = "geneClust", label = "Number of clusters",choices = as.list(1:ng), selected = 1)
      
    }
    
  })
  
  output$intersectionNameUI <- renderUI({
    if(is.null(gVars$MQ_BMD)){ return(NULL) }
    print("intersectionNameUI")
    
    print(":")
    if(is.null(gVars$MQ_BMD_filtered)){
      BMD_tab <- gVars$MQ_BMD[[input$experiment_bmd]]#[[input$time_point_id_visual2]]$BMDValues_filtered
    }else{
      BMD_tab <- gVars$MQ_BMD_filtered[[input$experiment_bmd]]#[[input$time_point_id_visual2]]$BMDValues_filtered
    }     
    
    if(length(BMD_tab)>1){
      c(GL, ItemsList) %<-%  venn_diagram_bmd_genes_across_time_point(BMD_tab)
      
      ng = length(ItemsList[[length(ItemsList)]])
      selectInput(inputId = "intersectionName", label = "Select Gene Set",choices = as.list(names(ItemsList)), selected = names(ItemsList)[[length(ItemsList)]])
      
    }
  })
  
  
  
   
  
    
   
  # remember to remove
  #load(file="../BMD_tab.RData")
  # mtcars2 = mtcars[, 1:8]
  # output$x3 = DT::renderDataTable( DT::datatable(BMD_tab), server = TRUE, selection = "single")
  # 
  # # print the selected indices
  # output$x4 = renderPrint({
  #   s = input$x3_rows_selected
  #   if (length(s)) {
  #     cat('These rows were selected:\n\n')
  #     cat(s, sep = ', ')
  #   }
  # })
  
   
  
  output$bmd_fitting_legend = renderPlot({
    if(length(input$BMD_table_rows_selected) == 0){
      return(NULL)
    }
    selectedrowindex = input$BMD_table_rows_selected[length(input$BMD_table_rows_selected)]
    selectedrowindex = as.numeric(selectedrowindex)
    
    par(mar = c(0,0,0,0))
    plot(1, type="n", xlab="", ylab="", xlim=c(0, 2), ylim=c(0, 2), xaxt = "n", yaxt = "n", frame.plot = FALSE)
    legend("center",  legend=c("Single column analysis","BMDL","BMDU","IC50/EC50"), fill = c("blue","red","green","black"), ncol=4, bty = "n")
  })
  
  
  observeEvent(input$upload_pheno_submit, {
    shiny::validate(
      need(!is.null(gVars$inputPh()), "No Phenodata File Provided!")
    )
    
    
    
    
    phTable <- gVars$inputPh()
     
    gVars$phTable <- phTable
    # gVars$totalSamples <- nrow(phTable)
    
    
    
    shinyBS::toggleModal(session, "importPhenoModal", toggle="close")
    shinyBS::updateButton(session, "import_pheno_submit", style="success", icon=icon("check-circle"))
    shinyBS::updateCollapse(session, "bsSidebar1", open="LOAD VOCABULARY", style=list("LOAD PHENODATA"="success","LOAD VOCABULARY"="warning"))
    
    
  })
  
  
  
#  observeEvent(input$import_pheno_submit,{
 #   var_sep_choices <- c("Tab", ",", ";", "Space", "Other")
   # output$selSep <- renderUI({
  #    #selectInput("sepS", "Column Seperator", choices=gVars$sepChoices, selected=gVars$sepChoices[1])
  #    selectInput("sepS", "Field Separator", choices=var_sep_choices, selected=var_sep_choices[1])
#    })
#  })
  
  
   
  
  

  
  output$filtered <- DT::renderDataTable({
     shiny::validate(
       need(!is.null(gVars$original_phTable), "No Phenodata File Provided!")
     )
    if(is.null(gVars$original_phTable))
      return(NULL)
    
    phTable <- gVars$original_phTable[[1]] 
    DT::datatable(phTable, filter="none",
                  options = list(
                    search = list(regex=TRUE, caseInsensitive=FALSE),
                    scrollX=TRUE,
                    ordering=F
                  )
    )
  },server=TRUE)
  
  
  
  output$gExpMat <- DT::renderDataTable({
    
    if(is.null(gVars$inputGx))
      return(NULL)
    
    # if((class(gVars$inputGx) != "matrix") || (class(gVars$inputGx) != "data.frame"))
    #   return(NULL)
    
    inputGx <- gVars$inputGx[[1]] %>% setNames(., c("Term","Term Synonyms","Instance","Instance Synonyms"))
    DT::datatable(inputGx, filter="none",
                  options = list(
                    search = list(regex=TRUE, caseInsensitive=FALSE),
                    scrollX=TRUE,
                    ordering=F
                  )
    )
  },server=TRUE)
  
  observe({
    
    
    if(is.null(input$fPheno)){
      shinyjs::disable("load_pheno_submit")
    }else{
      shinyjs::enable("load_pheno_submit")
    }
    
    
    
    if(is.null(gVars$phLoaded)){
      shinyjs::hide("phenoPreviewDiv")
    }else{
      shinyjs::show("phenoPreviewDiv")
    }
  })
  
  
  
  output$loading_text <- renderText({
    #loadingText <- gVars$loadingText
    loadingText <- "LOADING"
    return(loadingText)
  })
  
 
  
  
  
  #output$selQuote <- renderUI({
  #  selectInput("quote", "Quotes", choices=gVars$quoteChoices, selected=gVars$quoteChoices[1])
  #})
  
  output$selSep <- renderUI({
    #selectInput("sepS", "Column Seperator", choices=gVars$sepChoices, selected=gVars$sepChoices[1])
    selectInput("sepS", "Field Separator", choices=gVars$sepChoices, selected=gVars$sepChoices[1])
  })
  
  
  output$selGxSep <- renderUI({
    selectInput("gxSepS", "Column Seperator", choices=gVars$sepChoices, selected=gVars$sepChoices[1])
  })
  
  output$selGxQuote <- renderUI({
    selectInput("gxQuote", "Quotes", choices=gVars$quoteChoices, selected=gVars$quoteChoices[1])
  })
  
 
  
  
  
   
  
  
  
  
   
  
     
  
  
  
  
  
  
  
  
  
   
  
  
    
  
  
  
 
    
   
  
  
  
  
  output$exportRpt <- shiny::downloadHandler(
    #arrange new file name
    current_df_file_name <- gVars$current_df_file_name %>% 
      sub('.[^.]*$', '', .) %>%
      paste(., collapse = "_"),
    
    filename = function(){
      paste("ESPERANTO_Analysis_Report_", current_df_file_name, Sys.Date(), '.html', sep='')
    },
    
    content = function(con){
      #start loading screen
      shinyjs::html(id="loadingText", "CREATING ANALYSIS REPORT")
      shinyjs::show(id="loading-content")
      
      on.exit({
        shinyjs::hide(id="loading-content", anim=TRUE, animType="fade")
      })
      #Disable Warning
      oldw <- getOption("warn")
      options(warn = -1)
      tempReport <- file.path((tempdir()), "report.Rmd")
      file.copy("report.Rmd", tempReport, overwrite=TRUE)
      
      
      #params <- list(gVars=gVars, input=input)
      params <- list(gVars=gVars, input=input, srcDir=srcDir)
      rmarkdown::render(tempReport, output_file=con,
                        params=params,
                        envir=new.env(parent=globalenv())
      )
      
      #Enable Warning
      options(warn = oldw)
    }
  )
  
})

