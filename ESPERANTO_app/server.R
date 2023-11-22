options(java.parameters = c("-XX:+UseConcMarkSweepGC", "-Xmx8192m"))
srcDir <- dirname(getSrcDirectory(function(x){x})) #Get source directory

library(magrittr)
library(dplyr) 
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
library(plotly)
library(shiny)
library(shinyjs)
library(shinyBS)
library(shinyalert)

library(knitr)
library(kableExtra)
library(rhandsontable)
library(shinydashboard)
library(shinyFeedback)

#Get source directory
source("ESPERANTO_app/functions/read_excel_allsheets.R")
source("ESPERANTO_app/functions/column_label_finder.R")
source("ESPERANTO_app/functions/duplicate_finder.R")
source("ESPERANTO_app/functions/single_col_recode_finder.R")
source("ESPERANTO_app/functions/colorwords.R")
source("ESPERANTO_app/functions/convert_df_to_dict.R")
source("ESPERANTO_app/functions/convert_dict_to_df.R") 
source("ESPERANTO_app/functions/label_shortener.R")
source("ESPERANTO_app/functions/undo_related_funcs.R")



# this instruction increase the maximum size of the file that can be uploaded in a shiny application
options(shiny.maxRequestSize=300*1024^2) 

phTable <- NULL
gxTable <- NULL




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
    sha256_stringID=NULL,     #stores the session identifier according to digest package sha256 algorithm
    original_phTable = NULL,  # setting original unmodified phenodata matrix
    all_original_colnames=NULL,      #setting the colnames of the original phenodata dataset
    working_status=NULL,     #setting the working mode,F for multiple dataframe, T for singles
    GLP_status=NULL,         #setting the abilitation of GLP mode, F if GLP active, T otherwise
    current_df_file_name=NULL, #Stores the name of the file currently processed
    label_first_version=collections::dict(NULL),   #in case of undoing recoding involving the original column names, stores those values before undo 
    dict_relab=collections::dict(NULL),
    username=NULL,         # current user name
    all_curators=NULL,     #list of all users who had a role during curation
    
    phTab_progress=NULL,
    sampleColID = FALSE,      # setting sample column number
    # doseColID = NULL,         # setting dose column number
    TPColID=NULL,             # setting TP column number
    inputGx=NULL,             # setting gene expression matrix
    compact_voc=NULL,         # storing compact version to display of the vocabulary
    dkeys=NULL,               # setting a more accessible vocabulary 
    original_dkeys=NULL,      # to allocate the original version of the vocabulary, when starting the curation 
    always=NULL,              # stores the columns always present in every dataset
    label=NULL,
    processedL=NULL,               # setting the colnames renamed or rejected in relation to an allowed label
    tmpLr = NULL,             #stores a temporary modified copy of the colnames  rejected in relation to an allowed label
    tmpLa = NULL,             #stores a temporary modified copy of the colnames renamed  in relation to an allowed label
    tmpdfL=NULL,       #stores temporary modified df with colnames renamed 
    relab_op_list=NULL,       # setting the list to contain all operations related to relabelling
    relabelling_ops_dataframe=NULL, #sotres all the steps of the relabelling phase as datframe
    dupl_decisions=NULL,     #stores the decision about duplicates, pos=1 the kept, following positions with deleted col names
    dupl_op_list=NULL,       # setting the list to contain all operations related to moval
    dupl_ops_dataframe=NULL, #sotres all the steps of the moval phase as datframe
    
    
    glp_comment_rep =NULL, 
    
    processedD=NULL,        #setting the potential column duplicates
    selected_cols=NULL, 
    potential_choices=NULL,     #stores names of identical columns among which the user should chose
    all_identical_cols=NULL, #stores all the different groups of identical columns
    deleted_colname = NULL,  #stores the name of the column completely deleted
    delete_op_list =NULL,
    dupl_op_list=NULL,       # setting the list to contain all operations related to duplicate deletion
    tmp_pt_value =NULL,        #temporarly sotres the pt text 
    
    recod_op_list=NULL,       # setting the list to contain all operations related to duplicate removal
    recod_ops_dataframe=NULL, #sotres all the steps of the duplicate removal phase as datframe
    rejected_full_col=NULL,   #temporarily stores the column when its full_recoding was rejected
    subset_df_recode_ops=NULL,   #stores the recoding ops of the chosen type (recoded, or rejected recoding of the content)
    recodeRJ_op_list=NULL, # stores all the column that were relabelled but whose content recoding was fully rejected
    originalcontent=NULL, #stores the original content value during recoding
    processedR= NULL,      #storing the name of the columns recoded   
    clean_recoding_pairs=NULL, #stroes temporary the column content to remove from recoding pair options
    map_recoding= NULL,    #stores the content and its recoded version 
    map_for_plot=NULL,   #stores values for plotting in the recoding section
    processedLIST_content2RECODE = collections::dict(NULL),  #storing the proposals for content recoding for each column  
    processedLIST2REC=NULL,
    tmp_recode_rj_element=NULL,    #stores the lab,labsyn,cont,contsyn vector to complete the recode_rj information for the undo
    tmp_stored_df=NULL,           #tmp storage for the current df with modifications
    last5list = NULL,            #container of last ops performed on the dataset during curation
    undo_message=NULL ,    #message to store in report after undoing some ops
    action_to_undo=NULL,   #stores the action to undo in relabelling in total undo section
    list_cols_tmp=NULL,
    recap_dupl = NULL,     #stores the recap (dupl kept, dupl deleted, dupl renamed) for every selected action in undo section
    pt_glp_list =list(),    #stores any pt/glp information for each step
    orig_lab=NULL, #stores label for vocabolary section
    orig_cont=NULL, #stroes content for vocabulary section 
    skip_marker=NULL, #stores whether the the content entry must be allocated in the temporary vocabulary after skip full recoding content (T) or after recoding (F)
    later_voc_upd_df=NULL, #stores the new items to check again before adding definitively to the vocabulary
    later_voc_upd_row_tmp=NULL, #stores temporarily the last op for the vocabulary
    later_voc_upd_row_tmp_tmp=NULL, #stores what will go in later_voc_upd_row_tmp to pass to another module 
    safe=NULL,                 #sub-group of the safe prestored new words to check (are the already present in the vocabulary)
    outsider=NULL,          #sub-group of the fast check" prestored new words to check (are both allowed words completely new )
    enrichments =NULL,    #sub-group of prestored new words to check with a bit more attentions (different combinations of synonyms and allowed)
    dt_temporary_cases_voc=NULL, #temporary store f the selected pre-stored (safe/outsider/enrichemnts)temporary cases for the vocabulary update
    empty_prestored_voc_tables=c("Safe","Fast check","Slow check"), #stores which group safe/check/overview was cpompleted processed -no more terms inside 
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
    
    tmp_new_names=NULL,      #stores temporarily the new column names inserted in the specials 
    agi_splitted_new_cols=NULL, #stores the columns created by splitting AGILENT filneame.txt string
    AGIsplit_msg=NULL,      #stores the message for pt procedural track in agilent
    addEmpty_msg=NULL,      #stores the message for pt procedural track in adding empty columns
    
    multidf_list=NULL, #sores a list containing  multiple datsets
    multidf=NULL, #stores the join of multiple dataframes
    grouping_multi_tables=c("Safe","Fast check","Slow check"),    #entries for a dropdown
    multi_safe=NULL,      # once retrieved from joining the multiple dataframes, it stores entries where colname and column content are in the vocabulary 
    
    multi_slow_check=NULL,      # once retrieved from joining the multiple dataframes, it stores entries where one or more entries are not present in the vocabulary 
    multi_ACmsg=NULL,               #stores messages of multidf merging accept
    multi_accept_colnames=NULL,    #stores accepted colnames from multidf merging
    multi_ISSmsg=NULL,               #stores messages of multidf issue report
    issue_tracking_ID =NULL,          #stores the step number to track the issue message from procedural track to issue report
    save_issue_blocks_list=NULL,      # stores the amount of entry classified as issues
    multi_issue_colnames=NULL,    #stores issue colnames from multidf merging
    multi_col_decisions=NULL,    #stores the decision above each column 
    col_issue_cases=NULL,       #stores the entries of the selected column that make the column an "issue" needing a second round of curation
    multi_flag=NULL,             #stores the type of operation performed to allocate the GLP comment in the right list
    correct_voc_mode=NULL,      #indicate to activate the modality in which the vocabulary can be modified to correct an error
    support_for_correction= NULL, 
    
    plot_tmp=NULL, #stores values for plot data
    
    
  )
  
  gVars$sepChoices <- c("Tab", ",", ";", "Space","", "Other")
  gVars$quoteChoices <- c(NA, "SINGLE", "DOUBLE")
  
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
    gVars$username <- input$enter_username
    gVars$all_curators <- c(gVars$all_curators, input$enter_username)
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
      gVars$pt_glp_list <- c(gVars$pt_glp_list, paste0("USER - Current session was carried over by: ",input$enter_username, " (", Sys.Date(), ")"))
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
        column(1, align="left",shinyBS::bsButton("import_session", label="Load", style="info"))
      ),
      footer =  modalButton("Cancel") 
    )
  }
  
  
  
  CSS <- function(colors){
    template <- "
.checkboxGroupButtons div.btn-group:nth-child(%s) button {
  background: %s !important;
  color: white !important;
}"
    paste0(
      apply(cbind(seq_along(colors), colors), 1, function(vc){
        sprintf(template, vc[1], vc[2])
      }),
      collapse = "\n"
    )
  }
  
  output$css <- renderUI({
    tags$style(HTML(CSS(cols_user())))
  })
  
  
  observeEvent(input$working_mode, {
    req(!is.null(input$working_mode))
    gVars$working_status <- input$working_mode
    if(input$working_mode) {   #T such as Single mode
      shinyBS::updateButton(session, "import_pheno_submit", label="Import Single PhenoData", disabled = FALSE)
      shinyBS::updateButton(session, "relabelling_block_button", disabled = FALSE)
      shinyBS::updateButton(session, "dupl_removal_block_button", disabled = FALSE)
      shinyBS::updateButton(session, "recoding_button", disabled = FALSE)
      gVars$pt_glp_list <- c(gVars$pt_glp_list, "MODE - Current session takes SINGLE dataset as input.")
      
    } else { #F, such as multiple mode
      shinyBS::updateButton(session, "import_pheno_submit", label="Import Multiple PhenoData", disabled = FALSE)
      shinyBS::updateButton(session, "relabelling_block_button", disabled=TRUE)
      shinyBS::updateButton(session, "dupl_removal_block_button", disabled=TRUE)
      shinyBS::updateButton(session, "recoding_button", disabled=TRUE)
      gVars$pt_glp_list <- c(gVars$pt_glp_list, "MODE - Current session takes MULTIPLE datasets as input.")
      
    }
    
  })
  
  
  output$disabledTab <- renderText({req (gVars$working_status == FALSE)
    "<b><i><big><big>This tab is not available in the current working mode</big></big></b></i>"})
  output$disabledTab_ph <- renderText({req (gVars$working_status == FALSE)
    "<b><i><big><big>This tab is not available in the current working mode</big></big></b></i>"})
  output$disabledTab_multi <- renderText({req (gVars$working_status == TRUE)
    "<b><i><big><big>This tab is not available in the current working mode</big></big></b></i>"}) 
  
  
  
  
  #save current session
  observeEvent(input$dosave_session, {
    req(input$dosave_session)
    Save_start <- showNotification(paste("Message:", "Saving the session will take some time, please wait"), duration = NULL)
    #arrange new file name
    if(gVars$working_status == TRUE ){ # single 
      current_df_file_name <- gVars$current_df_file_name %>% 
        sub('.[^.]*$', '', .) %>%
        paste(., collapse = "_")
    } else { # multiple  
      current_df_file_name <- "multiples"
    }
    newfile = paste0(format(Sys.time(), "%Y-%m-%d_%H-%M-%S"), "_",
                     current_df_file_name, "_current_session.RData")
    
    #glp/pt comment
    gVars$pt_glp_list <- c(gVars$pt_glp_list, paste("SAVED the current session: as",newfile))
    save_session <- gVars
    
    save(save_session, file=newfile)#file = file_path)
    Save_done <- showNotification(paste("Message:", "The session has been saved"), duration = NULL)
    data$whichBAR <- ifelse(isTRUE(gVars$working_status), "updSession" ,"updSession_multi")
    data$stop_dwn <- TRUE
    
    shinyBS::updateCollapse(session, "bsSidebar0_5", close="SESSION MANAGEMENT")
  })
  
  
  #Load data saved session
  observeEvent(input$import_session, {
    req(!is.null(input$load_sessionfile))
    gc()   #cleans space
    load(input$load_sessionfile$name)
    gc()
    previous_session_gVars <<- save_session
    #overwriting of gVars
    lapply(names(previous_session_gVars), function(k) {
      gVars[[k]] <- previous_session_gVars[[k]]
    })  
    
    Load_done <- showNotification(paste("Message:", "The session has been restored"), duration = NULL)
    
    # fix sidebar buttons
    if(!gVars$GLP_status ) { 
      shinyBS::updateButton(session, "GLP_inactivation_mode", label= " GLP Mode Enabled",  style="success", icon=icon("check-circle")) 
      shinyBS::updateCollapse(session, "bsSidebar1", close="STRUCTURE HOMOGENIZATION", style=list("GLP MODE"="success"))
    }
    if(!is.null(gVars$phTable[[1]]) |!is.null(gVars$multidf) ){
      shinyBS::updateButton(session, "import_pheno_submit", style="success", icon=icon("check-circle"), disabled=TRUE)
      shinyBS::updateCollapse(session, "bsSidebar1", close="LOAD VOCABULARY", style=list("LOAD PHENODATA"="success"))
    }
    
    if(!is.null(gVars$dkeys)){   #vocab
      shinyBS::updateButton(session, "import_voc_submit", style="success", icon=icon("check-circle"), disabled=TRUE)
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
    output$working_mode_scenario <- reactive({  gVars$working_status}) 
    outputOptions(output, "working_mode_scenario", suspendWhenHidden=FALSE)
    
    #new comment to pt/GLP
    timing <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")
    gVars$pt_glp_list <- c(gVars$pt_glp_list, paste("RESTORED the previous session at ",timing))
    gVars$pt_glp_list <- c(gVars$pt_glp_list, paste0("USER - From now, the restored session is carried over by: ",input$enter_username," (", Sys.Date(), ")"))
    gVars$all_curators <- c(gVars$all_curators, input$enter_username)
    gVars$username <- input$enter_username
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
        column(12, align="center",shinyBS::bsButton("reset_session", label="Reset", style="danger", icon=icon("exclamation-circle")))
      ),
      footer =  modalButton("Cancel")
      
    )
  }
  
  
  observeEvent(input$reset_session,{
    #reset gVars to NULL
    lapply(names(gVars), function(k) {
      gVars[[k]] <- NULL
      
      gVars$pt_glp_list =list()
      gVars$voc_updating_report =list()
      gVars$voc_issues_report = list()
      gVars$processedLIST_content2RECODE = collections::dict(NULL)
      
      grouping_multi_tables=c("Safe","Fast check","Slow check")
      empty_prestored_voc_tables=c("Safe","Fast check","Slow check")
    })
    
    
    # fix sidebar buttons
    shinyBS::updateButton(session, "GLP_inactivation_mode", label= " GLP Mode Disabled",  style="danger", icon=icon("exclamation-circle")) 
    shinyBS::updateCollapse(session, "bsSidebar1", close="STRUCTURE HOMOGENIZATION", style=list("GLP MODE"="warning"))
    
    shinyBS::updateButton(session, "import_pheno_submit", style="danger", icon=icon("exclamation-circle"))
    shinyBS::updateCollapse(session, "bsSidebar1", close="LOAD VOCABULARY", style=list("LOAD PHENODATA"="warning"))
    
    shinyBS::updateButton(session, "import_voc_submit", style="danger", icon=icon("exclamation-circle"))
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
    
    gVars$current_dict_file_name <- gxFile$name
    gx = read_excel_allsheets(filename = gxFile$datapath,tibble = FALSE)  
    gVars$inputGx <- gx
    # compact version of the vocabulary to display
    gVars$compact_voc <- gx[[1]]
    
    # loading of allowed labels and related synonyms.
    dict_keys <- data.frame(gx[[1]]) %>% 
      separate_rows(., lab_syn, sep=",") %>% 
      separate_rows(., allowed_features, sep=",") %>% 
      separate_rows(., syn_features, sep="\\|") %>% .[order(.$label),]
    gVars$dkeys <- dict_keys
    #creates the original starting vocabulary version
    if (is.null(gVars$original_dkeys)){
      gVars$original_dkeys <- dict_keys}
    
    gVars$pt_glp_list <- c(gVars$pt_glp_list, paste0("VOCABULARY VERSION FILE - ", gxFile$name)) 
    
    shinyBS::toggleModal(session, "importGxModal", toggle="close")
    shinyBS::updateButton(session, "import_voc_submit", style="success", icon=icon("check-circle"))
    shinyBS::updateCollapse(session, "bsSidebar1", open="GLP MODE", style=list("LOAD VOCABULARY"="success","STRUCTURE HOMOGENIZATION"="warning"))
    shiny::updateTabsetPanel(session, "display",selected = "gExpTab")
  })
  
  
  
  observeEvent(input$GLP_inactivation_mode, {
    if (!input$GLP_inactivation_mode){     #GLP mode active
      gVars$pt_glp_list <- c(gVars$pt_glp_list, "GLP mode enabled") 
      shinyBS::updateButton(session, "GLP_inactivation_mode", label= " GLP Mode Enabled",  style="success", icon=icon("check-circle"))
      shinyBS::updateCollapse(session, "bsSidebar1", open="STRUCTURE HOMOGENIZATION", style=list("GLP MODE"="success","STRUCTURE HOMOGENIZATION"="warning"))
      gVars$GLP_status <- FALSE
    }
    else {
      gVars$pt_glp_list <- c(gVars$pt_glp_list, "GLP mode disabled") 
      shinyBS::updateButton(session, "GLP_inactivation_mode", label=" GLP Mode Disabled",  style="danger", icon=icon("exclamation-circle"))
      shinyBS::updateCollapse(session, "bsSidebar1", close="STRUCTURE HOMOGENIZATION", style=list("GLP MODE"="warning","STRUCTURE HOMOGENIZATION"="warning"))
      gVars$GLP_status <- TRUE 
    }    
    
  })
  
  
  observeEvent(input$import_session, {
    req(!is.null(gVars$GLP_status))
   if (!gVars$GLP_status){     #GLP mode active
      click("GLP_inactivation_mode")
    }
 
  })
  
  
  
  
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
    gVars$all_original_colnames <- colnames(gVars$original_phTable[[1]])
    
  })
  
  
  #upload an excel file with N sheets such as the number of experiments that we are considering
  
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
  
  
  
  
  gVars$phColTypes <- reactive({
    if(is.null(gVars$inputPh()))
      return(NULL)
    
    phTable <- gVars$inputPh()[[1]]
    colNames <- list("unch"=NULL,"usu"=NULL)
    
    always <- scan("./ESPERANTO_app/files/always.txt", character(), quote = "", skip=1)
    gVars$always <- always
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
    req(!is.null(gVars$inputPh()))
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
  
  
  # enable/disable content homogenization ops button
  #relabelling
  observeEvent (c(input$accept_lab_candidate, input$reject_lab_candidate),{
    req(!is.null(gVars$working_status))
    shinyBS::updateButton(session, "dupl_removal_block_button", disabled = TRUE)
    shinyBS::updateButton(session, "recoding_button", disabled = TRUE)
  })
  observeEvent (input$glp_pt_button_relab,{
    req(!is.null(gVars$working_status))
    shinyBS::updateButton(session, "dupl_removal_block_button", disabled = FALSE)
    shinyBS::updateButton(session, "recoding_button", disabled = FALSE)
  })
  #duplicate removal
  observeEvent ( input$submit ,{
    req(!is.null(gVars$working_status))
    shinyBS::updateButton(session, "relabelling_block_button", disabled = TRUE)
    shinyBS::updateButton(session, "recoding_button", disabled = TRUE)
  })
  observeEvent (input$glp_pt_button_dupl,{
    req(!is.null(gVars$working_status))
    shinyBS::updateButton(session, "relabelling_block_button", disabled = FALSE)
    shinyBS::updateButton(session, "recoding_button", disabled = FALSE)
  })
  #content homogenization
  observeEvent ( c(input$save_recoding, input$reject_full_recoding, 
                   input$step_submit_del, input$save_agil_button, input$add_Fempty_cols_button,
                   input$run_separ, input$run_separ_regex ),{
                     req(!is.null(gVars$working_status)) 
                     shinyBS::updateButton(session, "relabelling_block_button", disabled = TRUE)
                     shinyBS::updateButton(session, "dupl_removal_block_button", disabled = TRUE)
                   })
  observeEvent (c(input$glp_pt_button_rec,
                  input$glp_pt_button_rec_del, input$glp_pt_button_AGIsplit, input$glp_pt_button_addEmpty, 
                  input$glp_pt_button_split),{
                    req(!is.null(gVars$working_status))
                    shinyBS::updateButton(session, "relabelling_block_button", disabled = FALSE)
                    shinyBS::updateButton(session, "dupl_removal_block_button", disabled = FALSE)
                  })
  
  
  
  #enable,disable, "session management" sidebar, when relabelling, duplicate removal and content homogenization operations in process
  observeEvent (c(input$accept_lab_candidate, input$reject_lab_candidate, input$save_recoding, input$reject_full_recoding,
                  input$step_submit_del, input$save_agil_button, input$add_Fempty_cols_button, 
                  input$run_separ, input$run_separ_regex
  ),{ 
    req(!is.null(gVars$working_status))
    shinyjs::disable ("bsSidebar0_5")
  })          
  
  observeEvent (c(input$glp_pt_button_relab, input$glp_pt_button_dupl, input$glp_pt_button_rec,
                  input$glp_pt_button_rec_del, input$glp_pt_button_AGIsplit, input$glp_pt_button_addEmpty, 
                  input$glp_pt_button_split
  ),{
    req(!is.null(gVars$working_status))
    shinyjs::enable("bsSidebar0_5")
    
    #enables all options in radicont radiobuttons
    shinyjs::enable(selector = "#step_choice [type=radio][value =1 ]")
    shinyjs::runjs("$('#step_choice [type=radio][value =1]').parent().addClass('enabled').css('opacity', 1)")
    shinyjs::enable(selector = "#step_choice [type=radio][value =2 ]")
    shinyjs::runjs("$('#step_choice [type=radio][value =2]').parent().addClass('enabled').css('opacity', 1)")
    shinyjs::enable(selector = "#step_choice [type=radio][value =3 ]")
    shinyjs::runjs("$('#step_choice [type=radio][value =3]').parent().addClass('enabled').css('opacity', 1)")
    
  })
  
  
  # enable, disable alternative content homogenization options, if the third is selected
  # step_choice = c("Delete" = 1, "Modify and/or Save" = 2, "Specials" =3)
  #keep "1"(delete of "step_choice" radiocont enabled and it disables 2(modify/save) and 3(specials) radiobutton options 
  observeEvent(input$step_submit_del, { 
    req(!is.null(gVars$phTable))
    shinyjs::enable(selector = "#step_choice [type=radio][value =1 ]")
    shinyjs::runjs("$('#step_choice [type=radio][value =1]').parent().addClass('enabled').css('opacity', 1)")
    shinyjs::disable(selector = "#step_choice [type=radio][value =2 ]")
    shinyjs::runjs("$('#step_choice [type=radio][value =2]').parent().addClass('disabled').css('opacity', 0.4)")
    shinyjs::disable(selector = "#step_choice [type=radio][value =3 ]")
    shinyjs::runjs("$('#step_choice [type=radio][value =3]').parent().addClass('disabled').css('opacity', 0.4)")
  })
  
  #keep "2"(modify/save) of "step_choice" radiocont enabled and it disables 1(delete) and 3(specials) radiobutton options 
  observeEvent(c(input$save_recoding, input$reject_full_recoding), {
    req(!is.null(gVars$phTable))
    shinyjs::enable(selector = "#step_choice [type=radio][value =2 ]")
    shinyjs::runjs("$('#step_choice [type=radio][value =2]').parent().addClass('enabled').css('opacity', 1)")
    shinyjs::disable(selector = "#step_choice [type=radio][value =1 ]")
    shinyjs::runjs("$('#step_choice [type=radio][value =1]').parent().addClass('disabled').css('opacity', 0.4)")
    shinyjs::disable(selector = "#step_choice [type=radio][value =3 ]")
    shinyjs::runjs("$('#step_choice [type=radio][value =3]').parent().addClass('disabled').css('opacity', 0.4)")
  })
  
  #keep "3"(specials) of "step_choice" radiocont enabled and it disables 1(delete) and 2(modify/save) radiobutton options 
  observeEvent(c(input$save_agil_button, input$add_Fempty_cols_button,  input$run_separ, input$run_separ_regex), {
    req(!is.null(gVars$phTable))
    shinyjs::enable(selector = "#step_choice [type=radio][value =3 ]")
    shinyjs::runjs("$('#step_choice [type=radio][value =3]').parent().addClass('enabled').css('opacity', 1)")
    shinyjs::disable(selector = "#step_choice [type=radio][value =1 ]")
    shinyjs::runjs("$('#step_choice [type=radio][value =1]').parent().addClass('disabled').css('opacity', 0.4)")
    shinyjs::disable(selector = "#step_choice [type=radio][value =2 ]")
    shinyjs::runjs("$('#step_choice [type=radio][value =2]').parent().addClass('disabled').css('opacity', 0.4)")
  })
  
  
  observeEvent(c(input$step_choice, input$pick_cols_recoding),{
    req(!is.null(gVars$phTable))
    req(!is.null(input$pick_cols_recoding)) 
    if(identical(input$pick_cols_recoding,"")){
      disable("step_submit_del")
      disable("step_submit")
    } else {
      enable("step_submit_del")
      enable("step_submit")
      enable("step_submit_spec")
    }
  })
  
  
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
    
    pickerInput(
      inputId = "pick_col_labelling",
      label = "Select the column(s) of the dataset you wish to display (Vocabulary label -> Dataset column name):",
      choices = tmp$c,
      choicesOpt=list(content=tmp$dropdownText),
      options = list(`actions-box` = TRUE, 
                     `selected-text-format` = paste0("count > ", length(colnames(data$fnCOL_LAB_FINDER)) - 1),
                     `count-selected-text` = "All pairs",
                     liveSearch = TRUE,
                     liveSearchPlaceholder = "Search entry"),   # build buttons for collective selection
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
    req(!is.null(gVars$phTable), !is.null(gVars$inputGx))
    
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
      gVars$tmpLr <- chosen_match
      temp <- data$fnCOL_LAB_FINDER 
    }
    
    
    #cleaning of text box
    #update buttons
    shinyBS::updateButton(session, "reject_lab_candidate", style="success")
    disable("accept_lab_candidate")
    disable("reject_lab_candidate")
    disable("undo_relab_button")
    enable("glp_pt_button_relab")
    disable("pick_col_labelling")
    
    
    #updates the PT box with what was rejected
    Value <- paste0("'",chosen_lab, "' is rejected as relabelling candidate for '",chosen_match,"'.")
    gVars$pt_glp_list <- c(gVars$pt_glp_list, Value) 
    output$pt_label <- renderText({Value})
  })
  
  
  #by pressing add/glp button, it reupdates all the buttons and perform the relabelling/or not
  observeEvent(input$glp_pt_button_relab, {
    if(!gVars$GLP_status){    #such as GLPmode activated 
      showModal(dataModal())
      disable("undo_relab_button")
    } else { # only procedural track report
      # re-update buttons 
      shinyBS::updateButton(session, "accept_lab_candidate", style="info")
      shinyBS::updateButton(session, "reject_lab_candidate", style="info")
      shinyBS::updateButton(session, "glp_pt_button_relab", disabled=TRUE)
      enable("accept_lab_candidate")
      enable("reject_lab_candidate")
      enable("undo_relab_button")
      enable("pick_col_labelling")
      
      
      #re-update boxes
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
      shinyBS::bsButton("glp_store_comment", label="Add", style="info"),
      
      if (failed){
        div(br(),tags$b("In GLP mode, you must justify your decisions. Please do."), style = "color: red;")},
      
      footer =  NULL#modalButton("Cancel")
    )
  }
  
  observeEvent(input$testami,{
    browser()
  })
  
  # When Add button(glp_store_comment) is pressed, it tests if the textareainput box is not empty. 
  # If successful, remove the modal, otherwise it shows another modal with a failure message.
  observeEvent(input$glp_store_comment, {
    # Check that box for glp comments is filled
    if (input$glp_comments_box != "") {
      tabulated_comment <- paste0("\t","COMMENT: ",input$glp_comments_box)
      list_elem <- list(tabulated_comment)
      gVars$pt_glp_list <- c(gVars$pt_glp_list, list_elem)
      removeModal()  
      if (!is.null(input$step_choice)){
          if (input$step_choice == 1) {
            shinyjs::delay(200, toggleModal(session, "modalstep1", "close"))
          } else if (input$step_choice == 2) {
            shinyjs::delay(200, toggleModal(session, "modalstep", "close"))
            shinyBS::updateButton(session, "save_recoding", style="info", disabled=FALSE)
            shinyBS::updateButton(session, "reject_full_recoding", style="info", disabled=FALSE)
            
            updateRadioButtons(session, "store_labels", 
                               choices = list("as Label Synonym" = 1, "Do not store"=2), 
                               selected = character(0))
            
            updateRadioButtons(session, "store_contents",
                               choices = list("as Content Synonym" = 3, "Do not store"=4),
                               selected = character(0))
            
            shinyBS::updateButton(session, "store_button", style="info", disabled=TRUE)
            
            shinyBS::updateButton(session, "glp_pt_button_rec", disabled=TRUE)
          } else if (input$step_choice == 3) {
                shinyjs::delay(200, toggleModal(session, "SpecialOPmodal", toggle="close"))
                if (input$navlist_specialOPS %in% "Agilent") {
                  updateSelectizeInput(session,
                                       inputId = "pick_cols_recoding",
                                       #gVars$list_cols_tmp options present before",
                                       choices = c(gVars$list_cols_tmp, "gsm", "slide", "array"),
                                       selected = character(0),
                                       server=TRUE)
                  updateButton(session,  "glp_pt_button_split",disabled = TRUE)
                  updateButton(session,  "test_agil_button",disabled = FALSE)
                  updateButton(session,  "save_agil_button", style="info", disabled = TRUE)
                  updateButton(session,  "glp_pt_button_AGIsplit", style="info", disabled = TRUE) 
                  
                  output$pt_AGIsplit <- renderText({"<i><small>Waiting for action!</small></i>"})
                  gVars$support_for_correction <- NULL
                
              }else if (input$navlist_specialOPS %in% "Adding empty columns") {
                  updateSelectizeInput(session,
                                       inputId = "pick_cols_recoding",
                                       #gVars$list_cols_tmp options present before",
                                       choices = c(gVars$list_cols_tmp, gVars$tmp_new_names[[1]]),
                                       selected = character(0),
                                       server=TRUE)
                  
                  updateNumericInput(session,inputId = "new_cols", label= HTML("Column(s) to add"), min=0, value=0) 
                  updateTextInput(session, inputId = "names_added_cols","Name new column(s)", value="")
                  updateButton(session,  "add_Fempty_cols_button", style="info",disabled = FALSE)
                  updateButton(session,  "glp_pt_button_addEmpty", style="info",disabled = TRUE)
                  output$pt_addEmpty <- renderText({"<i><small>Waiting for action!</small></i>"})
                  gVars$support_for_correction <- NULL
               } else if (input$navlist_specialOPS %in% "Splitting column") {
                     #Separator and common
                     updateSelectizeInput(session,
                                          inputId = "pick_cols_recoding",
                                          #gVars$list_cols_tmp options present before",
                                          choices = c(gVars$list_cols_tmp, gVars$tmp_new_names),
                                          selected = character(0),
                                          server=TRUE) 
                     updateCheckboxInput(session, "other_col_split", "Select manually another specific column", value = FALSE)
                     
                     updateButton(session,  "glp_pt_button_split",disabled = TRUE)
                     updateButton(session,  "reset_test_table",disabled = TRUE)
                     updateButton(session,  "run_separ",disabled = TRUE,style="info") 
                     updateButton(session,  "test_separ",disabled = TRUE)
                     
                     output$test_splitted_cols <-NULL
                     updateTextAreaInput(session, inputId="PT_store_split", "Procedural track", value = "Waiting for action!")  
                     
                     updateSelectizeInput(session,
                                          inputId = "type_of_separator", 
                                          choices = c("Tab", ",", ";", "Space","", "Other"), 
                                          selected = character(0),
                                          server=TRUE)
                     updateTextInput(session,inputId="other_sep",  value= character(0))
                     updateTextInput(session,inputId="names_split_cols",  value= character(0))
                     updateCheckboxInput(session, "name_splitted_cols",  value = FALSE)
                     updateSelectizeInput(session,inputId = "select_col_df_SPLIT",   selected = character(0), #multiple = FALSE,
                                          options = list(
                                            placeholder= "columns from loaded dataframe", onInitialize = I('function() { this.setValue(""); }')),
                                          server=TRUE)
                     updateRadioGroupButtons( session,
                                              inputId = "type_of_splitter",
                                              label = "Splitting method",
                                              choices = c("Separator", "Regular Expression"),
                                              selected=character(0),
                                              status = "radioGROUPclass", 
                                              checkIcon = list(
                                                yes = icon("check-square"),
                                                no = icon("square-o") #("fas fa-square-o")
                                              ))
                     gVars$dt_split <- NULL
                     gVars$support_for_correction <- NULL
                     
                     #regular expression
                     updateButton(session,  "reset_test_table_regex",disabled = TRUE)
                     updateButton(session,  "run_separ_regex",disabled = TRUE,style="info") 
                     updateButton(session,  "test_separ_regex",disabled = TRUE)
                     
                     updateTextInput(session,inputId="regex_splitting",  value= character(0))
                     
                     updateTextInput(session,inputId="names_split_cols_regex",  value= character(0))
                     
                     
              }else{return()}
            
           } else { return ()}
        
      shinyBS::updateButton(session, "undo_save_button", style="info", disabled=FALSE)
      updateRadioButtons(session, "step_choice",    choices = c("Delete" = 1, "Modify and/or Save" = 2, "Specials" =3)  , selected = character(0), inline=TRUE)
      
      }  
    } else {
      showModal(dataModal(failed = TRUE))
    }
    
  })

  
  
  #reste all elelemnts in the "specials" window, every time the specials next is pressed
  observeEvent(input$step_submit_spec, { 
    req(isTruthy(input$pick_cols_recoding) | isTruthy(input$other_col_split))
    req(is.null(gVars$support_for_correction))
    updateButton(session,  "glp_pt_button_split",disabled = TRUE)
      
    updateButton(session,  "test_agil_button",disabled = FALSE)
    updateButton(session,  "save_agil_button", style="info", disabled = TRUE)
    updateButton(session,  "glp_pt_button_AGIsplit", style="info", disabled = TRUE) 
    gVars$agi_splitted_new_cols <- NULL
    output$pt_AGIsplit <- renderText({"<i><small>Waiting for action!</small></i>"})
      
    updateNumericInput(session,inputId = "new_cols", label= HTML("Column(s) to add"), min=0, value=0) 
    updateTextInput(session, inputId = "names_added_cols","Name new column(s)", value="")
    updateButton(session,  "add_Fempty_cols_button", style="info",disabled = FALSE)
    updateButton(session,  "glp_pt_button_addEmpty", style="info",disabled = TRUE)
    output$pt_addEmpty <- renderText({"<i><small>Waiting for action!</small></i>"})
    
    updateCheckboxInput(session, "other_col_split", "Select manually another specific column", value = FALSE)
    updateButton(session,  "glp_pt_button_split",disabled = TRUE)
    updateButton(session,  "reset_test_table",disabled = TRUE)
    updateButton(session,  "run_separ",disabled = TRUE,style="info") 
    updateButton(session,  "test_separ",disabled = TRUE)
    output$test_splitted_cols <-NULL
    updateTextAreaInput(session, inputId="PT_store_split", "Procedural track", value = "Waiting for action!")  
    updateSelectizeInput(session,
                           inputId = "type_of_separator", 
                           choices = c("Tab", ",", ";", "Space","", "Other"), 
                           selected = character(0),
                           server=TRUE)
    updateTextInput(session,inputId="other_sep",  value= character(0))
    updateTextInput(session,inputId="names_split_cols",  value= character(0))
    updateCheckboxInput(session, "name_splitted_cols",  value = FALSE)
    updateSelectizeInput(session,inputId = "select_col_df_SPLIT",   selected = character(0), #multiple = FALSE,
                           options = list(
                             placeholder= "columns from loaded dataframe", onInitialize = I('function() { this.setValue(""); }')),
                           server=TRUE)
    updateRadioGroupButtons( session,
                               inputId = "type_of_splitter",
                               label = "Splitting method",
                               choices = c("Separator", "Regular Expression"),
                               selected=character(0),
                               status = "radioGROUPclass", 
                               checkIcon = list(
                                 yes = icon("check-square"),
                                 no = icon("square-o") #("fas fa-square-o")
                               ))
    gVars$dt_split <- NULL
      
    updateButton(session,  "reset_test_table_regex",disabled = TRUE)
    updateButton(session,  "run_separ_regex",disabled = TRUE,style="info") 
    updateButton(session,  "test_separ_regex",disabled = TRUE)
    updateTextInput(session,inputId="regex_splitting",  value= character(0))
    updateTextInput(session,inputId="names_split_cols_regex",  value= character(0))
    
  })
    
  
  
  #update of all buttons and perform the cleaning if needed of data boxes i.e. the selected column
  observeEvent(input$glp_store_comment, {
    req(input$glp_comments_box != "")
    
    
    if(req(input$pick_col_labelling!="")){  
      # re-update buttons 
      shinyBS::updateButton(session, "accept_lab_candidate", style="info")
      shinyBS::updateButton(session, "reject_lab_candidate", style="info")
      enable("accept_lab_candidate")
      enable("reject_lab_candidate")
      enable("undo_relab_button")
      disable("glp_pt_button_relab")
      
      #re-update boxes 
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
    req(!is.null(gVars$phTable), !is.null(gVars$inputGx))
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
    
    #updates the different buttons
    shinyBS::updateButton(session, "accept_lab_candidate", style="success")
    disable("accept_lab_candidate")
    disable("reject_lab_candidate")
    disable("undo_relab_button")
    enable("glp_pt_button_relab")
    disable("pick_col_labelling")
    
    
    #updates the PT box with what was done and accepted
    Value <- paste0("'",chosen_lab, "' relabels '",chosen_match,"'.")
    gVars$pt_glp_list <- c(gVars$pt_glp_list, Value)
    output$pt_label <- renderText({Value})
  })
  
  
  
  
  
  #activate/deactivate GLP/add button according GLP mode abilitation 
  observe({
    if (!input$GLP_inactivation_mode) {    #GLP enabled
      shinyBS::updateButton(session, "glp_pt_button_relab", label=" GLP", style="info")
      shinyBS::addTooltip(session,"glp_pt_button_relab", "Launch a graphical window, to enrich the procedural track info with comments according to GLP regulations.", placement="bottom") 
    } else {
      shinyBS::updateButton(session, "glp_pt_button_relab", label=" Add", style="info")
      shinyBS::removeTooltip(session,"glp_pt_button_relab") 
    }
  })
  
  
  
  
  
  
  
  # undo buttons  
  observeEvent(input$undo_relab_button | input$undo_dupl_button | input$undo_del_button | input$undo_save_button |  
                 input$undo_agil_button | input$undo_addEmpty  | input$undo_multi_button,{  
                   req(!is.null(gVars$pt_glp_list))
                   req(!is.na(gVars$pt_glp_list))
                   #stores undo 
                   if (!is.null(gVars$last5list)){      
                     if (!is_empty(gVars$last5list)){ 
                       torestore <- tail (gVars$last5list,1)[1]
                       #when action is undone, the last element of the list (storing that action) is deleted
                       gVars$last5list = gVars$last5list[0:(length(gVars$last5list)-1)]
                       output_undofunction <- undofunction(torestore,gVars=gVars)
                       gVars<- output_undofunction[[1]]
                       gVars$pt_glp_list <- c(gVars$pt_glp_list, paste("UNDO:", gVars$undo_message))
                       
                       name_op <-names(torestore)
                       
                       if(name_op %in% c("relab_AC","relab_RJ")) {
                         gVars$relab_op_list[[ which(gVars$relab_op_list %in% torestore)]] <-NULL
                         data$fnDUPL_FINDER <- duplicate_finder(gVars$phTable[[1]])
                       }
                       else if(name_op %in% "dupl_AC") {gVars$dupl_op_list[[ which(gVars$dupl_op_list %in% torestore)]]<-NULL}
                       else if(name_op %in% "deleted"){
                         updateSelectizeInput(session,
                                              inputId = "pick_cols_recoding",
                                              #label = "Select the column to recode you wish to display:",
                                              choices = output_undofunction[[2]] ,
                                              selected = character(0),
                                              server=TRUE)
                         gVars$delete_op_list[[ which(gVars$delete_op_list %in% torestore)]] <-NULL}
                       else if(name_op %in% "recode_RJ"){ 
                         updateSelectizeInput(session,
                                              inputId = "pick_cols_recoding",
                                              #label = "Select the column to recode you wish to display:",
                                              choices = output_undofunction[[2]] ,
                                              selected = character(0),
                                              server=TRUE)
                         gVars$recodeRJ_op_list[[ which(gVars$recodeRJ_op_list %in% torestore)]] <-NULL}
                       else if(name_op %in% "recode_AC"){ 
                         updateSelectizeInput(session,
                                              inputId = "pick_cols_recoding",
                                              #label = "Select the column to recode you wish to display:",
                                              choices = output_undofunction[[2]] ,
                                              selected = character(0),
                                              server=TRUE)
                         gVars$recod_ops_dataframe <- tail(gVars$recod_ops_dataframe, -1)}
                       else if(name_op %in% c("special_agiSPL","special_SPL","special_SPLREG")){
                         updateSelectizeInput(session,
                                              inputId = "pick_cols_recoding",
                                              #label = "Select the column to recode you wish to display:",
                                              choices = output_undofunction[[2]] ,
                                              selected = character(0),
                                              server=TRUE)
                         gVars$special_op_list[[ which(gVars$special_op_list %in% torestore)]] <-NULL}
                       else if(name_op %in% "special_addEMPTY"){
                         updateSelectizeInput(session,
                                              inputId = "pick_cols_recoding",
                                              #label = "Select the column to recode you wish to display:",
                                              choices = output_undofunction[[2]] ,
                                              selected = character(0),
                                              server=TRUE)
                         gVars$special_op_list[[ which(gVars$special_op_list %in% torestore)]] <-NULL}
                       else if(name_op %in% "multi") {
                         #updates the table if needed
                         if(output_undofunction[[2]]){
                           updateSelectizeInput( 
                             session,
                             inputId="multijoin_outcomes",
                             choices=gVars$grouping_multi_tables,
                             selected = character(0),
                             options = list(
                               placeholder = 'Please select an option below',
                               onInitialize = I('function() { this.setValue(""); }')),
                             server=TRUE) 
                         } 
                         output$dt_toshow_multi_selected_group <- DT::renderDataTable(NULL) }
                       else {return(NULL)}
                       
                       #if GLPmode activated, stores the message and open the GLPcomment modal window
                       req(!input$GLP_inactivation_mode)
                       showModal(dataModal(failed = FALSE))
                     }
                     else { shinyjs::info("It is not possible to undo more operations") }
                   }
                 })
  
  
  
  
  
  observeEvent(input$undo_del_button | input$undo_save_button ,{
    gVars$flag_REMOVE_WIDGETS <- FALSE
  })
  
  #undoblock
  
  #undo block
  
  
  
  # activate/deactivate GLP/add button according GLP mode abilitation
  observe({
    if (!input$GLP_inactivation_mode) {    #GLP enabled
      shinyBS::updateButton(session, "glp_pt_button_dupl", label=" GLP", style="info")
      shinyBS::addTooltip(session,"glp_pt_button_dupl", "Launch a graphical window, to enrich the procedural track info with comments according to GLP regulations.", placement="bottom") 
    } else {
      shinyBS::updateButton(session, "glp_pt_button_dupl", label=" Add", style="info")
      shinyBS::removeTooltip(session,"glp_pt_button_dupl") 
    }
  })
  
  
  
  ##################### Duplicate removal part
  output$pt_text_dupl <- renderText({"<b>Procedural Track</b>"})
  output$pt_dupl <- renderText({"<i><small>Waiting for action!</small></i>"})
  
  data <- reactiveValues(fnDUPL_FINDER = NULL, test=NULL)
  
  #it elaborates phenodata table to retrieve potential column duplicates 
  output$pick_col_duplicates <- renderUI({
    procesD <- gVars$processedD
    if (is.null(procesD) | is_empty(procesD)) {
      df= gVars$phTable[[1]]
    } else {
      df= gVars$phTable[[1]] %>% .[,!(names(.) %in% procesD) ] 
    }
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
                     liveSearchPlaceholder = "Search entry"),   # build buttons for collective selection
      multiple = FALSE)
  })
  
  
  
  # as consequence of selecting a specific group of identical columns, their contents are shown; the group name is splitted to store the specific colnames to match in order to identify which needed to be shown. 
  output$content_dupl_remov <- renderDataTable({
    req(input$pick_col_duplicate)
    dupl_cols = unlist(strsplit(as.character(input$pick_col_duplicate), split = " -- "))  
    table1 <- gVars$phTable[[1]]
    req(dupl_cols %in% colnames(table1))
    if(gVars$flag_REMOVE_WIDGETS){
      table1<-dplyr::select(table1,dupl_cols)
      colnames(table1) <-dupl_cols
      gVars$potential_choices = dupl_cols
      table1
    }else {NULL}
    
  })
  
  
  
  # as consequence of selecting a specific pair, it shows the correspondent label in the upper left box
  observeEvent(input$pick_col_duplicate,{
    output$pt_dupl <- renderText({"..."})
    #radiobuttons are created dinamically here in the server and given to UI.
    output$radioDUPL <- renderUI({
      potential_choices  <- gVars$potential_choices  
      
      options <- c(potential_choices, "All", "None")  #it creates the option of radiobuttons; "none" to delete all columns of the group, "all" to keep all
      # The options are dynamically generated on the server
      radioButtons("reply", "Select the column to keep", options, selected = character(0))
    })
    gVars$flag_REMOVE_WIDGETS <<- TRUE
  })
  
  
  #as the radio button options are selected the PT is updated
  observeEvent(input$reply,{
    df <- gVars$phTable[[1]]
    choice <- as.character((input$reply))
    potential_choices <- gVars$potential_choices 
    toexclude <- potential_choices[!choice == potential_choices]
    gVars$dupl_decisions <- c(choice, toexclude)
    
    
    
    req(!is_empty(choice) )
    #updates the PT box with what was done and accepted to do
    if (choice == "None"){
      toexcl_2string <- paste0('"', paste(toexclude, collapse='", "'), '"')
      #paste0(" '", paste0(toexclude, collapse= "', '"),"' ")
      Value <- paste0("All duplicated columns (", toexcl_2string, ") are deleted.")
    } else if (choice == "All") {   
      toexcl_2string <- paste0('"', paste(toexclude, collapse='", "'), '"')
      Value <- paste0("All duplicates (",toexcl_2string,") are kept.")}
    else {
      if(length(toexclude)==1) {
        indicator <- c("duplicate", "is") 
        toexcl_2string <- paste0(" '", toexclude, "' ")}
      else {
        indicator <- c("duplicates","are") 
        toexcl_2string <- paste0(" '", paste0(toexclude, collapse= "', '"),"' ")}
      Value <- paste0("The column '",choice, "' is kept, while its ", indicator[1], toexcl_2string, indicator[2]," deleted.")
    } 
    
    output$pt_dupl <- renderText({Value})
    gVars$tmp_pt_value <- Value
  })
  
  
  
  
  
  
  observeEvent( input$submit,{
    
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
    
    shinyBS::updateButton(session, "submit", style="success")
    
    disable("submit")
    
  })
  
  
  
  
  observeEvent(input$glp_pt_button_dupl,{
    #it stores the decision about each duplicate group: dupl_decis are (kept, deleteted colnames)
    dupl_decis <- gVars$dupl_decisions
    df <- gVars$phTable[[1]]
    req(input$reply)
    
    gVars$flag_REMOVE_WIDGETS <<- FALSE
    potential_choices <- gVars$potential_choices
    if(dupl_decis[1] %in% "All") {
      choice<-dupl_decis[2:length(dupl_decis)]
      gVars$processedD <- c(gVars$processedD, choice)
    } else{ choice <-dupl_decis[1]} 
    toexclude <- potential_choices[!choice == potential_choices]
    gVars$tmpdf <- df[,!(names(df) %in% toexclude)]
    gVars$selected_cols <- choice
    shinyjs::enable("submit")
    gVars$phTable[[1]] <- gVars$tmpdf
    
    shinyBS::updateButton(session, "submit", style="info")
    shinyBS::updateButton(session, "glp_pt_button_dupl", style="info", disabled=TRUE)
    
    #storing for undostep
    dupl_AC <- list( choice,df[,potential_choices] )
    gVars$dupl_op_list <- c(gVars$dupl_op_list, (list(dupl_AC=dupl_AC)))
    gVars$last5list <- c(gVars$last5list, (list(dupl_AC=dupl_AC)))
    
    if(!input$GLP_inactivation_mode){    #such as GLPmode activated
      showModal(dataModal())
      disable("undo_dupl_button")
    } else { # only procedural track report
      # re-update buttons 
      shinyBS::updateButton(session, "submit", style="info")
      enable("submit")
      enable("undo_dupl_button")
      
    }
    gVars$potential_choices <- NULL  
  }) 
  
  
  
  
  
  ###########START: recoding content part
  data <- reactiveValues(fnCOLUMNS_to_RECODE = NULL)
  previous_cols_list <- NULL
  
  
  #it elaborates phenodata table to retrieve a first group of columns to recode
  observeEvent(c(input$recoding_button, input$glp_pt_button_rec),{  
    req(!is.null(gVars$dkeys) , !is.null(gVars$phTable))
    df <- gVars$phTable[[1]]
    dkeys = gVars$dkeys
    alw <- gVars$always
    procesLIST2REC <- gVars$processedLIST2REC 
    proces_cols_RECOD <- gVars$processed_cols_RECOD
    
    df_2recode = df[,!colnames(df) %in% alw]
    data$fnCOLUMNS_to_RECODE = data.frame(df_2recode)
    
    # it ONLY shows the list of those clumns that are going to be recorded and it creates a dropdown menu
    a <- colnames(df_2recode)
    
    tmp <- data.frame(a) %>% filter(.,!(a %in% procesLIST2REC)) %>% filter(.,!(a %in% proces_cols_RECOD)) %>%
      filter(.,!(a %in% gVars$deleted_colname))    %>% rowwise()
    
    gVars$list_cols_tmp <- tmp$a
    if (data$nrow_upd_fnSING_COL_RECODE_FINDER >=1 || is.null(data$nrow_upd_fnSING_COL_RECODE_FINDER)) { 
      updateSelectizeInput(session,
                           inputId = "pick_cols_recoding",
                           #label = "Select the column to recode you wish to display:",
                           choices = tmp$a,
                           selected = current_selection(),
                           server=TRUE)
    }    
    data$nrow_upd_fnSING_COL_RECODE_FINDER = NULL
  })
  
  current_selection <- reactiveVal(NULL)
  observeEvent(input$pick_cols_recoding, {
    current_selection(input$pick_cols_recoding)
  })
  
  
  # as consequence of selecting a specific column name from the "column to recode" dropdown menu, it shows the column content of that column.
  output$contentLIST_COLS <- renderDataTable({
    req(input$pick_cols_recoding)
    colnam <- colnames( gVars$phTable[[1]] )
    req(input$pick_cols_recoding %in% colnam)
    if(gVars$flag_REMOVE_WIDGETS & input$pick_cols_recoding %in% colnam){     
      table1 <- gVars$phTable[[1]]        
      table1<-data.frame(table1[,req(input$pick_cols_recoding)])
      colnames(table1) <-input$pick_cols_recoding
      table1
    } else {NULL}
  })
  
  
  
  
  data <- reactiveValues(fnSING_COL_RECODE_FINDER = NULL, nrow_upd_fnSING_COL_RECODE_FINDER= NULL)
  
  observeEvent(input$pick_cols_recoding, {
    gVars$flag_REMOVE_WIDGETS <<-TRUE
  })
  
  
  # as consequence of selecting a specific column name from the "column to recode" dropdown menu, it shows the correspondent column name in the TextInput box above on the left(recoded_label). There, it could be modified and saved again into the dataset. Just below, the unique cell contents of the column and their recoded version are shown. 
  #creates a double input to observe and react to remove the elaborated and stored element from the list of recoded contents
  double_reactors <- reactive({
    paste(input$pick_cols_recoding) 
  })
  
  recode_synthesis <- eventReactive( double_reactors(), {
    req(input$pick_cols_recoding)
    df = gVars$phTable[[1]]
    dkeys = gVars$dkeys
    chosen_rec_label <- ifelse(!is.null(input$recoded_label),
                               ifelse(identical(input$pick_cols_recoding,input$recoded_label), input$recoded_label,  input$pick_cols_recoding ),
                               input$pick_cols_recoding)
    
    if(gVars$flag_REMOVE_WIDGETS){ 
      gVars$orig_lab <- reactiveVal(chosen_rec_label)  
      fnSING_COL_RECODE_FINDER = single_col_recode_finder(dkeys,df[[chosen_rec_label]])
      intersected_content_keys <- gVars$processedLIST_content2RECODE$keys() %>% .[. %in% fnSING_COL_RECODE_FINDER$cases] %>%
        lapply(. , function (newC) { 
          is_pairCont_Lab_present <- input$pick_cols_recoding %in% unique(unlist(strsplit(gVars$processedLIST_content2RECODE$get(newC, "_\\*_"), "_\\*_") ))
          out <- ifelse(is_pairCont_Lab_present, newC, "NULL") 
          out <- out
        } )
      
      #filters out the values already present  
      upd_fnSING_COL_RECODE_FINDER <- isolate({filter(fnSING_COL_RECODE_FINDER, !fnSING_COL_RECODE_FINDER$cases %in% intersected_content_keys )})
      data$nrow_upd_fnSING_COL_RECODE_FINDER <- nrow(upd_fnSING_COL_RECODE_FINDER)
      
      list(colname = chosen_rec_label, uniquecontents = upd_fnSING_COL_RECODE_FINDER)
    }
  })
  
  
  
  
  # discriminant to show feedback according to the presence in recoded_label in the vocabulary
  data <- reactiveValues(classificationLAB = NULL)
  
  observeEvent((input$recoded_label),{
    isoRL <- isolate (input$recoded_label)
    req(!is.null(gVars$dkeys), !is.null(input$recoded_label))
    # req(input$recoded_label != "")
    unilab <- unique(gVars$dkeys$label)
    uniLsyn <- unique(gVars$dkeys$lab_syn)
    isoRL <- isolate (input$recoded_label)
    if (isoRL %in% unilab) {
      tag <-"RL"
    } else if (!isoRL %in% unilab & isoRL %in% uniLsyn){
      tag <- "LS"
    } else {
      tag <- "UNKL"
    }
    data$classificationLAB <- tag
  })
  
  observe({
    req(!is.null(input$pick_cols_recoding))
    iso_clasL <- data$classificationLAB
    if (is.null(iso_clasL)) {hideFeedback("recoded_label")
    } else { 
      if (iso_clasL=="RL"){
        hideFeedback("recoded_label")
        showFeedbackSuccess(
          inputId = ("recoded_label"),
          text = "Known reference label")  
      } else if (iso_clasL=="LS"){
        hideFeedback("recoded_label")
        showFeedback(
          inputId = ("recoded_label"),
          text = "Known label synonym",
          color ="blue",
          icon = shiny::icon("warning-sign", lib = "glyphicon"))  
      } else{      # (iso_clasL=="UNKL")
        hideFeedback("recoded_label")
        showFeedbackDanger(
          inputId = ("recoded_label"),
          text = "Unknown label")   
      }
    }  
  })
  
  
  observeEvent(input$pick_cols_recoding, {
    req(input$pick_cols_recoding !="")
    if(gVars$flag_REMOVE_WIDGETS){ 
      updateTextInput(session,inputId="recoded_label", "Recoded column name", value = recode_synthesis()$colname) 
      
    }
    
  })
  
  
  
  
  # discriminant to show feedback according to the presence in recoded_label in the vocabulary
  data <- reactiveValues(classificationCONT = NULL)
  
  observeEvent(c(input$recoded_content,input$recoded_label),{
    isoRC <- isolate (input$recoded_content)
    req(!is.null(gVars$dkeys), !is.null(input$recoded_content))
    # req(input$recoded_label != "")
    subset_dict <- gVars$dkeys[gVars$dkeys$label %in% input$recoded_label,]
    unicont <- unique(subset_dict$allowed_features) #unique(gVars$dkeys$allowed_features)
    uniCsyn <- unique(subset_dict$syn_features)#unique(gVars$dkeys$syn_features)
    if (isoRC %in% unicont) {
      tagC <-"RC"
    } else if (!isoRC %in% unicont & isoRC %in% uniCsyn){
      tagC <- "CS"
    } else {
      tagC <- "UNKC"
    }
    data$classificationCONT <- tagC
  })
  
  observe({
    req(!is.null(input$pick_cols_recoding))
    iso_clasC <- data$classificationCONT
    if (is.null(iso_clasC)) {hideFeedback("recoded_content")
    } else { 
      if (iso_clasC=="RC"){
        hideFeedback("recoded_content")
        showFeedbackSuccess(
          inputId = ("recoded_content"),
          text = "Known reference content")  
      } else if (iso_clasC=="CS"){
        hideFeedback("recoded_content")
        showFeedback(
          inputId = ("recoded_content"),
          text = "Known content synonym",
          color ="blue",
          icon = shiny::icon("warning-sign", lib = "glyphicon"))  
      } else{      # (iso_clasC=="UNKC")
        hideFeedback("recoded_content")
        showFeedbackDanger(
          inputId = ("recoded_content"),
          text = "Unknown content")   
      }
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
    gVars$map_recoding <- isolate({data.frame(d,e,f,g)   })
    gVars$map_for_plot <- gVars$map_recoding  
    gVars$test <- isolate({data.frame(d,e,f,g)  %>% filter(., !(g ==0)) })  
    
    
    tmp <- data.frame(d,e,f,g) %>%   
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
                                      liveSearchPlaceholder = "Search entry")   # build buttons for collective selection
    )
    
    
  })
  
  
  
  observeEvent(c(input$glp_pt_button_rec) ,{   #input$step_submit
    req(!is.null(gVars$map_recoding)) 
    if (!is.null(input$pick_recoding_content_pairs)){
      chosen_rec_orig_content <-  gVars$map_recoding[gVars$map_recoding$f %in% input$pick_recoding_content_pairs,]$d
      chosen_rec_content <-  input$pick_recoding_content_pairs   
      
    }
  })
  
  
  
  observeEvent(c(input$step_submit) ,{   #input$step_submit
    req(!is.null(gVars$map_recoding)) 
    if (!is.null(input$pick_recoding_content_pairs)){
      chosen_rec_orig_content <-  gVars$map_recoding[gVars$map_recoding$f %in% input$pick_recoding_content_pairs,]$d
      chosen_rec_content <-  input$pick_recoding_content_pairs  
      gVars$orig_cont <- reactiveVal(chosen_rec_orig_content)
      
    }  
    req(!is.null(gVars$phTable[[1]]),!is.null(gVars$dkeys))
    req(!is_empty(gVars$orig_lab())   )
    req(!is.null(gVars$orig_cont))  #req(!is.null(gVars$orig_cont())   )
    item <- gVars$orig_cont()
    updateSelectizeInput( 
      session,
      inputId="content_TOclean_FORvocabulary",
      label= "Potential content-entry",
      choices=item,
      selected = item[1], 
      options = list(maxItems = 1, create=TRUE, plugins = list('restore_on_backspace')),
      server=TRUE) 
    
  })
  
  
  
  
  
  
  observeEvent(input$pick_recoding_content_pairs , {
    req(!is.null( isolate(input$pick_cols_recoding)), !is.null(gVars$phTable))
    
    chosen_rec_content <-  input$pick_recoding_content_pairs  
    gVars$originalcontent <- (chosen_rec_content)        
    
    msg <- c()
    req(!is.null(input$pick_recoding_content_pairs))
    if(input$pick_recoding_content_pairs=="") {msg <-"Leave blank for NA"} 
    updateTextInput(session, inputId="recoded_content", "Recoded unique content", value = chosen_rec_content) 
    
  })
  
  
  
  
  
  output$barplot <- renderPlot({  
    req(!is.null(input$pick_recoding_content_pairs))
    #input$pick_recoding_content_pairs
    iso <- isolate(input$pick_cols_recoding) 
    req(!is.null(iso ), !is.null(input$pick_recoding_content_pairs))
    req(!identical(iso ,""))
    colnam <- colnames((gVars$phTable[[1]]) )
    req(iso %in% colnam )
    
    if(gVars$flag_REMOVE_WIDGETS ){ 
      dt <- (gVars$phTable[[1]][,iso]) %>%  
        table(.) %>%  data.frame(unclass(.))  
      dt[is.na(dt)] <- "NA"
      colnames(dt) <- c("id","Frequency")
      dt <- dt[c("id","Frequency")]
      row.names(dt) <- NULL
      
      
      oldc<- dplyr::filter(gVars$map_for_plot, f %in% input$pick_recoding_content_pairs )$d
      if (is_empty(oldc)) {return ()}
      dt$highlight <- ifelse(dt$id == oldc,  1,  0) 
      
      ggplot(dt,
             aes(x = id, y = Frequency, fill=highlight)) +
        geom_bar(stat = "identity") +
        theme(axis.title.x = element_blank(),
              axis.text.x = element_text(angle = 60, hjust = 1),
              legend.position="none")+ scale_x_discrete(labels = function(x) label_shortener(x, 22))
       
    }  
  })
  
  
  
  
  
  observeEvent(      input$step_submit_del,{
    req(input$pick_cols_recoding)
    df <- (gVars$phTable[[1]])
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
      
      output$pt_recod_del_msg <- renderText({ del_op_message_reactive$msg})  
      shinyBS::updateButton(session, "glp_pt_button_rec_del", disabled=FALSE)
      
      gVars$pt_glp_list <- c(gVars$pt_glp_list, del_op_message_reactive$msg)
      
      #updatebutton
      shinyBS::updateButton(session, "undo_save_button", disabled=TRUE) 
      
    }
  })
  
  
  
  observeEvent(input$glp_pt_button_rec_del, {
    shinyBS::updateButton(session, "glp_pt_button_rec_del", disabled=TRUE)
    shinyBS::updateButton(session, "undo_save_button", disabled=FALSE) 
    output$pt_recod_del_msg <- renderText({"<i><small>Waiting for action!</small></i>"})
    
    if(!input$GLP_inactivation_mode){    #such as GLPmode activated
      showModal(dataModal()) 
    } else {
      updateRadioButtons(session, "step_choice", choices= c("Delete" = 1, "Modify and/or Save" = 2, "Specials" =3)  , selected = character(0), inline=TRUE) 
      shinyjs::delay(500, toggleModal(session, "modalstep1", "close"))
    }
    
  })
  
  
  
  
  
  observeEvent(      input$step_submit,{ 
    df <- data.frame(gVars$phTable[[1]])
    choice <- as.character((input$step_choice))
    gVars$skip_marker <- FALSE
    if(is_empty(choice)){
      shinyjs::info("Please select the next step!")
      return()
    }
    else { #(choice == "Modify and/or Save") #it shows newly edited label and content 
      d<-gVars$dkeys
      output$new_label_text <- renderText({ 
        if(!input$recoded_label %in% gVars$all_original_colnames) {     #label was modified
          outL <- " Edited Label: "
          
        } else {outL<-"Confirmed Label: "
        }
        outL})
      output$edited_label <- renderText({colorwords(isolate({input$recoded_label}),d)})
      
      output$new_content_text <- renderText({
        req(!is.null(input$recoded_content))
        if(!input$recoded_content %in% gVars$orig_cont()) {     #content was modified
          outC <- " Edited Content: "
        } else {          #content is also in the original table
          outC <- " Confirmed Content: "
        }
        outC
      })
      
      
      
      output$edited_content <- renderText({colorwords(isolate({input$recoded_content}),d)})
    } 
    
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
    
    
    
    req(!is.null(gVars$phTable[[1]]),!is.null(gVars$dkeys))
    if(input$pick_cols_recoding != ""){
      df_col <- gVars$phTable[[1]][,req(input$pick_cols_recoding)] %>% unique()
      gVars$rejected_full_col <- as.character(df_col)
      
    }else {df_col <- gVars$rejected_full_col} 
    
    
    # When reject is pressed, it keeps the picked column (keep_original_column) and rejects the full recoding version of its content by deleting the colname from the dropdown menu (the one with column names on the left of the app-page)
    newlabel <- input$recoded_label
    oldlabel <- input$pick_cols_recoding
    gVars$phTable[[1]]<-setnames((gVars$phTable[[1]]), oldlabel, newlabel)
    gVars$tmp_stored_df <-gVars$phTable[[1]]
    
    
    t_modified <- c(newlabel, oldlabel, NA,NA, gVars$username)
    gVars$skip_marker <- TRUE
    gVars$tmp_recode_rj_element <- t_modified
    
    #update button
    shinyBS::updateButton(session, "save_recoding",  disabled=TRUE)
    shinyBS::updateButton(session, "reject_full_recoding", style="success", disabled = TRUE) 
    shinyBS::updateButton(session,"undo_save_button", disabled = TRUE)
    shinyBS::updateButton(session,"store_button", disabled = FALSE) 
    output$pt_recod <- renderText({"..."})
    
    #if wished to store contents, they must be unmodified and so stored as reference instead of syns
    updateRadioButtons(session, "store_contents", 
                       choices = list("as Reference Content" = 3, "Do not store"=4),
                       selected = character(0))
    
    data$voc_enrich_panel <- TRUE
    shinyjs::disable("pick_recoding_content_pairs") 
    shinyjs::disable("pick_cols_recoding")
    
    
    
    updateSelectizeInput( 
      session,
      inputId="content_TOclean_FORvocabulary",
      label= "Potential content-entry",
      choices=df_col,
      selected = df_col[df_col %in% gVars$orig_cont()],
      options = list(maxItems = 99999999, create=FALSE),  
      server=TRUE) 
    
    updatePrettyCheckbox( session=session, inputId = "select_all", value = FALSE )
     
  })
  
  # discriminant to show "select all" checkbox in ui 
  skip_marker_react <- reactive({  gVars$skip_marker})
  output$skip_marker_out <- reactive({  gVars$skip_marker})
  outputOptions(output, "skip_marker_out", suspendWhenHidden = FALSE)
  
  
  observe({
    req(!is.null(input$select_all))
    input$select_all
    len_col <- length(gVars$rejected_full_col)
    if(len_col<1) {shinyjs::disable("select_all")}
    else {shinyjs::enable("select_all")}
  })  
  
  
  
  
  observe({
    
    req(!is.null(gVars$phTable), !is.null(gVars$dkeys) )
    req(!is.null(input$select_all))
    input$select_all
    
    same_col <- gVars$rejected_full_col
    same_col_length <- length(same_col)
    req(same_col_length != 0)
    
    if(input$select_all==TRUE ) {
      updateSelectizeInput( 
        session,
        inputId="content_TOclean_FORvocabulary",
        label= "List of potential content-entry",
        choices=same_col,
        selected = same_col[1:same_col_length], 
        options = list(maxItems=99999999,create=FALSE),
        server=TRUE) 
    } else {#if (input$select_all==FALSE & skip_marker_react() == FALSE ) {
      req(input$select_all==FALSE & skip_marker_react() == FALSE )
      updateSelectizeInput( 
        session,
        inputId="content_TOclean_FORvocabulary",
        label= "List of potential content-entry",
        choices=same_col,
        selected = same_col[1],
        options = list(create=FALSE),
        server=TRUE)
      
    } 
  })
  
  entry_selected <- reactiveVal(NULL)
  
  observeEvent(input$reset,{
    req(!is.null(input$reset) && input$reset == TRUE)
    req(!is.null(gVars$orig_cont()))
    same_col <- gVars$rejected_full_col
    updatePrettyCheckbox(session, inputId = "select_all", value=FALSE )
    updateSelectizeInput( 
      session,
      inputId="content_TOclean_FORvocabulary",
      label= "Potential content-entry",
      choices=same_col,
      selected = same_col[same_col %in% gVars$orig_cont() ],
      options = list(create=FALSE),
      server=TRUE)
    shinyjs::delay(1500, updatePrettyCheckbox(session, inputId = "reset", value=FALSE )) 
    
  })
  
  
  observe({
    req(input$pick_cols_recoding != "")
    req(!is.null(skip_marker_react()))
    req (skip_marker_react() == TRUE)
    req(input$select_all ==TRUE)
    same_col <- gVars$rejected_full_col
    same_col_length <- length(same_col)
    req(!is.null(input$content_TOclean_FORvocabulary))
    current_content_box_length <- length(input$content_TOclean_FORvocabulary)
    if(current_content_box_length < same_col_length & current_content_box_length != 1) {
      entry_selected(input$content_TOclean_FORvocabulary)
      updateSelectizeInput( 
        session,
        inputId="content_TOclean_FORvocabulary",
        label= "List of potential content-entry",
        choices=same_col,
        selected = same_col[same_col %in% entry_selected()],  
        options = list(create=FALSE),
        server=TRUE)
      
      if (input$select_all){
        updatePrettyCheckbox( session, inputId = "select_all", value = FALSE )
        
      }
    }  
    
  })
  
  
  
  observeEvent(input$glp_pt_button_rec,{
    if (!is.null(gVars$rejected_full_col)) { gVars$rejected_full_col<-NULL
    updatePrettyCheckbox( session=session, inputId = "select_all", value = FALSE )
    
    }
  })   
  
  
  
  
  
  
  # button accept in recoding section
  observeEvent(input$save_recoding,{     
    req(!is.null(gVars$phTable), !is.null(gVars$inputGx))
    
    # check if colname is modified and only if yes, it saves the new version (input$recoded_label) deleting the old(input$pick_cols_recoding)
    df <- gVars$phTable[[1]]
    cmodification <- rep(NA,5) 
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
      gVars$original_label <- input$pick_cols_recoding
      original_oldcolname <<- gVars$original_label
      gVars$flag_REMOVE_WIDGETS <<- TRUE
      
      
    }
    originalcontent <-  ifelse(gVars$originalcontent != "", gVars$originalcontent, NA)
    
    mr_tmp <-  data.frame(gVars$map_recoding)
    if (!input$recoded_content %in% originalcontent) {
      old_content <- mr_tmp[mr_tmp$f %in% originalcontent,]$d      
      mr_tmp["f"][mr_tmp["f"] == originalcontent] <- input$recoded_content 
      gVars$map_recoding <- data.frame(mr_tmp)
    } else {      old_content <- filter(mr_tmp, (f == input$recoded_content))$d}
    
    
    #substitution in the df
    new_content <- input$recoded_content  
    df[new_colname][df[new_colname] == old_content] <- new_content 
    
    #stores temporarely this value to clean the recoding pairs later
    gVars$clean_recoding_pairs <- c(paste(new_colname, original_oldcolname,sep="_*_" ), new_content)
    
    
    #prepare the new voices to update the vocabulary
    cmodification[c(1:5)] <- c(new_colname, original_oldcolname, new_content, old_content,gVars$username)
    gVars$recod_ops_dataframe <-  as.data.frame(rbind(gVars$recod_ops_dataframe , cmodification)) %>% 
      setNames(., c("newlab","oldlab","newcont","oldcont","username")) %>%
      `rownames<-`(NULL) 
    
    
    # storing the operation for the undo last5step and general undo
    #storing for undostep
    recode_AC <- list(gVars$recod_ops_dataframe )
    recode_AC_forlast5 <- list(tail(recode_AC[[1]],1))
    
    gVars$recode_list <- list(recode_AC=recode_AC)
    gVars$last5list <- append(gVars$last5list, (list(recode_AC=recode_AC_forlast5)))
    
    #update button
    shinyBS::updateButton(session, "save_recoding",   style="success", disabled=TRUE)
    shinyBS::updateButton(session,"reject_full_recoding", disabled = TRUE) 
    shinyBS::updateButton(session,"undo_save_button", disabled = TRUE)
    shinyBS::updateButton(session,"store_button", disabled = FALSE) 
    output$pt_recod <- renderText({"..."})
    data$voc_enrich_panel <- TRUE
    disable("pick_recoding_content_pairs")
    shinyjs::disable("pick_cols_recoding") 
    gVars$tmp_stored_df <-df
    
     
  })
  
  
  
  observe({
    input$label_TOclean_FORvocabulary
    req(!is.null(gVars$phTable))
    if(!isTruthy(input$label_TOclean_FORvocabulary)) {
      updateRadioButtons(session, "store_labels",  
                         selected = 2)
      shinyjs::disable(selector = "#store_labels [type=radio][value =1 ]")
      shinyjs::runjs("$('#store_labels [type=radio][value =1]').parent().addClass('disabled').css('opacity', 0.4)")
    } else {
      shinyjs::enable(selector = "#store_labels [type=radio][value =1 ]")
      shinyjs::runjs("$('#store_labels [type=radio][value =1]').parent().addClass('enabled').css('opacity', 1)")
      updateRadioButtons(session, "store_labels", 
                         selected = character(0))
    }
  })
  
  observe({
    input$content_TOclean_FORvocabulary
    req(!is.null(gVars$phTable))
    if(!isTruthy(input$content_TOclean_FORvocabulary)) {
      updateRadioButtons(session, "store_contents",  
                         selected = 2)
      shinyjs::disable(selector = "#store_contents [type=radio][value =3 ]")
      shinyjs::runjs("$('#store_contents [type=radio][value =3]').parent().addClass('disabled').css('opacity', 0.4)")
    } else {
      shinyjs::enable(selector = "#store_contents [type=radio][value =3 ]")
      shinyjs::runjs("$('#store_contents [type=radio][value =3]').parent().addClass('enabled').css('opacity', 1)")
      updateRadioButtons(session, "store_contents", 
                         selected = character(0))
    }  
    
  })
  
  # discriminant to show "vocabulary enrichment" 2nd panel in the content homogenization section 
  #gVars$voc_enrich_panel <- reactiveVal( NULL )
  data <- reactiveValues(voc_enrich_panel = NULL)
  observe({
    if (is.null(data$voc_enrich_panel )) {shinyjs::hideElement(id= "vocab_syn_decision_panel")}
    else {
      if (data$voc_enrich_panel == TRUE){
        shinyjs::showElement(id= "vocab_syn_decision_panel")
      } else {shinyjs::hideElement(id= "vocab_syn_decision_panel")} 
    }
  })
  
  
  observeEvent(input$glp_pt_button_rec,{
    req(!is.null(gVars$tmp_stored_df))
    req(!is_empty(gVars$clean_recoding_pairs))
    gVars$phTable[[1]]<-gVars$tmp_stored_df
    
    #storing of the newly recoded content
    if (!isTruthy(gVars$processedLIST_content2RECODE)) { 
      #dict key:newcontent, value=newlabel
      gVars$processedLIST_content2RECODE <- collections::dict(list[[gVars$clean_recoding_pairs[2]]] <- gVars$clean_recoding_pairs[1])
      
    } else {
      #if dict has already the key - key is already present 
      if(gVars$processedLIST_content2RECODE$has(gVars$clean_recoding_pairs[2])){
        gVars$processedLIST_content2RECODE$set(gVars$clean_recoding_pairs[2], 
                                               c(gVars$clean_recoding_pairs[1], gVars$processedLIST_content2RECODE$get(gVars$clean_recoding_pairs[2])))
      } else {
        gVars$processedLIST_content2RECODE$set(gVars$clean_recoding_pairs[2], gVars$clean_recoding_pairs[1])
      }
    }
    
    
    
    #once eliminated all recoding proposals in recoding dropdown menu, it deletes the proposed column name on related dropdown menu  (on the left)
    if(is.null(input$pick_recoding_content_pairs)){
      new_colname <- input$label_TOclean_FORvocabulary
      gVars$processedLIST2REC <- c(gVars$processedLIST2REC,new_colname)  
    }
    
    if (data$nrow_upd_fnSING_COL_RECODE_FINDER ==1) {     
      gVars$processed_cols_RECOD <- c( gVars$processed_cols_RECOD, input$recoded_label)
    }
    
    #cleaning if something is stored 
    if (!is.null(gVars$tmp_recode_rj_element)) {gVars$tmp_recode_rj_element <- NULL}
    
  })
  
  
  output$original_label <-  renderUI({
    req(!is_empty(gVars$orig_lab()))
    textInput(inputId="label_TOclean_FORvocabulary", "Potential label-entry", value = gVars$orig_lab())  
  })
  
  
  
  observeEvent(input$recoded_label, {
    req(input$pick_cols_recoding, input$recoded_label)
    reclab<- input$recoded_label
    dkeys = gVars$dkeys
    h <- dkeys$label
    i <- dkeys$lab_syn
    
    tmp <- rbind("", unique(data.frame(h,i)))   
    
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
  
  
  
  
  
  
  observeEvent( input$update_pre_VOCstorage_button,{   
    req(nrow(gVars$later_voc_upd_df)!=0)
    candidate_df <- gVars$later_voc_upd_df[!duplicated(gVars$later_voc_upd_df),]
    
    dict <- gVars$dkeys  
    uniqDictLab <- unique(dict$label)  
    choices_options <- c("Safe","Fast check","Slow check")
    
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
    if (nrow(enrichments)==0) {choices_options <- choices_options[!choices_options %in% "Slow check"]}
    
    shinyBS::updateButton(session, "generate_dict_updated_version", style="info")
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
    if (input$outcomes_dictXcandidates == "Slow check"){
      table_toshow <- gVars$enrichments()
    } else if (input$outcomes_dictXcandidates == "Fast check"){
      table_toshow <- gVars$outsider()
    } else { # safe items
      table_toshow <- gVars$safe()
    }
    
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
    hit_terms <- lapply(seq_along(tmp_lst), function(k){ 
      d <- dict[dict$label == tmp_lst[[k]][1],] 
      list(c(tmp_lst[[k]][1] %in% d$label              ,                         
             tmp_lst[[k]][2]%in% d$lab_syn  ,            
             tmp_lst[[k]][3]%in% d$allowed_features  ,
             tmp_lst[[k]][4] %in% d$syn_features
      ))
    }) 
    hit_terms <-  t(bind_cols(hit_terms)) 
    
    if(is_empty(hit_terms)){
      #remove the outcome
      gVars$empty_prestored_voc_tables <- gVars$empty_prestored_voc_tables[!gVars$empty_prestored_voc_tables %in% input$outcomes_dictXcandidates]
    }
    if (is_empty(hit_terms)){ 
      hit_terms <- NULL 
    }
    table_toshow <- cbind(table_toshow,hit_terms) %>% data.frame(., check.names=FALSE, row.names = NULL)
    
    req(input$outcomes_dictXcandidates)
    input_tab <- table_toshow %>% dplyr::rename("Reference Label"=2,"Label Synonym"=3,"Content"=5,"Content Synonym"=6)
    
    output$dt_toshow <- DT::renderDataTable(
      DT::datatable(
        input_tab,  class = "row-border", selection=list(mode="single", target ="row"),
        options=list(columnDefs=list(list(visible=FALSE, targets=extra_ghost_cols)))
      )  %>%
        formatStyle(columns=vect_table_cols,
                    valueColumns = ghost_cols,
                    color= styleEqual(c("0","1"),
                                      c("#DC3545","#04AA6D")) 
                    #       target="row",
                    #      backgroundColor = styleEqual(c(0, 1), c( 'transparent', "red"))
        )
    )
    
    
    shinyBS::updateButton(session, "generate_dict_updated_version", style="info")
    gVars$dt_temporary_cases_voc <- data.frame(table_toshow) 
  })
  
  
  observe({
    req(is.null(input$dt_toshow_rows_selected))
    shinyBS::updateButton(session, "update_To_review", style="info", disabled = TRUE)
    shinyBS::updateButton(session, "update_To_discard", style="info", disabled = TRUE)
    shinyBS::updateButton(session, "update_voc_button", style="info", disabled = TRUE)
  })
  
  
  #retrieved the correspondent entry from the dictionary
  observeEvent(input$dt_toshow_rows_selected,{    
    
    shinyBS::updateButton(session, "update_To_review", style="info", disabled = FALSE)
    shinyBS::updateButton(session, "update_To_discard", style="info", disabled = FALSE)
    shinyBS::updateButton(session, "update_voc_button", style="info", disabled = FALSE) 
    shinyBS::updateButton(session, "generate_dict_updated_version", style="info")
    
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
    shinyBS::updateButton(session, "update_voc_button", style="success")
    
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
    shinyBS::updateButton(session, "update_To_review", style="success")  
    
    full_df_entries <- gVars$potential_new_entries_report
    new_entries <- gVars$dt_temporary_cases_voc[input$dt_toshow_rows_selected,-c(1,4,7,8:11)] #input$dt_toshow_rows_selected
    tmp_for_removal_new_entry <- gVars$dt_temporary_cases_voc[input$dt_toshow_rows_selected,-c(8:11)] 
    
    new_entries<-new_entries%>% 
      setNames(.,c("Label","Label_synonym","Content","Content_synonym"))
    
    
    #retrieve the corresponding row index of the row containing all entries in the main entry table
    row_index <- which(Reduce(`&`, Map(`%in%`, full_df_entries[,c(3,4,6,7)], new_entries[1,])))
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
    shinyBS::updateButton(session, "update_To_discard", style="success")
    
    full_df_entries <- gVars$potential_new_entries_report
    new_entries <- gVars$dt_temporary_cases_voc[input$dt_toshow_rows_selected,-c(1,4,7,8:11)] #input$dt_toshow_rows_selected
    tmp_for_removal_new_entry <- gVars$dt_temporary_cases_voc[input$dt_toshow_rows_selected,-c(8:11)] 
    
    new_entries<-new_entries%>% 
      setNames(.,c("Label","Label_synonym","Content","Content_synonym"))
    
    
    #retrieve the corresponding row index of the row containing all entries in the main entry table
    row_index <- which(Reduce(`&`, Map(`%in%`, full_df_entries[,c(3,4,6,7)], new_entries[1,])))
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
      shinyBS::bsButton("glp_store_commentVOC", label="Add", style="info"),
      
      if (failed){
        div(br(),tags$b("In GLP mode, you must justify your decisions. Please do."), style = "color: red;")},
      footer =  NULL#modalButton("Cancel")
      
    )
  }
  
  
  
  # When Add button(glp_store_comment) is pressed, it tests if the textareainput box is not empty. 
  # If successful, remove the modal, otherwise it shows another modal with a failure message.
  observeEvent(input$glp_store_commentVOC, {
    row_index <- gVars$selected_row_index
    # Check that box for glp comments is filled
    if (input$glp_comments_box != "") {
      tabulated_comment <- paste0("\t","COMMENT: ",input$glp_comments_box)
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
  
  #cleans the datatable asfe the rows are checked(both if accepted, or sent to issue)
  observeEvent(react_trigger_UPDvoc_and_issue(),{
    req(input$outcomes_dictXcandidates, input$dt_toshow_rows_selected)
    req(!is.null(gVars$dkeys))
    
    if (input$outcomes_dictXcandidates == "Slow check"){
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
      tmptable <- gVars$safe()
      tmptable<-tmptable[-input$dt_toshow_rows_selected,]
      tmp_nrow<- nrow(tmptable)
      gVars$safe(tmptable)
      
    }
    
    
    #cleans the selectizeinput from safe/to chekc/Slow check once they are empty (=all entries were processed)
    if (tmp_nrow == 0){
      gVars$empty_prestored_voc_tables <- gVars$empty_prestored_voc_tables[!gVars$empty_prestored_voc_tables %in% input$outcomes_dictXcandidates]
      if (length(gVars$empty_prestored_voc_tables) != 0){
        updateSelectizeInput(session,
                             inputId="outcomes_dictXcandidates",
                             label="Potential outcome:",
                             choices=gVars$empty_prestored_voc_tables,#c("Safe","Fast check","Slow check"),
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
    #req(!is.null(gVars$dkeys)) 
    curr_dict <- gVars$dkeys %>% filter(if_any(.cols=everything(), ~ !grepl("*fillertoremove*",.)))
    new_entries <- dplyr::filter(gVars$potential_new_entries_report, Decision=="Newly Accepted")[,c(3,4,6,7)] %>% 
      setNames(., c("label","lab_syn", "allowed_features", "syn_features")) 
  
    tmp_upd_vocabolary <- rbind(curr_dict,new_entries) %>% group_by(label, lab_syn,syn_features) %>% 
      
      # summarize(~paste(unique(na.omit(.)), collapse = ',')) %>%
      summarize_all(~paste(unique(na.omit(.)), collapse = ',')) %>%
      .[,c("label", "lab_syn","allowed_features", "syn_features")]
    #tmp_upd_vocabolary$syn_features <- strsplit(tmp_upd_vocabolary$syn_features, ",") %>% lapply(., unique) 
    #I:current vocabulary, O: convert to an object collections::dict  (key:value) enriched by new terms
    new_OBJdict <- convert_df_to_dict(tmp_upd_vocabolary) 
    
    # I:enriched new object collections::dict    O: list of (compact_dict_to_display, extended_dict) 
    tmp_convert_dict_to_df <- convert_dict_to_df(new_OBJdict) 
    gVars$compact_voc <- tmp_convert_dict_to_df[[1]]
    gVars$dkeys <- tmp_convert_dict_to_df[[2]] 
    
    shinyBS::updateButton(session, "update_To_review", style="info")
    shinyBS::updateButton(session, "update_To_discard", style="info")
    shinyBS::updateButton(session, "update_voc_button", style="info")
    shinyBS::updateButton(session, "generate_dict_updated_version", style="success")
    
  }) 
  
  
  output$pt_text_recod <- renderText({"<b>Procedural Track</b>"})
  output$pt_recod <- renderText({"<i><small>Waiting for action!</small></i>"})
  
  
  
  #disable skip full column recoding if the content was edited (it is not user intention to skip what was modified)
  observe({
    req(input$step_choice ==2)
    req(!is.null(isolate(input$recoded_content)), !is_empty(gVars$orig_cont),!is.null(input$pick_cols_recoding)  ) 
    
    #if last stored reference label is the current one (identical values), it will not be possible to reject the full column recoding, so the button will be disabled 
    req(!is.null(gVars$later_voc_upd_df))
    last_label <- tail(gVars$later_voc_upd_df,1)$mainL
    skip_active <-  ifelse(identical(last_label, input$pick_cols_recoding), FALSE, TRUE)
    
    
    if(!skip_active) {
      shinyBS::updateButton (session, "reject_full_recoding", disabled = TRUE)  
    } else if (skip_active & !isolate(input$recoded_content) %in% (gVars$orig_cont())){
      shinyBS::updateButton (session, "reject_full_recoding", disabled = TRUE) 
    } else {
      shinyBS::updateButton (session, "reject_full_recoding", disabled = FALSE) 
    }
    
  })
  
  
  
  
  
  
  #YES VOCABULARY ENRICHMENT  
  #vocabulary and storage pattern sections
  pt_STOR_PATTreactors <- reactive({
    paste(input$ON_vocabulary_panel ,input$store_labels, input$store_contents, input$store_button )  
  })   
  
  
  sub_paragraph_storage_pattern <- eventReactive( pt_STOR_PATTreactors(), {
    req(!is.null(input$recoded_label), input$recoded_label !="")
    
    if(!input$recoded_label %in% gVars$all_original_colnames) {     #label was modified
      label_string <- " Edited Label: "
      markerL <- "edited"
    } else {              #label is also in the original table
      label_string <- " Confirmed Label: "
      markerL <- "confirmed"}  
    
    if(!input$recoded_content %in% gVars$orig_cont()) {     #content was modified
      content_string <- " Edited Content: "
      markerC <- "edited"
    } else {          #content is also in the original table
      content_string <- " Confirmed Content: "
      markerC <- "confirmed"
    }  
    
    ed_s <- paste0(paste0(label_string,input$recoded_label,"\n"), 
                   paste0(content_string, input$recoded_content))  
    req(input$store_labels , input$store_contents )  
    
    
    # if input$content_TOclean_FORvocabulary has multiple item, the dataframe for the candidates is build differently
    skip <- gVars$skip_marker
    req(!is.null(input$content_TOclean_FORvocabulary))
    if (skip==TRUE){  #obtained from skip_full_recoding
      collapsed_multi_SKP_orig_contents <- paste(input$content_TOclean_FORvocabulary,collapse=",")
      later_voc_upd_row <- data.frame(t(c(input$store_labels, input$recoded_label, input$label_TOclean_FORvocabulary, 
                                          input$store_contents, collapsed_multi_SKP_orig_contents, NA, gVars$username))) %>%
        setNames(.,c("typeL","mainL","newL","typeC","mainC","newC", "usr")) %>% 
        mutate_all(na_if, "") %>%
        separate_rows(., mainC, sep=",")
      # updates of the "confirmed content" string above
      
      ed_s <- paste0(paste0(label_string,input$recoded_label,"\n"), 
                     paste0(content_string, paste0(length(input$content_TOclean_FORvocabulary)," out of ",length(gVars$rejected_full_col), " contents evaluated.")))  
      markerC <- "multiC_entries"
    } else{ # skip_marker=FALSE obtained from normal recoding
      later_voc_upd_row <- data.frame(t(c(input$store_labels, input$recoded_label, input$label_TOclean_FORvocabulary, 
                                          input$store_contents, input$recoded_content, input$content_TOclean_FORvocabulary, gVars$username))) %>%
        setNames(.,c("typeL","mainL","newL","typeC","mainC","newC", "usr")) %>% 
        mutate_all(na_if, "") %>%
        separate_rows(., mainC, sep=",") 
    }
    
    
    
    req(!is.null(input$label_TOclean_FORvocabulary) & nzchar(input$label_TOclean_FORvocabulary))
    
    if (input$store_labels== 1){      # old label as synonym of the edited label
      if (markerL %in% "confirmed") {
        op_label_string <- paste0 ("\t","As confirmed value, '", input$label_TOclean_FORvocabulary, "' is temporarily stored only as 'Reference Label' and discarded as 'Synonym'.") 
      } else if (markerL %in% "edited" && (!input$recoded_label %in% input$pick_cols_recoding) ) {  #edited but different from the original (i.e. first colname modification in a recodeAC column round)
        op_label_string <- paste0 ("\t","'", input$label_TOclean_FORvocabulary, "' is temporarily stored as a 'Label Synonym' of the edited label.")  #confirmed
      } else {  #edited but identical to the original (i.e. NOT first colname modification in a recodeAC column round, so the new "orig" is the recoded label of the first round)
        op_label_string <- paste0 ("\t","The edited label is temporarily stored only as 'Reference Label'. The original has already been previously processed.")  
      }
      between_Labels_and_Content_Strings <- ifelse(op_label_string=="", "\t","\n \t")
      
      if (input$store_contents ==3){
        if(markerC %in% "edited"){
          s <- paste0(paste0 (op_label_string, between_Labels_and_Content_Strings),
                      paste0 ("'",input$content_TOclean_FORvocabulary, "' is temporarily stored as a 'Content Synonym' of the edited content.")) 
        } else if (markerC %in% "confirmed") {   
          s <- paste0(paste0 (op_label_string, between_Labels_and_Content_Strings),
                      paste0 ("As confirmed value, '", input$content_TOclean_FORvocabulary, "' is temporarily stored only as 'Reference Content' and discarded as 'Synonym'.")) 
        } else {   #markerC "multiC_entries"
          extended_contents <- toString(input$content_TOclean_FORvocabulary)
          len <- length(extended_contents)
          verb <- ifelse(len ==1, "was", "were") 
          pluralC <- ifelse(len==1, "Content", "Contents") 
          selected_stringed_in_one <- paste0('"', paste(input$content_TOclean_FORvocabulary, collapse='", "'), '"')
          s <- paste0(paste0 (op_label_string, between_Labels_and_Content_Strings),
                      paste0 ("Skipped full recoding, but", selected_stringed_in_one," ", verb, " temporarily stored only as 'Reference ", pluralC,  "'.")) 
        }
      }
      
      else if (input$store_contents ==4){        #  do not store contents
        if(markerC %in% "edited"){
          s <- paste0(paste0 (op_label_string, between_Labels_and_Content_Strings),
                      paste0("The edited content is stored, while the original ('",input$content_TOclean_FORvocabulary ,"') was discarded."))
          
        } else if (markerC %in% "confirmed"){   #markerC "Confirmed"
          s <- paste0(paste0 (op_label_string, between_Labels_and_Content_Strings),
                      paste0 ("As confirmed value, '", input$content_TOclean_FORvocabulary, "' is temporarily stored only as 'Reference Content' and discarded as 'Synonym'.")) 
        } else {   #markerC "multiC_entries"
          s <- paste0(paste0 (op_label_string, between_Labels_and_Content_Strings),
                      paste0 ("Skipped full recoding, and all original entries were discarded from being potential 'Reference Content'.")) 
          if (length(toString(input$content_TOclean_FORvocabulary))==1) {
            later_voc_upd_row[,5] <- NA 
          }
        }
        later_voc_upd_row[,6] <- NA 
      }
    }
    
    
    if (input$store_labels==2){       #do not store original label  
      if (markerL %in% "confirmed") {
        op_label_string <- paste0 ("\t","As confirmed value, '", input$label_TOclean_FORvocabulary, "' is temporarily stored only as 'Reference Label' and discarded as 'Synonym'.") 
      } else if (markerL %in% "edited" && (!input$recoded_label %in% input$pick_cols_recoding) ) {  #edited but different from the original (i.e. first colname modification in a recodeAC column round)
        op_label_string <- paste0("\tThe edited label is stored, while the original ('",input$label_TOclean_FORvocabulary ,"') was discarded. ")  
      } else {  #edited but identical to the original (i.e. NOT first colname modification in a recodeAC column round, so the new "orig" is the recoded label of the first round)
        op_label_string <- paste0 ("\t","The edited label is temporarily stored only as 'Reference Label'. The original has already been previously processed.")  
      }
      between_Labels_and_Content_Strings <- ifelse(op_label_string=="", "\t","\n \t")
      
      
      if (input$store_contents ==3){
        if(markerC %in% "edited"){
          s <- paste0(paste0 (op_label_string, between_Labels_and_Content_Strings),
                      paste0 ("'",input$content_TOclean_FORvocabulary, "' is temporarily stored as a 'Content Synonym' of the edited content.")) 
        } else if (markerC %in% "confirmed") {   
          s <- paste0(paste0 (op_label_string, between_Labels_and_Content_Strings),
                      paste0 ("As confirmed value, '", input$content_TOclean_FORvocabulary, "' is temporarily stored only as 'Reference Content' and discarded as 'Synonym'.")) 
        } else {   #markerC "multiC_entries"
          extended_contents <- input$content_TOclean_FORvocabulary
          len <- length(extended_contents)
          verb <- ifelse(len ==1, "was", "were") 
          selected_stringed_in_one <- paste0('"', paste(input$content_TOclean_FORvocabulary, collapse='", "'), '"')
          pluralC <- ifelse(len==1, "Content", "Contents") 
          s <- paste0(paste0 (op_label_string, between_Labels_and_Content_Strings),
                      paste0 ("Skipped full recoding, but ", selected_stringed_in_one," ", verb, " temporarily stored only as 'Reference ", pluralC,  "'.")) 
        }
        
        later_voc_upd_row[,3] <- NA
      }
      
      
      else if (input$store_contents ==4){        #  do not store contents
        if(markerC %in% "edited"){
          s <- paste0(paste0 (op_label_string, between_Labels_and_Content_Strings),
                      paste0("The edited content is stored, while the original ('",input$content_TOclean_FORvocabulary ,"') was discarded."))
          
        } else if (markerC %in% "confirmed"){   #markerC "Confirmed"
          s <- paste0(paste0 (op_label_string, between_Labels_and_Content_Strings),
                      paste0 ("As confirmed value, '", input$content_TOclean_FORvocabulary, "' is temporarily stored only as 'Reference Content' and discarded as 'Synonym'.")) 
        } else {   #markerC "multiC_entries"
          s <- paste0(paste0 (op_label_string, between_Labels_and_Content_Strings),
                      paste0 ("Skipped full recoding, and all original entries were discarded from being potential 'Reference Content'.")) 
          if (length(toString(input$content_TOclean_FORvocabulary))==1) {
            later_voc_upd_row[,5] <- NA 
          }
        }
        
        later_voc_upd_row[,c(3,6)] <- NA 
      }
    }
    
    gVars$later_voc_upd_row_tmp_tmp <- later_voc_upd_row
    final_s <- paste(ed_s,"\n",s)
    final_s
  })
  
  observeEvent(input$store_button, {
    req(!is.null(gVars$later_voc_upd_row_tmp_tmp))
    #simplify intermediate dict in case of identical values
    later_voc_upd_row <- gVars$later_voc_upd_row_tmp_tmp %>% as.data.frame(.)
    if (dim(later_voc_upd_row)[1]==1){
      if (identical(later_voc_upd_row[[2]],later_voc_upd_row[[3]])){  later_voc_upd_row[[3]] <-NA}
      if (!is.na(later_voc_upd_row[[5]]) && identical(later_voc_upd_row[[5]],later_voc_upd_row[[6]])){  later_voc_upd_row[[6]] <-NA}
    } else {  
      if (identical(later_voc_upd_row[[2]],later_voc_upd_row[[3]])){  later_voc_upd_row[[3]] <-NA}
      if (!anyNA(later_voc_upd_row[[5]]) && identical(later_voc_upd_row[[5]],later_voc_upd_row[[6]])){  later_voc_upd_row[[6]] <-NA}
      
    }
    gVars$later_voc_upd_row_tmp <- as_tibble(later_voc_upd_row)
    
  })
  
  
  
  observe({ 
    req(!is_null(input$store_labels), !is_null(input$store_contents))
    if(!is.null(input$label_TOclean_FORvocabulary) | !is.null(input$content_TOclean_FORvocabulary) |
       ! "" %in% c(input$label_TOclean_FORvocabulary, input$content_TOclean_FORvocabulary)) {
      shinyjs::enable("store_button")
    } else {
      shinyjs::disable("store_button")
    }
  })
  
  
  
  
  observeEvent(input$store_button, { 
    req(!is.null(gVars$skip_marker) && gVars$skip_marker==TRUE)
    tmp_recode_rj_element <- gVars$tmp_recode_rj_element
    #storing for undostep
    recode_RJ <- list( tmp_recode_rj_element, input$content_TOclean_FORvocabulary ) 
    gVars$recodeRJ_op_list <- append(gVars$recodeRJ_op_list, list(recode_RJ=recode_RJ))
    
    gVars$last5list <- c(gVars$last5list, (list(recode_RJ=recode_RJ)))
    
    
    newlabel <- tmp_recode_rj_element[1]
    oldlabel <- tmp_recode_rj_element[2]
    #cleaning the list to show in selectizeInput
    gVars$processedLIST2REC <- c(gVars$processedLIST2REC,newlabel)
    gVars$list_cols_tmp <- gVars$list_cols_tmp[gVars$list_cols_tmp != oldlabel]
    t1 <- gVars$list_cols_tmp [!gVars$list_cols_tmp %in% gVars$processedLIST2REC] %>% .[!. %in% gVars$deleted_colname] %>%
      .[!. %in%gVars$processed_cols_RECOD]
    updateSelectizeInput(session,
                         inputId = "pick_cols_recoding",
                         choices = t1,
                         selected = 2,
                         server=TRUE
    )
    
  })
  
  
  
  #updates the proedural track with what was done, (YES vocabulary enrichment in recoding part)
  observeEvent( pt_STOR_PATTreactors() ,{
    Value <-  sub_paragraph_storage_pattern()
    
    #prepares msg for pt box 
    Value <- Value %>% gsub(pattern = "\n", replacement = "<br/>", .)  %>%
      gsub(pattern = "\t", replacement = "&nbsp; &nbsp; &nbsp;", .) 
    output$pt_recod <- renderText({Value})
    
  })
  
  observeEvent (input$store_button ,{ 
    Value <-  sub_paragraph_storage_pattern()
    
    #prepares msg for pt box 
    Value <- Value %>% gsub(pattern = "\n", replacement = "<br/>", .)  %>%
      gsub(pattern = "\t", replacement = "&nbsp; &nbsp;", .) 
    gVars$pt_glp_list <- c(gVars$pt_glp_list, Value)
    
    #updates buttons
    shinyBS::updateButton(session, "store_button", style="success", disabled=TRUE)
    shinyBS::updateButton(session, "glp_pt_button_rec", disabled=FALSE)
    
    
  })
  
  
  
  observeEvent(input$glp_pt_button_rec,{
    req(!is_empty( gVars$later_voc_upd_row_tmp),!is.null( gVars$later_voc_upd_row_tmp))
    #stores the ops
    gVars$later_voc_upd_df <- rbind(gVars$later_voc_upd_df, gVars$later_voc_upd_row_tmp) 
    data$voc_enrich_panel <- FALSE
    enable("pick_recoding_content_pairs")
    shinyjs::enable("pick_cols_recoding") 
    
  })
  
  
  
  
  #Activation/deactivation of all buttons and menus in recoding page
  # modification of buttons add/GLP according to the selection of GLP mode activation in sidebar
  observe({
    if (!input$GLP_inactivation_mode) {    #GLP enabled
      shinyBS::updateButton(session, "glp_pt_button_del", label=" GLP", style="info")
      shinyBS::addTooltip(session,"glp_pt_button_del", "Launch a graphical window, to enrich the procedural track info with comments according to GLP regulations.", placement="bottom")
      
      shinyBS::updateButton(session, "glp_pt_button_rec", label=" GLP", style="info")
      shinyBS::addTooltip(session,"glp_pt_button_rec", "Launch a graphical window, to enrich the procedural track info with comments according to GLP regulations.", placement="bottom")
      
    } else {
      shinyBS::updateButton(session, "glp_pt_button_del", label=" Add", style="info")
      shinyBS::removeTooltip(session,"glp_pt_button_del")
      
      shinyBS::updateButton(session, "glp_pt_button_rec", label=" Add", style="info")
      shinyBS::removeTooltip(session,"glp_pt_button_rec")
    }
  })
  
  
  
  
  
  observeEvent(input$glp_pt_button_rec, {
    enable("undo_save_button")
    if(!input$GLP_inactivation_mode){    #such as GLPmode activated
      showModal(dataModal())
    } 
    else{
      #reupdate buttons and menus
      shinyBS::updateButton(session, "save_recoding", style="info", disabled=FALSE)
      shinyBS::updateButton(session, "reject_full_recoding", style="info", disabled=FALSE)
      toggleModal(session, "modalstep", "close")
      
      
      updateRadioButtons(session, "store_labels", 
                         choices = list("as Label Synonym" = 1, "Do not store"=2), 
                         selected = character(0))
      
      
      updateRadioButtons(session, "store_contents",
                         choices = list("as Content Synonym" = 3, "Do not store"=4),
                         selected = character(0))
      
      
      shinyBS::updateButton(session, "store_button", style="info", disabled=TRUE) 
      shinyBS::updateButton(session, "glp_pt_button_rec", disabled=TRUE)
      updateRadioButtons(session, "step_choice",   choices= c("Delete" = 1, "Modify and/or Save" = 2, "Specials" =3) , selected = character(0), inline=TRUE)
      
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
    if(!isTruthy(input$pick_cols_recoding)){
      shinyjs::info("Please select a column in the previous window!")
      return (NULL)
    }
    req(isTruthy(input$pick_cols_recoding))
    current_df <- gVars$phTable[[1]]
    tmp_selected_col <- current_df[,input$pick_cols_recoding]
    
    gsm = sapply(tmp_selected_col, function(x) substr(x,0,regexpr('_', x)[1]-1))
    slide = sapply(tmp_selected_col, function(x) substr(x,regexpr('_', x)[1] + 1, regexpr('_S',x)[1] - 1))
    array = sapply(tmp_selected_col, function(x) substr(x,regexpr('_[1-4]_', x)[1]+1, regexpr('_[1-4]\\.txt',x)[1]+1))
    
    gVars$agi_splitted_new_cols <- data.frame(gsm=gsm, slide=slide, array=array)
    
    new_colnames <- c("gsm", "slide", "array")
    if(is.null(gVars$support_for_correction) && any(colnames(gVars$phTable[[1]]) %in% new_colnames) ) {  
      shinyjs::info(paste0("The indicated names of newly splitted columns are already present in the dataset (", toString(new_colnames[new_colnames %in% colnames(gVars$phTable[[1]])]) ,"). Please change!"))
      return (NULL)
    } 
    
    if(any(gVars$agi_splitted_new_cols!="")){ 
      updateButton(session,  "save_agil_button",disabled = FALSE)
      updateButton(session,  "test_agil_button",style="success",disabled = FALSE)
      updateButton(session,  "reset_agitest_table",disabled = FALSE)
        
      gVars$AGIsplit_msg <- paste0("Column '",input$pick_cols_recoding, "' was splitted into 'gsm', 'slide' and 'array'. ")
      output$pt_AGIsplit <- renderText({"..."})
    }
  })
  
  
  output$table_AGI_splitted_cols <- renderDataTable({
    req(isTruthy(input$pick_cols_recoding))
    req(!is_empty(gVars$agi_splitted_new_cols))
    cols_toshow <- as.data.frame(gVars$agi_splitted_new_cols)
    if(all(cols_toshow=="")) { 
      col_toshow <- NULL
      return (NULL)
    }
    DT::datatable(
      cols_toshow, selection="none",options =list(scrollX=TRUE,dom="t"))
  })
  
  
  
  #reset test table
  observeEvent(input$reset_agitest_table,{ 
    output$table_AGI_splitted_cols <-renderDataTable({NULL})
    updateRadioButtons(session, "step_choice",    choices = c("Delete" = 1, "Modify and/or Save" = 2, "Specials" =3) , selected = character(0), inline=TRUE)
    updateButton(session,  "reset_agitest_table",disabled = TRUE)
    updateButton(session,  "save_agil_button", style="info", disabled = TRUE)
    updateButton(session,  "test_agil_button",style="info",disabled = TRUE)
    
  })
  
  
  observeEvent(input$save_agil_button,{
    req(input$pick_cols_recoding)
    req(any(gVars$agi_splitted_new_cols!=""))
    gVars$phTable[[1]] <- cbind(gVars$phTable[[1]],gVars$agi_splitted_new_cols)
    gVars$support_for_correction <- "action_done"
    
    #storing stage for undostep
    special_agiSPL <- list( input$pick_cols_recoding,colnames(gVars$agi_splitted_new_cols) )
    gVars$special_op_list <- c(gVars$special_op_list, (list(special_agiSPL =special_agiSPL)))
    gVars$last5list <- c(gVars$last5list, (list(special_agiSPL =special_agiSPL)))
    
    value <- gVars$AGIsplit_msg
    updateButton(session,  "glp_pt_button_AGIsplit",disabled = FALSE)
    updateButton(session,  "save_agil_button", style="success", disabled = TRUE)
    updateButton(session,  "test_agil_button",style="info",disabled = TRUE)
    updateButton(session,  "reset_agitest_table",disabled = TRUE)
    updateButton(session,  "undo_save_button",disabled = TRUE)
    
    output$pt_AGIsplit <- renderText({value})
    
  })
  
  
  
  observeEvent(input$glp_pt_button_AGIsplit,{
    gVars$pt_glp_list <- c(gVars$pt_glp_list, gVars$AGIsplit_msg)
    if(!input$GLP_inactivation_mode){    #such as GLPmode activated
      showModal(dataModal())
    } else{
          updateSelectizeInput(session,
                               inputId = "pick_cols_recoding",
                               #gVars$list_cols_tmp options present before",
                               choices = c(gVars$list_cols_tmp, "gsm", "slide", "array"),
                               selected = character(0),
                               server=TRUE)
          updateButton(session,  "glp_pt_button_split",disabled = TRUE)
          updateButton(session,  "test_agil_button",disabled = FALSE)
          updateButton(session,  "save_agil_button", style="info", disabled = TRUE)
          updateButton(session,  "glp_pt_button_AGIsplit", style="info", disabled = TRUE)
          updateButton(session, "undo_save_button", disabled=FALSE) 
          gVars$support_for_correction <- NULL
          
          output$pt_AGIsplit <- renderText({"<i><small>Waiting for action!</small></i>"})
          updateRadioButtons(session, "step_choice",    choices= c("Delete" = 1, "Modify and/or Save" = 2, "Specials" =3) , selected = character(0), inline=TRUE)
    }
  })
  
  
  observe({
    if (!input$GLP_inactivation_mode) {    #GLP enabled
      shinyBS::updateButton(session, "glp_pt_button_AGIsplit", label=" GLP", style="info")
      shinyBS::addTooltip(session,"glp_pt_button_AGIsplit", "Launch a graphical window, to enrich the procedural track info with comments according to GLP regulations.", placement="bottom")
    } else {
      shinyBS::updateButton(session, "glp_pt_button_AGIsplit", label=" Add", style="info")
      shinyBS::removeTooltip(session,"glp_pt_button_AGIsplit") 
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
    #check if new names are already present
    if(any(colnames(gVars$phTable[[1]]) %in% new_names) ) {
      shinyjs::info(paste0("One or more of the names indicated to rename the new columns is already present in the dataset (", toString(new_names[new_names %in% colnames(gVars$phTable[[1]])]) ,"). Please change!"))
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
    gVars$support_for_correction <- "action_done"
    
    #storing stage for undostep
    special_addEMPTY <- gVars$tmp_new_names <- list(new_names )
    gVars$special_op_list <- c(gVars$special_op_list, (list(special_addEMPTY =special_addEMPTY)))
    gVars$last5list <- c(gVars$last5list, (list(special_addEMPTY =special_addEMPTY)))
    
    #for pt message
    gVars$addEmpty_msg <- paste(input$new_cols, "new column(s) was/were added and named", toString(new_names),".")
    output$pt_addEmpty <- renderText({gVars$addEmpty_msg})
    
    updateButton(session,  "add_Fempty_cols_button", style="success",disabled = TRUE)
    updateButton(session,  "glp_pt_button_addEmpty",disabled = FALSE)
    updateButton(session, "undo_save_button", disabled=TRUE) 
  })
  
  
  
  observeEvent(input$glp_pt_button_addEmpty,{
    gVars$pt_glp_list <- c(gVars$pt_glp_list, gVars$addEmpty_msg) 
    gVars$list_cols_tmp <- c(gVars$list_cols_tmp,gVars$tmp_new_names[[1]] )
    if(!input$GLP_inactivation_mode){    #such as GLPmode activated
      showModal(dataModal())
    } else{
        updateSelectizeInput(session,
                             inputId = "pick_cols_recoding",
                             #gVars$list_cols_tmp options present before",
                             choices = c(gVars$list_cols_tmp, gVars$tmp_new_names[[1]]),
                             selected = character(0),
                             server=TRUE)
        
        output$pt_AGIsplit <- renderText({"<i><small>Waiting for action!</small></i>"})
        updateNumericInput(session,inputId = "new_cols", label= HTML("Column(s) to add"), min=0, value=0) 
        updateTextInput(session, inputId = "names_added_cols","Name new column(s)", value="")
        updateButton(session,  "add_Fempty_cols_button", style="info",disabled = FALSE)
        updateButton(session,  "glp_pt_button_addEmpty", style="info",disabled = TRUE)
        updateButton(session, "undo_save_button", disabled=FALSE) 
        updateRadioButtons(session, "step_choice",    choices = c("Delete" = 1, "Modify and/or Save" = 2, "Specials" =3) , selected = character(0), inline=TRUE)
        output$pt_addEmpty <- renderText({"<i><small>Waiting for action!</small></i>"})
        gVars$support_for_correction <- NULL
    }
    
  })
  
  
  observe({
    if (!input$GLP_inactivation_mode) {    #GLP enabled
      shinyBS::updateButton(session, "glp_pt_button_addEmpty", label=" GLP", style="info")
      shinyBS::addTooltip(session,"glp_pt_button_addEmpty", "Launch a graphical window, to enrich the procedural track info with comments according to GLP regulations.", placement="bottom")
    } else {
      shinyBS::updateButton(session, "glp_pt_button_addEmpty", label=" Add", style="info")
      shinyBS::removeTooltip(session,"glp_pt_button_addEmpty") 
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
    req(isTruthy(input$pick_cols_recoding) | isTruthy(input$select_col_df_SPLIT))
    updateButton(session,  "reset_test_table",disabled = FALSE)
    updateButton(session,  "run_separ",disabled = FALSE)
    updateTextAreaInput(session, inputId="PT_store_split", "Procedural track", value = "...")
    
    output$test_splitted_cols <- renderDataTable({
      req(!is.null(input$type_of_separator),!is.null(gVars$sel_col_split)) 
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
        if(is.null(gVars$support_for_correction) && any(colnames(gVars$phTable[[1]]) %in% new_colnames) ) {
          shinyjs::info(paste0("The indicated names of newly splitted columns are already present in the dataset (", toString(new_colnames[new_colnames %in% colnames(gVars$phTable[[1]])]) ,"). Please change!"))
          test_table <-  NULL 
          return (NULL)
        } 
        
      }
      req(!is.null( test_table))
      gVars$dt_split = test_table 
      DT::datatable(
        test_table, selection="none",options =list(scrollX=TRUE))
      
    })
    
  })
  
  
  #reset test table
  observeEvent(c(input$reset_test_table,input$reset_test_table_regex),{
    output$test_splitted_cols <-NULL
    updateButton(session,  "reset_test_table",disabled = TRUE)
    updateButton(session,  "run_separ", style="info", disabled = TRUE)
    updateButton(session,  "test_separ",style="info",disabled=FALSE)
    updateSelectizeInput(session,
                         inputId = "type_of_separator", 
                         choices = c("Tab", ",", ";", "Space","", "Other"), 
                         selected = character(0),
                         server=TRUE)
    updateTextInput(session,inputId="other_sep",  value= character(0))
    updateTextInput(session,inputId="names_split_cols",  value= character(0))
    updateCheckboxInput(session, "name_splitted_cols",  value = FALSE)
    updateRadioGroupButtons( session,
                             inputId = "type_of_splitter",
                             label = "Splitting method",
                             choices = c("Separator", "Regular Expression"),
                             selected=character(0),
                             status = "radioGROUPclass", 
                             checkIcon = list(
                               yes = icon("check-square"),
                               no = icon("square-o") #("fas fa-square-o")
                             ))
    
  })
  
  
  
  observeEvent(input$run_separ,{
    req(!is.null(gVars$dt_split))
    req(!is_empty(gVars$dt_split) )
    updateButton(session,  "reset_test_table",disabled = TRUE)
    updateButton(session,  "run_separ",style="success") 
    updateButton(session,  "test_separ",disabled = TRUE)
    updateButton(session, "undo_save_button", disabled=TRUE) 
    
    gVars$phTable[[1]]<- cbind(gVars$phTable[[1]],gVars$dt_split)
    gVars$tmp_new_names <- colnames(gVars$dt_split)
    gVars$support_for_correction <- "action_done"
    
    #storing stage for undostep
    special_SPL <- list(input$pick_cols_recoding, colnames(gVars$dt_split))
    gVars$special_op_list <- c(gVars$special_op_list, (list(special_SPL =special_SPL)))
    gVars$last5list <- c(gVars$last5list, (list(special_SPL =special_SPL)))
    
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
  
  
  observeEvent(input$glp_pt_button_split,{
    req(!is.null(gVars$phTable[[1]] ))
    req(isTruthy(input$glp_pt_button_split)) 
    if (isTRUE(gVars$GLP_status)){
        updateRadioGroupButtons( session,
                                 inputId = "type_of_splitter",
                                 label = "Splitting method",
                                 choices = c("Separator", "Regular Expression"),
                                 selected=character(0),
                                 status = "radioGROUPclass", 
                                 checkIcon = list(
                                   yes = icon("check-square"),
                                   no = icon("square-o") #("fas fa-square-o")
                                 ))
      }
  })
  
  
  #if manual is ticked/unticked - resets the values downflow
  observeEvent(input$other_col_split,{
    req(!is.null(gVars$phTable[[1]] ))
    if (isTRUE(gVars$GLP_status)){
      updateRadioGroupButtons( session,
                               inputId = "type_of_splitter",
                               label = "Splitting method",
                               choices = c("Separator", "Regular Expression"),
                               selected=character(0),
                               status = "radioGROUPclass", 
                               checkIcon = list(
                                 yes = icon("check-square"),
                                 no = icon("square-o") #("fas fa-square-o")
                               ))
  }  
    gVars$sel_col_split <-NULL   #remove the selected shown column table
  })
  
  
  ##storing in procedural track - splitting column section
  sentence1 <- reactive({
    input$run_separ
    input$run_separ_regex
    req(!is_empty(gVars$sel_col_split))
    req(!is.null(gVars$dt_split))
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
  })
  
  
  
  
  pt_SPLIT_MESreactors <- reactive({
    paste(input$run_separ,
          input$run_separ_regex)})
  
  
  #updates the procedural track with what have been done
  observeEvent (pt_SPLIT_MESreactors(),{
    Value <- Value_split()
    updateTextAreaInput(session, inputId="PT_store_split", "Procedural track", value = Value)
    updateButton(session,  "glp_pt_button_split",disabled = FALSE)
  })    
  
  
  
                
  
  
  #undo for both regex and normal splitting part
  observeEvent(input$undo_agil_button,{ 
    #cleaning of the tables
    output$test_splitted_cols <-NULL
    output$col_to_showSPLIT <-NULL
    
    shinyBS::toggleModal(session, "SpecialOPmodal", toggle="close")
  })
  
  
  
  #REGEXP
  observeEvent(input$test_separ_regex,{
    req(isTruthy(input$pick_cols_recoding) | isTruthy(input$select_col_df_SPLIT))
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
      
      if(is.null(gVars$support_for_correction) && any(colnames(gVars$phTable[[1]]) %in% newcolnames) ) {
          shinyjs::info(paste0("The indicated names of newly splitted columns are already present in the dataset (", toString(newcolnames[newcolnames %in% colnames(gVars$phTable[[1]])]) ,"). Please change!"))
          test_table <-  NULL
          return (NULL)
      }
          
      req(!is.null(test_table))
      gVars$dt_split <- test_table 
      DT::datatable(
            test_table, selection="none",options =list(scrollX=TRUE))
      
    })
    
  })
  
  
  
  #reset test table
  observeEvent(c(input$reset_test_table,input$reset_test_table_regex),{
    output$test_splitted_cols <-NULL
    updateButton(session,  "reset_test_table_regex",disabled = TRUE)
    
    updateButton(session,  "run_separ_regex", style="info", disabled = TRUE)
    updateButton(session,  "test_separ_regex",style="info",disabled=FALSE)
    
    updateTextInput(session,inputId="regex_splitting",  value= character(0))
    updateTextInput(session,inputId="names_split_cols_regex",  value= character(0)) 
    gVars$dt_split <-NULL
  })
  
  
  observeEvent(input$run_separ_regex,{ 
    req(!is.null(gVars$dt_split))
    req(!is_empty(gVars$dt_split) )
    updateButton(session,  "reset_test_table_regex",disabled = TRUE)
    updateButton(session,  "run_separ_regex",style="success") 
    updateButton(session,  "test_separ_regex",disabled = TRUE)
    updateButton(session, "undo_save_button", disabled=TRUE) 
    
    gVars$phTable[[1]]<- cbind(gVars$phTable[[1]],gVars$dt_split)
    gVars$tmp_new_names <- colnames(gVars$dt_split)
    gVars$support_for_correction <- "action_done"
    
    #storing stage for undostep
    special_SPLREG <- list(input$pick_cols_recoding,colnames(gVars$dt_split))
    gVars$special_op_list <- c(gVars$special_op_list, (list(special_SPLREG =special_SPLREG)))
    gVars$last5list <- c(gVars$last5list, (list(special_SPLREG =special_SPLREG)))
    
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
  
  
  #for both "Separator" and "Regular Expression"
  observeEvent(input$glp_pt_button_split,{
    gVars$pt_glp_list <- c(gVars$pt_glp_list, input$PT_store_split) 
    gVars$list_cols_tmp <- c(gVars$list_cols_tmp,gVars$tmp_new_names )
    if(!input$GLP_inactivation_mode){    #such as GLPmode activated
      showModal(dataModal())
    } else{
        #Separator and common
      updateSelectizeInput(session,
                           inputId = "pick_cols_recoding",
                           #gVars$list_cols_tmp options present before",
                           choices = c(gVars$list_cols_tmp, gVars$tmp_new_names),
                           selected = character(0),
                           server=TRUE) 
      updateCheckboxInput(session, "other_col_split", "Select manually another specific column", value = FALSE)
      
      updateButton(session,  "glp_pt_button_split",disabled = TRUE)
      updateButton(session,  "reset_test_table",disabled = TRUE)
      updateButton(session,  "run_separ",disabled = TRUE,style="info") 
      updateButton(session,  "test_separ",disabled = TRUE)
      updateRadioButtons(session, "step_choice",    choices = c("Delete" = 1, "Modify and/or Save" = 2, "Specials" =3) , selected = character(0), inline=TRUE)
      
      output$test_splitted_cols <-NULL
      updateTextAreaInput(session, inputId="PT_store_split", "Procedural track", value = "Waiting for action!") 
      updateButton(session, "undo_save_button", disabled=FALSE) 
      
      updateSelectizeInput(session,
                           inputId = "type_of_separator", 
                           choices = c("Tab", ",", ";", "Space","", "Other"), 
                           selected = character(0),
                           server=TRUE)
      updateTextInput(session,inputId="other_sep",  value= character(0))
      updateTextInput(session,inputId="names_split_cols",  value= character(0))
      updateCheckboxInput(session, "name_splitted_cols",  value = FALSE)
      updateSelectizeInput(session,inputId = "select_col_df_SPLIT",   selected = character(0), #multiple = FALSE,
                           options = list(
                             placeholder= "columns from loaded dataframe", onInitialize = I('function() { this.setValue(""); }')),
                           server=TRUE)
      updateRadioGroupButtons( session,
                               inputId = "type_of_splitter",
                               label = "Splitting method",
                               choices = c("Separator", "Regular Expression"),
                               selected=character(0),
                               status = "radioGROUPclass", 
                               checkIcon = list(
                                 yes = icon("check-square"),
                                 no = icon("square-o") #("fas fa-square-o")
                               ))
      gVars$dt_split <- NULL
      gVars$support_for_correction <- NULL
      
       #regular expression
        updateButton(session,  "reset_test_table_regex",disabled = TRUE)
        updateButton(session,  "run_separ_regex",disabled = TRUE,style="info") 
        updateButton(session,  "test_separ_regex",disabled = TRUE)
        
        updateTextInput(session,inputId="regex_splitting",  value= character(0))
        
        updateTextInput(session,inputId="names_split_cols_regex",  value= character(0))
        
    }
    
    
  })
  
  
  observe({
    if (!input$GLP_inactivation_mode) {    #GLP enabled
      shinyBS::updateButton(session, "glp_pt_button_split", label=" GLP", style="info")
      shinyBS::addTooltip(session,"glp_pt_button_split", "Launch a graphical window, to enrich the procedural track info with comments according to GLP regulations.", placement="bottom")
    } else {
      shinyBS::updateButton(session, "glp_pt_button_split", label=" Add", style="info")
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
  

  observeEvent(c(input$update_glp_button, input$download_single_rpt, input$download_multi_rpt, input$reset_window_reports), {
    req(!is.null(gVars$username))
    tmpcolname<- ifelse (!input$GLP_inactivation_mode, "operation_and_comment", "only_operation") 
    report_pt_glp <- gVars$pt_glp_list 
    if (!"list" %in% class(report_pt_glp)){
      gVars$pt_glp_list <- as.list(report_pt_glp)
    }
    gVars$glp_comment_rep <- as.data.frame(report_pt_glp) %>% do.call(rbind.data.frame, .)  %>% 
      setNames(.,tmpcolname ) %>%    cbind(nstep=row.names(.), .)
  })
  
  
  output$glp_report <- renderDT(
    gVars$glp_comment_rep,rownames=F  ,escape=F, options = list(scrollX = TRUE, iDisplayLength = 50))
  
  #activates button only if the table is updated to the last performed operation
  output$activate_download <- renderUI({
    req(!is.null(gVars$pt_glp_list), !is.null(gVars$glp_comment_rep))
    if(nrow(gVars$glp_comment_rep) == length(gVars$pt_glp_list)) {
      shinyBS::updateButton(session, "update_glp_button",  label="No Need to Update",  style="info", disabled = FALSE) 
      downloadButton("download_Upd_GLP", "Download procedural/GLP steps")
    } else {shinyBS::updateButton(session, "update_glp_button",   label="Update to Download", style="warning", disabled = FALSE) }
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
      shinyjs::disable("generate_dict_updated_version")
    } else {
      shinyjs::enable("download_to_review_accepted")
      shinyjs::enable("download_Upd_Vocabulary")
      shinyjs::enable("generate_dict_updated_version")
    }
  })
  
  
  #issue
  output$download_to_review_issue <- downloadHandler(
    #arrange new file name
    current_df_file_name <- gVars$current_df_file_name %>% 
      sub('.[^.]*$', '', .) %>%
      paste(., collapse = "_"),
    
    filename = function() {
      paste0("issue_cand_for_voc_",current_df_file_name,"_v",
             format(Sys.time(), "%Y-%m-%d_%H-%M-%S"),"_",gVars$sha256_stringID,
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
  
  
  output$download_to_review_issue2 <- downloadHandler(
    #arrange new file name
    current_df_file_name <- gVars$current_df_file_name %>% 
      sub('.[^.]*$', '', .) %>%
      paste(., collapse = "_"),
    
    filename = function() {
      file = paste0("issue_cand_for_voc_",current_df_file_name,"_v",
                    format(Sys.time(), "%Y-%m-%d_%H-%M-%S"),"_",gVars$sha256_stringID,
                    ".xlsx", 
                    sep = "")
    },
    
    content = function(file) {
         req(!is.null(gVars$potential_new_entries_report))
      
      shinyjs::html(id="loadingText", "Selecting Issue Vocabulary entries")
      shinyjs::show(id="loading-content")
      on.exit({
        shinyjs::hide(id="loading-content", anim=TRUE, animType="fade")    
      })
      
      for_report <- gVars$potential_new_entries_report
      issue_file <- data.frame(for_report) %>% .[.$Decision %in% c("Issue"),]
      
      write.xlsx(issue_file, file, row.names=FALSE)
      print("Issue list stored!")
      data$whichBAR <- "upd_voc_issue"
    }
  )
  
  
  
  #discard
  output$download_to_review_discard <- downloadHandler(
    #arrange new file name
    current_df_file_name <- gVars$current_df_file_name %>% 
      sub('.[^.]*$', '', .) %>%
      paste(., collapse = "_"),
    
    filename = function() {
      file = paste0("discard_cand_for_voc_",current_df_file_name, "_v",
                    format(Sys.time(), "%Y-%m-%d_%H-%M-%S"),"_",gVars$sha256_stringID,
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
  
  
  output$download_to_review_discard2 <- downloadHandler(
    #arrange new file name
    current_df_file_name <- gVars$current_df_file_name %>% 
      sub('.[^.]*$', '', .) %>%
      paste(., collapse = "_"),
    
    filename = function() {
      file = paste0("discard_cand_for_voc_",current_df_file_name,"_v",
                    format(Sys.time(), "%Y-%m-%d_%H-%M-%S"),"_",gVars$sha256_stringID,
                    ".xlsx", 
                    sep = "")
    },
    
    content = function(file) {
      #   req(!is.null(gVars$potential_new_entries_report))
      
      shinyjs::html(id="loadingText", "Selecting Discarded Vocabulary entries")
      shinyjs::show(id="loading-content")
      on.exit({
        shinyjs::hide(id="loading-content", anim=TRUE, animType="fade")    
      })
      
      for_report <- data.frame(a=c(1,2,b=c(3,4)))#(gVars$potential_new_entries_report
      discard_file <- data.frame(for_report) #%>% .[.$Decision %in% c("Discard"),]
      
      write.xlsx(discard_file, file, row.names=FALSE)
      print("Discard list stored!")
      data$whichBAR <- "upd_voc_discard"
    }
  )
  
  
  
  
  
  
  
  #accepted
  output$download_to_review_accepted <- downloadHandler(
    #arrange new file name
    current_df_file_name <- gVars$current_df_file_name %>% 
      sub('.[^.]*$', '', .) %>%
      paste(., collapse = "_"),
    
    filename = function() {
      file = paste0("accepted_cand_for_voc_",current_df_file_name, "_v",
                    format(Sys.time(), "%Y-%m-%d_%H-%M-%S"),"_",gVars$sha256_stringID,
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
  
  output$download_to_review_accepted2 <- downloadHandler(  
    #arrange new file name
    current_df_file_name <- gVars$current_df_file_name %>% 
      sub('.[^.]*$', '', .) %>%
      paste(., collapse = "_"),
    
    filename = function() {
      file = paste0("accepted_cand_for_voc_",current_df_file_name, "_v",
                    format(Sys.time(), "%Y-%m-%d_%H-%M-%S"),"_",gVars$sha256_stringID,
                    ".xlsx", 
                    sep = "")
    },
    
    content = function(file) {
         req(!is.null(gVars$potential_new_entries_report))
      
      shinyjs::html(id="loadingText", "Selecting Accepted Vocabulary entries")
      shinyjs::show(id="loading-content")
      on.exit({
        shinyjs::hide(id="loading-content", anim=TRUE, animType="fade")    
      })
      
      for_report <- gVars$potential_new_entries_report
      accepted_file <- data.frame(for_report) %>% .[.$Decision %in% c("Accepted", "Newly Accepted"),]
      
      write.xlsx(accepted_file, file, row.names=FALSE)
      print("Accepted list stored!")
      data$whichBAR <- "upd_voc_accepted"
    }
  )
  
  
  
  #updated vocabulary
  output$download_Upd_Vocabulary <- downloadHandler(
        filename = function() {
          file = paste0("updated_vocabulary_v",
                        format(Sys.time(), "%Y-%m-%d_%H-%M-%S"),"_",gVars$sha256_stringID,
                        ".xlsx", 
                        sep = "")
        },
        content = function(file) {
          req(!is.null(gVars$dkeys))
          upd_voc <- data.frame(gVars$dkeys) %>% 
            group_by(label, lab_syn,allowed_features) %>%
            summarise(syn_features = str_c(syn_features, collapse="|"))
          
          shinyjs::html(id="loadingText", "Updating Vocabulary")
          shinyjs::show(id="loading-content")
          on.exit({
            shinyjs::hide(id="loading-content", anim=TRUE, animType="fade")    
          })
          upd_voc <- data.frame(upd_voc)

          write.xlsx(upd_voc,file, row.names=FALSE)
          #flag to trigger the report
          data$whichBAR <- "updVoc" 
          
        }
      )
  
  
  #phenodata download
  observeEvent(input$update_df_button,{ 
    output$upd_phenodf <- renderDT(
      gVars$phTable[[1]] , options = list(scrollX = TRUE))
  })
  
  
  output$download_Upd_Phdata <- downloadHandler(
    #arrange new file name
    current_df_file_name <- gVars$current_df_file_name %>% 
      sub('.[^.]*$', '', .) %>%
      paste(., collapse = "_"),
    
    filename = function() {
      filename = paste("updated_",current_df_file_name,"v",
                       format(Sys.time(), "%Y-%m-%d_%H-%M-%S"),"_",gVars$sha256_stringID,
                       ".xlsx", 
                       sep = "")
    },
    content = function(file) {
      if(is.null(gVars$phTable) ){
        print('no input')
        
        return(NULL)
      }
      
      shinyjs::html(id="loadingText", "Updating dataset")
      shinyjs::show(id="loading-content")
      on.exit({
        print("inside on exit")
        shinyjs::hide(id="loading-content", anim=TRUE, animType="fade")  
        
      })
      
      print(is.null(gVars$phTable) )
      
      
      upd_df<-  gVars$phTable[[1]]
      row.names(upd_df) <-NULL
      
      write.xlsx(upd_df,file, row.names=FALSE) 
    } 
    
  )
  
  
  
  
  #GLP download 
  output$download_Upd_GLP <- shiny::downloadHandler(
    #arrange new file name
    if(isFALSE(gVars$working_status)) {  #multiple mode)
      current_df_file_name <- "MultiFile_"
    } else {   #single mode
      current_df_file_name <- gVars$current_df_file_name %>% 
        sub('.[^.]*$', '', .) %>%
        paste(., collapse = "_")
    },
    
    
    filename = function(){
      paste("Track_PT_GLP_", current_df_file_name,"_", Sys.Date(),"_",gVars$sha256_stringID, '.html', sep='')
    },
    
    content = function(con){
      #start loading screen
      shinyjs::html(id="loadingText", "CREATING PROCEDURAL/GLP REPORT")
      shinyjs::show(id="loading-content")
      
      on.exit({
        shinyjs::hide(id="loading-content", anim=TRUE, animType="fade")
      })
      #Disable Warning
      oldw <- getOption("warn")
      options(warn = -1)
      tempReport <- file.path((tempdir()), "report_PT_GLP.Rmd")
      file.copy("ESPERANTO_app/report/report_PT_GLP.Rmd", tempReport, overwrite=TRUE)
      
      
      params <- list(gVars=gVars, input=input, srcDir=srcDir)
      rmarkdown::render(tempReport, output_file=con,
                        params=params,
                        envir=new.env(parent=globalenv())
      )
      #Enable Warning
      options(warn = oldw)
    }
  )
  
  #block of reports
  output$download_Upd_GLPv2 <- shiny::downloadHandler(
    #arrange new file name
    current_df_file_name <- gVars$current_df_file_name %>% 
      sub('.[^.]*$', '', .) %>%
      paste(., collapse = "_"),
    
    filename = function(){
      paste("Track_PT_GLP_", current_df_file_name, "_", Sys.Date(),"_",gVars$sha256_stringID, '.html', sep='')
    },
    
    content = function(con){
      #start loading screen
      shinyjs::html(id="loadingText", "CREATING PROCEDURAL/GLP REPORT")
      shinyjs::show(id="loading-content")
      
      on.exit({
        shinyjs::hide(id="loading-content", anim=TRUE, animType="fade")
      })
      #Disable Warning
      oldw <- getOption("warn")
      options(warn = -1)
      tempReport <- file.path((tempdir()), "report_PT_GLP.Rmd")
      file.copy("ESPERANTO_app/report/report_PT_GLP.Rmd", tempReport, overwrite=TRUE)
      
      
      params <- list(gVars=gVars, input=input, srcDir=srcDir)
      rmarkdown::render(tempReport, output_file=con,
                        params=params,
                        envir=new.env(parent=globalenv())
      )
      data$go_stopPT_GLP <-TRUE
      data$finaldownloadtrigger <-FALSE
      data$whichBAR <- "updPT_GLP"  
      #Enable Warning
      options(warn = oldw)
    }
  )
  
  
  
  
  
  
  
  
  
  
  #multiple integration section
  #issue
  output$download_multi_issue <- downloadHandler(
    filename = function() {     
      file = paste0("issue_multiple_integration_v",
                    format(Sys.time(), "%Y-%m-%d_%H-%M-%S"),"_",gVars$sha256_stringID,
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
      
      tempReport <- file.path((tempdir()),"report_multi_issue.Rmd") 
      file.copy("ESPERANTO_app/report/report_multi_issue.Rmd", tempReport, overwrite = TRUE)
      
      # Set up parameters to pass to Rmd document
      params <- list(gVars=gVars, input=input, srcDir=srcDir)
      
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
      print("Issue entries report for multiple integration stored!")
      
    }
  )
  
  
  output$download_multi_issueV2 <- downloadHandler(
    filename = function() {    
      
      file = paste0("issue_multiple_integration_v",
                    format(Sys.time(), "%Y-%m-%d_%H-%M-%S"),"_",gVars$sha256_stringID,
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
      
      tempReport <- file.path((tempdir()), "report_multi_issue.Rmd") 
      file.copy("ESPERANTO_app/report/report_multi_issue.Rmd", tempReport, overwrite = TRUE)
      
      # Set up parameters to pass to Rmd document
      params <- list(gVars=gVars, input=input, srcDir=srcDir)
      
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
      print("Issue entries report for multiple integration stored!")
      data$whichBAR <- "upd_multi_issue"
    }
  )
  
  
  #consistent
  output$download_multi_consistent <- downloadHandler(
    filename = function() {     
      
    
      file = paste0("consistent_multiple_integration","_v",
                    format(Sys.time(), "%Y-%m-%d_%H-%M-%S"),"_", gVars$sha256_stringID,
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
      
      tempReport <- file.path((tempdir()), "report_multi_consistent.Rmd") 
      file.copy("ESPERANTO_app/report/report_multi_consistent.Rmd", tempReport, overwrite = TRUE)
      
      # Set up parameters to pass to Rmd document
      params <- list(gVars=gVars, input=input, srcDir=srcDir)
      
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
      
      print("consistent entries report for multiple integration stored!")
      
    }
  )
  
  output$download_multi_consistentV2 <- downloadHandler(
    filename = function() {     
      
      current_df_file_name <- gVars$current_df_file_name %>% 
        sub('.[^.]*$', '', .) %>%
        paste(., collapse = "_")
      
      file = paste0("consistent_multiple_integration","_v",
                    format(Sys.time(), "%Y-%m-%d_%H-%M-%S"),"_", gVars$sha256_stringID,
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
      
      tempReport <- file.path((tempdir()), "report_multi_consistent.Rmd") 
      file.copy("ESPERANTO_app/report/report_multi_consistent.Rmd", tempReport, overwrite = TRUE)
      
      # Set up parameters to pass to Rmd document
      params <- list(gVars=gVars, input=input, srcDir=srcDir)
      
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
      
      print("consistent entries report for multiple integration stored!")
      data$whichBAR <- "upd_multi_consistent"
    }
  )
  
  
  
  
  #PLOTTING TOOL
  # populates the choices of abscissa box
  observeEvent(input$checkboxSINGLE_MULTIphdata, {
    req(!is.null(input$checkboxSINGLE_MULTIphdata))
    if (input$checkboxSINGLE_MULTIphdata=="single"){   
      df <- gVars$phTable[[1]]
    } else{
      df <-gVars$multidf
    }
    gVars$plot_tmp <- colnames(df) 
    updateSelectizeInput(session,
                         inputId = "multi_colnamesX",
                         choices = colnames(df),
                         selected = 1,
                         server=TRUE)
    
  })
  
  observe({
    x_class_to_show <-ifelse(!is.null(input$multi_colnamesX) && (input$multi_colnamesX)!="", paste("Data format: ", class(gVars$phTable[[1]][[input$multi_colnamesX]]) ), "Data format: -") 
    output$class_x <- renderText(HTML("<font size=-1.5>","<em>",x_class_to_show,"</em>","</font>"))
  })
  
  # populates the choices of ordinate box
  observeEvent(input$multi_colnamesX, {
    req(!is.null(input$checkboxSINGLE_MULTIphdata))
    to_chose <- gVars$plot_tmp
    updateSelectizeInput(session,
                         inputId = "multi_colnamesY",
                         choices = c("",to_chose[!to_chose %in% input$multi_colnamesX]), 
                         selected = 1,
                         server=TRUE)
  })
  
  
  observe({
    y_class_to_show <-ifelse(!is.null(input$multi_colnamesY) && (input$multi_colnamesY)!="", paste("Data format: ", class(gVars$phTable[[1]][[input$multi_colnamesY]]) ), "Data format: -") 
    output$class_y <- renderText(HTML("<font size=-1.5>","<em>",y_class_to_show,"</em>","</font>"))
  })
  
  
  # populates the choices of condition box
  observeEvent(list(input$multi_colnamesX,input$multi_colnamesY), {
    req(input$multi_colnamesX != "", input$multi_colnamesY != "")
    to_chose <- gVars$plot_tmp
    updateSelectizeInput(session,
                         inputId = "multi_colnamesCOND",
                         choices = to_chose[!to_chose %in% c(input$multi_colnamesX ,input$multi_colnamesY)],
                         selected = 1,
                         server=TRUE)
  })
  
  observe({
    cond_class_to_show <-ifelse(!is.null(input$multi_colnamesCOND) && (input$multi_colnamesCOND)!="", paste("Data format: ", class(gVars$phTable[[1]][[input$multi_colnamesCOND]]) ), "Data format: -") 
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
            HTML("<font size=-5>", "&nbsp;","&nbsp;","&nbsp;","&nbsp;","&nbsp;" ,"Current format: ", class(gVars$phTable[[1]][[input$multi_colnamesX]]),"</font>"),
            div(style = "margin-top: 12px"),
            
            tags$label(em("Y-axis")),
            div(style = "margin-top: -10px" ),
            checkboxInput(inputId = "y_conversion", label = input$multi_colnamesY, value=FALSE ),
            div(style = "margin-top: -20px"),
            HTML("<font size=-5>", "&nbsp;","&nbsp;","&nbsp;","&nbsp;","&nbsp;" ,"Current format: ", class(gVars$phTable[[1]][[input$multi_colnamesY]]),"</font>"),
            div(style = "margin-top: 12px"),
            
            tags$label(em("Condition-axis")),
            div(style = "margin-top: -10px"),
            checkboxInput(inputId = "cond_conversion", label = input$multi_colnamesCOND, value=FALSE ),
            div(style = "margin-top: -20px"),
            HTML("<font size=-5>", "&nbsp;","&nbsp;", "&nbsp;","&nbsp;","&nbsp;" ,"Current format: ", class(gVars$phTable[[1]][[input$multi_colnamesCOND]]),"</font>"),
            div(style = "margin-top: 12px"),
            
            br(),
            radioButtons(inputId = "conversion_options", label = "Convert to:", choices=list("Numeric","Character","Factor"), selected=character(0) ,inline=TRUE),
            
            br(),
            shinyBS::bsButton("convert_button", label="Convert", style="info")
          ),    
          
          box(fluidPage(
            shinyBS::bsButton("donotshowcol", label="Show listed columns", type = "toggle", value = TRUE, style="danger", icon=icon("exclamation-circle")),
            fluidRow(br()), 
            fluidRow(DT::DTOutput("converting_cols"))
          ))
        ) 
        
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
      shinyBS::updateButton(session, "donotshowcol", label= " Showing columns",  style="success") 
      
    }
    else {
      shinyBS::updateButton(session, "GLP_inactivation_mode", label=" GLP Mode Disabled",  style="danger", icon=icon("exclamation-circle"))
      shinyBS::updateCollapse(session, "bsSidebar1", close="STRUCTURE HOMOGENIZATION", style=list("GLP MODE"="warning","STRUCTURE HOMOGENIZATION"="warning"))
    }
    
  })
  
  
  
  observe({
    req(input$multi_colnamesX != "", input$multi_colnamesY != "",!is_null(input$type_plot_relation), !is_null(input$checkboxSINGLE_MULTIphdata), !is_null(input$checkboxBAR_TILEplot))
    if(req(!is.null(input$multi_colnamesY), !is.null(input$multi_colnamesY),!is_empty(input$type_plot_relation) )){
      shinyBS::updateButton(session, "plot_multipic", disabled=FALSE)
    } else {shinyBS::updateButton(session, "plot_multipic", disabled=TRUE)}
  })
  
  #reset selections for plotting
  observeEvent(input$reset_plot_param, {
    updateRadioButtons(session,  "checkboxSINGLE_MULTIphdata", selected = character(0) )
    updateRadioButtons(session,  "checkboxBAR_TILEplot", selected = character(0) )
    output$multiplot   <- NULL
    updateSelectizeInput(session,
                         inputId="multi_colnamesX",
                         choices=NULL,
                         selected=character(0),
                         options = list(
                           placeholder = 'Please select an option below',
                           onInitialize = I('function() { this.setValue(""); }')), server=TRUE    
    )
    updateSelectizeInput(session,
                         inputId="multi_colnamesY",
                         choices=NULL,
                         selected=character(0), 
                         options = list(
                           placeholder = 'Please select an option below',
                           onInitialize = I('function() { this.setValue(""); }')), server=TRUE    
    )
    updateSelectizeInput(session,
                         inputId="multi_colnamesCOND",
                         choices="",
                         selected=character(0),
                         options = list(
                           placeholder = 'Please select an option below',
                           onInitialize = I('function() { this.setValue(""); }')), server=TRUE    
    )
    updateRadioButtons(session,  "type_plot_relation", selected = character(0) )
    
  })
  
  
  observeEvent(input$plot_multipic,{
    output$multiplot <- plotly::renderPlotly({ 
      req(input$multi_colnamesX != "", input$multi_colnamesY != "",!is_null(input$type_plot_relation), !is_null(input$checkboxSINGLE_MULTIphdata), !is_null(input$checkboxBAR_TILEplot))
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
          theme(legend.position = leg_pos,  
                axis.text.x = element_text(angle = 60, hjust = 1))+ scale_x_discrete(labels = function(x) label_shortener(x, 22))  
      } else {    #tileplot
        pic<- ggplot(df, aes(fill=fill_var, y=y_var, x=x_var)) + 
          geom_tile()+
          xlab(xlab)+ ylab(ylab)+ 
          labs(fill=legend_lab)+  
          theme(legend.position = leg_pos, 
                axis.text.x = element_text(angle = 60, hjust = 1))+ scale_x_discrete(labels = function(x) label_shortener(x, 22)) 
      }
      
      ggplotly(pic) 
    })
  })
  
  
  #it updates the label and legend labelling
  observe({
    req(!is.null(input$checkboxSINGLE_MULTIphdata))
    updateTextInput(session, "x_axis_label",  value = input$multi_colnamesX)
    updateTextInput(session, "y_axis_label",  value = input$multi_colnamesY)
    updateTextInput(session, "cond_legend_label",  value = input$multi_colnamesCOND)
  })
  
  
  #if "no condition" is selected in "type of barplot", the boxes for the choice of condition and the one for labelling the legend are disabled
  observe({
    if (req(input$type_plot_relation) == "nocond") { 
      shinyjs::disable("cond_legend_label")
      
    } else{
      shinyjs::enable("cond_legend_label")
    }
    
  })
  
  
  
  #if condition box to plot is empty, it disables the option in radiobutton different from "no condition"
  observe({
    if (is.null(input$multi_colnamesCOND)){#{  disable("multi_colnamesCOND")}
      shinyjs::enable(selector = "[type=radio][value =nocond ]")
      shinyjs::runjs("$('[type=radio][value =nocond]').parent().addClass('enabled').css('opacity', 1)")
      
      shinyjs::disable(selector = "[type=radio][value =stacked ]")
      shinyjs::runjs("$('[type=radio][value =stacked]').parent().addClass('disabled').css('opacity', 0.4)")
      shinyjs::disable(selector = "[type=radio][value =dodge ]")
      shinyjs::runjs("$('[type=radio][value =dodge]').parent().addClass('disabled').css('opacity', 0.4)")
    }
    req(!is.null(input$multi_colnamesCOND))
    if(input$multi_colnamesCOND =="" ){
      shinyjs::disable(selector = "[type=radio][value =stacked ]")
      shinyjs::runjs("$('[type=radio][value =stacked]').parent().addClass('disabled').css('opacity', 0.4)")
      shinyjs::disable(selector = "[type=radio][value =dodge ]")
      shinyjs::runjs("$('[type=radio][value =dodge]').parent().addClass('disabled').css('opacity', 0.4)")
    } else {
      shinyjs::enable(selector = "[type=radio][value =stacked ]")
      shinyjs::runjs("$('[type=radio][value =stacked]').parent().addClass('enabled').css('opacity', 1)")
      shinyjs::enable(selector = "[type=radio][value =dodge ]")
      shinyjs::runjs("$('[type=radio][value =dodge]').parent().addClass('enabled').css('opacity', 1)")
      
    } 
    
    
  })
  
  
  observe({
    req(!is.null(input$session_type), !is.null(gVars$working_status))
    if(gVars$working_status == TRUE ){ # single 
      #keep "single" of "type of phenodata" enabled and it disables "multiple" radiobutton option in plot section
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
  
  
  
  
  
  
  
  
  #multiintegration tool
  observeEvent(input$upload_multi_pheno_submit, {
    multi_fPhenoFile <- input$multi_fPheno
    gVars$current_df_file_name <- input$multi_fPheno$name
    if (is.null(multi_fPhenoFile))
      return(NULL)
    
    #import always - contains typical colnames in metadata which are not of interest in curation
    always <- scan("./ESPERANTO_app/files/always.txt", character(), quote = "", skip=1)
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
    
    shinyBS::updateButton(session, "upload_multi_pheno_submit",  style="success")
    
  })
  
  
  observeEvent(input$go_to_multi_tool_integ_button,{
    req(length(gVars$multidf_list)!=0)
    shinyjs::disable("import_pheno_submit")
    gVars$multidf <- as_tibble(do.call(plyr::rbind.fill,gVars$multidf_list)) %>% dplyr::select(mainfile_name, everything())
    
    gVars$sha256_stringID <- digest::digest(gVars$current_df_file_name, "sha256")
    text_sha256 <- paste0(tags$b("SESSION ID: "), gVars$sha256_stringID)
    
    listed_files <- gVars$current_df_file_name %>%  paste0(., collapse = "<br/> &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp;") 
    gVars$pt_glp_list <- c(text_sha256, gVars$pt_glp_list, paste0("INPUT FILES - ","<br/> &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp;", listed_files))
    
    #updates tabpanel and sidebar
    updateTabsetPanel(session, "display", selected="multiple_integration_tool_page")
    shinyBS::updateButton(session, "upload_multi_pheno_submit" ,style="info")
    
    shinyBS::toggleModal(session, "importPhenoModal", toggle="close")
    shinyBS::updateButton(session, "import_pheno_submit", style="success")
    shinyBS::updateCollapse(session, "bsSidebar1", open="LOAD VOCABULARY", style=list("LOAD PHENODATA"="success","LOAD VOCABULARY"="warning"))
  })
  
  
  output$multidf_table <- DT::renderDataTable({
    req(!is.null(gVars$working_status)) 
    req(!is.null(gVars$multidf) )
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
      apply(.,1, function(k) lapply(as.list(pre_safe_check[[k]]), function(j){ 
        all(na.omit(unique(j)) %in%  unique(dict$allowed_features[dict$label==k]))})) %>%
      sapply(., function(z) all(z==TRUE))
    
    
    safe_check <- pre_safe_check[,(real_safe_colnames_index)]   
    
    fast_check <- multidf %>% .[ , !names(.) %in%  colnames(pre_safe_check)] %>%
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
    paste(input$multijoin_outcomes, input$glp_pt_button_multi,  input$undo_multi_button) 
  })
  
  
  observeEvent(trig_column_table(),{
    req(nzchar(input$multijoin_outcomes))
    if (input$multijoin_outcomes == "Slow check"){
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
    req(!is_empty(input$columns_multi_selected_group_cells_selected) && nzchar(input$multijoin_outcomes))
    
    if (input$multijoin_outcomes == "Slow check"){
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
    gVars$col_issue_cases <- dt_multi_toshow %>% cbind(mainfile_name=gVars$multidf$mainfile_name,.) # %>% .[.[[3]] == FALSE,1:2] %>% unique(.) #unique(.) #
    if(all(gVars$col_issue_cases[,3])) {gVars$col_issue_cases <- unique(gVars$col_issue_cases)
    } else { gVars$col_issue_cases <- gVars$col_issue_cases  %>% .[.[[3]] == FALSE,1:2] %>% unique(.)}
    
    
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
  
  
  observe({
    req(!is.null(input$columns_multi_selected_group_cells_selected) )
    nrows<- dim(input$columns_multi_selected_group_cells_selected)[1]
    if(nrows == 0){
      shinyBS::updateButton(session, "undo_multi_button", disabled = FALSE)
      output$dt_toshow_multi_selected_group <-  DT::renderDataTable(NULL)
      if(!is.null(input$dt_toshow_multi_selected_group_columns_selected) ){
        shinyBS::updateButton(session, "undo_multi_button", disabled = FALSE)
      }
      #ifs for download reports
      if(!is.null(gVars$multi_ISSmsg)){shinyjs::enable("download_multi_issue")}
      if(!is.null(gVars$multi_ACmsg)){shinyjs::enable("download_multi_consistent")}
    } else { 
      shinyBS::updateButton(session, "undo_multi_button", disabled = TRUE)
      #ifs for download reports
      shinyjs::disable("download_multi_issue")
      shinyjs::disable("download_multi_consistent")
    }
  })
  
  observe({
    req(!is.null(input$columns_multi_selected_group_cells_selected) )
    nrows<- dim(input$columns_multi_selected_group_cells_selected)[1]
    if(nrows == 0){
      shinyBS::updateButton(session, "undo_multi_button", disabled = FALSE)
      output$dt_toshow_multi_selected_group <-  DT::renderDataTable(NULL)
      if(!is.null(input$dt_toshow_multi_selected_group_columns_selected) ){
        shinyBS::updateButton(session, "undo_multi_button", disabled = FALSE)
      }
      #ifs for download reports
      if(!is.null(gVars$multi_ISSmsg)){shinyjs::enable("download_multi_issue")}
      if(!is.null(gVars$multi_ACmsg)){shinyjs::enable("download_multi_consistent")}
    } else { 
      shinyBS::updateButton(session, "undo_multi_button", disabled = TRUE)
      #ifs for download reports
      shinyjs::disable("download_multi_issue")
      shinyjs::disable("download_multi_consistent")
    }
  })
  
  observe({
    req(!is.null(input$multijoin_outcomes))
    if(input$multijoin_outcomes == "" ){
      output$columns_multi_selected_group <-  DT::renderDataTable(NULL)  
    }
  })
  
  
  observe({
    req(!is.null(input$columns_multi_selected_group_cells_selected) )
    nrows<- dim(input$columns_multi_selected_group_cells_selected)[1]
    if(nrows == 0){
      shinyBS::updateButton(session, "undo_multi_button", disabled = FALSE)
      output$dt_toshow_multi_selected_group <-  DT::renderDataTable(NULL)
      if(!is.null(input$dt_toshow_multi_selected_group_columns_selected) ){
        shinyBS::updateButton(session, "undo_multi_button", disabled = FALSE)
      }
      #ifs for download reports
      if(length(gVars$multi_ISSmsg)!=0){shinyjs::enable("download_multi_issue")}
      if(length(gVars$multi_ACmsg) !=0){shinyjs::enable("download_multi_consistent")}
    } else { 
      shinyBS::updateButton(session, "undo_multi_button", disabled = TRUE)
      #ifs for download reports
      if(length(gVars$multi_ISSmsg)==0){shinyjs::disable("download_multi_issue")}
      if(length(gVars$multi_ACmsg)==0){shinyjs::disable("download_multi_consistent")}
    }
  })
  
  
  observe({
    req(is.null(input$columns_multi_selected_group_cells_selected) )
    if(length(gVars$multi_ISSmsg)!=0){
      shinyjs::enable("download_multi_issue")
    } else {
      shinyjs::disable("download_multi_issue")
    }
    if(length(gVars$multi_ACmsg)!=0){
      shinyjs::enable("download_multi_consistent")
    } else {
      shinyjs::disable("download_multi_consistent")
    }
  })  
  
  observe({
    if(!is.null(input$dt_toshow_multi_selected_group_columns_selected) ){
      shinyBS::updateButton(session, "issue_multi_button", disabled = FALSE)
      shinyBS::updateButton(session, "accept_multi_button", disabled = FALSE) 
      shinyBS::updateButton(session, "undo_multi_button", disabled = TRUE)
    } else{
      shinyBS::updateButton(session, "issue_multi_button", disabled = TRUE)
      shinyBS::updateButton(session, "accept_multi_button", disabled = TRUE) 
      shinyBS::updateButton(session, "undo_multi_button", disabled = FALSE)
      
    }
    
  })
  
  
  
  
  #cleans the datatable as the columns are checked(both if accepted, or sent to issue)
  observeEvent(input$glp_pt_button_multi,{
    shinyBS::updateButton(session, "issue_multi_button", disabled = TRUE)
    shinyBS::updateButton(session, "accept_multi_button", disabled = TRUE) 
    shinyBS::updateButton(session, "undo_multi_button", disabled = FALSE) 
    req(input$multijoin_outcomes, input$dt_toshow_multi_selected_group_columns_selected)
    req(!is.null(gVars$dkeys))
    req(input$columns_multi_selected_group_cell_clicked$value)
    
    if (input$multijoin_outcomes == "Slow check"){
      tmptable<- gVars$multi_slow_check()
      #stores for undo
      column <- data.frame(tmptable[,input$columns_multi_selected_group_cell_clicked$value] ) %>% setNames(.,input$columns_multi_selected_group_cell_clicked$value)
      undo_element <- list(column, input$multijoin_outcomes, gVars$multi_flag)
      gVars$last5list <- c(gVars$last5list, (list(multi = undo_element)))
      tmptable <- (tmptable[,!colnames(tmptable) %in% input$columns_multi_selected_group_cell_clicked$value, drop=FALSE ])
      tmp_col <- ncol(tmptable)
      gVars$multi_slow_check(tmptable)
    } else if (input$multijoin_outcomes == "Fast check"){
      tmptable<- gVars$multi_fast_check() 
      #stores for undo
      column <- data.frame(tmptable[,input$columns_multi_selected_group_cell_clicked$value] ) %>% setNames(.,input$columns_multi_selected_group_cell_clicked$value)
      undo_element <- list(column, input$multijoin_outcomes, gVars$multi_flag)
      gVars$last5list <- c(gVars$last5list, (list(multi = undo_element)))
      tmptable <- tmptable[,!colnames(tmptable) %in% input$columns_multi_selected_group_cell_clicked$value, drop=FALSE ]
      tmp_col<- ncol(tmptable)
      gVars$multi_fast_check(tmptable)
    } else { # safe items
      tmptable<-gVars$multi_safe_check()  
      #stores for undo
      column <- data.frame(tmptable[,input$columns_multi_selected_group_cell_clicked$value] ) %>% setNames(.,input$columns_multi_selected_group_cell_clicked$value)
      undo_element <- list(column, input$multijoin_outcomes, gVars$multi_flag)
      gVars$last5list <- c(gVars$last5list, (list(multi = undo_element)))
      tmptable<- tmptable[, !(colnames(tmptable) %in% input$columns_multi_selected_group_cell_clicked$value), drop=FALSE ]
      tmp_col<- ncol(tmptable)
      gVars$multi_safe_check(tmptable) 
    }
    
    #cleans the selectizeinput from safe/to chekc/Slow check once they are empty (=all entries were processed)
    if (tmp_col == 0){
      gVars$grouping_multi_tables <- gVars$grouping_multi_tables[!gVars$grouping_multi_tables %in% input$multijoin_outcomes]
      
      if (length(gVars$grouping_multi_tables) != 0){
        updateSelectizeInput(session,
                             inputId="multijoin_outcomes",
                             label="Potential outcome:",
                             choices=gVars$grouping_multi_tables,#c("Safe","Fast check","Slow check"),
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
      }
    }
  })
  
  
  output$pt_text_MULTI <- renderText({"<b>Procedural Track</b>"})
  output$pt_MULTI <- renderText({"<i><small>Waiting for action!</small></i>"})
  
  #stores whether the entry was accepted in the final report file.
  observeEvent(input$accept_multi_button,{
    shinyBS::updateButton(session, "glp_pt_button_multi", disabled = FALSE)
    shinyBS::updateButton(session, "issue_multi_button", disabled = TRUE)
    shinyBS::updateButton(session, "accept_multi_button", style="success", disabled = TRUE)
    shinyBS::updateButton(session, "undo_multi_button", disabled = TRUE)
    
    req(input$columns_multi_selected_group_cell_clicked$value)
    gVars$multi_accept_colnames <- c(gVars$multi_accept_colnames, input$columns_multi_selected_group_cell_clicked$value)
    tmp_msg <- paste(tags$b(tags$span(style="color:green", input$columns_multi_selected_group_cell_clicked$value)), "is accepted. (CONSISTENT)" )
    gVars$multi_ACmsg <- append (gVars$multi_ACmsg, list(c(tmp_msg,gVars$username)))
    output$pt_MULTI <- renderText({tmp_msg})
    
    #in pt track report
    gVars$pt_glp_list <- c(gVars$pt_glp_list, tmp_msg)
    gVars$multi_flag <- "accept"
  })
  
  
  #stores whether the entry was added to issue report.
  observeEvent(input$issue_multi_button,{
    shinyBS::updateButton(session, "glp_pt_button_multi", disabled = FALSE)
    shinyBS::updateButton(session, "accept_multi_button", disabled = TRUE)
    shinyBS::updateButton(session, "issue_multi_button", style="success", disabled = TRUE)
    shinyBS::updateButton(session, "undo_multi_button", disabled = TRUE)
    
    req(input$columns_multi_selected_group_cell_clicked$value)
    gVars$multi_issue_colnames <- c(gVars$multi_issue_colnames, input$columns_multi_selected_group_cell_clicked$value)
    tmp_msg <- paste(tags$b(tags$span(style="color:red", input$columns_multi_selected_group_cell_clicked$value)), "needs further modification. (ISSUE)")
    
    #identify entries which need a further round of modification to store in issue report message
    unique_mainfiles <- unique(gVars$col_issue_cases$mainfile_name)
    
    
    issue_entries <- sapply(unique_mainfiles, function(file) { 
      entries <- pull(gVars$col_issue_cases[gVars$col_issue_cases$mainfile_name==file,],input$columns_multi_selected_group_cell_clicked$value) 
      retrieved_values <-data.frame(filename=file, col=0,row=0, shortmessage="NA", message="NA")   
    #  if(!all(is.na(entries))) { 
        
        entries_collapsed <- entries %>% paste(., collapse="|")
        y<- paste(  "File Name: ",tags$b(file),"<br/>")
        retrieved_values[retrieved_values$filename==file]$message <-paste0(y, "&#x09;",entries_collapsed,"<br/>")
        retrieved_values[retrieved_values$filename==file]$shortmessage <- y 
        retrieved_values[retrieved_values$filename==file]$col <-  retrieved_values[retrieved_values$filename==file]$col+1
        retrieved_values[retrieved_values$filename==file]$row <-  length(entries)
   #   }
      
      
      
      retrieved_values
    })
    
    
    gVars$issue_tracking_ID <- ifelse(is.null(gVars$issue_tracking_ID), 1, gVars$issue_tracking_ID + 1)
    issue_block <- t(issue_entries) %>% data.frame() %>% `row.names<-`(NULL) #%>% .[!is.na(.$message),]
    issue_block$reference_col <- input$columns_multi_selected_group_cell_clicked$value
    gVars$save_issue_blocks_list <- append(gVars$save_issue_blocks_list, list(issue_block)) 
    
    full_tmp_msgpt <- paste0(tags$b(tags$em("issue report tracking_ID:")),tags$b(gVars$issue_tracking_ID), "<br/> &nbsp; &nbsp; &nbsp;", tmp_msg, "<pre>", toString(issue_block$shortmessage))
    full_tmp_msgpt <- full_tmp_msgpt %>%  
      gsub(pattern = "<br/>, ", replacement = "<br/>", .) %>% 
      gsub(pattern = "\\|", replacement = "<br/> &#x09;", .)  
    
    if (ncol(gVars$col_issue_cases)>2 && all(gVars$col_issue_cases[[3]])){
      full_tmp_msg <- paste0(tags$b(tags$em("issue report tracking_ID:")),tags$b(gVars$issue_tracking_ID),  
                             "<br/> &nbsp; &nbsp; &nbsp;", tmp_msg,
                             "<br/> &nbsp; &nbsp; &nbsp;", tags$b("Attention! "), "All entries were already in the vocabulary.",  
                             "<pre>",toString(issue_block$shortmessage),"</pre>","<br/>"  )
    } else {
      full_tmp_msg <- paste0(tags$b(tags$em("issue report tracking_ID:")),tags$b(gVars$issue_tracking_ID), 
                             "<br/> &nbsp; &nbsp; &nbsp;", tmp_msg,  "<pre>",toString(issue_block$message), "</pre>","<br/>" )
    }
    full_tmp_msg <- full_tmp_msg %>%  
      gsub(pattern = "<br/>, ", replacement = "<br/>", .) %>% 
      gsub(pattern = "\\|", replacement = "<br/> &#x09;", .)  
    
    gVars$multi_ISSmsg <- append (gVars$multi_ISSmsg, list(c(full_tmp_msg,gVars$username)))
    
    output$pt_MULTI <- renderText({tmp_msg})
    
    gVars$multi_flag <- "issue" 
    #in pt track report
    gVars$pt_glp_list <- c(gVars$pt_glp_list, full_tmp_msgpt)
  })
  
  
  observeEvent(input$glp_pt_button_multi,{
    shinyBS::updateButton(session, "glp_pt_button_multi", disabled = TRUE)
    shinyBS::updateButton(session, "issue_multi_button", style="info", disabled = TRUE)
    shinyBS::updateButton(session, "accept_multi_button", style="info", disabled = TRUE)
    shinyBS::updateButton(session, "undo_multi_button", disabled = FALSE)
    
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
      shinyBS::bsButton("glp_store_commentMULTI", label="Add", style="info"),
      if (failed){
        div(br(),tags$b("In GLP mode, you must justify your decisions. Please do."), style = "color: red;")},   
      footer =  NULL 
    )
  }
  
  # When Add button(glp_store_comment) is pressed, it tests if the textareainput box is not empty. 
  # If successful, remove the modal, otherwise it shows another modal with a failure message.
  observeEvent(input$glp_store_commentMULTI, {
    # Check that box for glp comments is filled
    if (input$glp_comments_box != "") {
      tabulated_comment <- paste0("\t","COMMENT: ",input$glp_comments_box)
      list_element <- list(c(tabulated_comment,""))
      if (gVars$multi_flag=="accept"){
        gVars$multi_ACmsg <- c(gVars$multi_ACmsg, list_element)
      }
      else{
        gVars$multi_ISSmsg <- c(gVars$multi_ISSmsg, list_element)
      }
      
      #adds the comment to the complete procedural track
      gVars$pt_glp_list <- c(gVars$pt_glp_list, tabulated_comment)
      
      removeModal() 
    } else {
      showModal(dataModal_MULTI_comments(failed = TRUE))
    }
    
  }) 
  
  
  
  observeEvent(input$upload_pheno_submit, {
    shiny::validate(
      need(!is.null(gVars$inputPh()), "No Phenodata File Provided!")
    )
    
    phTable <- gVars$inputPh()
    gVars$sha256_stringID <- digest::digest(gVars$current_df_file_name, "sha256")
    text_sha256 <- paste0(tags$b("SESSION ID: "), gVars$sha256_stringID)
    gVars$pt_glp_list <- c(text_sha256, gVars$pt_glp_list, paste0("INPUT FILE - ", gVars$current_df_file_name)) 
    gVars$phTable <- phTable
    shinyBS::toggleModal(session, "importPhenoModal", toggle="close")
    shinyBS::updateButton(session, "import_pheno_submit", style="success", icon=icon("check-circle"))
    shinyBS::updateCollapse(session, "bsSidebar1", open="LOAD VOCABULARY", style=list("LOAD PHENODATA"="success","LOAD VOCABULARY"="warning"))
  })
  
  
  output$filtered <- DT::renderDataTable({
    shiny::validate(
      need(!is.null(gVars$original_phTable), "No Phenodata File Provided!")
    ) 
    req(!is.null(gVars$working_status)) 
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
    
    inputGx <- gVars$inputGx[[1]] %>% separate_rows(., allowed_features, sep=",") %>% setNames(., c("Reference Label","Label Synonym","Content","Content Synonym"))
    # a filler was needed for the empty starting dictionary
    filler_removed <- inputGx %>%filter(if_any(.cols=everything(), ~ !grepl("*fillertoremove*",.)))
    DT::datatable(filler_removed, filter="none",
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
    loadingText <- "LOADING"
    return(loadingText)
  })
  
  #output$selQuote <- renderUI({
  #  selectInput("quote", "Quotes", choices=gVars$quoteChoices, selected=gVars$quoteChoices[1])
  #})
  
  output$selSep <- renderUI({
    selectInput("sepS", "Field Separator", choices=gVars$sepChoices, selected=gVars$sepChoices[1])
  })
  
  
  output$selGxSep <- renderUI({
    selectInput("gxSepS", "Column Seperator", choices=gVars$sepChoices, selected=gVars$sepChoices[1])
  })
  
  output$selGxQuote <- renderUI({
    selectInput("gxQuote", "Quotes", choices=gVars$quoteChoices, selected=gVars$quoteChoices[1])
  })
  
  
  

  
  
  data <- reactiveValues(buttonstyle = NULL)
  ## css styling for selectizeInput menu
  CSSgb <- function(colors){
    template <- "
            .btn-group-toggle  .btn-custom-class2 {
                      border-radius: 10px !important;
                      border: none;
                      opacity: 1;
                      pointer-events: none;
             }                          
            .btn-group-toggle  .btn-custom-class2.active {
                      background: #04AA6D !important;
                      color: white !important;
                      border: red !important;
             }                                  
            .btn-group-toggle:nth-child(%s) .btn-custom-class2 {
                      background: %s !important;
                      color: white !important;
            }"
    paste0(
      apply(cbind(seq_along(colors), colors), 1, function(vc){
        sprintf(template, vc[1], vc[2])
      }),
      collapse = "\n"
    )
  }
  
  observeEvent(c(input$showFinalDownload, input$reset_window_reports), {
    req(isTRUE(gVars$working_status))   #single mode
    for_report <- gVars$potential_new_entries_report
    'Voc. Issue' <- data.frame(for_report) %>% .[.$Decision %in% c("Issue"),]
    'Voc. Discard' <- data.frame(for_report) %>% .[.$Decision %in% c("Discard"),]
    'Voc. Accepted'  <- data.frame(for_report) %>% .[.$Decision %in% c("Accepted", "Newly Accepted"),]
    category <- c("Voc. Issue","Voc. Discard","Voc. Accepted" )
    style <- sapply(category, function(k) { 
      stl<-  if( nrow(get(k))!=0) {"red"}
      else {"#CCCCCC"}
      stl
    })
    data$buttonstyle <- style 
  })
  
  
  #MULTIPLE INTEGRATION CASE
  observeEvent(c(input$showFinalDownload,input$reset_window_reports), {
    req(isFALSE(gVars$working_status))   #multiple mode
    'Issue Entry' <- gVars$multi_ISSmsg
    'Consistent Entry'  <- gVars$multi_ACmsg
    category <- c("Issue Entry","Consistent Entry" )
    style <- sapply(category, function(k) { 
      stl<-  if( length(get(k))!=0) {"red"}
      else {"#CCCCCC"}
      stl
    })
    data$buttonstyle <- style 
  })
  
  
  
  # update coloursof the second block of buttons according to the presence of data stored
  output$css_checkbox <- renderUI({ 
    tags$style(HTML(CSSgb(data$buttonstyle ))) })
  
  
  
  
  
  output$download_Upd_PhdataV2 <- shiny::downloadHandler(
    #arrange new file name
    current_df_file_name <- gVars$current_df_file_name %>% 
      sub('.[^.]*$', '', .) %>%
      paste(., collapse = "_"),
    
    filename = function() {
      filename = paste("updated_",current_df_file_name,"v",
            format(Sys.time(), "%Y-%m-%d_%H-%M-%S"),"_",gVars$sha256_stringID,
            ".xlsx", 
            sep = "")
      
    },
    content = function(file) {
      shinyjs::html(id="loadingText", "Updating dataset")
      shinyjs::show(id="loading-content")
      on.exit({ 
        shinyjs::hide(id="loading-content", anim=TRUE, animType="fade")  
      })
      upd_df<-  gVars$phTable[[1]]
      row.names(upd_df) <-NULL
      
      write.xlsx(upd_df,file, row.names=FALSE) 
      #flag to trigger the report
      data$whichBAR <- "updPHdt" 
    }
  )
  
  
  
  output$download_Upd_VocabularyV2 <- downloadHandler(
    filename = function() {
      file = paste0("updated_vocabulary_v",
                    format(Sys.time(), "%Y-%m-%d_%H-%M-%S"),"_",gVars$sha256_stringID,
                    ".xlsx", 
                    sep = "")
    },
    content = function(file) {
      req(!is.null(gVars$dkeys))
      upd_voc <- data.frame(gVars$dkeys) %>% 
        group_by(label, lab_syn,allowed_features) %>%
        summarise(syn_features = str_c(syn_features, collapse="|"))
      
      shinyjs::html(id="loadingText", "Updating Vocabulary")
      shinyjs::show(id="loading-content")
      on.exit({
        shinyjs::hide(id="loading-content", anim=TRUE, animType="fade")    
      })
      
      upd_voc <- data.frame(upd_voc)
      write.xlsx(upd_voc,file, row.names=FALSE)
      #flag to trigger the report
      data$whichBAR <- "updVoc" 
      
    }
  )
  
  
  output$download_Upd_PhdataV2multi <- shiny::downloadHandler(
    #arrange new file name
    filename = function() {
      file=paste("multiple_dataset_v",
            format(Sys.time(), "%Y-%m-%d_%H-%M-%S"),"_",gVars$sha256_stringID,
            ".xlsx", 
            sep = "")
      
    },
    content = function(file) {
      shinyjs::html(id="loadingText", "Saving Integrated Dataset")
      shinyjs::show(id="loading-content")
      on.exit({ 
        shinyjs::hide(id="loading-content", anim=TRUE, animType="fade")  
      })
      multidf<-  data.frame(gVars$multidf)
      
      write.xlsx(multidf,file, row.names=FALSE) 
      #flag to trigger the report
      data$whichBAR <- "updPHdt_multi" 
    }
  )
  
  
  
  
  
  
  
  data <- reactiveValues(whichBAR =NULL, finaldownloadtrigger=NULL, go_stopMAINreport = NULL,go_stopENDINGsession = NULL, go_stopUPDphdata = NULL,  go_stopUPDvoc = NULL, go_stopPT_GLP =NULL, selector_voc_subreports = NULL, names_subreports=NULL, clicked_btns=NULL, end_main_downloads=NULL)
  
  
  observeEvent(input$download_single_rpt,{           #download general report 
    req(!is.null(gVars$phTable[[1]]), !is.null(gVars$dkeys) )
    data$go_stopMAINreport <-FALSE
    data$finaldownloadtrigger <-FALSE
    data$whichBAR <- NULL
    data$stop_dwn <- NULL
    disable("download_single_rpt")
    click("update_glp_button")
    click("update_df_button")     #upd dataset
    click("exportRpt")
  })
  
  observeEvent(data$go_stopENDINGsession, {   #download general report upd df
    req(isTRUE(gVars$working_status))   #single mode
    req(isTRUE(data$go_stopENDINGsession))
    updateCheckboxGroupButtons(session,
                               inputId = "button_main_report",
                               selected =  "Main Report",  
                               disabled=TRUE) 
    data$whichBAR <- NULL
    data$stop_dwn <- NULL
    gc()
    sendSweetAlert(title="Info", text = "Saving the session will take some time, please wait")
    click("dosave_session")
  })
  
  
  observeEvent(data$go_stopUPDphdata, {   #download general report upd df
    req(isTRUE(gVars$working_status))   #single mode
    req(isTRUE(data$go_stopUPDphdata))
    updateCheckboxGroupButtons(session,
                               inputId = "button_main_report",
                               selected = c("Main Report", "Current Session"), 
                               disabled=TRUE)
    data$whichBAR <- NULL
    data$stop_dwn <- NULL
    click("update_df_button")
    gc()
    click("download_Upd_PhdataV2")
    
    
  })
  
  observeEvent(data$go_stopUPDvoc, {
    req(isTRUE(gVars$working_status))   #single mode
    req(isTRUE(data$go_stopUPDvoc))
    updateCheckboxGroupButtons(session,
                               inputId = "button_main_report",
                               selected = c("Main Report", "Current Session", "Updated Phenodata"),
                               disabled=TRUE)
    data$whichBAR <- NULL
    data$stop_dwn <- NULL
    # click("generate_dict_updated_version")
    delay(1000, click("download_Upd_VocabularyV2") )
  })
  
  observeEvent(data$go_stopPT_GLP, {
    req(isTRUE(gVars$working_status))   #single mode
    req(isTRUE(data$go_stopPT_GLP))
    updateCheckboxGroupButtons(session,
                               inputId = "button_main_report",
                               selected = c("Main Report", "Current Session", "Updated Phenodata", "Updated Vocabulary"),
                               disabled=TRUE)
    data$whichBAR <- NULL
    data$stop_dwn <- NULL
    click("update_glp_button")
    delay(1000, click("download_Upd_GLPv2") )
    gc()
    
  })
  
  
  
  
  
  
  
  
  
  
  
  
  #SINGLE CURATION CASE
  observe({
    data$whichBAR 
    req(isTRUE(gVars$working_status))   #single mode
    req(isFALSE(data$finaldownloadtrigger))
    if(is.null(data$whichBAR)) { 
      return ()
    } 
    else if (data$whichBAR == "mainS" && isTRUE(data$go_stopMAINreport) ){
      for (i in 1:100) {
        updateProgressBar(
          session = session,
          id = "mainS",
          value = i, total = 100
        )
        Sys.sleep(0.1)
      }
      data$go_stopMAINreport <- FALSE
      data$whichBAR=NULL
      data$go_stopENDINGsession <-TRUE
    } 
    else if(data$whichBAR == "updSession" && isTRUE(data$go_stopENDINGsession) ){
      closeSweetAlert(session)
      for (i in 1:100) {
        updateProgressBar(
          session = session,
          id = "updSession",
          value = i, total = 100
        )
        Sys.sleep(0.2)
      }
      #flag to trigger the report
      data$go_stopENDINGsession <-FALSE
      data$whichBAR <- NULL
      data$go_stopUPDphdata <- TRUE
    } 
    else if(data$whichBAR == "updPHdt" && isTRUE(data$go_stopUPDphdata) ){
      for (i in 1:100) {
        updateProgressBar(
          session = session,
          id = "updPHdt",
          value = i, total = 100
        )
        Sys.sleep(0.1)
      }
      #flag to trigger the report
      data$go_stopUPDphdata <-FALSE
      data$whichBAR <- NULL
      data$go_stopUPDvoc <- TRUE
    } else if(data$whichBAR == "updVoc" && isTRUE(data$go_stopUPDvoc) ){
      for (i in 1:100) {
        updateProgressBar(
          session = session,
          id = "updVoc",
          value = i, total = 100
        )
        Sys.sleep(0.15)
      }
      #flag to trigger the report
      data$go_stopUPDvoc <-FALSE
      data$whichBAR <- NULL 
      data$go_stopPT_GLP <- TRUE
      
    } else if(data$whichBAR == "updPT_GLP" && isTRUE(data$go_stopPT_GLP) ){
          for (i in 1:100) {
            updateProgressBar(
              session = session,
              id = "updPT_GLP",
              value = i, total = 100
            )
            Sys.sleep(0.2)
          }
          #flag to trigger the report
          data$go_stopPT_GLP <-FALSE
          data$whichBAR <- NULL
          n_reports_madein_upd_voc <- length(data$buttonstyle[data$buttonstyle=="red"])
          if (n_reports_madein_upd_voc >0){
            data$selector_voc_subreports <- TRUE
            data$names_subreports <- names(data$buttonstyle[data$buttonstyle =="red"])
            data$end_main_downloads <- TRUE
          } 
          else {
            updateCheckboxGroupButtons(session,
                                       inputId = "button_main_report",
                                       selected = c("Main Report", "Current Session", "Updated Phenodata", "Updated Vocabulary", "Procedures/GLP"),
                                       disabled=TRUE)
            data$finaldownloadtrigger <- TRUE} 
        }
    else if (data$whichBAR == "upd_voc_issue" && isTRUE(data$buttonstyle[1]=="red") ){
      for (i in 1:100) {
        updateProgressBar(
          session = session,
          id = "upd_voc_issue",
          value = i, total = 100
        )
        Sys.sleep(0.1)
      }
      data$names_subreports[data$names_subreports == "Voc. Issue"] <- FALSE
      data$whichBAR=NULL
      data$clicked_btns <- unique(c(data$clicked_btns, "Voc. Issue"))
    } 
    else if (data$whichBAR == "upd_voc_discard" && isTRUE(data$buttonstyle[2]=="red") ){
      for (i in 1:100) {
        updateProgressBar(
          session = session,
          id = "upd_voc_discard",
          value = i, total = 100
        )
        Sys.sleep(0.1)
      }
      data$names_subreports[data$names_subreports == "Voc. Discard"] <- FALSE
      data$whichBAR=NULL
      data$clicked_btns <- unique(c(data$clicked_btns, "Voc. Discard"))
    } 
    else if (data$whichBAR == "upd_voc_accepted" && isTRUE(data$buttonstyle[3]=="red") ){
      for (i in 1:100) {
        updateProgressBar(
          session = session,
          id = "upd_voc_accepted",
          value = i, total = 100
        )
        Sys.sleep(0.1)
      }
      data$names_subreports[data$names_subreports == "Voc. Accepted"] <- FALSE
      data$whichBAR=NULL
      data$clicked_btns <- unique(c(data$clicked_btns, "Voc. Accepted"))
      data$buttonstyle[3] <- "#04AA6D"
    } else {  return ()}
  })
  
  
#  observe({
#    data$finaldownloadtrigger
#    req(isTRUE(data$finaldownloadtrigger))
#    if(data$selector_voc_subreports) {
#      delay(1000,
#            data$whichBAR <- data$finaldownloadtrigger <- data$go_stopMAINreport <- 
#              data$go_stopENDINGsession <- data$go_stopUPDphdata <- data$go_stopUPDvoc <- 
#              data$go_stopPT_GLP <- data$selector_voc_subreports <- NULL)
#    }
#  })
  
  
  
  
  observe({
    req(isTRUE(gVars$working_status))   #single mode
    req(!is.null(data$buttonstyle))
    other_rep_logical <- data$buttonstyle %in% "red"
    if (other_rep_logical[1]){   #issue
      output$panelStatus_Vissue <- reactive({
        TRUE
      }) 
      outputOptions(output, "panelStatus_Vissue", suspendWhenHidden = FALSE)
    } else {output$panelStatus_Vissue <- reactive({FALSE}) }
    
    if (other_rep_logical[2]){   #discard
      output$panelStatus_Vdiscard <- reactive({
        TRUE
      }) 
      outputOptions(output, "panelStatus_Vdiscard", suspendWhenHidden = FALSE)
    } else {output$panelStatus_Vdiscard <- reactive({FALSE}) }
    
    if (other_rep_logical[3]){   #accepted
      output$panelStatus_Vaccept <- reactive({
        TRUE
      }) 
      outputOptions(output, "panelStatus_Vaccept", suspendWhenHidden = FALSE)
    } else {output$panelStatus_Vaccept <- reactive({FALSE}) }
    
  })
  
  
  observe({
    req(isFALSE(gVars$working_status))   #multiple mode
    req(!is.null(data$buttonstyle))
    other_rep_logical <- data$buttonstyle %in% "red"
    if (other_rep_logical[1]){   #issue multi
      output$panelStatus_Missue <- reactive({
        TRUE
      }) 
      outputOptions(output, "panelStatus_Missue", suspendWhenHidden = FALSE)
    } else {output$panelStatus_Missue <- reactive({FALSE}) }
    
    if (other_rep_logical[2]){   #consistent multi
      output$panelStatus_Mconsistent <- reactive({
        TRUE
      }) 
      outputOptions(output, "panelStatus_Mconsistent", suspendWhenHidden = FALSE)
    } else {output$panelStatus_Mconsistent <- reactive({FALSE}) }
    
  })
  
  
  observeEvent(c(input$reset_window_reports, input$showFinalDownload),{  
    req(!is.null(gVars$working_status))
    enable("download_single_rpt")
    enable("download_multi_rpt")
    closeSweetAlert(session)
    
    data$whichBAR <- data$finaldownloadtrigger <- data$go_stopMAINreport <- 
      data$go_stopENDINGsession <- data$go_stopUPDphdata <- data$go_stopUPDvoc <- 
      data$go_stopPT_GLP <- data$selector_voc_subreports <- 
      data$names_subreports <- data$clicked_btns <- NULL
    
      updateCheckboxGroupButtons(session,
                                 inputId = "button_main_report",selected=character(0))
      updateCheckboxGroupButtons(session,
                                 inputId = "button_main_report_multi", selected=character(0))
      updateCheckboxGroupButtons(session,
                                 inputId = "button_voc_report", selected=character(0))
      updateCheckboxGroupButtons(session,
                                 inputId = "button_voc_report_multi", selected=character(0))
      updateProgressBar(id = "mainS", value = 0)
      updateProgressBar(id = "mainS_multi", value = 0)
      updateProgressBar(id = "updSession", value = 0)
      updateProgressBar(id = "updPHdt", value = 0)
      updateProgressBar(id = "updVoc", value = 0)
      updateProgressBar(id = "updPT_GLP", value = 0)
      updateProgressBar(id = "upd_voc_issue", value = 0)
      updateProgressBar(id = "upd_voc_discard", value = 0)
      updateProgressBar(id = "upd_voc_accepted", value = 0)
      
      updateProgressBar(id = "mainS_multi", value = 0)
      updateProgressBar(id = "updSession_multi", value = 0)
      updateProgressBar(id = "updPHdt_multi", value = 0)
      updateProgressBar(id = "updPT_GLP_multi", value = 0)
      updateProgressBar(id = "upd_multi_issue", value = 0)
      updateProgressBar(id = "upd_multi_consistent", value = 0)
  })

  #single case
  observeEvent(data$end_main_downloads,{ 
    req(isTRUE(gVars$working_status))   #single mode
    req(isTRUE(data$end_main_downloads))
    updateCheckboxGroupButtons(session,
                               inputId = "button_main_report",
                               selected = c("Main Report", "Current Session", "Updated Phenodata", "Updated Vocabulary", "Procedures/GLP"))
    data$end_main_downloads <- NULL
  })
  
  #multi case
  observeEvent(data$end_main_downloads,{ 
    req(isFALSE(gVars$working_status))   #multiple mode
    req(isTRUE(data$end_main_downloads))
    updateCheckboxGroupButtons(session,
                               inputId = "button_main_report_multi",
                               selected = c("Main Report", "Current Session", "Integrated Phenodata", "Procedures/GLP"))
    data$end_main_downloads <- NULL
  })
  
  
  observe({   #single mode
    req(isTRUE(gVars$working_status))
    data$names_subreports 
    tmp<- as.logical(data$names_subreports)
    req(isFALSE(unique(tmp)))
    updateCheckboxGroupButtons(session,
                               inputId = "button_voc_report",
                               selected = data$clicked_btns,
                               status = "custom-class2",
                               disabled=TRUE)
  })
  
  
  observe({   #multimode
    req(isFALSE(gVars$working_status))
    data$names_subreports 
    tmp<- as.logical(data$names_subreports)
    req(isFALSE(unique(tmp)))
    updateCheckboxGroupButtons(session,
                               inputId = "button_voc_report_multi",
                               selected = data$clicked_btns,
                               status = "custom-class2",
                               disabled=TRUE)
  })
 
  
  
  observeEvent(data$clicked_btns,{ 
    if(isFALSE(gVars$working_status)) {  #multiple mode
          updateCheckboxGroupButtons(session,
                               inputId = "button_voc_report_multi", 
                               selected =  data$clicked_btns,
                               status =  "custom-class2", 
                               disabled=TRUE)  
    } else {   #single mode
      updateCheckboxGroupButtons(session,
                                 inputId = "button_voc_report", 
                                 selected =  data$clicked_btns,
                                 status =  "custom-class2", 
                                 disabled=TRUE) 
      }
  })
  
  
  
  
  observeEvent(data$names_subreports,{   
    req(isTruthy(data$names_subreports))
    for (other_reports in (data$names_subreports)){
      if (other_reports %in%  "Voc. Issue") {
          
        data$whichBAR <- NULL
        data$stop_dwn <- NULL
        gc()
        click("download_to_review_issue2")
        
      }else if (other_reports %in%  "Voc. Discard") {
         
        data$whichBAR <- NULL
        data$stop_dwn <- NULL
        gc()
        click("download_to_review_discard2")
      } else if(other_reports %in%  "Voc. Accepted"){
        
        
        data$whichBAR <- NULL
        data$stop_dwn <- NULL
        gc()
        click("download_to_review_accepted2")
      }
      #multi
      else if (other_reports %in%  "Issue Entry") {
        
        data$whichBAR <- NULL
        data$stop_dwn <- NULL
        gc()
        click("download_multi_issueV2")    #123
      } else if(other_reports %in%  "Consistent Entry"){
        
        
        data$whichBAR <- NULL
        data$stop_dwn <- NULL
        gc()
        click("download_multi_consistentV2")    #123
      }else {
        return ()}
    } 
  })
  
  
  
  
  
  
  output$exportRpt <- shiny::downloadHandler(
    #arrange new file name
    if(isFALSE(gVars$working_status)) {  #multiple mode)
      current_df_file_name <- "MULTIFILE_"
      } else {   #single mode
          current_df_file_name <- gVars$current_df_file_name %>% 
            sub('.[^.]*$', '', .) %>%
            paste(., collapse = "_")
    },
    
    filename = function(){
      paste("ESPERANTO_Analysis_Report_", current_df_file_name, Sys.Date(),"_",gVars$sha256_stringID, '.html', sep='')
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
      file.copy("ESPERANTO_app/report/report.Rmd", tempReport, overwrite=TRUE)
      
      
      params <- list(gVars=gVars, input=input, srcDir=srcDir)
      rmarkdown::render(tempReport, output_file=con,
                        params=params,
                        envir=new.env(parent=globalenv())
      )
      #flag to trigger the report
      data$go_stopMAINreport <-TRUE
      data$finaldownloadtrigger <-FALSE
      data$whichBAR <- ifelse(isTRUE(gVars$working_status), "mainS" ,"mainS_multi") 
      #Enable Warning
      options(warn = oldw)
    }
  )
 
  
  
  
  output$download_Upd_GLPv2multi <- shiny::downloadHandler(
    
    filename = function(){
      paste("Track_PT_GLP_MultiFile_",Sys.Date(),"_",gVars$sha256_stringID, '.html', sep='')
    },
    
    content = function(con){
      #start loading screen
      shinyjs::html(id="loadingText", "CREATING PROCEDURAL/GLP REPORT")
      shinyjs::show(id="loading-content")
      
      on.exit({
        shinyjs::hide(id="loading-content", anim=TRUE, animType="fade")
      })
      #Disable Warning
      oldw <- getOption("warn")
      options(warn = -1)
      tempReport <- file.path((tempdir()), "report_PT_GLP.Rmd")
      file.copy("ESPERANTO_app/report/report_PT_GLP.Rmd", tempReport, overwrite=TRUE)
      
      
      params <- list(gVars=gVars, input=input, srcDir=srcDir)
      rmarkdown::render(tempReport, output_file=con,
                        params=params,
                        envir=new.env(parent=globalenv())
      )
      data$go_stopPT_GLP <-TRUE
      data$finaldownloadtrigger <-FALSE
      data$whichBAR <-  "updPT_GLP_multi"
      #Enable Warning
      options(warn = oldw)
    }
  )
  
  
  
  
  #multiple integration donwloads reports
  observeEvent(input$download_multi_rpt,{           #download general report 
    req(!is.null(gVars$multidf), !is.null(gVars$dkeys) )
    data$go_stopMAINreport <-FALSE
    data$finaldownloadtrigger <-FALSE
    data$whichBAR <- NULL
    data$stop_dwn <- NULL
    disable("download_multi_rpt")
    click("update_glp_button")
    click("update_df_button")     #upd dataset
    click("exportRpt")
    
    
  })
  
  observeEvent(data$go_stopENDINGsession, {   #download general report upd df
    req(isFALSE(gVars$working_status))   #multiple mode
    req(isTRUE(data$go_stopENDINGsession))
    updateCheckboxGroupButtons(session,
                               inputId = "button_main_report_multi",
                               selected =  "Main Report",  
                               disabled=TRUE) 
    data$whichBAR <- NULL
    data$stop_dwn <- NULL
    gc()
    sendSweetAlert(title="Info", text = "Saving the session will take some time, please wait")
    click("dosave_session")  
  })
  
  
  observeEvent(data$go_stopUPDphdata, {   #download general report upd df
    req(isFALSE(gVars$working_status))   #multiple mode
    req(isTRUE(data$go_stopUPDphdata))
    updateCheckboxGroupButtons(session,
                               inputId = "button_main_report_multi",
                               selected = c("Main Report", "Current Session"), 
                               disabled=TRUE)
    data$whichBAR <- NULL
    data$stop_dwn <- NULL
    gc()
    click("download_Upd_PhdataV2multi")        
    
    
  })
  
  
  
  observeEvent(data$go_stopPT_GLP, {
    req(isFALSE(gVars$working_status))   #multiple mode
    req(isTRUE(data$go_stopPT_GLP))
    updateCheckboxGroupButtons(session,
                               inputId = "button_main_report_multi",
                               selected = c("Main Report", "Current Session", "Integrated Phenodata"),
                               disabled=TRUE)
    data$whichBAR <- NULL
    data$stop_dwn <- NULL 
    delay(500, click("download_Upd_GLPv2multi") )                 
    gc()
    
  })
  
  
  
  
  observe({
    data$whichBAR 
    req(isFALSE(gVars$working_status))   #multiple mode
    req(isFALSE(data$finaldownloadtrigger))
    if(is.null(data$whichBAR)) { 
      return ()
    } 
    else if (data$whichBAR == "mainS_multi" && isTRUE(data$go_stopMAINreport) ){
      for (i in 1:100) {
        updateProgressBar(
          session = session,
          id = "mainS_multi",
          value = i, total = 100
        )
        Sys.sleep(0.1)
      }
      data$go_stopMAINreport <- FALSE
      data$whichBAR=NULL
      data$go_stopENDINGsession <-TRUE
    } 
    else if(data$whichBAR == "updSession_multi" && isTRUE(data$go_stopENDINGsession) ){
      closeSweetAlert(session)
      for (i in 1:100) {
        updateProgressBar(
          session = session,
          id = "updSession_multi",
          value = i, total = 100
        )
        Sys.sleep(0.2)
      }
      #flag to trigger the report
      data$go_stopENDINGsession <-FALSE
      data$whichBAR <- NULL
      data$go_stopUPDphdata <- TRUE
    } 
    else if(data$whichBAR == "updPHdt_multi" && isTRUE(data$go_stopUPDphdata) ){
      for (i in 1:100) {
        updateProgressBar(
          session = session,
          id = "updPHdt_multi",
          value = i, total = 100
        )
        Sys.sleep(0.1)
      }
      #flag to trigger the report
      data$go_stopUPDvoc <-FALSE
      data$whichBAR <- NULL 
      data$go_stopPT_GLP <- TRUE
      
    } else if(data$whichBAR == "updPT_GLP_multi" && isTRUE(data$go_stopPT_GLP) ){
      for (i in 1:100) {
        updateProgressBar(
          session = session,
          id = "updPT_GLP_multi",
          value = i, total = 100
        )
        Sys.sleep(0.2)
      }
      #flag to trigger the report
      data$go_stopPT_GLP <-FALSE
      data$whichBAR <- NULL
      n_reports_madein_upd_voc <- length(data$buttonstyle[data$buttonstyle=="red"])
      if (n_reports_madein_upd_voc >0){
        data$selector_voc_subreports <- TRUE
        data$names_subreports <- names(data$buttonstyle[data$buttonstyle =="red"])
        data$end_main_downloads <- TRUE
      } 
      else {
        updateCheckboxGroupButtons(session,
                                   inputId = "button_main_report_multi",
                                   selected = c("Main Report", "Current Session", "Integrated Phenodata", "Procedures/GLP"),
                                   disabled=TRUE)
        data$finaldownloadtrigger <- TRUE} 
    }
    else if (data$whichBAR == "upd_multi_issue" && isTRUE(data$buttonstyle[1]=="red") ){
      for (i in 1:100) {
        updateProgressBar(
          session = session,
          id = "upd_multi_issue",
          value = i, total = 100
        )
        Sys.sleep(0.1)
      }
      data$names_subreports[data$names_subreports == "Issue Entry"] <- FALSE
      data$whichBAR=NULL
      data$clicked_btns <- unique(c(data$clicked_btns, "Issue Entry"))
    } 
    else if (data$whichBAR == "upd_multi_consistent" && isTRUE(data$buttonstyle[2]=="red") ){
      for (i in 1:100) {
        updateProgressBar(
          session = session,
          id = "upd_multi_consistent",
          value = i, total = 100
        )
        Sys.sleep(0.1)
      }
      data$names_subreports[data$names_subreports == "Consistent Entry"] <- FALSE
      data$whichBAR=NULL
      data$clicked_btns <- unique(c(data$clicked_btns, "Consistent Entry"))
      data$buttonstyle[3] <- "#04AA6D"
    } else {  return ()}
  })
  
  
  
  
  #da cancellare
  output$exportRpt_singlebtn <- shiny::downloadHandler(
    #arrange new file name
    if(isFALSE(gVars$working_status)) {  #multiple mode)
      current_df_file_name <- "MULTIFILE_"
    } else {   #single mode
      current_df_file_name <- gVars$current_df_file_name %>% 
        sub('.[^.]*$', '', .) %>%
        paste(., collapse = "_")
    },
    
    filename = function(){
      paste("ESPERANTO_Analysis_Report_", current_df_file_name, Sys.Date(),"_",gVars$sha256_stringID, '.html', sep='')
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
      file.copy("ESPERANTO_app/report/report.Rmd", tempReport, overwrite=TRUE)
      
      
      params <- list(gVars=gVars, input=input, srcDir=srcDir)
      rmarkdown::render(tempReport, output_file=con,
                        params=params,
                        envir=new.env(parent=globalenv())
      )
      
      #Enable Warning
      options(warn = oldw)
    }
  )
  
  
  
  output$download_Int_Phdata <- shiny::downloadHandler(
    #arrange new file name
    filename = function() {
      file=paste("multiple_dataset_v",
                 format(Sys.time(), "%Y-%m-%d_%H-%M-%S"),"_",gVars$sha256_stringID,
                 ".xlsx", 
                 sep = "")
      
    },
    content = function(file) {
      shinyjs::html(id="loadingText", "Saving Integrated Dataset")
      shinyjs::show(id="loading-content")
      on.exit({ 
        shinyjs::hide(id="loading-content", anim=TRUE, animType="fade")  
      })
      multidf<-  data.frame(gVars$multidf)
      
      write.xlsx(multidf,file, row.names=FALSE) 
      
    }
  )
  
  
  
  
  
  
   
})
