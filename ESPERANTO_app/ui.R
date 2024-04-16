suppressMessages(library(shinydashboard))
#suppressMessages(library(shinyFiles))
suppressMessages(library(DT))
suppressMessages(library(rhandsontable))
suppressMessages(library(shinycssloaders))
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
library(xlsx)
library(tidyverse)
library(plotly)
library(shiny)
library(shinyjs)
library(shinyBS)
library(shinyalert)
library(shinyFeedback)
library(knitr)
library(kableExtra)
library(splitstackshape)

appCSS <- "
//.modal-lg {
//width: 95%;
//}
//.main-header { z-index: 100000; }
.main-sidebar { background-color: white !important;width: 252px; }
.sidebar { color: black; max-height: 900px; overflow-y: scroll; width: 250px; }
.sidebar a { color: blue; margin: 20px !important;}
.content-wrapper { margin-left: 15%;  }
//.panel { background-color: #222d32; }
.panel-title a { font-weight: bold; color: white !important; }
.panel-warning .panel-heading { background-color: #00c0ef; }
.panel-warning { border-color: #8de9ff; }
.panel-sample .panel-heading { background-color: #2C6C99; }
.panel-sample { border-color: #2C6C99; }
.panel-danger .panel-heading { background-color: #dd4b39; }
.panel-success .panel-heading { background-color: #00a65a; }
.panel-info .panel-heading { background-color: #7e46ff; }
.panel-primary .panel-heading { background-color: #3079ae; }
.multicol { 
height: 300px;
-webkit-column-count: 4; /* Chrome, Safari, Opera */ 
-moz-column-count: 4;    /* Firefox */ 
column-count: 4; 
-moz-column-fill: auto;
-column-fill: auto;
} 
#loading-content {
position: absolute;
background: white !important;
opacity: 0.8;
z-index: 1000000;
left: 0;
right: 0;
top: 0;
bottom: 0;
font-size: 50px;
text-align: center;
color: #black;
}
#loading-gif { 
opacity: 0.8; 
display: block;
margin-left: auto;
margin-right: auto;
vertical-align: middle;
z-index: 1000000;
}

"

jsCode <- "
callback = 'function(table) {
table.on('click.dt', 'tr', function() {
table.$('tr.selected').removeClass('selected');
$(this).toggleClass('selected');            
Shiny.onInputChange('rows',
table.rows('.selected').data()[0][0]);
});
}'
"

unclosable_modal <- function(...){
  b = shinyBS::bsModal(...)
  b[[2]]$`data-backdrop` = "static"
  b[[2]]$`data-keyboard` = "false"
  return(b)
}




ui <- function(request) {
  fluidPage( 
    useShinyFeedback(), 
    useShinyjs(),
    inlineCSS(appCSS), 
    hidden(div(id="loading-content",
               img(id="loading-gif", src="screen-loading.gif"),
               p(id="loadingText", "WORKING"),
               p("...")
    )),
    
    
    shinyWidgets::useSweetAlert(), 
    
    dashboardPage(
      dashboardHeader(title="ESPERANTO: sEmi-SuPERvised toxicogenomics meta-dAta curatioN TOol", titleWidth="60%"),
      dashboardSidebar(disable=FALSE,
                       bsCollapse(id="bsSidebar1", open="LOAD PHENODATA",
                                  bsCollapsePanel("LOAD PHENODATA", style="warning",
                                                  fluidRow(
                                                    column(12, align="center",
                                                           shinyBS::bsButton("import_pheno_submit", label="Import PhenoData", style="danger", icon("exclamation-circle"), disabled=TRUE),
                                                           shinyBS::bsTooltip("import_pheno_submit", "Launch a graphical window, to configure import of phenodata from a file!", placement="bottom")
                                                    )
                                                  )
                                  ),
                                  bsCollapsePanel("LOAD VOCABULARY", style="warning",
                                                  fluidRow(
                                                    column(12, align="center",
                                                           shinyBS::bsButton("import_voc_submit", label="Import Vocabulary", style="danger", icon=icon("exclamation-circle")),
                                                           shinyBS::bsTooltip("import_voc_submit", "Launch a graphical window, to configure import of a vocabulary from a file!", placement="bottom")
                                                           
                                                    )
                                                  )
                                  ),
                                  bsCollapsePanel("GLP MODE", style="warning",
                                                  fluidRow(
                                                    column(12, align="center",
                                                           shinyBS::bsButton("GLP_inactivation_mode", label="GLP mode disabled", type = "toggle", value = TRUE, style="danger", icon=icon("exclamation-circle")),
                                                           shinyBS::bsTooltip("GLP_inactivation_mode", "When toggled, GLP mode is activated and the user can insert comments about each step of the curation!", placement="bottom"),
                                                           
                                                    )
                                                  )
                                  ),
                                  bsCollapsePanel("STRUCTURE HOMOGENIZATION", style="warning",
                                                  fluidRow(
                                                    column(12, align="center",
                                                           shinyBS::bsButton("relabelling_block_button", label="Re-Labelling", style="danger"),
                                                           shinyBS::bsTooltip("relabelling_block_button", "Launch a graphical window, to show the pairs of suggested labels coded by vocabulary for the dataset columns!", placement="bottom")
                                                    )
                                                  ),
                                                  fluidRow(
                                                    column(12, align="center",
                                                           shinyBS::bsButton("dupl_removal_block_button", label="Duplicate Removal", style="danger"),
                                                           shinyBS::bsTooltip("dupl_removal_block_button", "Launch a graphical window, to show the candidate columns for duplicate removal!", placement="bottom")
                                                    )
                                                  ),
                                                  
                                                  fluidRow(
                                                    column(12, align="center",
                                                           shinyBS::bsButton("recoding_button", label="Content Homogenization", style="danger"),
                                                           shinyBS::bsTooltip("recoding_button", "Launch a graphical window, to show the candidate features to homogenize the cell contents of the dataset!", placement="bottom")
                                                           
                                                    )
                                                  )       
                                  )
                                  
                                  
                       ),
                       
                       bsCollapse(id="bsSidebar0_5", #open="MORE",
                                  bsCollapsePanel("SESSION MANAGEMENT", style="warning",
                                                  fluidRow(
                                                    column(12, align="center",
                                                           shinyBS::bsButton("dorestore_session", label="Load session", style="danger",icon=icon("exclamation-circle")),
                                                           shinyBS::bsTooltip("dorestore_session", "Launch a graphical window, to restore a previously saved curation session", placement="bottom"),
                                                           
                                                           shinyBS::bsButton("dosave_session", label="Save session", style="danger",icon=icon("exclamation-circle")),
                                                           shinyBS::bsTooltip("dosave_session", "Launch a graphical window, to save the current curation session", placement="bottom"),
                                                           
                                                           shinyBS::bsButton("launch_reset_modal", label="Reset Session", style="danger",icon=icon("exclamation-circle")),
                                                           shinyBS::bsTooltip("launch_reset_modal", "Launch a graphical window, to reset the session", placement="bottom")    
                                                           
                                                    )
                                                  )
                                  ) 
                       ),
                       
                       bsCollapse(id="bsSidebar0", open="MORE INFO",
                                  bsCollapsePanel("DOWNLOAD REPORTS",style="sample",
                                                  fluidRow(
                                                    column(12, align="center",
                                                           shinyWidgets::actionBttn(
                                                             inputId = "showFinalDownload",
                                                             label = "Export Reports", 
                                                             style = "gradient",
                                                             color = "primary",
                                                             size= "sm",
                                                             icon = shiny::icon("download")
                                                           )
                                                           
                                                           
                                                          # ,downloadButton("exportRpt_singlebtn", "Only_Analysis_Rpt"), actionBttn("brow","brow")
                                                           
                                                           
                                                    )
                                                  )            
                                  ),
                                  bsCollapsePanel("MORE INFO", style="sample",
                                                  fluidRow(
                                                    HTML(" <a style=color:blue;   href=\"https://github.com/fhaive/esperanto\">GitHub</a>")
                                                  ),
                                                  fluidRow(
                                                    HTML("<a style=color:blue;  href=\"https://github.com/fhaive/esperanto/wiki/ESPERANTO-User-Guide\">Manual</a>")
                                                  ),
                                                  fluidRow(
                                                    HTML("<a style=color:blue;   href=\"https://github.com/fhaive/esperanto/tree/master/sample_data\">Sample phenodata</a>")
                                                  )
                                  )
                       )
      ),
      dashboardBody(
        ###beginning relabelling section
        
        shinyBS::bsModal("computeAnova", "Re-labelling", "relabelling_block_button", size="large",
                         
                         tags$head(
                           tags$style(".col1 {min-width: 100px; display: inline-block; }"),
                           tags$style(".col2 {min-width: 50px;  display: inline-block; }"),
                           tags$style(".col3 {min-width: 20px;  display: inline-block; }"),
                           tags$style(".filter-option-inner-inner .col1 {min-width: auto; 
                                    display: inline-block; visibility: hidden; }"),
                           tags$style(".filter-option-inner-inner .col2 {min-width: auto; 
                                    display: inline-block; visibility: hidden; }")
                         ),
                         fluidRow(
                           column(6,tabItem(tabName = "Labelling",
                                            helpText(paste("This tab displays the candidate labels retrieved from the vocabulary to rename the dataset columns.")),
                                            fluidRow(
                                              column(width = 12, 
                                                     tags$head(tags$style(" 
                                            .bs-select-all{display: none;}
                                            .bs-deselect-all{width: 200%;}
                                          ")),
                                                     
                                                     uiOutput("pick_col_labelling"),
                                                     dataTableOutput("content1")
                                              )
                                            )
                           )
                           
                           ), #chius column  
                           column(6,
                                  
                                  tabItem(tabName = "Label_candidate",
                                          helpText(paste("This tab displays the actually chosen candidate label.")),
                                          helpText(paste("Select only one candidate before accepting.")),
                                          fluidRow(
                                            column(width = 12,
                                                   textOutput("candidate_text"),
                                                   verbatimTextOutput("candidate_label")
                                                   
                                            )
                                          )),  
                                  
                                  fluidRow(
                                    column(1, align="right",shinyBS::bsButton("accept_lab_candidate", label="Accept",style="info")
                                    ),
                                    column(1, offset=2,align="right",shinyBS::bsButton("reject_lab_candidate", label="Reject", style="info")
                                    ),
                                    column(1, offset=2,align="right",shinyBS::bsButton("undo_relab_button", label="Undo", style="info"))
                                  ),  
                                  br(),br(),
                                  fluidRow(
                                    column(12, wellPanel(fluidRow(
                                      column(12, htmlOutput("pt_text"), uiOutput("pt_label"),
                                             fluidRow(br(),      
                                                      column(4, align="left",shinyBS::bsButton("glp_pt_button_relab", label="GLP", style="info", disabled=TRUE)),
                                                      
                                             )) 
                                    ) 
                                    ) 
                                    ))
                           )) 
                         
        ),  
        
        
        
        
        shinyBS::bsModal("computeTrend", "Duplicate removal", "dupl_removal_block_button", size="large",
                         
                         tags$head(
                           tags$style(".col1 {min-width: 100px; display: inline-block; }"),
                           
                           tags$style(".filter-option-inner-inner .col1 {min-width: auto; 
                                    display: inline-block; visibility: visible; }"),
                         ),
                         
                         fluidRow(
                           column(6,tabItem(tabName = "Dupl_Finder",
                                            helpText(paste("This tab displays the potential column duplicates present in the loaded dataset.")),
                                            fluidRow(
                                              column(width = 12,
                                                     uiOutput("pick_col_duplicates"), 
                                                     fluidRow    (
                                                       column(width = 12, 
                                                              dataTableOutput("content_dupl_remov")  
                                                       )))
                                            )
                           )
                           
                           ), #chius column
                           column(6,                 
                                  tabItem(tabName = "Duplicate_candidate_removal",
                                          
                                          uiOutput('radioDUPL'), #x<-etc uioutp
                                          
                                          div(style="display: inline-block; vertical-align:center; horizontal-align:left; margin-right:200px", 
                                              shinyBS::bsButton("submit", label="Accept",style="info"),
                                              shinyBS::bsButton("undo_dupl_button", label="Undo", style="info")
                                          ),
                                          br()
                                  ),
                                  
                                  fluidRow(br(),
                                           column(12, wellPanel(fluidRow(
                                             column(12, htmlOutput("pt_text_dupl"), uiOutput("pt_dupl"),  
                                                    fluidRow(     br(),  
                                                                  column(4, align="left",shinyBS::bsButton("glp_pt_button_dupl", label="GLP", style="info", disabled = TRUE)),
                                                    )) 
                                           ))))  
                                  
                           )
                         ) 
                         
        ),  
        
        
        
        
        
        
        #########recoding
        
        shinyBS::bsModal("findtorecode", "Content Homogenization", "recoding_button", size="large",
                         
                         tags$head(
                           tags$style(".col1 {min-width: 100px; display: inline-block; }"),
                           
                           tags$style(".filter-option-inner-inner .col1 {min-width: auto; 
                                    display: inline-block; visibility: visible; }"),
                         ),
                         fluidRow(column(12, 
                                         "This tab displays the un-curated columns with the proposed recoding." 
                         )),
                         br(),
                         
                         
                         fluidRow(column(6,
                                         box(tags$b("Current columns and contents"), width=12, 
                                             
                                             fluidRow( 
                                               column(width = 12,
                                                      br(),
                                                      selectizeInput(
                                                        inputId="pick_cols_recoding",
                                                        label="Select the column to recode you wish to display:",
                                                        choices=NULL,
                                                        selected=1, multiple=FALSE),
                                                      br(),
                                                      dataTableOutput("contentLIST_COLS")
                                               )
                                             )
                                         )
                                         
                         ), 
                         
                         
                         column(6,
                                conditionalPanel(
                                  condition= "input.pick_cols_recoding != '' ",
                                  
                                  box( tags$b("Modifiable  recoded version label and contents"),width=12,
                                       fluidRow(column(12, HTML("The dropdown menu contains all occurrences present in the selected column."))),
                                       fluidRow(
                                         column(width = 12, br(),
                                                
                                                textInput(inputId="recoded_label", "Recoded column name", value = NULL, width = NULL, placeholder = NULL), 
                                                
                                                tags$head(
                                                  tags$style(".col4 {min-width: 120px; display: inline-block; }"),
                                                  tags$style(".col5 {min-width: 80px;  display: inline-block; }"),
                                                  tags$style(".col6 {min-width: 120px;  display: inline-block; }"),
                                                  tags$style("width: 500px; }"),
                                                  
                                                ),
                                                
                                                fluidRow(
                                                  column(width = 12,
                                                         pickerInput(
                                                           inputId = "pick_recoding_content_pairs",
                                                           label = "Select the content of the column with its recoded version:",
                                                           choices = NULL,
                                                           multiple=FALSE
                                                         )
                                                  )
                                                ),
                                                
                                                
                                                fluidRow( 
                                                  column(12, textInput(inputId="recoded_content", "Recoded unique content", value = NULL, width = NULL, placeholder = NULL)       
                                                  )
                                                )))
                                  ),
                                  br(),
                                  
                                  
                                  box(width=12,
                                      wellPanel(
                                        fluidRow(
                                          column(width = 12,
                                                 
                                                 tags$b("Consult current vocabulary"),
                                                 br(),
                                                 fluidRow(column(12, 
                                                                 pickerInput(
                                                                   inputId = "pick_to_check_dict_labels",
                                                                   label = "Labels and synonyms",
                                                                   choices = NULL,
                                                                   multiple=FALSE
                                                                 )
                                                 )
                                                 ), 
                                                 
                                                 
                                                 fluidRow( 
                                                   column(12, uiOutput("pick_to_check_dict_contents")
                                                   )
                                                 ) 
                                                 
                                          ))  
                                        
                                      )),
                                  
                                  box(width=12,
                                      fluidRow(
                                        column(width = 12,
                                               fluidRow(
                                                 tags$head(tags$style("#shiny-modal img { max-width: 100%; }")),
                                                 column(12,plotOutput("barplot"))
                                               )
                                               
                                        ) 
                                      ))
                                ))
                         ),
                         
                         
                         fluidRow(  
                           column(12, align="center",
                                  br(),
                                  
                                  radioButtons('step_choice', 'Select the next step to take:', 
                                               choices = c("Delete" = 1, "Modify and/or Save" = 2, "Specials" =3) , selected = character(0), inline=TRUE),
                                  conditionalPanel(
                                    condition = "input.step_choice == '1'",    
                                    shinyBS::bsButton("step_submit_del", label="Delete!",style="info")),
                                  
                                  conditionalPanel(
                                    condition = "input.step_choice == '2'",    
                                    shinyBS::bsButton("step_submit", label="Next",style="info")),
                                  
                                  conditionalPanel(
                                    condition = "input.step_choice == '3'",    
                                    shinyBS::bsButton("step_submit_spec", label="Next",style="info"))
                           ), 
                           br()
                         ) , 
                         
                         fluidRow(
                           column(1, align="right",shinyBS::bsButton("undo_save_button", label="Undo", style="info", disabled = FALSE)
                           ))  
        ),  
        
        
        
        unclosable_modal("modalstep1", "Deletion commenting", "step_submit_del", size = "large",
                         tags$head(tags$style("#modalstep1 
                                                    .modal-footer{ display:none;}
                                                    .close {display: none;}
                                             ")),
                         fluidRow(
                           column(12, wellPanel(fluidRow(
                             column(12, htmlOutput("pt_recod_del"), uiOutput("pt_recod_del_msg"),
                                    fluidRow(br(),      
                                             column(4, align="left",shinyBS::bsButton("glp_pt_button_rec_del", label="Add", style="info",  disabled=TRUE))
                                    ))
                           )  
                           ), 
                           
                           ) 
                         ) 
                         
        ),
        
        
        
        
        
        shinyBS::bsModal("modalstep", "Recoding and storing", "step_submit", size = "large",
                         wellPanel(tabItem(tabName = "New_label_and_content",
                                           tags$strong("Curated dataset"),
                                           helpText(paste("This tab displays the label and the unique content as they were modified in the previous page and allows to update the dataset.")),
                                           fluidRow(
                                             column(3,
                                                    textOutput("new_label_text"),
                                                    textOutput("new_content_text")
                                             ),
                                             column(6,
                                                    
                                                    htmlOutput("edited_label"),
                                                    htmlOutput("edited_content")
                                             )
                                           ), br(),
                                           
                                           fluidRow(
                                             column(1, align="right", shinyBS::bsButton("save_recoding", label="Save",style="info")
                                             ), 
                                             column(3,  offset=1, align="right",shinyBS::bsButton("reject_full_recoding", label="Skip full column recoding", style="info")
                                             )
                                           ),
                         )), 
                         
                         
                         wellPanel( id="vocab_syn_decision_panel", 
                                    tabItem(tabName = "Vocabulary_label_and_contents",
                                            tags$strong("Vocabulary enrichment"),
                                            helpText(paste("Classify original label and content into the vocabulary.")),
                                            
                                            tags$head(
                                              tags$style(".col7 {min-width: 100px; display: inline-block; }"),
                                              tags$style(".col8 {min-width: 100px;  display: inline-block; }"),
                                              tags$style(".col9 {min-width: 100px; display: inline-block; }"),
                                              tags$style(".col10 {min-width: 100px;  display: inline-block; }")
                                            ),
                                            
                                            
                                            fluidRow(
                                              column(6,
                                                     htmlOutput("original_label")
                                              ),
                                              column(6, 
                                                     tags$style(
                                                       '#content_TOclean_FORvocabulary+ .plugin-selectize-plugin-a11y .has-items::after {display: none;}'
                                                     ),
                                                     selectizeInput( 
                                                       inputId="content_TOclean_FORvocabulary",
                                                       label= "List of potential content-entry",
                                                       choices=NULL,
                                                       selected = NULL,
                                                       multiple=TRUE),
                                                     
                                                     
                                                     conditionalPanel(
                                                       condition = "output.skip_marker_out == true  &&   output.content_TOclean_FORvocabulary != 'null'",
                                                       div(style = "margin-top: -15px"), 
                                                       tags$style("   .checkbox_actions_on_skip .shiny-input-container {
                                                            display: inline-block;
                                                            width: 100px;
                                                            
                                                     }"),
                                                       span(class="checkbox_actions_on_skip",
                                                            prettyCheckbox( inputId = "select_all", label = "Select All", animation="pulse", status = "success", outline = TRUE ,value=FALSE),
                                                            prettyCheckbox( inputId = "reset", label = "Reset Selection",  animation="pulse", status = "danger", outline = TRUE ,value=FALSE),
                                                       )
                                                       
                                                       
                                                       
                                                     )
                                                     
                                              )
                                            )), br(),
                                    
                                    
                                    
                                    div(style = "margin-top: -25px"),
                                    tabItem(tabName = "Storing_new_in_dict",
                                            helpText(paste("Given the main (edited) references, how to store the original label/content into an updated version of the vocabulary?")),
                                            
                                            fluidRow(
                                              column(6, radioButtons("store_labels", label = ("Labels"),
                                                                     choices = list("as Label Synonym" = 1, "Do not store"=2),
                                                                     selected = character(0))), 
                                              column(6, radioButtons("store_contents", label = ("Contents"),
                                                                     choices = list("as Content Synonym" = 3, "Do not store"=4),
                                                                     selected = character(0))   ) 
                                            ),         
                                            
                                            fluidRow(
                                              column(1, align="left",shinyBS::bsButton("store_button", label="Store",style="info", disabled = TRUE)) , 
                                            )
                                    )),  
                         
                         
                         #section PT recoding modify
                         fluidRow(
                           column(12, wellPanel(fluidRow(
                             column(12, htmlOutput("pt_text_recod"), uiOutput("pt_recod"),
                                    fluidRow(br(),      
                                             column(4, align="left",shinyBS::bsButton("glp_pt_button_rec", label="Add", style="info",  disabled=TRUE))
                                    )) 
                           )  
                           ) 
                           ))
                         
                         
                         
        ), 
        
        
        
        ### modal to work with special operations to process dataset contents during curation  
        shinyBS::bsModal("SpecialOPmodal", "Special actions", "step_submit_spec", size = "large",     
                         
                         fluidPage(
                           tags$head(tags$style(HTML(
                             "
        .navbar-nav > li > a, .navbar-brand {
                            padding-top:4px !important; 
                            padding-bottom:4px !important;
                            height: 30px;
                            }
                           .navbar {min-height:30px !important;}
      
      .navbar-default .navbar-nav > .active > a,
        .navbar-default .navbar-nav > .active > a:focus,
        .navbar-default .navbar-nav > .active > a:hover {color: white;background-color: #04AA6D;}
        
        "))),
                           
                           
                           
                           navbarPage("", id="navlist_specialOPS", collapsible=TRUE,
                                      tabPanel(id="agilent_button", "Agilent", 
                                               fluidRow(column(12, align="left",
                                                               tabItem(tabName = "agilent_function_box",
                                                                       fluidRow(column(4,align="left",
                                                                                       actionLink(inputId = "link_info_agilent", label = "Info about the function"))),
                                                                       
                                                                       #bsmodal triggered by the actionlink above
                                                                       bsModal(id = "modal-info_agilent", title = "Information about Agilent string splitting function", trigger = "link_info_agilent",
                                                                               h5("This function extracts GSM and array information (slide and area) from file_name.txt typically generated by Agilent machines and structured as follows:"),
                                                                               div(style = "margin-top: -10px"),
                                                                               strong(em(HTML("&nbsp","&nbsp","&nbsp","&nbsp","&nbsp","US11263921_257236348514_S01_GE2_1200_Jun14_2_1.txt")) ), 
                                                                               h5("The selected column will be splitted into columns named GSM, slide and array."),
                                                                               
                                                                               tags$head(tags$style("#modal-info_agilent .modal-footer{ display:none}"))
                                                                       ), 
                                                                       
                                                                       
                                                                       fluidRow(br()),
                                                                       div(style="display: inline-block; vertical-align:center; horizontal-align:left; margin-right:300px",
                                                                           shinyBS::bsButton("test_agil_button", label="Test",style="info",  disabled=FALSE),
                                                                           shinyBS::bsButton("save_agil_button", label="Save",style="info",  disabled=TRUE),
                                                                           shinyBS::bsButton("reset_agitest_table", label="Reset",style="info", disabled=TRUE)
                                                                       ),
                                                                       
                                                                       
                                                                       fluidRow(br()),
                                                                       fluidRow(box(
                                                                         column(12, align="left", 
                                                                                htmlOutput("colname_to_showAGIL"),
                                                                                br(),
                                                                                dataTableOutput("col_to_showAGIL")
                                                                         )
                                                                         
                                                                       ) ,
                                                                       box(
                                                                         column(12, align="left",
                                                                                div(style = "margin-top: +36px"),
                                                                                dataTableOutput("table_AGI_splitted_cols")
                                                                         )
                                                                       )
                                                                       ), 
                                                                       
                                                                       wellPanel(fluidRow(
                                                                         column(12, htmlOutput("pt_text_AGIsplit"), uiOutput("pt_AGIsplit"),br(), 
                                                                                fluidRow(       
                                                                                  column(4, align="left",shinyBS::bsButton("glp_pt_button_AGIsplit", label="Add", style="info", disabled = TRUE))
                                                                                  
                                                                                )) 
                                                                       ) 
                                                                       )        
                                                               )))
                                      ),
                                      
                                      
                                      
                                      
                                      tabPanel(id="add_cols_button", "Adding empty columns",
                                               fluidRow(column(12,align="left",
                                                               box(width="100%",
                                                                   fluidRow( 
                                                                     column(12, align="left",
                                                                            fixedRow(
                                                                              column(6, align="left",
                                                                                     numericInput(inputId = "new_cols", label= HTML("Column(s) to add"), min=0, value=0) ),
                                                                              column(6,   div(style = "margin-top: +25px"),
                                                                                     shinyBS::bsButton("add_Fempty_cols_button", label="Add",style="info",  disabled=FALSE) )
                                                                            ))    
                                                                     
                                                                     
                                                                   ),
                                                                   
                                                                   conditionalPanel(
                                                                     condition= "input.new_cols > 0",
                                                                     fluidRow(
                                                                       column(6, align="left",
                                                                              textInput(inputId = "names_added_cols","Name new column(s)", placeholder = "Please insert new column names separated only by a comma"),
                                                                       ))      
                                                                   )
                                                               ), 
                                                               
                                                               wellPanel(fluidRow(
                                                                 column(12, htmlOutput("pt_text_addEmpty"), uiOutput("pt_addEmpty"),br(), 
                                                                        fluidRow(       
                                                                          column(4, align="left",shinyBS::bsButton("glp_pt_button_addEmpty", label="Add", style="info", disabled = TRUE))
                                                                          
                                                                        ))
                                                               )  
                                                               )
                                               ))
                                      ),
                                      
                                      
                                      
                                      tabPanel(id="splitting_button", "Splitting column",
                                               box(width="100%",
                                                   div(style = "margin-top: -10px"),
                                                   checkboxInput("other_col_split", label = "Select manually another specific column", value = FALSE),
                                                   
                                                   
                                                   conditionalPanel(condition= "input.other_col_split == 1 ",
                                                                    fluidRow(
                                                                      
                                                                      column(6, align="left", 
                                                                             selectizeInput("select_col_df_SPLIT", "Select a column",  choices=NULL, selected = NULL, multiple = FALSE,
                                                                                            options = list(placeholder= "columns from loaded dataframe"))
                                                                      )
                                                                    )
                                                   ),
                                                   
                                                   
                                                   
                                                   #definition of CSS class for button of radiogroupbuttons
                                                   tags$style(HTML("
                      .btn-radioGROUPclass.btn {
                      color: #fff;
                      background-color: #33b5e5;
                      border: 2px #5bc0de solid;
                      }
                      .btn-radioGROUPclass.btn:hover {
                      color: #fff;
                      background-color: #04AA6D;
                      }
                      .btn-radioGROUPclass.active {
                      color: #fff;
                      background-color: #04AA6D;
                      border-color: #007E33;
                      }
                 ")),
                                                   
                                                   fixedRow(
                                                     column(5, align="left",  
                                                            radioGroupButtons(
                                                              inputId = "type_of_splitter",
                                                              label = "Splitting method",
                                                              choices = c("Separator", "Regular Expression"),
                                                              selected=character(0),
                                                              status = "radioGROUPclass", 
                                                              checkIcon = list(
                                                                yes = icon("check-square"),
                                                                no = icon("fas fa-square-o")
                                                              ) 
                                                            )),
                                                     column(3, div(style = "margin-top: +34px; margin-left: -60px;",
                                                                   actionLink(inputId = "link_info_splitting", label = "Info about the splitting")),
                                                            
                                                            bsModal(id = "modal-info_splitting", title = "Information about splitting column cell content", trigger = "link_info_splitting",
                                                                    h5("This window allows to split the content of the selected column by means of a separator or a regular expression."), 
                                                                    div(style = "margin-top: 20px"),
                                                                    h5("The option based on separator is more suitable for simple splitting, while regular expression offers more complex solutions. The regular expression splitting is based on tidyr::extract() function."),
                                                                    div(style = "margin-top: -10px"),
                                                                    h5("Due to the higher complexity, the user has the possibility to test if the set regular expression works properly before effectively modify the data and save the result.")
                                                            ), 
                                                            
                                                            
                                                     )),
                                                   conditionalPanel(condition= "input.type_of_splitter =='Separator' ",
                                                                    
                                                                    fluidRow(
                                                                      column(5,
                                                                             selectizeInput("type_of_separator", "Type of separator:",choices= c("Tab", ",", ";", "Space","", "Other"), selected = character(0),multiple=FALSE,
                                                                                            
                                                                                            options = list(
                                                                                              placeholder = 'Please select an option below',
                                                                                              onInitialize = I('function() { this.setValue(""); }'))           
                                                                             )         
                                                                      ),
                                                                      
                                                                      
                                                                      column(7, 
                                                                             conditionalPanel(condition= "input.type_of_separator =='Other' ",
                                                                                              
                                                                                              div(style = "margin-top: +5px"),
                                                                                              textInput(inputId = "other_sep","", placeholder = "Please insert the customized separator")
                                                                                              
                                                                             )   
                                                                      )  
                                                                    ),   
                                                                    
                                                                    fluidRow(
                                                                      column(5, div(style = "margin-top: -12px"),
                                                                             checkboxInput("name_splitted_cols", label = "Name splitted columns", value = FALSE)
                                                                      ), 
                                                                      column(7,
                                                                             conditionalPanel(condition= "input.name_splitted_cols == 1 ",
                                                                                              div(style = "margin-top: -28px"),
                                                                                              textInput(inputId = "names_split_cols","", placeholder = "Please insert column names separated only by a comma")   
                                                                             )  
                                                                      ) 
                                                                    ), 
                                                                    
                                                                    div(style="display: inline-block; vertical-align:center; horizontal-align:left; margin-right:200px", 
                                                                        shinyBS::bsButton("test_separ", label="Test",style="info", disabled=FALSE),
                                                                        shinyBS::bsButton("run_separ", label="Separate",style="info",  disabled=FALSE),
                                                                        shinyBS::bsButton("reset_test_table", label="Reset test table",style="info",  disabled=TRUE)
                                                                    )
                                                   ), 
                                                   
                                                   conditionalPanel(condition= "input.type_of_splitter =='Regular Expression' ",
                                                                    textInput(inputId = "regex_splitting","Regular expression", placeholder = "Please insert the regular expression"), 
                                                                    fluidRow(column(5,
                                                                                    textInput(inputId = "names_split_cols_regex","New column names", placeholder = "Please insert column names separated only by a comma"),
                                                                    )),
                                                                    fluidRow(column(12,
                                                                                    div(style="display: inline-block; vertical-align:center; horizontal-align:left; margin-right:200px",  
                                                                                        shinyBS::bsButton("test_separ_regex", label="Test",style="info",  disabled=FALSE),
                                                                                        shinyBS::bsButton("run_separ_regex", label="Separate",style="info", disabled=FALSE),
                                                                                        shinyBS::bsButton("reset_test_table_regex", label="Reset test table",style="info", disabled=TRUE)
                                                                                    ))
                                                                    )
                                                   ),
                                               ),   
                                               
                                               
                                               fluidRow(br()),
                                               fluidRow(box(
                                                 column(12, align="left", 
                                                        htmlOutput("colname_to_showSPLIT"),
                                                        br(),
                                                        dataTableOutput("col_to_showSPLIT")
                                                 )
                                                 
                                               ) ,
                                               box(
                                                 column(12, align="left", 
                                                        htmlOutput("test_colname_to_showSPLIT"),
                                                        br(),
                                                        dataTableOutput("test_splitted_cols")
                                                 )
                                                 
                                               ),
                                               
                                               ),  
                                               
                                               
                                               wellPanel(fluidRow(
                                                 column(12, textAreaInput(inputId="PT_store_split", "Procedural track", value = "Waiting for action!", width = "100%",height = "100%", placeholder = NULL), 
                                                        fluidRow(       
                                                          column(4, align="left",shinyBS::bsButton("glp_pt_button_split", label="Add", style="info", disabled=TRUE)),
                                                          
                                                        ))
                                               )
                                               )   
                                      )
                                      
                           )  
                           
                         ) 
                         
        ), 
        
        
        
        
        shinyBS::bsModal("importGxModal", "Import Vocabulary", "import_voc_submit", size="large",
                         
                         fluidRow(
                           column(3,fileInput("gx", label="File")),
                         ),fluidRow(
                           column(12, align="right",shinyBS::bsButton("upload_voc_submit", label="Import", style="info"))
                         )    
        ),   
        shinyBS::bsModal("importPhenoModal", "Import Phenodata", "import_pheno_submit", size="large", 
                         conditionalPanel(
                           condition = "input.working_mode == 1",
                           fluidRow(column(3, fileInput("fPheno", label="Phenodata File")),
                                    column(3, uiOutput("selSep")),
                                    column(3, textInput("sepT", "Other Separator", value=":"))#,
                           ),
                           fluidRow(
                             column(3,actionButton("load_pheno_submit", "Preview")),
                             column(2, align="left", textOutput("phRowsText"),textOutput("phColsText"))
                           ),hr(),
                           fluidRow(
                             column(12,
                                    hidden(div(id="phenoPreviewDiv",
                                               fluidRow(
                                                 column(12,
                                                        rhandsontable::rHandsontableOutput("phenoTypesRH")
                                                 )
                                               ),hr(),
                                               fluidRow(column(12, align="right",shinyBS::bsButton("upload_pheno_submit", label="Import", style="info")))
                                    ))
                             )
                           )
                         ),
                         conditionalPanel(
                           condition = "input.working_mode == 0",
                           fluidRow(column(3, fileInput("multi_fPheno", label="Multiple Phenodata File", multiple=TRUE))
                                    #    column(3, uiOutput("selSep")),
                                    #    column(3, textInput("sepT", "Other Separator", value=":"))#,
                                    #column(3, uiOutput("selQuote"))
                           ),
                           fluidRow(column(12,align="left",
                                           div(style="display: inline-block; vertical-align:center; horizontal-align:left; margin-right:200px", #class = "row-fluid",
                                               shinyBS::bsButton("upload_multi_pheno_submit", label="Import", style="info"),
                                               shinyBS::bsButton("go_to_multi_tool_integ_button", label="Open Integration Support", style="info")
                                           ))  
                           ),
                           hr(),
                           
                           fluidRow(
                             column(12,
                                    DT::DTOutput("multifiles"))),
                           br(),br(),
                           
                           conditionalPanel(
                             condition= "input.multifiles_rows_selected != null",
                             fluidRow(
                               column(12,
                                      dataTableOutput("selected_df_from_multi")))
                           )   
                         ) 
        ), 
        shinyBS::bsModal("showDownload_modal", "Analysis Reports", "showFinalDownload", size = "large",
                         conditionalPanel(
                           condition = "input.working_mode == 1 | output.working_mode_scenario == 1",    #single dataset
                           fluidRow(column(12, align="left",
                                           shinyBS::bsButton("download_single_rpt", label="Download All",style="info"),
                                           downloadButton("download_Upd_PhdataV2", label="Upd_ph", class="downloadbutton_tohide"), 
                                           downloadButton("download_Upd_VocabularyV2",label="Upd_voc",class="downloadbutton_tohide"),
                                           downloadButton("download_Upd_GLPv2",label="Upd_pt_GLP",class="downloadbutton_tohide"),
                                           downloadButton("download_to_review_issue2",label="Upd_voc_Iss",class="downloadbutton_tohide"),
                                           downloadButton("download_to_review_discard2",label="Upd_voc_Disc",class="downloadbutton_tohide"),
                                           downloadButton("download_to_review_accepted2",label="Upd_voc_Acc",class="downloadbutton_tohide"),
                                           
                                           tags$head(tags$style(" 
                                                                .downloadbutton_tohide{
                                                                              background-color: white;
                                                                              color: white;
                                                                              border: white;
                                                                              border: 1px solid white;
                                                                              pointer-events: none;
                                                                }
                                                                .downloadbutton_tohide:hover {
                                                                              background-color: red;
                                                                              color: white;
                                                                              border: white;
                                                                              border: 1px solid white;
                                                                }              
                                                                
                                                                ")),
                                           br()
                           )),
                           
                           
                           fluidRow(column(12, align="center",
                                           h3(tags$b("Single File Curation Reports"))#,br()
                           )),
                           fluidRow(column(12, align="center",
                                           br(),
                                           tags$head(   
                                             tags$style(HTML(" 
                                           .btn-reportGROUPclass.btn.checkbtn {
                                                  border-radius: 10px !important;
                                                  border: none;
                                                  opacity: 1;
                                                  pointer-events: none;
                                            }
                                           .btn-reportGROUPclass.btn {
                                                  background: red !important;
                                                  color: white !important;
                                                  border: red !important;
                                            }
                                          .btn-reportGROUPclass.btn.active {
                                                  background: #04AA6D !important;
                                                  color: white !important;
                                                  border: #04AA6D !important;
                                            }" ))),
                                           checkboxGroupButtons(
                                             inputId = "button_main_report",
                                             choices = c("Main Report", "Current Session",
                                                         "Updated Phenodata", "Updated Vocabulary", "Procedures/GLP"), 
                                             status = "reportGROUPclass",
                                             size = "normal",
                                             individual = TRUE,
                                             disabled=FALSE,
                                             
                                             checkIcon = list(
                                               yes = icon("check", 
                                                          lib = "glyphicon"),
                                               no = icon("square"))
                                           ))), 
                           
                           
                           fluidRow(column(12,align="center",
                                           tags$head(
                                             uiOutput("css_checkbox")
                                           ),
                                           checkboxGroupButtons(
                                             inputId = "button_voc_report",
                                             choices = c("Voc. Issue","Voc. Discard","Voc. Accepted"),
                                             status = "custom-class2",
                                             size = "normal",
                                             individual = TRUE,
                                             disabled=FALSE, 
                                             checkIcon = list(
                                               yes = icon("check", 
                                                          lib = "glyphicon"),
                                               no = icon("square"))
                                           )) ),  
                           br(),
                           fluidRow(column(12, align="center",
                                           progressBar(
                                             id = "mainS",
                                             value = 0,
                                             title = "Main Report",
                                             display_pct = TRUE,
                                             status = "custom"
                                           ),
                                           progressBar(
                                             id = "updSession",
                                             value = 0,
                                             title = "Current Session",
                                             display_pct = TRUE,
                                             status = "custom"
                                           ),
                                           progressBar(
                                             id = "updPHdt",
                                             value = 0,
                                             title = "Updated Phenodata",
                                             display_pct = TRUE,
                                             status = "custom"
                                           ),
                                           
                                           progressBar(
                                             id = "updVoc",
                                             value = 0,
                                             title = "Updated Vocabulary",
                                             display_pct = TRUE,
                                             status = "custom"
                                           ),
                                           progressBar(
                                             id = "updPT_GLP",
                                             value = 0,
                                             title = "Procedures/GLP",
                                             display_pct = TRUE,
                                             status = "custom"
                                           )   #NEXT reports
                           ),
                           tags$style(".progress-bar-custom {background-color: #25c484;}")
                           ),
                           conditionalPanel('output.panelStatus_Vissue==true',
                              fluidRow(column(12, align="center",
                                           progressBar(
                                             id = "upd_voc_issue",
                                             value = 0,
                                             title = "Issue Report",
                                             display_pct = TRUE,
                                             status = "custom"
                                           )
                           ))),
                         conditionalPanel('output.panelStatus_Vdiscard==true',
                              fluidRow(column(12, align="center",
                                         progressBar(
                                           id = "upd_voc_discard",
                                           value = 0,
                                           title = "Discard Report",
                                           display_pct = TRUE,
                                           status = "custom"
                                         )
                         ))),
                        conditionalPanel('output.panelStatus_Vaccept==true',
                        fluidRow(column(12, align="center",
                                        progressBar(
                                          id = "upd_voc_accepted",
                                          value = 0,
                                          title = "Accepted Report",
                                          display_pct = TRUE,
                                          status = "custom"
                                        )
        )))
        ),
        conditionalPanel(
               condition = "input.working_mode == 0 | output.working_mode_scenario == 0",   #multiple case
                           
               fluidRow(column(12, align="left",
                               shinyBS::bsButton("download_multi_rpt", label="Download All",style="info"),
                               downloadButton("download_Upd_PhdataV2multi", label="Upd_ph", class="downloadbutton_tohide"), 
                                 downloadButton("download_Upd_GLPv2multi",label="Upd_pt_GLP",class="downloadbutton_tohide"),
                                  downloadButton("download_multi_issueV2",label="Upd_multi_Iss",class="downloadbutton_tohide"),
                                  downloadButton("download_multi_consistentV2",label="Upd_multi_Consis",class="downloadbutton_tohide"),
                               
                               tags$head(tags$style(" 
                                                                .downloadbutton_tohide{
                                                                              background-color: white;
                                                                              color: white;
                                                                              border: white;
                                                                              border: 1px solid white;
                                                                              pointer-events: none;
                                                                }
                                                                .downloadbutton_tohide:hover {
                                                                              background-color: red;
                                                                              color: white;
                                                                              border: white;
                                                                              border: 1px solid white;
                                                                }              
                                                                
                                                                ")),
                               br()
               )),
               
               
               fluidRow(column(12, align="center",
                               h3(tags$b("Multiple Curated Files Integration  Reports")) 
               )),
               fluidRow(column(12, align="center",
                               br(),
                               tags$head(   
                                 tags$style(HTML(" 
                                           .btn-reportGROUPclass.btn.checkbtn {
                                                  border-radius: 10px !important;
                                                  border: none;
                                                  opacity: 1;
                                                  pointer-events: none;
                                            }
                                           .btn-reportGROUPclass.btn {
                                                  background: red !important;
                                                  color: white !important;
                                                  border: red !important;
                                            }
                                          .btn-reportGROUPclass.btn.active {
                                                  background: #04AA6D !important;
                                                  color: white !important;
                                                  border: #04AA6D !important;
                                            }" ))),
                               checkboxGroupButtons(
                                 inputId = "button_main_report_multi",
                                 choices = c("Main Report", "Current Session",
                                             "Integrated Phenodata", "Procedures/GLP"), 
                                 status = "reportGROUPclass",
                                 size = "normal",
                                 individual = TRUE,
                                 disabled=FALSE,
                                 
                                 checkIcon = list(
                                   yes = icon("check", 
                                              lib = "glyphicon"),
                                   no = icon("square"))
                               ))), 
               
               
               fluidRow(column(12,align="center",
                               tags$head(
                                 uiOutput("css_checkbox_multi")
                               ),
                               checkboxGroupButtons(
                                 inputId = "button_voc_report_multi",
                                 choices = c("Issue Entry","Consistent Entry"),
                                 status = "custom-class2",
                                 size = "normal",
                                 individual = TRUE,
                                 disabled=FALSE, 
                                 checkIcon = list(
                                   yes = icon("check", 
                                              lib = "glyphicon"),
                                   no = icon("square"))
                               )) ),  
               br(),
               fluidRow(column(12, align="center",
                               progressBar(
                                 id = "mainS_multi",
                                 value = 0,
                                 title = "Main Report",
                                 display_pct = TRUE,
                                 status = "custom"
                               ),
                               progressBar(
                                 id = "updSession_multi",
                                 value = 0,
                                 title = "Current Session",
                                 display_pct = TRUE,
                                 status = "custom"
                               ),
                               progressBar(
                                 id = "updPHdt_multi",
                                 value = 0,
                                 title = "Integrated Phenodata",
                                 display_pct = TRUE,
                                 status = "custom"
                               ),
                               
                               progressBar(
                                 id = "updPT_GLP_multi",
                                 value = 0,
                                 title = "Procedures/GLP",
                                 display_pct = TRUE,
                                 status = "custom"
                               )   #NEXT reports
               ),
               tags$style(".progress-bar-custom {background-color: #25c484;}")
               ),
               conditionalPanel('output.panelStatus_Missue==true',
                                fluidRow(column(12, align="center",
                                                progressBar(
                                                  id = "upd_multi_issue",
                                                  value = 0,
                                                  title = "Issue Entry Report",
                                                  display_pct = TRUE,
                                                  status = "custom"
                                                )
                                ))),
               conditionalPanel('output.panelStatus_Mconsistent==true',
                                fluidRow(column(12, align="center",
                                                progressBar(
                                                 id = "upd_multi_consistent",
                                                  value = 0,
                                                  title = "Consistent Entry Report",
                                                  display_pct = TRUE,
                                                  status = "custom"
                                                )
                                )))
               
                  
                         ),
        br(),
        fluidRow(column(12, align="left",shinyBS::bsButton("reset_window_reports", label="Reset Window Settings",style="info"),
                 downloadButton("exportRpt", "Only_Analysis_Rpt", class="downloadbutton_tohide"))) 
                         
        ),
        
        fluidRow(column(12,
                        tabBox(id="display", title="", width=12, 
                               tabPanel(value="pdTab", title="Phenodata",  
                                        conditionalPanel(
                                          condition = "input.working_mode == 1 | output.working_mode_scenario == 1",
                                          fluidRow(column(12, br(),
                                                          DT::dataTableOutput("filtered")))
                                        ),
                                        conditionalPanel(
                                          condition = "input.working_mode == 0 | output.working_mode_scenario == 0", 
                                          fluidRow(column(12,br(),
                                                          DT::dataTableOutput("multidf_table")))
                                        )
                               ),
                               
                               
                               tabPanel(value="gExpTab", title="Vocabulary",
                                        fluidRow(column(12,br(),
                                                        DT::dataTableOutput("gExpMat")))
                               ),
                               tabPanel(value="Upd_Voc_Tab",  title="Updated Vocabulary",
                                        conditionalPanel(                            
                                          condition="input.working_mode == 0 | output.working_mode_scenario == 0",
                                          fluidRow(column(12, align="center", br(),br(),
                                                          htmlOutput("disabledTab"), br(),br(),
                                          ))
                                        ),
                                        
                                        conditionalPanel(                            
                                          condition="input.working_mode == 1 | output.working_mode_scenario == 1",   
                                          
                                          
                                          
                                          #authorize vocab modifications
                                          wellPanel(
                                            fluidRow(
                                              column(2, align="left",shinyBS::bsButton("update_pre_VOCstorage_button", label="Load new terms suggestions",style="info")), 
                                            ),br(),
                                            fluidRow(
                                              tags$head(
                                                tags$style("
                                                                  #outcomes_dictXcandidates ~ .selectize-control .option {
                                                                  font-weight: bold;
                                                                  }
                                                      
                                                                  #outcomes_dictXcandidates ~ .selectize-control .option[data-value = 'Safe'], 
                                                                                                                    .item[data-value = 'Safe'] {
                                                                  color: #04AA6D;
                                                                  }
                                                                  
                                                                  #outcomes_dictXcandidates ~ .selectize-control .option[data-value = 'Fast check'],
                                                                                                                    .item[data-value = 'Fast check'] {
                                                                  color: #FFC107;
                                                                  }
                                                                  
                                                                  #outcomes_dictXcandidates ~ .selectize-control .option[data-value = 'Slow check'],
                                                                                                                    .item[data-value = 'Slow check']{
                                                                  color: #DC3545;
                                                                  }
                                                                  .selectize-input  .item[data-value = 'Safe']    {font-weight: bolder !important; color:  #04AA6D !important;}
                                                                  .selectize-input  .item[data-value = 'Fast check'] {font-weight: bolder !important; color:  #FFC107 !important;}
                                                                  .selectize-input  .item[data-value = 'Slow check'] {font-weight: bolder !important; color:  #DC3545 !important;}
                                                                 
                                                        
                                                        "
                                                )
                                              ), 
                                              
                                              column(4, 
                                                     selectizeInput(
                                                       inputId="outcomes_dictXcandidates",
                                                       label="Potential outcome:",
                                                       choices=NULL,#c("Safe","Fast check","To overview"),
                                                       multiple=FALSE,
                                                       options = list(
                                                         placeholder = 'Please select an option below',
                                                         onInitialize = I('function() { this.setValue(""); }')))
                                                     
                                              ))), 
                                          
                                          
                                          fluidRow(column(12,
                                                          conditionalPanel(
                                                            condition= "output.for_retrieved_voc_entry == false & output.left_outcomes == true", 
                                                            
                                                            wellPanel(strong("Correspondent entry retrieved from the dictionary"),
                                                                      
                                                                      fluidRow(    
                                                                        column(12,DT::dataTableOutput("retrieved_from_dict"))
                                                                      ))))), 
                                          
                                          fluidRow(column(12,
                                                          conditionalPanel( condition="output.left_outcomes == true", 
                                                                            
                                                                            
                                                                            wellPanel(strong("Suggestions from curation"),
                                                                                      fluidRow(br()),
                                                                                      fluidRow
                                                                                      (column(12,
                                                                                              
                                                                                              
                                                                                              DT::dataTableOutput("dt_toshow")),
                                                                                        br()         
                                                                                      )))
                                          )
                                          ),
                                          
                                          tags$style('.btn{ margin-right: 15px;}'),  # add the spacing
                                          
                                          fluidRow(column(12,
                                                          div(style="display: inline-block; vertical-align:center; horizontal-align:left; margin-right:100px", 
                                                              shinyBS::bsButton("update_To_review", label="Store to issues",style="info", disabled = TRUE) ,
                                                              shinyBS::bsButton("update_To_discard", label="Discard Entry",style="info", disabled = TRUE) ,
                                                              shinyBS::bsButton("update_voc_button", label="Store Entry",style="info", disabled = TRUE) ,
                                                              shinyBS::bsButton("generate_dict_updated_version", label="Generate Updated Vocabulary",style="info", disabled = TRUE) ,
                                                          ))
                                                   
                                          ),   br(), 
                                          
                                          
                                          fluidRow(column(12,
                                                          div(style="display: inline-block; vertical-align:center; horizontal-align:left; margin-right:100px", 
                                                              downloadButton("download_to_review_issue", label="Download Issue Report") ,
                                                              downloadButton("download_to_review_discard", label="Download Discard Report") ,
                                                              downloadButton("download_to_review_accepted", label="Download Stored Report") 
                                                          ))
                                          ),  br(), 
                                          
                                          fluidRow(
                                            column(2, align="left", downloadButton("download_Upd_Vocabulary", "Download Updated Vocabulary")
                                                  
                                            )),
                                          
                                          br(),  
                                          
                                          
                                        )
                               ),
                               tabPanel(value="Upd_Pheno_Tab", title="Updated Phenodata",
                                        conditionalPanel(                            
                                          condition="input.working_mode == 0 | output.working_mode_scenario == 0",
                                          fluidRow(br(),
                                                   column(1, align="left", downloadButton("download_Int_Phdata", "Download Integrated Phenodata"))
                                                          
                                          )
                                        ),
                                        
                                        conditionalPanel(                            
                                          condition="input.working_mode == 1 | output.working_mode_scenario == 1",   
                                          fluidRow(
                                            column(1, align="left",shinyBS::bsButton("update_df_button", label="Update",style="info")) ,
                                            column(2,offset=1,downloadButton("download_Upd_Phdata", "Download"))
                                          ), br(),
                                          fluidRow(column(12,DT::DTOutput("upd_phenodf")))
                                        )), 
                               
                               
                               tabPanel(value="PT_GLPTab", title="Procedures/GLP",
                                        
                                        fluidRow(
                                          column(1, align="left",shinyBS::bsButton("update_glp_button", label="Update to Download",style="info", disabled=TRUE)) ,
                                          column(2, offset=2, align="left", uiOutput("activate_download"))
                                        ), 
                                        fluidRow(), br(),
                                        fluidRow(column(12,DT::DTOutput("glp_report")))
                               ),
                               
                               
                               
                               
                               tabPanel(value="plot_page", title="Plotting Tool",
                                        tags$head(
                                          tags$style(HTML(".help-block  a{color: black ;}"))
                                        ),
                                        
                                        fluidRow( column(6, align="left",
                                                         tabItem(tabName = "plotting_parameters", br(),
                                                                 radioButtons(
                                                                   "checkboxSINGLE_MULTIphdata", label = "Type of phenodata", 
                                                                   choices = list("Single"="single", "Multiple"="multiple"), selected = character(0) ,inline = TRUE, width = '100%')),
                                                         
                                                         fluidRow(column(6, align="left",
                                                                         
                                                                         radioButtons(
                                                                           "checkboxBAR_TILEplot", label = "Type of plot", 
                                                                           choices = list("Barplot"="barplot", "Tileplot"="tileplot"), selected = character(0) ,inline = TRUE, width = '100%'))),
                                                         
                                                         fluidRow(br()),
                                                         
                                                         fluidRow( column(6, align="left",
                                                                          helpText(a (paste("Select the variables you wish to show: "))))),
                                                         
                                                         fluidRow(column(6, align="left",
                                                                         
                                                                         tabItem(tabName = "tabmultiX",
                                                                                 selectizeInput(
                                                                                   inputId="multi_colnamesX",
                                                                                   label="Abscissa",
                                                                                   choices=NULL,
                                                                                   selected=1,
                                                                                   multiple=TRUE,
                                                                                   options = list(
                                                                                     maxItems=1,
                                                                                     placeholder = 'Please select an option below',
                                                                                     onInitialize = I('function() { this.setValue(""); }'))),    
                                                                         ),
                                                                         #div(style = "margin-top: -20px"),
                                                                         #uiOutput( "class_x"),
                                                                         #div(style = "margin-top: 12px")
                                                         )), 
                                                         
                                                         fluidRow(    column(6, align="left",
                                                                             tabItem(tabName = "tabmultiY",
                                                                                     selectizeInput(
                                                                                       inputId="multi_colnamesY",
                                                                                       label="Ordinate",
                                                                                       choices=NULL,
                                                                                       selected=character(0),
                                                                                       multiple=TRUE,
                                                                                       options = list(
                                                                                         maxItems=1,
                                                                                         placeholder = 'Please select an option below',
                                                                                         onInitialize = I('function() { this.setValue(""); }'),persist=F)),
                                                                             ),
                                                                             
                                                                        #     div(style = "margin-top: -20px"),
                                                                        #     uiOutput( "class_y"),
                                                                        #     div(style = "margin-top: 12px")
                                                         )),
                                                         
                                                         fluidRow(  column(6, align="left",  
                                                                           tabItem(tabName = "tabmultiCONDITION",
                                                                                   selectizeInput(
                                                                                     inputId="multi_colnamesCOND",
                                                                                     label="Grouping condition",
                                                                                     choices="",
                                                                                     selected=1,
                                                                                     multiple=TRUE,
                                                                                     options = list(
                                                                                       maxItems=1,
                                                                                       placeholder = 'Please select an option below',
                                                                                       onInitialize = I('function() { this.setValue(""); }')))
                                                                           ),
                                                                        #   div(style = "margin-top: -20px"),
                                                                         #  uiOutput("class_cond"),
                                                                        # div(style = "margin-top: 12px")
                                                         )),
                                                         
                                                         
                                                         
                                                         fluidRow(br()),
                                                         
                                                         fluidRow( column(6, align="left",
                                                                          radioButtons("type_plot_relation", label = "Type of data relation in the plot:", 
                                                                                       choices = list("Stacked"="stacked", "Grouped"="dodge","No condition"="nocond"),
                                                                                       selected = character(0) ,inline = TRUE, width = '100%'))
                                                         ),   
                                                         
                                                         
                                                         fluidRow(br()),
                                                         fluidRow(column(6,align="left", tags$strong(helpText(a(paste("Plotting customization:")))),  
                                                                         checkboxInput("type_value_barplot", label = "y-axis values as Percent", value=FALSE),
                                                                         checkboxInput("remove_legend", label = "Remove legend", value=FALSE),
                                                                         textInput("x_axis_label", label= "X axis label", width = "100%"),
                                                                         textInput("y_axis_label", label= "Y axis label", width = "100%"),
                                                                         textInput("cond_legend_label", label= "Legend label", width = "100%")
                                                         )
                                                         ),  
                                                         
                                                         
                                                         div(style="display: inline-block; vertical-align:center; horizontal-align:left; margin-right:200px",  
                                                             shinyBS::bsButton("plot_multipic", label="Plot",style="info", disabled=TRUE),
                                                             shinyBS::bsButton("reset_plot_param", label="Reset Selections",style="info", disabled=FALSE)
                                                         )
                                        ),
                                        
                                        
                                        column(6, align="left",
                                               
                                               tabItem(tabName = "plot_barplot_section",
                                                       plotlyOutput("multiplot", height  = "400px"),
                                               ))
                                        )   
                               ),
                               
                               
                               
                               tabPanel(value="multiple_integration_tool_page",  title="Multiple Integration Tool Check",
                                        tags$head(
                                          tags$style(HTML(".help-block  a{color: black ;}"))
                                        ),
                                        
                                        conditionalPanel(                            
                                          condition="input.working_mode == 1 | output.working_mode_scenario == 1",
                                          fluidRow(column(12, align="center", br(),br(),
                                                          htmlOutput("disabledTab_multi"), br(),br(),
                                          ))
                                        ),
                                        
                                        conditionalPanel(                            
                                          condition="input.working_mode == 0 | output.working_mode_scenario == 0",   
                                          
                                          
                                          fluidRow(
                                            tags$head(
                                              tags$style("
                                                                  #multijoin_outcomes ~ .selectize-control .option {
                                                                  font-weight: bold;
                                                                  }
                                                      
                                                                  #multijoin_outcomes ~ .selectize-control .option[data-value = 'Safe'], 
                                                                                                                    .item[data-value = 'Safe'] {
                                                                  color: #04AA6D;
                                                                  }
                                                                  
                                                                  #multijoin_outcomes ~ .selectize-control .option[data-value = 'Fast check'],
                                                                                                                    .item[data-value = 'Fast check'] {
                                                                  color: #FFC107;
                                                                  }
                                                                  
                                                                  #multijoin_outcomes ~ .selectize-control .option[data-value = 'Slow check'],
                                                                                                                    .item[data-value = 'Slow check']{
                                                                  color: #DC3545;
                                                                  }
                                                                  .selectize-input  .item[data-value = 'Safe']    {font-weight: bolder !important; color:  #04AA6D !important;}
                                                                  .selectize-input  .item[data-value = 'Fast check'] {font-weight: bolder !important; color:  #FFC107 !important;}
                                                                  .selectize-input  .item[data-value = 'Slow check'] {font-weight: bolder !important; color:  #DC3545 !important;}
                                                  "
                                              )
                                            ), 
                                            br(),
                                            column(6,
                                                   tabItem(tabName = "Multi_tabitem_left",
                                                           fluidRow( 
                                                             column(12,    
                                                                    selectizeInput(
                                                                      inputId="multijoin_outcomes",
                                                                      label="Potential outcome:",
                                                                      choices=NULL, 
                                                                      multiple=FALSE,
                                                                      options = list(
                                                                        placeholder = 'Please select an option below',
                                                                        onInitialize = I('function() { this.setValue(""); }')))
                                                             )),
                                                           br(),
                                                           
                                                           fluidRow(
                                                             column(12,
                                                                    DT::dataTableOutput("columns_multi_selected_group")),
                                                             br()
                                                           )
                                                   )), 
                                            
                                            
                                            column(6,
                                                   tabItem(tabName = "Multi_tabitem_right",
                                                           br(),br(),
                                                           fluidRow(column(12,  
                                                                           div(style="display: inline-block; vertical-align:center; horizontal-align:left; margin-right:200px",
                                                                               shinyBS::bsButton("accept_multi_button", label="Consistent",style="info", disabled = TRUE) ,
                                                                               shinyBS::bsButton("issue_multi_button", label="Issue",style="info", disabled =TRUE) ,
                                                                               shinyBS::bsButton("undo_multi_button", label="Undo",style="info", disabled =FALSE)  
                                                                           ))),
                                                           
                                                           
                                                           column(12,
                                                                  fluidRow(
                                                                    br(),
                                                                    div(style = "margin-bottom: 7px"),
                                                                    column(12,DT::dataTableOutput("dt_toshow_multi_selected_group")),
                                                                    br(),
                                                                    
                                                                  )
                                                           ))
                                            )            
                                          ),
                                          
                                          br(),
                                          wellPanel(
                                            fluidRow(
                                              column(12, htmlOutput("pt_text_MULTI"), uiOutput("pt_MULTI"), 
                                                     fluidRow(       br(),
                                                                     column(4, align="left",shinyBS::bsButton("glp_pt_button_multi", label="Add", style="info", disabled = TRUE)),
                                                     )
                                              ))), 
                                          br(),
                                          
                                          fluidRow(column(12,
                                                          div(style="display: inline-block; vertical-align:center; horizontal-align:left; margin-right:100px", 
                                                              downloadButton("download_multi_issue", label="Download Issue Report") ,
                                                              downloadButton("download_multi_consistent", label="Download Consistent Report") 
                                                          ))
                                          ),  br(), 
                                          
                                          
                                        ) 
                               ) 
                        )
        ))
        
      )
    )
  )
}
