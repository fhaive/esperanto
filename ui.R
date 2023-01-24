suppressMessages(library(shiny))
suppressMessages(library(shinyjs))
suppressMessages(library(shinyBS))
suppressMessages(library(shinydashboard))
suppressMessages(library(shinyFiles))
suppressMessages(library(DT))
suppressMessages(library(rhandsontable))
suppressMessages(library(shinycssloaders))
library(plotly)
library(xtable)
library(shinyhelper)
library(shinyFeedback)
library(shinyWidgets)
#library(shinyThings)

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


ui <- function(request) {
fluidPage(
 # useShinyFeedback(),
  useShinyjs(),
  inlineCSS(appCSS), 
  hidden(div(id="loading-content",
             img(id="loading-gif", src="screen-loading.gif"),
             p(id="loadingText", "WORKING"),
             p("...")
  )),
  
  
  shinyWidgets::useSweetAlert(),
 # shinyalert::useShinyalert(),

  dashboardPage(
    dashboardHeader(title="ESPERANTO: sEmi-SuPERvised toxicogenomics meta-dAta curatioN TOol", titleWidth="60%"),
    dashboardSidebar(disable=FALSE,
                     bsCollapse(id="bsSidebar1", open="LOAD PHENODATA",
                                bsCollapsePanel("LOAD PHENODATA", style="warning",
                                                fluidRow(
                                                  column(12, align="center",
                                                         shinyBS::bsButton("import_pheno_submit", label="Import PhenoData", style="danger",icon=icon("exclamation-circle"), disabled=TRUE),
                                                         shinyBS::bsTooltip("import_pheno_submit", "Launch a graphical window, to configure import of phenodata from a file!", placement="bottom")
                                                  )
                                                )
                                ),
                                bsCollapsePanel("LOAD VOCABULARY", style="warning",
                                                fluidRow(
                                                  column(12, align="center",
                                                         shinyBS::bsButton("import_expr_submit", label="Import Vocabulary", style="danger", icon=icon("exclamation-circle")),
                                                         shinyBS::bsTooltip("import_expr_submit", "Launch a graphical window, to configure import of a vocabulary from a file!", placement="bottom")
                                                         
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
                                                         shinyBS::bsButton("relabelling_block_button", label="Re-Labelling", style="danger", icon=icon("exclamation-circle")),
                                                         shinyBS::bsTooltip("relabelling_block_button", "Launch a graphical window, to show the pairs of suggested labels coded by vocabulary for the dataset columns!", placement="bottom")
                                                  )
                                                ),
                                                fluidRow(
                                                  column(12, align="center",
                                                         shinyBS::bsButton("dupl_removal_block_button", label="Duplicate Removal", style="danger", icon=icon("exclamation-circle")),
                                                         shinyBS::bsTooltip("dupl_removal_block_button", "Launch a graphical window, to show the candidate columns for duplicate removal!", placement="bottom")
                                                  )
                                                ),
                                                
                                                fluidRow(
                                                  column(12, align="center",
                                                         shinyBS::bsButton("recoding_button", label="Content Homogenization", style="danger", icon=icon("exclamation-circle")),
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
                                                         shinyBS::bsTooltip("launch_reset_modal", "Launch a graphical window, to change the currently set working mode", placement="bottom")    
                                                         
                                                  )
                                                )
                                )#,chius bscollpase
                     ),
                     
                     bsCollapse(id="bsSidebar0", open="MORE INFO",
                                bsCollapsePanel("DOWNLOAD REPORT",style="sample",
                                                fluidRow(
                                                  column(12, align="center",
                                                         downloadButton("exportRpt", "Analysis Report")
                                                                           
                                                  )
                                                )            
                                ),
                                bsCollapsePanel("MORE INFO", style="sample",
                                                fluidRow(
                                                  HTML(" <a style=color:blue;  target=\"_blank\"; href=\"https://github.com/Greco-Lab/BMDx\">GitHub</a>")
                                                ),
                                                fluidRow(
                                                  HTML("<a style=color:blue; target=\"_blank\";  href=\"Manual.pdf\">Manual</a>")
                                                ),
                                                fluidRow(
                                                  HTML("<a style=color:blue;  target=\"_blank\"; href=\"https://github.com/Greco-Lab/BMDx/blob/master/pheno_list_2_exp_4_TP.xlsx\">Sample pheno data</a>")
                                                ),
                                                fluidRow(
                                                  HTML("<a style=color:blue;  target=\"_blank\"; href=\"https://github.com/Greco-Lab/BMDx/blob/master/exp_mat_file_2_exp_4_tp.xlsx\">Sample expression data</a>")
                                                )
                                )
                     )
    ),
    dashboardBody(
    #  shinyBS::bsModal("enrichPathways", "Final", "enrich_button", size="large",
     # ),
    
      
      
      
      
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
                                # helpText(paste("The decision to authorize the change should be taken one pair at the time by pushing 'Accept' button")),
                                 fluidRow(
                                   column(width = 12,
                                          tags$head(tags$style("
                                            .bs-select-all{display: none;}
                                            .bs-deselect-all{width: 200%;
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
                               )), #chius fluidrow,
                               
                               fluidRow(
                               column(1, align="right",shinyBS::bsButton("accept_lab_candidate", label="Accept",style="info", icon=icon("hand-o-right"))
                               ),#3acol
                               column(1, offset=2,align="right",shinyBS::bsButton("reject_lab_candidate", label="Reject", style="info", icon=icon("hand-o-right"))
                               ),#4acol
                               column(1, offset=2,align="right",shinyBS::bsButton("undo_relab_button", label="Undo", style="info", icon=icon("hand-o-right")))
                       ), #chius fluidrow
                       br(),br(),
                       fluidRow(
                       column(12, wellPanel(fluidRow(
                         column(12, htmlOutput("pt_text"), uiOutput("pt_label"),
                          fluidRow(br(),      
                            column(4, align="left",shinyBS::bsButton("glp_pt_button_relab", label="GLP", style="info", icon=icon("hand-o-right"), disabled=TRUE)),
                           
                         ))# fluidRow and column
                       ) #fluidRow
                       )#wellpanel
                       ))
                               
                               
                               
                        
                       ))#chiusura nuovacol
                       
                       
      ),  #chius bsmodal
      
      
      
      
      ######
      shinyBS::bsModal("computeTrend", "Duplicate removal", "dupl_removal_block_button", size="large",
                       
                       tags$head(
                         tags$style(".col1 {min-width: 100px; display: inline-block; }"),
                #         tags$style(".col2 {min-width: 50px;  display: inline-block; }"),
                #         tags$style(".col3 {min-width: 20px;  display: inline-block; }"),
                #         
                        tags$style(".filter-option-inner-inner .col1 {min-width: auto; 
                                    display: inline-block; visibility: visible; }"),
                #         tags$style(".filter-option-inner-inner .col2 {min-width: auto; 
                #                    display: inline-block; visibility: hidden; }")
                       ),
                
  
                
                
                
                
                
                       fluidRow(
                         column(6,tabItem(tabName = "Dupl_Finder",
                                          helpText(paste("This tab displays the potential column duplicates present in the loaded dataset.")),
                                      #     helpText(paste("The decision to authorize the change should be taken one group at the time by pushing 'Accept' button")),
                                    #  br(),
                                    #  br(),
                                    #  br(),
                                           fluidRow(
                                             column(width = 12,
                                                   uiOutput("pick_col_duplicates"), 
                                               fluidRow    (
                                    column(width = 12, 
                                             dataTableOutput("content_dupl_remov")  
                                   )))
                                           )
                         )
                                
                                
                          
                                
                                
                                
                                
                                
                                
                               # )
                                      #      )
                                      #     )
                        #  )
              #           
                         ), #chius column
                         column(6,
              #                  
                               tabItem(tabName = "Duplicate_candidate_removal",
                                       #helpText(paste("If possible, the suggestion is to prioritize keeping columns with names labelled according the allowed labels reported in the vocabulary.")),
                                        #fluidRow(
                                        #  column(width = 6,
                                         uiOutput('radioDUPL'), #x<-etc uioutp
              
              div(style="display: inline-block; vertical-align:center; horizontal-align:left; margin-right:200px", 
                 shinyBS::bsButton("submit", label="Accept",style="info", icon=icon("hand-o-right")),
                 shinyBS::bsButton("undo_dupl_button", label="Undo", style="info", icon=icon("hand-o-right"))
              ),
              br()
                       ),
              
             #  
              fluidRow(br(),
                column(12, wellPanel(fluidRow(
                  column(12, htmlOutput("pt_text_dupl"), uiOutput("pt_dupl"), #textAreaInput(inputId="PT_dupl_input", "Procedural track", value = "...", width = "100%",height = "100%",rows=9, placeholder = NULL), 
                         fluidRow(     br(),  
                           column(4, align="left",shinyBS::bsButton("glp_pt_button_dupl", label="GLP", style="info", icon=icon("hand-o-right"),disabled = TRUE)),
                          # column(4,shinyBS::bsTooltip("glp_pt_button_dupl", "Launch a graphical window, to enrich the procedural track info with comments according to GLP regulations.", placement="bottom")), 
                           
                        #   column(4, align="left",shinyBS::bsButton("add_pt_dupl", label="Add", style="info", icon=icon("hand-o-right")))
                         ))# fluidRow and column
                ) #fluidRow
                )#wellpanel
                ) 
                )
                               )
              
              
              
                
            
                       ) #chius fluidrow
                       
    ),  #chius bsmodal
      
      
      
    
    
    
    #########recoding
    ######
    
    shinyBS::bsModal("findtorecode", "Content Homogenization", "recoding_button", size="large",
                     
                     tags$head(
                       tags$style(".col1 {min-width: 100px; display: inline-block; }"),
                       #         tags$style(".col2 {min-width: 50px;  display: inline-block; }"),
                       #         tags$style(".col3 {min-width: 20px;  display: inline-block; }"),
                       #         
                       tags$style(".filter-option-inner-inner .col1 {min-width: auto; 
                                    display: inline-block; visibility: visible; }"),
                       #         tags$style(".filter-option-inner-inner .col2 {min-width: auto; 
                       #                    display: inline-block; visibility: hidden; }")
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
                       
                       )#box
                  
                       ), #chius column
                 
                       
                  column(6,
                         conditionalPanel(
                           condition= "input.pick_cols_recoding != '' ",
                         
                       box( tags$b("Modifiable  recoded version label and contents"),width=12,
                             fluidRow(column(12, HTML("The dropdown menu contains all occurrences present in the selected column."))),
                                       fluidRow(
                                         column(width = 12, br(),
                                                
                                     htmlOutput("recoded_lab_value"),
                                       
                                       tags$head(
                                         tags$style(".col4 {min-width: 120px; display: inline-block; }"),
                                         tags$style(".col5 {min-width: 80px;  display: inline-block; }"),
                                         tags$style(".col6 {min-width: 120px;  display: inline-block; }"),
                                         tags$style("width: 500px; }"),
                                         
                                         #       tags$style(".filter-option-inner-inner .col4 {min-width: auto; 
                                         #  display: inline-block; visibility: hidden; }"),
                                         #       tags$style(".filter-option-inner-inner .col5 {min-width: auto; 
                                         #  display: inline-block; visibility: hidden; }")
                                       ),
                                  
                                       fluidRow(
                                                            column(width = 12,
                                                                   #uiOutput("pick_recoding_content_pairs"),
                                                                   pickerInput(
                                                                     inputId = "pick_recoding_content_pairs",
                                                                     label = "Select the content of the column with its recoded version:",
                                                                     choices = NULL,
                                                                     multiple=FALSE
                                                                   )
                                             #                      dataTableOutput("content1")    htmlOutput("recoded_content_value")
                                                            )
                                                          ),
                                         
                                         
                                  fluidRow( 
                                    column(12, htmlOutput("recoded_content_value")
                                     )# column
                                  )))
                                  ), #fluidrow
                                  
                                  
                                  
                                  
                                  
                                  
                                
                                  br(),
                          #     tabItem(tabName = "medium-right",
                       box(width=12,
                                       wellPanel(
                                       fluidRow(
                                         column(width = 12,
                                                # tabItem("qeuq", fluidRow(column(12,
                                    tags$b("Consult current vocabulary"),
                                    br(),
                                      fluidRow(column(12, #uiOutput("pick_to_check_dict_labels")
                                                      pickerInput(
                                                        inputId = "pick_to_check_dict_labels",
                                                        label = "Labels and synonyms",
                                                        choices = NULL,
                                                        multiple=FALSE
                                                       )
                                    )# column
                                  ), #fluidrow
                                  
                                  
                                  fluidRow( 
                                    column(12, uiOutput("pick_to_check_dict_contents")
                                    )# column
                                  ) #fluidrow
                                  
                                  ))  #wp
                                  
                        )),
                                  
                                  
                       # tabItem(tabName = "down-right",
                       box(width=12,
                                fluidRow(
                                  column(width = 12,
                                         fluidRow(
                                    tags$head(tags$style("#shiny-modal img { max-width: 100%; }")),
                                    #column(12,plotOutput('barplot_contents'))
                                    column(12,plotOutput('barplot'))
                                  )
                                  
                                 ) # chius col
                                ))
                               ))
                  ),
                       
                       
                              fluidRow(  
                                # y<-uiOutput("radioOPTIONShomogenization"),
                                column(12, align="center",
                                       #"Cols_toRecode",
                                       br(),
                                     #  uiOutput("radioOPTIONShomogenization"),
                                     radioButtons('step_choice', 'Select the next step to take:', 
                                                                   choices = c("Delete" = 1, "Modify and/or Save" = 2, "Specials" =3) , selected = character(0), inline=TRUE),
                                     conditionalPanel(
                                       condition = "input.step_choice == '1'",    
                                     shinyBS::bsButton("step_submit_del", label="Next",style="info", icon=icon("hand-o-right"))),
                                     
                                     conditionalPanel(
                                       condition = "input.step_choice == '2'",    
                                       shinyBS::bsButton("step_submit", label="Next",style="info", icon=icon("hand-o-right"))),
                                     
                                     conditionalPanel(
                                       condition = "input.step_choice == '3'",    
                                       shinyBS::bsButton("step_submit_spec", label="Next",style="info", icon=icon("hand-o-right")))
                                       ), 
                                
                                
                                 
                                 br()
                              ) , 
                                
                              fluidRow(
                                       column(1, align="right",shinyBS::bsButton("undo_save_button", label="Undo", style="info", icon=icon("hand-o-right"))
                                       ))  
                               
                       
    ),  #chius bsmodal
    
    
    
  
  
  shinyBS::bsModal("modalstep1", "Deletion commenting", "step_submit_del", size = "large",
                     
                     
                     fluidRow(
                       column(12, wellPanel(fluidRow(
                         column(12, htmlOutput("pt_recod_del"), uiOutput("pt_recod_del_msg"),
                                fluidRow(br(),      
                                         column(4, align="left",shinyBS::bsButton("glp_pt_button_rec_del", label="Add", style="info", icon=icon("hand-o-right"), disabled=TRUE))
                                         #**#
                                ))# fluidRow and column
                       ) #fluidRow
                       ), #wellpanel
                       
                       align="left",shinyBS::bsButton("undo_del_button", label="Undo", style="info", icon=icon("hand-o-right"), disabled=TRUE)
                       
                       ) 
                     
                     
                   )), #condpanel_del
                   
  
    
    
    
    
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
                                     column(1, align="right", shinyBS::bsButton("save_recoding", label="Save",style="info", icon=icon("hand-o-right"))
                                     ),#3acol
                                    column(3,  offset=1, align="right",shinyBS::bsButton("reject_full_recoding", label="Reject full column recoding", style="info", icon=icon("hand-o-right"))
                                    )
                                    ),
                                )),# chius tavItem and wellPanel
                     
                     
                            wellPanel(tabItem(tabName = "Vocabulary_label_and_contents",
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
                                     #textOutput("new_label_text"),
                                     htmlOutput("original_label")
                              ),
                              column(6,
                                     
                                     #htmlOutput("edited_label"),
                                     htmlOutput("original_content")
                              )
                            )), br(),
                            
                            
                            
                    
                    tabItem(tabName = "Storing_new_in_dict",
                            helpText(paste("Given the main (edited) references, how to store the original label/content into an updated version of the vocabulary?")),
                         
                            fluidRow(
                              column(6, uiOutput("radiostoreLAB")),
                              column(6, uiOutput("radiostoreCONT"))
                            ),         
                                
                              fluidRow(
                                column(1, align="left",shinyBS::bsButton("store_button", label="Store",style="info", icon=icon("hand-o-right"),disabled = TRUE)) ,
                               # column(2,offset=1,align="right",shinyBS::bsButton("undo_upd_button", label="Undo", style="info", icon=icon("hand-o-right"),disabled = TRUE))
                                ),
                    
                          br(),fluidRow()
                    
          )), #chius tabitem and wellpanel
        
           #section PT recoding modify
          
          fluidRow(
            column(12, wellPanel(fluidRow(
              column(12, htmlOutput("pt_text_recod"), uiOutput("pt_recod"),
                     fluidRow(br(),      
                              column(4, align="left",shinyBS::bsButton("glp_pt_button_rec", label="Add", style="info", icon=icon("hand-o-right"), disabled=TRUE))
                              ))# fluidRow and column
            ) #fluidRow
            )#wellpanel
            ))
          
        
    
    ),#chius 2fin bsmodal
      
  
      
  
  
  
  
  
      
    
    
    
    
    
    # modal to work with special operations to process dataset contents during curation  
 shinyBS::bsModal("SpecialOPmodal", "Special actions", "step_submit_spec", size = "large",    #"to_trigger_special_ops_modal",  "runfunc_button"  special_op_button
                  
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
                                                 #div(style = "margin-top: 20px"),
                                                 h5("The selected column will be splitted into columns named GSM, slide and array."),
                                                 
                                                 tags$head(tags$style("#modal-info_agilent .modal-footer{ display:none}"))
                                         ), # modal-info_agilent
                                         
                                         
                                         
                                         
                                         
                                         
                                         fluidRow(br()),
                                      div(style="display: inline-block; vertical-align:center; horizontal-align:left; margin-right:300px",
                                            shinyBS::bsButton("test_agil_button", label="Test",style="info", icon=icon("hand-o-right"), disabled=FALSE),
                                            shinyBS::bsButton("save_agil_button", label="Save",style="info", icon=icon("hand-o-right"), disabled=TRUE),
                                            shinyBS::bsButton("reset_agitest_table", label="Reset",style="info", icon=icon("hand-o-right"), disabled=TRUE)
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
                                                    column(4, align="left",shinyBS::bsButton("glp_pt_button_AGIsplit", label="Add", style="info", icon=icon("hand-o-right"), disabled = TRUE))
                                                    
                                                  ))# fluidRow
                                         ) #fluidRow
                                         ),#wellpanel
                                      fluidRow(column(6,align="left",
                                                 #     shinyBS::bsButton("undo_agil_button", label="Undo",style="info", icon=icon("hand-o-right"), disabled=TRUE)
                                      )),
                                         
                                             
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
                                                          shinyBS::bsButton("add_Fempty_cols_button", label="Add",style="info", icon=icon("hand-o-right"), disabled=FALSE) )
                                             ))   #fixedrow, column and fluidRow
                                           
                                       
                                       ),
                                        
                                       conditionalPanel(
                                         condition= "input.new_cols > 0",
                                         fluidRow(
                                           column(6, align="left",
                                                  textInput(inputId = "names_added_cols","Name new column(s)", placeholder = "Please insert new column names separated only by a comma"),
                                           ))#frow and column         
                                       )#condpan
                                       ),#box
                                       
                    wellPanel(fluidRow(
                                   column(12, htmlOutput("pt_text_addEmpty"), uiOutput("pt_addEmpty"),br(), 
                                          fluidRow(       
                                            column(4, align="left",shinyBS::bsButton("glp_pt_button_addEmpty", label="Add", style="info", icon=icon("hand-o-right"), disabled = TRUE))
                                            
                                          ))# fluidRow
                                 ) #fluidRow
                                 ),#wellpanel
                                 fluidRow(column(6,align="left",
                                            
                                 ))               
                                   
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
                              no = icon("square-o")
                              ) 
                 )),
              column(3, div(style = "margin-top: +34px; margin-left: -60px;",
                 actionLink(inputId = "link_info_splitting", label = "Info about the splitting")),
                 
                 #bsmodal triggered by the actionlink above
                 bsModal(id = "modal-info_splitting", title = "Information about splitting column cell content", trigger = "link_info_splitting",
                         h5("This window allows to split the content of the selected column by means of a separator or a regular expression."), 
                         div(style = "margin-top: 20px"),
                         h5("The option based on separator is more suitable for simple splitting, while regular expression offers more complex solutions."),
                         div(style = "margin-top: -10px"),
                         h5("Due to the higher complexity, the user has the possibility to test if the set regular expression works properly before effectively modify the data and save the result.")
                 ), # modal-info_splitting
                 
                 
                 
                 
              )),
                 conditionalPanel(condition= "input.type_of_splitter =='Separator' ",
                                  
                                fluidRow(
                                 # div(style="display: inline-block; vertical-align:center; horizontal-align:left; margin-right:200px", class = "row-fluid",
                                      
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
                                 
                                )   #condpan "other"
                                                 
                                                                 
                                )  #column condpan "other" 
                                ),  #fluidrow
                                
                                fluidRow(
                                  column(5, div(style = "margin-top: -12px"),
                                    checkboxInput("name_splitted_cols", label = "Name splitted columns", value = FALSE)
                                  ), #col
                                  column(7,
                                    conditionalPanel(condition= "input.name_splitted_cols == 1 ",
                                                     div(style = "margin-top: -28px"),
                                      textInput(inputId = "names_split_cols","", placeholder = "Please insert column names separated only by a comma"),
                                      
                                    ) #condpan
                                  ) #col
                                ),#frow
                                
                                div(style="display: inline-block; vertical-align:center; horizontal-align:left; margin-right:200px", #class = "row-fluid",
                                    shinyBS::bsButton("test_separ", label="Test",style="info", icon=icon("hand-o-right"), disabled=FALSE),
                                    shinyBS::bsButton("run_separ", label="Separate",style="info", icon=icon("hand-o-right"), disabled=FALSE),
                                    shinyBS::bsButton("reset_test_table", label="Reset test table",style="info", icon=icon("hand-o-right"), disabled=TRUE)
                                )
                                ), 
                 
                 conditionalPanel(condition= "input.type_of_splitter =='Regular Expression' ",
                                  textInput(inputId = "regex_splitting","Regular expression", placeholder = "Please insert the regular expression"),
                                # br(),
                                 fluidRow(column(5,
                                                 textInput(inputId = "names_split_cols_regex","New column names", placeholder = "Please insert column names separated only by a comma"),
                                 )),
                                 fluidRow(column(12,
                                   div(style="display: inline-block; vertical-align:center; horizontal-align:left; margin-right:200px", #class = "row-fluid",
                                       shinyBS::bsButton("test_separ_regex", label="1Test",style="info", icon=icon("hand-o-right"), disabled=FALSE),
                                       shinyBS::bsButton("run_separ_regex", label="1Separate",style="info", icon=icon("hand-o-right"), disabled=FALSE),
                                       shinyBS::bsButton("reset_test_table_regex", label="1Reset test table",style="info", icon=icon("hand-o-right"), disabled=TRUE)
                                   ))
                                 )
                 ),
                 ),  #chius box
                 
                 
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
                                  
                                  ), #1st fluidrow
                  
          
            wellPanel(fluidRow(
              column(12, textAreaInput(inputId="PT_store_split", "Procedural track", value = "Waiting for action!", width = "100%",height = "100%", placeholder = NULL), 
                     fluidRow(       
                       column(4, align="left",shinyBS::bsButton("glp_pt_button_split", label="Add", style="info", icon=icon("hand-o-right"),disabled=TRUE)),
                  
                     ))
            )
            ),
            
           #1 shinyBS::bsButton("undo_norm_regex_split_button", label="Undo",style="info", icon=icon("hand-o-right"), disabled=TRUE)
             
        )
        
      ),  
      
    ) 
                                 
    ), 
    
    
    
    
      shinyBS::bsModal("importGxModal", "Import Vocabulary", "import_expr_submit", size="large",
                       
                       
                       fluidRow(
                         column(3,fileInput("gx", label="File")),
                       ),fluidRow(
                         column(12, align="right",shinyBS::bsButton("upload_voc_submit", label="Import", style="info", icon=icon("hand-o-right")))
                       )    
      ),   
      shinyBS::bsModal("importPhenoModal", "Import Phenodata", "import_pheno_submit", size="large",
                conditionalPanel(
                  condition = "input.working_mode == 1",
                         fluidRow(column(3, fileInput("fPheno", label="Phenodata File")),
                                column(3, uiOutput("selSep")),
                                column(3, textInput("sepT", "Other Separator", value=":"))#,
                                #column(3, uiOutput("selQuote"))
                                ),fluidRow(
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
                                           fluidRow(
                                      #       column(4, uiOutput("selSampleIDCol")),
                                      #       column(4, uiOutput("selDoseCol")),
                                     #        column(4, uiOutput("selTPCol"))
                                           ),fluidRow(column(12, align="right",shinyBS::bsButton("upload_pheno_submit", label="Import", style="info", icon=icon("hand-o-right"))))
                                ))
                         )
                       )
      ),#condpan
                conditionalPanel(
                  condition = "input.working_mode == 0",
                  fluidRow(column(3, fileInput("multi_fPheno", label="Multiple Phenodata File", multiple=TRUE))
                           #    column(3, uiOutput("selSep")),
                           #    column(3, textInput("sepT", "Other Separator", value=":"))#,
                           #column(3, uiOutput("selQuote"))
                  ),
                  fluidRow(column(12,align="left",
                      div(style="display: inline-block; vertical-align:center; horizontal-align:left; margin-right:200px", #class = "row-fluid",
                        shinyBS::bsButton("upload_multi_pheno_submit", label="Import", style="info", icon=icon("hand-o-right")),
                        shinyBS::bsButton("go_to_multi_tool_integ_button", label="Open Integration Support", style="info", icon=icon("hand-o-right"))
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
                  
                  
                  
               #   fluidRow(
                #    column(12, align="center",
                 #          shinyBS::bsButton("import_multi_pheno_submit", label="Import Multiple PhenoData", style="danger",icon=icon("exclamation-circle")),
                  #         shinyBS::bsTooltip("import_multi_pheno_submit", "Launch a graphical window, to configure import of multiple phenodata files!", placement="bottom")
                    #  )
                    #  )
                  
                )#closing condpan
                ), #chius tabpan
      fluidRow(column(12,
                      tabBox(id="display", title="", width=12,
                             #123456789
                             tabPanel(value="pdTab", title="Phenodata", 
                                      conditionalPanel(
                                        condition = "input.working_mode == 1 | output.working_mode_scenario == 1",
                                        fluidRow(column(12,DT::dataTableOutput("filtered")))
                                        ),
                                      conditionalPanel(
                                        condition = "input.working_mode == 0 | output.working_mode_scenario == 0", 
                                        fluidRow(column(12,DT::dataTableOutput("multidf_table")))),
                                       # disable()
                                      ),
                                     
                             
                             tabPanel(value="gExpTab", title="Vocabulary",
                                      fluidRow(column(12,DT::dataTableOutput("gExpMat")))
                             ),
                             tabPanel(value="Upd_Voc_Tab",  title="Updated Vocabulary",
                                      #authorize vocab modifications
                                      
                                      
                                        wellPanel(
                                              fluidRow(
                                                    column(2, align="left",shinyBS::bsButton("update_pre_VOCstorage_button", label="Load new terms suggestions",style="info", icon=icon("hand-o-right"))), 
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
                                                                  
                                                                  #outcomes_dictXcandidates ~ .selectize-control .option[data-value = 'To overview'],
                                                                                                                    .item[data-value = 'To overview']{
                                                                  color: #DC3545;
                                                                  }
                                                                  .selectize-input  .item[data-value = 'Safe']    {font-weight: bolder !important; color:  #04AA6D !important;}
                                                                  .selectize-input  .item[data-value = 'Fast check'] {font-weight: bolder !important; color:  #FFC107 !important;}
                                                                  .selectize-input  .item[data-value = 'To overview'] {font-weight: bolder !important; color:  #DC3545 !important;}
                                                                 
                                                        
                                                        "
                                                      )
                                                    ),
                                            #.option[data-value=Safe], .item[data-value=Safe]{background: red !important;color: blue !important;}
                                        #.selectize-input  .item[data-value=Safe]{font-weight: bold; color: blue !important;}
                                        
                                                column(4,
                                                  
                                                   selectizeInput(
                                                     inputId="outcomes_dictXcandidates",
                                                     label="Potential outcome:",
                                                     choices=NULL,#c("Safe","Fast check","To overview"),
                                                     multiple=FALSE,
                                                     options = list(
                                                       placeholder = 'Please select an option below',
                                                       onInitialize = I('function() { this.setValue(""); }')))
                                                   
                                                   
                                         ))), #closing first wellpanel
                                        
                                        
                                      fluidRow(column(12,
                                                      conditionalPanel(
                                                        condition= "output.for_retrieved_voc_entry == false & output.left_outcomes == true", 
                                                      
                                         wellPanel(strong("Correspondent entry retrieved from the dictionary"),
                                           
                                                
                                                    
                                                fluidRow(    
                                                column(12,DT::dataTableOutput("retrieved_from_dict"))
                                             ))))), #chiusura wellpanel2
                                               
                                      
                                      fluidRow(column(12,
                                                      conditionalPanel( condition="output.left_outcomes == true", 
                                                    
                                                    
                                                    wellPanel(strong("Suggestions from curation"),
                                               fluidRow
                                                    (column(12,DT::dataTableOutput("dt_toshow")),
                                                    br(),
                                                    
                                         
                                                      
                                             )))
                                             )#condpan
                                             ),
                                           
                                         
                                                   
                                          
                                      tags$style('.btn{ margin-right: 15px;}'),  # add the spacing
                                          
                                      fluidRow(column(12,
                                        div(style="display: inline-block; vertical-align:center; horizontal-align:left; margin-right:100px", 
                                        shinyBS::bsButton("update_To_review", label="Store to issues",style="info", icon=icon("hand-o-right")) ,
                                        shinyBS::bsButton("update_To_discard", label="Discard Entry",style="info", icon=icon("hand-o-right")) ,
                                        shinyBS::bsButton("update_voc_button", label="Store Entry",style="info", icon=icon("hand-o-right")) ,
                                        shinyBS::bsButton("generate_dict_updated_version", label="Generate Updated Vocabulary",style="info", icon=icon("hand-o-right")) ,
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
                                        
                                        
                                    
                             ),
                             tabPanel(value="Upd_Pheno_Tab", title="Updated Phenodata",
                                      fluidRow(
                                               column(1, align="left",shinyBS::bsButton("update_df_button", label="Update",style="info", icon=icon("hand-o-right"))) ,
                                        column(2,offset=1,downloadButton("download_Upd_Phdata", "Download"))
                                      ), br(),
                                     fluidRow(column(12,DT::DTOutput("upd_phenodf")))
                             ), 
                             
                             
                          tabPanel(value="PT_GLPTab", title="Procedures/GLP",
                                   
                                   fluidRow(
                                           column(1, align="left",shinyBS::bsButton("update_glp_button", label="Update to Download",style="info", icon=icon("hand-o-right"), disabled=TRUE)) ,
                                           column(2, offset=2, align="left", uiOutput("activate_download"))
                                    ), 
                                   fluidRow(), br(),
                                   fluidRow(column(12,DT::DTOutput("glp_report")))
                                   ),
                          
                         
                             
                          
                          
                          
                          
                          
                          
                          
                          #BMDTab2 Modif_overview
                          #BMDTabs Modification Overview
                          tabPanel(value="single_ops_undo", title="Undo Single Modification",
                              conditionalPanel(                            
                                condition="input.working_mode == 0 | output.working_mode_scenario == 0",
                                  fluidRow(column(12, align="center", br(),br(),
                                           htmlOutput("disabledTab"), br(),br(),
                                ))
                              ),
                              conditionalPanel(                            
                                condition="input.working_mode == 1 | output.working_mode_scenario == 1",     
                                   fluidRow(
                                     column(width = 12,
                                            htmlOutput("total_modifications")
                                            
                                     )
                                   ),br(),

                                   fluidRow(column(12,
                                      bsCollapse(id="Tot_modif_sidebar",# open="Single column analysis",
                                                                                                       bsCollapsePanel("Relabelling phase", style="primary",
                                                                                                                       column(3,htmlOutput("total_relab_steps")), column(4,htmlOutput("only_relab_steps")), column(5,htmlOutput("only_relabRJ_steps")),
                                                                                                                       fluidRow(),br(),
                                                                                                                       
                                                                                                                       fluidRow(
                                                                                                                         
                                                                                                                         
                                                                                                                         column(width = 6,
                                                                                                                                
                                                                                                                                        selectizeInput(
                                                                                                                                          inputId="pick_relab_modif",
                                                                                                                                          label="Type of modification:",
                                                                                                                                          choices=c("Accepting relabelling proposal","Rejecting relabelling proposal"),
                                                                                                                                          multiple=FALSE,
                                                                                                                                          options = list(
                                                                                                                                            placeholder = 'Please select an option below',
                                                                                                                                            onInitialize = I('function() { this.setValue(""); }'))),
                                                                                                                                
                                                                                                                                
                                                                                                                                
                                                                                                                                
                                                                                                                                fluidRow(
                                                                                                                                  column(12,
                                                                                                                                         conditionalPanel(
                                                                                                                                           condition = "input.pick_relab_modif == 'Accepting relabelling proposal' || input.pick_relab_modif == 'Rejecting relabelling proposal'",
                                                                                                                                           
                                                                                                                                           
                                                                                                                                           #           column(width = 6,
                                                                                                                                           selectizeInput(
                                                                                                                                             inputId= "relab_ops_done",
                                                                                                                                             label="Performed modification:",
                                                                                                                                             choices=NULL,
                                                                                                                                             multiple=FALSE,
                                                                                                                                             options = list(
                                                                                                                                               placeholder = 'Please select an option below',
                                                                                                                                               onInitialize = I('function() { this.setValue(""); }'))),
                                                                                                                                           
                                                                                                                                           #   ),
                                                                                                                                           
                                                                                                                                        
                                                                                                                                           checkboxInput("checkbox_show_relab_cols", label = "Show columns", value = FALSE),
                                                                                                                                           
                                                                                                                                         )
                                                                                                                                        
                                                                                                                                         ,#condPan e fluidrow
                                                                                                                                        
                                                                                                                                          
                                                                                                                                         fluidRow(
                                                                                                                                           column(3, shinyBS::bsButton("undo_relab_ops", label="Undo", style="info", icon=icon("hand-o-right"),disabled = TRUE))
                                                                                                                                         )
                                                                                                                                  )#tabitem ecolumn   
                                                                                                                                )   
                                                                                                                                
                                                                                                                                
                                                                                                                                
                                                                                                                         ),
                                                                                                                         column(width = 5,
                                                                                                                              conditionalPanel(  
                                                                                                                                condition=" input.checkbox_show_relab_cols== 1",
                                                                                                                                dataTableOutput("operated_relab_cols_content")
                                                                                                                              )#condpan
                                                                                                                         )#column
                                                                                                                         ), # e fluidrow
                                                                                                                                        
                                                                                                                      
                                                                                                                       
                                                                                                                         
                                                                                                  
                                                                                                                       
                                                                                                                                shinycssloaders::withSpinner(plotlyOutput("BMD_dist_TP"), type = 6)
                                                                                                       ),
                                                                                                       bsCollapsePanel("Duplicate Removal phase", style="primary",
                                                                                                                       
                                                                                                                       fluidRow(
                                                                                                                       column(3,htmlOutput("total_dupl_steps") ),
                                                                                                                       ),# column(4,htmlOutput("only_relab_steps")), column(5,htmlOutput("only_relabRJ_steps")),
                                                                                                                       
                                                                                                                       fluidRow(),br(),
                                                                                                                       
                                                                                                                       fluidRow(
                                                                                                                         column(width = 6,
                                                                                                                              tabItem(tabName =  "left_col_selectors",
                                                                                                                                
                                                                                                                                 
                                                                                                                                column(12, 
                                                                                                                                       selectizeInput(inputId = "dupl_ops_done",
                                                                                                                                                      label="Performed modification:",
                                                                                                                                                      choices = NULL,
                                                                                                                                                      multiple=FALSE,
                                                                                                                                                      selected=character(0),
                                                                                                                                                      options = list(
                                                                                                                                                        placeholder = 'Please select an option below',
                                                                                                                                                        onInitialize = I('function() { this.setValue(""); }'))),
                                                                                                                                       conditionalPanel(
                                                                                                                                         condition= "input.dupl_ops_done !== ''" ,
                                                                                                                                            textOutput("dupl_kept_text"),
                                                                                                                                            verbatimTextOutput("kept"),
                                                                                                                                
                                                                                                                                            textOutput("dupl_deleted_text"),
                                                                                                                                            verbatimTextOutput("deleted"),
                                                                                                                                
                                                                                                                                            checkboxInput("checkbox_show_dupl_cols", label = "Show columns", value = FALSE)
                                                                                                                                        )
                                                                                                                                       ),
                                                                                                                         
                                                                                                                                column(3, shinyBS::bsButton("undo_dupl_ops", label="Undo", style="info", icon=icon("hand-o-right"),disabled = TRUE))
                                                                                                                       
                                                                                                                       )), #col &tabitem
                                                                                                                        
                                                                                                                       
                                                                                                                       column(width = 6, tabItem(tabName =  "right_col_table",
                                                                                                                              conditionalPanel(
                                                                                                                                condition="input.checkbox_show_dupl_cols == 1",
                                                                                                                              column(12,
                                                                                                                              DT::dataTableOutput("operated_dupl_cols_content")
                                                                                                                              )
                                                                                                                              )#condpan
                                                                                                                       )
                                                                                                                       )),
                                                                                                                       
                                                                                                                       fluidRow(br()),
                                                                                                                       #         wellPanel(fluidRow(
                                                                                                                         #         column(12, htmlOutput("pt_text_undo_DUPL"), uiOutput("pt_label_undo_DUPL"),br(),#textAreaInput(inputId="PT_store_split1", "Procedural track", value = "...", width = "100%",height = "100%", placeholder = NULL), 
                                                                                                                                #                fluidRow(       
                                                                                                                                  #                    column(4, align="left",shinyBS::bsButton("glp_pt_button_split_undo_DUPL", label="Add", style="info", icon=icon("hand-o-right"))),
                                                                                                                                  
                                                                                                                                  #          ))# fluidRow
                                                                                                                         #        ) #fluidRow
                                                                                                               #        ),#wellpanel
                                                                                                                       
                                                                                                                       shinycssloaders::withSpinner(plotlyOutput("BMD_pval_fitting"), type = 6)
                                                                                                                       
                                                                                                       ),
                                                 
                                                 
                                                 
                                                 
                                                                                                       bsCollapsePanel("Content Homogenization phase", style="primary",
                                                                                                                       column(3,htmlOutput("total_recode_steps")), column(3,htmlOutput("only_recodAC_steps")),  column(3,htmlOutput("only_recodRJ_steps")),column(3,htmlOutput("only_delete_steps")),    
                                                                                                                       fluidRow(),br(),
                                                                                                                       
                                                                                                                       fluidRow(
                                                                                                                         
                                                                                                                         
                                                                                                                         column(width = 6,
                                                                                                                                
                                                                                                                                selectizeInput(
                                                                                                                                  inputId="pick_type_recode_modif",
                                                                                                                                  label="Type of modification:",
                                                                                                                                  choices=c("Deleted unmodified column","Recoded column", "Recoded but rejecting full recoding"),
                                                                                                                                  multiple=FALSE,
                                                                                                                                  options = list(
                                                                                                                                    placeholder = 'Please select an option below',
                                                                                                                                    onInitialize = I('function() { this.setValue(""); }'))),
                                                                                                                                
                                                                                                                                
                                                                                                                                       fluidRow(
                                                                                                                                          column(12,
                                                                                                                                                 conditionalPanel(
                                                                                                                                                   condition = "input.pick_type_recode_modif == 'Recoded column' || input.pick_type_recode_modif == 'Recoded but rejecting full recoding'",                   
                                                                                                                                                   selectizeInput(
                                                                                                                                                     inputId= "recode_ops_done_newlab",
                                                                                                                                                     label="New label:",
                                                                                                                                                     choices=NULL,
                                                                                                                                                     multiple=FALSE,
                                                                                                                                                     options = list(
                                                                                                                                                       placeholder = 'Please select an option below',
                                                                                                                                                       onInitialize = I('function() { this.setValue(""); }'))),
                                                                                                                                                   
                                                                                                                                                   
                                                                                                                                                 #  checkboxInput("checkbox_show_relab_cols", label = "Show columns", value = FALSE),
                                                                                                                                                   selectizeInput(
                                                                                                                                                        inputId= "recode_ops_done_oldlab",
                                                                                                                                                        label="Correspondent old label:",
                                                                                                                                                        choices=NULL,
                                                                                                                                                        multiple=FALSE,
                                                                                                                                                        options = list(
                                                                                                                                                          placeholder = 'Please select an option below',
                                                                                                                                                          onInitialize = I('function() { this.setValue(""); }'))),
                                                                                                                                                   
                                                                                                                                                 conditionalPanel(
                                                                                                                                                   condition = "input.pick_type_recode_modif == 'Recoded column' ",                   
                                                                                                                                                     selectizeInput(
                                                                                                                                                        inputId= "recode_ops_done_newcont",
                                                                                                                                                        label="New content:",
                                                                                                                                                        choices=NULL,
                                                                                                                                                        multiple=FALSE,
                                                                                                                                                        options = list(
                                                                                                                                                          placeholder = 'Please select an option below',
                                                                                                                                                          onInitialize = I('function() { this.setValue(""); }'))),
                                                                                                                                                 
                                                                                                                                                      selectizeInput(
                                                                                                                                                        inputId= "recode_ops_done_oldcont",
                                                                                                                                                        label="Correspondent old content:",
                                                                                                                                                        choices=NULL,
                                                                                                                                                        multiple=FALSE,
                                                                                                                                                        options = list(
                                                                                                                                                          placeholder = 'Please select an option below',
                                                                                                                                                          onInitialize = I('function() { this.setValue(""); }'))),
                                                                                                                                                  ) #2nd condpanel
                                                                                                                                                 
                                                                                                                                                 
                                                                                                                                                 
                                                                                                                                                 
                                                                                                                                                 
                                                                                                                                                 ),#1st condPan e fluidrow
                                                                                                                                                 
                                                                                                                                                 
                                                                                                                                                 #for deleted columns
                                                                                                                                                 conditionalPanel(
                                                                                                                                                   condition = "input.pick_type_recode_modif == 'Deleted unmodified column'",                   
                                                                                                                                                   selectizeInput(
                                                                                                                                                     inputId= "recode_ops_done_delete",
                                                                                                                                                     label="Deleted unmodified column:",
                                                                                                                                                     choices=NULL,
                                                                                                                                                     multiple=FALSE,
                                                                                                                                                     options = list(
                                                                                                                                                       placeholder = 'Please select an option below',
                                                                                                                                                       onInitialize = I('function() { this.setValue(""); }')))
                                                                                                                                                   
                                                                                                                                                      ), #condpan deleted cols
                                                                                                                                                   
                                                                                                                                                 
                                                                                                                                                 
                                                                                                                                                 fluidRow(
                                                                                                                                                   column(3, shinyBS::bsButton("undo_recode_ops", label="Undo", style="info", icon=icon("hand-o-right"),disabled = TRUE))
                                                                                                                                                 )
                                                                                                                                          )#tabitem ecolumn   
                                                                                                                                       )   
                                                                                                                                
                                                                                                                                
                                                                                                                                
                                                                                                                         ),
                                                                                                                         #$         column(width = 5,
                                                                                                                         
                                                                                                                         #$               dataTableOutput("operated_relab_cols_content")
                                                                                                                         #$        )#column
                                                                                                                       ), # e fluidrow
                                                                                                                       
                                                                                                                       
                                                                                                                       #$              fluidRow(br()),
                                                                                                                       #$       wellPanel(fluidRow(
                                                                                                                       #$         column(12, htmlOutput("pt_text_undo_RELAB"), uiOutput("pt_label_undo_RELAB"),br(),#textAreaInput(inputId="PT_store_split1", "Procedural track", value = "...", width = "100%",height = "100%", placeholder = NULL), 
                                                                                                                       #$                fluidRow(       
                                                                                                                       #$                  column(4, align="left",shinyBS::bsButton("glp_pt_button_split_undo_RELAB", label="Add", style="info", icon=icon("hand-o-right"))),
                                                                                                                       #$                  
                                                                                                                       #$              ))# fluidRow
                                                                                                                       #$       ) #fluidRow
                                                                                                                       #$       ),#wellpanel
                                                                                                                       
                                                                                                                       
                                                                                                                       
                                                                                                                 shinycssloaders::withSpinner(plotlyOutput("BMD_BMDL"), type = 6)          
                                                                                                                       
                                                                                                                       
                                                                                                                       
                                                                                                                       
                                                                                                                       
                                                                                                                       
                                                                                                                       
                                                                                                                       
                                                                                                                       
                                                                                                                       
                                                                                                                       
                                                                                                                       
                                                                                                                       
                                                                                                                       
                                                                                                                       
                                                                                                                       
                                                                                                                       
                                                                                                                       
                                                                                                                       
                                                                                                                       
                                                                                                                       
                                                                                                                       
                                                                                                                       
                                                                                                            #           shinycssloaders::withSpinner(plotlyOutput("BMD_BMDL"), type = 6)
                                                                                                                       
                                                                                                       )
                                                                                            ) 
                                                                            )
                                                                            
                                                                            )     
                                                                   
                                                          
                                                        
                                                   
                              )#chius condpan
                                   ),
                          

                       
                        #  tabPanel(value="special_ops", title="Special Operations",
                        #      fluidPage(
                        #         tabsetPanel(
                        #           tabPanel(id= "agilent","Agilent",
                        #                    yyy<- df
                                            
                                            
                        #                   ),
                        #           tabPanel(id="adding_empty_col","Adding empty columns",
                        #                    fluidRow()
                                            
                                            
                                            
                        #                    ),
                        #           tabPanel(id="split_col","Splitting column",),
                        #           tabPanel(id="convert_col","Convert column format"),
                        #           tabPanel(id="customF","Custom function")
                        #         )
                        #         )
                               
                                   
                        #  ),
                          
                        
                       
                        
                        
                           
                        
                         
                          tabPanel(value="plot_page", title="Plotting Tool",
                                   tags$head(
                                     tags$style(HTML(".help-block  a{color: black ;}"))
                                   ),
                                   
                                   fluidRow( column(6, align="left",
                                      tabItem(tabName = "plotting_parameters",
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
                                                        multiple=FALSE,
                                                        options = list(
                                                        placeholder = 'Please select an option below',
                                                        onInitialize = I('function() { this.setValue(""); }'))),    
                                            ),
                                                    div(style = "margin-top: -20px"),
                                                    uiOutput( "class_x"),
                                                    div(style = "margin-top: 12px")
                                            )), 
                                   
                                            fluidRow(    column(6, align="left",
                                                    tabItem(tabName = "tabmultiY",
                                                        selectizeInput(
                                                        inputId="multi_colnamesY",
                                                        label="Ordinate",
                                                        choices=NULL,
                                                        selected=1,
                                                        multiple=FALSE,
                                                        options = list(
                                                        placeholder = 'Please select an option below',
                                                        onInitialize = I('function() { this.setValue(""); }'))),
                                                        ),
                                                        
                                                    div(style = "margin-top: -20px"),
                                                    uiOutput( "class_y"),
                                                    div(style = "margin-top: 12px")
                                            )),
                                   
                                          fluidRow(  column(6, align="left",  
                                                    tabItem(tabName = "tabmultiCONDITION",
                                                        selectizeInput(
                                                        inputId="multi_colnamesCOND",
                                                        label="Grouping condition",
                                                        choices=NULL,
                                                        selected=1,
                                                        multiple=FALSE,
                                                        options = list(
                                                        placeholder = 'Please select an option below',
                                                        onInitialize = I('function() { this.setValue(""); }')))
                                                    ),
                                                    div(style = "margin-top: -20px"),
                                                    uiOutput("class_cond"),
                                                    div(style = "margin-top: 12px")
                                            )),
                                       
                                      
                                      
                                          fluidRow(br()),
                                   
                                          fluidRow( column(6, align="left",
                                                    radioButtons("type_plot_relation", label = "Type of data relation in the plot:", 
                                                                       choices = list("Stacked"="stacked", "Grouped"="dodge","No condition"="nocond"),
                                                                       selected = character(0) ,inline = TRUE, width = '100%'))
                                            ),  #fludirow
                                      
                                          
                                          fluidRow(br()),
                                          fluidRow(column(6,align="left", tags$strong(helpText(a(paste("Plotting customization:")))),  
                                                    checkboxInput("type_value_barplot", label = "y-axis values as Percent", value=FALSE),
                                                    checkboxInput("remove_legend", label = "Remove legend", value=FALSE),
                                               #     radioButtons("legend_position", label = "Legend orientation:", 
                                              #                   choices = list("Right"="right", "Left"="left","Top"="top","Bottom"="bottom"),
                                              #                   selected = "right" ,inline = TRUE, width = '100%'),
                                                    textInput("x_axis_label", label= "X axis label", width = "100%"),
                                                    textInput("y_axis_label", label= "Y axis label", width = "100%"),
                                                    textInput("cond_legend_label", label= "Legend label", width = "100%")
                                                   )
                                            ),  #fludirow
                                      
                                      
                                   
                                   
                                   
                        #           fluidRow(
                        #       #      column(1, align="left",shinyBS::bsButton("save_multipic", label="Download Image",style="info", icon=icon("hand-o-right"), disabled=FALSE)) 
                        #             column(1, align="left",shinyBS::bsButton("plot_multipic", label="Plot",style="info", icon=icon("hand-o-right"), disabled=FALSE)), 
                        #             column(1, align="left",shinyBS::bsButton("save_multipic", label="Reset Selections",style="info", icon=icon("hand-o-right"), disabled=FALSE)) 
                        #           )
                               
                                          
                        
                        
                        
                        div(style="display: inline-block; vertical-align:center; horizontal-align:left; margin-right:200px", #class = "row-fluid",
                            shinyBS::bsButton("plot_multipic", label="Plot",style="info", icon=icon("hand-o-right"), disabled=FALSE),
                            shinyBS::bsButton("save_multipic", label="Reset Selections",style="info", icon=icon("hand-o-right"), disabled=FALSE)
                            )
                    
                    
                        
                        
                                   ),
                               
                               
                               column(6, align="left",
                                      
                                      tabItem(tabName = "plot_barplot_section",
                                              plotlyOutput("multiplot", height  = "400px"),
                                         
                                              
                                              
                                              
                                              
                                              ))
                               )
                               
                               
                               
                          ),#chius tabpanel
                          
                          
                        
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
                                                                  
                                                                  #multijoin_outcomes ~ .selectize-control .option[data-value = 'To overview'],
                                                                                                                    .item[data-value = 'To overview']{
                                                                  color: #DC3545;
                                                                  }
                                                                  .selectize-input  .item[data-value = 'Safe']    {font-weight: bolder !important; color:  #04AA6D !important;}
                                                                  .selectize-input  .item[data-value = 'Fast check'] {font-weight: bolder !important; color:  #FFC107 !important;}
                                                                  .selectize-input  .item[data-value = 'To overview'] {font-weight: bolder !important; color:  #DC3545 !important;}
                                                                 
                                                        
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
                                                        choices=NULL,# c("Safe","Fast check","To overview"),
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
                                      )),#tabitem
                                                
                                         
                                    column(6,
                                           tabItem(tabName = "Multi_tabitem_right",
                                           br(),br(),
                                           fluidRow(column(12, #align="left",
                                               div(style="display: inline-block; vertical-align:center; horizontal-align:left; margin-right:200px",
                                                    shinyBS::bsButton("accept_multi_button", label="Consistent",style="info", icon=icon("hand-o-right"),disabled = TRUE) ,
                                                    shinyBS::bsButton("issue_multi_button", label="Issue",style="info", icon=icon("hand-o-right"),disabled =TRUE) ,
                                                    shinyBS::bsButton("undo_multi_button", label="Undo",style="info", icon=icon("hand-o-right"),disabled =F) ,
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
                                                                 column(4, align="left",shinyBS::bsButton("glp_pt_button_multi", label="Add", style="info", icon=icon("hand-o-right"),disabled = TRUE)),
                                )
                              ))), 
                              br(),
                              
                              fluidRow(column(12,
                                              div(style="display: inline-block; vertical-align:center; horizontal-align:left; margin-right:100px", 
                                                  downloadButton("download_multi_issue", label="Download Issue Report") ,
                                                  downloadButton("download_multi_consistent", label="Download Consistent Report") 
                                              ))
                              ),  br(), 
                              
                                     
                        )#chius condpan
                      ), #chius tabpanel 
                        
                        
                        
                        
                        
                        
                        
                        
                        
                        
                        
                        
                        
                        
                           
                             
                      )
      ))
 
                                                           )
                                                           )
                         )
}
