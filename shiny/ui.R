# 
# User interface for the inhibition data base shiny app 
# 

library(shiny)
source("shiny/helper_file_shiny.R")

ui <- fluidPage(
  
  # TITLE 
  titlePanel("Information about data in inhibition data base"),
  
  # INTRO PART
  
  # short intro text
  fluidRow(
    column(12,
           htmlOutput("short_intro"))
  ),
  
  hr(),
  
  # action button explaining the project 
  fluidRow(
    column(12, 
           actionButton("action_explain_db", "What is the inhibition task data base?")),
           htmlOutput("explanation_db")
          # imageOutput("img_structure_db")
  ), 
  
  br(),
  
  # action button explaining how to contribute
  fluidRow(
    column(12, 
           actionButton("action_contribute", "How can I contribute my data to the data base?"), 
           htmlOutput("explanation_contribute")
    )
  ),
  
  hr(),
  

  # MAIN PART OF SHINY APP: USER INPUT & OUTPUT PANELS  
  br(),
  
  # SIDE BAR FOR USER INPUT
  sidebarPanel(
    
    # create tabs
    shiny::tabsetPanel(
      type = "pills",
      
      # TAB 1
      tabPanel("Filter datasets",
               
               # add argument ---
               fluidRow(    # split sidebar into 3 columns 
                 column(4, 
                        # drop down menu criterion
                        selectInput(inputId = "criterion1",
                                    label = "Choose criterion:",
                                    choices = c(" ", criteria),
                                    selected = "")), 
                 
                 column(4, 
                        # conditional operator
                        uiOutput("operator1")),
                 
                 column(4, 
                        # conditional value field 
                        uiOutput("value1"),
                        # binary choice for yes/no questions
                        uiOutput("yes_no_choice"),
                        # choice of task type(s)
                        uiOutput("choice_task_type"),
                        # choice of pub_code
                        uiOutput("choice_pubcode")
                        )
               ), # end fluid row
               
               
               fluidRow(
                 column(4),  # empty 
                 
                 column(4),  # empty 
                 
                 column(4, 
                        # conditional second value for "between" operator
                        uiOutput("value1b"))
               ), # end fluid row
               
               # option to add argument ---
               fluidRow(
                 column(4, 
                        actionButton("action_add_arg", "Add argument to list")),
                 
                 column(4),  # empty 
                 
                 column(4)  # empty 
               ), # end fluid row
               
               # summary of chosen arguments ---
               
               tableOutput("summary"),
               
               fluidRow(
                 column(6,  # button remove recent argument
                        uiOutput("conditional_action_remove")),   
                 
                 column(6, # button reset list
                        uiOutput("conditional_action_reset"))
               ) # end fluid row
               
               ),
      
      # TAB 2
      tabPanel("Filter data",
               h3("Filter data across all datasets")
               
      ), 
      
      # TAB 3
      tabPanel("Publication list",
               h3("List of all publications & datasets")
               
      ), 
      
    ) # end tabsetpanel 
  
    ), # end sidebar
  
  # MAIN PANEL FOR OUTPUT
  mainPanel(
    # create tabs for main bar
    shiny::tabsetPanel(
      type = "pills",
      
      # TAB 1
      tabPanel("Overview of suited datasets",
               
               tableOutput("suited_datasets")),
      
      # TAB 2
      tabPanel("Descriptives",
               
               tableOutput("descriptives")),
      
      # TAB 3
      tabPanel("Get the data",
               
               # print R code
               h2("Use this code to access the data directly in R:"),
               verbatimTextOutput("Rcode"),
               
               # option to download data as csv file
               h2("Or download it directly as a csv file:"), 
               downloadLink("downloadData", "Download csv file")
      )
    
    ) # end tabset 
  ) # end main bar
)

