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
  ), 
  
  hr(),
  
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
    # create tabs within sidebar
    shiny::tabsetPanel(
      type = "pills",
      
      # tab 1
      tabPanel("Find dataset", h3("What kind of data are you looking for?"),
               
               # choose task type ---
               fluidRow(
                 column(6,
                        # radio buttons to choose task type
                        radioButtons(inputId = "task_type", 
                                     label = "Choose task type:",
                                     c("Stroop task" = "stroop",
                                       "Simon task" = "simon",
                                       "Flanker task" = "flanker",
                                       "Other" = "other")),
                       ),
                 
                 column(6) # empty 
                 
               ),
               
               # add argument ---
               fluidRow(    # split sidebar into 3 columns 
                 column(4, 
                        # drop down menu criterion
                        selectInput(inputId = "criterion1",
                                    label = "Choose new criterion to filter datasets",
                                    choices = c("Select", criteria),
                                    selected = "Select")), 
                 
                 column(4, 
                        # conditional operator
                        uiOutput("operator1")),
                 
                 column(4, 
                        # conditional value field 
                        uiOutput("value1"),
                        # binary choice for yes/no questions
                        uiOutput("yes_no_choice")),
                 
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
                
                column(4),  # empty 
                
              ), # end fluid row
              
              # summary of chosen arguments --- 
              
              tableOutput("summary"),
              
              fluidRow(
                column(6,  # button remove recent argument
                       actionButton("action_remove_recent", "Remove recent argument")),  
                
                column(6, # button reset list
                       actionButton("action_reset_list", "Reset list")),
              )
                 
            ), # end tab
            
           
      # tab 2
      tabPanel("Info about specific dataset", h3("Get info about specific dataset"))       
               
              
              ), # end tabSet
      
    
  ), # end sidebarpanel
  
  # main panel for output
  mainPanel(
    # create tabs for main bar
    shiny::tabsetPanel(
      type = "pills",
      
      # tab 1
      tabPanel("Overview of suited datasets"),
      
      # tab 2
      tabPanel("Descriptives"),
      
      # tab 3
      tabPanel("Get the data")
      
      # tab 4
      # tabPanel("Download data)
    
    ) # end tabset 
  ) # end main bar
)

