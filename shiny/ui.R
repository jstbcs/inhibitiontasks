# 
# User interface for the inhibition data base shiny app 
# 

library(shiny)

ui <- fluidPage(
  
  # title 
  titlePanel("Information about data in inhibition data base"),
  
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
  

  # main part of shiny app: user input & output panels 
  br(),
  
  # side bar for user input
  sidebarPanel(
    # create tabs within sidebar
    shiny::tabsetPanel(
      type = "pills",
      
      # tab 1
      tabPanel("Tab 1", h1("Find suited dataset") ),
      
      # tab 2
      tabPanel("Tab 2", h1("Get info about specific dataset"))
    )
    
  ) # end sidebarpanel
  
  
)
