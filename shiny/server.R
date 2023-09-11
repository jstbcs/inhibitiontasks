# # R Shiny Sever
#
# Defining server for the "inhibition data base" shiny app
#

library(shiny)
source("shiny/helper_file_shiny.R")


server <- function(input, output){
  
  # print short intro text
  output$short_intro <- renderUI({
    HTML(paste(
      "[xxx]",
      " ", " ", sep="<br/>"))
  })
  
  # print explanation of project 
  output$explanation_db <- renderUI({
    # show text when number of clicks is uneven; hide if even
    if(input$action_explain_db %% 2 == 1){
      updateActionButton(inputId = "action_explain_db", label = "Got it!")
      renderUI({HTML("[xxx]. <br> <br>"
        # <img src='db_structure.png' alt='Structure of inhibition db' width='400' height='400'>"
        )})
    } else {
      updateActionButton(inputId = "action_explain_db", label = "What is the inhibition task data base?")
      tagList()
    }
  })
  
  # print description of how to contribute 
  output$explanation_contribute <- renderUI({
    # show text when number of clicks is uneven; hide if even
    if(input$action_contribute %% 2 == 1){
      updateActionButton(inputId = "action_contribute", label = "Got it!")
      renderUI({HTML("[xxx]. <br> <br>"
                     )})
    } else {
      updateActionButton(inputId = "action_contribute", label = "How can I contribute my data to the data base?")
      tagList()
    }
  })
  
  # SIDEBAR PANELS 
  
  # tab 1
  # conditional panel to choose 1st operator based on criterion1
  output$operator1 <- renderUI({
    conditionalPanel(
      condition = "input.criterion1 != 'Select' & input.criterion1 != 'Neutral stimuli included?' &  input.criterion1 != 'Existence of between-subject manipulation' & input.criterion1 != 'Existence of within-subject manipulation (besides congruency)'",
      selectInput(inputId = "operator1",
                  label = "Choose operator",
                  choices =  c("Select", "less", "greater", "between", "equal")))
  })
  
  # conditional panel to choose 1st value based on operator
  output$value1 <- renderUI({
    conditionalPanel(
      condition = "input.operator1 != 'Select'",
      numericInput(
        inputId = "value1",
        label = "Choose value",
        value = get_default_value(input$criterion1), # TODO: get this running
        min = 0,
        max = 1000)
    ) 
  }) 
  
   
  
}



#
shinyApp(ui = ui, server = server)
