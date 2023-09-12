# # R Shiny Sever
#
# Defining server for the "inhibition data base" shiny app
#

library(shiny)
source("shiny/helper_file_shiny.R")


server <- function(input, output, session){
  
  # FOR INTRO ---
  
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
  
  # SIDEBAR PANELS ---
  
  # tab 1
  
  # add arguments
  # conditional panel to choose 1st operator based on criterion1
  output$operator1 <- renderUI({
    conditionalPanel(
      condition = "input.criterion1 != 'Select' & input.criterion1 != 'Neutral stimuli included?' &  input.criterion1 != 'Existence of between-subject manipulation' & input.criterion1 != 'Existence of within-subject manipulation (besides congruency)'",
      selectInput(inputId = "operator1",
                  label = "Choose operator",
                  choices =  c("Select", "less", "greater", "between", "equal")))
  })
  
  # conditional panel to choose value based on operator
  output$value1 <- renderUI({
    conditionalPanel(
      condition = "input.operator1 != 'Select'",
      numericInput(
        inputId = "value1",
        label = "Choose value",
        value = get_default_value(input$criterion1, input$operator1)[1]
        )
    ) 
  }) 
  
  # conditional panel to choose value for between operator
  output$value1b <- renderUI({
    conditionalPanel(
      condition = "input.operator1 == 'between'",
      numericInput(
        inputId = "value1b",
        label = "and",
        value = get_default_value(input$criterion1, input$operator1)[2])
    ) 
  }) 
  
  
  # logic behind adding new argument to argument summary --
  
  # create df as reactive value
  argument_df <- data.frame(
    criertion = NA,
    operator = NA, 
    value = NA
  )
  
  rv <- reactiveValues(x = argument_df)
  
  # specify action whenever "Add argument to list" is clicked
  observeEvent(input$action_second_arg, {
    # add current choices to argument dataframe 
    new_entry <- data.frame(criterion = input$criterion1,
                            operator = input$operator1,
                            value = input$value1)
    
    rv$argument_df <- rbind(rv$argument_df, new_entry)
    
    # reset drop down menu for criterion choice
    updateSelectInput(session, 
                      inputId = "criterion1", 
                      selected = "Select")
    
    updateSelectInput(session, 
                      inputId = "operator1", 
                      selected = "Select")
    
  })
  
  # print summary df of chosen arguments
  output$summary <- renderTable(rv$argument_df)
  
}



#
shinyApp(ui = ui, server = server)
