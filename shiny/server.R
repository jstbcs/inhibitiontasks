# # R Shiny Sever
#
# Defining server for the "inhibition data base" shiny app
#

library(shiny)


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
  
   
  
}



#
shinyApp(ui = ui, server = server)
