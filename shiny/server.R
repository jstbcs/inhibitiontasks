# # R Shiny Sever
#
# Defining server for the "inhibition data base" shiny app
#

source("./shiny/helper_file_shiny.R")
source("./shiny/ui.R")


server <- function(input, output, session){
  
  # FOR INTRO ---
  
  # print short intro text
  output$short_intro <- renderUI({
    HTML(paste(
      "This shiny app provides an overview over datasets in the inhibition task data base. <br><br>
      ...",
      " ", " ", sep="<br/>"))
  })
  
  # print explanation of project 
  output$explanation_db <- renderUI({
    # show text when number of clicks is uneven; hide if even
    if(input$action_explain_db %% 2 == 1){
      updateActionButton(inputId = "action_explain_db", label = "Got it!")
      renderUI({HTML("The inhibition task data base contains inhibition task data (i.e., stroop, flanker or simon task) from over 40 datasets as well as information about the respective studies and publications. <br>
                     It is meant to enhance access to open inhibition task data. <br>
                     Data can be accessed either via SQL or our R package (?). <br> <br>"
                    # <img src='shiny/www/db_structure.png' alt='Structure of inhibition task db' width='400' height='400'>"
                     )
        })
    } else {
      updateActionButton(inputId = "action_explain_db", label = "What is the inhibition task data base?")
      tagList()
    }
  })
  
  output$img_structure_db <- renderImage({
      list(src = "shiny/www/structure_db.png",
         width = "100%",
         height = 330)
    
  }, deleteFile = F)
  
  # print description of how to contribute 
  output$explanation_contribute <- renderUI({
    # show text when number of clicks is uneven; hide if even
    if(input$action_contribute %% 2 == 1){
      updateActionButton(inputId = "action_contribute", label = "Got it!")
      renderUI({HTML("[Link to form]. <br> <br>"
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
      condition = "input.criterion1 != 'Select' & input.criterion1 != 'Task type(s)' & input.criterion1 != 'Neutral stimuli included?' &  input.criterion1 != 'Existence of between-subject manipulation?' & input.criterion1 != 'Existence of within-subject manipulation (besides congruency)?'",
      selectInput(inputId = "operator1",
                  label = "Choose operator",
                  choices =  c("Select", "less", "greater", "between", "equal"))
      )
  })
  
  # conditional panel to choose value based on operator
  output$value1 <- renderUI({
    conditionalPanel(
      condition = "input.operator1 != 'Select'",
      numericInput(
        inputId = "value1",
        label = "Choose ........ value",
        value = get_default_value(input$criterion1, input$operator1)[1])
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
  
  # conditional panel to answer yes/no questions in criterion field 
  output$yes_no_choice <- renderUI({
    conditionalPanel(
      condition = "input.criterion1 == 'Neutral stimuli included?' |  input.criterion1 == 'Existence of between-subject manipulation?' | input.criterion1 == 'Existence of within-subject manipulation (besides congruency)?'",
      selectInput(inputId = "yes_no",
                  label = " ",
                  choices =  c("Select", "Yes", "No")) # TODO: change outcome value to 1/0? (Sven)
    ) 
  }) 
  
  # conditional panel to choose task types
  output$choice_task_type <- renderUI({
    conditionalPanel(
      condition = "input.criterion1 == 'Task type(s)'",
      checkboxGroupInput(inputId = "task_type",
                         label = "Choose task type:",
                         choices = 
                           c("Select",
                             "Stroop task" = "stroop",
                             "Simon task" = "simon",
                             "Flanker task" = "flanker",
                             "Other" = "other"))
    ) 
  }) 
  
  # logic behind adding new argument to argument summary --
  
  # create df as reactive value
  argument_df <- data.frame(
    criertion = NA,
    operator = NA, 
    value = NA,
    value2 = NA
  )
  
  rv <- reactiveValues(x = argument_df)
  
  # specify action whenever "Add argument to list" is clicked
  observeEvent(input$action_add_arg, {
    # add current choices to argument data frame 
    if(input$operator1 != "Select" & input$operator1 != "between"){
      new_entry <- data.frame(criterion = input$criterion1,
                              operator = input$operator1,
                              value = input$value1,
                              value2 = "")
      
    } else if (input$operator1 == "between") {
      new_entry <- data.frame(criterion = input$criterion1,
                              operator = input$operator1,
                              value = input$value1,
                              value2 = input$value1b)
      
      } else if(input$yes_no != "Select"){
        new_entry <- data.frame(criterion = input$criterion1,
                              operator = "",
                              value = input$yes_no,
                              value2 = "")
      
      } else if(!is.null(input$task_type)){
      
        new_entry <-  data.frame(criterion = input$criterion1,
                                operator = "",
                                value = input$task_type,
                                value2 = "")
        
    }
    
    rv$argument_df <- rbind(rv$argument_df, new_entry)
    
    # reset drop down menu for criterion choice
    updateSelectInput(session, 
                      inputId = "criterion1", 
                      selected = "Select")
    
    updateSelectInput(session, 
                      inputId = "operator1", 
                      selected = "Select")
    
  })
  
  # remove last row from argument_df when 'action_remove_recent' is clicked
  observeEvent(input$action_remove_recent, {
    rv$argument_df <-  rv$argument_df[-nrow(rv$argument_df), ]
  })
  
  # reset argument_df when 'action_reset_list' is clicked
  observeEvent(input$action_reset_list, {
    rv$argument_df <- rv$argument_df[0,]
  })
  
  # print summary df of chosen arguments
  output$summary <- renderTable(rv$argument_df)
  
  # conditional action button to delete last entry to argument list 
  output$conditional_action_remove <- renderUI({
    if(!is.null(rv$argument_df[1,1])){ # show action button only after first argument was added
      actionButton("action_remove_recent", "Remove recent argument")
    }
  }) 
  
  # conditional action button to reset list 
  output$conditional_action_reset <- renderUI({
    if(!is.null(rv$argument_df[1,1])){ # show action button only after first argument was added
      actionButton("action_reset_list", "Reset list",
                   style="color: #000000; border-color: #FF000")
    }
  }) 
  
  
  
}



#
shinyApp(ui = ui, server = server)
