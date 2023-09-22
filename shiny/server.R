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
    HTML("This shiny app provides an overview over datasets in the inhibition task data base.")
  })
  
  # print explanation of project 
  output$explanation_db <- renderUI({
    # show text when number of clicks is uneven; hide if even
    if(input$action_explain_db %% 2 == 1){
      updateActionButton(inputId = "action_explain_db", label = "Got it!")
      renderUI({HTML("The inhibition task data base contains attentional control task data (i.e., stroop, flanker or simon task) from over 40 datasets as well as information about the respective studies and publications. <br>
                     It is meant to enhance access to open inhibition task data. <br>
                     Data can be accessed either via SQL or our R package (?). <br> <br>
                     <img src='/shiny/www/db_structure.png' alt='Structure of inhibition task db' width='400' height='400'>"
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
      renderUI({HTML("If you have data on attention control tasks that you would like to make available to other researchers, we would be happy to include it in our data base. <br>
                     Note that the study/ studies which data were collected must have been published (this includes preprints). <br>
                     Furthermore, suited data must fulfill include the following information: <ul>
                     <li>An ID variable</li> 
                     <li>A congruency variable, indicating stimuli were congruent or conflicting </li> 
                     <li>Reaction time of each trial, in milliseconds </li>
                     <li>Accuracyof each trials (correct/ incorrect)</li>
                     <li>A between variable indicating between subject manipulation (if applicable)</li>
                     <li>A within variable indicating within subject manipulation (if applicable)</li>
                     <br> 
                     You can submit your data via <a href=http://www.ampl-psych.com/inhibition-database/ target='_blank' rel='noopener noreferrer'>this</a> online form. <br>
                     In case you have any questions, feel free to contact <a href = 'mailto: j.m.haaf@uva.nl'>j.m.haaf@uva.nl</a>.</div>"
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
      condition = "input.criterion1 != ' ' & input.criterion1 != 'Task type(s)' & input.criterion1 != 'Publication Code' & input.criterion1 != 'Neutral stimuli included?' &  input.criterion1 != 'Existence of between-subject manipulation?' & input.criterion1 != 'Existence of within-subject manipulation (besides congruency)?'",
      selectInput(inputId = "operator1",
                  label = "Choose operator",
                  choices =  c("", "less", "greater", "between", "equal"))
      )
  })
  
  # conditional panel to choose value based on operator
  output$value1 <- renderUI({
    conditionalPanel(
      condition = "input.operator1 != ''",
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
                  choices =  c("", "Yes", "No")) # TODO: change outcome value to 1/0? (Sven)
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
  
  # conditional panel to choose publication code
  output$choice_pubcode <- renderUI({
    conditionalPanel(
      condition = "input.criterion1 == 'Publication Code'",
      selectInput(inputId = "pub_code",
                  label = "Choose publiction code",
                  choices = c("", sort(publication_codes)))
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
    if(input$operator1 != "" & input$operator1 != "between"){
      new_entry <- data.frame(criterion = input$criterion1,
                              operator = input$operator1,
                              value = input$value1,
                              value2 = "")
      
    } else if (input$operator1 == "between") {
      new_entry <- data.frame(criterion = input$criterion1,
                              operator = input$operator1,
                              value = input$value1,
                              value2 = input$value1b)
      
      } else if(input$yes_no != ""){
        new_entry <- data.frame(criterion = input$criterion1,
                              operator = "",
                              value = input$yes_no,
                              value2 = "")
      
      } else if(!is.null(input$task_type)){
        new_entry <-  data.frame(criterion = input$criterion1,
                                operator = "",
                                value = input$task_type,
                                value2 = "")
        
      } else if (!is.null(input$pub_code)){
        new_entry <-  data.frame(criterion = input$criterion1,
                                 operator = "",
                                 value = input$pub_code,
                                 value2 = "")
      
    }
    
    rv$argument_df <- rbind(rv$argument_df, new_entry)
    
    # reset drop down menu for criterion choice
    updateSelectInput(session, 
                      inputId = "criterion1", 
                      selected = "")
    
    updateSelectInput(session, 
                      inputId = "operator1", 
                      selected = "")
    
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
  
  # MAIN PANEL 
  
  # TAB 1
  # print data frame of suited data sets ----
  suited_data_df <- data.frame(
    pub_code = "hedge_2018_reliability",
    authors = "Hedge et al.",
    conducted = 2018,
    dataset_id = 32,
    between_manipulation = "None", 
    within_manipulation = "Time of measurement", 
    n_participants = 200,
    n_blocks = 5, 
    n_trials = 30
  )
  colnames(suited_data_df) <- colnames_suited
  
  output$suited_datasets <- renderTable(suited_data_df)
  
  # print dataframe of descriptives ----
  descriptives_df <- data.frame(
    dataset_id = 32,
    trials_pp = 150,
    percentage_congruent = 0.66,
    mean_rt = 750, 
    mean_accuracy = 0.94, 
    n_conditions = 2, 
    time_limit = 2000, 
    data_excl = "None"
  )
  colnames(descriptives_df) <- colnames_descriptives
  
  output$descriptives <- renderTable(descriptives_df)
  
  # print R Code to access data ----
  output$Rcode <- renderPrint({
    cat("library(inhibitiontasks)", "some query code", sep = "\n")
  })
  
  # logic behind download button
  output$downloadData <- downloadHandler(
       filename = function() {
         paste('inhibition_task_data-', Sys.Date(), '.csv', sep='')
       },
       content = function(con) {
         write.csv(descriptives_df, con)
       }
     )
  
  
}



#
shinyApp(ui = ui, server = server)

