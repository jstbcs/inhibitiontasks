# 
# Helper functions and vectors for inhibition task db shiny app 
# 


# vector storing criteria to be chosen
criteria <- c("Mean reaction time (in ms)", "Mean accuracy", "Number of participants",
              "Number of blocks per participant", "Number of trials per block",
              "Neutral stimuli included?", "Time limit for responses (in ms)",
              "Existence of between-subject manipulation?", 
              "Existence of within-subject manipulation (besides congruency)?"
              )


# function to choose default value of "value" fields 
get_default_value <- function(criterion, operator){
  # only execute once operator has been chosen
  if(!is.na(operator)){
    
    # TODO: check
    default_value <- switch(criterion,
                            "Mean reaction time (in ms)" = 700, 
                            "Mean accuracy" = 0.8, 
                            "Number of participants" = 100, 
                            "Number of blocks per participant" = 5, 
                            "Number of trials per block" = 30, 
                            "Time limit for responses (in ms)" = 2000,
                            #"Conducted (Year of Publication" = 2010
    )
    
    # optionally: second default value for between operator 
    default_value_b <- ifelse(operator == "between",
                              default_value * 1.2,
                              NA)
    
    
    #if(criterion == "Conducted (Year of Publication)" & operator == "between"){
    #  default_value_b <- format(Sys.Date(), "%Y")
    #  } else if (operator == "between"){
    #  default_value_b <- default_value * 1.2 
    #  } else {
    #    default_value_b <- NA
    #  }
    #
    return(c(default_value, default_value_b))
    
  }
}
