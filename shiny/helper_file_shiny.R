# 
# Helper functions and vectors for inhibition task db shiny app 
# 


# verctor storing criteria to be chosen
criteria <- c(#"Task type",
              "Mean reaction time (in ms)", "Mean accuracy", "Number of participants",
              "Number of blocks per participant", "Number of trials per block",
              "Neutral stimuli included?", "Time limit for responses",
              "Existence of between-subject manipulation", 
              "Existence of within-subject manipulation (besides congruency)")


# function to choose default value of "value" fields 
get_default_value <- function(criterion){
  # TODO: check
  default_value <- switch(criterion,
                          "Mean reaction time (in ms)" = 700, 
                          "Mean accuracy" = 0.8, 
                          "Number of participants" = 100, 
                          "Number of blocks per participant" = 4, 
                          "Number of trials per block" = 30, 
                          "Time limit for responses")
  return(default_value)
}
