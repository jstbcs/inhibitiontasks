# 
# Helper functions and vectors for inhibition task db shiny app 
# 


# vector storing criteria to be chosen
criteria <- c("Task type(s)",
              "Mean reaction time (in ms)", "Mean accuracy", "Number of participants",
              "Number of blocks per participant", "Number of trials per block",
              "Neutral stimuli included?", "Time limit for responses (in ms)",
              "Existence of between-subject manipulation?", 
              "Existence of within-subject manipulation (besides congruency)?",
              "Conducted (Year of Publication)",
              "Publication Code"
              )

# TODO:connect to db to update automatically
publication_codes <- c("pratte_2010_exploring",
               "mermet_2018_should ",
               "hedge_2018_reliability",
               "vonbastiaan_2015_evidence",
               "whitehead_2020",
               "tang_2022_dual",
               "chetverikov_2017_blame",
               "stahl_2014_behavioral",
               "ebersole_2016_many")


# function to choose default value of "value" fields 
get_default_value <- function(criterion, operator){
  # only execute once operator has been chosen
  if(!is.null(operator)){
    
    # TODO: check
    default_value <- switch(criterion,
                            "Mean reaction time (in ms)" = 700, 
                            "Mean accuracy" = 0.8, 
                            "Number of participants" = 100, 
                            "Number of blocks per participant" = 5, 
                            "Number of trials per block" = 30, 
                            "Time limit for responses (in ms)" = 2000,
                            "Conducted (Year of Publication)" = 2010
    )
    
    # optionally: second default value for between operator 
    default_value_b <- ifelse(operator == "between",
                              ifelse(criterion == "Conducted (Year of Publication)", # for "conducted" choose current year
                                     format(Sys.Date(), "%Y"),
                                     default_value * 1.2),  # in all other cases: increase by 20%
                              NA)
    
    
        return(c(default_value, default_value_b))
    
  }
}

# column names for suited_data_df
colnames_suited <- c("Publication Code", "Authors", "Conducted", "Dataset ID", 
                     "Between person manipulation", "Within person manipulation",
                     "Sample size", "Blocks per participant", "Trials per block")

# column names for descriptives_df 
colnames_descriptives <- c("Dataset ID", "Mean number of trials per participant", 
                           "Percentage congruent", "Mean reaction time", 
                           "Mean accuracy", "Number of conditions", "Time limit (in ms)",
                           "Data exclusion criteria")

