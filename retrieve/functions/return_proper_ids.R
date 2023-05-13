# Takes a list of connected id lists and a sequence and returns the joined version

return_proper_ids <- function(argument_matches, argument_sequence){
  # Easiest way to do this is via the observation arguments, then return connected ids
  # Can find a faster way in the future
  id_matches = list(
    publication_table = c(),
    study_table = c(),
    task_table = c(),
    between_table = c(),
    within_table = c(),
    condition_table = c(),
    dataset_table = c(),
    observation_table = c()
  )
  
  possible_tables = names(id_matches)
  
  for (table in possible_tables){
    proper_ids = c()
    ids = list()
    for (i in seq_along(argument_matches)){
      ids[[i]] = argument_matches[[i]][[table]]
    }
    
    for (i in seq_along(ids)){
      if (i == 1){
        proper_ids = ids[[i]]
      } else {
        if (argument_sequence[i] == argument_sequence[i-1]){
          proper_ids = unique(c(proper_ids, ids[[i]]))
        } else {
          proper_ids = proper_ids[proper_ids %in% ids[[i]]]
        }
      }
    }
    id_matches[[table]] = proper_ids
  }
  return(id_matches)
}



