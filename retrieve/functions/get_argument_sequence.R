get_argument_sequence <- function(arguments, argument_relation){
  if (length(argument_relation) == 1){
    if (argument_relation == "and"){
      argument_sequence = 1:length(arguments)
    } else if (argument_relation == "or"){
      argument_sequence = rep(1, length(arguments))
    }
  }
  else if (length(arguments) != length(argument_relation)){
    stop("When specifying custom argument-relations, make sure that you have a vector of same length as the number of arguments")
  } else {
    argument_sequence = argument_relation
  }
  return(argument_sequence)
}
