#' Prompt user with warning message and stop if user chooses to cancel
#'
#' This function prompts the user with a warning message and gives the option to
#' continue or cancel. If the user chooses to cancel, the function throws an error
#' with a message "Process cancelled".
#'
#' @param message A warning message to display to the user.
#' @export
#' @examples
#' continue_after_warning("Are you sure you want to continue?")
continue_after_warning <- function(message) {
  answer <- require_warning_input(message)
  if (answer == 2) {
    stop("Process cancelled")
  }
}