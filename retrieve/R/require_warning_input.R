#' Prompt user for input with warning message
#'
#' This function prompts the user with a warning message and a choice to continue
#' or not. It is useful for confirming user input that may have unintended consequences.
#'
#' @param message A warning message to display to the user.
#' @return An integer value representing the user's choice (1 for "Yes" and 2 for "No").
#' @export
#' @examples
#' choice <- require_warning_input("Are you sure you want to continue?")
#' if (choice == 1) {
#' # do something
#' } else {
#' # do something else
#' }
#' 
require_warning_input <- function(message) {
  utils::menu(
    choices = c("Yes, I want to continue anyways", "No. That is not what I want"),
    title = paste0(message)
  )
}