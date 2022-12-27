library(kableExtra)





#' Create the payments table
#' 
#' Create the table showed in the HTML files and export the columns showed to
#' participants.
#'
#' @param name The pair of procedure for a given table.
#' @param dir The directory where the files are stored.
#'
#' @return The table showing the results to subjects.
#' @export
#'
#' @examples
show_payments <- function(name, dir='.'){
  # Import the data
  data <- read.csv(file = file.path(dir, paste0(paste0(name,collapse = "_"), ".csv")))
  allocating_procedure <- data$allocating_procedure[1]
  
  # Create the columns that are intelligible to participants.
  data$type <- ifelse(data$participant_cost, "A", "B")
  data$selected_procedure <- ifelse(data$participant_mechanism == name[1], "1", "2")
  data$group <- ifelse(data[[paste("lulu", allocating_procedure, sep="_")]], "Blue", "Green")
  
  # Export the data
  columns <- c("participant_code", "type", "payoff", "USD_payoff",
               "selected_procedure", paste("wta", allocating_procedure, sep = "_"),
               "group", "lulu_compensation", "lulu_private_payoff", 
               "lulu_collective_payoff", "comprehension_payoff",
               "lottery_payoff")
  return(data[, columns])
}