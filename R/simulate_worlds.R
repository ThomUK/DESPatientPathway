#' Simulate multiple replications over multiple sets of initial conditions
#'
#' @param configs List. A list of lists containing multiple initial conditions
#' @param n Integer. The number of replications to run for each initial condition
#'
#' @return List. A complex nested list structure to be analysed with the help
#' of helper functions
#' @export
#'
simulate_worlds <- function(configs, n){

  purrr::map(configs, \(x) run_multi_sim(x, n))

}
