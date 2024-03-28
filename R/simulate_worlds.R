#' Simulate multiple replications over multiple sets of initial conditions
#'
#' @param configs Dataframe. A dataframe of initial conditions, each row being a simulation scenario
#' @param n Integer. The number of replications to run for each initial condition
#'
#' @return List. A complex nested list structure to be analysed with the help
#' of helper functions
#' @export
#'
simulate_worlds <- function(configs, n){

  configs_list <- convert_to_nested_list(configs)

  purrr::map(configs_list, \(x) run_multi_sim(x, n))

}
