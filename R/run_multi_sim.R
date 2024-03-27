
#' Run multiple replications of a simulation
#'
#' @param n Integer. Number of replications to run
#' @param model_config List. List of inputs for the model
#'
#' @return List. A list of sim outputs, one for each replication
#' @export
#'
run_multi_sim <- function(model_config, n){

  purrr::map(1:n, \(x) run_sim(model_config))

}
