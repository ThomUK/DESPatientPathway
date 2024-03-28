#' Convert scenarios from dataframe to nested list of function lists
#'
#' @param scenarios_df Dataframe. A dataframe of input scenarios
#'
#' @return List. Each scenario as a list item in a function wrapper
#'
convert_to_nested_list <- function(scenarios_df){

  nested_list <- scenarios_df |>

    # convert each row into a separate dataframe in a nested list
    split(seq(nrow(scenarios_df))) |>

    # convert each df to a list
    purrr::map(as.list) |>

    # convert to a function for future compatibility with shiny wrapper
    purrr::map(\(x) function()x)

  return(nested_list)

}
