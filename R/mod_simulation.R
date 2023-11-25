#' simulation UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_simulation_ui <- function(id){
  ns <- NS(id)
  tagList(
    actionButton(NS(id, "updateButton"), "Update Model!", class = "btn-success"),

    plotOutput(NS(id, "plot"))
  )
}

#' simulation Server Functions
#'
#' @noRd
mod_simulation_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    observeEvent(input$updateButton, {

      # run the simulation
      sim <- run_sim()

      # compute some stats
      sim_resources <- sim |>
        get_mon_resources()

      # make a plot
      output$plot <- renderPlot(
        plot(sim_resources, metric = "usage", items = c("server", "queue"), steps = TRUE) +
          scale_x_continuous(name = "days", labels = scales::number_format(scale = 1/60/24))  # format labels to represent days
      )
    })

  })
}

## To be copied in the UI
# mod_simulation_ui("simulation_1")

## To be copied in the server
# mod_simulation_server("simulation_1")