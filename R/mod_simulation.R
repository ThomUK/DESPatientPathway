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
    sliderInput(NS(id, "numForecastLength"), "Future period to forecast (weeks)", value = 104, min = 2, max = 260),
    actionButton(NS(id, "updateButton"), "Update Model!", class = "btn-success"),

    plotOutput(NS(id, "queuePlot")),
    plotOutput(NS(id, "serverPlot")),
    plotOutput(NS(id, "utilisationPlot"))
  )
}

#' simulation Server Functions
#'
#' @noRd
mod_simulation_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # prepare the config object
    model_config <- reactive(
      list(
        forecast_length = input$numForecastLength,
        pat_referral_rate = input$numPatReferralRate
      )
    )

    observeEvent(input$updateButton, {

      # run the simulation
      sim <- run_sim(model_config)

      # compute some stats
      sim_resources <- sim |>
        get_mon_resources()

      # make a plot
      output$queuePlot <- renderPlot(
        plot(sim_resources, metric = "usage", items = "queue", steps = TRUE) +
          scale_x_continuous(name = "days", labels = scales::number_format(scale = 1/60/24))  # format labels to represent days
      )
      output$serverPlot <- renderPlot(
        plot(sim_resources, metric = "usage", items = "server", steps = TRUE) +
          scale_x_continuous(name = "days", labels = scales::number_format(scale = 1/60/24))  # format labels to represent days
      )
      output$utilisationPlot <- renderPlot(
        plot(sim_resources, metric = "utilization")
      )
    })

  })
}

## To be copied in the UI
# mod_simulation_ui("simulation_1")

## To be copied in the server
# mod_simulation_server("simulation_1")
