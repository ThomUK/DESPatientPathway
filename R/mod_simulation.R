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
    selectInput(NS(id, "var"), "Variable", names(mtcars)),
    numericInput(NS(id, "bins"), "bins", 10, min = 1),
    plotOutput(NS(id, "hist"))
  )
}

#' simulation Server Functions
#'
#' @noRd
mod_simulation_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    data <- reactive(mtcars[[input$var]])
    output$hist <- renderPlot({
      hist(data(), breaks = input$bins, main = input$var)
    }, res = 96)

      # sim <- run_sim()
      #
      # sim_resources <- sim |>
      #   get_mon_resources()
      #
      # output$plot <- plot(sim_resources, metric = "usage", items = c("server", "queue"), steps = FALSE) +
      #   scale_x_continuous(name = "days", labels = scales::number_format(scale = 1/60/24))  # format labels to represent days

  })
}

## To be copied in the UI
# mod_simulation_ui("simulation_1")

## To be copied in the server
# mod_simulation_server("simulation_1")
