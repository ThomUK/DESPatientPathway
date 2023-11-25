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
    column(
      width = 6,
      div(
        style = "border: 2px solid #ddd; border-radius: 5px; padding: 10px;",
        sliderInput(NS(id, "numForecastLength"), "Future period to forecast (weeks)", value = 104, min = 2, max = 260),
        sliderInput(NS(id, "numPatReferralRate"), "Number of new patient referrals (monthly)", value = 100, min = 0, max = 1000),
        sliderInput(NS(id, "numPatBacklogSize"), "Number of existing patients in the OP clinic backlog", value = 500, min = 0, max = 5000),
        sliderInput(NS(id, "numAdmitConversionRate"), "Outpatient conversion rate (OP -> admission / treatment waiting list)", value = 0.1, min = 0, max = 1),
        sliderInput(NS(id, "numFupRate"), "Outpatient followup rate", value = 0.25, min = 0, max = 1), #TODO check and improve this logic
      ),
      div(
        style = "border: 2px solid #ddd; border-radius: 5px; padding: 10px; margin-top: 5px;",
        sliderInput(NS(id, "numOpClinicLength"), "Length of an OP clinic (minutes)", value = 30, min = 5, max = 120),
      ),
      div(
        style = "border: 2px solid #ddd; border-radius: 5px; padding: 10px; margin-top: 5px;",
        numericInput(NS(id, "numBeds"), "Total beds (pre & post-operative combined)", value = 6, min = 1, max = 36),
        sliderInput(NS(id, "numPreOpLos"), "Average pre-operative length of stay (hours)", value = 6, min = 0, max = 36),
        sliderInput(NS(id, "numPostOpLos"), "Average post-operative length of stay (hours)", value = 36, min = 1, max = 96),
      )
    ),
    column(
      width = 6,
      actionButton(NS(id, "updateButton"), "Update Model!", class = "btn-success"),
    ),
    column(12,
      plotOutput(NS(id, "queuePlot")),
      plotOutput(NS(id, "serverPlot")),
      plotOutput(NS(id, "utilisationPlot"))
    )
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
        pat_referral_rate = input$numPatReferralRate,
        pat_backlog_size = input$numPatBacklogSize,
        op_conversion_rate = input$numAdmitConversionRate,
        op_fup_rate = input$numFupRate,
        op_clinic_length = input$numOpClinicLength,
        total_beds = input$numBeds,
        pre_op_los = input$numPreOpLos,
        post_op_los = input$numPostOpLos
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
