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
    p("This is a 'Discrete Event Simulation' of the flow of patients through a typical acute trust hopsital service.  Patients are referred in to be seen at an outpatient (OP) clinic.  The clinic takes a decision to admit to waiting list, followup with another clinic appointment later, or discharge completely.  If admitted the patient visits a pre-operative ward, the operating theatre, and finally a post-operative ward before being discharged home."),
    fluidRow(
      column(
        width = 12,
        DiagrammeR::grVizOutput(NS(id, "pathwayDiagram"))
      )
    ),
    fluidRow(
      column(
        width = 12,
        p("This model is in development.  It is not yet ready to be used for planning.  Some of the issues to be resolved are detailed below."),
        HTML(
            "<ul>
              <li>Is the main queue appearing in the right place?  Is the typical real waiting list post referral and pre OP appointment, or is it post OP appt and pre-admission.</li>
              <li>How to model the queue between OP appt and hospital admission?</li>
              <li>The method used to implement the OP follow-up rate is not accurate.</li>
              <li>The length of stay and theatre procedure lengths are modelled as normal distributions, but these are typically right-skewed in real data.</li>
              <li>Appointment non-attendance is not yet modelled.  Attendance is assumed to be 100%.</li>
              <li>The ward is modelled as a shared pre and post-operative ward, but the priority and behaviour of post-op vs. pre-op patients needs to be checked.</li>
              <li>Need to add an additional class of 'priority' patients (eg. cancer) in parallel to the standard patients. Define the queuing behaviour.</li>
              <li>The OP clinic and Theatre schedules are hard-coded as 8hrs/day, 7days/week.  This needs to be user-configurable.</li>
            </ul>"
        )
      )
    ),
    fluidRow(
      column(
        width = 6,
        h4("Model options:"),
        div(
          style = "border: 2px solid #ddd; border-radius: 5px; padding: 10px;",
          sliderInput(NS(id, "numForecastLength"), "Future period to forecast (weeks)", value = 1, min = 1, max = 104),
        )
      ),
      column(
        width = 6,
        h4("Service config:"),
        div(
          style = "border: 2px solid #ddd; border-radius: 5px; padding: 10px; margin-top: 5px;",
          sliderInput(NS(id, "numOpClinicLength"), "Length of an OP clinic (minutes)", value = 30, min = 5, max = 120),
          numericInput(NS(id, "numBeds"), "Total beds (pre & post-operative combined)", value = 6, min = 1, max = 36),
        )
      )
    ),
    fluidRow(
      column(
        width = 6,
        h4("Patient pathway config:"),
        div(
          style = "border: 2px solid #ddd; border-radius: 5px; padding: 10px;",
          sliderInput(NS(id, "numPatBacklogSize"), "Number of existing patients in the OP clinic backlog", value = 500, min = 0, max = 5000),
          sliderInput(NS(id, "numPatReferralRate"), "Number of new patient referrals (monthly)", value = 100, min = 0, max = 1000),
          sliderInput(NS(id, "numFupRate"), "Outpatient followup rate", value = 0.25, min = 0, max = 1), #TODO check and improve this logic
          sliderInput(NS(id, "numAdmitConversionRate"), "Outpatient conversion rate (OP -> admission / treatment waiting list)", value = 0.1, min = 0, max = 1),
          sliderInput(NS(id, "numPreOpLos"), "Average pre-operative length of stay (hours)", value = 6, min = 0, max = 36),
          sliderInput(NS(id, "numPostOpLos"), "Average post-operative length of stay (hours)", value = 36, min = 1, max = 96),
          sliderInput(NS(id, "numTheatreProcLength"), "Average length of theatre procedures (minutes)", value = 90, min = 5, max = 720),
        )
      )
    ),
    column(
      width = 12,
      actionButton(NS(id, "updateButton"), "Run Model (Adjust options above, Run the model, Review results below...)", class = "btn-success", width = "100%", style = "margin-top: 10px;"),
    ),
    column(12,
      hr(),
      plotOutput(NS(id, "queuePlot")),
      hr(),
      plotOutput(NS(id, "serverPlot")),
      hr(),
      plotOutput(NS(id, "utilisationPlot")),
      hr(),
      h4("Patient pathway:"),
      DiagrammeR::grVizOutput(NS(id, "trajectoryPlot"), height = "2000px"),
      hr()
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
        post_op_los = input$numPostOpLos,
        theatre_proc_length = input$numTheatreProcLength
      )
    )

    # pathway diagram
    output$pathwayDiagram <- DiagrammeR::renderGrViz(DiagrammeR::grViz(
      "
      digraph {

        # graph attributes
        graph [layout = dot,
                rankdir = LR,
                fontname = Arial,
                label = 'Patient Pathway',
                labelloc = t]

        # node attributes
        node [shape = box,
              color = black,
              style = filled,
              fillcolor = White;
              height = 0.8,
              width = 1.5]

        # edge attributes
        edge [color = black]

        # node statements
        A [label = 'Patient \n Referral', shape = circle, width = 1.2];
        B [label = 'Outpatient \n Appointment', fillcolor = Linen];
        C [label = 'Pre-Op Bed', fillcolor = Linen];
        D [label = 'Operating \n Theatre', fillcolor = Linen];
        E [label = 'Post-Op Bed', fillcolor = Linen];
        F [label = 'Discharge \n Home', shape = circle, width = 1.2];


        # edge statements

        A->B;
        B->C;
        B->B [dir=back, label = 'Followup \n arranged'];
        C->D;
        D->E;
        E->F;
        B->F;

      }
      ")
    )

    observeEvent(input$updateButton, {

      # it's most convenient to use library calls while prototyping
      # may remove later
      library(simmer)
      library(simmer.plot)
      library(ggplot2)

      # run the simulation
      res <- run_sim(model_config)

      # extract data from the result object
      sim <- res$sim
      patient <- res$patient

      # compute some stats
      sim_resources <- sim |>
        get_mon_resources() |>
        dplyr::mutate(
          resource = factor(resource, levels = c("OP Clinic", "Theatre", "Bed"))
        )

      # make a plot
      output$queuePlot <- renderPlot(
        plot(sim_resources, metric = "usage", items = "queue", steps = TRUE) +
          scale_x_continuous(name = "Days", labels = scales::number_format(scale = 1/60/24)) + # format labels to represent days
          labs(
            title = "Queue size",
            y = "Number of patients"
          ) +
          theme_minimal() +
          theme(legend.position = "none")
      )
      output$serverPlot <- renderPlot(
        plot(sim_resources, metric = "usage", items = "server", steps = TRUE) +
          scale_x_continuous(name = "Days", labels = scales::number_format(scale = 1/60/24)) + # format labels to represent days
            scale_color_manual(values = "lightgreen") +
            labs(
            subtitle = "Dotted line = max capacity, Solid line = actual usage",
            y = "Used"
          ) +
          theme_minimal() +
          theme(legend.position = "none")
      )
      output$utilisationPlot <- renderPlot(
        plot(sim_resources, metric = "utilization") +
          labs(
            title = "Resource utilisation",
            x = "Resource",
            y = "Utilisation"
          ) +
          theme_minimal()
      )
      output$trajectoryPlot <- DiagrammeR::renderGrViz(
        plot(patient, verbose = TRUE)
      )


    })

  })
}

## To be copied in the UI
# mod_simulation_ui("simulation_1")

## To be copied in the server
# mod_simulation_server("simulation_1")
