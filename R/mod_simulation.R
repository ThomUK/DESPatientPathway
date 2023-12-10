#' simulation UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_simulation_ui <- function(id) {
  ns <- NS(id)
  tagList(
    shinyjs::useShinyjs(),
    p("This is a 'Discrete Event Simulation' of the flow of patients through a typical acute trust hopsital service."),
    p("Patients are referred in to be seen at an outpatient (OP) clinic.  The patients either attend, or do not attend (DNA).  DNAs are re-booked and these patients re-join the clinic waiting list.  Patients that attend the clinic are either admitted to the treatment waiting list, re-booked for a followup appointment, or are discharged completely.  If admitted the patient visits a pre-operative bed, the operating theatre, then a post-operative bed before being discharged home.  The pre and post-operative beds are a shared resource on the same ward."),
    p("This model is in development.  It is not yet ready to be used for planning.  See the 'Notes & Assumptions' tab for more details."),
    fluidRow(
      column(
        width = 12,
        DiagrammeR::grVizOutput(NS(id, "pathwayDiagram"))
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
          sliderInput(NS(id, "numOPDNA"), "OP DNA rate (%)", value = 10, min = 0, max = 100),
          sliderInput(NS(id, "numOPOutcomeFup"), "OP outcome: Book followup (%)", value = 25, min = 0, max = 100),
          sliderInput(NS(id, "numOPOutcomeAdmit"), "OP outcome: Admit (%)", value = 10, min = 0, max = 100),
          uiOutput(NS(id, "OPOutcomeDischarge")), # a shinyjs output
          sliderInput(NS(id, "numPreOpLos"), "Average pre-operative length of stay (hours)", value = 6, min = 0, max = 36),
          sliderInput(NS(id, "numPostOpLos"), "Average post-operative length of stay (days)", value = 3, min = 0, max = 42, step = 0.1),
          sliderInput(NS(id, "numTheatreProcLength"), "Average length of theatre procedures (minutes)", value = 90, min = 5, max = 720),
        )
      )
    ),
    fluidRow(
      column(1),
      column(2, bookmarkButton(style = "margin-top: 10px;")),
      column(
        width = 8,
        actionButton(NS(id, "updateButton"), "Run Model", class = "btn-success", width = "100%", style = "margin-top: 10px;"),
      ),
      column(1)
    ),
    column(
      12,
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
mod_simulation_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    #### REACTIVITY ####
    # display a disabled input for the discharge rate (which depends on followup and admit rates)
    output$OPOutcomeDischarge <- renderUI({
      shinyjs::disabled(sliderInput(
        inputId = "OPOutcomeDischarge", label = "OP outcome: Discharge (%)",
        min = 0, max = 100, value = max(0, (100 - input$numOPOutcomeFup - input$numOPOutcomeAdmit))
      ))
    })

    # handle the cases where discharge rate is already zero
    observeEvent(input$numOPOutcomeFup, {
      if (input$numOPOutcomeFup + input$numOPOutcomeAdmit > 100) {
        updateSliderInput(session = session, inputId = "numOPOutcomeAdmit", value = 100 - input$numOPOutcomeFup)
      }
    })
    observeEvent(input$numOPOutcomeAdmit, {
      if (input$numOPOutcomeFup + input$numOPOutcomeAdmit > 100) {
        updateSliderInput(session = session, inputId = "numOPOutcomeFup", value = 100 - input$numOPOutcomeAdmit)
      }
    })


    # prepare the config object
    model_config <- reactive(
      list(
        forecast_length = input$numForecastLength,
        pat_referral_rate = input$numPatReferralRate,
        pat_backlog_size = input$numPatBacklogSize,
        op_dna_rate = input$numOPDNA,
        op_admit_rate = input$numOPOutcomeAdmit,
        op_fup_rate = input$numOPOutcomeFup,
        op_clinic_length = input$numOpClinicLength,
        total_beds = input$numBeds,
        pre_op_los = input$numPreOpLos,
        post_op_los = input$numPostOpLos,
        theatre_proc_length = input$numTheatreProcLength
      )
    )

    # pathway diagram
    output$pathwayDiagram <- DiagrammeR::renderGrViz(DiagrammeR::grViz(
      '
      digraph {

        # graph attributes
        graph [layout = dot,
                rankdir = LR,
                fontname = Arial,
                label = "Patient Pathway",
                labelloc = t,
                ordering = out]

        # circle nodes
        node [shape = circle,
              color = black,
              style = filled,
              fillcolor = White;
              width = 1.2]
              A;F;

        # rectangle nodes
        node [shape = box,
              color = black,
              style = filled,
              fillcolor = Linen,
              height = 0.8,
              width = 1.5
              ]
              B;C;D;E;

        # nodes for comments (easier to place than edge comments)
        node [  shape = plain
                style = ""]
                c1;c2

        # edge attributes
        edge [  color = black,
                minlen = 2]

        # node statements
        A [label = "Patient \n Referral"];
        B [label = "Outpatient \n Appointment"];
        C [label = "Pre-Op Bed"];
        D [label = "Operating \n Theatre"];
        E [label = "Post-Op Bed"];
        F [label = "Discharge \n Home"];
        c1 [label = "Followup \n arranged"];
        c2 [label = "DNA \n (clinic slot \nwasted)"];

        # edge statements
        A->B;
        B:e->C;
        {
            rank=same
            B:ne->c2:e;
            c2:w->B:nw;
            B:se->c1:e;
            c1:w->B:sw;

        }
        B:e->F;
        C->D;
        D->E;
        E->F;

      }
      '
    ))

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
          scale_x_continuous(name = "Days", labels = scales::number_format(scale = 1 / 60 / 24)) + # format labels to represent days
          labs(
            title = "Queue size",
            y = "Number of patients"
          ) +
          theme_minimal(base_size = 16) +
          theme(legend.position = "none")
      )
      output$serverPlot <- renderPlot(
        plot(sim_resources, metric = "usage", items = "server", steps = TRUE) +
          scale_x_continuous(name = "Days", labels = scales::number_format(scale = 1 / 60 / 24)) + # format labels to represent days
          scale_color_manual(values = "lightgreen") +
          labs(
            subtitle = "Dotted line = max capacity, Solid line = actual usage",
            y = "Used"
          ) +
          theme_minimal(base_size = 16) +
          theme(legend.position = "none")
      )
      output$utilisationPlot <- renderPlot(
        plot(sim_resources, metric = "utilization") +
          labs(
            title = "Resource utilisation",
            x = "Resource",
            y = "Utilisation"
          ) +
          theme_minimal(base_size = 16)
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
