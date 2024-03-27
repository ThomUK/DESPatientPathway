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
        h4("Demand:"),
        div(
          style = "border: 2px solid #ddd; border-radius: 5px; padding: 10px; margin-top: 5px;",
          sliderInput(NS(id, "numPatReferralRate"), "Number of new patient referrals (monthly)", value = 100, min = 0, max = 1000),
          sliderInput(NS(id, "numPatBacklogSize"), "Number of existing patients in the OP clinic backlog", value = 0, min = 0, max = 5000),
        )
      )
    ),
    fluidRow(
      column(
        width = 6,
        h4("Capacity:"),
        div(
          style = "border: 2px solid #ddd; border-radius: 5px; padding: 10px; margin-top: 5px;",
          sliderInput(NS(id, "numOpClinicSlots"), "Number of OP clinic slots (patients per week)", value = 25, min = 0, max = 1400),
          sliderInput(NS(id, "numTheatreSlots"), "Number of theatre slots (patients per week)", value = 4, min = 0, max = 200),
          numericInput(NS(id, "numBeds"), "Total beds (pre & post-operative combined)", value = 6, min = 1, max = 36),
        )
      )
    ),
    fluidRow(
      column(
        width = 6,
        h4("Service performance:"),
        div(
          style = "border: 2px solid #ddd; border-radius: 5px; padding: 10px;",
          sliderInput(NS(id, "numOPDNA"), "OP DNA rate (%)", value = 10, min = 0, max = 100),
          sliderInput(NS(id, "numOPOutcomeFup"), "OP outcome: Book followup (%)", value = 25, min = 0, max = 100),
          sliderInput(NS(id, "numOPOutcomeAdmit"), "OP outcome: Admit (%)", value = 10, min = 0, max = 100),
          uiOutput(NS(id, "OPOutcomeDischarge")), # a shinyjs output
          sliderInput(NS(id, "numPreOpLos"), "Average pre-operative length of stay (days)", value = 0.2, min = 0, max = 7, step = 0.1),
          sliderInput(NS(id, "numPostOpLos"), "Average post-operative length of stay (days)", value = 1.8, min = 0, max = 42, step = 0.1)
        )
      )
    ),
    fluidRow(
      column(
        width = 6,
        h4("Model options:"),
        div(
          style = "border: 2px solid #ddd; border-radius: 5px; padding: 10px;",
          sliderInput(NS(id, "numForecastLength"), "Future period to forecast (weeks)", value = 52, min = 1, max = 208),
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
      textOutput(NS(id, "configDetails1")),
      hr(),
      plotOutput(NS(id, "serverPlot")),
      textOutput(NS(id, "configDetails2")),
      hr(),
      plotOutput(NS(id, "utilisationPlot")),
      textOutput(NS(id, "configDetails3")),
      hr(),
      h4("Simulation results:"),
      p("This table shows the simulation results patient by patient.  Use the search box to search for an event, or a patient."),
      dataTableOutput(NS(id, "attributesTable")),
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
        pat_referral_rate = input$numPatReferralRate,
        pat_backlog_size = input$numPatBacklogSize,
        op_clinic_slots = input$numOpClinicSlots,
        theatre_slots = input$numTheatreSlots,
        total_beds = input$numBeds,
        op_dna_rate = input$numOPDNA,
        op_admit_rate = input$numOPOutcomeAdmit,
        op_fup_rate = input$numOPOutcomeFup,
        pre_op_los = input$numPreOpLos,
        post_op_los = input$numPostOpLos,
        forecast_length = input$numForecastLength
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
      patient_trajectory <- res$patient_trajectory

      # compute some stats
      sim_resources <- sim |>
        get_mon_resources() |>
        # specify factor levels so that the facet plot ordering is correct
        dplyr::mutate(
          resource = factor(resource, levels = c("OP Clinic", "Theatre", "Bed"))
        )

      sim_attributes <- sim |>
        get_mon_attributes() |>
        dplyr::select(
          Week = time,
          Patient = name,
          Event = key
        ) |>
        dplyr::mutate(
          Day = round(Week * 7, 2),
          .after = "Week"
        )

      # rename the config details to be human-readable, then collapse them into a string
      config_details <- listr::list_rename(
        model_config(),
        `Patient referrals (per month)` = pat_referral_rate,
        `Patient backlog size` = pat_backlog_size,
        `OP clinic slots (patients per week)` = op_clinic_slots,
        `Theatre slots (patients per week)` = theatre_slots,
        `Number of beds` = total_beds,
        `OP DNA rate (%)` = op_dna_rate,
        `OP admission rate (%)` = op_admit_rate,
        `OP followup rate (%)` = op_fup_rate,
        `Pre-op LOS (days)` = pre_op_los,
        `Post-op LOS (days)` = post_op_los,
        `Simulation length (weeks)` = forecast_length
      )
      config_details <- paste(
          names(config_details),
          config_details,
          sep = ": ",
          collapse = ", "
        )

      # create a text summary of the model config
      # needs to store in 3 objects to avoid multiple html elements with same ID
      output$configDetails1 <- output$configDetails2 <- output$configDetails3 <- renderText(config_details)

      # make a plot
      output$queuePlot <- renderPlot(
        ggplot2::ggplot(sim_resources, ggplot2::aes(time, queue)) +
            ggplot2::geom_line(colour = "firebrick", alpha = 0.7) +
            ggplot2::facet_wrap(ggplot2::vars(resource), scale = "free_y") +
          labs(
            title = "Queue sizes for each resource",
            subtitle = "Are the queue sizes stable?  Do backlogs build up or reduce over time?",
            x = "Weeks",
            y = "Number of queueing patients"
          ) +
          theme_minimal(base_size = 16) +
          theme(legend.position = "none")
      )
      output$serverPlot <- renderPlot(
        ggplot(sim_resources, aes(time)) +
          geom_line(aes(y = capacity), colour = "firebrick", alpha = 0.7, size = 2, linetype = "43") +
          geom_line(aes(y = server), colour = "lightgreen", alpha = 0.9, size = 0.5) +
          facet_wrap(vars(resource), scales = "free_y") +
          labs(
            title = "Resource usage",
            subtitle = "Red line = capacity, Green line = actual usage",
            x = "Weeks",
            y = "Resource units used (number of clinics, beds, etc)"
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
      output$attributesTable <- renderDataTable(sim_attributes)
      output$trajectoryPlot <- DiagrammeR::renderGrViz(
        plot(patient_trajectory, verbose = TRUE)
      )
    })
  })
}

## To be copied in the UI
# mod_simulation_ui("simulation_1")

## To be copied in the server
# mod_simulation_server("simulation_1")
