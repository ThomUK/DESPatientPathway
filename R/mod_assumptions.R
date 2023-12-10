#' assumptions UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_assumptions_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        12,
        h2("Source Code"),
        HTML("<p>The code for this project is open-source, and is on <a href='https://github.com/ThomUK/DESPatientPathway' target='_blank'>GitHub.</a></p>")
      )
    ),
    fluidRow(
      column(
        width = 12,
        h2("Notes"),
        p("This model is in development.  It is not yet ready to be used for planning.  Some of the issues to be resolved are detailed below."),
        HTML(
          "<ul>
              <li>Is the main queue appearing in the right place?  Is the typical real waiting list post referral and pre OP appointment, or is it post OP appt and pre-admission.</li>
              <li>How to visualise the queue between OP appt and hospital admission?</li>
              <li>The ward is modelled as a shared pre and post-operative ward, but the priority and behaviour of post-op vs. pre-op patients needs to be checked.</li>
              <li>Need to add an additional class of 'priority' patients (eg. cancer) in parallel to the standard patients. Define the queuing behaviour.</li>
              <li>The OP clinic and Theatre schedules are hard-coded as 8hrs/day, 7days/week.  This needs to be user-configurable.</li>
            </ul>"
        )
      )
    ),
    fluidRow(
      column(
        12,
        h2("Assumptions"),
        h3("Probability distributions"),
        p("Exponential distributions are used to model: Patient arrivals (referral times), ward bed length of stay, and theatre length of procedure."),
        p("In the case of ward and theatre length of stay, this is for simplicity.  A log-normal or similar distribution may fit better, but would require tuning of 2 parameters to fit."),
        p("There is no probability distribution modelled for OP clinic lengths.  These are assumed to be fixed and regular timings.")
      )
    ),
  )
}

#' assumptions Server Functions
#'
#' @noRd
mod_assumptions_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
  })
}

## To be copied in the UI
# mod_assumptions_ui("assumptions_1")

## To be copied in the server
# mod_assumptions_server("assumptions_1")
