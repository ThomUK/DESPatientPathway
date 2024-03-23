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
        p("This model is in development.  Potential future improvements are detailed below."),
        HTML(
          "<ul>
              <li>Need to add an additional class of 'priority' patients (eg. cancer) in parallel to the standard patients. Define the demand and queuing behaviours separately for each class of patient.</li>
            </ul>"
        )
      )
    ),
    fluidRow(
      column(
        12,
        h2("Assumptions"),
        h3("Working patterns"),
        p("The simulation runs on a weekly pattern.  It does not account for holidays, cancelled clinics, or cancelled theatre procedures.  These inefficiencies should be included in the numbers selected by the users.  For example:"),
        HTML(
          "<ul>
              <li>If the OP schedule allows for 30 patients weekly, but historically 10% of clinic slots are cancelled due to consultant holiday, the number of slots input to this model should be 27.
              </li>
              <li>Similarly, if there are nominally 10 operating theatre patient slots, but historic theatre session utilisation is 80%, only 8 slots should be specificed in this model. 
              </li>
          </ul>"
        ),
        h3("Probability distributions and randomness"),
        p("Exponential distribution are used to model the patient arrivals (referral times).  This intentionally introduces randomness into the model, and is the reason that repeat runs do not give identical results."),
        p("The decision points (ie. the outcome of the clinic appointment), are also randomly sampled, introducing further randomness.  The overall proportion of decisions match those specificd by the user in the user interface.  ")
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
