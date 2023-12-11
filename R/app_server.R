#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic
  mod_simulation_server("simulation_1")
  mod_assumptions_server("assumptions_1")

  output$news_changelog <- renderUI({
    suppressWarnings(
      HTML(
        readLines(
          rmarkdown::render(
            'NEWS.md',
            output_format = "html_document",
            runtime = "shiny",
            quiet = TRUE
          )
        )
      )
    )
  })

}
