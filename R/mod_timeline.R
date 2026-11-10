mod_timeline_ui <- function(id) {
  ns <- NS(id)
  card(
    full_screen = TRUE,
    card_header(class = "d-flex justify-content-between align-items-center",
                "Subject timeline",
                checkboxGroupInput(ns("domains"), NULL, inline = TRUE,
                                   choices = c("Exposure", "Adverse event", "Con med"),
                                   selected = c("Exposure", "Adverse event", "Con med"))),
    card_body(plotly::plotlyOutput(ns("plot"), height = "560px"))
  )
}

mod_timeline_server <- function(id, subj, dark) {
  moduleServer(id, function(input, output, session) {
    output$plot <- plotly::renderPlotly({
      df <- build_timeline_data(subj())
      validate(need(nrow(df) > 0, "No AE, con-med or exposure records with study days for this subject."))
      df <- dplyr::filter(df, domain %in% input$domains)
      validate(need(nrow(df) > 0, "Select at least one domain."))
      plot_timeline(df, subj()$adsl, dark())
    })
  })
}