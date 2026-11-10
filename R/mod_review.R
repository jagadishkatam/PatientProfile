mod_review_ui <- function(id) {
  ns <- NS(id)
  layout_columns(
    col_widths = c(4, 8),
    card(
      card_header("Add review note"),
      radioButtons(ns("status"), "Status",
                   c("Reviewed - no issues", "Reviewed - follow-up needed", "Query raised")),
      selectInput(ns("area"), "Area",
                  c("General", "Adverse events", "Labs", "Vitals", "Con meds", "Exposure")),
      textAreaInput(ns("note"), "Comment", rows = 4, width = "100%"),
      textInput(ns("reviewer"), "Reviewer initials"),
      actionButton(ns("save"), "Save note", class = "btn-primary", icon = icon("floppy-disk"))
    ),
    card(
      card_header(class = "d-flex justify-content-between align-items-center",
                  "Session review log",
                  downloadButton(ns("dl"), "CSV", class = "btn-sm btn-outline-secondary")),
      reactable::reactableOutput(ns("log")),
      card_footer(class = "small text-muted",
                  "Notes live only in this browser session and are NOT an audit trail. Download before closing.")
    )
  )
}

mod_review_server <- function(id, usubjid, dark) {
  moduleServer(id, function(input, output, session) {
    log <- reactiveVal(dplyr::tibble(timestamp = character(), usubjid = character(),
                                     area = character(), status = character(),
                                     reviewer = character(), comment = character()))
    observeEvent(input$save, {
      req(usubjid())
      if (!nzchar(trimws(input$reviewer))) {
        showNotification("Please enter reviewer initials.", type = "warning")
        return()
      }
      log(dplyr::bind_rows(log(), dplyr::tibble(
        timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"), usubjid = usubjid(),
        area = input$area, status = input$status, reviewer = input$reviewer,
        comment = input$note)))
      updateTextAreaInput(session, "note", value = "")
      showNotification(paste("Note saved for", usubjid()), type = "message")
    })
    output$log <- reactable::renderReactable({
      reactable::reactable(log(), theme = rt_theme(dark()), compact = TRUE, striped = TRUE,
                           searchable = TRUE, defaultSorted = list(timestamp = "desc"))
    })
    output$dl <- csv_download(log, function() paste0("review_log_", Sys.Date(), ".csv"))
  })
}