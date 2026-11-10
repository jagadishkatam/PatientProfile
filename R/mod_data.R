mod_data_ui <- function(id) {
  ns <- NS(id)
  tagList(
    radioButtons(ns("source"), "Data source",
                 c("Demo (pharmaverseadam)" = "demo", "Upload ADaM files" = "upload")),
    conditionalPanel(
      "input.source == 'upload'", ns = ns,
      fileInput(ns("files"), "ADSL required; ADAE/ADCM/ADEX/ADLB/ADVS optional",
                multiple = TRUE, accept = c(".xpt", ".sas7bdat", ".rds", ".csv")),
      helpText("Name files by domain, e.g. adsl.xpt, adae.sas7bdat. CSV dates must be ISO 8601.")
    )
  )
}

mod_data_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    reactive({
      if (input$source == "demo") return(DEMO_DATA)
      req(input$files)
      raw <- tryCatch(
        purrr::map2(input$files$datapath, input$files$name, read_domain_file) |>
          purrr::set_names(domain_from_filename(input$files$name)),
        error = function(e) e
      )
      validate(need(!inherits(raw, "error"), paste("Could not read files:", conditionMessage(raw))))
      validate(need("adsl" %in% names(raw), "Please include an ADSL file (e.g. adsl.xpt)."))
      prepare_data(raw)
    })
  })
}