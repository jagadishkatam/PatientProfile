listing_card <- function(ns, title) {
  card(
    full_screen = TRUE,
    card_header(class = "d-flex justify-content-between align-items-center", title,
                downloadButton(ns("dl"), "CSV", class = "btn-sm btn-outline-secondary")),
    reactable::reactableOutput(ns("tbl"))
  )
}

mod_ae_ui <- function(id) listing_card(NS(id), "Adverse events")

mod_ae_server <- function(id, subj, dark) {
  moduleServer(id, function(input, output, session) {
    d <- reactive({
      ae <- subj()$adae
      validate(need(!is.null(ae) && nrow(ae) > 0, "No adverse events recorded for this subject."))
      ae |> dplyr::arrange(ASTDY) |>
        dplyr::select(AEBODSYS, AEDECOD, ASTDY, AENDY, AESEV, AETOXGR, AESER, AEREL, AEOUT, TRTEMFL)
    })
    output$tbl <- reactable::renderReactable({
      reactable::reactable(
        d(), theme = rt_theme(dark()), searchable = TRUE, striped = TRUE, highlight = TRUE,
        compact = TRUE, defaultPageSize = 15,
        columns = list(
          AEBODSYS = reactable::colDef(name = "System organ class", minWidth = 180),
          AEDECOD  = reactable::colDef(name = "Preferred term", minWidth = 160),
          ASTDY    = reactable::colDef(name = "Start day", width = 85),
          AENDY    = reactable::colDef(name = "End day", width = 85),
          AESEV    = reactable::colDef(name = "Severity", width = 110, cell = severity_badge),
          AETOXGR  = reactable::colDef(name = "Grade", width = 70),
          AESER    = reactable::colDef(name = "Serious", width = 80, cell = function(v)
            if (identical(v, "Y")) htmltools::span(class = "badge bg-danger", "SAE") else "-"),
          AEREL    = reactable::colDef(name = "Related"),
          AEOUT    = reactable::colDef(name = "Outcome", minWidth = 140),
          TRTEMFL  = reactable::colDef(name = "TEAE", width = 65)
        )
      )
    })
    output$dl <- csv_download(d, function() paste0(subj()$adsl$USUBJID[1], "_adae.csv"))
  })
}

mod_cm_ui <- function(id) listing_card(NS(id), "Concomitant medications")

mod_cm_server <- function(id, subj, dark) {
  moduleServer(id, function(input, output, session) {
    d <- reactive({
      cm <- subj()$adcm
      validate(need(!is.null(cm) && nrow(cm) > 0, "No concomitant medications for this subject."))
      cm |> dplyr::arrange(ASTDY) |>
        dplyr::select(CMDECOD, CMCLAS, CMINDC, CMDOSE, CMDOSU, CMROUTE, ASTDY, AENDY, ONTRTFL)
    })
    output$tbl <- reactable::renderReactable({
      reactable::reactable(
        d(), theme = rt_theme(dark()), searchable = TRUE, striped = TRUE, highlight = TRUE,
        compact = TRUE, defaultPageSize = 15,
        columns = list(
          CMDECOD = reactable::colDef(name = "Medication", minWidth = 160),
          CMCLAS  = reactable::colDef(name = "Class", minWidth = 140),
          CMINDC  = reactable::colDef(name = "Indication", minWidth = 140),
          ASTDY   = reactable::colDef(name = "Start day", width = 85),
          AENDY   = reactable::colDef(name = "End day", width = 85),
          ONTRTFL = reactable::colDef(name = "On-treatment", width = 105, cell = function(v)
            if (identical(v, "Y")) htmltools::span(class = "badge bg-primary", "Y") else "-")
        )
      )
    })
    output$dl <- csv_download(d, function() paste0(subj()$adsl$USUBJID[1], "_adcm.csv"))
  })
}