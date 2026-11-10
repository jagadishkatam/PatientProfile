mod_bds_ui <- function(id) {
  ns <- NS(id)
  layout_sidebar(
    fillable = FALSE,
    sidebar = sidebar(
      width = 270, position = "right",
      selectizeInput(ns("params"), "Parameters", choices = NULL, multiple = TRUE),
      radioButtons(ns("xvar"), "X axis", c("Study day" = "ADY", "Visit number" = "AVISITN"),
                   inline = TRUE),
      radioButtons(ns("yvar"), "Y axis", c("Value" = "AVAL", "Change from baseline" = "CHG",
                                           "Multiple of ULN" = "ULN")),
      input_switch(ns("range"), "Show normal range", TRUE),
      input_switch(ns("ae_overlay"), "Overlay AE onsets (study day only)", TRUE)
    ),
    card(full_screen = TRUE, card_header("Trends"),
         plotly::plotlyOutput(ns("plot"), height = "540px")),
    listing_card(ns, "Listing")
  )
}

mod_bds_server <- function(id, subj, domain, dark) {
  moduleServer(id, function(input, output, session) {
    d_all <- reactive({
      d <- subj()[[domain]]
      validate(need(!is.null(d) && nrow(d) > 0,
                    sprintf("No %s data for this subject.", toupper(domain))))
      dplyr::filter(d, !is.na(AVAL))
    })
    
    observeEvent(d_all(), {
      p <- sort(unique(d_all()$PARAM))
      keep <- intersect(isolate(input$params), p)
      if (!length(keep)) keep <- utils::head(p, 4)
      updateSelectizeInput(session, "params", choices = p, selected = keep)
    })
    
    plot_data <- reactive({
      req(input$params)
      yv <- input$yvar
      d <- d_all() |>
        dplyr::filter(PARAM %in% input$params) |>
        dplyr::mutate(
          X = as.numeric(.data[[input$xvar]]),
          ANRLO = as.numeric(ANRLO), ANRHI = as.numeric(ANRHI),
          Y  = dplyr::case_when(yv == "CHG" ~ as.numeric(CHG), yv == "ULN" ~ AVAL / ANRHI,
                                TRUE ~ AVAL),
          LO = dplyr::case_when(yv == "AVAL" ~ ANRLO, yv == "ULN" ~ ANRLO / ANRHI,
                                TRUE ~ NA_real_),
          HI = dplyr::case_when(yv == "AVAL" ~ ANRHI, yv == "ULN" ~ 1, TRUE ~ NA_real_),
          flag = dplyr::case_when(toupper(ANRIND) %in% c("HIGH", "H") ~ "High",
                                  toupper(ANRIND) %in% c("LOW", "L") ~ "Low",
                                  TRUE ~ "Normal"),
          tip = sprintf("<b>%s</b><br>%s (day %s)<br>Value: %s %s<br>Range: %s - %s<br>Flag: %s",
                        PARAM, AVISIT, ADY, AVAL, dplyr::coalesce(as.character(AVALU), ""),
                        ANRLO, ANRHI, flag)
        ) |>
        dplyr::filter(!is.na(X), !is.na(Y)) |>
        dplyr::arrange(PARAM, X)
      validate(need(nrow(d) > 0, "Selected axis variables are not available for these parameters."))
      d
    })
    
    output$plot <- plotly::renderPlotly({
      ae <- subj()$adae
      ae_days <- if (isTRUE(input$ae_overlay) && input$xvar == "ADY" && !is.null(ae))
        unique(stats::na.omit(ae$ASTDY)) else NULL
      ylab <- c(AVAL = "Value", CHG = "Change from baseline", ULN = "x ULN")[[input$yvar]]
      xlab <- if (input$xvar == "ADY") "Study day" else "Visit number"
      plot_bds(plot_data(), ae_days, isTRUE(input$range), xlab, ylab, dark())
    })
    
    listing <- reactive({
      d_all() |> dplyr::arrange(PARAM, ADY) |>
        dplyr::select(PARAM, AVISIT, ADY, AVAL, BASE, CHG, ANRLO, ANRHI, ANRIND, ATOXGR)
    })
    output$tbl <- reactable::renderReactable({
      reactable::reactable(
        listing(), theme = rt_theme(dark()), searchable = TRUE, filterable = TRUE,
        striped = TRUE, compact = TRUE, defaultPageSize = 15,
        columns = list(
          PARAM  = reactable::colDef(name = "Parameter", minWidth = 200),
          ANRIND = reactable::colDef(name = "Ref. ind.", style = function(v) {
            v <- toupper(v)
            if (isTRUE(v %in% c("HIGH", "H"))) list(color = "#C0392B", fontWeight = 600)
            else if (isTRUE(v %in% c("LOW", "L"))) list(color = "#E69F00", fontWeight = 600)
            else NULL
          })
        )
      )
    })
    output$dl <- csv_download(listing, function() paste0(subj()$adsl$USUBJID[1], "_", domain, ".csv"))
  })
}