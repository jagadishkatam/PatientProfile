mod_subject_ui <- function(id) {
  ns <- NS(id)
  tagList(
    selectInput(ns("arm"), "Treatment arm", choices = NULL, multiple = TRUE),
    selectInput(ns("site"), "Site", choices = NULL, multiple = TRUE),
    checkboxInput(ns("sae_only"), "Only subjects with a serious AE", FALSE),
    selectizeInput(ns("usubjid"), "Subject (USUBJID)", choices = NULL,
                   options = list(placeholder = "Type to search...")),
    div(class = "d-flex gap-2",
        actionButton(ns("prev"), "Prev", icon = icon("chevron-left"),
                     class = "btn-outline-primary btn-sm flex-fill"),
        actionButton(ns("nxt"), "Next", icon = icon("chevron-right"),
                     class = "btn-outline-primary btn-sm flex-fill")),
    div(class = "small text-muted mt-2", textOutput(ns("pos")))
  )
}

mod_subject_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    observeEvent(data(), {
      adsl <- data()$adsl
      updateSelectInput(session, "arm", choices = sort(unique(adsl$TRT)), selected = character(0))
      updateSelectInput(session, "site", choices = sort(unique(adsl$SITEID)), selected = character(0))
    })
    
    pool <- reactive({
      adsl <- data()$adsl
      if (length(input$arm))  adsl <- dplyr::filter(adsl, TRT %in% input$arm)
      if (length(input$site)) adsl <- dplyr::filter(adsl, SITEID %in% input$site)
      if (isTRUE(input$sae_only) && !is.null(data()$adae)) {
        sae_ids <- data()$adae |> dplyr::filter(AESER == "Y") |> dplyr::pull(USUBJID)
        adsl <- dplyr::filter(adsl, USUBJID %in% sae_ids)
      }
      sort(unique(adsl$USUBJID))
    })
    
    observeEvent(pool(), {
      ids <- pool()
      if (!length(ids)) {
        updateSelectizeInput(session, "usubjid", choices = character(0), server = TRUE)
        return()
      }
      cur <- isolate(input$usubjid)
      sel <- if (isTRUE(cur %in% ids)) cur else ids[1]
      updateSelectizeInput(session, "usubjid", choices = ids, selected = sel, server = TRUE)
    })
    
    step <- function(by) {
      ids <- pool()
      i <- match(input$usubjid, ids)
      if (!length(ids) || is.na(i)) return()
      j <- ((i - 1 + by) %% length(ids)) + 1
      updateSelectizeInput(session, "usubjid", choices = ids, selected = ids[j], server = TRUE)
    }
    observeEvent(input$prev, step(-1))
    observeEvent(input$nxt, step(1))
    
    output$pos <- renderText({
      ids <- pool()
      i <- match(input$usubjid, ids)
      sprintf("Subject %s of %s in current filter", ifelse(is.na(i), "-", i), length(ids))
    })
    
    reactive({
      req(isTRUE(input$usubjid %in% pool()))
      input$usubjid
    })
  })
}