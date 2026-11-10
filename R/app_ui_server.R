app_ui <- function(request) {
  page_sidebar(
    title = div(class = "d-flex justify-content-between align-items-center w-100",
                span(bsicons::bs_icon("person-vcard"), " Patient Profile Explorer"),
                input_dark_mode(id = "dark_mode", mode = "light")),
    theme = app_theme(),
    fillable = FALSE,
    sidebar = sidebar(width = 320, mod_data_ui("data"), hr(), mod_subject_ui("subject")),
    useBusyIndicators(),
    mod_header_ui("header"),
    navset_card_underline(
      nav_panel("Timeline", icon = bsicons::bs_icon("bar-chart-steps"), mod_timeline_ui("timeline")),
      nav_panel("Adverse events", icon = bsicons::bs_icon("exclamation-octagon"), mod_ae_ui("ae")),
      nav_panel("Labs", icon = bsicons::bs_icon("droplet"), mod_bds_ui("lab")),
      nav_panel("Vitals", icon = bsicons::bs_icon("heart-pulse"), mod_bds_ui("vs")),
      nav_panel("Con meds", icon = bsicons::bs_icon("capsule-pill"), mod_cm_ui("cm")),
      nav_panel("Review notes", icon = bsicons::bs_icon("chat-left-text"), mod_review_ui("review")),
      nav_spacer(),
      nav_panel("About", icon = bsicons::bs_icon("info-circle"),
                markdown("**Exploratory review tool.** Outputs are not validated deliverables and must not
                  be used for submission. Study day = date - TRTSDT + 1 (no day 0)."))
    )
  )
}

app_server <- function(input, output, session) {
  dark    <- reactive(identical(input$dark_mode, "dark"))
  data    <- mod_data_server("data")
  usubjid <- mod_subject_server("subject", data)
  subj    <- reactive(subset_subject(data(), usubjid()))
  
  mod_header_server("header", subj)
  mod_timeline_server("timeline", subj, dark)
  mod_ae_server("ae", subj, dark)
  mod_bds_server("lab", subj, "adlb", dark)
  mod_bds_server("vs", subj, "advs", dark)
  mod_cm_server("cm", subj, dark)
  mod_review_server("review", usubjid, dark)
}