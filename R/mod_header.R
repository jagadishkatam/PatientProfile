mod_header_ui <- function(id) uiOutput(NS(id, "boxes"))

mod_header_server <- function(id, subj) {
  moduleServer(id, function(input, output, session) {
    output$boxes <- renderUI({
      s  <- subj()
      dm <- s$adsl[1, ]
      ae <- s$adae
      n_teae <- if (is.null(ae) || !nrow(ae)) 0 else if (all(is.na(ae$TRTEMFL))) nrow(ae) else sum(ae$TRTEMFL %in% "Y")
      n_ser  <- if (is.null(ae)) 0 else sum(ae$AESER %in% "Y")
      n_sev  <- if (is.null(ae)) 0 else sum(toupper(ae$AESEV) %in% "SEVERE")
      dur    <- as.numeric(dm$TRTEDT - dm$TRTSDT) + 1
      status <- if (dm$DTHFL %in% "Y") "DIED" else dplyr::coalesce(as.character(dm$EOSSTT), "Unknown")
      
      layout_column_wrap(
        width = "220px", fill = FALSE, heights_equal = "row",
        value_box("Subject", dm$USUBJID,
                  sprintf("%s y | %s | %s", dm$AGE, dm$SEX, dm$RACE),
                  showcase = bsicons::bs_icon("person-badge"), theme = "primary"),
        value_box("Treatment", dm$TRT, sprintf("Site %s | %s", dm$SITEID, dm$COUNTRY),
                  showcase = bsicons::bs_icon("capsule"), theme = "secondary"),
        value_box("Exposure", ifelse(is.na(dur), "n/a", paste(dur, "days")),
                  sprintf("%s to %s", format(dm$TRTSDT), format(dm$TRTEDT)),
                  showcase = bsicons::bs_icon("calendar-range"), theme = "info"),
        value_box("TEAEs", n_teae, sprintf("%s serious | %s severe", n_ser, n_sev),
                  showcase = bsicons::bs_icon("exclamation-triangle"),
                  theme = if (n_ser > 0) "danger" else "success"),
        value_box("Study status", status, "End of study status (ADSL)",
                  showcase = bsicons::bs_icon("flag"),
                  theme = if (status == "DIED") "danger" else "light")
      )
    })
  })
}