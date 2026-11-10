SEV_PAL <- c(MILD = "#F2C14E", MODERATE = "#F08A24", SEVERE = "#C0392B",
             UNKNOWN = "#9E9E9E", "CON MED" = "#5B8DB8", EXPOSURE = "#2E8B57")

# Add any missing (optional) columns as NA so code never fails on uploads
ensure_cols <- function(df, cols) {
  for (m in setdiff(cols, names(df))) df[[m]] <- NA
  df
}

# CDISC study day: day 1 = first dose, no day 0
study_day <- function(date, ref) {
  d <- as.numeric(as.Date(date) - as.Date(ref))
  ifelse(is.na(d), NA_real_, ifelse(d >= 0, d + 1, d))
}

app_theme <- function() {
  sans <- bslib::font_collection(bslib::font_google("Inter", local = FALSE),
                                 "Segoe UI", "Roboto", "Helvetica Neue", "Arial", "sans-serif")
  bslib::bs_theme(
    version = 5,
    primary = "#0B5394", secondary = "#5B6770", success = "#2E8540",
    info = "#17768F", warning = "#E69F00", danger = "#C0392B",
    base_font = sans, heading_font = sans,
    code_font = bslib::font_collection(bslib::font_google("JetBrains Mono", local = FALSE),
                                       "Consolas", "monospace"),
    "border-radius" = "0.6rem"
  ) |>
    bslib::bs_add_rules("
      .card-header { font-weight: 600; }
      .value-box-title { font-size: .78rem; text-transform: uppercase; letter-spacing: .04em; }
      .sev-badge { padding: 2px 8px; border-radius: 10px; font-size: .78em; font-weight: 600; }
    ")
}

rt_theme <- function(dark = FALSE) {
  if (dark) {
    reactable::reactableTheme(
      color = "#E9ECEF", backgroundColor = "#212529", borderColor = "#3A3F44",
      stripedColor = "#2B3035", highlightColor = "#343A40",
      inputStyle = list(backgroundColor = "#2B3035", color = "#E9ECEF"),
      selectStyle = list(backgroundColor = "#2B3035", color = "#E9ECEF"),
      pageButtonStyle = list(color = "#E9ECEF")
    )
  } else {
    reactable::reactableTheme(stripedColor = "#F7F9FC", highlightColor = "#EEF3FA",
                              borderColor = "#E5E7EB")
  }
}

severity_badge <- function(v) {
  key <- toupper(dplyr::coalesce(as.character(v), "UNKNOWN"))
  bg  <- if (key %in% names(SEV_PAL)) SEV_PAL[[key]] else SEV_PAL[["UNKNOWN"]]
  fg  <- if (key == "MILD") "#212529" else "#FFFFFF"
  htmltools::span(class = "sev-badge", style = sprintf("background:%s;color:%s;", bg, fg), key)
}

csv_download <- function(data_fun, file_fun) {
  shiny::downloadHandler(
    filename = file_fun,
    content  = function(file) utils::write.csv(data_fun(), file, row.names = FALSE, na = "")
  )
}