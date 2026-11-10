theme_profile <- function(dark = FALSE) {
  fg   <- if (dark) "#E9ECEF" else "#212529"
  grid <- if (dark) "#3A3F44" else "#E5E7EB"
  ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      text = ggplot2::element_text(colour = fg),
      axis.text = ggplot2::element_text(colour = fg),
      strip.text = ggplot2::element_text(colour = fg, face = "bold"),
      panel.grid.major = ggplot2::element_line(colour = grid),
      panel.grid.minor = ggplot2::element_blank(),
      legend.position = "bottom"
    )
}

plotly_profile <- function(p, dark = FALSE) {
  fg <- if (dark) "#E9ECEF" else "#212529"
  plotly::ggplotly(p, tooltip = "text") |>
    plotly::layout(paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
                   font = list(color = fg), legend = list(orientation = "h", y = -0.15)) |>
    plotly::config(displaylogo = FALSE,
                   toImageButtonOptions = list(format = "png", scale = 2))
}

build_timeline_data <- function(s) {
  out <- list()
  ae <- s$adae; cm <- s$adcm; ex <- s$adex
  if (!is.null(ae) && nrow(ae)) {
    out$ae <- dplyr::transmute(ae,
                               domain = "Adverse event", term = as.character(AEDECOD), start = ASTDY, end = AENDY,
                               cat = toupper(dplyr::coalesce(as.character(AESEV), "UNKNOWN")),
                               serious = AESER %in% "Y",
                               detail = sprintf("SOC: %s<br>Serious: %s | Related: %s<br>Outcome: %s",
                                                AEBODSYS, AESER, AEREL, AEOUT))
  }
  if (!is.null(cm) && nrow(cm)) {
    out$cm <- dplyr::transmute(cm,
                               domain = "Con med", term = as.character(CMDECOD), start = ASTDY, end = AENDY,
                               cat = "CON MED", serious = FALSE,
                               detail = sprintf("Indication: %s<br>Route: %s", CMINDC, CMROUTE))
  }
  if (!is.null(ex) && nrow(ex)) {
    if (any(ex$PARAMCD %in% "DOSE")) ex <- dplyr::filter(ex, PARAMCD == "DOSE")
    out$ex <- ex |>
      dplyr::distinct(EXTRT, EXDOSE, EXDOSU, ASTDY, AENDY) |>
      dplyr::transmute(domain = "Exposure", term = as.character(EXTRT), start = ASTDY,
                       end = AENDY, cat = "EXPOSURE", serious = FALSE,
                       detail = sprintf("Dose: %s %s", EXDOSE, EXDOSU))
  }
  df <- dplyr::bind_rows(out)
  if (!nrow(df)) return(df)
  df <- dplyr::filter(df, !is.na(start))
  if (!nrow(df)) return(df)
  max_day <- max(c(df$start, df$end), na.rm = TRUE)
  dplyr::mutate(df, ongoing = is.na(end), end = dplyr::coalesce(end, max_day),
                row = paste0(domain, ": ", term))
}

plot_timeline <- function(df, adsl, dark = FALSE) {
  lvl <- df |>
    dplyr::group_by(row) |>
    dplyr::summarise(d = dplyr::first(domain), s = min(start), .groups = "drop") |>
    dplyr::arrange(factor(d, c("Exposure", "Adverse event", "Con med")), s) |>
    dplyr::pull(row)
  df <- dplyr::mutate(df,
                      row = factor(row, levels = rev(lvl)),
                      tip = sprintf("<b>%s</b><br>Day %s to %s<br>%s", term, start,
                                    ifelse(ongoing, "ongoing", end), detail))
  trt_end <- study_day(adsl$TRTEDT[1], adsl$TRTSDT[1])
  
  p <- ggplot2::ggplot(df, ggplot2::aes(x = start, xend = end, y = row, yend = row,
                                        colour = cat, text = tip)) +
    ggplot2::geom_vline(xintercept = 1, linetype = "dashed", colour = "#2E8540") +
    ggplot2::geom_segment(linewidth = 4, lineend = "round") +
    ggplot2::geom_point(size = 2) +
    ggplot2::geom_point(data = dplyr::filter(df, serious),
                        shape = 18, size = 4.5, colour = "#7B1FA2") +
    ggplot2::scale_colour_manual(values = SEV_PAL, name = NULL) +
    ggplot2::labs(x = "Study day (day 1 = first dose; purple diamond = serious AE)", y = NULL) +
    theme_profile(dark)
  if (!is.na(trt_end)) {
    p <- p + ggplot2::geom_vline(xintercept = trt_end, linetype = "dotted", colour = "#5B6770")
  }
  plotly_profile(p, dark)
}

plot_bds <- function(d, ae_days = NULL, show_range = TRUE, xlab = "", ylab = "", dark = FALSE) {
  p <- ggplot2::ggplot(d, ggplot2::aes(x = X, y = Y))
  if (length(ae_days)) {
    p <- p + ggplot2::geom_vline(xintercept = ae_days, colour = "#C0392B",
                                 alpha = 0.35, linetype = "dotted")
  }
  if (show_range) {
    p <- p +
      ggplot2::geom_line(ggplot2::aes(y = LO, group = PARAM), colour = "#2E8540",
                         linetype = "dashed", na.rm = TRUE) +
      ggplot2::geom_line(ggplot2::aes(y = HI, group = PARAM), colour = "#2E8540",
                         linetype = "dashed", na.rm = TRUE)
  }
  p +
    ggplot2::geom_line(ggplot2::aes(group = PARAM), colour = "#0B5394", na.rm = TRUE) +
    ggplot2::geom_point(ggplot2::aes(colour = flag, text = tip), size = 2.4, na.rm = TRUE) +
    ggplot2::scale_colour_manual(values = c(Normal = "#0B5394", High = "#C0392B",
                                            Low = "#E69F00"), name = NULL) +
    ggplot2::facet_wrap(~PARAM, scales = "free_y", ncol = 2) +
    ggplot2::labs(x = xlab, y = ylab) +
    theme_profile(dark) |>
    identity() -> p
  plotly_profile(p, dark)
}