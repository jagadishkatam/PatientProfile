load_demo_data <- function() {
  list(
    adsl = pharmaverseadam::adsl, adae = pharmaverseadam::adae,
    adcm = pharmaverseadam::adcm, adex = pharmaverseadam::adex,
    adlb = pharmaverseadam::adlb, advs = pharmaverseadam::advs
  )
}

read_domain_file <- function(path, name) {
  ext <- tolower(tools::file_ext(name))
  switch(ext,
         xpt      = haven::read_xpt(path),
         sas7bdat = haven::read_sas(path),
         rds      = readRDS(path),
         csv      = utils::read.csv(path, stringsAsFactors = FALSE),
         stop("Unsupported file type: .", ext)
  )
}

domain_from_filename <- function(name) tolower(tools::file_path_sans_ext(basename(name)))

to_dates <- function(d) {
  dplyr::mutate(d, dplyr::across(dplyr::any_of(c("TRTSDT", "TRTEDT", "ASTDT", "AENDT", "ADT")),
                                 as.Date))
}

derive_days <- function(df, adsl) {
  df <- ensure_cols(df, c("ASTDT", "AENDT", "ASTDY", "AENDY", "ADT", "ADY"))
  df <- dplyr::select(df, -dplyr::any_of("TRTSDT"))
  df |>
    dplyr::left_join(dplyr::select(adsl, USUBJID, TRTSDT), by = "USUBJID") |>
    dplyr::mutate(
      ASTDY = dplyr::coalesce(as.numeric(ASTDY), study_day(ASTDT, TRTSDT)),
      AENDY = dplyr::coalesce(as.numeric(AENDY), study_day(AENDT, TRTSDT)),
      ADY   = dplyr::coalesce(as.numeric(ADY),   study_day(ADT,   TRTSDT))
    )
}

prep_occ <- function(d, adsl, cols) {
  if (is.null(d)) return(NULL)
  d |> ensure_cols(cols) |> derive_days(adsl)
}

prep_bds <- function(d, adsl) {
  if (is.null(d)) return(NULL)
  d |>
    ensure_cols(c("PARAMCD", "PARAM", "AVAL", "AVALU", "BASE", "CHG", "AVISIT", "AVISITN",
                  "ANRLO", "ANRHI", "ANRIND", "ATOXGR")) |>
    derive_days(adsl) |>
    dplyr::mutate(AVAL = as.numeric(AVAL), PARAM = as.character(PARAM))
}

prepare_data <- function(raw) {
  raw <- purrr::map(raw, \(d) if (is.null(d)) NULL else to_dates(dplyr::as_tibble(d)))
  adsl <- raw$adsl |>
    ensure_cols(c("USUBJID", "AGE", "SEX", "RACE", "ARM", "ACTARM", "TRT01A", "SITEID",
                  "COUNTRY", "TRTSDT", "TRTEDT", "EOSSTT", "DTHFL")) |>
    dplyr::mutate(
      TRT    = dplyr::coalesce(as.character(TRT01A), as.character(ACTARM), as.character(ARM)),
      SITEID = as.character(SITEID),
      TRTSDT = as.Date(TRTSDT), TRTEDT = as.Date(TRTEDT)
    )
  list(
    adsl = adsl,
    adae = prep_occ(raw$adae, adsl, c("AEDECOD", "AEBODSYS", "AESEV", "AETOXGR", "AESER",
                                      "AEREL", "AEOUT", "TRTEMFL")),
    adcm = prep_occ(raw$adcm, adsl, c("CMDECOD", "CMCLAS", "CMINDC", "CMDOSE", "CMDOSU",
                                      "CMROUTE", "ONTRTFL")),
    adex = prep_occ(raw$adex, adsl, c("EXTRT", "EXDOSE", "EXDOSU", "PARAMCD")),
    adlb = prep_bds(raw$adlb, adsl),
    advs = prep_bds(raw$advs, adsl)
  )
}

subset_subject <- function(data, id) {
  purrr::map(data, \(d) if (is.null(d)) NULL else dplyr::filter(d, USUBJID == id))
}