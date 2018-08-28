
#' Read a Panalytical spectra file into a spectra object
#'
#' @param path A filename or vector of filenames that correspond to Panalytical spectrum files
#'   (usually a .mp2 file)
#' @param parse_meta Parse meta information into numerics where applicable?
#'
#' @return A spectra object
#' @export
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @examples
#' pan_dir <- system.file("spectra_files/Panalytical", package = "xrf")
#' read_xrf_panalytical(file.path(pan_dir, "Panalytical_2017-08-1632-Omnian.mp2"))
#'
read_xrf_panalytical <- function(path, parse_meta = TRUE) {
  meta_df <- read_xrf_meta_panalytical(path, parse_meta = parse_meta)
  spect_df <- read_xrf_spectra_panalytical(path)
  meta_df$.spectra <- spect_df$.spectra
  meta_df
}

#' @rdname read_xrf_panalytical
#' @export
read_xrf_meta_panalytical <- function(path, parse_meta = TRUE) {
  stopifnot(is.character(path), all(file.exists(path)))
  if(length(path) == 0) {
    return(tibble::tibble(.path = character(0), .position = integer(0)))
  } else if(length(path) > 1) {
    tbl <- purrr::map_dfr(path, read_xrf_meta_panalytical, parse_meta = FALSE)
    if(parse_meta) {
      return(panalytical_parse_meta(tbl))
    } else {
      return(tbl)
    }
  }

  # read header info
  header <- readr::read_lines(path, n_max = 100)

  # number of header lines = first line without a colon plus 1
  n_header_lines <- which(!stringr::str_detect(header, ":"))[1] - 1
  stopifnot(is.finite(n_header_lines))
  header_df <- tibble::tibble(line = utils::head(header, n_header_lines)) %>%
    tidyr::separate(.data$line, c("key", "value"), ":", extra = "merge")

  meta_df <- rlang::set_names(
    stringr::str_trim(header_df$value),
    tibble::tidy_names(stringr::str_trim(header_df$key), quiet = TRUE)
  ) %>%
    as.list() %>%
    tibble::as_tibble() %>%
    dplyr::mutate(.path = path, .position = 1L)

  if(parse_meta) {
    panalytical_parse_meta(meta_df)
  } else {
    meta_df
  }
}

#' @rdname read_xrf_panalytical
#' @export
read_xrf_spectra_panalytical <- function(path) {
  stopifnot(is.character(path), all(file.exists(path)))
  if(length(path) == 0) {
    return(tibble::tibble(.path = character(0), .position = 1L, .spectra = list()))
  } else if(length(path) > 1) {
    return(purrr::map_dfr(path, read_xrf_spectra_panalytical))
  }

  # read header info
  header <- readr::read_lines(path, n_max = 100)
  # number of header lines = first line without a colon plus 1
  n_header_lines <- which(!stringr::str_detect(header, ":"))[1] - 1
  stopifnot(is.finite(n_header_lines))

  # need some information from the meta to calculate cps
  meta <- read_xrf_meta_panalytical(path)

  spect_df <- readr::read_delim(
    path, "\t", skip = n_header_lines,
    col_types = readr::cols(.default = readr::col_double())
  )
  names(spect_df) <- names(spect_df) %>%
    stringr::str_to_lower() %>%
    stringr::str_replace_all("[^a-z0-9]+", "_") %>%
    stringr::str_remove_all("(^_)|(_$)")

  spect_df$cps <- spect_df$counts / meta$LiveTime
  if("background" %in% names(spect_df)) {
    spect_df$baseline <- spect_df$background / meta$LiveTime
  }

  tibble::tibble(.path = path, .position = 1L, .spectra = list(spect_df))
}

panalytical_parse_meta <- function(tbl) {
  parsers <- panalytical_parsers()

  tbl[] <- purrr::map2(
    names(tbl) %>% stringr::str_remove("..[0-9]+$"),
    tbl,
    function(col_name, col) {
      if(col_name %in% names(parsers)) {
        parsers[[col_name]](col)
      } else {
        col
      }
  })

  tbl
}

panalytical_parsers <- function() {
  list(
    SampleIdent = readr::parse_character,
    TimeMeasured = purrr::partial(readr::parse_datetime, format = "%d/%m/%Y %H:%M:%S"),
    Application = readr::parse_character,
    ConditionSet = readr::parse_character,
    Instrument = readr::parse_character,
    InstrumentSerial = readr::parse_character,
    TubeAnode = readr::parse_character,
    Detector = readr::parse_character,
    kV = readr::parse_double,
    mA = readr::parse_double,
    uA = readr::parse_double,
    Filter = readr::parse_character,
    Medium = readr::parse_character,
    PeakingTimeNr = readr::parse_double,
    ZeroReference = readr::parse_double,
    GainReference = readr::parse_double,
    NoiseReference = readr::parse_double,
    FanoReference = readr::parse_double,
    ZeroInit = readr::parse_double,
    GainInit = readr::parse_double,
    NoiseInit = readr::parse_double,
    FanoInit = readr::parse_double,
    ZeroFit = readr::parse_double,
    GainFit = readr::parse_double,
    NoiseFit = readr::parse_double,
    FanoFit = readr::parse_double,
    NormFactor = readr::parse_double,
    NormRefCounts = readr::parse_double,
    NormCurCounts = readr::parse_double,
    `ROI-Fit (chan)` = readr::parse_double,
    `Chi-square` = readr::parse_double,
    RefPressure = readr::parse_double,
    RefTemp = readr::parse_double,
    MeasPressure = readr::parse_double,
    MeasTemp = readr::parse_double,
    Delta_hPa = readr::parse_double,
    HeliumFlow = readr::parse_double,
    LiveTime = readr::parse_double,
    TrueTime = readr::parse_double,
    FastChanLiveT = readr::parse_double,
    FastChanDeadT = readr::parse_double,
    DeadTime = readr::parse_double,
    DeadTimePct = readr::parse_double,
    FastChanCounts = readr::parse_integer,
    OutputCounts = readr::parse_integer,
    UnderflowCounts = readr::parse_integer,
    OverflowCounts = readr::parse_integer,
    TotalCounts = readr::parse_integer,
    NrOfChannels = readr::parse_integer
  )
}
