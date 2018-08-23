
#' Read a Panalytical spectra file into a spectra object
#'
#' @param path A filename or vector of filenames that correspond to Panalytical spectrum files
#'   (usually a .mp2 file)
#'
#' @return A spectra object
#' @export
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @examples
#' pan_dir <- system.file("spectra_files/Panalytical", package = "xrf")
#' read_spec_panalytical(file.path(pan_dir, "Panalytical_2017-08-1632-Omnian.mp2"))
#'
read_spec_panalytical <- function(path) {
  stopifnot(is.character(path), all(file.exists(path)))
  if(length(path) == 0) {
    return(tibble::tibble(.path = character(0), .spectra = list()))
  } else if(length(path) > 1) {
    return(purrr::map_dfr(path, read_spec_panalytical))
  }

  # read header info
  header <- readr::read_lines(path, n_max = 100)

  # number of header lines = first line without a colon plus 1
  n_header_lines <- which(!stringr::str_detect(header, ":"))[1] - 1
  stopifnot(is.finite(n_header_lines))
  header_df <- tibble::tibble(line = utils::head(header, n_header_lines)) %>%
    tidyr::separate(.data$line, c("key", "value"), ":", extra = "merge") %>%
    tibble::add_row(key = ".path", value = path)

  spect_df <- readr::read_delim(
    path, "\t", skip = n_header_lines,
    col_types = readr::cols(.default = readr::col_double())
  )
  names(spect_df) <- names(spect_df) %>%
    stringr::str_to_lower() %>%
    stringr::str_replace_all("[^a-z0-9]+", "_") %>%
    stringr::str_remove_all("(^_)|(_$)")

  rlang::set_names(
    stringr::str_trim(header_df$value),
    tibble::tidy_names(stringr::str_trim(header_df$key), quiet = TRUE)
  ) %>%
    as.list() %>%
    tibble::as_tibble() %>%
    dplyr::mutate(.spectra = list(spect_df))
}
