
#' Create an XRF spectra
#'
#' @param .spectra A data frame or list of data frames with columns (at least) "energy_kev" and "cps".
#' @param ... Key/value pairs, passed to \link[tibble]{tibble}.
#' @param .position If there is more than one spectrum in .path, a number describing which spectrum within .path
#' @param .path The file from whence the spectrum came. Can be NA.
#' @param .validate Use FALSE to skip validation
#'
#' @return A spectra df.
#' @export
#'
xrf_spectra <- function(.spectra, ..., .position = NA_integer_, .path = NA_character_, .validate = TRUE) {
  stopifnot(is.numeric(.position), is.character(.path))

  if(is.data.frame(.spectra)) {
    stopifnot(all(c("energy_kev", "cps") %in% colnames(.spectra)))
    .spectra <- list(.spectra)
  }

  meta <- tibble::tibble(..., .path = .path, .position = .position, .spectra = .spectra)
  spec <- new_spectra(meta)
  if(.validate) validate_spectra(spec)
  spec
}

#' Create, validate spectra
#'
#' @param x An object
#' @param subclass Additional classes that should be included
#'
#' @return x, reclassed and/or validated
#' @export
#'
new_spectra <- function(x, subclass = character(0)) {
  stopifnot(is.data.frame(x), is.character(subclass))
  new_class <- c(subclass, "spectra", class(x))
  class(x) <- unique(new_class)
  x
}

#' @rdname new_spectra
#' @export
validate_spectra <- function(x) {
  stopifnot(
    inherits(x, "spectra"),
    tibble::is_tibble(x),
    all(c(".position", ".path", ".spectra") %in% colnames(x)),
    is.character(x$.path),
    is.numeric(x$.position),
    is.list(x$.spectra)
  )

  is_df <- purrr::map_lgl(x$.spectra, is.data.frame)
  if(!all(is_df)) stop("Some .spectra are not data frames: ", paste(which(!is_df), collapse = ", "))

  has_energy <- purrr::map_lgl(x$.spectra, function(y) "energy_kev" %in% colnames(y))
  if(!all(has_energy)) {
    stop("Some .spectra are missing column 'energy_kev': ", paste(which(!has_energy), collapse = ", "))
  }

  energy_correct_type <- purrr::map_lgl(x$.spectra, function(y) is.numeric(y$energy_kev))
  if(!all(energy_correct_type)) {
    stop("Some .spectra$energy_kev are not numeric: ", paste(which(!energy_correct_type), collapse = ", "))
  }

  has_cps <- purrr::map_lgl(x$.spectra, function(y) "cps" %in% colnames(y))
  if(!all(has_cps)) {
    stop("Some .spectra are missing column 'cps': ", paste(which(!has_cps), collapse = ", "))
  }

  cps_correct_type <- purrr::map_lgl(x$.spectra, function(y) is.numeric(y$cps))
  if(!all(cps_correct_type)) {
    stop("Some .spectra$cps are not numeric: ", paste(which(!cps_correct_type), collapse = ", "))
  }

  invisible(x)
}

#' @rdname new_spectra
#' @export
xrf_despectra <- function(x) {
  class(x) <- intersect(class(x), c("tbl_df", "tbl", "data.frame"))
  x
}

#' @importFrom dplyr filter
#' @export
dplyr::filter

#' @importFrom dplyr filter
#' @export
filter.spectra <- function(.data, ...) {
  new_spectra(NextMethod(), subclass = class(.data))
}

#' @importFrom dplyr slice
#' @export
slice.spectra <- function(.data, ...) {
  new_spectra(NextMethod(), subclass = class(.data))
}

#' @importFrom dplyr mutate
#' @export
mutate.spectra <- function(.data, ...) {
  out <- new_spectra(NextMethod(), subclass = class(.data))
  # catches dropped columns, improper type changes of required columns
  validate_spectra(out)
  out
}

#' @importFrom dplyr rename
#' @export
rename.spectra <- function(.data, ...) {
  out <- new_spectra(NextMethod(), subclass = class(.data))
  # catches dropped columns
  stopifnot(all(c(".position", ".path", ".spectra") %in% colnames(out)))
  out
}

#' @importFrom dplyr select
#' @export
select.spectra <- function(.data, ...) {
  out <- new_spectra(
    dplyr::select(xrf_despectra(.data), ..., .path = ".path", .position = ".position", .spectra = ".spectra"),
    subclass = class(.data)
  )
  # catches dropped columns (can they occur?)
  stopifnot(all(c(".position", ".path", ".spectra") %in% colnames(out)))
  out
}

#' @export
`[.spectra` <- function(x, i, j, ...) {
  out <- new_spectra(NextMethod(), subclass = class(x))
  validate_spectra(out)
  out
}

#' @importFrom tidyr unnest
#' @export
unnest.spectra <- function(data, ...) {
  # something about the implementation of the above methods leads to errors in unnest()
  tidyr::unnest(xrf_despectra(data), ...)
}

#' Combine XRF spectra
#'
#' Behaviour is identical to \link[dplyr]{bind_rows}.
#'
#' @param ... XRF spectra or lists of XRF spectra.
#' @param .id Column in ouput that will contain names or indicies of input
#'
#' @return A spectra.
#' @export
#'
xrf_combine_spectra <- function(..., .id = NULL) {
  objects <- rlang::list2(...)
  classes <- class_recursive(objects)
  objects <- lapply(objects, structure, class = "data.frame")
  result <- purrr::invoke(dplyr::bind_rows, objects, .id = .id)
  out <- new_spectra(
    tibble::as_tibble(result),
    subclass = setdiff(classes, c("spectra", "tbl_df", "data.frame"))
  )
  validate_spectra(out)
  out
}

class_recursive <- function(x) {
  if(is.data.frame(x)) {
    class(x)
  } else if(is.list(x)) {
    unname(unlist(lapply(x, class_recursive)))
  } else {
    NULL
  }
}

#' @rdname xrf_combine_spectra
#' @export
rbind.spectra <- function(...) {
  xrf_combine_spectra(...)
}
