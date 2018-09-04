
#' Plot spectra
#'
#' @param object A \link{xrf_spectra}.
#' @param x,y Columns in .spectra to be used for plotting
#' @param subset An expression to subset the unnested data.
#' @param group Override the group aesthetic
#' @param facet An expression to facet by
#' @param ... Additional \link[ggplot2]{aes} mappings.
#' @param nrow,ncol,scales Passed to \link[ggplot2]{facet_wrap}.
#'
#' @return A ggplot2 plot.
#' @export
#' @importFrom ggplot2 autoplot
#'
autoplot.spectra <- function(object, subset = NULL, x = .data$energy_kev, y = .data$cps, group = seq_len(nrow(object)), ...,
                             facet = NULL, nrow = NULL, ncol = NULL, scales = "free") {
  x <- enquo(x)
  y <- enquo(y)
  group <- enquo(group)
  facet <- enquo(facet)
  subset <- enquo(subset)
  dots <- quos(...)

  data <- object %>%
    xrf_despectra() %>%
    dplyr::mutate(.group = !!group) %>%
    tidyr::unnest()

  if(!rlang::quo_is_null(subset)) {
    data <- dplyr::filter(data, !!subset)
  }

  ggplot2::ggplot(data, ggplot2::aes(x = !!x, y = !!y, group = .data$.group, !!!dots)) +
    ggplot2::geom_line() +
    if(!rlang::quo_is_null(facet)) {
      ggplot2::facet_wrap(ggplot2::vars(!!facet), nrow = nrow, ncol = ncol, scales = scales)
    }
}

#' Add XRF Peaks to a ggplot
#'
#' @param mapping Mapping for layer (x and y required)
#' @param data Data for layer
#' @param position Position for layer
#' @param ... Passed to \link[ggrepel]{geom_label_repel}. \code{nudge_y} is particularly useful.
#' @param element_list An element list (sanitized using \link{xrf_element_list})
#' @param energy_subset Used to subset \link{xrf_energies}.
#' @param box.padding,size Passed to \link[ggrepel]{geom_label_repel}
#' @param epsilon Minimum peak height to label
#' @param inherit.aes Inherit aesthetics from ggplot()?
#' @param show.legend Show legend for this layer?
#'
#' @return A ggplot2 layer
#' @export
#'
stat_xrf_peaks <- function(mapping = NULL, data = NULL, position = "identity",
                           ...,
                           element_list = "lake_sediment", energy_subset = NULL, epsilon = 0.01, box.padding = 0.5,
                           size = 2, inherit.aes = TRUE, show.legend = FALSE) {
  ggplot2::layer(
    geom = ggrepel::GeomLabelRepel,
    stat = StatXrfPeaks,
    data = data,
    mapping = mapping,
    position = position,
    params = list(element_list = element_list, direction = "y", box.padding = box.padding,
                  epsilon = epsilon, size = size, energy_subset = enquo(energy_subset), ...),
    inherit.aes = inherit.aes,
    show.legend = show.legend
  )
}

#' @export
#' @rdname stat_xrf_peaks
StatXrfPeaks <- ggplot2::ggproto(
  "StatXrfPeaks",
  ggplot2::Stat,
  required_aes = c("x", "y"),
  compute_panel = function(self, data, scales, element_list = "everything", epsilon = 0.01,
                           energy_subset = rlang::quo(NULL)) {
    if (is.null(data) || (nrow(data) == 0)) return(data.frame())

    energies <- xrf_get_energies(element_list)
    if(!rlang::quo_is_null(energy_subset)) {
      energies <- dplyr::filter(energies, !!energy_subset)
    }

    energies <- energies %>%
      dplyr::filter(.data$energy_kev > min(!!data$x, na.rm = TRUE), .data$energy_kev < max(!!data$y)) %>%
      dplyr::mutate(label = paste(.data$element, .data$trans_siegbahn)) %>%
      dplyr::select(x = "energy_kev", "label")

    data_nest <- data %>%
      dplyr::group_by(.data$group) %>%
      tidyr::nest()
    data_nest$energy <- purrr::map(data_nest$data, function(x) {
        stats::approx(x$x, x$y, energies$x)$y
    })

    energies$y <- purrr::invoke(pmax, data_nest$energy, na.rm = TRUE)
    energies$group <- data$group[1]
    energies$PANEL <- data$PANEL[1]
    energies %>% dplyr::filter(.data$y > epsilon)
  }
)



