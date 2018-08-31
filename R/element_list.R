
#' Get or set element lists
#'
#' Useful for keeping track of commonly used lists of elements.
#'
#' @param name The name for the list. Individual element symbols are returned as-is.
#' @param elements A list of element symbols.
#' @param default The value to return if the element list is not found
#'
#' @return A character vector of element symbols.
#' @export
#'
#' @examples
#' xrf_set_element_list("my_precious", c("Ag", "Au", "Pt"))
#' xrf_element_list("my_precious")
#' xrf_set_element_list("my_precious", NULL)
#' xrf_element_list("my_precious", default = NULL)
#'
xrf_set_element_list <- function(name, elements) {
  name <- as.character(name)
  stopifnot(length(name) == 1, !(name %in% all_elements))

  if(is.null(elements)) {
    rm(list = name, envir = element_lists)
  } else {
    elements <- unique(as.character(elements))
    stopifnot(all(elements %in% all_elements))
    element_lists[[name]] <- elements
  }
}

#' @rdname xrf_set_element_list
#' @export
xrf_element_list <- function(name = "major", default = stop("Element list '", name, "' not found")) {
  name <- as.character(name)
  if(length(name) == 0) {
    return(character(0))
  } else if(length(name) > 1) {
    result <- purrr::map(name, xrf_element_list)
    return(unique(unname(unlist(result))))
  }

  if(name %in% names(element_lists)) {
    element_lists[[name]]
  } else if(name %in% all_elements) {
    name
  } else {
    as.character(default)
  }
}

all_elements <- c(
  "H", "He", "Li", "Be", "B", "C", "N", "O", "F", "Ne", "Na",
  "Mg", "Al", "Si", "P", "S", "Cl", "Ar", "K", "Ca", "Sc", "Ti",
  "V", "Cr", "Mn", "Fe", "Co", "Ni", "Cu", "Zn", "Ga", "Ge", "As",
  "Se", "Br", "Kr", "Rb", "Sr", "Y", "Zr", "Nb", "Mo", "Tc", "Ru",
  "Rh", "Pd", "Ag", "Cd", "In", "Sn", "Sb", "Te", "I", "Xe", "Cs",
  "Ba", "La", "Ce", "Pr", "Nd", "Pm", "Sm", "Eu", "Gd", "Tb", "Dy",
  "Ho", "Er", "Tm", "Yb", "Lu", "Hf", "Ta", "W", "Re", "Os", "Ir",
  "Pt", "Au", "Hg", "Tl", "Pb", "Bi", "Po", "At", "Rn", "Fr", "Ra",
  "Ac", "Th", "Pa", "U", "Np", "Pu", "Am", "Cm", "Bk", "Cf", "Es",
  "Fm", "Md", "No", "Lr", "Rf", "Db", "Sg", "Bh", "Hs", "Mt", "Ds",
  "Rg", "Cn", "Nh", "Fl", "Mc", "Lv", "Ts", "Og"
)

element_lists <- new.env(parent = emptyenv())
element_lists[["major"]] <- c("Si", "Al", "Ca", "Mg", "Na", "K", "Ti", "Fe", "Mn", "P")
element_lists[["lake_sediment"]] <- c(
  "Si", "Al", "Fe", "Ca", "Na", "K", "Mg", "S", "Ti", "P",
  "Mn", "Ba", "Sr", "Zn", "Zr", "V", "Pb", "Rb", "Cr",
  "La", "Cu", "Nd", "Ni", "Y", "As"
)
element_lists[["everything"]] <- all_elements
