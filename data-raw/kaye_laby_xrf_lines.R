
library(tidyverse)
library(rvest)

# lots of greek characters in XRF-speak, this keeps a CRAN warning from popping up
fix_encoding <- function(x) {
  Encoding(x) <- "UTF-8"
  x
}

# source on XRF lines
# curl::curl_download(
#   "http://www.kayelaby.npl.co.uk/atomic_and_nuclear_physics/4_2/4_2_1.html",
#   "data-raw/kaye_laby_xrf_lines.html"
# )

doc <- read_html("data-raw/kaye_laby_xrf_lines.html")

# a reminder that text in superscript nodes to deal with footnotes
# (delt with below because all values have 3 digits after decimal point
# so values with 4 digits are footnotes)

tbl <- doc %>%
  html_node("table.table") %>%
  html_table(header = FALSE, fill = TRUE) %>%
  as.matrix()

tbl_melt <- reshape2::melt(tbl) %>%
  rename(row = Var1, col = Var2) %>%
  mutate(col = str_remove(col, "X") %>% as.integer(), value = as.character(value))

elements <- tbl_melt %>%
  filter(col == 1, str_detect(value, "^[0-9]+\\s+[A-Z][a-z]{0,2}")) %>%
  select(-col) %>%
  separate(value, c("atomic_number", "symbol"), "\\s+") %>%
  mutate(atomic_number = as.integer(atomic_number))

header <- tbl_melt %>%
  filter(row <= 3, col > 1) %>%
  mutate(row = c("series", "term_label", "label")[row]) %>%
  spread(row, value) %>%
  mutate(
    series = str_remove(series, "-series$"),
    energy_type = if_else(str_detect(term_label, "edge$"), "edge", "emission"),
    term_label = str_remove(term_label, "edge$")
  ) %>%
  # a few things got mixed up in the html_table() read
  mutate(
    label = case_when(
      energy_type == "edge" ~ term_label,
      term_label == "KLII" ~ "Kα2",
      term_label == "LIMII" ~ "Lβ4",
      term_label == "LIIMIV" ~ "Lβ1",
      term_label == "LIIIMI" ~ "Ll",
      term_label == "MIINIV" ~ "MIINIV", # no common label?
      term_label == "MIIINV" ~ "Mγ",
      term_label == "MIVNVI" ~ "Mβ",
      TRUE ~ label
    )
  ) %>%
  extract(term_label, "edge", "([A-Za-z]+)[MNL]", remove = FALSE) %>%
  mutate(edge = if_else(energy_type == "edge", term_label, edge)) %>%
  mutate_if(is.character, fix_encoding)

intensity_rows <- tbl_melt %>%
  filter(col == 1, value == "Intensity") %>%
  pull(row)

footnotes <- paste("Unresolved lines:", c("KNII,III (Kβ2)", "L1NII,III (Lγ2,3)")) %>%
  fix_encoding()

# 1909 rows
energies <- tbl_melt %>%
  filter(!(row %in% intensity_rows)) %>%
  # add qualifying information
  left_join(elements, by = "row") %>%
  left_join(header, by = "col") %>%
  # filter to numbers in table
  filter(!is.na(series), !is.na(symbol), str_detect(value, ".+")) %>%
  # one double period, two trailing periods in the table
  mutate(value = str_replace(value, "\\.\\.", ".") %>% str_remove("\\.$")) %>%
  extract(value, "footnote", "\\.[0-9]{3}([0-9])$", remove = FALSE, convert = TRUE) %>%
  mutate(
    footnote = footnotes[footnote],
    energy_kev = str_replace(value, "(\\.[0-9]{3})[0-9]$", "\\1") %>% as.numeric()
  ) %>%
  select(-value) %>%
  select(atomic_number, symbol, label, energy_kev, energy_type, everything()) %>%
  # there are some duplicate values due to colspanning
  # (0 energies for Am cause problems, are not colspans)
  mutate(value_group = if_else(energy_kev == 0, col + 1e4, energy_kev)) %>%
  group_by(energy_type, row, value_group) %>%
  mutate(n = n()) %>%
  mutate_if(is.numeric, mean) %>%
  mutate_if(is.character, . %>% unique() %>% paste(collapse = "/") %>% na_if("NA/NA") %>% na_if("NA")) %>%
  slice(1) %>%
  ungroup() %>%
  arrange(desc(energy_type), row, col) %>%
  select(-n, -value_group, -row, -col)

# a few spot checks
stopifnot(
  nrow(energies) == 1843,
  energies %>%
    filter(atomic_number == 98, energy_type == "edge", label == "LIII") %>%
    pull(energy_kev) %>%
    `==`(19.930),
  energies %>%
    filter(symbol == "Fe", energy_type == "emission", label == "KMII/KMIII") %>%
    pull(energy_kev) %>%
    `==`(7.058)
)

# Now using NIST values
# x_ray_energies <- energies
# devtools::use_data(x_ray_energies, overwrite = TRUE)
