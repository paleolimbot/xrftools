
library(tidyverse)

# Landing page: https://physics.nist.gov/PhysRefData/XrayTrans/Html/search.html
# Energies
# HTML version: https://physics.nist.gov/cgi-bin/XrayTrans/search.pl?element=All&trans=KL2&trans=KL3&trans=KM2&trans=KM3&trans=KM4&trans=KM5&trans=KN2&trans=KN3&trans=KN4&trans=KN5&trans=L2M4&trans=L3M4&trans=L3M5&trans=L3N5&lower=&upper=&units=eV
# curl::curl_download(
#   "https://physics.nist.gov/cgi-bin/XrayTrans/search.pl?download=tab&element=All&trans=KL2&trans=KL3&trans=KM2&trans=KM3&trans=KM4&trans=KM5&trans=KN2&trans=KN3&trans=KN4&trans=KN5&trans=L2M4&trans=L3M4&trans=L3M5&trans=L3N5&lower=&upper=&units=eV",
#   "data-raw/nist_xrf_lines.tsv"
# )

# Edges
# https://physics.nist.gov/cgi-bin/XrayTrans/search.pl?download=tab&element=All&trans=Kedge&trans=L1edge&trans=L2edge&trans=L3edge&lower=&upper=&units=eV
# curl::curl_download(
#   "https://physics.nist.gov/cgi-bin/XrayTrans/search.pl?download=tab&element=All&trans=Kedge&trans=L1edge&trans=L2edge&trans=L3edge&lower=&upper=&units=eV",
#   "data-raw/nist_xrf_edges.tsv"
# )

nice_names <- . %>%
  str_to_lower() %>%
  str_replace_all("[^a-z0-9]+", "_") %>%
  str_remove("(^_)|(_$)")

header <- read_lines("data-raw/nist_xrf_lines.tsv", skip = 1, n_max = 1) %>%
  str_remove("^Transitions:") %>%
  str_remove("\\band\\b") %>%
  str_split(",") %>%
  first() %>%
  str_trim() %>%
  tibble(x = .) %>%
  extract(x, c("trans", "trans_siegbahn"), "(.*?) \\((.*?)\\)")

energies <- c(
  "data-raw/nist_xrf_edges.tsv",
  "data-raw/nist_xrf_lines.tsv"
) %>%
  map_dfr(
    read_tsv,
    skip = 4,
    col_types = cols(
      Ele. = col_character(),
      A = col_integer(),
      Trans. = col_character(),
      `Theory (eV)` = col_double(),
      `Unc. (eV)` = col_double(),
      `Direct (eV)` = col_double(),
      `Unc. (eV)_1` = col_double(),
      Blend = col_character(),
      Ref. = col_character()
    )
  ) %>%
  rename_all(nice_names) %>%
  rename(element = ele, theory_unc_ev = unc_ev, direct_unc_ev = unc_ev_1, combined_unc_kev = unc_ev_2,
         vapour_unc_kev = unc_ev_3) %>%
  left_join(header, by = "trans") %>%
  select(-a) %>%
  mutate_at(vars(ends_with("_ev")), "/", 1000) %>%
  rename_at(vars(ends_with("_ev")), str_replace, "_ev$", "_kev") %>%
  mutate(element = as_factor(element)) %>%
  arrange(element, trans_siegbahn) %>%
  mutate(element = as.character(element))

x_ray_energies <- energies
devtools::use_data(x_ray_energies, overwrite = TRUE)
