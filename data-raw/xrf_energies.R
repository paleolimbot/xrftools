
library(tidyverse)
library(rvest)

nice_names <- . %>%
  str_to_lower() %>%
  str_replace_all("[^a-z0-9]+", "_") %>%
  str_remove("(^_)|(_$)") %>%
  tibble::tidy_names(quiet = TRUE)

nist_header <- read_lines("data-raw/nist_xrf_lines.tsv", skip = 1, n_max = 1) %>%
  str_remove("^Transitions:") %>%
  str_remove("\\band\\b") %>%
  str_split(",") %>%
  first() %>%
  str_trim() %>%
  tibble(x = .) %>%
  extract(x, c("trans", "trans_siegbahn"), "(.*?) \\((.*?)\\)") %>%
  select(trans_siegbahn, trans)

# Kenna et al. 2011 energies
kenna_lines <- readxl::read_excel("data-raw/Kenna et al. 2011 Table 3.xlsx") %>%
  rename_all(nice_names)

kenna_lines %>%
  rename(energy_kev = kev, trans_siegbahn = primary_x_ray_line, energy_upper_kev = region_upper_kev) %>%
  select(-starts_with("factory"), -soil_protocol) %>%
  filter(!is.na(energy_kev)) %>%
  distinct() %>%
  mutate(trans_siegbahn = trans_siegbahn %>% str_replace("a", "alpha") %>% str_replace("b", "beta")) %>%
  left_join(nist_header, by = "trans_siegbahn") %>%
  View()


# wikipedia energies
xrf_page <- read_html("https://en.wikipedia.org/wiki/X-ray_fluorescence")
xrf_wiki <- xrf_page %>%
  html_node("table.wikitable") %>%
  html_table()
names(xrf_wiki) <- nice_names(names(xrf_wiki))

tibble(
  element = xrf_wiki %>% select(starts_with("element")) %>% invoke(c, .),
  wavelength_nm = xrf_wiki %>% select(starts_with("wavelength")) %>% invoke(c, .)
)
