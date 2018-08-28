
library(tidyverse)
library(rvest)
library(xrf)

# curl::curl_download("https://en.wikipedia.org/wiki/X-ray_fluorescence", "data-raw/wiki_xrf.html")

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

xrf_energies_kenna <- kenna_lines %>%
  rename(energy_kev = kev, trans_siegbahn = primary_x_ray_line, energy_upper_kev = region_upper_kev) %>%
  select(-starts_with("factory"), -soil_protocol) %>%
  filter(!is.na(energy_kev)) %>%
  distinct() %>%
  mutate(trans_siegbahn = trans_siegbahn %>% str_replace("a", "alpha") %>% str_replace("b", "beta")) %>%
  left_join(nist_header, by = "trans_siegbahn")


# wikipedia energies
xrf_page <- read_html("data-raw/wiki_xrf.html")
xrf_wiki <- xrf_page %>%
  html_node("table.wikitable") %>%
  html_table()
names(xrf_wiki) <- nice_names(names(xrf_wiki))

xrf_energies_wiki <- tibble(
  element = xrf_wiki %>% select(starts_with("element")) %>% invoke(c, .),
  line = xrf_wiki %>% select(starts_with("line")) %>% invoke(c, .),
  trans_siegbahn = str_replace(line, "α", "alpha") %>% str_replace("alpha$", "alpha1,2"),
  trans_siegbahn_2 = c("Kalpha1,2" = "Kalpha2")[trans_siegbahn],
  wavelength_nm = xrf_wiki %>% select(starts_with("wavelength")) %>% invoke(c, .),
  energy_kev = energy(wavelength_nm)
) %>%
  mutate(trans_siegbahn = str_remove(trans_siegbahn, ",2$")) %>%
  left_join(nist_header, by = "trans_siegbahn") %>%
  left_join(nist_header %>% rename_all(paste0, "_2"), by = "trans_siegbahn_2")

# check energies using NIST values
data("x_ray_energies")

xrf_energies <- xrf_energies_wiki %>%
  left_join(x_ray_energies %>% select(-trans_siegbahn), by = c("element", "trans")) %>%
  # if an energy has never been measured by NIST, should it really be used for xrf?
  filter(!is.na(direct_kev)) %>%
  # check duplicate trans values
  left_join(
    x_ray_energies %>% select(-trans_siegbahn) %>% rename(trans_2 = trans),
    by = c("element", "trans_2"),
    suffix = c("", "_2")
  ) %>%
  # all eps are tiny
  mutate(eps_2 = abs(direct_kev - direct_kev_2)) %>%
  # use NIST energies, not wikipedia's
  select(-energy_kev) %>%
  select(element, trans, trans_siegbahn, energy_kev = direct_kev) %>%
  mutate(line = "primary")


# secondary peaks:
# Fe at 7.058175 (Kbeta1 + 3)


devtools::use_data(xrf_energies, overwrite = TRUE)
