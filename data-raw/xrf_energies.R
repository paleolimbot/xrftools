
library(tidyverse)
library(xrf)

xrf_edges <- x_ray_energies %>%
  filter(str_detect(trans, "edge")) %>%
  mutate(edge = str_remove(trans, "\\s*edge$")) %>%
  select(element, edge, edge_kev = direct_kev) %>%
  filter(!is.na(edge_kev))

xrf_energies <- x_ray_energies %>%
  filter(trans_siegbahn %in% c("Kalpha1", "Kbeta1", "Lalpha1", "Lbeta1"), !is.na(direct_kev)) %>%
  # some heavy elements have more than one ref
  group_by(element, trans_siegbahn) %>%
  slice(1) %>%
  ungroup() %>%
  mutate(z = chemr::elz(element)) %>%
  arrange(z, trans_siegbahn) %>%
  select(element, z, trans, trans_siegbahn, energy_kev = direct_kev) %>%
  extract(trans, "edge", "^([KL][0-9]?)", remove = FALSE) %>%
  left_join(xrf_edges, by = c("element", "edge")) %>%
  left_join(x_ray_emission_probabilities, by = c("element", "trans", "z")) %>%
  filter(!is.na(edge_kev), !is.na(emission_probability))

devtools::use_data(xrf_energies, overwrite = TRUE)
