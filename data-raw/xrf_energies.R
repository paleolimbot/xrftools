
library(tidyverse)
library(xrf)

xrf_edges <- x_ray_energies %>%
  filter(str_detect(trans, "edge")) %>%
  mutate(edge = str_remove(trans, "\\s*edge$")) %>%
  select(element, edge, edge_kev = direct_kev) %>%
  filter(!is.na(edge_kev))

x_ray_xrf_energies <- x_ray_energies %>%
  left_join(x_ray_transitions, by = "trans") %>%
  filter(!is.na(direct_kev), !str_detect(trans, "edge")) %>%
  # some heavy elements have more than one ref
  group_by(element, trans) %>%
  slice(1) %>%
  ungroup() %>%
  mutate(z = chemr::elz(element)) %>%
  arrange(z, trans_siegbahn) %>%
  select(element, z, trans, trans_siegbahn, energy_kev = direct_kev) %>%
  extract(trans, "edge", "^([KL][0-9]?)", remove = FALSE) %>%
  left_join(xrf_edges, by = c("element", "edge")) %>%
  filter(!is.na(edge_kev))

devtools::use_data(x_ray_xrf_energies, overwrite = TRUE)
