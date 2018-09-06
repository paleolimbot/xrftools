
library(tidyverse)

# Original source for files:
# https://www-nds.iaea.org/epdl97/libsall.htm
#
# Using the nicely parsed versions from PyMca5
# https://pypi.org/project/PyMca5/
#
# Easiest way to obtain the EADL and EAPL files included here:
# pip3 install PyMca5
# >>> import PyMca5
# >>> PyMca5.getDataFile("EADL97_BindingEnergies.dat")
# >>> PyMca5.getDataFile("EPDL97_CrossSections.dat")
#
# They are also in the third-party/fisx/fisx_data/ folder of the PyMca5 source code
# https://pypi.org/project/PyMca5/#files
#
# These files are in 'specfile' format, whatever the f that is

read_specfile <- function(path) {
  lines <- read_lines(path)
  tibble(
    start = str_which(lines, "^#L") + 1,
    end = c(str_which(lines, "^#S")[-1] - 1, length(lines)),
    col_names = map(start, ~str_split(lines[. - 1], "\\s+") %>% .[[1]] %>% .[-1]),
    section_title = lines[start - 3]
  ) %>%
    pmap(function(start, end, col_names, ...) {
      lines[start:end] %>%
        str_subset("[^\\s]+") %>%
        str_split("\\s+") %>%
        do.call(rbind, .) %>%
        as_tibble() %>%
        set_names(col_names) %>%
        mutate(...)
    }) %>%
    bind_rows() %>%
    select(section_title, everything())
}

# x_ray_binding_energies <- read_specfile("data-raw/EADL97_BindingEnergies.dat") %>%
#   mutate_at(vars(-1), as.numeric) %>%
#   mutate(element = chemr::elsymbol(Z)) %>%
#   select(element, z = Z, everything()) %>%
#   gather(shell, energy_kev, -element, -z) %>%
#   extract(shell, c("shell", "shell_code"), "^(.*?)\\((.*?)\\)$") %>%
#   filter(energy_kev > 0) %>%
#   arrange(z, shell)

x_ray_emission_probabilities <- bind_rows(
  read_specfile("data-raw/EADL97_KShellRadiativeRates.dat"),
  read_specfile("data-raw/EADL97_LShellRadiativeRates.dat")
) %>%
  gather(trans, emission_probability, -section_title, -Z) %>%
  mutate_at(vars(Z, emission_probability), as.numeric) %>%
  mutate(element = chemr::elsymbol(Z)) %>%
  filter(emission_probability > 0) %>%
  arrange(Z, trans) %>%
  select(element, z = Z, trans, emission_probability)

x_ray_cross_sections <- read_specfile("data-raw/EPDL97_CrossSections.dat") %>%
  extract(section_title, c("z", "element"), "([0-9]+)\\s+([A-Z][a-z]?)") %>%
  mutate_at(vars(-element), as.numeric) %>%
  gather(type, cross_section, -z, -element, -starts_with("PhotonEnergy")) %>%
  extract(type, c("type", "cross_section_unit"), "(.*?)\\[(.*?)\\]") %>%
  extract(type, "shell", "^([KLM][123]?)\\(") %>%
  filter(!is.na(shell), cross_section > 0) %>%
  select(element, z, energy_kev = starts_with("PhotonEnergy"), shell, cross_section) %>%
  group_by(element, z, shell) %>%
  nest() %>%
  mutate(
    interp_data = map(
      data,
      ~tibble(
        energy_kev = seq(0, 100, by = 5),
        cross_section = approx(.$energy_kev, .$cross_section, energy_kev)$y
      )
    )
  ) %>%
  unnest(interp_data) %>%
  arrange(z, shell, energy_kev)

# this is a shot at relative peak heights
# I get the sense that this is a bit more complicated than I thought

# a <- x_ray_cross_sections %>%
#   filter(element == "Fe", energy_kev == 10)
# xrf::x_ray_transitions %>%
#   select(-trans_siegbahn) %>%
#   mutate(trans = as_factor(trans)) %>%
#   crossing(., .) %>%
#   filter(as.integer(trans) < as.integer(trans1), shell_from == shell_from1) %>%
#   mutate_at(vars(starts_with("trans")), as.character) %>%
#   select(shell = shell_from, trans_from = trans, trans_to = trans1, shell_from = shell_to, shell_to = shell_to1) %>%
#   inner_join(a %>% select(-cross_section), by = c("shell_from" = "shell")) %>%
#   inner_join(a, by = c("element", "z", "energy_kev", "shell_to" = "shell")) %>%
#   group_by(shell) %>%
#   mutate(cross_section = cross_section / max(cross_section)) %>%
#   ungroup() %>%
#   View()


# checking the original vs. interpolated data
# x_ray_cross_sections %>%
#   filter(element == "Fe") %>%
#   ggplot(aes(energy_kev, cross_section, col = shell)) +
#   geom_line() +
#   scale_y_log10()

shell_constants <- list(
  read_specfile("data-raw/EADL97_KShellConstants.dat"),
  read_specfile("data-raw/EADL97_LShellConstants.dat"),
  read_specfile("data-raw/EADL97_MShellConstants.dat")
) %>%
  map(. %>% gather(param, value, -section_title, -Z) %>% filter(!is.na(value))) %>%
  bind_rows() %>%
  mutate(value = as.numeric(value), Z = as.numeric(Z), element = chemr::elsymbol(Z)) %>%
  rename(z = Z)

x_ray_fluorescence_yields <- shell_constants %>%
  extract(param, "shell", "omega([A-Z][0-9]?)") %>%
  filter(!is.na(shell), value > 0) %>%
  rename(fluorescence_yield = value) %>%
  select(element, z, shell, fluorescence_yield)

x_ray_coster_kronig_probabilities <- shell_constants %>%
  filter(str_detect(param, "^f")) %>%
  extract(section_title, "shell", "\\s+([LM][1234])") %>%
  filter(value > 0) %>%
  select(element, z, shell, coster_kronig_trans = param, coster_kronig_prob = value)

# devtools::use_data(x_ray_binding_energies, overwrite = TRUE)
devtools::use_data(x_ray_cross_sections, overwrite = TRUE)
devtools::use_data(x_ray_emission_probabilities, overwrite = TRUE)
devtools::use_data(x_ray_fluorescence_yields, overwrite = TRUE)
devtools::use_data(x_ray_coster_kronig_probabilities, overwrite = TRUE)
