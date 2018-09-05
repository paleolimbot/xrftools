
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
# These files are in 'specfile' format

nice_names <- . %>%
  str_to_lower() %>%
  str_replace_all("[^a-z0-9]+", "_") %>%
  str_remove_all("(^_)|(_$)")


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

ea_tbls <- list.files("data-raw", "^EA.*dat$", full.names = TRUE) %>%
  set_names() %>%
  map(read_specfile)

x_ray_binding_energies <- read_specfile("data-raw/EADL97_BindingEnergies.dat") %>%
  mutate_at(vars(-1), as.numeric) %>%
  mutate(element = chemr::elsymbol(Z)) %>%
  select(element, z = Z, everything()) %>%
  gather(orbital, energy_kev, -element, -z) %>%
  extract(orbital, c("orbital", "orbital_code"), "^(.*?)\\((.*?)\\)$") %>%
  filter(energy_kev > 0) %>%
  arrange(z, orbital)

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
  extract(type, "orbital", "^([KLM][123]?)\\(") %>%
  filter(!is.na(orbital), cross_section > 0) %>%
  select(element, z, orbital, cross_section, cross_section_unit)

devtools::use_data(x_ray_binding_energies, overwrite = TRUE)
devtools::use_data(x_ray_cross_sections, overwrite = TRUE)
devtools::use_data(x_ray_emission_probabilities, overwrite = TRUE)
