
library(tidyverse)

# Landing page: https://physics.nist.gov/PhysRefData/XrayTrans/Html/search.html
# Energies and edges
# HTML version: https://physics.nist.gov/cgi-bin/XrayTrans/search.pl?element=Ne&element=Na&element=Mg&element=Al&element=Si&element=P&element=S&element=Cl&element=Ar&element=K&element=Ca&element=Sc&element=Ti&element=V&element=Cr&element=Mn&element=Fe&element=Co&element=Ni&element=Cu&element=Zn&element=Ga&element=Ge&element=As&element=Se&element=Br&element=Kr&element=Rb&element=Sr&element=Y&element=Zr&element=Nb&element=Mo&element=Tc&element=Ru&element=Rh&element=Pd&element=Ag&element=Cd&element=In&element=Sn&element=Sb&element=Te&element=I&element=Xe&element=Cs&element=Ba&element=La&element=Ce&element=Pr&element=Nd&element=Pm&element=Sm&element=Eu&element=Gd&element=Tb&element=Dy&element=Ho&element=Er&element=Tm&element=Yb&element=Lu&element=Hf&element=Ta&element=W&element=Re&element=Os&element=Ir&element=Pt&element=Au&element=Hg&element=Tl&element=Pb&element=Bi&element=Po&element=At&element=Rn&element=Fr&element=Ra&element=Ac&element=Th&element=Pa&element=U&trans=KL1&trans=KL2&trans=KL3&trans=KM1&trans=KM2&trans=KM3&trans=KM4&trans=KM5&trans=KN1&trans=KN2&trans=KN3&trans=KN4&trans=KN5&trans=Kedge&trans=L1M1&trans=L1M2&trans=L1M3&trans=L1M4&trans=L1M5&trans=L1N1&trans=L1N2&trans=L1N3&trans=L1N4&trans=L1N5&trans=L1N6&trans=L1N7&trans=L1edge&trans=L2M1&trans=L2M2&trans=L2M3&trans=L2M4&trans=L2M5&trans=L2N1&trans=L2N2&trans=L2N3&trans=L2N4&trans=L2N5&trans=L2N6&trans=L2N7&trans=L2edge&trans=L3M1&trans=L3M2&trans=L3M3&trans=L3M4&trans=L3M5&trans=L3N1&trans=L3N2&trans=L3N3&trans=L3N4&trans=L3N5&trans=L3N6&trans=L3edge&lower=&upper=&units=eV
# curl::curl_download(
#   "https://physics.nist.gov/cgi-bin/XrayTrans/search.pl?download=tab&element=Ne&element=Na&element=Mg&element=Al&element=Si&element=P&element=S&element=Cl&element=Ar&element=K&element=Ca&element=Sc&element=Ti&element=V&element=Cr&element=Mn&element=Fe&element=Co&element=Ni&element=Cu&element=Zn&element=Ga&element=Ge&element=As&element=Se&element=Br&element=Kr&element=Rb&element=Sr&element=Y&element=Zr&element=Nb&element=Mo&element=Tc&element=Ru&element=Rh&element=Pd&element=Ag&element=Cd&element=In&element=Sn&element=Sb&element=Te&element=I&element=Xe&element=Cs&element=Ba&element=La&element=Ce&element=Pr&element=Nd&element=Pm&element=Sm&element=Eu&element=Gd&element=Tb&element=Dy&element=Ho&element=Er&element=Tm&element=Yb&element=Lu&element=Hf&element=Ta&element=W&element=Re&element=Os&element=Ir&element=Pt&element=Au&element=Hg&element=Tl&element=Pb&element=Bi&element=Po&element=At&element=Rn&element=Fr&element=Ra&element=Ac&element=Th&element=Pa&element=U&trans=KL1&trans=KL2&trans=KL3&trans=KM1&trans=KM2&trans=KM3&trans=KM4&trans=KM5&trans=KN1&trans=KN2&trans=KN3&trans=KN4&trans=KN5&trans=Kedge&trans=L1M1&trans=L1M2&trans=L1M3&trans=L1M4&trans=L1M5&trans=L1N1&trans=L1N2&trans=L1N3&trans=L1N4&trans=L1N5&trans=L1N6&trans=L1N7&trans=L1edge&trans=L2M1&trans=L2M2&trans=L2M3&trans=L2M4&trans=L2M5&trans=L2N1&trans=L2N2&trans=L2N3&trans=L2N4&trans=L2N5&trans=L2N6&trans=L2N7&trans=L2edge&trans=L3M1&trans=L3M2&trans=L3M3&trans=L3M4&trans=L3M5&trans=L3N1&trans=L3N2&trans=L3N3&trans=L3N4&trans=L3N5&trans=L3N6&trans=L3edge&lower=&upper=&units=eV",
#   "data-raw/nist_xrf_lines.tsv"
# )

nice_names <- . %>%
  str_to_lower() %>%
  str_replace_all("[^a-z0-9]+", "_") %>%
  str_remove_all("(^_)|(_$)")

header <- read_lines("data-raw/nist_xrf_lines.tsv", skip = 1, n_max = 1) %>%
  str_remove("^Transitions:") %>%
  str_remove("\\band\\b") %>%
  str_split(",") %>%
  first() %>%
  str_trim() %>%
  tibble(x = .) %>%
  extract(x, c("trans", "trans_siegbahn"), "^(.*?)\\s*(\\(.*\\))?$") %>%
  mutate(trans_siegbahn = str_remove_all(trans_siegbahn, "[\\(\\)]"))

energies <- read_tsv(
  "data-raw/nist_xrf_lines.tsv",
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
  select(-a) %>%
  mutate_at(vars(ends_with("_ev")), "/", 1000) %>%
  rename_at(vars(ends_with("_ev")), str_replace, "_ev$", "_kev") %>%
  mutate(element = as_factor(element)) %>%
  arrange(element, trans) %>%
  mutate(element = as.character(element))

x_ray_transitions <- header %>%
  filter(!str_detect(trans, "edge")) %>%
  extract(trans, c("shell_from", "shell_to"), "([A-Z][0-9]?)([A-Z][0-9]?)", remove = FALSE)

x_ray_energies <- energies
devtools::use_data(x_ray_energies, overwrite = TRUE)
devtools::use_data(x_ray_transitions, overwrite = TRUE)
