
library(tidyverse)

nist_header <- read_lines("data-raw/nist_xrf_lines.tsv", skip = 1, n_max = 1) %>%
  str_remove("^Transitions:") %>%
  str_remove("\\band\\b") %>%
  str_split(",") %>%
  first() %>%
  str_trim() %>%
  tibble(x = .) %>%
  extract(x, c("trans", "trans_siegbahn"), "(.*?) \\((.*?)\\)") %>%
  select(trans_siegbahn, trans) %>%
  deframe() %>%
  c(
    "Kalpha3" = "KL1",
    "Kbeta2" = "(KN3+KN2)",
    "Kbeta4" = "(KN4+KN5)",
    "Kbeta5" = "(KM4+KM5)",
    "Kbeta" = "(KM3+KN2+KN3+KN2+KM2+KN4+KN5+KM4+KM5)"
  )

fix_notation_seig <- function(x) {
  x %>%
    str_replace_all("Kbeta2", "(`Kbeta2 I`+`Kbeta2 II`)") %>%
    str_replace_all("Kbeta4", "(`Kbeta4 I`+`Kbeta4 II`)") %>%
    str_replace_all("Kbeta5", "(`Kbeta5 I`+`Kbeta5 II`)")
}

fix_notation <- function(x) {
  unname(nist_header[x])
}

salem_table_1 <- readxl::read_excel("data-raw/Salem et al. 1974 Table I.xlsx")

x_ray_intensities <- salem_table_1 %>%
  rename(element = Element) %>%
  mutate(Ka1 = 1) %>%
  rename_at(vars(ends_with("/Ka1")), str_remove, "/Ka1$") %>%
  select(element, Ka1, everything()) %>%
  mutate(Kb = (Ka1 + Ka2) / `Kb/Ka`, Kb3 = Kb1 * `Kb3/Kb1`) %>%
  select(-contains("/")) %>%
  gather(trans_salem, relative_intensity, -element) %>%
  mutate(
    trans_salem = trans_salem %>%
      str_replace("a", "alpha") %>%
      str_replace("b", "beta"),
    trans_siegbahn = trans_salem %>%
      fix_notation_seig(),
    trans = fix_notation(trans_salem)
  ) %>%
  filter(!is.na(relative_intensity))

devtools::use_data(x_ray_intensities, overwrite = TRUE)
