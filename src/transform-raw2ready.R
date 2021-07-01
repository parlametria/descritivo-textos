library(dplyr)
library(tidyr)
library(stringr)
library(readr)
library(purrr, warn.conflicts = F)

read_many_files <- function(files, col_types) {
    files %>%
        map_df(read_csv, col_types = col_types)
}

falas_raw = read_many_files(list.files(
    path = here::here("data/raw/comissoes-2019"),
    pattern = "*.csv",
    full.names = T),
    col_types = "cicc")

falas = falas_raw %>%
    mutate(
        orador2 = orador %>%
            str_remove_all("O SR. |A SRA. |\\(|\\)|PRESIDENTE |\\.") %>%
            str_replace("/", " ") %>%
            str_to_upper() %>% 
            str_remove_all(" BLOCO"),
        parlamentar = str_ends(orador2, "- .."),
        partido_uf = if_else(parlamentar, str_trim(str_extract(orador2, " \\w+ - ..$")), NA_character_),
        nome = if_else(parlamentar, str_remove(orador2, " \\w+ - ..$"), orador2)
    ) %>%
    select(-orador2) %>%
    separate(partido_uf, into = c("partido", "uf"), sep = "-")

falas %>% 
    write_csv(here::here("data/ready/falas-comissoes.csv"))
