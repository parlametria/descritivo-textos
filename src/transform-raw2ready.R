library(dplyr, warn.conflicts = F)
library(tidyr)
library(stringr)
library(readr)
library(purrr, warn.conflicts = F)
library(futile.logger)

read_many_files <- function(files, col_types) {
    files %>%
        map_df(read_csv, col_types = col_types)
}

transform_comissoes = function(path = "data/raw/comissoes-2019") {
    falas_raw = read_many_files(list.files(
        path = here::here(path),
        pattern = "*.csv",
        full.names = T
    ),
    col_types = "cicc")
    
    falas = falas_raw %>%
        mutate(
            orador2 = orador %>%
                str_remove_all("O SR. |A SRA. |\\(|\\)|PRESIDENTE |\\.") %>%
                str_replace("/", " ") %>%
                str_to_upper() %>%
                str_remove_all(" BLOCO"),
            parlamentar = str_ends(orador2, "- .."),
            partido_uf = if_else(parlamentar, str_trim(str_extract(
                orador2, " \\w+ - ..$"
            )), NA_character_),
            nome = if_else(parlamentar, str_remove(orador2, " \\w+ - ..$"), orador2)
        ) %>%
        select(-orador2) %>%
        separate(partido_uf,
                 into = c("partido", "uf"),
                 sep = "-")
    
    falas
}


main <- function(argv = NULL) {
    flog.threshold(TRACE)
    in_comissoes = "data/raw/comissoes-2019"
    out_comissoes = "data/ready/proposicoes.csv"
    
    flog.info(str_glue("Transformando falas em comissões de {in_comissoes}"))
    transform_comissoes(in_comissoes) %>%
        write_csv(here::here(out_comissoes))
}


if (!interactive()) {
    argv <- commandArgs(TRUE)
    main(argv)
}