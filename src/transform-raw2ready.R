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

transform_comissoes = function(path = "data/raw/comissoes") {
    todos = list.files(
        path = here::here(path),
        recursive = T,
        pattern = "*.csv",
        full.names = T
    )
    
    arq_metadados = todos[basename(todos) == "metadados_transcricoes.csv"]
    transcricoes = todos[basename(todos) != "metadados_transcricoes.csv"]
    
    metadados = read_csv(
        arq_metadados,
        col_types = cols(
            .default = col_character(),
            data = col_date(format = "%d/%m/%Y"),
            horario = col_time(format = "")
        )
    )
    
    falas_raw = read_many_files(transcricoes, col_types = "cicc")
    
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
    
    falas %>% 
        left_join(metadados, by = "id_evento")
}


main <- function(argv = NULL) {
    flog.threshold(TRACE)
    in_comissoes = "data/raw/comissoes"
    out_comissoes = "data/ready/falas-comissoes.csv"
    
    flog.info(str_glue("Transformando falas em comissões de {in_comissoes}"))
    transform_comissoes(in_comissoes) %>%
        write_csv(here::here(out_comissoes))
}


if (!interactive()) {
    argv <- commandArgs(TRUE)
    main(argv)
}