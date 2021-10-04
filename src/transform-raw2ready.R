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

.process_metadados <- function(path_metadados = "data/raw/entidades.csv") {
    metadados_deputados = read_csv(
        path_metadados,
        col_types = cols(.default = col_character())
    ) %>% 
        filter(casa == "camara", is_parlamentar == 1) %>% 
        distinct(id = id_entidade, nome, uf, partido)
    
    metadados_deputados %>% 
        mutate(nome = toupper(nome)) %>% 
        group_by(id) %>% 
        filter(row_number() == n()) %>% 
        ungroup()
}

match_nomes_falas <- function(path_metadados = "data/raw/entidades.csv",
                              path_falas = "data/ready/falas-comissoes.csv"){
    deputados = .process_metadados(path_metadados)
    
    falas = read_csv(path_falas,
                     show_col_types = F) 
    
    falas %>% 
        filter(parlamentar) %>% 
        distinct(nome, uf) %>% 
        mutate(nome_original = nome,
               nome = str_replace_all(
                   nome,
                   c(
                   "DR " = "DR. ",
                   "PR " = "PR. ",
                   "DRA " = "DRA. ",
                   "MAJOR VITOR HUGO" = "VITOR HUGO",
                   "JÚNIOR BOZZELLA" = "BOZZELLA", 
                   "PROFESSOR LUIZÃO" = "LUIZÃO", 
                   "JOÃO H CAMPOS" = "JOÃO H. CAMPOS"
                   )
               )) %>% 
        inner_join(deputados,
                   by = c("nome", "uf"),
                   suffix = c("_falas", "_parla")) %>% 
        distinct() %>% 
        rename(nome_parlametria = nome, id_camara = id)
}

main <- function(argv = NULL) {
    flog.threshold(TRACE)
    in_comissoes = "data/raw/comissoes"
    out_comissoes = "data/ready/falas-comissoes.csv"
    
    flog.info(str_glue("Transformando falas em comissões de {in_comissoes}"))
    transform_comissoes(in_comissoes) %>%
        write_csv(here::here(out_comissoes))
    flog.info(str_glue("Dados de falas em comissões salvos em {out_comissoes}"))
    
    in_metadados_deputados = "data/raw/entidades.csv"
    out_oradores = "data/ready/oradores.csv"
    
    flog.info(str_glue("Realizando o mapeamento orador -> id"))
    match_nomes_falas(path_metadados = in_metadados_deputados,
                      path_falas = out_comissoes) %>% 
        write_csv(here::here(out_oradores))
    flog.info(str_glue("Dados de oradores salvos em {out_oradores}"))
}


if (!interactive()) {
    argv <- commandArgs(TRUE)
    main(argv)
}