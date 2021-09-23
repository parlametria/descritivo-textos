library(readr)
library(dplyr)
library(jsonlite)
library(tidyr)

interessam_raw = fromJSON(
    "https://api.leggo.org.br/proposicoes?interesse=transparencia-e-integridade&data_referencia=2021-09-16",
    flatten = T
)

interessam = interessam_raw %>%
    select(sigla_camara) %>%
    filter(!is.na(sigla_camara))

temas_raw = read_csv2(
    c(
        "https://dadosabertos.camara.leg.br/arquivos/proposicoesTemas/csv/proposicoesTemas-2020.csv",
        "https://dadosabertos.camara.leg.br/arquivos/proposicoesTemas/csv/proposicoesTemas-2019.csv"
    ),
    col_types = cols(.default = col_character())
) %>%
    janitor::clean_names()

temas_usados = temas_raw %>%
    mutate(sigla_camara = str_glue("{sigla_tipo} {numero}/{ano}")) %>%
    inner_join(interessam)

temas_usados %>%
    count(tema, sort = T)

interessam_sep = interessam %>%
    separate(sigla_camara, into = c("sigla_tipo", "numero", "ano"))

interessam_sep %>%
    count(ano)

justificativas_raw = read_csv(
    here::here("data/ready/justificativas_legislatura_atual_13-09-21.csv"),
    col_types = cols(.default = col_character())
) %>%
    janitor::clean_names()

just_periodo = justificativas_raw %>%
    filter(ano %in% 2019:2020)

justificativas_match_temas = just_periodo %>%
    left_join(temas_raw) %>%
    filter(tema %in% temas_usados$tema) %>%
    distinct(sigla_tipo, numero, ano)

justificativas = just_periodo %>%
    inner_join(justificativas_match_temas)

positivas = interessam_sep %>%
    filter(ano %in% 2019:2020, sigla_tipo %in% justificativas$sigla_tipo) %>%
    mutate(relevancia = 1)

dataset = justificativas %>% 
    left_join(positivas) %>% 
    mutate(relevancia = if_else(is.na(relevancia), 0, relevancia))

dataset %>% 
    filter(!is.na(texto_completo)) %>% 
    write_csv(here::here("data/classificacao/transparencia-gabarito-20200916.csv"))
