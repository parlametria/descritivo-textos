library(readr)
library(dplyr)

justificativas = read_csv(
    here::here("data/ready/justificativas_legislatura_atual_13-09-21.csv"),
    col_types = cols(.default = col_character())
) %>%
    janitor::clean_names()

todas_ma = read_csv(
    "https://github.com/parlametria/relatorio-bianuario-ma/raw/main/data/inputs/2-fetch-input/proposicoes/proposicoes_preenchidas.csv",
    col_types = cols(.default = col_character())
)

avaliacoes = read_csv(
    "https://github.com/parlametria/relatorio-bianuario-ma/raw/main/data/ready/proposicoes.csv",
    col_types = cols(.default = col_character())
)

dataset = todas_ma %>%
    filter(casa == "camara", !is.na(classificacao_ambientalismo), 
           !(sigla_tipo %in% c("MPV", "PDL")),
           ano %in% c(2019, 2020)) %>%
    select(
        id,
        sigla_tipo,
        numero,
        ano,
        classificacao_ambientalismo,
        ementa,
        autor,
        link
    ) %>% 
    mutate(relacionada = if_else(
        classificacao_ambientalismo == "Fora do tema",
        0,
        1
    ))

final = dataset %>%  
    left_join(select(justificativas, -ementa)) %>% 
    filter(!is.na(data_apresentacao)) %>% 
    filter(!is.na(justificada), as.logical(justificada))

final %>% 
    filter(!is.na(texto_completo)) %>% 
    write_csv(here::here("data/classificacao/socioambiental-gabarito-20200916.csv"))