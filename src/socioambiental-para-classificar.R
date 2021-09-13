library(tidyverse)

temas = read_csv2("https://dadosabertos.camara.leg.br/arquivos/proposicoesTemas/csv/proposicoesTemas-2021.csv", 
                  col_types = cols(.default = col_character(), 
                                   numero = col_integer(),
                                   ano = col_integer())) %>% 
    janitor::clean_names()

justificativas = read_csv(here::here("data/ready/justificativas_19-21.csv"),
                          col_types = c("cciTiccccTcccll")) %>% 
    janitor::clean_names()


interessam = temas %>% 
    filter(str_detect(tema, "Meio Ambiente|Fundiária|Agricultura"), 
           sigla_tipo %in% c("PL", "PLP", "PEC"))


interessa_cj = interessam %>% 
    left_join(justificativas)

# FALTANDO
interessa_cj %>%
    filter(is.na(extraivel)) %>%
    transmute(proposicao = str_glue("{sigla_tipo} {numero}/{ano}"))

interessa_cj %>% 
    filter(!is.na(extraivel)) %>% 
    write_csv(here::here("data/ready/socioambiental-2021.csv"))
