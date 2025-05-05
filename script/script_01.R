# PACOTES ----------------------------------------------------------------------

library(janitor)
library(tidyverse)
library(writexl)



# FUNÇÕES ----------------------------------------------------------------------

# função para deflacionar valores com base em um ano escolhido

deflate_num <- function(data, year, var_name, var_nominal) {
  ipca_df <- tibble::tibble(
    ano = 2000:2023,
    ipca = c(5.97, 7.67, 12.53, 9.30, 7.60, 5.69, 3.14, 4.46, 5.90, 4.31,
             5.91, 6.50, 5.84, 5.91, 6.41, 10.67, 6.29, 2.95, 3.75, 4.31,
             4.52, 10.06, 5.79, 4.62)
  )
  
  ipca_deflator <- ipca_df %>%
    mutate(deflator = ipca[ano == {{year}}] / ipca)
  
  data %>%
    left_join(ipca_deflator, by = "ano") %>%
    mutate("{var_name}" := .data[[var_nominal]] * .data[["deflator"]]) %>%
    select(-ipca, -deflator)
}


# função para calcular valores per capita ou por mil habitantes

var_pc_pm <- function(data, var_base, por_mil = FALSE) {
  nome_var <- paste0(var_base, ifelse(por_mil, "_por_mil", "_pc"))
  
  data %>%
    left_join(qt_pop, by = c("cod_mun", "ano")) %>%
    mutate(
      "{nome_var}" := if (por_mil) 
          (.data[[var_base]] * 1000) / qt_pop 
        else 
          .data[[var_base]] / qt_pop
    ) %>%
    rename(nome_mun = nome_mun.x) %>%
    select(-nome_mun.y) %>%
    filter(!is.na(qt_pop)) %>%
    arrange(cod_mun) %>%
    select(-qt_pop)
}


# função para interpolar para anos faltantes

interpolate_num <- function(data, var_name, year_range) {
  data %>%
    complete(cod_mun, ano = full_seq(year_range, 1)) %>%
    group_by(cod_mun) %>%
    arrange(ano) %>%
    mutate(
      nome_mun = first(na.omit(nome_mun)),
      "{var_name}" := if (sum(!is.na(.data[[var_name]])) >= 2) {
        approx(x = ano, y = .data[[var_name]], xout = ano, rule = 2)$y
      } else {
        .data[[var_name]]
      }
    ) %>%
    ungroup() %>%
    relocate(cod_mun, .after = ano)
}


# função para limpar o número e transformar em numérico

clear_num <- function(x) {
  x %>%
    stringr::str_replace_all("[^0-9,\\.]", "") %>%
    stringr::str_replace_all("\\.", "") %>%
    stringr::str_replace(",", ".") %>%
    as.numeric()
}

# ESF --------------------------------------------------------------------------

# tratamento dos dados de esf de 1999 a 2006, selecionado somente dezembro

#Fiz um código para ler o csv. O prblema era o encoding. Comentei o seu, mas sem
#problemas se rodar pelo seu também


esf_x1999_x2006 <- read.csv2("dados/dados_brutos/ESF 1999 a 2006.csv", fileEncoding = "latin1") %>%
  clean_names() %>%
  rename("percent_cobertura" = x_cobertura)

# esf_x1999_x2006 <- readxl::read_xlsx("dados/dados_brutos/ESF 1999 a 2006.xlsx") %>% 
#   clean_names()

esf_1999_2006 <- esf_x1999_x2006 %>%
  select(-percent_cobertura, -mun6) %>% 
  filter(mes == 12) %>% 
  rename(
    cod_mun = codigo,
    nome_mun = municipio,
    qt_pop = populacao,
    qt_cob_esf = pop_acomp
  ) %>% 
  mutate(
    pct_cob_esf = (qt_cob_esf / qt_pop) * 100, 
    cod_mun = str_sub(as.character(cod_mun), 1, 6),
    nome_mun = str_to_upper(nome_mun)
  ) %>% 
  relocate(
    ano, cod_mun, nome_mun, qt_cob_esf, pct_cob_esf
  ) %>% 
  arrange(cod_mun)


# calculando o grau de cobertura de esf

# esf_1999_2006 <- esf_1999_2006 %>%
#   arrange(cod_mun, ano) %>%
#   mutate(
#     grau_cob_esf = case_when(
#       pct_cob_esf == 0 ~ 0,
#       pct_cob_esf < 30 ~ 1,
#       pct_cob_esf >= 30 & pct_cob_esf < 70 ~ 2,
#       pct_cob_esf >= 70 ~ 3
#     )
#   )
# 
# esf_1999_2006 <- esf_1999_2006 %>%
#   group_by(cod_mun) %>%
#   mutate(
#     cobertura_70 = pct_cob_esf >= 70,
#     grau_cob_esf = {
#       grau_temp <- grau_cob_esf
#       n_linhas <- n()
#       if (n_linhas >= 4) {
#         for (i in 4:n_linhas) {
#           if (all(cobertura_70[(i - 3):i])) {
#             grau_temp[i:n_linhas] <- 3
#             break
#           }
#         }
#       }
#       grau_temp
#     }
#   ) %>%
#   ungroup() %>% 
#   select(-cobertura_70) %>% 
#   relocate(grau_cob_esf, esf_implantada, .after = pct_cob_esf)

#Usando uma função chamada rle (run length encoding)

#Roda isso abaixo e nota o que ele cospe.

#Pensa no vetor dentro da função como um apanhado de resultados de cara e coroa

rle(c("H", "T", "T", "H", "H", "H", "H", "H", "T", "H"))

#A função retorna dois argumetos, lengths e values. O primeiro 
#(que é o que queremos) diz quantas vezes consecutivas o elemento aparece
#Então "H", o primeiro elemento do vetor, ficou com length = 1 e "T", o segundo
#elemento do vetor, que aparece consecutivamente duas vezes, ficou com length = 2

esf_1999_2006 <- esf_1999_2006 %>%
  arrange(cod_mun, ano) %>% 
  group_by(cod_mun) %>%
  mutate(
    cobertura_70 = pct_cob_esf >= 70
    )

#Se fizermos rle(esf_1999_2006$cobertura_70), vamos obter a quantidade de vezes que
#a variavel cobertura_70 se repete como TRUE ou como FALSE. Vamos usar isso na
#nossa base. Mas para isso, precisamos expandir esse vetor para os números se repetirem
#e encaixarem certinho em cada linha da base

esf_1999_2006 <- esf_1999_2006 %>% 
  group_by(cod_mun) %>% 
  mutate(vezes_consec = rep(rle(cobertura_70)$lengths, rle(cobertura_70)$lengths))

#Agora nossa variavel vezes_consec traz, para cada linha, a quantidade de vezes que aquela
#linha apareceu como TRUE ou FALSE consecutivamente.

#Com isso, temos uma variavel que nos diz se aquela linha tem cobertura >= 70, resultando
# em TRUE ou FALSE. E uma variavel que nos diz quantas vezes aquele TRUE ou FALSE se repete,
#que é a variavel vezes_consec.

#Agora é só pegar os municipios que tem cobertura_70 == TRUE e vezes_consec >= 4


esf_1999_2006 <- esf_1999_2006 %>% 
  mutate(grau_cob_esf = case_when(
    pct_cob_esf == 0 ~ 0,
    pct_cob_esf < 30 ~ 1,
    pct_cob_esf >= 30 & pct_cob_esf < 70 ~ 2,
    (cobertura_70 == TRUE & vezes_consec >= 4) ~ 3
  ))

esf_1999_2006 <- esf_1999_2006 %>% 
  select(ano,
         cod_mun,
         nome_mun,
         qt_cob_esf,
         pct_cob_esf,
         mes,
         uf,
         qt_pop,
         competencia,
         teto_de_esf,
         esf_qualificadas,
         esf_no_siab,
         esf_implantada,
         uf_comp,
         grau_cob_esf
  )
  
#Funciona, mas nota que nosso case_when está omitindo um outro caso,
#o caso em que a cobertura é maior do que 70 mas por vezes consecutivas menores
#do que 4. Para a pesquisa não vai ser um problema, Dani só quer estes casos acima.

# tratamento dos dados de esf de 2007 a 2020, selecionado somente dezembro

# lendo as abas do Excel, na qual cada uma contém o dado de um ano 

abas_esf_x2007_x2020 <- readxl::excel_sheets("dados/dados_brutos/ESF-2007-202012.xlsx")

dados_esf_x2007_x2020 <- map(abas_esf_x2007_x2020, function(x)
{readxl::read_xlsx("dados/dados_brutos/ESF-2007-202012.xlsx", sheet = x)})


# removendo a última aba, que não contém dados

dados_esf_x2007_x2020[15]

dados_esf_x2007_x2020 <- dados_esf_x2007_x2020[-15]


# juntando os dados de esf em base única

base_esf_x2007_x2020 <- list_rbind(dados_esf_x2007_x2020)


# tratando os dados de esf de 2007 a 2020

esf_2007_2020 <- base_esf_x2007_x2020 %>% 
  filter(str_sub(NU_COMPETENCIA, 5, 6) == "12") %>% 
  select(-c(QT_EQUIPE_AB_PARAMETRIZADA, QT_CH_MEDICO, QT_CH_ENFERMEIRO, QT_EQUIPE_AB_EQUIVALENTE_CH,
            NU_ANO_POPULACAO_CONSIDERADA, TP_ORIGEM_BASE_POPULACAO)) %>% 
  clean_names() %>% 
  mutate(ano = str_sub(nu_competencia, 1, 4)) %>% 
  relocate(ano, .before = nu_competencia) %>% 
  rename(
    cod_uf_ibge = co_uf_ibge, 
    sg_uf = sg_uf, 
    cod_mun = co_municipio_ibge, 
    nome_mun = no_municipio_acentuado,
    qt_pop = qt_populacao, 
    qt_cob_esf = qt_cobertura_sf
  ) %>% 
  mutate(
    ano = as.numeric(ano),
    qt_pop = clear_num(qt_pop),
    qt_cob_esf = clear_num(qt_cob_esf),
    pct_cob_esf = (qt_cob_esf / qt_pop) * 100, 
    qt_equipe_sf = as.numeric(qt_equipe_sf)
  ) %>% 
  select(-qt_equipe_sf_ab, -pc_cobertura_sf, -qt_cobertura_ab, -pc_cobertura_ab) %>% 
  rename(
    cod_regiao = co_regiao,
    nome_regiao = no_regiao,
    cod_uf = cod_uf_ibge,
    nome_uf = no_uf_acentuado
  ) %>% 
  relocate(ano, cod_mun, nome_mun, qt_cob_esf, pct_cob_esf, qt_equipe_sf, cod_regiao, nome_regiao, 
           sg_regiao, cod_uf)


# calculando o grau de cobertura de esf de 2007 a 2020

#Comentei o que voce fez mas pode descomentar se quiser.

# esf_2007_2020 <- esf_2007_2020 %>%
#   arrange(cod_mun, ano) %>%
#   mutate(
#     grau_cob_esf = case_when(
#       pct_cob_esf == 0 ~ 0,
#       pct_cob_esf < 30 ~ 1,
#       pct_cob_esf >= 30 & pct_cob_esf < 70 ~ 2,
#       pct_cob_esf >= 70 ~ 2
#     )
#   )
# 
# esf_2007_2020 <- esf_2007_2020 %>%
#   group_by(cod_mun) %>%
#   mutate(
#     cobertura_70 = pct_cob_esf >= 70,
#     grau_cob_esf = {
#       grau_temp <- grau_cob_esf
#       n_linhas <- n()
#       if (n_linhas >= 4) {
#         for (i in 4:n_linhas) {
#           if (all(cobertura_70[(i - 3):i])) {
#             grau_temp[i:n_linhas] <- 3
#             break
#           }
#         }
#       }
#       grau_temp
#     }
#   ) %>%
#   ungroup() %>% 
#   select(-cobertura_70) %>% 
#   relocate(grau_cob_esf, .after = pct_cob_esf)
# 
# esf_2007_2020 <- esf_2007_2020 %>%
#   arrange(cod_mun, ano) %>% 
#   group_by(cod_mun) %>%
#   mutate(
#     cobertura_70 = pct_cob_esf >= 70)

esf_2007_2020 <- esf_2007_2020 %>% 
  mutate(cobertura_70 = pct_cob_esf >= 70)

esf_2007_2020 <- esf_2007_2020 %>% 
  group_by(cod_mun) %>% 
  mutate(vezes_consec = rep(rle(cobertura_70)$lengths, rle(cobertura_70)$lengths))

esf_2007_2020 <- esf_2007_2020 %>% 
  mutate(grau_cob_esf = case_when(
    pct_cob_esf == 0 ~ 0,
    pct_cob_esf < 30 ~ 1,
    pct_cob_esf >= 30 & pct_cob_esf < 70 ~ 2,
    (cobertura_70 == TRUE & vezes_consec >= 4) ~ 3
  ))

#Juntando ambas as bases

esf_1999_2006 %>% colnames()

esf_2007_2020 %>% colnames()

setdiff(colnames(esf_1999_2006),colnames(esf_2007_2020))

setdiff(colnames(esf_2007_2020),colnames(esf_1999_2006))

base_esf <- 
  bind_rows(esf_1999_2006 %>% 
            select(-c(mes,uf,competencia, teto_de_esf, uf_comp)) %>% 
            mutate(qt_equipe_sf = NA),
          esf_2007_2020 %>% 
            select(-c(cobertura_70, vezes_consec, nu_competencia, nome_uf, sg_uf, cod_regiao, sg_regiao, nome_regiao)) %>% 
            mutate(esf_qualificadas = NA,
                   esf_no_siab = NA,
                   esf_implantada = NA)) %>% 
  ungroup()

#Coluna que não tinha na base esf_1999_2006 = qt_equipe_sf, colunas que não tinham na base esf_2007_2020 = c(esf_qualificadas, esf_no_siab, esf_implantada)

# POP --------------------------------------------------------------------------

# tratamento dos dados de população de 2000 a 2020, selecionando somente dezembro

qt_pop_2000_2006 <- esf_1999_2006 %>% 
  filter(mes == "12", ano >= "2000" & ano <= "2006") %>% 
  select(ano, cod_mun, nome_mun, qt_pop) %>% 
  mutate(
    ano = as.numeric(ano)
  )

qt_pop_2007_2020 <- esf_2007_2020 %>% 
  filter(str_sub(nu_competencia, 5, 6) == "12") %>% 
  select(ano, cod_mun, nome_mun, qt_pop) %>% 
  mutate(
    ano = as.numeric(ano)
  )


# juntando os dados de população

qt_pop <- bind_rows(qt_pop_2000_2006, qt_pop_2007_2020) %>% 
  arrange(cod_mun)


writexl::write_xlsx(qt_pop, "dados/dados_tratados/qt_pop.xlsx")

qt_pop <- readxl::


# calculando o porte dos municípios

pop_porte_mun <- qt_pop %>% 
  select(ano, cod_mun, nome_mun, qt_pop) %>% 
  mutate(
    porte_mun = case_when(
      qt_pop < 50000 ~ 1,
      qt_pop >= 50000 & qt_pop < 75000 ~ 2,
      qt_pop >= 75000 & qt_pop < 100000 ~ 3,
      qt_pop >= 100000 & qt_pop < 500000 ~ 4,
      qt_pop >= 500000 & qt_pop < 2000000 ~ 5,
      qt_pop >= 2000000 & qt_pop < 5000000 ~ 6,
      qt_pop >= 5000000 ~ 7)
  ) %>% 
  select(-qt_pop)

# PIB --------------------------------------------------------------------------

# tratamento dados do PIB de 2002 a 2009

pib_x2002_x2009 <- readxl::read_xls("dados/dados_brutos/PIB_2002_2009.xls") %>%
  clean_names()

pib_2002_2009 <- pib_x2002_x2009 %>%
  select(
    ano, 
    codigo_do_municipio, 
    nome_do_municipio, 
    produto_interno_bruto_a_precos_correntes_r_1_000
  ) %>% 
  rename(
    cod_mun = codigo_do_municipio,
    nome_mun = nome_do_municipio,
    pib_nom = produto_interno_bruto_a_precos_correntes_r_1_000
  ) %>% 
  mutate(
    cod_mun = str_sub(as.character(cod_mun), 1, 6),
    nome_mun = str_to_upper(nome_mun), 
    pib_nom = pib_nom * 1000
  ) 


# tratamento dos dados do PIB de 2010 a 2021

pib_x2010_x2021 <- readxl::read_xlsx("dados/dados_brutos/PIB_2010_2021.xlsx") %>%
  clean_names()

pib_2010_2021 <- pib_x2010_x2021 %>%
  select(
    ano, 
    codigo_do_municipio, 
    nome_do_municipio, 
    produto_interno_bruto_a_precos_correntes_r_1_000
  ) %>% 
  rename(
    cod_mun = codigo_do_municipio,
    nome_mun = nome_do_municipio,
    pib_nom = produto_interno_bruto_a_precos_correntes_r_1_000
  ) %>% 
  mutate(
    cod_mun = str_sub(as.character(cod_mun), 1, 6),
    nome_mun = str_to_upper(nome_mun), 
    pib_nom = pib_nom * 1000
  )


# tratamento dos dados auxiliares do PIB 2002 a 2009 

pib_aux_01 <- pib_x2002_x2009 %>%
  select(
    ano, 
    codigo_do_municipio, 
    nome_do_municipio,
    codigo_da_grande_regiao,
    nome_da_grande_regiao, 
    codigo_da_unidade_da_federacao,
    nome_da_unidade_da_federacao
  ) %>% 
  arrange(
    desc(ano)
  ) %>% 
  rename(
    cod_mun = codigo_do_municipio,
    nome_mun = nome_do_municipio,
    cod_gr_ibge = codigo_da_grande_regiao,
    nome_gr = nome_da_grande_regiao,
    cod_uf_ibge = codigo_da_unidade_da_federacao,
    nome_uf = nome_da_unidade_da_federacao
  )


# tratamento dos dados auxiliares do PIB de 2010 a 2021

pib_aux_02 <- pib_x2010_x2021 %>%
  select(
    ano, 
    codigo_do_municipio, 
    nome_do_municipio,
    codigo_da_grande_regiao,
    nome_da_grande_regiao, 
    codigo_da_unidade_da_federacao,
    nome_da_unidade_da_federacao
  ) %>% 
  arrange(
    desc(ano)
  ) %>% 
  rename(
    cod_mun = codigo_do_municipio,
    nome_mun = nome_do_municipio,
    cod_gr_ibge = codigo_da_grande_regiao,
    nome_gr = nome_da_grande_regiao,
    cod_uf_ibge = codigo_da_unidade_da_federacao,
    nome_uf = nome_da_unidade_da_federacao
  )


# juntando os dados do PIB de 2002 a 2021

pib <- bind_rows(pib_2002_2009, pib_2010_2021) %>% 
  arrange(cod_mun)


# colocando a preços constantes de 2019

pib <- deflate_num(pib, 2019, "pib_real_2019", "pib_nom")


# calculando o PIB per capita a preços constantes de 2019

pib <- var_pc_pm(pib, "pib_real_2019")


# juntando os dados auxiliares do PIB de 2002 a 2021

ref_cod_mun_aux <- bind_rows(pib_aux_01, pib_aux_02)

ref_cod_mun_aux_2021 <- ref_cod_mun_aux %>% 
  filter(ano == max(ano))


# carregamento dos dados para o Excel

writexl::write_xlsx(pib, "dados/dados_tratados/pib_2002_2021.xlsx")

writexl::write_xlsx(ref_cod_mun_aux_2021, "dados/dados_tratados/ref_cod_mun_aux_tratado.xlsx")



# URBAN ------------------------------------------------------------------------

# tratamento dos dados de urbanização de 2022

urban_x2022 <- readxl::read_xlsx("dados/dados_brutos/urban_2022.xlsx") %>% 
  clean_names()

urban_2022 <- urban_x2022 %>%
  select(x2, x3, x6) %>% 
  rename(
    cod_mun = x2,
    nome_mun = x3,
    tx_urban = x6
  ) %>% 
  slice(-(1:4), -n()) %>%
  mutate(
    ano = 2022,
    tx_urban = as.numeric(tx_urban) / 100,
    nome_mun = str_remove(nome_mun, "\\(.*\\)"),
    nome_mun = str_trim(nome_mun),
    cod_mun = str_sub(as.character(cod_mun), 1, 6),
    nome_mun = str_to_upper(nome_mun)
  ) %>% 
  relocate(ano)


# tratamento dos dados de urbanização de 1991, 2000 e 2010

urban_x1991_x2000_x2010 <- readxl::read_xlsx("dados/dados_brutos/urban_1991_2000_2010.xlsx") %>%
  clean_names()

urban_1991_2000_2010 <- urban_x1991_x2000_x2010 %>%
  select(x2, x3, x6, x12, x18) %>% 
  rename(
    cod_mun = x2,
    nome_mun = x3,
    "1991" = x6,
    "2000" = x12,
    "2010" = x18
  ) %>% 
  slice(-(1:5), -n()) %>%
  pivot_longer(
    cols = c("1991", "2000", "2010"),
    names_to = "ano",
    values_to = "tx_urban"
  ) %>% 
  mutate(
    nome_mun = str_remove(nome_mun, "\\(.*\\)"),
    nome_mun = str_trim(nome_mun),
    ano = as.numeric(ano),
    tx_urban = as.numeric(tx_urban) / 100,
    cod_mun = str_sub(as.character(cod_mun), 1, 6),
    nome_mun = str_to_upper(nome_mun)
  ) %>% 
  relocate(ano) %>% 
  arrange(ano)


# juntando os dados de 1991, 2000, 2010 e 2022

urban_1991_2022 <- bind_rows(urban_1991_2000_2010, urban_2022)


# interpolando para os anos faltantes

urban <- interpolate_num(urban_1991_2022, "tx_urban", 1991:2022)


# carregamento dos dados para o Excel

writexl::write_xlsx(urban_1991_2022, "dados/dados_tratados/urban_1991_2022.xlsx")



# ANALF ------------------------------------------------------------------------

# tratamento dos dados de analfabetismo de 1991, 2000, 2010 e 2022

analf_x1991_x2022 <- readxl::read_xls("dados/dados_brutos/analfab_1991_2022.xls") %>% 
  clean_names()

analf_1991_2022 <- analf_x1991_x2022 %>% 
  select(-sigla) %>%
  rename(
    cod_mun = codigo,
    nome_mun = municipio,
    "1991" = x1991,
    "2000" = x2000,
    "2010" = x2010,
    "2022" = x2022
  ) %>% 
  pivot_longer(
    cols = 3:6,
    names_to = "ano",
    values_to = "tx_analf"
  ) %>% 
  mutate(
    ano = as.numeric(ano),
    tx_analf = tx_analf / 100, 
    cod_mun = str_sub(as.character(cod_mun), 1, 6),
    nome_mun = str_to_upper(nome_mun)
  ) %>% 
  relocate(ano) %>% 
  arrange(cod_mun)
  

# interpolando para os anos faltantes

analf <- interpolate_num(analf_1991_2022, "tx_analf", 1991:2022)


# carregamento dos dados para o Excel

writexl::write_xlsx(analf_1991_2022, "dados/dados_tratados/analf_1991_2022.xlsx")



# ÁGUA -------------------------------------------------------------------------

# tratamento dos dados de água de 2000

agua_x2000 <- readxl::read_xlsx("dados/dados_brutos/agua_2000.xlsx") %>% 
  clean_names()

agua_2000 <- agua_x2000 %>% 
  select(x2, x3, x5) %>% 
  rename(
    cod_mun = x2,
    nome_mun = x3,
    agua = x5
  ) %>% 
  slice(-(1:4), -n()) %>%
  mutate(
    nome_mun = str_remove(nome_mun, "\\(.*\\)"),
    nome_mun = str_trim(nome_mun),
    ano = 2000,
    agua = as.numeric(agua) / 100,
    cod_mun = str_sub(as.character(cod_mun), 1, 6),
    nome_mun = str_to_upper(nome_mun)
  ) %>% 
  relocate(ano) %>% 
  arrange(cod_mun)


# tratamento dos dados de água de 2010

agua_x2010 <- readxl::read_xlsx("dados/dados_brutos/agua_2010.xlsx") %>% 
  clean_names()

agua_2010 <- agua_x2010 %>% 
  select(x2, x3, x6) %>% 
  rename(
    cod_mun = x2,
    nome_mun = x3,
    agua = x6
  ) %>% 
  slice(-(1:5), -n()) %>% 
  mutate(
    nome_mun = str_remove(nome_mun, "\\(.*\\)"),
    nome_mun = str_trim(nome_mun),
    ano = 2010,
    agua = as.numeric(agua) / 100, 
    cod_mun = str_sub(as.character(cod_mun), 1, 6),
    nome_mun = str_to_upper(nome_mun)
  ) %>% 
  relocate(ano) %>% 
  arrange(cod_mun)


# tratamento dos dados de água de 2022

agua_x2022 <- readxl::read_xlsx("dados/dados_brutos/agua_2022.xlsx") %>% 
  clean_names()

agua_2022 <- agua_x2022 %>% 
  select(x2, x3, x5, x7) %>% 
  rename(
    cod_mun = x2,
    nome_mun = x3,
    agua_01 = x5,
    agua_02 = x7
  ) %>% 
  slice(-(1:6), -n()) %>%
  mutate(
    nome_mun = str_remove(nome_mun, "\\(.*\\)") %>% str_trim(),
    ano = 2022,
    across(c(agua_01, agua_02), as.numeric),
    agua = (agua_01 + agua_02) / 100, 
    cod_mun = str_sub(as.character(cod_mun), 1, 6),
    nome_mun = str_to_upper(nome_mun)
  ) %>% 
  relocate(ano) %>% 
  select(-c(agua_01, agua_02)) %>% 
  arrange(cod_mun)


# juntando os dados de água de 2000, 2010 e 2022

agua_2000_2010_2022 <- bind_rows(agua_2000, agua_2010, agua_2022)


# interpolando para os anos faltantes

agua  <- interpolate_num(agua_2000_2010_2022, "agua", 2000:2022)


# carregamento dos dados para o Excel

writexl::write_xlsx(agua_2000_2010_2022, "dados/dados_tratados/agua_2000_2010_2022.xlsx")



# ESGOTO -----------------------------------------------------------------------

# tratamento dos dados de esgoto de 2000

esgoto_x2000 <- readxl::read_xlsx("dados/dados_brutos/esgoto_2000.xlsx") %>% 
  clean_names()

esgoto_2000 <- esgoto_x2000 %>% 
  select(x2, x3, x5) %>% 
  rename(
    cod_mun = x2,
    nome_mun = x3,
    esgoto = x5
  ) %>% 
  slice(-(1:4), -n()) %>% 
  mutate(
    nome_mun = str_remove(nome_mun, "\\(.*\\)") %>% str_trim(),
    ano = 2000,
    esgoto = as.numeric(esgoto) / 100, 
    cod_mun = str_sub(as.character(cod_mun), 1, 6),
    nome_mun = str_to_upper(nome_mun)
  ) %>% 
  relocate(ano) %>% 
  arrange(cod_mun)


# tratamento dos dados de esgoto de 2010

esgoto_x2010 <- readxl::read_xlsx("dados/dados_brutos/esgoto_2010.xlsx") %>% 
  clean_names()

esgoto_2010 <- esgoto_x2010 %>% 
  select(x2, x3, x4, x6) %>% 
  rename(
    cod_mun = x2,
    nome_mun = x3,
    esgoto_01 = x4,
    esgoto_02 = x6
  ) %>% 
  slice(-(1:8), -n()) %>%
  mutate(
    nome_mun = str_remove(nome_mun, "\\(.*\\)") %>% str_trim(),
    ano = 2010,
    across(c(esgoto_01, esgoto_02), as.numeric),
    esgoto = (esgoto_01 + esgoto_02) / 100, 
    cod_mun = str_sub(as.character(cod_mun), 1, 6),
    nome_mun = str_to_upper(nome_mun)
  ) %>% 
  relocate(ano) %>% 
  select(-c(esgoto_01, esgoto_02)) %>% 
  arrange(cod_mun)


# tratamento dos dados de esgoto de 2022

esgoto_x2022 <- readxl::read_xlsx("dados/dados_brutos/esgoto_2022.xlsx") %>% 
  clean_names()

esgoto_2022 <- esgoto_x2022 %>% 
  select(x2, x3, x5) %>% 
  rename(
    cod_mun = x2,
    nome_mun = x3,
    esgoto = x5
  ) %>% 
  slice(-(1:5), -n()) %>%
  mutate(
    nome_mun = str_remove(nome_mun, "\\(.*\\)") %>% str_trim(),
    ano = 2022,
    esgoto = as.numeric(esgoto) / 100, 
    cod_mun = str_sub(as.character(cod_mun), 1, 6),
    nome_mun = str_to_upper(nome_mun)
  ) %>% 
  relocate(ano) %>% 
  arrange(cod_mun)


# juntando os dados de 2000, 2010 e 2022

esgoto_2000_2010_2022 <- bind_rows(esgoto_2000, esgoto_2010, esgoto_2022)


# interpolando para os anos faltantes

esgoto <- interpolate_num(esgoto_2000_2010_2022, "esgoto", 2000:2022)



# BOLSA FAM --------------------------------------------------------------------

# tratamento dos dados de bolsa família de 2004 a 2023

bf_x2004_x2023 <- readxl::read_xls("dados/dados_brutos/BF_2004_2023.xls")

bf_2004_2023 <- bf_x2004_x2023 %>% 
  select(-Sigla) %>% 
  rename(
    cod_mun = Codigo,
    nome_mun = Município
  ) %>% 
  pivot_longer(
    cols = -(1:2),
    names_to = "ano",
    values_to = "bf"
  ) %>% 
  mutate(
    ano = as.numeric(ano), 
    cod_mun = str_sub(as.character(cod_mun), 1, 6),
    nome_mun = str_to_upper(nome_mun)
    ) %>% 
  relocate(ano) %>% 
  arrange(cod_mun)


# calculando bf a preços constantes de 2019

bf <- deflate_num(bf_2004_2023, 2019, "bf_2019", "bf")


# calculando bf per capita a preços constantes de 2019

bf <- bf %>% 
  left_join(qt_pop, by = c("cod_mun", "ano")) %>% 
  mutate(
    bf_2019_pc = (bf_2019 * 1000) / qt_pop
  ) %>% 
  rename(nome_mun = nome_mun.x) %>% 
  select(-nome_mun.y) %>% 
  filter(!is.na(qt_pop)) %>% 
  arrange(cod_mun) %>% 
  select(-qt_pop)



# CONS MED ------------------------------------------------------------------

# tratamento dos dados de consulta médica de 2008 a 2020

con_med_x2008_x2020 <- read.csv2("dados/dados_brutos/consultamedico_2008_2020.csv", fileEncoding = "latin1", skip = 5) %>% 
  clean_names() %>% 
  as.tibble()

#con_med_x2008_x2020 <- readxl::read_xlsx("dados/dados_brutos/consultamedico_2008_2020.xlsx") %>% 
#  clean_names()

con_med_2008_2020 <- con_med_x2008_x2020 %>%
  select(-last_col()) %>% 
  separate(
    municipio, 
    into = c("cod_mun", "nome_mun"), 
    sep = " ", 
    extra = "merge"
    ) %>% 
  rename_with(
    ~ gsub("^x(\\d{4})_dez$", "\\1", .), 
    starts_with("x")
  ) %>% 
  pivot_longer(
    cols = -(1:2),
    names_to = "ano",
    values_to = "consulta_med"
  ) %>% 
  mutate(
    ano = as.numeric(ano),
    consulta_med = as.numeric(consulta_med), 
    cod_mun = str_sub(as.character(cod_mun), 1, 6),
    nome_mun = str_to_upper(nome_mun)
  ) %>% 
  relocate(ano) %>% 
  arrange(cod_mun)

#Removendo as linhas que não são municipios

con_med_2008_2020 <- con_med_2008_2020 %>% filter(str_detect(cod_mun, "^\\d{6}$"))

#Colocando 0 nos municipios com NA

con_med_2008_2020 <- con_med_2008_2020 %>% 
  mutate(consulta_med = if_else(is.na(consulta_med), 0, consulta_med))

# calculando consultas médicas por mil habitantes

cons_med <- var_pc_pm(con_med_2008_2020, "consulta_med", por_mil = T)



# VISITAS ----------------------------------------------------------------------

# tratamento dos dados de visitas de 2008 a 2020

visita_x2008_x2020 <- read.csv2("dados/dados_brutos/visita_2008_2020.csv", fileEncoding = "latin1", skip = 5) %>% 
  clean_names() %>% as.tibble()

#visita_x2008_x2020 <- readxl::read_xlsx("dados/dados_brutos/visita_2008_2020.xlsx")

visita_2008_2020 <- visita_x2008_x2020 %>% 
  select(-last_col()) %>% 
  separate(
    municipio, 
    into = c("cod_mun", "nome_mun"), 
    sep = " ", 
    extra = "merge"
  ) %>% 
  rename_with(
    ~ gsub("^x(\\d{4})_dez$", "\\1", .), 
    starts_with("x")) %>% 
  pivot_longer(
    cols = -(1:2),
    names_to = "ano",
    values_to = "visitas"
  ) %>%
  mutate(
    ano = as.numeric(ano),
    visitas = as.numeric(visitas), 
    cod_mun = str_sub(as.character(cod_mun), 1, 6),
    nome_mun = str_to_upper(nome_mun)
  ) %>% 
  relocate(ano) %>% 
  arrange(cod_mun)

#Removendo as linhas que não são municipios

visita_2008_2020 <- visita_2008_2020 %>% filter(str_detect(cod_mun, "^\\d{6}$"))

#Colocando 0 nos municipios com NA

visita_2008_2020 <- visita_2008_2020 %>% 
  mutate(visitas = if_else(is.na(visitas), 0, visitas))

# calculando visitas por mil habitantes

visita <- var_pc_pm(visita_2008_2020, "visitas", por_mil = T)



# CONS PROF -----------------------------------------------------------------

# tratamento dos dados de consultas profissionais de 2008 a 2020

con_prof_x2008_x2020 <- read.csv2("dados/dados_brutos/consultaprofsup_2008_2020.csv", fileEncoding = "latin1", skip = 5) %>% 
  clean_names() %>% as.tibble()

#con_prof_x2008_x2020 <- readxl::read_xlsx("dados/dados_brutos/consultaprofsup_2008_2020.xlsx") %>% 
#  clean_names()



cons_prof_2008_2020 <- con_prof_x2008_x2020 %>%
  slice(1:(which(con_prof_x2008_x2020$municipio == "Total")-1)) %>% 
  select(-last_col()) %>% 
  separate(
    municipio, 
    into = c("cod_mun", "nome_mun"), 
    sep = " ", 
    extra = "merge"
  ) %>% 
  rename_with(
    ~ gsub("^x(\\d{4})_dez$", "\\1", .), 
    starts_with("x")) %>% 
  rename_with(
    ~ gsub("/.*$", "", .), 
    starts_with("2")
  ) %>% 
  pivot_longer(
    cols = -(1:2),
    names_to = "ano",
    values_to = "consulta_prof_sup"
  ) %>% 
  mutate(
    across(c(ano, consulta_prof_sup), as.numeric), 
    cod_mun = str_sub(as.character(cod_mun), 1, 6),
    nome_mun = str_to_upper(nome_mun)
  ) %>% 
  relocate(ano) %>% 
  arrange(cod_mun)

#Colocando 0 nos municipios com NA

cons_prof_2008_2020 <- cons_prof_2008_2020 %>% 
  mutate(consulta_prof_sup = if_else(is.na(consulta_prof_sup), 0, consulta_prof_sup))


# calculando consultas profissionais por mil habitantes

cons_prof <- var_pc_pm(cons_prof_2008_2020, "consulta_prof_sup", por_mil = T)



# LEITOS -----------------------------------------------------------------------

# tratamento dos dados de leitos de 2005 a 2020

leitos_x2005_x2020 <- read.csv2("dados/dados_brutos/leitos_2005_2020.csv", fileEncoding = "latin1", skip = 3) %>% 
  clean_names() %>% as.tibble(.)

leitos_2005_2020 <- leitos_x2005_x2020 %>% 
  slice(1:(n() - 13)) %>% 
  separate(
    municipio, 
    into = c("cod_mun", "nome_mun"), 
    sep = " ", 
    extra = "merge"
  ) %>% 
  rename_with(
    ~ gsub("^x(\\d{4})_dez$", "\\1", .), 
    starts_with("x")) %>% 
  rename_with(
    ~ gsub("/.*$", "", .), 
    starts_with("2")
  ) %>%
  pivot_longer(
    cols = -(1:2),
    names_to = "ano",
    values_to = "leitos"
  ) %>% 
  mutate(
    across(c(ano, leitos), as.numeric), 
    cod_mun = str_sub(as.character(cod_mun), 1, 6),
    nome_mun = str_to_upper(nome_mun)
  ) %>% 
  relocate(ano) %>% 
  arrange(cod_mun)

#Colocando 0 nos municipios com NA

cons_prof_2008_2020 <- cons_prof_2008_2020 %>% 
  mutate(consulta_prof_sup = if_else(is.na(consulta_prof_sup), 0, consulta_prof_sup))

# calculando leitos por mil habitantes

leitos <- var_pc_pm(leitos_2005_2020, "leitos", por_mil = T)



# DESP PROP ----------------------------------------------------------------

# tratamento dos dados de despesas com recursos próprios de 2000 a 2021


desp_prop_x2000_x2021 <- read.csv2("dados/dados_brutos/desppropria_2000_2021.csv", fileEncoding = "latin1", skip = 3) %>% 
  clean_names() %>% as.tibble(.)

# desp_prop_x2000_x2021 <- readxl::read_xlsx("dados/dados_brutos/desppropria_2000_2021.xlsx") %>% 
#   clean_names()

desp_prop_2000_2021 <- desp_prop_x2000_x2021 %>% 
  select(-last_col()) %>% 
  slice(-(1:2), -n()) %>% 
  separate(
    munic_br, 
    into = c("cod_mun", "nome_mun"), 
    sep = " ", 
    extra = "merge"
  ) %>% 
  pivot_longer(
    cols = -c(cod_mun, nome_mun),
    names_to = "ano",
    values_to = "desp_prop"
  ) %>% 
  mutate(
    ano = str_sub(ano,2,5),
    across(c(ano, desp_prop), as.numeric), 
    cod_mun = str_sub(as.character(cod_mun), 1, 6),
    nome_mun = str_to_upper(nome_mun)
  ) %>% 
  relocate(ano) %>% 
  arrange(cod_mun) 


# colocando a preços constantes de 2019

desp_prop <- deflate_num(desp_prop_2000_2021, 2019, "desp_prop_2019", "desp_prop")


# calculando a despesa per capita a preços constantes de 2019

desp_prop <- var_pc_pm(desp_prop, "desp_prop_2019")



# DESP SAÚDE ----------------------------------------------------------------

# tratamento dos dados de despesa total em saúde de 2000 a 2021

desp_tot_x2000_x2021 <- read.csv2("dados/dados_brutos/desptot_2000_2021.csv", fileEncoding = "latin1", skip = 3) %>% 
  clean_names() %>% as.tibble(.)

# desp_tot_x2000_x2021 <- readxl::read_xlsx("dados/dados_brutos/desptot_2000_2021.xlsx") %>% 
#   clean_names()

desp_tot_2000_2021 <- desp_tot_x2000_x2021 %>% 
  select(-last_col()) %>% 
  slice(-(1:2), -n()) %>% 
  separate(
    munic_br, 
    into = c("cod_mun", "nome_mun"), 
    sep = " ", 
    extra = "merge"
  ) %>% 
  pivot_longer(
    cols = -c(cod_mun, nome_mun),
    names_to = "ano",
    values_to = "desp_tot"
  ) %>% 
  mutate(
    ano = str_sub(ano,2,5),
    across(c(ano, desp_tot), as.numeric), 
    cod_mun = str_sub(as.character(cod_mun), 1, 6),
    nome_mun = str_to_upper(nome_mun)
  ) %>% 
  relocate(ano) %>% 
  arrange(cod_mun)


# colocando a preços constantes de 2019

desp_tot <- deflate_num(desp_tot_2000_2021, 2019, "desp_tot_2019", "desp_tot")


# calculando a despesa per capita a preços constantes de 2019

desp_tot <- var_pc_pm(desp_tot, "desp_tot_2019")



# MÉDICOS ----------------------------------------------------------------------

# tratamento dos dados de médicos de 2005 a 2006

medico_x2005_x2006 <- read.csv2("dados/dados_brutos/medico_2005_2006.csv", fileEncoding = "latin1", skip = 4) %>% 
  clean_names() %>% 
  as.tibble()

# medico_x2005_x2006 <- readxl::read_xlsx("dados/dados_brutos/medico_2005_2006.xlsx") %>% 
#   clean_names()

medico_2005_2006 <- medico_x2005_x2006 %>% 
  slice(1:(n() - 2)) %>% 
  separate(
    municipio, 
    into = c("cod_mun", "nome_mun"), 
    sep = " ", 
    extra = "merge"
  ) %>% 
  rename_with(
    ~ gsub("/.*$", "", .), 
    starts_with("2")
  ) %>%
  pivot_longer(
    cols = -c(cod_mun, nome_mun),
    names_to = "ano",
    values_to = "medicos"
  ) %>% 
  mutate(ano = str_sub(ano,2,5),
    across(c(ano, medicos), as.numeric), 
    cod_mun = str_sub(as.character(cod_mun), 1, 6),
    nome_mun = str_to_upper(nome_mun)
  ) %>% 
  relocate(ano) %>% 
  arrange(cod_mun)


# tratamento dos dados de médicos de 2007 a 2020

medico_x2007_x2020 <- read.csv2("dados/dados_brutos/medico_2007_2020.csv", fileEncoding = "latin1", skip = 4) %>% 
  clean_names() %>% as.tibble(.)
 
# medico_x2007_x2020 <- readxl::read_xlsx("dados/dados_brutos/medico_2007_2020.xlsx") %>% 
#   clean_names()

medico_2007_2020 <- medico_x2007_x2020 %>% 
  slice(1:(n() - 10)) %>% 
  separate(
    municipio, 
    into = c("cod_mun", "nome_mun"), 
    sep = " ", 
    extra = "merge"
  ) %>% 
  rename_with(
    ~ gsub("/.*$", "", .), 
    starts_with("2")
  ) %>%
  pivot_longer(
    cols = -c(cod_mun, nome_mun),
    names_to = "ano",
    values_to = "medicos"
  ) %>% 
  mutate(
    ano = str_sub(ano,2,5),
    across(c(ano, medicos), as.numeric), 
    cod_mun = str_sub(as.character(cod_mun), 1, 6),
    nome_mun = str_to_upper(nome_mun)
  ) %>% 
  relocate(ano) %>% 
  arrange(cod_mun)


# juntando os dados de 2005 a 2020

medicos <- bind_rows(medico_2005_2006, medico_2007_2020) %>% 
  arrange(ano)


# calculando médicos por mil habitantes

medicos <- var_pc_pm(medicos, "medicos", por_mil = T)



# BENEF PL PRIV ----------------------------------------------------------------

# tratamento dos dados de beneficiários de plano privado de saúde de 2000 a 2020

benef_priv_x2000_x2020 <- read.csv2("dados/dados_brutos/plsaudebenef_2000_2020.csv", fileEncoding = "latin1", skip = 5) %>% 
  clean_names() %>% 
  as.tibble(.)

# benef_priv_x2000_x2020 <- readxl::read_xlsx("dados/dados_brutos/plsaudebenef_2000_2020.xlsx")

benef_priv_2000_2020 <- benef_priv_x2000_x2020 %>% 
  slice(1:(n() - 8)) %>% 
  separate(
    municipio, 
    into = c("cod_mun", "nome_mun"), 
    sep = " ", 
    extra = "merge"
  ) %>% 
  pivot_longer(
    cols = -c(cod_mun, nome_mun),
    names_to = "ano",
    values_to = "benef_plano_priv"
  ) %>% 
  mutate(
    ano = paste0("20", str_sub(ano,5,6)),
    across(c(ano, benef_plano_priv), as.numeric), 
    cod_mun = str_sub(as.character(cod_mun), 1, 6),
    nome_mun = str_to_upper(nome_mun)
  ) %>% 
  relocate(ano) %>% 
  filter(cod_mun != "110000")


# calculando beneficiários de plano privado per capita

benef_priv <- var_pc_pm(benef_priv_2000_2020, "benef_plano_priv")



# ASPS -------------------------------------------------------------------------

# tratamento dos dados de ASPS de 2000 a 2021

pct_asps_x2000_x2021 <- read.csv2("dados/dados_brutos/percASPS_2000_2021.csv", fileEncoding = "latin1", skip = 3) %>% 
  clean_names() %>% as.tibble(.)

#pct_asps_x2000_x2021 <- readxl::read_xlsx("dados/dados_brutos/percASPS_2000_2021.xlsx") %>% 
#  clean_names()

pct_asps_2000_2021 <- pct_asps_x2000_x2021 %>%
  slice(-(1:2), -n()) %>% 
  separate(
    munic_br, 
    into = c("cod_mun", "nome_mun"), 
    sep = " ", 
    extra = "merge"
  ) %>%
  mutate(
    across(starts_with("x"), function(x) {as.numeric(str_replace(x, ",","."))})
  ) %>% 
  pivot_longer(
    cols = -c(cod_mun, nome_mun),
    names_to = "ano",
    values_to = "pct_asps"
  ) %>% 
  mutate(
    ano = as.numeric(str_sub(ano,2,5)),
    pct_asps = as.numeric(pct_asps) / 100, 
    cod_mun = str_sub(as.character(cod_mun), 1, 6),
    nome_mun = str_to_upper(nome_mun)
  ) %>% 
  relocate(ano) %>% 
  arrange(cod_mun)



# TRANSF SUS -------------------------------------------------------------------

# tratamento dos dados de transferências do sus de 2000 a 2021

transf_sus_x2000_x2021 <- read.csv2("dados/dados_brutos/perctransfSUS_2000_2021.csv", fileEncoding = "latin1", skip = 3) %>% 
  clean_names() %>% as.tibble(.)

#transf_sus_x2000_x2021 <- readxl::read_xlsx("dados/dados_brutos/perctransfSUS_2000_2021.xlsx") %>% 
#  clean_names()

transf_sus_2000_2021 <- transf_sus_x2000_x2021 %>%
  select(-last_col()) %>% 
  separate(
    munic_br, 
    into = c("cod_mun", "nome_mun"), 
    sep = " ", 
    extra = "merge"
  ) %>%
  mutate(
    across(starts_with("x"), function(x) {as.numeric(str_replace(x, ",","."))})
  ) %>% 
  pivot_longer(
    cols = -c(cod_mun, nome_mun),
    names_to = "ano",
    values_to = "pct_transf_sus"
  ) %>% 
  mutate(
    ano = as.numeric(str_sub(ano,2,5)),
    pct_transf_sus = as.numeric(pct_transf_sus) / 100, 
    cod_mun = str_sub(as.character(cod_mun), 1, 6),
    nome_mun = str_to_upper(nome_mun)
  ) %>% 
  relocate(ano) %>% 
  arrange(cod_mun)



# BASE GERAL -------------------------------------------------------------------

# criando uma lista para armazenar todos os dataframes

lista_geral <- list()

variaveis <- c("pib", "urban", "analf", "agua", "esgoto", "bf", "cons_med",
               "visita", "cons_prof", "leitos", "desp_prop", "desp_tot",
               "medicos", "benef_priv", "pct_asps_2000_2021", "transf_sus_2000_2021", 
               "esf_1999_2006", "esf_2007_2020", "qt_pop", "pop_porte_mun")

lista_geral <- map(variaveis, ~ get(.x))


# usando reduce para aplicar o left_join iterativamente

base_geral <- reduce(lista_geral, left_join, by = c("ano", "cod_mun", "nome_mun")) %>%
  mutate(
    qt_cob_esf = coalesce(qt_cob_esf.x, qt_cob_esf.y),
    pct_cob_esf = coalesce(pct_cob_esf.x, pct_cob_esf.y), 
    grau_cob_esf = coalesce(grau_cob_esf.x, grau_cob_esf.y)
  ) %>% 
  select(-qt_cob_esf.x, -qt_cob_esf.y, -pct_cob_esf.x, -pct_cob_esf.y, 
         -grau_cob_esf.x, -grau_cob_esf.y) %>% 
  relocate(qt_cob_esf, pct_cob_esf, grau_cob_esf, qt_pop, porte_mun, .before = esf_implantada)

writexl::write_xlsx(base_geral, "dados/dados_tratados/base_geral.xlsx")


head(base_geral)

tail(base_geral)

dim(base_geral)

names(base_geral)

str(base_geral)



base_geral %>% 
  dfSummary(
    graph.col = T, 
    style = "grid", 
    graph.magnif = 0.75
  ) %>% 
  stview()


base_geral %>% 
  freq() %>% 
  stview()



base <- readxl::read_xlsx("dados/dados_tratados/base_geral.xlsx")


base %>% 
  view()

