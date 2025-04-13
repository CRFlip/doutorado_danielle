# PACOTES ----------------------------------------------------------------------

library(janitor)
library(tidyverse)
library(writexl)



# IPCA -------------------------------------------------------------------------

# dados do IPCA de 2000 a 2023

ipca_2000_2023 <- tibble::tibble(
  ano = 2000:2023,
  ipca = c(5.97, 7.67, 12.53, 9.30, 7.60, 5.69, 3.14, 4.46, 5.90, 4.31,
           5.91, 6.50, 5.84, 5.91, 6.41, 10.67, 6.29, 2.95, 3.75, 4.31,
           4.52, 10.06, 5.79, 4.62))


# calculando o deflator do IPCA com base em 2020

ipca_deflator_2020 <- ipca_2000_2023 %>%
  mutate(deflator_2020 = ipca[ano == 2020] / ipca)



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

pib_2002_2021 <- bind_rows(pib_2002_2009, pib_2010_2021)


# colocando a preços constantes de 2020

pib_const_2020 <- pib_2002_2021 %>%
  left_join(ipca_deflator_2020, by = "ano") %>%
  mutate(pib_nom_2020 = pib_nom * deflator_2020) %>% 
  select(-ipca, -deflator_2020)


# calculando o PIB per capita a preços constantes de 2020, de 2002 a 2021

pib_2002_2021_pc_2020 <- pib_const_2020 %>% 
  left_join(qt_pop, by = c("cod_mun", "ano")) %>% 
  mutate(
    pib_nom_pc = pib_nom_2020 / qt_pop
  ) %>% 
  rename(nome_mun = nome_mun.x) %>% 
  select(-nome_mun.y) %>% 
  filter(!is.na(qt_pop)) %>% 
  arrange(cod_mun)


# juntando os dados auxiliares do PIB de 2002 a 2021

ref_cod_mun_aux <- bind_rows(pib_aux_01, pib_aux_02)

ref_cod_mun_aux_2021 <- ref_cod_mun_aux %>% 
  filter(ano == max(ano))


# carregamento dos dados para o Excel

writexl::write_xlsx(pib_2002_2021, "dados/dados_tratados/pib_2002_2021.xlsx")

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

urban_int <- urban_1991_2022 %>%
  complete(cod_mun, ano = full_seq(1991:2022, 1)) %>%
  group_by(cod_mun) %>%
  arrange(ano) %>%
  mutate(
    nome_mun = first(na.omit(nome_mun)),
    tx_urban = if (sum(!is.na(tx_urban)) >= 2) {
      approx(x = ano, y = tx_urban, xout = ano, rule = 2)$y
    } else {
      tx_urban
    }
  ) %>%
  ungroup()


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

analf_int <- analf_1991_2022 %>%
  complete(cod_mun, ano = full_seq(1991:2022, 1)) %>%
  group_by(cod_mun) %>%
  arrange(ano) %>%
  mutate(
    nome_mun = first(na.omit(nome_mun)),
    tx_analf = if (sum(!is.na(tx_analf)) >= 2) {
      approx(x = ano, y = tx_analf, xout = ano, rule = 2)$y
    } else {
      tx_analf
    }
  ) %>%
  ungroup()


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

agua_int <- agua_2000_2010_2022 %>%
  complete(cod_mun, ano = full_seq(2000:2022, 1)) %>%
  group_by(cod_mun) %>%
  arrange(ano) %>%
  mutate(
    nome_mun = first(na.omit(nome_mun)),
    agua = if (sum(!is.na(agua)) >= 2) {
      approx(x = ano, y = agua, xout = ano, rule = 2)$y
    } else {
      agua
    }
  ) %>%
  ungroup()


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

esgoto_int <- esgoto_2000_2010_2022 %>%
  complete(cod_mun, ano = full_seq(2000:2022, 1)) %>%
  group_by(cod_mun) %>%
  arrange(ano) %>%
  mutate(
    nome_mun = first(na.omit(nome_mun)),
    esgoto = if (sum(!is.na(esgoto)) >= 2) {
      approx(x = ano, y = esgoto, xout = ano, rule = 2)$y
    } else {
      esgoto
    }
  ) %>%
  ungroup()



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


# calculando bf a preços constantes de 2020

bf_const_2020 <- bf_2004_2023 %>%
  left_join(ipca_deflator_2020, by = "ano") %>%
  mutate(bf_2020 = bf * deflator_2020) %>% 
  select(-ipca, -deflator_2020)


# calculando bf per capita a preços constantes de 2020

bf_2004_2023_pc_2020 <- bf_const_2020 %>% 
  left_join(qt_pop, by = c("cod_mun", "ano")) %>% 
  mutate(
    bf_pc_2020 = (bf_2020 * 1000) / qt_pop
  ) %>% 
  rename(nome_mun = nome_mun.x) %>% 
  select(-nome_mun.y) %>% 
  filter(!is.na(qt_pop)) %>% 
  arrange(cod_mun)



# CONS MED ------------------------------------------------------------------

# tratamento dos dados de consulta médica de 2008 a 2020

con_med_x2008_x2020 <- readxl::read_xlsx("dados/dados_brutos/consultamedico_2008_2020.xlsx") %>% 
  clean_names()

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


# calculando consultas médicas por mil habitantes

con_med_2008_2020 <- con_med_2008_2020 %>% 
  left_join(qt_pop, by = c("cod_mun", "ano")) %>% 
  mutate(
    consulta_med_por_mil = (consulta_med * 1000) / qt_pop
  ) %>% 
  filter(!is.na(qt_pop)) %>% 
  arrange(cod_mun)



# VISITAS ----------------------------------------------------------------------

# tratamento dos dados de visitas de 2008 a 2020

visita_x2008_x2020 <- readxl::read_xlsx("dados/dados_brutos/visita_2008_2020.xlsx")

visita_2008_2020 <- visita_x2008_x2020 %>% 
  separate(
    Município, 
    into = c("cod_mun", "nome_mun"), 
    sep = " ", 
    extra = "merge"
  ) %>% 
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
  arrange(cod_mun) %>% 
  left_join(qt_pop, by = c("cod_mun", "ano")) %>% 
  mutate(
    visitas_por_mil = (visitas * 1000) / qt_pop
  ) %>% 
  filter(!is.na(qt_pop)) %>% 
  arrange(cod_mun)


# calculando visitas por mil habitantes

visita_2008_2020 <- visita_2008_2020 %>% 
  left_join(qt_pop, by = c("cod_mun", "ano")) %>% 
  mutate(
    visitas_por_mil = (visitas * 1000) / qt_pop
  ) %>% 
  filter(!is.na(qt_pop)) %>% 
  arrange(cod_mun)



# CONS PROF -----------------------------------------------------------------

# tratamento dos dados de consultas profissionais de 2008 a 2020

con_prof_x2008_x2020 <- readxl::read_xlsx("dados/dados_brutos/consultaprofsup_2008_2020.xlsx") %>% 
  clean_names()

cons_prof_2008_2020 <- con_prof_x2008_x2020 %>%
  select(-x15) %>% 
  slice(-(1:4)) %>% 
  slice(1:(n() - 12)) %>% 
  row_to_names(row_number = 1) %>% 
  separate(
    Município, 
    into = c("cod_mun", "nome_mun"), 
    sep = " ", 
    extra = "merge"
  ) %>% 
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


# calculando consultas profissionais por mil habitantes

cons_prof_2008_2020 <- cons_prof_2008_2020 %>% 
  left_join(qt_pop, by = c("cod_mun", "ano")) %>% 
  mutate(
    consulta_prof_sup_por_mil = (consulta_prof_sup * 1000) / qt_pop
  ) %>% 
  filter(!is.na(qt_pop)) %>% 
  arrange(cod_mun)



# LEITOS -----------------------------------------------------------------------

# tratamento dos dados de leitos de 2005 a 2020

leitos_x2005_x2020 <- readxl::read_xlsx("dados/dados_brutos/leitos_2005_2020.xlsx") %>% 
  clean_names()

leitos_2005_2020 <- leitos_x2005_x2020 %>% 
  slice(-(1:2)) %>% 
  slice(1:(n() - 13)) %>% 
  row_to_names(row_number = 1) %>% 
  separate(
    Município, 
    into = c("cod_mun", "nome_mun"), 
    sep = " ", 
    extra = "merge"
  ) %>% 
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


# calculando leitos por mil habitantes

leitos_2005_2020 <- leitos_2005_2020 %>%
  left_join(qt_pop, by = c("cod_mun", "ano")) %>% 
  mutate(
    leitos_por_mil = (leitos * 1000) / qt_pop
  ) %>% 
  filter(!is.na(qt_pop))



# DESP PROP ----------------------------------------------------------------

# tratamento dos dados de despesas com recursos próprios de 2000 a 2021

desp_prop_x2000_x2021 <- readxl::read_xlsx("dados/dados_brutos/desppropria_2000_2021.xlsx") %>% 
  clean_names()

desp_prop_2000_2021 <- desp_prop_x2000_x2021 %>% 
  select(-last_col()) %>% 
  slice(-(1:2), -n()) %>% 
  row_to_names(row_number = 1) %>% 
  separate(
    `Munic-BR`, 
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
    across(c(ano, desp_prop), as.numeric), 
    cod_mun = str_sub(as.character(cod_mun), 1, 6),
    nome_mun = str_to_upper(nome_mun)
  ) %>% 
  relocate(ano) %>% 
  arrange(cod_mun) 


# colocando a preços constantes de 2020

desp_prop_const_2020 <- desp_prop_2000_2021 %>%
  left_join(ipca_deflator_2020, by = "ano") %>%
  mutate(desp_prop_2020 = desp_prop * deflator_2020) %>% 
  select(-ipca, - deflator_2020)


# calculando a despesa per capita a preços constantes de 2020

desp_prop_const_2020_pc <- desp_prop_const_2020 %>% 
  left_join(qt_pop, by = c("cod_mun", "ano")) %>% 
  mutate(
    desp_prop_pc_2020 = desp_prop_2020 / qt_pop
  ) %>% 
  rename(nome_mun = nome_mun.x) %>% 
  select(-nome_mun.y) %>% 
  filter(!is.na(qt_pop)) %>% 
  arrange(cod_mun)




# DESP SAÚDE ----------------------------------------------------------------

# tratamento dos dados de despesa total em saúde de 2000 a 2021

desp_tot_x2000_x2021 <- readxl::read_xlsx("dados/dados_brutos/desptot_2000_2021.xlsx") %>% 
  clean_names()

desp_tot_2000_2021 <- desp_tot_x2000_x2021 %>% 
  select(-last_col()) %>% 
  slice(-(1:2), -n()) %>% 
  row_to_names(row_number = 1) %>% 
  separate(
    `Munic-BR`, 
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
    across(c(ano, desp_tot), as.numeric), 
    cod_mun = str_sub(as.character(cod_mun), 1, 6),
    nome_mun = str_to_upper(nome_mun)
  ) %>% 
  relocate(ano) %>% 
  arrange(cod_mun)


# colocando a preços constantes de 2020

desp_tot_const_2020 <- desp_tot_2000_2021 %>%
  left_join(ipca_deflator_2020, by = "ano") %>%
  mutate(desp_tot_2020 = desp_tot * deflator_2020) %>% 
  select(-ipca, - deflator_2020)


# calculando a despesa per capita a preços constantes de 2020

desp_tot_const_2020_pc <- desp_tot_const_2020 %>% 
  left_join(qt_pop, by = c("cod_mun", "ano")) %>% 
  mutate(
    desp_tot_pc_2020 = desp_tot_2020 / qt_pop
  ) %>% 
  rename(nome_mun = nome_mun.x) %>% 
  select(-nome_mun.y) %>% 
  filter(!is.na(qt_pop)) %>% 
  arrange(cod_mun)



# MÉDICOS ----------------------------------------------------------------------

# tratamento dos dados de médicos de 2005 a 2006

medico_x2005_x2006 <- readxl::read_xlsx("dados/dados_brutos/medico_2005_2006.xlsx") %>% 
  clean_names()

medico_2005_2006 <- medico_x2005_x2006 %>% 
  slice(-(1:3)) %>% 
  slice(1:(n() - 2)) %>% 
  row_to_names(row_number = 1) %>% 
  separate(
    Município, 
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
    across(c(ano, medicos), as.numeric), 
    cod_mun = str_sub(as.character(cod_mun), 1, 6),
    nome_mun = str_to_upper(nome_mun)
  ) %>% 
  relocate(ano) %>% 
  arrange(cod_mun)


# tratamento dos dados de médicos de 2007 a 2020

medico_x2007_x2020 <- readxl::read_xlsx("dados/dados_brutos/medico_2007_2020.xlsx") %>% 
  clean_names()

medico_2007_2020 <- medico_x2007_x2020 %>% 
  slice(-(1:3)) %>% 
  slice(1:(n() - 10)) %>% 
  row_to_names(row_number = 1) %>% 
  separate(
    Município, 
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

medicos <- medicos %>%
  left_join(qt_pop, by = c("cod_mun", "ano")) %>% 
  mutate(
    medicos_por_mil = (medicos * 1000) / qt_pop
  ) %>% 
  rename(nome_mun = nome_mun.x) %>% 
  select(-nome_mun.y) %>%
  filter(!is.na(qt_pop)) %>% 
  arrange(cod_mun)



# BENEF PL PRIV ----------------------------------------------------------------

# tratamento dos dados de beneficiários de plano privado de saúde de 2000 a 2020

benef_priv_x2000_x2020 <- readxl::read_xlsx("dados/dados_brutos/plsaudebenef_2000_2020.xlsx")

benef_priv_2000_2020 <- benef_priv_x2000_x2020 %>% 
  slice(-(1:4)) %>% 
  slice(1:(n() - 8)) %>% 
  row_to_names(row_number = 1) %>% 
  select(
    1, 
    rev(2:ncol(.))
  ) %>% 
  separate(
    Município, 
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
    across(c(ano, benef_plano_priv), as.numeric), 
    cod_mun = str_sub(as.character(cod_mun), 1, 6),
    nome_mun = str_to_upper(nome_mun)
  ) %>% 
  relocate(ano) %>% 
  filter(cod_mun != "110000")


# calculando beneficiários de plano privado per capita

benef_priv_2000_2020 <- benef_priv_2000_2020 %>% 
  left_join(qt_pop, by = c("cod_mun", "ano")) %>% 
  mutate(
    cob_plano_priv = benef_plano_priv / qt_pop
  ) %>% 
  rename(nome_mun = nome_mun.x) %>% 
  select(-nome_mun.y) %>%
  filter(!is.na(qt_pop)) %>% 
  arrange(cod_mun)



# ASPS -------------------------------------------------------------------------

# tratamento dos dados de ASPS de 2000 a 2021

pct_asps_x2000_x2021 <- readxl::read_xlsx("dados/dados_brutos/percASPS_2000_2021.xlsx") %>% 
  clean_names()

pct_asps_2000_2021 <- pct_asps_x2000_x2021 %>%
  select(-last_col()) %>% 
  slice(-(1:2), -n()) %>% 
  row_to_names(row_number = 1) %>% 
  separate(
    `Munic-BR`, 
    into = c("cod_mun", "nome_mun"), 
    sep = " ", 
    extra = "merge"
  ) %>%
  mutate(
    across(`2000`:`2021`, as.numeric)
  ) %>% 
  pivot_longer(
    cols = -c(cod_mun, nome_mun),
    names_to = "ano",
    values_to = "pct_asps"
  ) %>% 
  mutate(
    ano = as.numeric(ano),
    pct_asps = as.numeric(pct_asps) / 100, 
    cod_mun = str_sub(as.character(cod_mun), 1, 6),
    nome_mun = str_to_upper(nome_mun)
  ) %>% 
  relocate(ano) %>% 
  arrange(cod_mun)



# TRANSF SUS -------------------------------------------------------------------

# tratamento dos dados de transferências do sus de 2000 a 2021

transf_sus_x2000_x2021 <- readxl::read_xlsx("dados/dados_brutos/perctransfSUS_2000_2021.xlsx") %>% 
  clean_names()

transf_sus_2000_2021 <- transf_sus_x2000_x2021 %>%
  select(-last_col()) %>% 
  slice(-(1:2), -n()) %>% 
  row_to_names(row_number = 1) %>% 
  separate(
    `Munic-BR`, 
    into = c("cod_mun", "nome_mun"), 
    sep = " ", 
    extra = "merge"
  ) %>%
  mutate(
    across(`2000`:`2021`, as.numeric)
  ) %>% 
  pivot_longer(
    cols = -c(cod_mun, nome_mun),
    names_to = "ano",
    values_to = "pct_transf_sus"
  ) %>% 
  mutate(
    ano = as.numeric(ano),
    pct_transf_sus = as.numeric(pct_transf_sus) / 100, 
    cod_mun = str_sub(as.character(cod_mun), 1, 6),
    nome_mun = str_to_upper(nome_mun)
  ) %>% 
  relocate(ano) %>% 
  arrange(cod_mun)



# ESF --------------------------------------------------------------------------

# tratamento dos dados de esf de 1999 a 2006

esf_x1999_x2006 <- readxl::read_xlsx("dados/dados_brutos/ESF 1999 a 2006.xlsx") %>% 
  clean_names()

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
    grau_cob_esf = case_when(
      pct_cob_esf == 0 ~ 0,
      pct_cob_esf < 30 ~ 1,
      pct_cob_esf >= 30 & pct_cob_esf < 70 ~ 2,
      pct_cob_esf >= 70 ~ 3), 
    cod_mun = str_sub(as.character(cod_mun), 1, 6),
    nome_mun = str_to_upper(nome_mun)
    ) %>% 
  relocate(
    ano, cod_mun, nome_mun, qt_cob_esf, qt_pop, pct_cob_esf, grau_cob_esf
  ) %>% 
  arrange(cod_mun)


# calculando o grau de cobertura de esf de 1999 a 2006

esf_grau <- esf_1999_2006 %>%
  arrange(cod_mun, ano) %>%
  mutate(
    pct_cob_esf = (qt_cob_esf / qt_pop) * 100,
    grau_cob_esf = case_when(
      pct_cob_esf == 0 ~ 0,
      pct_cob_esf < 30 ~ 1,
      pct_cob_esf >= 30 & pct_cob_esf < 70 ~ 2,
      pct_cob_esf >= 70 ~ 2
    )
  )

esf_grau <- esf_grau %>%
  group_by(cod_mun) %>%
  mutate(
    cobertura_70 = pct_cob_esf >= 70,
    grau_cob_esf = {
      grau_temp <- grau_cob_esf
      n_linhas <- n()
      if (n_linhas >= 4) {
        for (i in 4:n_linhas) {
          if (all(cobertura_70[(i - 3):i])) {
            grau_temp[i:n_linhas] <- 3
            break
          }
        }
      }
      grau_temp
    }
  ) %>%
  ungroup()


# tratamento dos dados de esf de 2007 a 2020

# lendo as abas do Excel, na qual cada uma contém o dado de um ano 

abas_esf_x2007_x2020 <- readxl::excel_sheets("dados/dados_brutos/ESF-2007-202012.xlsx")

dados_esf_x2007_x2020 <- map(abas_esf_x2007_x2020, function(x)
  {readxl::read_xlsx("dados/dados_brutos/ESF-2007-202012.xlsx", sheet = x)})


# removendo a última aba, que não contém dados

dados_esf_x2007_x2020 <- dados_esf_x2007_x2020[-15]


# juntando os dados de esf em base única

base_esf_x2007_x2020 <- list_rbind(dados_esf_x2007_x2020)


# tratando os dados de esf de 2007 a 2020

base_esf_2007_2020 <- base_esf_x2007_x2020 %>% 
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
    qt_pop = clear_num(qt_pop),
    qt_cob_esf = clear_num(qt_cob_esf),
    pct_cob_esf = (qt_cob_esf / qt_pop) * 100, 
    grau_cob_esf = case_when(
      pct_cob_esf == 0 ~ 0,
      pct_cob_esf < 30 ~ 1,
      pct_cob_esf >= 30 & pct_cob_esf < 70 ~ 2,
      pct_cob_esf >= 70 ~ 3
    )) %>% 
  relocate(ano, cod_mun, nome_mun, qt_cob_esf, qt_pop, pct_cob_esf, grau_cob_esf)


# criando uma função para limpar o número e transformar em numérico

clear_num <- function(x) {
  x %>%
    stringr::str_replace_all("[^0-9,\\.]", "") %>%
    stringr::str_replace_all("\\.", "") %>%
    stringr::str_replace(",", ".") %>%
    as.numeric()
}



# POP --------------------------------------------------------------------------

# tratamento dos dados de população de 2000 a 2020

qt_pop_2000_2006 <- esf_1999_2006 %>% 
  filter(mes == "12", ano >= "2000" & ano <= "2006") %>% 
  select(ano, cod_mun, nome_mun, qt_pop) %>% 
  mutate(
    ano = as.numeric(ano)
  )

qt_pop_2007_2020 <- base_esf_2007_2020 %>% 
  filter(str_sub(nu_competencia, 5, 6) == "12") %>% 
  select(ano, cod_mun, nome_mun, qt_pop) %>% 
  mutate(
    ano = as.numeric(ano)
  )


# juntando os dados de população

qt_pop <- bind_rows(qt_pop_2000_2006, qt_pop_2007_2020) %>% 
  arrange(cod_mun)


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
    )










