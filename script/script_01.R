# PIB -------------------------------------

library(janitor)
library(tidyverse)

pib_2002_2009 <- readxl::read_xls("dados/dados_brutos/PIB_2002_2009.xls") %>% # importação do PIB de 2002 a 2009
  clean_names


pib_principal_01 <- pib_2002_2009 %>% # tratamento de dados da base principal de 2002 a 2009
  select(
    ano, 
    codigo_do_municipio, 
    nome_do_municipio, 
    produto_interno_bruto_a_precos_correntes_r_1_000, 
    produto_interno_bruto_per_capita_a_precos_correntes_r_1_00
  ) %>% 
  rename(
    cod_mun_ibge = codigo_do_municipio,
    nome_mun = nome_do_municipio,
    pib_nom = produto_interno_bruto_a_precos_correntes_r_1_000,
    pib_nom_pc = produto_interno_bruto_per_capita_a_precos_correntes_r_1_00
  )
  

pib_aux_01 <- pib_2002_2009 %>% # tratamento de dados da base auxiliar de 2002 a 2009
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
    cod_mun_ibge = codigo_do_municipio,
    nome_mun = nome_do_municipio,
    cod_gr_ibge = codigo_da_grande_regiao,
    nome_gr = nome_da_grande_regiao,
    cod_uf_ibge = codigo_da_unidade_da_federacao,
    nome_uf = nome_da_unidade_da_federacao
  )



pib_2010_2021 <- readxl::read_xlsx("dados/dados_brutos/PIB_2010_2021.xlsx") %>% # importação do PIB de 2010 a 2021
  clean_names()


pib_principal_02 <- pib_2010_2021 %>% # tratamento de dados da base principal de 2010 a 2021
  select(
    ano, 
    codigo_do_municipio, 
    nome_do_municipio, 
    produto_interno_bruto_a_precos_correntes_r_1_000, 
    produto_interno_bruto_per_capita_a_precos_correntes_r_1_00
  ) %>% 
  rename(
    cod_mun_ibge = codigo_do_municipio,
    nome_mun = nome_do_municipio,
    pib_nom = produto_interno_bruto_a_precos_correntes_r_1_000,
    pib_nom_pc = produto_interno_bruto_per_capita_a_precos_correntes_r_1_00
  )


pib_aux_02 <- pib_2010_2021 %>% # tratamento de dados da base auxiliar de 2010 a 2021
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
    cod_mun_ibge = codigo_do_municipio,
    nome_mun = nome_do_municipio,
    cod_gr_ibge = codigo_da_grande_regiao,
    nome_gr = nome_da_grande_regiao,
    cod_uf_ibge = codigo_da_unidade_da_federacao,
    nome_uf = nome_da_unidade_da_federacao
  )


pib_principal <- bind_rows(pib_principal_01, pib_principal_02) # juntando as bases principais do PIB

pib_aux <- bind_rows(pib_aux_01, pib_aux_02) # juntando as bases auxiliares do PIB


# URBANIZAÇÃO ----------------------------------------------

urbaniz_2022 <- readxl::read_xlsx("dados/dados_brutos/urban_2022.xlsx") %>% # importação da base de 2022
  clean_names() 

urban_2022 <- urbaniz_2022 %>% # tratamento dos dados
  select(x2, x3, x6) %>% 
  rename(
    cod_mun_ibge = x2,
    nome_mun = x3,
    perc_urb = x6
  ) %>% 
  slice(-c(1:4)) %>%
  slice(-n()) %>% 
  mutate(
    ano = 2022,
    perc_urb = as.numeric(perc_urb),
    nome_mun = str_remove(nome_mun, "\\(.*\\)"),
    nome_mun = str_trim(nome_mun)
  ) %>% 
  relocate(ano)


urbaniz_1991 <- readxl::read_xlsx("dados/dados_brutos/urban_1991_2000_2010.xlsx") %>% # importação da base de 1991
  clean_names()

urban_1991 <- urbaniz_1991 %>% # tratamento dos dados
  select(x2, x3, x6, x12, x18) %>% 
  rename(
    cod_mun_ibge = x2,
    nome_mun = x3,
    "1991" = x6,
    "2000" = x12,
    "2010" = x18
  ) %>% 
  slice(-c(1:5)) %>%
  slice(-n()) %>%
  pivot_longer(
    cols = c("1991", "2000", "2010"),
    names_to = "ano",
    values_to = "perc_urb"
  ) %>% 
  mutate(
    nome_mun = str_remove(nome_mun, "\\(.*\\)"),
    nome_mun = str_trim(nome_mun),
    ano = as.numeric(ano),
    perc_urb = as.numeric(perc_urb)
  ) %>% 
  relocate(ano) %>% 
  arrange(ano)
  

urb_1991_2022 <- bind_rows(urban_1991, urban_2022) # juntando as duas bases de urbanização



dim(urban_1991)
dim(urban_2022)

diff_1991_not_in_2022 <- anti_join(urban_1991, urban_2022, by = c("cod_mun_ibge", "nome_mun"))
diff_2022_not_in_1991 <- anti_join(urban_2022, urban_1991, by = c("cod_mun_ibge", "nome_mun"))

