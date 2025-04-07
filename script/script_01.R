# PACOTES ----------------------------------

library(janitor)
library(tidyverse)
library(writexl)


# PIB -------------------------------------

pib_2002_2009 <- readxl::read_xls("dados/dados_brutos/PIB_2002_2009.xls") %>% # importação do PIB de 2002 a 2009
  clean_names()


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


# juntando as bases auxiliares

ref_cod_mun_aux <- bind_rows(pib_aux_01, pib_aux_02)

ref_cod_mun_aux_2021 <- ref_cod_mun_aux %>% 
  filter(ano == max(ano))


writexl::write_xlsx(pib_principal, "dados/dados_tratados/pib_tratado.xlsx")

writexl::write_xlsx(ref_cod_mun_aux_2021, "dados/dados_tratados/ref_cod_mun_aux_tratado.xlsx")



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



# ANALFABETISMO ------------------------------------------

analfab_1991_2022 <- readxl::read_xls("dados/dados_brutos/analfab_1991_2022.xls") %>% 
  clean_names()

analfab <- analfab_1991_2022 %>% 
  select(-sigla) %>%
  rename(
    cod_mun_ibge = codigo,
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
    ano = as.numeric(ano)
  ) %>% 
  relocate(ano) %>% 
  arrange(ano)
  


# ÁGUA ------------------------------------------

# carregamento e tratamento dos dados de ÁGUA 2000

agua_x2000 <- readxl::read_xlsx("dados/dados_brutos/agua_2000.xlsx") %>% 
  clean_names()

agua_2000 <- agua_x2000 %>% 
  select(x2, x3, x5) %>% 
  rename(
    cod_mun_ibge = x2,
    nome_mun = x3,
    agua = x5
  ) %>% 
  slice(-c(1:4)) %>% 
  slice(-n()) %>% 
  mutate(
    nome_mun = str_remove(nome_mun, "\\(.*\\)"),
    nome_mun = str_trim(nome_mun),
    ano = 2000,
    agua = as.numeric(agua) / 100
  ) %>% 
  relocate(ano)


# carregamento e tratamento de dados de ÁGUA 2010

agua_x2010 <- readxl::read_xlsx("dados/dados_brutos/agua_2010.xlsx") %>% 
  clean_names() %>% 
  view()

agua_2010 <- agua_x2010 %>% 
  select(x2, x3, x6) %>% 
  rename(
    cod_mun_ibge = x2,
    nome_mun = x3,
    agua = x6
  ) %>% 
  slice(-c(1:5)) %>% 
  slice(-n()) %>% 
  mutate(
    nome_mun = str_remove(nome_mun, "\\(.*\\)"),
    nome_mun = str_trim(nome_mun),
    ano = 2010,
    agua = as.numeric(agua) / 100
  ) %>% 
  relocate(ano)


# carregamento e tratamento de dados de ÁGUA 2022

agua_x2022 <- readxl::read_xlsx("dados/dados_brutos/agua_2022.xlsx") %>% 
  clean_names() %>% 
  view()

agua_2022 <- agua_x2022 %>% 
  select(x2, x3, x5, x7) %>% 
  rename(
    cod_mun_ibge = x2,
    nome_mun = x3,
    agua_01 = x5,
    agua_02 = x7
  ) %>% 
  slice(-c(1:6)) %>% 
  slice(-n()) %>% 
  mutate(
    nome_mun = str_remove(nome_mun, "\\(.*\\)") %>% str_trim(),
    ano = 2022,
    across(c(agua_01, agua_02), as.numeric),
    agua = (agua_01 + agua_02) / 100
  ) %>% 
  relocate(ano) %>% 
  select(-c(agua_01, agua_02))


# juntando as bases de água

agua_principal <- bind_rows(agua_2000, agua_2010, agua_2022)



# ESGOTO -----------------------------------------------

# carregamento e tratamento dos dados de ESGOTO 2000

esgoto_x2000 <- readxl::read_xlsx("dados/dados_brutos/esgoto_2000.xlsx") %>% 
  clean_names() %>% 
  view()

esgoto_2000 <- esgoto_x2000 %>% 
  select(x2, x3, x5) %>% 
  rename(
    cod_mun_ibge = x2,
    nome_mun = x3,
    esgoto = x5
  ) %>% 
  slice(-c(1:4)) %>% 
  slice(-n()) %>% 
  mutate(
    nome_mun = str_remove(nome_mun, "\\(.*\\)") %>% str_trim(),
    ano = 2000,
    esgoto = as.numeric(esgoto) / 100
  ) %>% 
  relocate(ano)


# carregamento e tratamento dos dados de ESGOTO 2010

esgoto_x2010 <- readxl::read_xlsx("dados/dados_brutos/esgoto_2010.xlsx") %>% 
  clean_names() %>% 
  view()

esgoto_2010 <- esgoto_x2010 %>% 
  select(x2, x3, x4, x6) %>% 
  rename(
    cod_mun_ibge = x2,
    nome_mun = x3,
    esgoto_01 = x4,
    esgoto_02 = x6
  ) %>% 
  slice(-(1:8), -n()) %>%
  mutate(
    nome_mun = str_remove(nome_mun, "\\(.*\\)") %>% str_trim(),
    ano = 2010,
    across(c(esgoto_01, esgoto_02), as.numeric),
    esgoto = (esgoto_01 + esgoto_02) / 100
  ) %>% 
  relocate(ano) %>% 
  select(-c(esgoto_01, esgoto_02))


# carregamento e tratamento dos dados de ESGOTO 2022

esgoto_x2022 <- readxl::read_xlsx("dados/dados_brutos/esgoto_2022.xlsx") %>% 
  clean_names() %>% 
  view()

esgoto_2022 <- esgoto_x2022 %>% 
  select(x2, x3, x5) %>% 
  rename(
    cod_mun_ibge = x2,
    nome_mun = x3,
    esgoto = x5
  ) %>% 
  slice(-(1:8), -n()) %>%
  mutate(
    nome_mun = str_remove(nome_mun, "\\(.*\\)") %>% str_trim(),
    ano = 2022,
    esgoto = as.numeric(esgoto) / 100
  ) %>% 
  relocate(ano)


# BOLSA FAMILIA ----------------------------------

bf_2004_2023 <- readxl::read_xls("dados/dados_brutos/BF_2004_2023.xls") %>%
  view()

bols_fam <- bf_2004_2023 %>% 
  select(-Sigla) %>% 
  rename(
    cod_mun_ibge = Codigo,
    nome_mun = Município
  ) %>% 
  pivot_longer(
    cols = -(1:2),
    names_to = "ano",
    values_to = "bf"
  ) %>% 
  relocate(ano)


# CONSULTA MÉDICA -------------------------------------

con_med_x2008_x2020 <- readxl::read_xlsx("dados/dados_brutos/consultamedico_2008_2020.xlsx") %>% 
  clean_names()

con_med_2008_2020 <- con_med_x2008_x2020 %>%
  select(-last_col()) %>% 
  separate(
    municipio, 
    into = c("cod_mun_ibge", "nome_mun"), 
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
    consulta_med = as.numeric(consulta_med)
  ) %>% 
  relocate(ano)


# VISITAS ---------------------------------------

visita_x2008_x2020 <- readxl::read_xlsx("dados/dados_brutos/visita_2008_2020.xlsx")

visita_2008_2020 <- visita_x2008_x2020 %>% 
  separate(
    Município, 
    into = c("cod_mun_ibge", "nome_mun"), 
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
    visitas = as.numeric(visitas)
  ) %>% 
  relocate(ano)


# CONSULTAS PROFISSIONAL -----------------------------------------

con_prof_x2008_x2020 <- readxl::read_xlsx("dados/dados_brutos/consultaprofsup_2008_2020.xlsx") %>% 
  clean_names()

cons_prof_sup_2008_2020 <- con_prof_x2008_x2020 %>%
  select(-x15) %>% 
  slice(-(1:4)) %>% 
  slice(1:(n() - 12)) %>% 
  row_to_names(row_number = 1) %>% 
  separate(
    Município, 
    into = c("cod_mun_ibge", "nome_mun"), 
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
    across(c(ano, consulta_prof_sup), as.numeric)
  ) %>% 
  relocate(ano)


# LEITOS -----------------------------------------

leitos_x2005_x2020 <- readxl::read_xlsx("dados/dados_brutos/leitos_2005_2020.xlsx") %>% 
  clean_names() %>% 
  view()

leitos_2005_2020 <- leitos_x2005_x2020 %>% 
  slice(-(1:2)) %>% 
  slice(1:(n() - 13)) %>% 
  row_to_names(row_number = 1) %>% 
  separate(
    Município, 
    into = c("cod_mun_ibge", "nome_mun"), 
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
    across(c(ano, leitos), as.numeric)
  ) %>% 
  relocate(ano)


# DESPESAS PRÓPRIAS ------------------------------------

desp_prop_x2000_x2021 <- readxl::read_xlsx("dados/dados_brutos/desppropria_2000_2021.xlsx") %>% 
  clean_names() %>% 
  view()

desp_prop <- desp_prop_x2000_x2021 %>% 
  select(-last_col()) %>% 
  slice(-(1:2), -n()) %>% 
  row_to_names(row_number = 1) %>% 
  separate(
    `Munic-BR`, 
    into = c("cod_mun_ibge", "nome_mun"), 
    sep = " ", 
    extra = "merge"
  ) %>% 
  pivot_longer(
    cols = -c(cod_mun_ibge, nome_mun),
    names_to = "ano",
    values_to = "desp_prop"
  ) %>% 
  mutate(
    across(c(ano, desp_prop), as.numeric)
  ) %>% 
  relocate(ano)


# DESPESAS EM SAÚDE ---------------------------------

desp_tot_x2000_x2021 <- readxl::read_xlsx("dados/dados_brutos/desptot_2000_2021.xlsx") %>% 
  clean_names() %>% 
  view()

desp_tot <- desp_tot_x2000_x2021 %>% 
  select(-last_col()) %>% 
  slice(-(1:2), -n()) %>% 
  row_to_names(row_number = 1) %>% 
  separate(
    `Munic-BR`, 
    into = c("cod_mun_ibge", "nome_mun"), 
    sep = " ", 
    extra = "merge"
  ) %>% 
  pivot_longer(
    cols = -c(cod_mun_ibge, nome_mun),
    names_to = "ano",
    values_to = "desp_tot"
  ) %>% 
  mutate(
    across(c(ano, desp_tot), as.numeric)
  ) %>% 
  relocate(ano)


# MÉDICOS -------------------------------------

# dados de 2005 a 2006

medico_x2005_x2006 <- readxl::read_xlsx("dados/dados_brutos/medico_2005_2006.xlsx") %>% 
  clean_names() %>% 
  view()

medico_2005_2006 <- medico_x2005_x2006 %>% 
  slice(-(1:3)) %>% 
  slice(1:(n() - 2)) %>% 
  row_to_names(row_number = 1) %>% 
  separate(
    Município, 
    into = c("cod_mun_ibge", "nome_mun"), 
    sep = " ", 
    extra = "merge"
  ) %>% 
  rename_with(
    ~ gsub("/.*$", "", .), 
    starts_with("2")
  ) %>%
  pivot_longer(
    cols = -c(cod_mun_ibge, nome_mun),
    names_to = "ano",
    values_to = "medicos"
  ) %>% 
  mutate(
    across(c(ano, medicos), as.numeric)
  ) %>% 
  relocate(ano)


# dados de 2007 a 2020

medico_x2007_x2020 <- readxl::read_xlsx("dados/dados_brutos/medico_2007_2020.xlsx") %>% 
  clean_names() %>% 
  view()

medico_2007_2020 <- medico_x2007_x2020 %>% 
  slice(-(1:3)) %>% 
  slice(1:(n() - 10)) %>% 
  row_to_names(row_number = 1) %>% 
  separate(
    Município, 
    into = c("cod_mun_ibge", "nome_mun"), 
    sep = " ", 
    extra = "merge"
  ) %>% 
  rename_with(
    ~ gsub("/.*$", "", .), 
    starts_with("2")
  ) %>%
  pivot_longer(
    cols = -c(cod_mun_ibge, nome_mun),
    names_to = "ano",
    values_to = "medicos"
  ) %>% 
  mutate(
    across(c(ano, medicos), as.numeric)
  ) %>% 
  relocate(ano)


# juntando os dados de 2005 a 2020

medicos <- bind_rows(medico_2005_2006, medico_2007_2020) %>% 
  arrange(ano)


# BENEFICIÁRIOS PLANO PRIVADO ---------------------------------

plsaudebenef_x2000_x2020 <- readxl::read_xlsx("dados/dados_brutos/plsaudebenef_2000_2020.xlsx") %>% 
  view()

benef_priv_2000_2020 <- plsaudebenef_x2000_x2020 %>% 
  slice(-(1:4)) %>% 
  slice(1:(n() - 8)) %>% 
  row_to_names(row_number = 1) %>% 
  select(
    1, 
    rev(2:ncol(.))
  ) %>% 
  separate(
    Município, 
    into = c("cod_mun_ibge", "nome_mun"), 
    sep = " ", 
    extra = "merge"
  ) %>% 
  pivot_longer(
    cols = -c(cod_mun_ibge, nome_mun),
    names_to = "ano",
    values_to = "benef_pl_priv"
  ) %>% 
  mutate(
    across(c(ano, benef_pl_priv), as.numeric)
  ) %>% 
  relocate(ano)


# ASPS -------------------------------------------------------------------------

perc_asps_2000_2021 <- readxl::read_xlsx("dados/dados_brutos/percASPS_2000_2021.xlsx") %>% 
  clean_names() %>% 
  view()

perc_asps <- perc_asps_2000_2021 %>%
  select(-last_col()) %>% 
  slice(-(1:2), -n()) %>% 
  row_to_names(row_number = 1) %>% 
  separate(
    `Munic-BR`, 
    into = c("cod_mun_ibge", "nome_mun"), 
    sep = " ", 
    extra = "merge"
  ) %>%
  mutate(
    across(`2000`:`2021`, as.numeric)
  ) %>% 
  pivot_longer(
    cols = -c(cod_mun_ibge, nome_mun),
    names_to = "ano",
    values_to = "pct_asps"
  ) %>% 
  mutate(
    ano = as.numeric(ano),
    pct_asps = as.numeric(pct_asps) / 100
  ) %>% 
  relocate(ano) %>% 
  arrange(ano)


# TRANSFERÊNCIA SUS ------------------------------------------------------------

transf_sus_2000_2021 <- readxl::read_xlsx("dados/dados_brutos/perctransfSUS_2000_2021.xlsx") %>% 
  clean_names() %>% 
  view()

transf_sus <- transf_sus_2000_2021 %>%
  select(-last_col()) %>% 
  slice(-(1:2), -n()) %>% 
  row_to_names(row_number = 1) %>% 
  separate(
    `Munic-BR`, 
    into = c("cod_mun_ibge", "nome_mun"), 
    sep = " ", 
    extra = "merge"
  ) %>%
  mutate(
    across(`2000`:`2021`, as.numeric)
  ) %>% 
  pivot_longer(
    cols = -c(cod_mun_ibge, nome_mun),
    names_to = "ano",
    values_to = "pct_transf_sus"
  ) %>% 
  mutate(
    ano = as.numeric(ano),
    pct_transf_sus = as.numeric(pct_transf_sus) / 100
  ) %>% 
  relocate(ano) %>% 
  arrange(ano)


# ESF --------------------------------------------------------------------------

# dados de ESF 1999 a 2006

esf_x1999_x2006 <- readxl::read_xlsx("dados/dados_brutos/ESF 1999 a 2006.xlsx") %>% 
  clean_names() %>% 
  view()

esf_1999_2006 <- esf_x1999_x2006 %>%
  rename(
    cod_mun_ibge = codigo,
    nome_mun = municipio,
    qt_pop = populacao,
    qt_cob_esf = pop_acomp
  ) %>% 
  mutate(
    cob_esf = qt_cob_esf / qt_pop * 100,
    grau_cob_esf = case_when(
      cob_esf == 0 ~ 0,
      cob_esf < 30 ~ 1,
      cob_esf >= 30 & cob_esf < 70 ~ 2,
      cob_esf >= 70 ~ 3
  )) %>% 
  relocate(
    ano, mes, uf, cod_mun_ibge, mun6, nome_mun, qt_pop, qt_cob_esf, cob_esf, grau_cob_esf
  )















