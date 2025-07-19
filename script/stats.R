# carregamento dos pacotes

library(tinytable)
library(moments)


# carregamento da base de dados

base <- readxl::read_xlsx("dados/dados_tratados/base_geral.xlsx")


# análise de estatísticas descritivas

stats <- base %>% 
  pivot_longer(
    cols = -c(ano, cod_mun, nome_mun), 
    names_to = "variaveis", 
    values_to = "valores"
  ) %>%
  distinct(ano, cod_mun, variaveis, valores) %>% 
  group_by(variaveis, ano) %>% 
  summarise(
    n_validos = sum(!is.na(valores)),
    media = mean(valores, na.rm = T), 
    mediana = median(valores, na.rm = T),
    dp = sd(valores, na.rm = T),
    min = min(valores, na.rm = T),
    max = max(valores, na.rm = T),
    q1 = quantile(valores, 0.25, na.rm = T),
    q3 = quantile(valores, 0.75, na.rm = T),
    iqr = q3 - q1,
    n_outliers = sum(valores < (q1 - 1.5 * iqr) | valores > (q3 + 1.5 * iqr), na.rm = T)
  ) %>%
  arrange(variaveis, ano) %>% 
  filter(variaveis == "agua")


# análise de duplicatas

duplicatas <- base %>% 
  pivot_longer(
    cols = -c(ano, cod_mun, nome_mun), 
    names_to = "variaveis", 
    values_to = "valores"
  ) %>% 
  group_by(ano, cod_mun, variaveis, valores) %>%
  filter(n() > 1) %>%
  arrange(ano, variaveis)

resumo_duplicatas <- duplicatas %>%
  group_by(ano, variaveis) %>%
  summarise(
    n_duplicatas = n(),
    n_municipios_afetados = n_distinct(cod_mun)
  ) %>%
  arrange(variaveis, ano)


