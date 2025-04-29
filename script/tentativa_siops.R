curl ^"http://siops.datasus.gov.br/rel_subfuncao.php^" ^
  -H ^"Accept: text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,image/apng,*/*;q=0.8,application/signed-exchange;v=b3;q=0.7^" ^
  -H ^"Accept-Language: pt-BR,pt;q=0.9,en;q=0.8,en-GB;q=0.7,en-US;q=0.6^" ^
  -H ^"Cache-Control: max-age=0^" ^
  -H ^"Connection: keep-alive^" ^
  -H ^"Content-Type: application/x-www-form-urlencoded^" ^
  -b ^"PHPSESSID=eah0srfjhh8qmvkti775m58hf1^" ^
  -H ^"Origin: http://siops.datasus.gov.br^" ^
  -H ^"Referer: http://siops.datasus.gov.br/rel_subfuncao.php?S=1^&UF=35;^&Municipio=355030;^&Fase=10;^&Pasta=5;^" ^
  -H ^"Upgrade-Insecure-Requests: 1^" ^
  -H ^"User-Agent: Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/135.0.0.0 Safari/537.36 Edg/135.0.0.0^" ^
  --data-raw ^"cmbUF=35^&cmbMunicipio^%^5B^%^5D=355030^&cmbFase=10^&cmbPasta=5^&BtConsultar=Consultar^" ^
  --insecure


library(httr)
library(rvest)

lista_cod_mun <- geobr::read_municipality(year = 2022) %>% sf::st_drop_geometry()

codigos_muni <- lista_cod_mun %>% mutate(code_muni = str_sub(code_muni,1,6)) %>% pull(code_muni)

codigos_uf <- lista_cod_mun %>% mutate(code_muni = str_sub(code_muni,1,2)) %>% pull(code_muni)

# URL e parâmetros
url <- "http://siops.datasus.gov.br/rel_subfuncao.php"

teste <- 

map2(codigos_uf,codigos_muni, function(x,y) {
  
  # Headers necessários (obtidos do curl)
  headers <- c(
    "Accept" = "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8",
    "Content-Type" = "application/x-www-form-urlencoded",
    "Origin" = "http://siops.datasus.gov.br",
    "Referer" = paste0("http://siops.datasus.gov.br/rel_subfuncao.php?S=1&UF=",x,";&Municipio=",y,";&Fase=10;&Pasta=5;"),
    "User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64)"
  )
  
  # Corpo da requisição (dados POST)
  body <- list(
    cmbUF = x,
    "cmbMunicipio[]" = y,
    cmbFase = "10",
    cmbPasta = "5",
    BtConsultar = "Consultar"
  )
  
  # Realiza a requisição POST
  response <- POST(url, 
                   add_headers(.headers = headers),
                   body = body, 
                   encode = "form")
  
  # Verifica se foi bem sucedido
  stop_for_status(response)
  
  # Lê a resposta HTML
  pagina_html <- content(response, "text", encoding = "latin1")
  
  # Usa rvest para extrair tabelas
  pagina <- read_html(pagina_html)
  
  tabelas <- pagina %>% html_table(fill = TRUE)
  
  if (length(tabelas)==1) {
    
    tabelas[[1]] %>% 
      rename_with(~ tabelas[[1]][1,] %>% unlist() %>% unname()) %>% 
      slice(-1) %>% 
      pivot_longer(cols = starts_with("20"), names_to = "ano", values_to = "valor") %>% 
      mutate(valor = str_replace_all(valor,fixed("."),"") %>% str_replace(., ",",".")) %>% 
      mutate(valor = as.numeric(valor)) %>% 
      filter(!(Código %in% c(
        "Total:",
        "Municípios contados:",
        "População contada:",
        "Total de Municípios:",
        "População Total:")),
        !(str_starts(Código, "NOTA"))) %>% mutate(cod_mun = y)
    
  } else {
    
    print(paste0("O município de código ", y, " por algum motivo não pode ser extraido. Quando puxou veio mais de uma tabela. Dá uma olhada nesse municipio lá no site pra entender"))
    
  }
  
  
  
  
})




df %>% distinct(Código) %>% clipr::write_clip()
