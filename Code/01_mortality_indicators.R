###################################################################################################
###################################################################################################

#### PROJETO: DESIGUALDADE TERRITORIAL EM SAUDE NO BRASIL ####
#### INDICADORES MUNICIPAIS DE MORTALIDADE - SINASC E SIM ####

rm(list = ls(all.names=TRUE)) # limpar area de trabalho do R
gc()

######### PACOTES #########

pacotes <- c("tidyverse", "data.table", "sf", "geobr", "readxl")
faltantes <- pacotes[!sapply(pacotes, requireNamespace, quietly = TRUE)]
if(length(faltantes) > 0) install.packages(faltantes, repos = "https://cloud.r-project.org")

library(tidyverse)
library(data.table)
library(sf)
library(geobr)
library(readxl)

######### ENDERECOS E PARAMETROS #########

Endereco_raw <- "Raw Data"
Endereco_projeto <- "Mortality Index Project"
Endereco_dados <- file.path(Endereco_projeto, "Data")
Endereco_saida <- file.path(Endereco_projeto, "Output")
Endereco_mapas <- file.path(Endereco_saida, "Maps")

dir.create(Endereco_dados, recursive = TRUE, showWarnings = FALSE)
dir.create(Endereco_saida, recursive = TRUE, showWarnings = FALSE)
dir.create(Endereco_mapas, recursive = TRUE, showWarnings = FALSE)

ano_referencia_mapa <- 2005L
numero_classes_mapa <- 7L
paleta_roxa <- rev(hcl.colors(numero_classes_mapa, "Purples 3"))

anos_analise <- as.integer(str_split(
  Sys.getenv("ANALYSIS_YEARS", "2000,2005,2010,2015"),
  ",",
  simplify = TRUE
))

arquivo_log <- file.path(Endereco_saida, "validation_log_R.txt")
writeLines("VALIDACAO - R", arquivo_log)

######### LOCALIZAR AS BASES #########

arquivos_sinasc <- list.files(
  file.path(Endereco_raw, "ETLSINASC"),
  recursive = TRUE,
  full.names = TRUE,
  pattern = "\\.(csv|txt|dbf|dbc|parquet|rds)$",
  ignore.case = TRUE
)

anos_arquivos_sinasc <- as.integer(
  str_match(basename(arquivos_sinasc), "_(\\d{4})_t")[,2]
)

arquivos_sinasc <- arquivos_sinasc[
  str_starts(basename(arquivos_sinasc), "ETLSINASC") &
    anos_arquivos_sinasc %in% anos_analise
]

arquivos_sim <- list.files(
  file.path(Endereco_raw, "ETLSIM"),
  recursive = TRUE,
  full.names = TRUE,
  pattern = "\\.(csv|txt|dbf|dbc|parquet|rds)$",
  ignore.case = TRUE
)

anos_arquivos_sim <- as.integer(
  str_match(basename(arquivos_sim), "_(\\d{4})_t")[,2]
)

arquivos_sim <- arquivos_sim[
  str_starts(basename(arquivos_sim), "ETLSIM") &
    anos_arquivos_sim %in% anos_analise
]

if(length(arquivos_sinasc) == 0 | length(arquivos_sim) == 0){
  stop("Bases SINASC/SIM ausentes.")
}

texto_log <- paste(
  "Arquivos SINASC:", length(arquivos_sinasc),
  "- arquivos SIM:", length(arquivos_sim)
)
message(texto_log)
cat(texto_log, "\n", file = arquivo_log, append = TRUE)

###################################################################################################
######### PARTE 1 - NASCIDOS VIVOS (SINASC) #########

resumo_nascimentos <- list()
contador <- 1

for(arquivo in arquivos_sinasc){

  colunas_sinasc <- c(
    "CODMUNRES", "CODMUNNASC", "ano_nasc",
    "res_codigo_adotado", "nasc_codigo_adotado",
    "res_MUNNOME", "nasc_MUNNOME"
  )

  extensao <- tolower(tools::file_ext(arquivo))

  if(extensao %in% c("csv", "txt")){
    cabecalho <- names(fread(arquivo, nrows = 0, encoding = "UTF-8"))
    selecionar <- intersect(colunas_sinasc, cabecalho)
    base <- fread(
      arquivo,
      select = selecionar,
      colClasses = "character",
      encoding = "UTF-8",
      showProgress = FALSE
    )
  }

  if(extensao == "parquet"){
    if(!requireNamespace("arrow", quietly = TRUE)) install.packages("arrow")
    base <- as.data.table(arrow::read_parquet(arquivo, col_select = colunas_sinasc))
  }

  if(extensao == "rds"){
    base <- as.data.table(readRDS(arquivo))
    base <- base[, intersect(colunas_sinasc, names(base)), with = FALSE]
  }

  if(extensao == "dbf"){
    base <- as.data.table(foreign::read.dbf(arquivo, as.is = TRUE))
    base <- base[, intersect(colunas_sinasc, names(base)), with = FALSE]
  }

  if(extensao == "dbc"){
    if(!requireNamespace("read.dbc", quietly = TRUE)) install.packages("read.dbc")
    base <- as.data.table(read.dbc::read.dbc(arquivo))
    base <- base[, intersect(colunas_sinasc, names(base)), with = FALSE]
  }

  if(!extensao %in% c("csv", "txt", "parquet", "rds", "dbf", "dbc")){
    stop(paste("Formato nao reconhecido:", arquivo))
  }

  if(!all(c("CODMUNRES", "CODMUNNASC") %in% names(base))){
    stop(paste("Colunas municipais SINASC ausentes:", basename(arquivo)))
  }

  ano_do_arquivo <- as.integer(str_match(basename(arquivo), "_(\\d{4})_t")[,2])
  base$ano <- if("ano_nasc" %in% names(base)) as.integer(base$ano_nasc) else ano_do_arquivo

  texto_log <- paste("SINASC", basename(arquivo), ":", nrow(base), "observacoes")
  message(texto_log)
  cat(texto_log, "\n", file = arquivo_log, append = TRUE)

  ######### NASCIMENTOS POR RESIDENCIA #########

  if("res_codigo_adotado" %in% names(base)){
    codigo_municipio <- coalesce(base$res_codigo_adotado, base$CODMUNRES)
  } else {
    codigo_municipio <- base$CODMUNRES
  }

  codigo_municipio <- str_trim(str_replace(as.character(codigo_municipio), "\\.0$", ""))
  codigo_municipio[!str_detect(codigo_municipio, "^\\d{6,7}$")] <- NA_character_
  codigo_municipio <- str_sub(codigo_municipio, 1, 6)
  codigo_municipio[str_ends(codigo_municipio, "0000")] <- NA_character_

  nascimentos_residencia <- data.table(
    municipality_code = codigo_municipio,
    municipality_name = if("res_MUNNOME" %in% names(base)) base$res_MUNNOME else NA_character_,
    year = base$ano,
    measure_type = "residence"
  )

  nascimentos_residencia <- nascimentos_residencia[
    !is.na(municipality_code) & !is.na(year)
  ]

  resumo_nascimentos[[contador]] <- nascimentos_residencia[, .(
    live_births = .N,
    municipality_name = first(na.omit(municipality_name))
  ), by = .(municipality_code, year, measure_type)]

  contador <- contador + 1

  ######### NASCIMENTOS POR OCORRENCIA #########

  if("nasc_codigo_adotado" %in% names(base)){
    codigo_municipio <- coalesce(base$nasc_codigo_adotado, base$CODMUNNASC)
  } else {
    codigo_municipio <- base$CODMUNNASC
  }

  codigo_municipio <- str_trim(str_replace(as.character(codigo_municipio), "\\.0$", ""))
  codigo_municipio[!str_detect(codigo_municipio, "^\\d{6,7}$")] <- NA_character_
  codigo_municipio <- str_sub(codigo_municipio, 1, 6)
  codigo_municipio[str_ends(codigo_municipio, "0000")] <- NA_character_

  nascimentos_ocorrencia <- data.table(
    municipality_code = codigo_municipio,
    municipality_name = if("nasc_MUNNOME" %in% names(base)) base$nasc_MUNNOME else NA_character_,
    year = base$ano,
    measure_type = "occurrence"
  )

  nascimentos_ocorrencia <- nascimentos_ocorrencia[
    !is.na(municipality_code) & !is.na(year)
  ]

  resumo_nascimentos[[contador]] <- nascimentos_ocorrencia[, .(
    live_births = .N,
    municipality_name = first(na.omit(municipality_name))
  ), by = .(municipality_code, year, measure_type)]

  contador <- contador + 1

  rm(base, nascimentos_residencia, nascimentos_ocorrencia, codigo_municipio)
  gc()
}

nascimentos <- rbindlist(resumo_nascimentos, fill = TRUE)[, .(
  live_births = sum(live_births),
  municipality_name = first(na.omit(municipality_name))
), by = .(municipality_code, year, measure_type)]

###################################################################################################
######### PARTE 2 - OBITOS (SIM) #########

## IDADE conforme dict_SIM.csv:
## 0 = minutos; 1 = horas; 2 = dias; 3 = meses; 4 = anos.
## 400 = menor de um ano, mas unidade exata desconhecida.

resumo_obitos <- list()
contador <- 1

for(arquivo in arquivos_sim){

  colunas_sim <- c(
    "TIPOBITO", "CODMUNRES", "CODMUNOCOR", "ano_obito", "IDADE",
    "SEXO", "CAUSABAS", "res_codigo_adotado", "ocor_codigo_adotado",
    "res_MUNNOME", "ocor_MUNNOME"
  )

  extensao <- tolower(tools::file_ext(arquivo))

  if(extensao %in% c("csv", "txt")){
    cabecalho <- names(fread(arquivo, nrows = 0, encoding = "UTF-8"))
    selecionar <- intersect(colunas_sim, cabecalho)
    base <- fread(
      arquivo,
      select = selecionar,
      colClasses = "character",
      encoding = "UTF-8",
      showProgress = FALSE
    )
  }

  if(extensao == "parquet"){
    if(!requireNamespace("arrow", quietly = TRUE)) install.packages("arrow")
    base <- as.data.table(arrow::read_parquet(arquivo, col_select = colunas_sim))
  }

  if(extensao == "rds"){
    base <- as.data.table(readRDS(arquivo))
    base <- base[, intersect(colunas_sim, names(base)), with = FALSE]
  }

  if(extensao == "dbf"){
    base <- as.data.table(foreign::read.dbf(arquivo, as.is = TRUE))
    base <- base[, intersect(colunas_sim, names(base)), with = FALSE]
  }

  if(extensao == "dbc"){
    if(!requireNamespace("read.dbc", quietly = TRUE)) install.packages("read.dbc")
    base <- as.data.table(read.dbc::read.dbc(arquivo))
    base <- base[, intersect(colunas_sim, names(base)), with = FALSE]
  }

  if(!extensao %in% c("csv", "txt", "parquet", "rds", "dbf", "dbc")){
    stop(paste("Formato nao reconhecido:", arquivo))
  }

  obrigatorias <- c("TIPOBITO", "CODMUNRES", "CODMUNOCOR", "IDADE", "SEXO", "CAUSABAS")
  if(!all(obrigatorias %in% names(base))){
    stop(paste("Colunas SIM ausentes:", basename(arquivo)))
  }

  base <- base[as.integer(TIPOBITO) == 2] # exclui obito fetal

  ano_do_arquivo <- as.integer(str_match(basename(arquivo), "_(\\d{4})_t")[,2])
  base$ano <- if("ano_obito" %in% names(base)) as.integer(base$ano_obito) else ano_do_arquivo

  idade <- suppressWarnings(as.numeric(base$IDADE))
  unidade_idade <- floor(idade / 100)
  quantidade_idade <- idade %% 100
  minutos_conhecidos <- unidade_idade == 0 & idade != 0

  base$neonatal_deaths <- as.integer(
    (minutos_conhecidos |
       unidade_idade == 1 |
       (unidade_idade == 2 & quantidade_idade <= 27)) %in% TRUE
  )

  base$infant_deaths <- as.integer(
    (minutos_conhecidos |
       unidade_idade %in% c(1,2,3) |
       idade == 400) %in% TRUE
  )

  base$under5_deaths <- as.integer(
    (base$infant_deaths == 1 |
       (unidade_idade == 4 & quantidade_idade < 5)) %in% TRUE
  )

  ## Morte materna = mulher com causa basica CID-10 O00-O95 ou O98-O99.
  ## O96/O97 (tardia/sequela) nao entram na razao padrao.
  cid <- str_replace_all(str_to_upper(base$CAUSABAS), "[^A-Z0-9]", "")
  cid_numero <- suppressWarnings(as.integer(str_match(cid, "^O(\\d{2})")[,2]))

  base$maternal_deaths <- as.integer(
    as.integer(base$SEXO) == 2 &
      ((cid_numero >= 0 & cid_numero <= 95) |
         (cid_numero >= 98 & cid_numero <= 99))
  )

  base$maternal_deaths[is.na(base$maternal_deaths)] <- 0

  texto_log <- paste("SIM", basename(arquivo), ":", nrow(base), "obitos nao fetais")
  message(texto_log)
  cat(texto_log, "\n", file = arquivo_log, append = TRUE)

  ######### OBITOS POR RESIDENCIA #########

  if("res_codigo_adotado" %in% names(base)){
    codigo_municipio <- coalesce(base$res_codigo_adotado, base$CODMUNRES)
  } else {
    codigo_municipio <- base$CODMUNRES
  }

  codigo_municipio <- str_trim(str_replace(as.character(codigo_municipio), "\\.0$", ""))
  codigo_municipio[!str_detect(codigo_municipio, "^\\d{6,7}$")] <- NA_character_
  codigo_municipio <- str_sub(codigo_municipio, 1, 6)
  codigo_municipio[str_ends(codigo_municipio, "0000")] <- NA_character_

  obitos_residencia <- data.table(
    municipality_code = codigo_municipio,
    municipality_name = if("res_MUNNOME" %in% names(base)) base$res_MUNNOME else NA_character_,
    year = base$ano,
    measure_type = "residence",
    neonatal_deaths = base$neonatal_deaths,
    infant_deaths = base$infant_deaths,
    under5_deaths = base$under5_deaths,
    maternal_deaths = base$maternal_deaths
  )

  obitos_residencia <- obitos_residencia[
    !is.na(municipality_code) & !is.na(year)
  ]

  resumo_obitos[[contador]] <- obitos_residencia[, .(
    neonatal_deaths = sum(neonatal_deaths),
    infant_deaths = sum(infant_deaths),
    under5_deaths = sum(under5_deaths),
    maternal_deaths = sum(maternal_deaths),
    municipality_name = first(na.omit(municipality_name))
  ), by = .(municipality_code, year, measure_type)]

  contador <- contador + 1

  ######### OBITOS POR OCORRENCIA #########

  if("ocor_codigo_adotado" %in% names(base)){
    codigo_municipio <- coalesce(base$ocor_codigo_adotado, base$CODMUNOCOR)
  } else {
    codigo_municipio <- base$CODMUNOCOR
  }

  codigo_municipio <- str_trim(str_replace(as.character(codigo_municipio), "\\.0$", ""))
  codigo_municipio[!str_detect(codigo_municipio, "^\\d{6,7}$")] <- NA_character_
  codigo_municipio <- str_sub(codigo_municipio, 1, 6)
  codigo_municipio[str_ends(codigo_municipio, "0000")] <- NA_character_

  obitos_ocorrencia <- data.table(
    municipality_code = codigo_municipio,
    municipality_name = if("ocor_MUNNOME" %in% names(base)) base$ocor_MUNNOME else NA_character_,
    year = base$ano,
    measure_type = "occurrence",
    neonatal_deaths = base$neonatal_deaths,
    infant_deaths = base$infant_deaths,
    under5_deaths = base$under5_deaths,
    maternal_deaths = base$maternal_deaths
  )

  obitos_ocorrencia <- obitos_ocorrencia[
    !is.na(municipality_code) & !is.na(year)
  ]

  resumo_obitos[[contador]] <- obitos_ocorrencia[, .(
    neonatal_deaths = sum(neonatal_deaths),
    infant_deaths = sum(infant_deaths),
    under5_deaths = sum(under5_deaths),
    maternal_deaths = sum(maternal_deaths),
    municipality_name = first(na.omit(municipality_name))
  ), by = .(municipality_code, year, measure_type)]

  contador <- contador + 1

  rm(base, obitos_residencia, obitos_ocorrencia, codigo_municipio)
  gc()
}

obitos <- rbindlist(resumo_obitos, fill = TRUE)[, .(
  neonatal_deaths = sum(neonatal_deaths),
  infant_deaths = sum(infant_deaths),
  under5_deaths = sum(under5_deaths),
  maternal_deaths = sum(maternal_deaths),
  municipality_name = first(na.omit(municipality_name))
), by = .(municipality_code, year, measure_type)]

###################################################################################################
######### PARTE 3 - INDICADORES MUNICIPAIS #########

indicadores <- full_join(
  nascimentos,
  obitos,
  by = c("municipality_code", "year", "measure_type"),
  suffix = c("_birth", "_death")
) %>%
  mutate(
    municipality_name = coalesce(municipality_name_birth, municipality_name_death),
    across(
      c(live_births, neonatal_deaths, infant_deaths, under5_deaths, maternal_deaths),
      ~replace_na(as.integer(.x), 0L)
    ),
    neonatal_per_1000 = if_else(
      live_births > 0, 1000 * neonatal_deaths / live_births, NA_real_
    ),
    infant_per_1000 = if_else(
      live_births > 0, 1000 * infant_deaths / live_births, NA_real_
    ),
    under5_per_1000 = if_else(
      live_births > 0, 1000 * under5_deaths / live_births, NA_real_
    ),
    maternal_per_100000 = if_else(
      live_births > 0, 100000 * maternal_deaths / live_births, NA_real_
    )
  ) %>%
  select(
    municipality_code, municipality_name, year, measure_type,
    live_births, neonatal_deaths, infant_deaths, under5_deaths, maternal_deaths,
    neonatal_per_1000, infant_per_1000, under5_per_1000, maternal_per_100000
  ) %>%
  arrange(municipality_code, year, measure_type)

fwrite(nascimentos, file.path(Endereco_saida, "sinasc_births_municipality_year_R.csv"))
fwrite(obitos, file.path(Endereco_saida, "sim_deaths_municipality_year_R.csv"))
fwrite(indicadores, file.path(Endereco_saida, "mortality_indicators_municipality_year_R.csv"))

mensagens_validacao <- c(
  paste("Anos SINASC:", min(nascimentos$year), "-", max(nascimentos$year)),
  paste("Anos SIM:", min(obitos$year), "-", max(obitos$year)),
  paste("Municipios na base final:", n_distinct(indicadores$municipality_code)),
  paste(
    "Municipio/ano/tipo com obitos infantis > nascidos vivos:",
    sum(indicadores$infant_deaths > indicadores$live_births)
  ),
  paste(
    "Ausentes - codigo:", sum(is.na(indicadores$municipality_code)),
    "- ano:", sum(is.na(indicadores$year))
  )
)

for(texto_log in mensagens_validacao){
  message(texto_log)
  cat(texto_log, "\n", file = arquivo_log, append = TRUE)
}

###################################################################################################
######### PARTE 4 - REGIOES DE SAUDE #########

arquivo_crosswalk <- file.path(Endereco_raw, "Health Regions BR", "tb_ibge.xlsx")
arquivo_regioes <- file.path(Endereco_raw, "Health Regions BR", "tb_regiao_saude.xlsx")

if(!file.exists(arquivo_crosswalk) | !file.exists(arquivo_regioes)){
  stop("Crosswalk ou geometria das regioes de saude nao encontrados.")
}

texto_crosswalk <- readxl::read_excel(arquivo_crosswalk, col_names = FALSE)[[1]]

crosswalk <- fread(
  text = paste(texto_crosswalk, collapse = "\n"),
  colClasses = "character",
  encoding = "UTF-8"
)

codigo_municipio <- str_trim(str_replace(as.character(crosswalk$ibge), "\\.0$", ""))
codigo_municipio[!str_detect(codigo_municipio, "^\\d{6,7}$")] <- NA_character_
codigo_municipio <- str_sub(codigo_municipio, 1, 6)
codigo_municipio[str_ends(codigo_municipio, "0000")] <- NA_character_

crosswalk <- crosswalk[, .(
  municipality_code = codigo_municipio,
  health_region_code = str_pad(co_regiao_saude, 5, pad = "0"),
  municipality_name_crosswalk = no_cidade
)]

crosswalk <- unique(
  crosswalk[!is.na(municipality_code) & health_region_code != "00000"]
)

texto_log <- paste("Municipios no crosswalk de regioes de saude:", nrow(crosswalk))
message(texto_log)
cat(texto_log, "\n", file = arquivo_log, append = TRUE)

texto_log <- paste("Regioes de saude no crosswalk:", uniqueN(crosswalk$health_region_code))
message(texto_log)
cat(texto_log, "\n", file = arquivo_log, append = TRUE)

grade_completa <- expand_grid(
  municipality_code = crosswalk$municipality_code,
  year = anos_analise,
  measure_type = c("residence", "occurrence")
)

indicadores <- grade_completa %>%
  left_join(indicadores, by = c("municipality_code", "year", "measure_type")) %>%
  left_join(
    select(crosswalk, municipality_code, municipality_name_crosswalk),
    by = "municipality_code"
  ) %>%
  mutate(
    municipality_name = coalesce(municipality_name, municipality_name_crosswalk),
    across(
      c(live_births, neonatal_deaths, infant_deaths, under5_deaths, maternal_deaths),
      ~replace_na(as.integer(.x), 0L)
    ),
    neonatal_per_1000 = if_else(
      live_births > 0, 1000 * neonatal_deaths / live_births, NA_real_
    ),
    infant_per_1000 = if_else(
      live_births > 0, 1000 * infant_deaths / live_births, NA_real_
    ),
    under5_per_1000 = if_else(
      live_births > 0, 1000 * under5_deaths / live_births, NA_real_
    ),
    maternal_per_100000 = if_else(
      live_births > 0, 100000 * maternal_deaths / live_births, NA_real_
    )
  ) %>%
  select(-municipality_name_crosswalk) %>%
  arrange(municipality_code, year, measure_type)

fwrite(indicadores, file.path(Endereco_saida, "mortality_indicators_municipality_year_R.csv"))

texto_log <- paste("Linhas no painel municipal completo:", nrow(indicadores))
message(texto_log)
cat(texto_log, "\n", file = arquivo_log, append = TRUE)

indicadores_regiao <- left_join(indicadores, crosswalk, by = "municipality_code")

sem_regiao <- indicadores_regiao %>%
  filter(is.na(health_region_code)) %>%
  distinct(municipality_code)

texto_log <- paste("Municipios da base final sem regiao de saude:", nrow(sem_regiao))
message(texto_log)
cat(texto_log, "\n", file = arquivo_log, append = TRUE)

indicadores_regiao <- indicadores_regiao %>%
  filter(!is.na(health_region_code)) %>%
  group_by(health_region_code, year, measure_type) %>%
  summarise(
    live_births = sum(live_births),
    neonatal_deaths = sum(neonatal_deaths),
    infant_deaths = sum(infant_deaths),
    under5_deaths = sum(under5_deaths),
    maternal_deaths = sum(maternal_deaths),
    .groups = "drop"
  ) %>%
  mutate(
    neonatal_per_1000 = if_else(
      live_births > 0, 1000 * neonatal_deaths / live_births, NA_real_
    ),
    infant_per_1000 = if_else(
      live_births > 0, 1000 * infant_deaths / live_births, NA_real_
    ),
    under5_per_1000 = if_else(
      live_births > 0, 1000 * under5_deaths / live_births, NA_real_
    ),
    maternal_per_100000 = if_else(
      live_births > 0, 100000 * maternal_deaths / live_births, NA_real_
    )
  ) %>%
  arrange(health_region_code, year, measure_type)

fwrite(
  indicadores_regiao,
  file.path(Endereco_saida, "mortality_indicators_health_region_year_R.csv")
)

###################################################################################################
######### PARTE 5 - MAPAS MUNICIPAIS #########

Endereco_shape <- file.path(Endereco_dados, "Shapefiles", "municipios_2020")
arquivo_shape <- file.path(Endereco_shape, "municipios_2020.shp")
dir.create(Endereco_shape, recursive = TRUE, showWarnings = FALSE)

if(!file.exists(arquivo_shape)){
  texto_log <- "Baixando malha municipal de 2020 com geobr."
  message(texto_log)
  cat(texto_log, "\n", file = arquivo_log, append = TRUE)

  GEO_MUNIC <- geobr::read_municipality(
    code_muni = "all",
    year = 2020,
    simplified = TRUE,
    showProgress = FALSE
  )

  st_write(GEO_MUNIC, arquivo_shape, delete_layer = TRUE, quiet = TRUE)

  texto_log <- paste("Shapefile salvo em:", arquivo_shape)
  message(texto_log)
  cat(texto_log, "\n", file = arquivo_log, append = TRUE)
}

if(file.exists(arquivo_shape)){

  GEO_MUNIC <- st_read(arquivo_shape, quiet = TRUE)
  opcoes_codigo <- c(
    "code_muni", "code_mn", "CODMUN", "CD_MUN",
    "CD_MUN7", "GEOCODIGO", "IBGE"
  )
  coluna_shape <- intersect(names(GEO_MUNIC), opcoes_codigo)[1]

  if(is.na(coluna_shape)){
    texto_log <- "AVISO MAPA: coluna municipal nao identificada no shapefile geobr."
    message(texto_log)
    cat(texto_log, "\n", file = arquivo_log, append = TRUE)
  } else {

    codigo_municipio <- str_trim(
      str_replace(as.character(GEO_MUNIC[[coluna_shape]]), "\\.0$", "")
    )
    codigo_municipio[!str_detect(codigo_municipio, "^\\d{6,7}$")] <- NA_character_
    codigo_municipio <- str_sub(codigo_municipio, 1, 6)
    codigo_municipio[str_ends(codigo_municipio, "0000")] <- NA_character_
    GEO_MUNIC$municipality_code <- codigo_municipio

    principal <- indicadores %>% filter(measure_type == "residence")
    sem_shape <- setdiff(
      unique(principal$municipality_code),
      unique(GEO_MUNIC$municipality_code)
    )

    texto_log <- paste("Municipios sem correspondencia no shapefile:", length(sem_shape))
    message(texto_log)
    cat(texto_log, "\n", file = arquivo_log, append = TRUE)

    variaveis <- c(
      "neonatal_per_1000", "infant_per_1000",
      "under5_per_1000", "maternal_per_100000"
    )

    escalas_municipios <- list()

    for(variavel in variaveis){
      referencia <- principal %>%
        filter(year == ano_referencia_mapa) %>%
        pull(.data[[variavel]])

      referencia <- as.numeric(referencia)
      referencia <- referencia[!is.na(referencia)]

      if(length(referencia) == 0){
        escalas_municipios[[variavel]] <- NULL
      } else if(length(unique(referencia)) == 1){
        valor_unico <- unique(referencia)
        escalas_municipios[[variavel]] <- list(
          quebras = c(-Inf, valor_unico, Inf),
          rotulos = c(
            paste0("<= ", round(valor_unico, 2)),
            paste0("> ", round(valor_unico, 2))
          )
        )
      } else {
        cortes <- unique(as.numeric(quantile(
          referencia,
          probs = seq(
            1 / numero_classes_mapa,
            (numero_classes_mapa - 1) / numero_classes_mapa,
            length.out = numero_classes_mapa - 1
          ),
          na.rm = TRUE,
          type = 7
        )))

        if(length(cortes) < (numero_classes_mapa - 1)){
          cortes <- seq(
            min(referencia, na.rm = TRUE),
            max(referencia, na.rm = TRUE),
            length.out = numero_classes_mapa + 1
          )[2:numero_classes_mapa]
        }

        cortes <- unique(cortes)
        limite_inf <- c(-Inf, cortes)
        limite_sup <- c(cortes, Inf)

        rotulos <- ifelse(
          is.infinite(limite_inf),
          paste0("<= ", round(limite_sup, 2)),
          ifelse(
            is.infinite(limite_sup),
            paste0("> ", round(limite_inf, 2)),
            paste0(round(limite_inf, 2), " - ", round(limite_sup, 2))
          )
        )

        escalas_municipios[[variavel]] <- list(
          quebras = c(-Inf, cortes, Inf),
          rotulos = rotulos
        )
      }
    }

    for(ano_ref in sort(unique(principal$year))){
      mapa_base <- left_join(
        GEO_MUNIC,
        filter(principal, year == ano_ref),
        by = "municipality_code"
      )

      for(variavel in variaveis){
        escala <- escalas_municipios[[variavel]]
        valores_mapa <- as.numeric(mapa_base[[variavel]])

        if(all(is.na(valores_mapa)) | is.null(escala)){
          mapa_base$classe_mapa <- factor(rep(NA_character_, length(valores_mapa)))
        } else {
          mapa_base$classe_mapa <- cut(
            valores_mapa,
            breaks = escala$quebras,
            labels = escala$rotulos,
            include.lowest = TRUE,
            ordered_result = TRUE
          )
        }

        grafico <- ggplot(mapa_base) +
          geom_sf(aes(fill = classe_mapa), size = .15, color = "white") +
          scale_fill_manual(values = paleta_roxa, na.value = "grey90", drop = FALSE) +
          theme_void(base_size = 10) +
          theme(
            legend.position = "right",
            legend.title = element_text(size = 8),
            legend.text = element_text(size = 7),
            plot.title = element_text(size = 11)
          ) +
          labs(
            title = paste(variavel, "- residencia -", ano_ref),
            fill = paste0(variavel, "\n7 categorias\nescala 2005")
          )

        ggsave(
          file.path(Endereco_mapas, paste0(variavel, "_residence_", ano_ref, ".png")),
          plot = grafico,
          width = 20,
          height = 15,
          units = "cm",
          dpi = 300
        )
      }
    }
  }
} else {
  texto_log <- "AVISO MAPA: shapefile municipal nao foi criado."
  message(texto_log)
  cat(texto_log, "\n", file = arquivo_log, append = TRUE)
}

###################################################################################################
######### PARTE 6 - MAPAS DAS REGIOES DE SAUDE #########

texto_regioes <- readxl::read_excel(arquivo_regioes, col_names = FALSE)[[1]]
inicio_registro <- which(str_detect(texto_regioes, "^[0-9]{5},"))
fim_registro <- c(inicio_registro[-1] - 1, length(texto_regioes))

registros <- character(length(inicio_registro))
for(i in seq_along(inicio_registro)){
  registros[i] <- paste0(
    texto_regioes[inicio_registro[i]:fim_registro[i]],
    collapse = ""
  )
}

regioes_tab <- fread(
  text = paste(c(texto_regioes[1], registros), collapse = "\n"),
  encoding = "UTF-8"
)

geometria_regiao <- vector("list", nrow(regioes_tab))
for(i in seq_len(nrow(regioes_tab))){
  hexadecimal <- regioes_tab$limite_geografico[i]
  geometria_regiao[[i]] <- as.raw(strtoi(
    substring(
      hexadecimal,
      seq(1, nchar(hexadecimal), 2),
      seq(2, nchar(hexadecimal), 2)
    ),
    16L
  ))
}
class(geometria_regiao) <- "WKB"

GEO_REGSAUDE <- st_sf(
  health_region_code = str_pad(
    as.character(regioes_tab$co_regiao_saude),
    5,
    pad = "0"
  ),
  health_region_name = regioes_tab$no_regiao_saude,
  geometry = st_as_sfc(geometria_regiao, EWKB = TRUE)
)

Endereco_shape_regiao <- file.path(Endereco_dados, "Shapefiles", "regioes_saude")
dir.create(Endereco_shape_regiao, recursive = TRUE, showWarnings = FALSE)

st_write(
  GEO_REGSAUDE,
  file.path(Endereco_shape_regiao, "regioes_saude.shp"),
  delete_layer = TRUE,
  quiet = TRUE
)

texto_log <- paste("Regioes na geometria:", nrow(GEO_REGSAUDE))
message(texto_log)
cat(texto_log, "\n", file = arquivo_log, append = TRUE)

principal_regiao <- indicadores_regiao %>% filter(measure_type == "residence")
sem_shape_regiao <- setdiff(
  unique(principal_regiao$health_region_code),
  unique(GEO_REGSAUDE$health_region_code)
)

texto_log <- paste(
  "Regioes de saude sem correspondencia na geometria:",
  length(sem_shape_regiao)
)
message(texto_log)
cat(texto_log, "\n", file = arquivo_log, append = TRUE)

escalas_regioes <- list()

for(variavel in variaveis){
  referencia <- principal_regiao %>%
    filter(year == ano_referencia_mapa) %>%
    pull(.data[[variavel]])

  referencia <- as.numeric(referencia)
  referencia <- referencia[!is.na(referencia)]

  if(length(referencia) == 0){
    escalas_regioes[[variavel]] <- NULL
  } else if(length(unique(referencia)) == 1){
    valor_unico <- unique(referencia)
    escalas_regioes[[variavel]] <- list(
      quebras = c(-Inf, valor_unico, Inf),
      rotulos = c(
        paste0("<= ", round(valor_unico, 2)),
        paste0("> ", round(valor_unico, 2))
      )
    )
  } else {
    cortes <- unique(as.numeric(quantile(
      referencia,
      probs = seq(
        1 / numero_classes_mapa,
        (numero_classes_mapa - 1) / numero_classes_mapa,
        length.out = numero_classes_mapa - 1
      ),
      na.rm = TRUE,
      type = 7
    )))

    if(length(cortes) < (numero_classes_mapa - 1)){
      cortes <- seq(
        min(referencia, na.rm = TRUE),
        max(referencia, na.rm = TRUE),
        length.out = numero_classes_mapa + 1
      )[2:numero_classes_mapa]
    }

    cortes <- unique(cortes)
    limite_inf <- c(-Inf, cortes)
    limite_sup <- c(cortes, Inf)

    rotulos <- ifelse(
      is.infinite(limite_inf),
      paste0("<= ", round(limite_sup, 2)),
      ifelse(
        is.infinite(limite_sup),
        paste0("> ", round(limite_inf, 2)),
        paste0(round(limite_inf, 2), " - ", round(limite_sup, 2))
      )
    )

    escalas_regioes[[variavel]] <- list(
      quebras = c(-Inf, cortes, Inf),
      rotulos = rotulos
    )
  }
}

for(ano_ref in sort(unique(principal_regiao$year))){
  mapa_regiao <- left_join(
    GEO_REGSAUDE,
    filter(principal_regiao, year == ano_ref),
    by = "health_region_code"
  )

  for(variavel in variaveis){
    escala <- escalas_regioes[[variavel]]
    valores_mapa <- as.numeric(mapa_regiao[[variavel]])

    if(all(is.na(valores_mapa)) | is.null(escala)){
      mapa_regiao$classe_mapa <- factor(rep(NA_character_, length(valores_mapa)))
    } else {
      mapa_regiao$classe_mapa <- cut(
        valores_mapa,
        breaks = escala$quebras,
        labels = escala$rotulos,
        include.lowest = TRUE,
        ordered_result = TRUE
      )
    }

    grafico <- ggplot(mapa_regiao) +
      geom_sf(aes(fill = classe_mapa), size = .10, color = "white") +
      scale_fill_manual(values = paleta_roxa, na.value = "grey90", drop = FALSE) +
      theme_void(base_size = 10) +
      theme(
        legend.position = "right",
        legend.title = element_text(size = 8),
        legend.text = element_text(size = 7),
        plot.title = element_text(size = 11)
      ) +
      labs(
        title = paste(
          variavel,
          "- regiao de saude - residencia -",
          ano_ref
        ),
        fill = paste0(variavel, "\n7 categorias\nescala 2005")
      )

    ggsave(
      file.path(
        Endereco_mapas,
        paste0(variavel, "_health_region_residence_", ano_ref, ".png")
      ),
      plot = grafico,
      width = 20,
      height = 15,
      units = "cm",
      dpi = 300
    )
  }
}

###################################################################################################
######### PARTE 7 - GRAFICOS DAS METAS OMS #########

script_metas_oms <- file.path(Endereco_projeto, "Code", "02_who_goal_progress.R")

if(file.exists(script_metas_oms)){
  source(script_metas_oms, local = TRUE)
} else {
  texto_log <- "AVISO OMS: script 02_who_goal_progress.R nao encontrado."
  message(texto_log)
  cat(texto_log, "\n", file = arquivo_log, append = TRUE)
}

###################################################################################################
###################################################################################################
