###################################################################################################
###################################################################################################

#### INDICADORES MUNICIPAIS DE MORTALIDADE - SINASC E SIM ####

# Executar na raiz do projeto:
# Programs/R-4.6.1/bin/Rscript.exe "Mortality Index Project/Code/01_mortality_indicators.R"
#
# Todos os municipios encontrados nas variaveis municipais sao agregados.

pacotes <- c("data.table", "dplyr", "tidyr", "stringr", "ggplot2", "sf", "geobr", "readxl")
faltantes <- pacotes[!sapply(pacotes, requireNamespace, quietly = TRUE)]
if(length(faltantes) > 0) install.packages(faltantes, repos = "https://cloud.r-project.org")

library(data.table)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(sf)
library(geobr)
library(readxl)

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

anos_analise <- as.integer(str_split(Sys.getenv("ANALYSIS_YEARS", "2000,2005,2010,2015"),
                                     ",", simplify = TRUE))
arquivo_log <- file.path(Endereco_saida, "validation_log_R.txt")
writeLines("VALIDACAO - R", arquivo_log)

logar <- function(texto){
  message(texto)
  cat(texto, "\n", file = arquivo_log, append = TRUE)
}

codigo6 <- function(x){
  x <- str_trim(str_replace(as.character(x), "\\.0$", ""))
  x[!str_detect(x, "^\\d{6,7}$")] <- NA_character_
  x <- str_sub(x, 1, 6)
  x[str_ends(x, "0000")] <- NA_character_
  x
}

quebras_mapa <- function(valor_referencia, n_classes = numero_classes_mapa){
  # Usa os valores de 2005 como escala fixa para todos os anos.
  # Assim, a cor de 2010/2015 e comparavel diretamente com 2005.
  ref <- as.numeric(valor_referencia)
  ref <- ref[!is.na(ref)]
  if(length(ref) == 0) return(NULL)
  if(length(unique(ref)) == 1){
    return(list(quebras = c(-Inf, unique(ref), Inf),
                rotulos = c(paste0("<= ", round(unique(ref), 2)),
                            paste0("> ", round(unique(ref), 2)))))
  }
  cortes <- unique(as.numeric(quantile(ref, probs = seq(1 / n_classes,
                                                        (n_classes - 1) / n_classes,
                                                        length.out = n_classes - 1),
                                       na.rm = TRUE, type = 7)))
  if(length(cortes) < (n_classes - 1)){
    cortes <- seq(min(ref, na.rm = TRUE), max(ref, na.rm = TRUE),
                  length.out = n_classes + 1)[2:n_classes]
  }
  cortes <- unique(cortes)
  quebras <- c(-Inf, cortes, Inf)
  limite_inf <- c(-Inf, cortes)
  limite_sup <- c(cortes, Inf)
  rotulos <- ifelse(
    is.infinite(limite_inf),
    paste0("<= ", round(limite_sup, 2)),
    ifelse(is.infinite(limite_sup),
           paste0("> ", round(limite_inf, 2)),
           paste0(round(limite_inf, 2), " - ", round(limite_sup, 2)))
  )
  list(quebras = quebras, rotulos = rotulos)
}

categorias_mapa <- function(valor, escala){
  # Aplica a escala fixa calculada com 2005.
  x <- as.numeric(valor)
  if(all(is.na(x))) return(factor(rep(NA_character_, length(x))))
  if(is.null(escala)) return(factor(rep(NA_character_, length(x))))
  cut(x, breaks = escala$quebras, labels = escala$rotulos,
      include.lowest = TRUE, ordered_result = TRUE)
}

tema_mapa <- function(titulo, legenda){
  list(
    labs(title = titulo, fill = legenda),
    theme_void(base_size = 10),
    theme(
      legend.position = "right",
      legend.title = element_text(size = 8),
      legend.text = element_text(size = 7),
      plot.title = element_text(size = 11)
    )
  )
}

ano_arquivo <- function(x){
  as.integer(str_match(basename(x), "_(\\d{4})_t")[,2])
}

localizar_arquivos <- function(pasta, prefixo){
  arquivos <- list.files(pasta, recursive = TRUE, full.names = TRUE,
                         pattern = "\\.(csv|txt|dbf|dbc|parquet|rds)$",
                         ignore.case = TRUE)
  anos <- ano_arquivo(arquivos)
  arquivos[str_starts(basename(arquivos), prefixo) & anos %in% anos_analise]
}

ler_base <- function(arquivo, colunas = NULL){
  extensao <- tolower(tools::file_ext(arquivo))
  if(extensao %in% c("csv", "txt")){
    cabecalho <- names(fread(arquivo, nrows = 0, encoding = "UTF-8"))
    selecionar <- if(is.null(colunas)) NULL else intersect(colunas, cabecalho)
    return(fread(arquivo, select = selecionar, colClasses = "character",
                 encoding = "UTF-8", showProgress = FALSE))
  }
  if(extensao == "parquet"){
    if(!requireNamespace("arrow", quietly = TRUE)) install.packages("arrow")
    base <- arrow::read_parquet(arquivo, col_select = colunas)
    return(as.data.table(base))
  }
  if(extensao == "rds"){
    base <- as.data.table(readRDS(arquivo))
    if(!is.null(colunas)) base <- base[, intersect(colunas, names(base)), with = FALSE]
    return(base)
  }
  if(extensao == "dbf"){
    base <- as.data.table(foreign::read.dbf(arquivo, as.is = TRUE))
    if(!is.null(colunas)) base <- base[, intersect(colunas, names(base)), with = FALSE]
    return(base)
  }
  if(extensao == "dbc"){
    if(!requireNamespace("read.dbc", quietly = TRUE)) install.packages("read.dbc")
    base <- as.data.table(read.dbc::read.dbc(arquivo))
    if(!is.null(colunas)) base <- base[, intersect(colunas, names(base)), with = FALSE]
    return(base)
  }
  stop(paste("Formato nao reconhecido:", arquivo))
}

arquivos_sinasc <- localizar_arquivos(file.path(Endereco_raw, "ETLSINASC"), "ETLSINASC")
arquivos_sim <- localizar_arquivos(file.path(Endereco_raw, "ETLSIM"), "ETLSIM")
if(length(arquivos_sinasc) == 0 | length(arquivos_sim) == 0) stop("Bases SINASC/SIM ausentes.")
logar(paste("Arquivos SINASC:", length(arquivos_sinasc), "- arquivos SIM:", length(arquivos_sim)))

######### NASCIDOS VIVOS #########

resumo_nascimentos <- list()
contador <- 1
for(arquivo in arquivos_sinasc){
  colunas <- c("CODMUNRES", "CODMUNNASC", "ano_nasc",
               "res_codigo_adotado", "nasc_codigo_adotado",
               "res_MUNNOME", "nasc_MUNNOME")
  base <- ler_base(arquivo, colunas)
  if(!all(c("CODMUNRES", "CODMUNNASC") %in% names(base))){
    stop(paste("Colunas municipais SINASC ausentes:", basename(arquivo)))
  }
  base[, ano := if("ano_nasc" %in% names(base)) as.integer(ano_nasc) else ano_arquivo(arquivo)]
  logar(paste("SINASC", basename(arquivo), ":", nrow(base), "observacoes"))
  
  for(tipo in c("residence", "occurrence")){
    coluna_codigo <- if(tipo == "residence") "CODMUNRES" else "CODMUNNASC"
    coluna_adotado <- if(tipo == "residence") "res_codigo_adotado" else "nasc_codigo_adotado"
    coluna_nome <- if(tipo == "residence") "res_MUNNOME" else "nasc_MUNNOME"
    codigo_municipio <- if(coluna_adotado %in% names(base)){
      coalesce(base[[coluna_adotado]], base[[coluna_codigo]])
    } else base[[coluna_codigo]]
    base_tipo <- data.table(
      municipality_code = codigo6(codigo_municipio),
      municipality_name = if(coluna_nome %in% names(base)) base[[coluna_nome]] else NA_character_,
      year = base$ano,
      measure_type = tipo
    )
    base_tipo <- base_tipo[!is.na(municipality_code) & !is.na(year)]
    resumo_nascimentos[[contador]] <- base_tipo[, .(
      live_births = .N,
      municipality_name = first(na.omit(municipality_name))
    ), by = .(municipality_code, year, measure_type)]
    contador <- contador + 1
  }
  rm(base)
  gc()
}

nascimentos <- rbindlist(resumo_nascimentos, fill = TRUE)[, .(
  live_births = sum(live_births),
  municipality_name = first(na.omit(municipality_name))
), by = .(municipality_code, year, measure_type)]

######### OBITOS #########

# IDADE conforme dict_SIM.csv:
# 0 = minutos; 1 = horas; 2 = dias; 3 = meses; 4 = anos.
# 400 = menor de um ano, mas unidade exata desconhecida.
classificar_idade <- function(idade){
  idade <- suppressWarnings(as.numeric(idade))
  unidade <- floor(idade / 100)
  quantidade <- idade %% 100
  minutos_conhecidos <- unidade == 0 & idade != 0
  neonatal <- minutos_conhecidos | unidade == 1 | (unidade == 2 & quantidade <= 27)
  infantil <- minutos_conhecidos | unidade %in% c(1,2,3) | idade == 400
  menor5 <- infantil | (unidade == 4 & quantidade < 5)
  list(neonatal = neonatal %in% TRUE, infantil = infantil %in% TRUE, menor5 = menor5 %in% TRUE)
}

resumo_obitos <- list()
contador <- 1
for(arquivo in arquivos_sim){
  colunas <- c("TIPOBITO", "CODMUNRES", "CODMUNOCOR", "ano_obito", "IDADE",
               "SEXO", "CAUSABAS", "res_codigo_adotado", "ocor_codigo_adotado",
               "res_MUNNOME", "ocor_MUNNOME")
  base <- ler_base(arquivo, colunas)
  obrigatorias <- c("TIPOBITO", "CODMUNRES", "CODMUNOCOR", "IDADE", "SEXO", "CAUSABAS")
  if(!all(obrigatorias %in% names(base))) stop(paste("Colunas SIM ausentes:", basename(arquivo)))
  base <- base[as.integer(TIPOBITO) == 2] # exclui obito fetal
  base[, ano := if("ano_obito" %in% names(base)) as.integer(ano_obito) else ano_arquivo(arquivo)]
  idade <- classificar_idade(base$IDADE)
  base[, neonatal_deaths := as.integer(idade$neonatal)]
  base[, infant_deaths := as.integer(idade$infantil)]
  base[, under5_deaths := as.integer(idade$menor5)]
  
  # Assuncao: morte materna = mulher com causa basica CID-10 O00-O95 ou O98-O99.
  # O96/O97 (tardia/sequela) nao entram na razao padrao. O codebook documenta
  # CAUSABAS, OBITOGRAV e OBITOPUERP, mas nao define o indicador agregado.
  cid <- str_replace_all(str_to_upper(base$CAUSABAS), "[^A-Z0-9]", "")
  cid_numero <- suppressWarnings(as.integer(str_match(cid, "^O(\\d{2})")[,2]))
  base[, maternal_deaths := as.integer(as.integer(SEXO) == 2 &
         ((cid_numero >= 0 & cid_numero <= 95) | (cid_numero >= 98 & cid_numero <= 99)))]
  base[is.na(maternal_deaths), maternal_deaths := 0]
  logar(paste("SIM", basename(arquivo), ":", nrow(base), "obitos nao fetais"))
  
  for(tipo in c("residence", "occurrence")){
    coluna_codigo <- if(tipo == "residence") "CODMUNRES" else "CODMUNOCOR"
    coluna_adotado <- if(tipo == "residence") "res_codigo_adotado" else "ocor_codigo_adotado"
    coluna_nome <- if(tipo == "residence") "res_MUNNOME" else "ocor_MUNNOME"
    codigo_municipio <- if(coluna_adotado %in% names(base)){
      coalesce(base[[coluna_adotado]], base[[coluna_codigo]])
    } else base[[coluna_codigo]]
    base_tipo <- data.table(
      municipality_code = codigo6(codigo_municipio),
      municipality_name = if(coluna_nome %in% names(base)) base[[coluna_nome]] else NA_character_,
      year = base$ano, measure_type = tipo,
      neonatal_deaths = base$neonatal_deaths, infant_deaths = base$infant_deaths,
      under5_deaths = base$under5_deaths, maternal_deaths = base$maternal_deaths
    )
    base_tipo <- base_tipo[!is.na(municipality_code) & !is.na(year)]
    resumo_obitos[[contador]] <- base_tipo[, .(
      neonatal_deaths = sum(neonatal_deaths),
      infant_deaths = sum(infant_deaths),
      under5_deaths = sum(under5_deaths),
      maternal_deaths = sum(maternal_deaths),
      municipality_name = first(na.omit(municipality_name))
    ), by = .(municipality_code, year, measure_type)]
    contador <- contador + 1
  }
  rm(base)
  gc()
}

obitos <- rbindlist(resumo_obitos, fill = TRUE)[, .(
  neonatal_deaths = sum(neonatal_deaths),
  infant_deaths = sum(infant_deaths),
  under5_deaths = sum(under5_deaths),
  maternal_deaths = sum(maternal_deaths),
  municipality_name = first(na.omit(municipality_name))
), by = .(municipality_code, year, measure_type)]

######### INDICADORES #########

indicadores <- full_join(nascimentos, obitos,
                         by = c("municipality_code", "year", "measure_type"),
                         suffix = c("_birth", "_death")) %>%
  mutate(
    municipality_name = coalesce(municipality_name_birth, municipality_name_death),
    across(c(live_births, neonatal_deaths, infant_deaths,
             under5_deaths, maternal_deaths), ~replace_na(as.integer(.x), 0L)),
    neonatal_per_1000 = if_else(live_births > 0, 1000*neonatal_deaths/live_births, NA_real_),
    infant_per_1000 = if_else(live_births > 0, 1000*infant_deaths/live_births, NA_real_),
    under5_per_1000 = if_else(live_births > 0, 1000*under5_deaths/live_births, NA_real_),
    maternal_per_100000 = if_else(live_births > 0, 100000*maternal_deaths/live_births, NA_real_)
  ) %>%
  select(municipality_code, municipality_name, year, measure_type,
         live_births, neonatal_deaths, infant_deaths, under5_deaths, maternal_deaths,
         neonatal_per_1000, infant_per_1000, under5_per_1000, maternal_per_100000) %>%
  arrange(municipality_code, year, measure_type)

fwrite(nascimentos, file.path(Endereco_saida, "sinasc_births_municipality_year_R.csv"))
fwrite(obitos, file.path(Endereco_saida, "sim_deaths_municipality_year_R.csv"))
fwrite(indicadores, file.path(Endereco_saida, "mortality_indicators_municipality_year_R.csv"))

logar(paste("Anos SINASC:", min(nascimentos$year), "-", max(nascimentos$year)))
logar(paste("Anos SIM:", min(obitos$year), "-", max(obitos$year)))
logar(paste("Municipios na base final:", n_distinct(indicadores$municipality_code)))
logar(paste("Municipio/ano/tipo com obitos infantis > nascidos vivos:",
            sum(indicadores$infant_deaths > indicadores$live_births)))
logar(paste("Ausentes - codigo:", sum(is.na(indicadores$municipality_code)),
            "- ano:", sum(is.na(indicadores$year))))

######### REGIOES DE SAUDE #########

# Os arquivos XLSX fornecidos guardam linhas CSV em uma unica coluna.
# tb_ibge possui exatamente os 5.570 municipios atuais e e usado como crosswalk.
arquivo_crosswalk <- file.path(Endereco_raw, "Health Regions BR", "tb_ibge.xlsx")
arquivo_regioes <- file.path(Endereco_raw, "Health Regions BR", "tb_regiao_saude.xlsx")
if(!file.exists(arquivo_crosswalk) | !file.exists(arquivo_regioes)){
  stop("Crosswalk ou geometria das regioes de saude nao encontrados.")
}

texto_crosswalk <- readxl::read_excel(arquivo_crosswalk, col_names = FALSE)[[1]]
crosswalk <- fread(text = paste(texto_crosswalk, collapse = "\n"),
                   colClasses = "character", encoding = "UTF-8")
crosswalk <- crosswalk[, .(
  municipality_code = codigo6(ibge),
  health_region_code = str_pad(co_regiao_saude, 5, pad = "0"),
  municipality_name_crosswalk = no_cidade
)]
crosswalk <- unique(crosswalk[!is.na(municipality_code) & health_region_code != "00000"])
logar(paste("Municipios no crosswalk de regioes de saude:", nrow(crosswalk)))
logar(paste("Regioes de saude no crosswalk:", uniqueN(crosswalk$health_region_code)))

# Completa o painel para manter os 5.570 municipios em todos os anos/tipos,
# inclusive quando nao ha nascimento nem obito registrado.
grade_completa <- tidyr::expand_grid(
  municipality_code = crosswalk$municipality_code,
  year = anos_analise,
  measure_type = c("residence", "occurrence")
)
indicadores <- grade_completa %>%
  left_join(indicadores, by = c("municipality_code", "year", "measure_type")) %>%
  left_join(select(crosswalk, municipality_code, municipality_name_crosswalk),
            by = "municipality_code") %>%
  mutate(
    municipality_name = coalesce(municipality_name, municipality_name_crosswalk),
    across(c(live_births, neonatal_deaths, infant_deaths,
             under5_deaths, maternal_deaths), ~replace_na(as.integer(.x), 0L)),
    neonatal_per_1000 = if_else(live_births > 0, 1000*neonatal_deaths/live_births, NA_real_),
    infant_per_1000 = if_else(live_births > 0, 1000*infant_deaths/live_births, NA_real_),
    under5_per_1000 = if_else(live_births > 0, 1000*under5_deaths/live_births, NA_real_),
    maternal_per_100000 = if_else(live_births > 0, 100000*maternal_deaths/live_births, NA_real_)
  ) %>%
  select(-municipality_name_crosswalk) %>%
  arrange(municipality_code, year, measure_type)

fwrite(indicadores, file.path(Endereco_saida, "mortality_indicators_municipality_year_R.csv"))
logar(paste("Linhas no painel municipal completo:", nrow(indicadores)))

indicadores_regiao <- left_join(indicadores, crosswalk, by = "municipality_code")
sem_regiao <- indicadores_regiao %>%
  filter(is.na(health_region_code)) %>%
  distinct(municipality_code)
logar(paste("Municipios da base final sem regiao de saude:", nrow(sem_regiao)))

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
    neonatal_per_1000 = if_else(live_births > 0, 1000*neonatal_deaths/live_births, NA_real_),
    infant_per_1000 = if_else(live_births > 0, 1000*infant_deaths/live_births, NA_real_),
    under5_per_1000 = if_else(live_births > 0, 1000*under5_deaths/live_births, NA_real_),
    maternal_per_100000 = if_else(live_births > 0, 100000*maternal_deaths/live_births, NA_real_)
  ) %>%
  arrange(health_region_code, year, measure_type)

fwrite(indicadores_regiao,
       file.path(Endereco_saida, "mortality_indicators_health_region_year_R.csv"))
######### MAPAS #########

Endereco_shape <- file.path(Endereco_dados, "Shapefiles", "municipios_2020")
dir.create(Endereco_shape, recursive = TRUE, showWarnings = FALSE)
arquivo_shape <- file.path(Endereco_shape, "municipios_2020.shp")

if(!file.exists(arquivo_shape)){
  logar("Baixando malha municipal de 2020 com geobr.")
  GEO_MUNIC <- geobr::read_municipality(code_muni = "all", year = 2020,
                                        simplified = TRUE, showProgress = FALSE)
  st_write(GEO_MUNIC, arquivo_shape, delete_layer = TRUE, quiet = TRUE)
  logar(paste("Shapefile salvo em:", arquivo_shape))
}

if(file.exists(arquivo_shape)){
  GEO_MUNIC <- st_read(arquivo_shape, quiet = TRUE)
  opcoes_codigo <- c("code_muni", "code_mn", "CODMUN", "CD_MUN", "CD_MUN7", "GEOCODIGO", "IBGE")
  coluna_shape <- intersect(names(GEO_MUNIC), opcoes_codigo)[1]
  if(is.na(coluna_shape)){
    logar("AVISO MAPA: coluna municipal nao identificada no shapefile geobr.")
  } else {
    GEO_MUNIC$municipality_code <- codigo6(GEO_MUNIC[[coluna_shape]])
    principal <- indicadores %>% filter(measure_type == "residence")
    sem_shape <- setdiff(unique(principal$municipality_code), unique(GEO_MUNIC$municipality_code))
    logar(paste("Municipios sem correspondencia no shapefile:", length(sem_shape)))
    
    variaveis <- c("neonatal_per_1000", "infant_per_1000",
                   "under5_per_1000", "maternal_per_100000")
    escalas_municipios <- lapply(
      variaveis,
      function(v) quebras_mapa(principal %>%
                                 filter(year == ano_referencia_mapa) %>%
                                 pull(.data[[v]]))
    )
    names(escalas_municipios) <- variaveis
    for(ano_ref in sort(unique(principal$year))){
      mapa_base <- left_join(GEO_MUNIC, filter(principal, year == ano_ref),
                             by = "municipality_code")
      for(variavel in variaveis){
        mapa_base$classe_mapa <- categorias_mapa(mapa_base[[variavel]],
                                                 escalas_municipios[[variavel]])
        grafico <- ggplot(mapa_base) +
          geom_sf(aes(fill = classe_mapa), size = .15, color = "white") +
          scale_fill_manual(values = paleta_roxa, na.value = "grey90", drop = FALSE) +
          tema_mapa(paste(variavel, "- residencia -", ano_ref),
                    paste0(variavel, "\n7 categorias\nescala 2005"))
        ggsave(file.path(Endereco_mapas, paste0(variavel, "_residence_", ano_ref, ".png")),
               plot = grafico, width = 20, height = 15, units = "cm", dpi = 300)
      }
    }
  }
} else {
  logar("AVISO MAPA: shapefile municipal nao foi criado.")
}

######### MAPAS - REGIOES DE SAUDE #########

# A geometria vem como WKB hexadecimal, dividida em varias celulas do XLSX.
texto_regioes <- readxl::read_excel(arquivo_regioes, col_names = FALSE)[[1]]
inicio_registro <- which(str_detect(texto_regioes, "^[0-9]{5},"))
fim_registro <- c(inicio_registro[-1] - 1, length(texto_regioes))
registros <- mapply(
  function(inicio, fim) paste0(texto_regioes[inicio:fim], collapse = ""),
  inicio_registro, fim_registro, USE.NAMES = FALSE
)
regioes_tab <- fread(text = paste(c(texto_regioes[1], registros), collapse = "\n"),
                     encoding = "UTF-8")

hex_para_raw <- function(hexadecimal){
  as.raw(strtoi(substring(hexadecimal,
                          seq(1, nchar(hexadecimal), 2),
                          seq(2, nchar(hexadecimal), 2)), 16L))
}

geometria_regiao <- structure(lapply(regioes_tab$limite_geografico, hex_para_raw),
                              class = "WKB")
GEO_REGSAUDE <- st_sf(
  health_region_code = str_pad(as.character(regioes_tab$co_regiao_saude), 5, pad = "0"),
  health_region_name = regioes_tab$no_regiao_saude,
  geometry = st_as_sfc(geometria_regiao, EWKB = TRUE)
)

Endereco_shape_regiao <- file.path(Endereco_dados, "Shapefiles", "regioes_saude")
dir.create(Endereco_shape_regiao, recursive = TRUE, showWarnings = FALSE)
st_write(GEO_REGSAUDE, file.path(Endereco_shape_regiao, "regioes_saude.shp"),
         delete_layer = TRUE, quiet = TRUE)
logar(paste("Regioes na geometria:", nrow(GEO_REGSAUDE)))

principal_regiao <- indicadores_regiao %>% filter(measure_type == "residence")
sem_shape_regiao <- setdiff(unique(principal_regiao$health_region_code),
                            unique(GEO_REGSAUDE$health_region_code))
logar(paste("Regioes de saude sem correspondencia na geometria:",
            length(sem_shape_regiao)))

escalas_regioes <- lapply(
  variaveis,
  function(v) quebras_mapa(principal_regiao %>%
                             filter(year == ano_referencia_mapa) %>%
                             pull(.data[[v]]))
)
names(escalas_regioes) <- variaveis

for(ano_ref in sort(unique(principal_regiao$year))){
  mapa_regiao <- left_join(GEO_REGSAUDE,
                           filter(principal_regiao, year == ano_ref),
                           by = "health_region_code")
  for(variavel in variaveis){
    mapa_regiao$classe_mapa <- categorias_mapa(mapa_regiao[[variavel]],
                                               escalas_regioes[[variavel]])
    grafico <- ggplot(mapa_regiao) +
      geom_sf(aes(fill = classe_mapa), size = .10, color = "white") +
      scale_fill_manual(values = paleta_roxa, na.value = "grey90", drop = FALSE) +
      tema_mapa(paste(variavel, "- regiao de saude - residencia -", ano_ref),
                paste0(variavel, "\n7 categorias\nescala 2005"))
    ggsave(file.path(Endereco_mapas,
                     paste0(variavel, "_health_region_residence_", ano_ref, ".png")),
           plot = grafico, width = 20, height = 15, units = "cm", dpi = 300)
  }
}

######### GRAFICOS DAS METAS OMS #########

script_metas_oms <- file.path(Endereco_projeto, "Code", "02_who_goal_progress.R")
if(file.exists(script_metas_oms)){
  source(script_metas_oms, local = TRUE)
} else {
  logar("AVISO OMS: script 02_who_goal_progress.R nao encontrado.")
}
