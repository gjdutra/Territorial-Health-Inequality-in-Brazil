###################################################################################################
###################################################################################################

#### GRAFICOS DAS METAS OMS - MORTALIDADE MATERNA, NEONATAL E MENOR DE 5 ANOS ####

## Este script e chamado no final do script 01_mortality_indicators.R.
## Ele apresenta cada etapa diretamente, sem funcoes auxiliares.
##
## Definicao operacional:
## - Meta atingida: indicador mais recente no valor da meta ou abaixo.
## - No caminho: tendencia linear projeta cumprimento da meta ate 2030.
## - Fora do caminho: projecao permanece acima da meta em 2030.
## - Sem dados suficientes: indicador recente ausente ou menos de dois anos validos.

######### PACOTES #########

pacotes <- c("data.table", "dplyr", "tidyr", "stringr", "ggplot2", "sf")
faltantes <- pacotes[!sapply(pacotes, requireNamespace, quietly = TRUE)]
if(length(faltantes) > 0) install.packages(faltantes, repos = "https://cloud.r-project.org")

library(data.table)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(sf)

######### ENDERECOS E PARAMETROS #########

Endereco_projeto <- "Mortality Index Project"
Endereco_dados <- file.path(Endereco_projeto, "Data")
Endereco_saida <- file.path(Endereco_projeto, "Output")
Endereco_mapas <- file.path(Endereco_saida, "Maps")
Endereco_oms <- file.path(Endereco_saida, "WHO_Goals")

dir.create(Endereco_oms, recursive = TRUE, showWarnings = FALSE)
dir.create(Endereco_mapas, recursive = TRUE, showWarnings = FALSE)

ano_meta <- 2030L

writeLines(
  c(
    "WHO goal progress outputs",
    "",
    "Targets used:",
    "- Maternal mortality: <= 70 per 100,000 live births.",
    "- Neonatal mortality: <= 12 per 1,000 live births.",
    "- Under-5 mortality: <= 25 per 1,000 live births.",
    "",
    "Classification:",
    "- Meta atingida: latest-year indicator is already at or below the target.",
    "- No caminho: latest-year indicator is above the target, but a linear trend across available years projects target achievement by 2030.",
    "- Fora do caminho: latest-year indicator is above the target and the projected 2030 value remains above the target.",
    "- Sem dados suficientes: missing latest-year value/live births or fewer than two valid years for the trend.",
    "",
    "The script uses residence-based indicators and automatically selects the most recent year in the final indicator datasets."
  ),
  file.path(Endereco_oms, "README_who_goal_method.txt")
)

metas_oms <- data.table(
  indicador = c(
    "maternal_per_100000",
    "neonatal_per_1000",
    "under5_per_1000"
  ),
  nome_indicador = c(
    "Maternal mortality",
    "Neonatal mortality",
    "Under-5 mortality"
  ),
  meta_oms = c(70, 12, 25),
  unidade = c(
    "per 100,000 live births",
    "per 1,000 live births",
    "per 1,000 live births"
  )
)

cores_status <- c(
  "Meta atingida" = "#009E73",
  "No caminho" = "#0072B2",
  "Fora do caminho" = "#D55E00",
  "Sem dados suficientes" = "grey80"
)

arquivo_municipios <- file.path(
  Endereco_saida,
  "mortality_indicators_municipality_year_R.csv"
)

arquivo_regioes <- file.path(
  Endereco_saida,
  "mortality_indicators_health_region_year_R.csv"
)

if(!file.exists(arquivo_municipios) | !file.exists(arquivo_regioes)){
  stop("Bases finais de indicadores nao encontradas no Output/. Execute o script 01 primeiro.")
}

###################################################################################################
######### PARTE 1 - AVALIAR AS METAS NOS MUNICIPIOS #########

indicadores_municipio <- fread(
  arquivo_municipios,
  colClasses = list(character = c("municipality_code", "measure_type"))
)

codigo_municipio <- str_trim(
  str_replace(as.character(indicadores_municipio$municipality_code), "\\.0$", "")
)
codigo_municipio[!str_detect(codigo_municipio, "^\\d{6,7}$")] <- NA_character_
codigo_municipio <- str_sub(codigo_municipio, 1, 6)
codigo_municipio[str_ends(codigo_municipio, "0000")] <- NA_character_
indicadores_municipio$municipality_code <- codigo_municipio

indicadores_municipio <- indicadores_municipio[measure_type == "residence"]
ano_recente_municipio <- max(indicadores_municipio$year, na.rm = TRUE)

municipio_longo <- melt(
  indicadores_municipio,
  id.vars = c(
    "municipality_code",
    "municipality_name",
    "year",
    "measure_type",
    "live_births"
  ),
  measure.vars = metas_oms$indicador,
  variable.name = "indicador",
  value.name = "valor"
)

municipio_longo <- merge(
  municipio_longo,
  metas_oms,
  by = "indicador",
  all.x = TRUE
)

status_municipio <- municipio_longo[, {
  dados_validos <- .SD[!is.na(valor) & !is.na(year)]
  ultimo <- dados_validos[year == ano_recente_municipio]

  valor_recente <- if(nrow(ultimo) > 0) ultimo$valor[1] else NA_real_
  nascidos_recente <- if(nrow(ultimo) > 0) ultimo$live_births[1] else NA_real_
  anos_validos <- uniqueN(dados_validos$year)

  inclinacao <- NA_real_
  projecao_2030 <- NA_real_

  if(anos_validos >= 2){
    modelo <- lm(valor ~ year, data = dados_validos)
    inclinacao <- unname(coef(modelo)[["year"]])
    projecao_2030 <- max(
      0,
      unname(predict(modelo, newdata = data.frame(year = ano_meta)))
    )
  }

  if(is.na(valor_recente) | is.na(nascidos_recente) | nascidos_recente == 0){
    status <- "Sem dados suficientes"
  } else if(valor_recente <= meta_oms[1]){
    status <- "Meta atingida"
  } else if(
    anos_validos >= 2 &
      !is.na(projecao_2030) &
      projecao_2030 <= meta_oms[1] &
      inclinacao < 0
  ){
    status <- "No caminho"
  } else if(anos_validos < 2){
    status <- "Sem dados suficientes"
  } else {
    status <- "Fora do caminho"
  }

  .(
    unidade_geografica = "municipality",
    nome_indicador = nome_indicador[1],
    unidade_indicador = unidade[1],
    meta_oms = meta_oms[1],
    ano_recente = ano_recente_municipio,
    valor_recente = valor_recente,
    nascidos_vivos_recente = nascidos_recente,
    anos_validos = anos_validos,
    inclinacao_anual = inclinacao,
    projecao_2030 = projecao_2030,
    status_oms = status
  )
}, by = c("municipality_code", "municipality_name", "indicador")]

setnames(
  status_municipio,
  c("municipality_code", "municipality_name"),
  c("codigo", "nome")
)

status_municipio$status_oms <- factor(
  status_municipio$status_oms,
  levels = names(cores_status)
)

fwrite(
  status_municipio,
  file.path(Endereco_oms, "municipality_who_goal_status.csv")
)

###################################################################################################
######### PARTE 2 - RESUMO DAS METAS NOS MUNICIPIOS #########

resumo_municipio <- status_municipio %>%
  count(nome_indicador, status_oms, name = "numero_unidades") %>%
  group_by(nome_indicador) %>%
  mutate(percentual = 100 * numero_unidades / sum(numero_unidades)) %>%
  ungroup()

fwrite(
  resumo_municipio,
  file.path(Endereco_oms, "municipality_who_goal_summary.csv")
)

grafico <- ggplot(
  resumo_municipio,
  aes(x = nome_indicador, y = percentual, fill = status_oms)
) +
  geom_col(color = "white") +
  coord_flip() +
  scale_fill_manual(values = cores_status, drop = FALSE) +
  theme_minimal(base_size = 11) +
  labs(
    title = "WHO goal progress summary - municipalities",
    x = "",
    y = "Percent of units",
    fill = "WHO status"
  )

ggsave(
  file.path(Endereco_mapas, "municipality_who_goal_summary.png"),
  plot = grafico,
  width = 20,
  height = 12,
  units = "cm",
  dpi = 300
)

###################################################################################################
######### PARTE 3 - MAPAS DAS METAS NOS MUNICIPIOS #########

arquivo_shape_municipio <- file.path(
  Endereco_dados,
  "Shapefiles",
  "municipios_2020",
  "municipios_2020.shp"
)

if(file.exists(arquivo_shape_municipio)){

  GEO_MUNIC <- st_read(arquivo_shape_municipio, quiet = TRUE)

  opcoes_codigo <- c(
    "code_muni", "code_mn", "CODMUN", "CD_MUN",
    "CD_MUN7", "GEOCODIGO", "IBGE"
  )
  coluna_shape <- intersect(names(GEO_MUNIC), opcoes_codigo)[1]

  if(!is.na(coluna_shape)){

    codigo_municipio <- str_trim(
      str_replace(as.character(GEO_MUNIC[[coluna_shape]]), "\\.0$", "")
    )
    codigo_municipio[!str_detect(codigo_municipio, "^\\d{6,7}$")] <- NA_character_
    codigo_municipio <- str_sub(codigo_municipio, 1, 6)
    codigo_municipio[str_ends(codigo_municipio, "0000")] <- NA_character_
    GEO_MUNIC$codigo <- codigo_municipio

    indicadores_mapa <- unique(status_municipio$indicador)

    for(ind_ref in indicadores_mapa){

      dados_mapa <- status_municipio %>% filter(indicador == ind_ref)
      mapa <- left_join(GEO_MUNIC, dados_mapa, by = "codigo")

      titulo <- paste0(
        unique(dados_mapa$nome_indicador),
        " - WHO goal progress - municipalities",
        " - latest year: ",
        unique(dados_mapa$ano_recente)
      )

      grafico <- ggplot(mapa) +
        geom_sf(aes(fill = status_oms), color = "white", size = .10) +
        scale_fill_manual(
          values = cores_status,
          drop = FALSE,
          na.value = "grey90"
        ) +
        theme_void(base_size = 10) +
        theme(
          legend.position = "right",
          legend.title = element_text(size = 8),
          legend.text = element_text(size = 8),
          plot.title = element_text(size = 11)
        ) +
        labs(title = titulo, fill = "WHO status")

      ggsave(
        file.path(
          Endereco_mapas,
          paste0("municipality_who_goal_", ind_ref, ".png")
        ),
        plot = grafico,
        width = 20,
        height = 15,
        units = "cm",
        dpi = 300
      )
    }
  }
}

###################################################################################################
######### PARTE 4 - AVALIAR AS METAS NAS REGIOES DE SAUDE #########

indicadores_regiao <- fread(
  arquivo_regioes,
  colClasses = list(character = c("health_region_code", "measure_type"))
)

indicadores_regiao$health_region_code <- str_pad(
  str_replace(
    as.character(indicadores_regiao$health_region_code),
    "\\.0$",
    ""
  ),
  5,
  pad = "0"
)

indicadores_regiao$health_region_name <- indicadores_regiao$health_region_code
indicadores_regiao <- indicadores_regiao[measure_type == "residence"]
ano_recente_regiao <- max(indicadores_regiao$year, na.rm = TRUE)

regiao_longa <- melt(
  indicadores_regiao,
  id.vars = c(
    "health_region_code",
    "health_region_name",
    "year",
    "measure_type",
    "live_births"
  ),
  measure.vars = metas_oms$indicador,
  variable.name = "indicador",
  value.name = "valor"
)

regiao_longa <- merge(
  regiao_longa,
  metas_oms,
  by = "indicador",
  all.x = TRUE
)

status_regiao <- regiao_longa[, {
  dados_validos <- .SD[!is.na(valor) & !is.na(year)]
  ultimo <- dados_validos[year == ano_recente_regiao]

  valor_recente <- if(nrow(ultimo) > 0) ultimo$valor[1] else NA_real_
  nascidos_recente <- if(nrow(ultimo) > 0) ultimo$live_births[1] else NA_real_
  anos_validos <- uniqueN(dados_validos$year)

  inclinacao <- NA_real_
  projecao_2030 <- NA_real_

  if(anos_validos >= 2){
    modelo <- lm(valor ~ year, data = dados_validos)
    inclinacao <- unname(coef(modelo)[["year"]])
    projecao_2030 <- max(
      0,
      unname(predict(modelo, newdata = data.frame(year = ano_meta)))
    )
  }

  if(is.na(valor_recente) | is.na(nascidos_recente) | nascidos_recente == 0){
    status <- "Sem dados suficientes"
  } else if(valor_recente <= meta_oms[1]){
    status <- "Meta atingida"
  } else if(
    anos_validos >= 2 &
      !is.na(projecao_2030) &
      projecao_2030 <= meta_oms[1] &
      inclinacao < 0
  ){
    status <- "No caminho"
  } else if(anos_validos < 2){
    status <- "Sem dados suficientes"
  } else {
    status <- "Fora do caminho"
  }

  .(
    unidade_geografica = "health_region",
    nome_indicador = nome_indicador[1],
    unidade_indicador = unidade[1],
    meta_oms = meta_oms[1],
    ano_recente = ano_recente_regiao,
    valor_recente = valor_recente,
    nascidos_vivos_recente = nascidos_recente,
    anos_validos = anos_validos,
    inclinacao_anual = inclinacao,
    projecao_2030 = projecao_2030,
    status_oms = status
  )
}, by = c("health_region_code", "health_region_name", "indicador")]

setnames(
  status_regiao,
  c("health_region_code", "health_region_name"),
  c("codigo", "nome")
)

status_regiao$status_oms <- factor(
  status_regiao$status_oms,
  levels = names(cores_status)
)

fwrite(
  status_regiao,
  file.path(Endereco_oms, "health_region_who_goal_status.csv")
)

###################################################################################################
######### PARTE 5 - RESUMO DAS METAS NAS REGIOES DE SAUDE #########

resumo_regiao <- status_regiao %>%
  count(nome_indicador, status_oms, name = "numero_unidades") %>%
  group_by(nome_indicador) %>%
  mutate(percentual = 100 * numero_unidades / sum(numero_unidades)) %>%
  ungroup()

fwrite(
  resumo_regiao,
  file.path(Endereco_oms, "health_region_who_goal_summary.csv")
)

grafico <- ggplot(
  resumo_regiao,
  aes(x = nome_indicador, y = percentual, fill = status_oms)
) +
  geom_col(color = "white") +
  coord_flip() +
  scale_fill_manual(values = cores_status, drop = FALSE) +
  theme_minimal(base_size = 11) +
  labs(
    title = "WHO goal progress summary - health regions",
    x = "",
    y = "Percent of units",
    fill = "WHO status"
  )

ggsave(
  file.path(Endereco_mapas, "health_region_who_goal_summary.png"),
  plot = grafico,
  width = 20,
  height = 12,
  units = "cm",
  dpi = 300
)

###################################################################################################
######### PARTE 6 - MAPAS DAS METAS NAS REGIOES DE SAUDE #########

arquivo_shape_regiao <- file.path(
  Endereco_dados,
  "Shapefiles",
  "regioes_saude",
  "regioes_saude.shp"
)

if(file.exists(arquivo_shape_regiao)){

  GEO_REGSAUDE <- st_read(arquivo_shape_regiao, quiet = TRUE)

  col_regiao <- intersect(
    names(GEO_REGSAUDE),
    c(
      "health_region_code", "hlth_rgn_c", "hlth_r_",
      "hlth_rg", "co_regsaud"
    )
  )[1]

  if(!is.na(col_regiao)){

    GEO_REGSAUDE$codigo <- str_pad(
      str_replace(
        as.character(GEO_REGSAUDE[[col_regiao]]),
        "\\.0$",
        ""
      ),
      5,
      pad = "0"
    )

    indicadores_mapa <- unique(status_regiao$indicador)

    for(ind_ref in indicadores_mapa){

      dados_mapa <- status_regiao %>% filter(indicador == ind_ref)
      mapa <- left_join(GEO_REGSAUDE, dados_mapa, by = "codigo")

      titulo <- paste0(
        unique(dados_mapa$nome_indicador),
        " - WHO goal progress - health regions",
        " - latest year: ",
        unique(dados_mapa$ano_recente)
      )

      grafico <- ggplot(mapa) +
        geom_sf(aes(fill = status_oms), color = "white", size = .10) +
        scale_fill_manual(
          values = cores_status,
          drop = FALSE,
          na.value = "grey90"
        ) +
        theme_void(base_size = 10) +
        theme(
          legend.position = "right",
          legend.title = element_text(size = 8),
          legend.text = element_text(size = 8),
          plot.title = element_text(size = 11)
        ) +
        labs(title = titulo, fill = "WHO status")

      ggsave(
        file.path(
          Endereco_mapas,
          paste0("health_region_who_goal_", ind_ref, ".png")
        ),
        plot = grafico,
        width = 20,
        height = 15,
        units = "cm",
        dpi = 300
      )
    }
  }
}

cat("Graficos das metas OMS criados em:", Endereco_mapas, "\n")
cat("Bases de status das metas OMS criadas em:", Endereco_oms, "\n")

###################################################################################################
###################################################################################################
