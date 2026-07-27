#####################################################################################################

################### PARTE 2 #######################

#### GRAFICOS:
rm(list = ls(all.names=TRUE)) # Limpar area de trabalho
gc()

pacotes = c("tidyverse", "dplyr", "readxl", "sf", "readr") 
lapply(pacotes, require, character.only = TRUE)

## Bases resumo SINASC:
resumo_BR <- readRDS("E:/Documentos/Data Paper - Distancias SINASC/resumo_BR2.rds")
resumo_res_REGIAO <- readRDS("E:/Documentos/Data Paper - Distancias SINASC/resumo_REGIAO2.rds")
resumo_res_SIGLA_UF <- readRDS("E:/Documentos/Data Paper - Distancias SINASC/resumo_UF2.rds")
resumo_COD_REGSAUDE_RES <- readRDS("E:/Documentos/Data Paper - Distancias SINASC/resumo_REGSAUDE2.rds")
resumo_CODMUNRES <- readRDS("E:/Documentos/Data Paper - Distancias SINASC/resumo_MUNIC2.rds")
resumo_CODMUNRES$CODMUNRES <- as.character(resumo_CODMUNRES$CODMUNRES)

resumo_UF_RS <- readRDS("E:/Documentos/Data Paper - Distancias SINASC/resumo_uf_reg_saude_1_v3.rds")
resumo_UF_RS <- resumo_UF_RS %>% filter(loc_res_nasc == "Diferente")
resumo_UF_RS$pp_deslocou <- 100*(resumo_UF_RS$soma_deslocou_rsaude / resumo_UF_RS$numero_deslocou)
  
## Loop para os graficos:
lista_regioes <- c("res_REGIAO", "res_NOME_UF", "COD_REGSAUDE_RES", "CODMUNRES")

for(i in lista_regioes){
  
  ## merge com shapefile
  
  end_shape <- "E:/Documentos/Banco de Dados/Dados GeoBR/"
  
  GEO_REGIAO <- readRDS(paste(end_shape,"region_shape_2020.rds",sep = ""))
  GEO_REGIAO$name_region <- ifelse(GEO_REGIAO$name_region == "Centro Oeste", "Centro-Oeste", GEO_REGIAO$name_region)
  GEO_MUNIC_BR <- readRDS(paste(end_shape,"municipality_shape_2020.rds",sep = ""))
  GEO_MUNIC_BR$code_muni6 <- str_sub(as.character(GEO_MUNIC_BR$code_muni),1,6)
  GEO_UF <- readRDS(paste(end_shape,"state_shape_2020.rds",sep = ""))
  GEO_SAUDE <- readRDS(paste(end_shape,"health_region_shape_2013.rds",sep = ""))
  GEO_SAUDE$code_health_region <- as.numeric(GEO_SAUDE$code_health_region)
  
  if(i == "res_REGIAO"){regiao_grafico <- left_join(GEO_REGIAO, resumo_res_REGIAO, by = c("name_region" = "res_REGIAO"))}
  if(i == "CODMUNRES"){munic_grafico <- left_join(GEO_MUNIC_BR, resumo_CODMUNRES, by = c("code_muni6" = "CODMUNRES"))}
  if(i == "res_NOME_UF"){
    uf_grafico <- left_join(GEO_UF, resumo_res_SIGLA_UF, by = c("abbrev_state" = "res_SIGLA_UF"))
    uf_grafico_rs <- left_join(GEO_UF, resumo_UF_RS, by = c("abbrev_state" = "res_SIGLA_UF"))}
  if(i == "COD_REGSAUDE_RES"){saude_grafico <- left_join(GEO_SAUDE, resumo_COD_REGSAUDE_RES, by = c("code_health_region" = "COD_REGSAUDE_RES"))}
  
  if(i == "CODMUNRES"){rm(GEO_UF, GEO_SAUDE, GEO_REGIAO, GEO_MUNIC_BR, end_shape)}
  gc()
  
  ## gerar e salvar os mapas
  
  Endereco_saida_dados <- "E:/Documentos/Data Paper - Distancias SINASC/Paper Adjustments/New Maps/"

  if(i == "res_REGIAO"){dados_grafico = regiao_grafico}
  if(i == "res_NOME_UF"){
    dados_grafico = uf_grafico
    dados_grafico_rs = uf_grafico_rs}
  if(i == "COD_REGSAUDE_RES"){dados_grafico = saude_grafico}
  if(i == "CODMUNRES"){dados_grafico = munic_grafico}
  
  dados_grafico <- dados_grafico %>% filter(ano_nasc == "2007" | ano_nasc == "2012" | ano_nasc == "2017")
  
  dados_grafico$pp_nasc_lrfb2 <- dados_grafico$numero_nasc_lrfb2 / dados_grafico$numero_nasc
  dados_grafico$pp_nasc_lrfb2 <- 100*dados_grafico$pp_nasc_lrfb2
  dados_grafico$parto_cesareo <- 100*dados_grafico$parto_cesareo
  dados_grafico$getacao_acima_37sema <- 100*dados_grafico$getacao_acima_37sema
  dados_grafico$gravidez_multi <- 100*dados_grafico$gravidez_multi
  dados_grafico$pp_nasc_fora <- 100*dados_grafico$pp_nasc_fora
  
  bordas_regiao <- regiao_grafico[,c(1,2,3,121)] %>% filter(ano_nasc == "2007" | ano_nasc == "2012" | ano_nasc == "2017")
  
  if(i == "res_REGIAO" | i == "res_NOME_UF"){ ## com linha entre as regioes
    
    ## PP nascimentos
    seq_breaks <- c(round(as.numeric(quantile(dados_grafico$pp_nasc_fora, 
                                              c(.125,.25,.375,.50,.625,.75,.875), na.rm = TRUE)),2))
    grafico <- dados_grafico %>% filter(!is.na(pp_nasc_fora)) %>% ggplot() + 
      geom_sf(aes(fill = pp_nasc_fora), size=.01) + 
      geom_sf(data = bordas_regiao$geom, fill = "transparent", size=0.4) +
      scale_fill_fermenter(breaks = seq_breaks, palette = "Oranges", direction = 1) +
      facet_wrap(vars(ano_nasc)) + 
      theme_void(base_size = 10) + theme(legend.position = 'right') +
      labs(title = "Share of births that took place outside of mother's residence municipality", 
           subtitle = "", fill = "% of displacements: ")
    
    ggsave(filename = paste(Endereco_saida_dados,"01_PP_Nasc_fora_rotas_",i,".png", sep =""),
           plot = grafico, width = 30, height = 15, units = c("cm"), dpi = 600, limitsize = TRUE)
    
    ## Distancia Media Fora (maes que deslocaram)
    seq_breaks <- c(round(as.numeric(quantile(dados_grafico$dist_rota_deslocou, 
                                              c(.125,.25,.375,.50,.625,.75,.875), na.rm = TRUE)),1))
    grafico <- dados_grafico %>% filter(!is.na(dist_rota_deslocou)) %>% ggplot() + 
      geom_sf(aes(fill = dist_rota_deslocou), size=.01) + 
      geom_sf(data = bordas_regiao$geom, fill = "transparent", size=0.4) + 
      scale_fill_fermenter(breaks = seq_breaks, palette = "Purples", direction = 1) +
      facet_wrap(vars(ano_nasc)) + 
      theme_void(base_size = 10) + theme(legend.position = 'right') +
      labs(title = "Average displacement distance (in kilometers)", 
           subtitle = "", fill = "Average distance: ")
    
    ggsave(filename = paste(Endereco_saida_dados,"02_Dist_Media_rotas_",i,".png", sep =""),
           plot = grafico, width = 30, height = 15, units = c("cm"), dpi = 600, limitsize = TRUE)
    
    ## Distancia Media Total (todas as maes)
    seq_breaks <- c(round(as.numeric(quantile(dados_grafico$dist_rota, 
                                              c(.125,.25,.375,.50,.625,.75,.875), na.rm = TRUE)),1))
    grafico <- dados_grafico %>% filter(!is.na(dist_rota)) %>% ggplot() + 
      geom_sf(aes(fill = dist_rota), size=.01) + 
      geom_sf(data = bordas_regiao$geom, fill = "transparent", size=0.4) +
      scale_fill_fermenter(breaks = seq_breaks, palette = "Purples", direction = 1) +
      facet_wrap(vars(ano_nasc)) + 
      theme_void(base_size = 10) + theme(legend.position = 'right') +
      labs(title = "Average distance (in kilometers)", 
           subtitle = "", fill = "Average distance: ")
    
    ggsave(filename = paste(Endereco_saida_dados,"03_Dist_total_rotas_",i,".png", sep =""),
           plot = grafico, width = 30, height = 15, units = c("cm"), dpi = 600, limitsize = TRUE)
    
    ## Gini Rotas - BASE COMPLETA
    seq_breaks <- c(round(as.numeric(quantile(dados_grafico$gini_dist_rota, 
                                              c(.125,.25,.375,.50,.625,.75,.875), na.rm = TRUE)),2))
    grafico <- dados_grafico %>% filter(!is.na(gini_dist_rota)) %>% ggplot() + 
      geom_sf(aes(fill = gini_dist_rota), size=.01) + 
      geom_sf(data = bordas_regiao$geom, fill = "transparent", size=0.4) +
      scale_fill_fermenter(breaks = seq_breaks, palette = "Blues", direction = 1) +
      facet_wrap(vars(ano_nasc)) + 
      theme_void(base_size = 10) + theme(legend.position = 'right') +
      labs(title = "Distance Gini Index", 
           subtitle = "", fill = "Gini Index: ")
    
    ggsave(filename = paste(Endereco_saida_dados,"04_gini_dist_rota_",i,".png", sep =""),
           plot = grafico, width = 30, height = 15, units = c("cm"), dpi = 600, limitsize = TRUE)
    
    ## Gini Tempo - BASE COMPLETA
    seq_breaks <- c(round(as.numeric(quantile(dados_grafico$gini_tempo_rota, 
                                              c(.125,.25,.375,.50,.625,.75,.875), na.rm = TRUE)),2))
    grafico <- dados_grafico %>% filter(!is.na(gini_tempo_rota)) %>% ggplot() + 
      geom_sf(aes(fill = gini_tempo_rota), size=.01) + 
      geom_sf(data = bordas_regiao$geom, fill = "transparent", size=0.4) +
      scale_fill_fermenter(breaks = seq_breaks, palette = "Blues", direction = 1) +
      facet_wrap(vars(ano_nasc)) + 
      theme_void(base_size = 10) + theme(legend.position = 'right') +
      labs(title = "Time Gini Index", 
           subtitle = "", fill = "Gini Index: ")
    
    ggsave(filename = paste(Endereco_saida_dados,"05_gini_tempo_rota_",i,".png", sep =""),
           plot = grafico, width = 30, height = 15, units = c("cm"), dpi = 600, limitsize = TRUE)
    
    ## Gini Rotas - DESLOCARAM
    seq_breaks <- c(round(as.numeric(quantile(dados_grafico$gini_dist_rota_deslocou, 
                                              c(.125,.25,.375,.50,.625,.75,.875), na.rm = TRUE)),2))
    grafico <- dados_grafico %>% filter(!is.na(gini_dist_rota_deslocou)) %>% ggplot() + 
      geom_sf(aes(fill = gini_dist_rota_deslocou),size=.01) + 
      geom_sf(data = bordas_regiao$geom, fill = "transparent", size=0.4) + 
      scale_fill_fermenter(breaks = seq_breaks, palette = "Blues", direction = 1) +
      facet_wrap(vars(ano_nasc)) + 
      theme_void(base_size = 10) + theme(legend.position = 'right') +
      labs(title = "Displacement distance Gini Index", 
           subtitle = "", fill = "Gini Index: ")
    
    ggsave(filename = paste(Endereco_saida_dados,"04_gini_dist_rota_deslocou_",i,".png", sep =""),
           plot = grafico, width = 30, height = 15, units = c("cm"), dpi = 600, limitsize = TRUE)
    
    ## Gini Tempo - DESLOCARAM
    seq_breaks <- c(round(as.numeric(quantile(dados_grafico$gini_tempo_rota_deslocou, 
                                              c(.125,.25,.375,.50,.625,.75,.875), na.rm = TRUE)),2))
    grafico <- dados_grafico %>% filter(!is.na(gini_tempo_rota_deslocou)) %>% ggplot() + 
      geom_sf(aes(fill = gini_tempo_rota_deslocou), size=.01) + 
      geom_sf(data = bordas_regiao$geom, fill = "transparent", size=0.4) +
      scale_fill_fermenter(breaks = seq_breaks, palette = "Blues", direction = 1) +
      facet_wrap(vars(ano_nasc)) + 
      theme_void(base_size = 10) + theme(legend.position = 'right') +
      labs(title = "Displacement time Gini Index", 
           subtitle = "", fill = "Gini Index: ")
    
    ggsave(filename = paste(Endereco_saida_dados,"05_gini_tempo_rota_deslocou_",i,".png", sep =""),
           plot = grafico, width = 30, height = 15, units = c("cm"), dpi = 600, limitsize = TRUE)
    
    ## Regiao de Saude - DESLOCARAM
    if(i == "res_NOME_UF"){
      dados_grafico_rs <- dados_grafico_rs %>% filter(ano_nasc == "2007" | ano_nasc == "2012" | ano_nasc == "2017")
      bordas_regiao_rs <- regiao_grafico[,c(1,2,3,121)] %>% filter(ano_nasc == "2007" | ano_nasc == "2012" | ano_nasc == "2017")
      
      seq_breaks <- c(round(as.numeric(quantile(dados_grafico_rs$pp_deslocou , 
                                                c(.125,.25,.375,.50,.625,.75,.875), na.rm = TRUE)),2))
      grafico <- dados_grafico_rs %>% filter(!is.na(pp_deslocou)) %>% ggplot() + 
        geom_sf(aes(fill = pp_deslocou ), size=.01) + 
        geom_sf(data = bordas_regiao_rs$geom, fill = "transparent", size=0.4) +
        scale_fill_fermenter(breaks = seq_breaks, palette = "Oranges", direction = 1) +
        facet_wrap(vars(ano_nasc)) + 
        theme_void(base_size = 10) + theme(legend.position = 'right') +
        labs(title = "Health Region - Displacement between health region", 
             subtitle = "", fill = "% of displacements: ")
      
      ggsave(filename = paste(Endereco_saida_dados,"06_reg_saude_deslocou_",i,".png", sep =""),
             plot = grafico, width = 30, height = 15, units = c("cm"), dpi = 600, limitsize = TRUE)
      
      
    }
  }
  
  if(i == "CODMUNRES" | i == "COD_REGSAUDE_RES"){ ## sem linha entre as regioes
    
    ## PP nascimentos
    seq_breaks <- c(round(as.numeric(quantile(dados_grafico$pp_nasc_fora, 
                                              c(.125,.25,.375,.50,.625,.75,.875), na.rm = TRUE)),2))
    grafico <- dados_grafico %>% filter(!is.na(pp_nasc_fora)) %>% ggplot() + 
      geom_sf(aes(fill = pp_nasc_fora), color = NA, size=.15) + 
      geom_sf(data = bordas_regiao$geom, fill = "transparent", size=0.4) +
      scale_fill_fermenter(breaks = seq_breaks, palette = "Oranges", direction = 1) +
      facet_wrap(vars(ano_nasc)) + 
      theme_void(base_size = 10) + theme(legend.position = 'right') +
      labs(title = "Share of births that took place outside of mother's residence municipality", 
           subtitle = "", fill = "% of displacements: ")
    
    ggsave(filename = paste(Endereco_saida_dados,"01_PP_Nasc_fora_rotas_",i,".png", sep =""),
           plot = grafico, width = 30, height = 15, units = c("cm"), dpi = 600, limitsize = TRUE)
    
    ## Distancia Media Fora (maes que deslocaram)
    seq_breaks <- c(round(as.numeric(quantile(dados_grafico$dist_rota_deslocou, 
                                              c(.125,.25,.375,.50,.625,.75,.875), na.rm = TRUE)),1))
    grafico <- dados_grafico %>% filter(!is.na(dist_rota_deslocou)) %>% ggplot() + 
      geom_sf(aes(fill = dist_rota_deslocou), color = NA, size=.15) + 
      geom_sf(data = bordas_regiao$geom, fill = "transparent", size=0.4) +
      scale_fill_fermenter(breaks = seq_breaks, palette = "Purples", direction = 1) +
      facet_wrap(vars(ano_nasc)) + 
      theme_void(base_size = 10) + theme(legend.position = 'right') +
      labs(title = "Average displacement distance (in kilometers)", 
           subtitle = "", fill = "Average distance: ")
    
    ggsave(filename = paste(Endereco_saida_dados,"02_Dist_Media_rotas_",i,".png", sep =""),
           plot = grafico, width = 30, height = 15, units = c("cm"), dpi = 600, limitsize = TRUE)
    
    ## Distancia Media Total (todas as maes)
    seq_breaks <- c(round(as.numeric(quantile(dados_grafico$dist_rota, 
                                              c(.125,.25,.375,.50,.625,.75,.875), na.rm = TRUE)),1))
    grafico <- dados_grafico %>% filter(!is.na(dist_rota)) %>% ggplot() + 
      geom_sf(aes(fill = dist_rota), color = NA, size=.15) + 
      geom_sf(data = bordas_regiao$geom, fill = "transparent", size=0.4) +
      scale_fill_fermenter(breaks = seq_breaks, palette = "Purples", direction = 1) +
      facet_wrap(vars(ano_nasc)) + 
      theme_void(base_size = 10) + theme(legend.position = 'right') +
      labs(title = "Average distance (in kilometers)", 
           subtitle = "", fill = "Average distance: ")
    
    ggsave(filename = paste(Endereco_saida_dados,"03_Dist_total_rotas_",i,".png", sep =""),
           plot = grafico, width = 30, height = 15, units = c("cm"), dpi = 600, limitsize = TRUE)
    
    ## Gini Rotas - BASE COMPLETA
    seq_breaks <- c(round(as.numeric(quantile(dados_grafico$gini_dist_rota, 
                                              c(.125,.25,.375,.50,.625,.75,.875), na.rm = TRUE)),2))
    grafico <- dados_grafico %>% filter(!is.na(gini_dist_rota)) %>% ggplot() + 
      geom_sf(aes(fill = gini_dist_rota), color = NA, size=.15) + 
      geom_sf(data = bordas_regiao$geom, fill = "transparent", size=0.4) +
      scale_fill_fermenter(breaks = seq_breaks, palette = "Blues", direction = 1) +
      facet_wrap(vars(ano_nasc)) + 
      theme_void(base_size = 10) + theme(legend.position = 'right') +
      labs(title = "Distance Gini Index", 
           subtitle = "", fill = "Gini Index: ")
    
    ggsave(filename = paste(Endereco_saida_dados,"04_gini_dist_rota_",i,".png", sep =""),
           plot = grafico, width = 30, height = 15, units = c("cm"), dpi = 600, limitsize = TRUE)
    
    ## Gini Tempo - BASE COMPLETA
    seq_breaks <- c(round(as.numeric(quantile(dados_grafico$gini_tempo_rota, 
                                              c(.125,.25,.375,.50,.625,.75,.875), na.rm = TRUE)),2))
    grafico <- dados_grafico %>% filter(!is.na(gini_tempo_rota)) %>% ggplot() + 
      geom_sf(aes(fill = gini_tempo_rota), color = NA, size=.15) + 
      geom_sf(data = bordas_regiao$geom, fill = "transparent", size=0.4) +
      scale_fill_fermenter(breaks = seq_breaks, palette = "Blues", direction = 1) +
      facet_wrap(vars(ano_nasc)) + 
      theme_void(base_size = 10) + theme(legend.position = 'right') +
      labs(title = "Time Gini Index", 
           subtitle = "", fill = "Gini Index: ")
    
    ggsave(filename = paste(Endereco_saida_dados,"05_gini_tempo_rota_",i,".png", sep =""),
           plot = grafico, width = 30, height = 15, units = c("cm"), dpi = 600, limitsize = TRUE)
    
    ## Gini Rotas - DESLOCARAM
    seq_breaks <- c(round(as.numeric(quantile(dados_grafico$gini_dist_rota_deslocou, 
                                              c(.125,.25,.375,.50,.625,.75,.875), na.rm = TRUE)),2))
    grafico <- dados_grafico %>% filter(!is.na(gini_dist_rota_deslocou)) %>% ggplot() + 
      geom_sf(aes(fill = gini_dist_rota_deslocou), color = NA, size=.15) + 
      geom_sf(data = bordas_regiao$geom, fill = "transparent", size=0.4) +
      scale_fill_fermenter(breaks = seq_breaks, palette = "Blues", direction = 1) +
      facet_wrap(vars(ano_nasc)) + 
      theme_void(base_size = 10) + theme(legend.position = 'right') +
      labs(title = "Displacement distance Gini Index", 
           subtitle = "", fill = "Gini Index: ")
    
    ggsave(filename = paste(Endereco_saida_dados,"04_gini_dist_rota_deslocou_",i,".png", sep =""),
           plot = grafico, width = 30, height = 15, units = c("cm"), dpi = 600, limitsize = TRUE)
    
    ## Gini Tempo - DESLOCARAM
    seq_breaks <- c(round(as.numeric(quantile(dados_grafico$gini_tempo_rota_deslocou, 
                                              c(.125,.25,.375,.50,.625,.75,.875), na.rm = TRUE)),2))
    grafico <- dados_grafico %>% filter(!is.na(gini_tempo_rota_deslocou)) %>% ggplot() + 
      geom_sf(aes(fill = gini_tempo_rota_deslocou), color = NA, size=.15) + 
      geom_sf(data = bordas_regiao$geom, fill = "transparent", size=0.4) +
      scale_fill_fermenter(breaks = seq_breaks, palette = "Blues", direction = 1) +
      facet_wrap(vars(ano_nasc)) + 
      theme_void(base_size = 10) + theme(legend.position = 'right') +
      labs(title = "Displacement time Gini Index", 
           subtitle = "", fill = "Gini Index: ")
    
    ggsave(filename = paste(Endereco_saida_dados,"05_gini_tempo_rota_deslocou_",i,".png", sep =""),
           plot = grafico, width = 30, height = 15, units = c("cm"), dpi = 600, limitsize = TRUE)
    
    
  }
  print(i)
}

rm(lista_regioes, dados_grafico, munic_grafico, saude_grafico, regiao_grafico, uf_grafico, 
   i, grafico, seq_breaks, bordas_regiao)
gc()

################################################################################################

## Regiao de Saude - Serie de tempo

resumo_UF_RS$Regiao <- case_when(
  # Norte:
  resumo_UF_RS$res_SIGLA_UF == "AM" ~ "Norte",
  resumo_UF_RS$res_SIGLA_UF == "RR" ~ "Norte",
  resumo_UF_RS$res_SIGLA_UF == "AP" ~ "Norte",
  resumo_UF_RS$res_SIGLA_UF == "PA" ~ "Norte",
  resumo_UF_RS$res_SIGLA_UF == "TO" ~ "Norte",
  resumo_UF_RS$res_SIGLA_UF == "RO" ~ "Norte",
  resumo_UF_RS$res_SIGLA_UF == "AC" ~ "Norte",
  # Nordeste
  resumo_UF_RS$res_SIGLA_UF == "MA" ~ "Nordeste",
  resumo_UF_RS$res_SIGLA_UF == "PI" ~ "Nordeste",
  resumo_UF_RS$res_SIGLA_UF == "CE" ~ "Nordeste",
  resumo_UF_RS$res_SIGLA_UF == "RN" ~ "Nordeste",
  resumo_UF_RS$res_SIGLA_UF == "PE" ~ "Nordeste",
  resumo_UF_RS$res_SIGLA_UF == "PB" ~ "Nordeste",
  resumo_UF_RS$res_SIGLA_UF == "SE" ~ "Nordeste",
  resumo_UF_RS$res_SIGLA_UF == "AL" ~ "Nordeste",
  resumo_UF_RS$res_SIGLA_UF == "BA" ~ "Nordeste",
  # Centro-Oeste
  resumo_UF_RS$res_SIGLA_UF == "DF" ~ "Centro-Oeste",
  resumo_UF_RS$res_SIGLA_UF == "MT" ~ "Centro-Oeste",
  resumo_UF_RS$res_SIGLA_UF == "MS" ~ "Centro-Oeste",
  resumo_UF_RS$res_SIGLA_UF == "GO" ~ "Centro-Oeste",
  # Sudeste
  resumo_UF_RS$res_SIGLA_UF == "SP" ~ "Sudeste",
  resumo_UF_RS$res_SIGLA_UF == "RJ" ~ "Sudeste",
  resumo_UF_RS$res_SIGLA_UF == "ES" ~ "Sudeste",
  resumo_UF_RS$res_SIGLA_UF == "MG" ~ "Sudeste",
  # Sul
  resumo_UF_RS$res_SIGLA_UF == "PR" ~ "Sul",
  resumo_UF_RS$res_SIGLA_UF == "RS" ~ "Sul",
  resumo_UF_RS$res_SIGLA_UF == "SC" ~ "Sul",
)

table(resumo_UF_RS$Regiao)

resumo_REGIAO_RS <- resumo_UF_RS %>% group_by(Regiao, ano_nasc, loc_res_nasc) %>% summarise(
  numero_nasc = sum(numero_nasc, na.rm = TRUE),
  numero_deslocou = sum(numero_deslocou, na.rm = TRUE),
  soma_deslocou_rsaude = sum(soma_deslocou_rsaude, na.rm = TRUE),
  pp_deslocou = soma_deslocou_rsaude/numero_deslocou
)

resumo_BR_RS <- resumo_UF_RS %>% group_by(ano_nasc, loc_res_nasc) %>% summarise(
  numero_nasc = sum(numero_nasc, na.rm = TRUE),
  numero_deslocou = sum(numero_deslocou, na.rm = TRUE),
  soma_deslocou_rsaude = sum(soma_deslocou_rsaude, na.rm = TRUE),
  pp_deslocou = soma_deslocou_rsaude/numero_deslocou
)

resumo_BR_RS$Regiao <- "BR"
resumo_BR_RS <- resumo_BR_RS[,c(7,1:6)]

resumo_REGIAO_RS <- rbind(resumo_REGIAO_RS, resumo_BR_RS)

resumo_REGIAO_RS$Regions <- case_when(
  resumo_REGIAO_RS$Regiao == "Norte" ~ "North",
  resumo_REGIAO_RS$Regiao == "Nordeste" ~ "Northeast",
  resumo_REGIAO_RS$Regiao == "Centro-Oeste" ~ "Central-West",
  resumo_REGIAO_RS$Regiao == "Sudeste" ~ "Southeast",
  resumo_REGIAO_RS$Regiao == "Sul" ~ "South",
  resumo_REGIAO_RS$Regiao == "BR" ~ "Brazil",
)

table(resumo_REGIAO_RS$Regiao, resumo_REGIAO_RS$Regions)

## Graph:
resumo_REGIAO_RS$ano_nasc <- as.integer(resumo_REGIAO_RS$ano_nasc)
resumo_REGIAO_RS$pp_deslocou <- 100*(resumo_REGIAO_RS$pp_deslocou)

resumo_REGIAO_RS %>% ggplot() +
  aes(x = ano_nasc, y = pp_deslocou, fill = Regions, colour = Regions) +
  geom_line(size = 2.25) +
  scale_fill_brewer(palette = "Oranges", direction = -1) +
  scale_color_brewer(palette = "Oranges", direction = -1) +
  labs(x = "Year", y = "Displacement", title = "Displacement between Health Region",
       #subtitle = "asdasd", caption = "adsads"
  ) + theme_minimal() + scale_x_continuous(breaks = seq(2006,2017,1))

