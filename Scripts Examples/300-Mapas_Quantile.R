###################################################################################################
###################################################################################################

#### MODELO PARA VARIACAO DAS DISTANTICAS ####

library(tidyverse)
library(geobr)

######### RECORTES GEOGRAFICOS ######### 

resumo_BR <- readRDS("E:/Documentos/Data Paper - Distancias SINASC/resumo_BR_quantile2.rds")
resumo_BR$BR = "BR"
resumo_BR <- resumo_BR[,c(1,27,2:26)]

resumo_MUNIC <- readRDS("E:/Documentos/Data Paper - Distancias SINASC/resumo_MUNIC_quantile2.rds")
resumo_REGIAO <- readRDS("E:/Documentos/Data Paper - Distancias SINASC/resumo_REGIAO_quantile2.rds")
resumo_REGSAUDE <- readRDS("E:/Documentos/Data Paper - Distancias SINASC/resumo_REGSAUDE_quantile2.rds")
resumo_UF <- readRDS("E:/Documentos/Data Paper - Distancias SINASC/resumo_UF_quantile2.rds")

### MAPAS:

REF_BASES <- c("resumo_REGIAO", "resumo_UF", "resumo_REGSAUDE", "resumo_MUNIC")

for (i in REF_BASES) {
  
  ## merge com shapefile
  
  end_shape <- "E:/Documentos/Banco de Dados/Dados GeoBR/"
  
  GEO_REGIAO <- readRDS(paste(end_shape,"region_shape_2020.rds",sep = ""))
  GEO_REGIAO$name_region <- ifelse(GEO_REGIAO$name_region == "Centro Oeste", "Centro-Oeste", GEO_REGIAO$name_region)
  GEO_MUNIC_BR <- readRDS(paste(end_shape,"municipality_shape_2020.rds",sep = ""))
  GEO_MUNIC_BR$code_muni6 <- str_sub(as.character(GEO_MUNIC_BR$code_muni),1,6)
  GEO_UF <- readRDS(paste(end_shape,"state_shape_2020.rds",sep = ""))
  GEO_SAUDE <- readRDS(paste(end_shape,"health_region_shape_2013.rds",sep = ""))
  GEO_SAUDE$code_health_region <- as.numeric(GEO_SAUDE$code_health_region)
  
  if(i == "resumo_REGIAO"){regiao_grafico <- left_join(GEO_REGIAO, resumo_REGIAO, by = c("name_region" = "res_REGIAO"))}
  if(i == "resumo_MUNIC"){munic_grafico <- left_join(GEO_MUNIC_BR, resumo_MUNIC, by = c("code_muni6" = "CODMUNRES"))}
  if(i == "resumo_UF"){uf_grafico <- left_join(GEO_UF, resumo_UF, by = c("abbrev_state" = "res_SIGLA_UF"))}
  if(i == "resumo_REGSAUDE"){saude_grafico <- left_join(GEO_SAUDE, resumo_REGSAUDE, by = c("code_health_region" = "COD_REGSAUDE_RES"))}
  
  if(i == "resumo_MUNIC"){rm(GEO_UF, GEO_SAUDE, GEO_REGIAO, GEO_MUNIC_BR, end_shape)}
  gc()
  
  ## gerar e salvar os mapas
  
  Endereco_saida_dados <- "E:/Documentos/Data Paper - Distancias SINASC/Mapas Quantile/"
  
  if(i == "resumo_REGIAO"){dados_grafico = regiao_grafico}
  if(i == "resumo_UF"){dados_grafico = uf_grafico}
  if(i == "resumo_REGSAUDE"){dados_grafico = saude_grafico}
  if(i == "resumo_MUNIC"){dados_grafico = munic_grafico}
  
  dados_grafico <- dados_grafico %>% filter(!is.na(quantile_dist) & ano_nasc == "2017")
  dados_grafico$pp_nasc_lrfb2 <- dados_grafico$numero_nasc_lrfb2 / dados_grafico$numero_nasc
  dados_grafico$pp_nasc_lrfb2 <- 100*dados_grafico$pp_nasc_lrfb2
  dados_grafico$parto_cesareo <- 100*dados_grafico$parto_cesareo
  dados_grafico$getacao_acima_37sema <- 100*dados_grafico$getacao_acima_37sema
  dados_grafico$gravidez_multi <- 100*dados_grafico$gravidez_multi
  
  ano_dados <- dados_grafico$ano_nasc[1]
  
  # 12.98 p 125, 19.10 p250, 25.17 p 375, 32.46 p500, 43.52 p625, 60.53 p750, 95.96 p975
  
  dados_grafico$quantile_dist <- case_when(
    dados_grafico$quantile_dist == "p250" ~ "P25 - (0 a 19] km",
    dados_grafico$quantile_dist == "p500" ~ "P50 - (19 a 32] km",
    dados_grafico$quantile_dist == "p750" ~ "P75 - (32 a 60] km",
    dados_grafico$quantile_dist == "p999" ~ "P75+ - (60 a +] km",
    dados_grafico$quantile_dist == "Nao Deslocou" ~ "Nao Deslocou - 0 km",
  )
  
  if(i == "resumo_REGIAO" | i == "resumo_UF"){ ## com linha entre as regioes
     
    ## 1 DISTANCIA ROTAS
    seq_breaks <- c(round(as.numeric(quantile(dados_grafico$dist_rota, # c(.125,.25,.375,.50,.625,.75,.875)
                                              c(.25,.50,.75), na.rm = TRUE)),2))
    grafico <- dados_grafico %>% filter(!is.na(dist_rota)) %>% ggplot() + 
      geom_sf(aes(fill = dist_rota), size=.15) + 
      scale_fill_fermenter(breaks = seq_breaks, palette = "Blues", direction = 1) +
      facet_wrap(vars(quantile_dist)) + 
      theme_void(base_size = 10) + theme(legend.position = 'right') +
      labs(title = "dist_rota", 
           subtitle = "", fill = "Distancia (km): ")
    
    ggsave(filename = paste(Endereco_saida_dados,"01_dist_rota_",i,"_",ano_dados,".png", sep =""),
           plot = grafico, width = 30, height = 15, units = c("cm"), dpi = 600, limitsize = TRUE)
    
    ## 2 CESAREO
    seq_breaks <- c(round(as.numeric(quantile(dados_grafico$parto_cesareo, 
                                              c(.25,.50,.75), na.rm = TRUE)),2))
    grafico <- dados_grafico %>% filter(!is.na(parto_cesareo)) %>% ggplot() + 
      geom_sf(aes(fill = parto_cesareo), size=.15) + 
      scale_fill_fermenter(breaks = seq_breaks, palette = "Blues", direction = 1) +
      facet_wrap(vars(quantile_dist)) + 
      theme_void(base_size = 10) + theme(legend.position = 'right') +
      labs(title = "parto_cesareo", 
           subtitle = "", fill = "% Cesareo: ")
    
    ggsave(filename = paste(Endereco_saida_dados,"02_parto_cesareo_",i,"_",ano_dados,".png", sep =""),
           plot = grafico, width = 30, height = 15, units = c("cm"), dpi = 600, limitsize = TRUE)
    
    ## 3 LRFB
    seq_breaks <- c(round(as.numeric(quantile(dados_grafico$pp_nasc_lrfb2, 
                                              c(.25,.50,.75), na.rm = TRUE)),2))
    grafico <- dados_grafico %>% filter(!is.na(pp_nasc_lrfb2)) %>% ggplot() + 
      geom_sf(aes(fill = pp_nasc_lrfb2), size=.15) + 
      scale_fill_fermenter(breaks = seq_breaks, palette = "Blues", direction = 1) +
      facet_wrap(vars(quantile_dist)) + 
      theme_void(base_size = 10) + theme(legend.position = 'right') +
      labs(title = "pp_nasc_lrfb2", 
           subtitle = "", fill = "% LRFB: ")
    
    ggsave(filename = paste(Endereco_saida_dados,"03_pp_nasc_lrfb2_",i,"_",ano_dados,".png", sep =""),
           plot = grafico, width = 30, height = 15, units = c("cm"), dpi = 600, limitsize = TRUE)
    
    ## 4 GESTACAO
    seq_breaks <- c(round(as.numeric(quantile(dados_grafico$getacao_acima_37sema, 
                                              c(.25,.50,.75), na.rm = TRUE)),2))
    grafico <- dados_grafico %>% filter(!is.na(getacao_acima_37sema)) %>% ggplot() + 
      geom_sf(aes(fill = getacao_acima_37sema), size=.15) + 
      scale_fill_fermenter(breaks = seq_breaks, palette = "Blues", direction = 1) +
      facet_wrap(vars(quantile_dist)) + 
      theme_void(base_size = 10) + theme(legend.position = 'right') +
      labs(title = "getacao_acima_37sema", 
           subtitle = "", fill = "% Gestação 37 Semanas: ")
    
    ggsave(filename = paste(Endereco_saida_dados,"04_getacao_acima_37sema_",i,"_",ano_dados,".png", sep =""),
           plot = grafico, width = 30, height = 15, units = c("cm"), dpi = 600, limitsize = TRUE)
    
    ## 5 gravidez_multi
    seq_breaks <- c(round(as.numeric(quantile(dados_grafico$gravidez_multi, 
                                              c(.25,.50,.75), na.rm = TRUE)),2))
    grafico <- dados_grafico %>% filter(!is.na(gravidez_multi)) %>% ggplot() + 
      geom_sf(aes(fill = gravidez_multi), size=.15) + 
      scale_fill_fermenter(breaks = seq_breaks, palette = "Blues", direction = 1) +
      facet_wrap(vars(quantile_dist)) + 
      theme_void(base_size = 10) + theme(legend.position = 'right') +
      labs(title = "gravidez_multi", 
           subtitle = "", fill = "% Gestação Multipla: ")
    
    ggsave(filename = paste(Endereco_saida_dados,"05_gravidez_multi_",i,"_",ano_dados,".png", sep =""),
           plot = grafico, width = 30, height = 15, units = c("cm"), dpi = 600, limitsize = TRUE)
    
    ## 6 Apgar 5
    seq_breaks <- c(round(as.numeric(quantile(dados_grafico$apgar_5_medio, 
                                              c(.25,.50,.75), na.rm = TRUE)),2))
    grafico <- dados_grafico %>% filter(!is.na(apgar_5_medio)) %>% ggplot() + 
      geom_sf(aes(fill = apgar_5_medio), size=.15) + 
      scale_fill_fermenter(breaks = seq_breaks, palette = "Blues", direction = 1) +
      facet_wrap(vars(quantile_dist)) + 
      theme_void(base_size = 10) + theme(legend.position = 'right') +
      labs(title = "apgar_5_medio", 
           subtitle = "", fill = "APGAR 5: ")
    
    ggsave(filename = paste(Endereco_saida_dados,"06_apgar_5_medio_",i,"_",ano_dados,".png", sep =""),
           plot = grafico, width = 30, height = 15, units = c("cm"), dpi = 600, limitsize = TRUE)
    
    ## 7 peso_medio
    seq_breaks <- c(round(as.numeric(quantile(dados_grafico$peso_medio, 
                                              c(.25,.50,.75), na.rm = TRUE)),2))
    grafico <- dados_grafico %>% filter(!is.na(peso_medio)) %>% ggplot() + 
      geom_sf(aes(fill = peso_medio), size=.15) + 
      scale_fill_fermenter(breaks = seq_breaks, palette = "Blues", direction = 1) +
      facet_wrap(vars(quantile_dist)) + 
      theme_void(base_size = 10) + theme(legend.position = 'right') +
      labs(title = "peso_medio", 
           subtitle = "", fill = "Peso Médio: ")
    
    ggsave(filename = paste(Endereco_saida_dados,"07_peso_medio_",i,"_",ano_dados,".png", sep =""),
           plot = grafico, width = 30, height = 15, units = c("cm"), dpi = 600, limitsize = TRUE)
    
    
  }
  
  if(i == "resumo_REGSAUDE" | i == "resumo_MUNIC"){ ## sem linha entre as regioes
    
    ## 1 DISTANCIA ROTAS
    seq_breaks <- c(round(as.numeric(quantile(dados_grafico$dist_rota, 
                                              c(.25,.50,.75), na.rm = TRUE)),2))
    grafico <- dados_grafico %>% filter(!is.na(dist_rota)) %>% ggplot() + 
      geom_sf(aes(fill = dist_rota), color = NA, size=.15) + 
      scale_fill_fermenter(breaks = seq_breaks, palette = "Blues", direction = 1) +
      facet_wrap(vars(quantile_dist)) + 
      theme_void(base_size = 10) + theme(legend.position = 'right') +
      labs(title = "dist_rota", 
           subtitle = "", fill = "Distancia (km): ")
    
    ggsave(filename = paste(Endereco_saida_dados,"01_dist_rota_",i,"_",ano_dados,".png", sep =""),
           plot = grafico, width = 30, height = 15, units = c("cm"), dpi = 600, limitsize = TRUE)
    
    ## 2 CESAREO
    seq_breaks <- c(round(as.numeric(quantile(dados_grafico$parto_cesareo, 
                                              c(.25,.50,.75), na.rm = TRUE)),2))
    grafico <- dados_grafico %>% filter(!is.na(parto_cesareo)) %>% ggplot() + 
      geom_sf(aes(fill = parto_cesareo), color = NA, size=.15) + 
      scale_fill_fermenter(breaks = seq_breaks, palette = "Blues", direction = 1) +
      facet_wrap(vars(quantile_dist)) + 
      theme_void(base_size = 10) + theme(legend.position = 'right') +
      labs(title = "parto_cesareo", 
           subtitle = "", fill = "% Cesareo: ")
    
    ggsave(filename = paste(Endereco_saida_dados,"02_parto_cesareo_",i,"_",ano_dados,".png", sep =""),
           plot = grafico, width = 30, height = 15, units = c("cm"), dpi = 600, limitsize = TRUE)
    
    ## 3 LRFB
    seq_breaks <- c(round(as.numeric(quantile(dados_grafico$pp_nasc_lrfb2, 
                                              c(.25,.50,.75), na.rm = TRUE)),2))
    grafico <- dados_grafico %>% filter(!is.na(pp_nasc_lrfb2)) %>% ggplot() + 
      geom_sf(aes(fill = pp_nasc_lrfb2), color = NA, size=.15) + 
      scale_fill_fermenter(breaks = seq_breaks, palette = "Blues", direction = 1) +
      facet_wrap(vars(quantile_dist)) + 
      theme_void(base_size = 10) + theme(legend.position = 'right') +
      labs(title = "pp_nasc_lrfb2", 
           subtitle = "", fill = "% LRFB: ")
    
    ggsave(filename = paste(Endereco_saida_dados,"03_pp_nasc_lrfb2_",i,"_",ano_dados,".png", sep =""),
           plot = grafico, width = 30, height = 15, units = c("cm"), dpi = 600, limitsize = TRUE)
    
    ## 4 GESTACAO
    seq_breaks <- c(round(as.numeric(quantile(dados_grafico$getacao_acima_37sema, 
                                              c(.25,.50,.75), na.rm = TRUE)),2))
    grafico <- dados_grafico %>% filter(!is.na(getacao_acima_37sema)) %>% ggplot() + 
      geom_sf(aes(fill = getacao_acima_37sema), color = NA, size=.15) + 
      scale_fill_fermenter(breaks = seq_breaks, palette = "Blues", direction = 1) +
      facet_wrap(vars(quantile_dist)) + 
      theme_void(base_size = 10) + theme(legend.position = 'right') +
      labs(title = "getacao_acima_37sema", 
           subtitle = "", fill = "% Gestação 37 Semanas: ")
    
    ggsave(filename = paste(Endereco_saida_dados,"04_getacao_acima_37sema_",i,"_",ano_dados,".png", sep =""),
           plot = grafico, width = 30, height = 15, units = c("cm"), dpi = 600, limitsize = TRUE)
    
    ## 5 gravidez_multi
    seq_breaks <- c(round(as.numeric(quantile(dados_grafico$gravidez_multi, 
                                              c(.25,.50,.75), na.rm = TRUE)),2))
    grafico <- dados_grafico %>% filter(!is.na(gravidez_multi)) %>% ggplot() + 
      geom_sf(aes(fill = gravidez_multi), color = NA, size=.15) + 
      scale_fill_fermenter(breaks = seq_breaks, palette = "Blues", direction = 1) +
      facet_wrap(vars(quantile_dist)) + 
      theme_void(base_size = 10) + theme(legend.position = 'right') +
      labs(title = "gravidez_multi", 
           subtitle = "", fill = "% Gestação Multipla: ")
    
    ggsave(filename = paste(Endereco_saida_dados,"05_gravidez_multi_",i,"_",ano_dados,".png", sep =""),
           plot = grafico, width = 30, height = 15, units = c("cm"), dpi = 600, limitsize = TRUE)
    
    ## 6 Apgar 5
    seq_breaks <- c(round(as.numeric(quantile(dados_grafico$apgar_5_medio, 
                                              c(.25,.50,.75), na.rm = TRUE)),2))
    grafico <- dados_grafico %>% filter(!is.na(apgar_5_medio)) %>% ggplot() + 
      geom_sf(aes(fill = apgar_5_medio), color = NA, size=.15) + 
      scale_fill_fermenter(breaks = seq_breaks, palette = "Blues", direction = 1) +
      facet_wrap(vars(quantile_dist)) + 
      theme_void(base_size = 10) + theme(legend.position = 'right') +
      labs(title = "apgar_5_medio", 
           subtitle = "", fill = "APGAR 5: ")
    
    ggsave(filename = paste(Endereco_saida_dados,"06_apgar_5_medio_",i,"_",ano_dados,".png", sep =""),
           plot = grafico, width = 30, height = 15, units = c("cm"), dpi = 600, limitsize = TRUE)
    
    ## 7 peso_medio
    seq_breaks <- c(round(as.numeric(quantile(dados_grafico$peso_medio, 
                                              c(.25,.50,.75), na.rm = TRUE)),2))
    grafico <- dados_grafico %>% filter(!is.na(peso_medio)) %>% ggplot() + 
      geom_sf(aes(fill = peso_medio), color = NA, size=.15) + 
      scale_fill_fermenter(breaks = seq_breaks, palette = "Blues", direction = 1) +
      facet_wrap(vars(quantile_dist)) + 
      theme_void(base_size = 10) + theme(legend.position = 'right') +
      labs(title = "peso_medio", 
           subtitle = "", fill = "Peso Médio: ")
    
    ggsave(filename = paste(Endereco_saida_dados,"07_peso_medio_",i,"_",ano_dados,".png", sep =""),
           plot = grafico, width = 30, height = 15, units = c("cm"), dpi = 600, limitsize = TRUE)
    
    
  }
  
  if(i != "resumo_BR"){rm(grafico,dados_grafico,ano_dados)}
  print(i)
  gc()
  
}
