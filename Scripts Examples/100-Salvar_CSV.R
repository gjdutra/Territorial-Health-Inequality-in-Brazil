###################################################################################################
###################################################################################################

#### MODELO PARA VARIACAO DAS DISTANTICAS ####

library(tidyverse)
library(geobr)

resumo_BR <- readRDS("E:/Documentos/Data Paper - Distancias SINASC/resumo_BR.rds")

colnames(resumo_BR)

maes <- resumo_BR[,c(1,26:35,57:66,88:97)]

write.table(resumo_BR, 
            file = "E:/Documentos/Data Paper - Distancias SINASC/resumo_BR_v3.csv", 
            sep = ";", na = "NA", dec = ",", row.names = FALSE,
            col.names = TRUE, qmethod = c("escape", "double"), fileEncoding = "")


######### RECORTES GEOGRAFICOS ######### 

resumo_BR <- readRDS("E:/Documentos/Data Paper - Distancias SINASC/resumo_BR.rds")
resumo_BR$BR = "BR"
resumo_BR <- resumo_BR[,c(1,103,2:102)]

resumo_MUNIC <- readRDS("E:/Documentos/Data Paper - Distancias SINASC/resumo_MUNIC.rds")
resumo_REGIAO <- readRDS("E:/Documentos/Data Paper - Distancias SINASC/resumo_REGIAO.rds")
resumo_REGSAUDE <- readRDS("E:/Documentos/Data Paper - Distancias SINASC/resumo_REGSAUDE.rds")
resumo_UF <- readRDS("E:/Documentos/Data Paper - Distancias SINASC/resumo_UF.rds")

# Modelo do Artigo:
# D a distância média percorrida por mães de um dado município de residência (incluindo quem sai e quem tem filho no próprio município, i.e., distância=0) para o município de nascimento do filho.
# F a fração de mães que viajam (ou seja, que saem do seu município de residência) para ter filho em outro município;
# C a distância média percorrida, condicional a ter viajado (i.e., apenas  entre mães cuja distância>0);
# Dados dois anos (digamos inicial=2007 e final=2017) podemos escrever a  seguinte igualdade relacionando essas três variáveis: D=F∗C

REF_BASES <- c("resumo_BR", "resumo_REGIAO", "resumo_UF", "resumo_REGSAUDE", "resumo_MUNIC")

for (i in REF_BASES) {
  
  if(i == "resumo_BR"){base_dados = resumo_BR
    modelo <- base_dados %>% select(BR, ano_nasc, dist_rota, pp_nasc_fora, dist_rota_deslocou)}
  
  if(i == "resumo_REGIAO"){base_dados = resumo_REGIAO
  modelo <- base_dados %>% select(res_REGIAO, ano_nasc, dist_rota, pp_nasc_fora, dist_rota_deslocou)}
  
  if(i == "resumo_UF"){base_dados = resumo_UF
  modelo <- base_dados %>% select(res_SIGLA_UF, ano_nasc, dist_rota, pp_nasc_fora, dist_rota_deslocou)}
  
  if(i == "resumo_REGSAUDE"){base_dados = resumo_REGSAUDE
  modelo <- base_dados %>% select(COD_REGSAUDE_RES, ano_nasc, dist_rota, pp_nasc_fora, dist_rota_deslocou)}
  
  if(i == "resumo_MUNIC"){base_dados = resumo_MUNIC
    modelo <- base_dados %>% select(CODMUNRES, ano_nasc, dist_rota, pp_nasc_fora, dist_rota_deslocou)}
  
  modelo <- rename(modelo, "D" = "dist_rota")
  modelo <- rename(modelo, "F" = "pp_nasc_fora")
  modelo <- rename(modelo, "C" = "dist_rota_deslocou")
  modelo$D = modelo$F* modelo$C
  
  modelo_07 <- modelo %>% filter(ano_nasc == 2007)
  modelo_07 <- rename(modelo_07, "D_07" = "D")
  modelo_07 <- rename(modelo_07, "F_07" = "F")
  modelo_07 <- rename(modelo_07, "C_07" = "C")
  modelo_12 <- modelo %>% filter(ano_nasc == 2012)
  modelo_12 <- rename(modelo_12, "D_12" = "D")
  modelo_12 <- rename(modelo_12, "F_12" = "F")
  modelo_12 <- rename(modelo_12, "C_12" = "C")
  modelo_17 <- modelo %>% filter(ano_nasc == 2017)
  modelo_17 <- rename(modelo_17, "D_17" = "D")
  modelo_17 <- rename(modelo_17, "F_17" = "F")
  modelo_17 <- rename(modelo_17, "C_17" = "C")
  
  if(i == "resumo_BR"){
    modelo_final <- left_join(modelo_07, modelo_12, by = "BR")
    modelo_final <- left_join(modelo_final, modelo_17, by = "BR")
  }
  
  if(i == "resumo_REGIAO"){
    modelo_final <- left_join(modelo_07, modelo_12, by = "res_REGIAO")
    modelo_final <- left_join(modelo_final, modelo_17, by = "res_REGIAO")
  }
  
  if(i == "resumo_UF"){
    modelo_final <- left_join(modelo_07, modelo_12, by = "res_SIGLA_UF")
    modelo_final <- left_join(modelo_final, modelo_17, by = "res_SIGLA_UF")
  }
  
  if(i == "resumo_REGSAUDE"){
    modelo_final <- left_join(modelo_07, modelo_12, by = "COD_REGSAUDE_RES")
    modelo_final <- left_join(modelo_final, modelo_17, by = "COD_REGSAUDE_RES")
  }
  
  if(i == "resumo_MUNIC"){
    modelo_final <- left_join(modelo_07, modelo_12, by = "CODMUNRES")
    modelo_final <- left_join(modelo_final, modelo_17, by = "CODMUNRES")
  }
  
  modelo_final <- modelo_final %>% mutate(
    Delta_F1 = (F_12 - F_07),
    Delta_C1 = (C_12 - C_07),
    Delta_D1 = (Delta_F1)*C_12 + F_12*(Delta_C1),
    
    Delta_F2 = (F_17 - F_12),
    Delta_C2 = (C_17 - C_12),
    Delta_D2 = (Delta_F2)*C_17 + F_17*(Delta_C2),
    
    Delta_F3 = (F_17 - F_07),
    Delta_C3 = (C_17 - C_07),
    Delta_D3 = (Delta_F3)*C_17 + F_17*(Delta_C3),
  )
  
  modelo_final1 <- modelo_final[,c(1,14:16)]
  modelo_final1 <- rename(modelo_final1, "Delta_F" = "Delta_F1")
  modelo_final1 <- rename(modelo_final1, "Delta_C" = "Delta_C1")
  modelo_final1 <- rename(modelo_final1, "Delta_D" = "Delta_D1")
  modelo_final1$ref <- "1 - Entre 07 e 12"
  
  modelo_final2 <- modelo_final[,c(1,17:19)]
  modelo_final2 <- rename(modelo_final2, "Delta_F" = "Delta_F2")
  modelo_final2 <- rename(modelo_final2, "Delta_C" = "Delta_C2")
  modelo_final2 <- rename(modelo_final2, "Delta_D" = "Delta_D2")
  modelo_final2$ref <- "2 - Entre 12 e 17"
  
  modelo_final3 <- modelo_final[,c(1,20:22)]
  modelo_final3 <- rename(modelo_final3, "Delta_F" = "Delta_F3")
  modelo_final3 <- rename(modelo_final3, "Delta_C" = "Delta_C3")
  modelo_final3 <- rename(modelo_final3, "Delta_D" = "Delta_D3")
  modelo_final3$ref <- "3 - Entre 07 e 17"
  
  modelo_final <- rbind(modelo_final1, modelo_final2, modelo_final3)
  
  assign(paste("modelo",i, sep = "_"), modelo_final)
  
  rm(modelo_07, modelo_12, modelo_17, modelo_final1, modelo_final2, modelo_final3, 
     modelo, base_dados, modelo_final)
  gc()
  
  ## merge com shapefile
  
  end_shape <- "E:/Documentos/Banco de Dados/Dados GeoBR/"
  
  GEO_REGIAO <- readRDS(paste(end_shape,"region_shape_2020.rds",sep = ""))
  GEO_REGIAO$name_region <- ifelse(GEO_REGIAO$name_region == "Centro Oeste", "Centro-Oeste", GEO_REGIAO$name_region)
  GEO_MUNIC_BR <- readRDS(paste(end_shape,"municipality_shape_2020.rds",sep = ""))
  GEO_MUNIC_BR$code_muni6 <- str_sub(as.character(GEO_MUNIC_BR$code_muni),1,6)
  GEO_UF <- readRDS(paste(end_shape,"state_shape_2020.rds",sep = ""))
  GEO_SAUDE <- readRDS(paste(end_shape,"health_region_shape_2013.rds",sep = ""))
  GEO_SAUDE$code_health_region <- as.numeric(GEO_SAUDE$code_health_region)
  
  if(i == "resumo_REGIAO"){regiao_grafico <- left_join(GEO_REGIAO, modelo_resumo_REGIAO, by = c("name_region" = "res_REGIAO"))}
  if(i == "resumo_MUNIC"){munic_grafico <- left_join(GEO_MUNIC_BR, modelo_resumo_MUNIC, by = c("code_muni6" = "CODMUNRES"))}
  if(i == "resumo_UF"){uf_grafico <- left_join(GEO_UF, modelo_resumo_UF, by = c("abbrev_state" = "res_SIGLA_UF"))}
  if(i == "resumo_REGSAUDE"){saude_grafico <- left_join(GEO_SAUDE, modelo_resumo_REGSAUDE, by = c("code_health_region" = "COD_REGSAUDE_RES"))}
  
  if(i == "resumo_MUNIC"){rm(GEO_UF, GEO_SAUDE, GEO_REGIAO, GEO_MUNIC_BR, end_shape)}
  gc()
  
  ## gerar e salvar os mapas
  
  Endereco_saida_dados <- "E:/Documentos/Data Paper - Distâncias SINASC/Mapas Modelo Artigo/"
  
  if(i == "resumo_REGIAO"){dados_grafico = regiao_grafico}
  if(i == "resumo_UF"){dados_grafico = uf_grafico}
  if(i == "resumo_REGSAUDE"){dados_grafico = saude_grafico}
  if(i == "resumo_MUNIC"){dados_grafico = munic_grafico}

  if(i == "resumo_REGIAO" | i == "resumo_UF"){ ## com linha entre as regioes
    
    ## PP nascimentos
    seq_breaks <- c(round(as.numeric(quantile(dados_grafico$Delta_F, 
                                              c(.125,.25,.375,.50,.625,.75,.875), na.rm = TRUE)),2))
    grafico <- dados_grafico %>% filter(!is.na(Delta_F)) %>% ggplot() + 
      geom_sf(aes(fill = Delta_F), size=.15) + 
      scale_fill_fermenter(breaks = seq_breaks, palette = "Blues", direction = 1) +
      facet_wrap(vars(ref)) + 
      theme_void(base_size = 10) + theme(legend.position = 'right') +
      labs(title = "Delta_F", 
           subtitle = "", fill = "Delta_F: ")
    
    ggsave(filename = paste(Endereco_saida_dados,"01_Delta_F_",i,".png", sep =""),
           plot = grafico, width = 30, height = 15, units = c("cm"), dpi = 600, limitsize = TRUE)
    
    ## Distancia Media Fora (maes que deslocaram)
    seq_breaks <- c(round(as.numeric(quantile(dados_grafico$Delta_C, 
                                              c(.125,.25,.375,.50,.625,.75,.875), na.rm = TRUE)),1))
    grafico <- dados_grafico %>% filter(!is.na(Delta_C)) %>% ggplot() + 
      geom_sf(aes(fill = Delta_C), size=.15) + 
      scale_fill_fermenter(breaks = seq_breaks, palette = "Blues", direction = 1) +
      facet_wrap(vars(ref)) + 
      theme_void(base_size = 10) + theme(legend.position = 'right') +
      labs(title = "Delta_C", 
           subtitle = "", fill = "Delta_C: ")
    
    ggsave(filename = paste(Endereco_saida_dados,"01_Delta_C_",i,".png", sep =""),
           plot = grafico, width = 30, height = 15, units = c("cm"), dpi = 600, limitsize = TRUE)
    
    ## Distancia Media Total (todas as maes)
    seq_breaks <- c(round(as.numeric(quantile(dados_grafico$Delta_D, 
                                              c(.125,.25,.375,.50,.625,.75,.875), na.rm = TRUE)),1))
    grafico <- dados_grafico %>% filter(!is.na(Delta_D)) %>% ggplot() + 
      geom_sf(aes(fill = Delta_D), size=.15) + 
      scale_fill_fermenter(breaks = seq_breaks, palette = "Blues", direction = 1) +
      facet_wrap(vars(ref)) + 
      theme_void(base_size = 10) + theme(legend.position = 'right') +
      labs(title = "Delta_D", 
           subtitle = "", fill = "Delta_D: ")
    
    ggsave(filename = paste(Endereco_saida_dados,"01_Delta_D_",i,".png", sep =""),
           plot = grafico, width = 30, height = 15, units = c("cm"), dpi = 600, limitsize = TRUE)
    
  }
  
  if(i == "resumo_REGSAUDE" | i == "resumo_MUNIC"){ ## sem linha entre as regioes
    
    ## PP nascimentos
    seq_breaks <- c(round(as.numeric(quantile(dados_grafico$Delta_F, 
                                              c(.125,.25,.375,.50,.625,.75,.875), na.rm = TRUE)),2))
    grafico <- dados_grafico %>% filter(!is.na(Delta_F)) %>% ggplot() + 
      geom_sf(aes(fill = Delta_F), color = NA, size=.15) + 
      scale_fill_fermenter(breaks = seq_breaks, palette = "Blues", direction = 1) +
      facet_wrap(vars(ref)) + 
      theme_void(base_size = 10) + theme(legend.position = 'right') +
      labs(title = "Delta_F", 
           subtitle = "", fill = "Delta_F: ")
    
    ggsave(filename = paste(Endereco_saida_dados,"01_Delta_F_",i,".png", sep =""),
           plot = grafico, width = 30, height = 15, units = c("cm"), dpi = 600, limitsize = TRUE)
    
    ## Distancia Media Fora (maes que deslocaram)
    seq_breaks <- c(round(as.numeric(quantile(dados_grafico$Delta_C, 
                                              c(.125,.25,.375,.50,.625,.75,.875), na.rm = TRUE)),1))
    grafico <- dados_grafico %>% filter(!is.na(Delta_C)) %>% ggplot() + 
      geom_sf(aes(fill = Delta_C), color = NA, size=.15) + 
      scale_fill_fermenter(breaks = seq_breaks, palette = "Blues", direction = 1) +
      facet_wrap(vars(ref)) + 
      theme_void(base_size = 10) + theme(legend.position = 'right') +
      labs(title = "Delta_C", 
           subtitle = "", fill = "Delta_C: ")
    
    ggsave(filename = paste(Endereco_saida_dados,"01_Delta_C_",i,".png", sep =""),
           plot = grafico, width = 30, height = 15, units = c("cm"), dpi = 600, limitsize = TRUE)
    
    ## Distancia Media Total (todas as maes)
    seq_breaks <- c(round(as.numeric(quantile(dados_grafico$Delta_D, 
                                              c(.125,.25,.375,.50,.625,.75,.875), na.rm = TRUE)),1))
    grafico <- dados_grafico %>% filter(!is.na(Delta_D)) %>% ggplot() + 
      geom_sf(aes(fill = Delta_D), color = NA, size=.15) + 
      scale_fill_fermenter(breaks = seq_breaks, palette = "Blues", direction = 1) +
      facet_wrap(vars(ref)) + 
      theme_void(base_size = 10) + theme(legend.position = 'right') +
      labs(title = "Delta_D", 
           subtitle = "", fill = "Delta_D: ")
    
    ggsave(filename = paste(Endereco_saida_dados,"01_Delta_D_",i,".png", sep =""),
           plot = grafico, width = 30, height = 15, units = c("cm"), dpi = 600, limitsize = TRUE)
    
  }
  
  if(i != "resumo_BR"){rm(grafico,dados_grafico)}
  print(i)
  gc()
  
}




modelo <- resumo_MUNIC %>% group_by(ano_nasc, CODMUNRES) %>% summarise(
  D = dist_rota,
  F = pp_nasc_fora,
  C = dist_rota_deslocou,
  D2 = F*C,
)

modelo_07 <- modelo %>% filter(ano_nasc == 2007)
modelo_17 <- modelo %>% filter(ano_nasc == 2017)

modelo_07 <- rename(modelo_07, "D_07" = "D")
modelo_07 <- rename(modelo_07, "F_07" = "F")
modelo_07 <- rename(modelo_07, "C_07" = "C")
modelo_07 <- rename(modelo_07, "D2_07" = "D2")

modelo_17 <- rename(modelo_17, "D_17" = "D")
modelo_17 <- rename(modelo_17, "F_17" = "F")
modelo_17 <- rename(modelo_17, "C_17" = "C")
modelo_17 <- rename(modelo_17, "D2_17" = "D2")

modelo_final <- left_join(modelo_07, modelo_17, by = "CODMUNRES")

rm(modelo_07, modelo_17)
gc()

# A variacaoo nas distancias medias (incluindo as que não viajam), D, pode ser dada  (aproximadamente)
# por: ∆D=∆F∗C+F∗∆C Onde o termo ∆F∗C representa “mais pessoas viajando” e o segundo F∗∆C representa
# as “distancias ficando maiores” em média para quem viaja. Queremos apresentar esses  três 
# componentes em mapas: Por regiao, uf, microrregiao de saude, municipio e faixas de tamanho dos municipios.

modelo_final <- modelo_final %>% mutate(
  Delta_F = (F_17 - F_07),
  Delta_C = (C_17 - C_07),
  Delta_D = (Delta_F)*C_17 + F_17*(Delta_C)
)

library(geobr)
library(sf)

GEO_MUNIC_BR <- readRDS("E:/Documentos/Banco de Dados/Dados GeoBR/municipality_shape_2020.rds")
GEO_MUNIC_BR$code_muni6 <- as.numeric(str_sub(GEO_MUNIC_BR$code_muni,1,6))
modelo_final$CODMUNRES <- as.numeric(modelo_final$CODMUNRES)

dados_grafico <- left_join(GEO_MUNIC_BR, modelo_final, by = c("code_muni6" = "CODMUNRES"))

seq_breaks <- c(round(as.numeric(quantile(dados_grafico$Delta_C, 
                                          c(.125,.25,.375,.50,.625,.75,.875), na.rm = TRUE)),2))
grafico <- dados_grafico %>% filter(!is.na(Delta_C)) %>% ggplot() + 
  geom_sf(aes(fill = Delta_C), color = NA, size=.15) + 
  scale_fill_fermenter(breaks = seq_breaks, palette = "Purples", direction = 1) +
  #facet_wrap(vars(ano_nasc)) + 
  theme_void(base_size = 10) + theme(legend.position = 'right') +
  labs(title = "Variação da Distância", 
       subtitle = "", fill = "Delta C: ")

grafico

modelo_uf <- resumo_UF %>% group_by(ano_nasc, res_SIGLA_UF) %>% summarise(
  D = dist_rota,
  F = pp_nasc_fora,
  C = dist_rota_deslocou,
  D2 = F*C,
)

modelo_07 <- modelo_uf %>% filter(ano_nasc == 2007)
modelo_17 <- modelo_uf %>% filter(ano_nasc == 2017)

modelo_07 <- rename(modelo_07, "D_07" = "D")
modelo_07 <- rename(modelo_07, "F_07" = "F")
modelo_07 <- rename(modelo_07, "C_07" = "C")
modelo_07 <- rename(modelo_07, "D2_07" = "D2")

modelo_17 <- rename(modelo_17, "D_17" = "D")
modelo_17 <- rename(modelo_17, "F_17" = "F")
modelo_17 <- rename(modelo_17, "C_17" = "C")
modelo_17 <- rename(modelo_17, "D2_17" = "D2")

modelo_uf_final <- left_join(modelo_07, modelo_17, by = "res_SIGLA_UF")

modelo_uf_final <- modelo_uf_final %>% mutate(
  Delta_F = (F_17 - F_07),
  Delta_C = (C_17 - C_07),
  Delta_D = (Delta_F)*C_17 + F_17*(Delta_C)
)

rm(modelo_07, modelo_17)
gc()

GEO_UF_BR <- readRDS("E:/Documentos/Banco de Dados/Dados GeoBR/state_shape_2020.rds")

dados_grafico_uf <- left_join(GEO_UF_BR, modelo_uf_final, by = c("abbrev_state" = "res_SIGLA_UF"))

seq_breaks <- c(round(as.numeric(quantile(dados_grafico_uf$Delta_C, 
                                          c(.125,.25,.375,.50,.625,.75,.875), na.rm = TRUE)),2))
grafico <- dados_grafico_uf %>% filter(!is.na(Delta_C)) %>% ggplot() + 
  geom_sf(aes(fill = Delta_C), size=.15) + 
  scale_fill_fermenter(breaks = seq_breaks, palette = "Purples", direction = 1) +
  #facet_wrap(vars(ano_nasc)) + 
  theme_void(base_size = 10) + theme(legend.position = 'right') +
  labs(title = "Variação da Distância", 
       subtitle = "", fill = "Delta C: ")

grafico


















###### Mapas: MUNICIPIOS: ###### 

## Merge dos shapefiles com os dados para plotar:
regiao_grafico <- left_join(GEO_REGIAO, resumo_res_REGIAO, by = c("name_region" = "res_REGIAO"))
munic_grafico <- left_join(GEO_MUNIC_BR, resumo_CODMUNRES, by = c("code_muni6" = "CODMUNRES"))
uf_grafico <- left_join(GEO_UF, resumo_res_SIGLA_UF, by = c("SIGLA_UF" = "res_SIGLA_UF"))
saude_grafico <- left_join(GEO_SAUDE, resumo_COD_REGSAUDE_RES, by = c("regiao" = "COD_REGSAUDE_RES"))

rm(GEO_UF, GEO_SAUDE, GEO_REGIAO, GEO_MUNIC_BR)
gc()

## Loop para os graficos:
lista_regioes <- c("res_REGIAO", "res_NOME_UF", "COD_REGSAUDE_RES", "CODMUNRES")

for(i in lista_regioes){
  
  Endereco_saida_dados <- "E:/accessibility/output/descritiva_distancias/"
  
  if(i == "res_REGIAO"){dados_grafico = regiao_grafico}
  if(i == "res_NOME_UF"){dados_grafico = uf_grafico}
  if(i == "COD_REGSAUDE_RES"){dados_grafico = saude_grafico}
  if(i == "CODMUNRES"){dados_grafico = munic_grafico}
  
  dados_grafico <- dados_grafico %>% filter(ano_nasc == "2007" | ano_nasc == "2012" | ano_nasc == "2017")
  
  if(i == "res_REGIAO" | i == "res_NOME_UF"){ ## com linha entre as regioes
    
    ## PP nascimentos
    seq_breaks <- c(round(as.numeric(quantile(dados_grafico$pp_nasc_fora, 
                                              c(.125,.25,.375,.50,.625,.75,.875), na.rm = TRUE)),2))
    grafico <- dados_grafico %>% filter(!is.na(pp_nasc_fora)) %>% ggplot() + 
      geom_sf(aes(fill = pp_nasc_fora), size=.15) + 
      scale_fill_fermenter(breaks = seq_breaks, palette = "Oranges", direction = 1) +
      facet_wrap(vars(ano_nasc)) + 
      theme_void(base_size = 10) + theme(legend.position = 'right') +
      labs(title = "Percentual de Nascimentos que acontecem fora do Municipio de Residencia das Maes", 
           subtitle = "", fill = "% Nascimentos: ")
    
    ggsave(filename = paste(Endereco_saida_dados,"01_PP_Nasc_fora_rotas_",i,".png", sep =""),
           plot = grafico, width = 20, height = 20, units = c("cm"), dpi = 600, limitsize = TRUE)
    
    ## Distancia Media Fora (maes que deslocaram)
    seq_breaks <- c(round(as.numeric(quantile(dados_grafico$dist_rota_deslocou, 
                                              c(.125,.25,.375,.50,.625,.75,.875), na.rm = TRUE)),1))
    grafico <- dados_grafico %>% filter(!is.na(dist_rota_deslocou)) %>% ggplot() + 
      geom_sf(aes(fill = dist_rota_deslocou), size=.15) + 
      scale_fill_fermenter(breaks = seq_breaks, palette = "Purples", direction = 1) +
      facet_wrap(vars(ano_nasc)) + 
      theme_void(base_size = 10) + theme(legend.position = 'right') +
      labs(title = "Distancia media percorrida pelas Maes do Municipio que deslocaram", 
           subtitle = "", fill = "Distancia em Km: ")
    
    ggsave(filename = paste(Endereco_saida_dados,"02_Dist_Media_rotas_",i,".png", sep =""),
           plot = grafico, width = 20, height = 20, units = c("cm"), dpi = 600, limitsize = TRUE)
    
    ## Distancia Media Total (todas as maes)
    seq_breaks <- c(round(as.numeric(quantile(dados_grafico$dist_rota, 
                                              c(.125,.25,.375,.50,.625,.75,.875), na.rm = TRUE)),1))
    grafico <- dados_grafico %>% filter(!is.na(dist_rota)) %>% ggplot() + 
      geom_sf(aes(fill = dist_rota), size=.15) + 
      scale_fill_fermenter(breaks = seq_breaks, palette = "Purples", direction = 1) +
      facet_wrap(vars(ano_nasc)) + 
      theme_void(base_size = 10) + theme(legend.position = 'right') +
      labs(title = "Distancia media percorrida considerando todas as Maes do Municipio", 
           subtitle = "", fill = "Distancia em Km: ")
    
    ggsave(filename = paste(Endereco_saida_dados,"03_Dist_total_rotas_",i,".png", sep =""),
           plot = grafico, width = 20, height = 20, units = c("cm"), dpi = 600, limitsize = TRUE)
    
  }
  
  if(i == "CODMUNRES" | i == "COD_REGSAUDE_RES"){ ## sem linha entre as regioes
    
    ## PP nascimentos
    seq_breaks <- c(round(as.numeric(quantile(dados_grafico$pp_nasc_fora, 
                                              c(.125,.25,.375,.50,.625,.75,.875), na.rm = TRUE)),2))
    grafico <- dados_grafico %>% filter(!is.na(pp_nasc_fora)) %>% ggplot() + 
      geom_sf(aes(fill = pp_nasc_fora), color = NA, size=.15) + 
      scale_fill_fermenter(breaks = seq_breaks, palette = "Oranges", direction = 1) +
      facet_wrap(vars(ano_nasc)) + 
      theme_void(base_size = 10) + theme(legend.position = 'right') +
      labs(title = "Percentual de Nascimentos que acontecem fora do Municipio de Residencia das Maes", 
           subtitle = "", fill = "% Nascimentos: ")
    
    ggsave(filename = paste(Endereco_saida_dados,"01_PP_Nasc_fora_rotas_",i,".png", sep =""),
           plot = grafico, width = 20, height = 20, units = c("cm"), dpi = 600, limitsize = TRUE)
    
    ## Distancia Media Fora (maes que deslocaram)
    seq_breaks <- c(round(as.numeric(quantile(dados_grafico$dist_rota_deslocou, 
                                              c(.125,.25,.375,.50,.625,.75,.875), na.rm = TRUE)),1))
    grafico <- dados_grafico %>% filter(!is.na(dist_rota_deslocou)) %>% ggplot() + 
      geom_sf(aes(fill = dist_rota_deslocou), color = NA, size=.15) + 
      scale_fill_fermenter(breaks = seq_breaks, palette = "Purples", direction = 1) +
      facet_wrap(vars(ano_nasc)) + 
      theme_void(base_size = 10) + theme(legend.position = 'right') +
      labs(title = "Distancia media percorrida pelas Maes do Municipio que deslocaram", 
           subtitle = "", fill = "Distancia em Km: ")
    
    ggsave(filename = paste(Endereco_saida_dados,"02_Dist_Media_rotas_",i,".png", sep =""),
           plot = grafico, width = 20, height = 20, units = c("cm"), dpi = 600, limitsize = TRUE)
    
    ## Distancia Media Total (todas as maes)
    seq_breaks <- c(round(as.numeric(quantile(dados_grafico$dist_rota, 
                                              c(.125,.25,.375,.50,.625,.75,.875), na.rm = TRUE)),1))
    grafico <- dados_grafico %>% filter(!is.na(dist_rota)) %>% ggplot() + 
      geom_sf(aes(fill = dist_rota), color = NA, size=.15) + 
      scale_fill_fermenter(breaks = seq_breaks, palette = "Purples", direction = 1) +
      facet_wrap(vars(ano_nasc)) + 
      theme_void(base_size = 10) + theme(legend.position = 'right') +
      labs(title = "Distancia media percorrida considerando todas as Maes do Municipio", 
           subtitle = "", fill = "Distancia em Km: ")
    
    ggsave(filename = paste(Endereco_saida_dados,"03_Dist_total_rotas_",i,".png", sep =""),
           plot = grafico, width = 20, height = 20, units = c("cm"), dpi = 600, limitsize = TRUE)
    
  }
  
}

rm(lista_regioes, dados_grafico, munic_grafico, saude_grafico, regiao_grafico, uf_grafico, 
   i, grafico, seq_breaks)
gc()