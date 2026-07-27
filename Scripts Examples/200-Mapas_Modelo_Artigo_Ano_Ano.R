###################################################################################################
###################################################################################################

#### MODELO PARA VARIACAO DAS DISTANTICAS ####

rm(list = ls(all.names=TRUE)) # limpar area de trabalho do R
gc()

library(tidyverse)
library(geobr)
library(cowplot)
library(dplyr)

######### RECORTES GEOGRAFICOS ######### 

resumo_BR <- readRDS("E:/Documentos/Data Paper - Distancias SINASC/resumo_BR.rds")
resumo_BR$BR = "BR"
resumo_BR <- resumo_BR[,c(1,103,2:102)]

resumo_UF <- readRDS("E:/Documentos/Data Paper - Distancias SINASC/resumo_UF.rds")

## Modelo do Artigo ano por ano:

REF_BASES <- c("resumo_BR", "resumo_UF")

for (i in REF_BASES) {
  
  if(i == "resumo_BR"){base_dados = resumo_BR
  modelo <- base_dados %>% select(BR, ano_nasc, dist_rota, pp_nasc_fora, dist_rota_deslocou)
  modelo_2 <- modelo %>% filter(ano_nasc == "2007" | ano_nasc == "2017")
  }

  if(i == "resumo_UF"){base_dados = resumo_UF
  modelo <- base_dados %>% select(res_SIGLA_UF, ano_nasc, dist_rota, pp_nasc_fora, dist_rota_deslocou)
  modelo <- modelo %>% arrange(res_SIGLA_UF, ano_nasc)
  modelo_2 <- modelo %>% filter(ano_nasc == "2007" | ano_nasc == "2017")
  }
  
  # PARTE 1 - Modelo Ano a Ano:
  modelo$lag_dist_rota <- lag(modelo$dist_rota)
  modelo$lag_pp_nasc_fora <- lag(modelo$pp_nasc_fora)
  modelo$lag_dist_rota_deslocou <- lag(modelo$dist_rota_deslocou)
  
  modelo$delta_D <- modelo$dist_rota - modelo$lag_dist_rota
  modelo$delta_F <- modelo$pp_nasc_fora - modelo$lag_pp_nasc_fora
  modelo$delta_C <- modelo$dist_rota_deslocou - modelo$lag_dist_rota_deslocou
  
  if(i == "resumo_BR"){modelo <- modelo %>% group_by(BR) %>% mutate(ref_cal = paste("Delta: ",c(6:17),"-",c(5:16),sep = ""))}
  if(i == "resumo_UF"){modelo <- modelo %>% group_by(res_SIGLA_UF) %>% mutate(ref_cal = paste("Delta: ",c(6:17),"-",c(5:16),sep = ""))}
  
  modelo$formula_delta_D <- ((modelo$delta_F)*modelo$dist_rota_deslocou) +
    (modelo$pp_nasc_fora*(modelo$delta_C)) # Delta_D = (Delta_F)*C_17 + F_17*(Delta_C),
  
  assign(paste("modelo",i, sep = "_"), modelo)
  
  # PARTE 2 - Modelo para dois anos:
  
  modelo_2$lag_dist_rota <- lag(modelo_2$dist_rota)
  modelo_2$lag_pp_nasc_fora <- lag(modelo_2$pp_nasc_fora)
  modelo_2$lag_dist_rota_deslocou <- lag(modelo_2$dist_rota_deslocou)
  
  modelo_2$delta_D <- modelo_2$dist_rota - modelo_2$lag_dist_rota
  modelo_2$delta_F <- modelo_2$pp_nasc_fora - modelo_2$lag_pp_nasc_fora
  modelo_2$delta_C <- modelo_2$dist_rota_deslocou - modelo_2$lag_dist_rota_deslocou
  
  if(i == "resumo_BR"){modelo_2 <- modelo_2 %>% group_by(BR) %>% mutate(ref_cal = paste("Delta: ",c(7,17),"-",c(0,7),sep = ""))}
  if(i == "resumo_UF"){modelo_2 <- modelo_2 %>% group_by(res_SIGLA_UF) %>% mutate(ref_cal = paste("Delta: ",c(7,17),"-",c(0,7),sep = ""))}
  
  modelo_2$formula_delta_D <- ((modelo_2$delta_F)*modelo_2$dist_rota_deslocou) +
    (modelo_2$pp_nasc_fora*(modelo_2$delta_C)) # Delta_D = (Delta_F)*C_17 + F_17*(Delta_C),
  
  assign(paste("modelo_2",i, sep = "_"), modelo_2)
  
  
  rm(base_dados, modelo, modelo_2)
}

######################################################################
## Grafico 1 do artigo:

resumo_BR$ano_nasc <- as.numeric(resumo_BR$ano_nasc)
resumo_BR$pp_nasc_fora <- 100*(resumo_BR$pp_nasc_fora)

par(mar=c(5, 4, 4, 6) + 0.1)

# criar o primeiro eixo Y
plot(x = resumo_BR$ano_nasc, y = resumo_BR$pp_nasc_fora, lty=1, axes=TRUE, 
     xlab="", ylab="", type="l", lwd = 3,
     col="orange", main = "Displacements and Distance") # grafico para a variavel X, orange
axis(col="orange", las=1) # etiqueta horizontal
mtext("% of Displacements", side=2, line=2.5) # nomear o primeiro eixo Y 
box() # criar uma caixa para o grafico

# criar o segundo eixo Y
par(new=TRUE)
plot(x = resumo_BR$ano_nasc, y = resumo_BR$dist_rota_deslocou, lty=2,  xlab="", ylab="", 
     axes=FALSE, type="l", lwd = 3, col="purple") # purple
mtext("Average Distance (Km)",side=4,col="black",line=4) 
axis(4, ylim=c(2,60), col="black",col.axis="black",las=1)
axis(1, at = seq(1,22,1))
# adicionar uma legenda ao grafico
legend("topleft",legend=c("Displacements","Distance"),
       text.col=c("orange","purple"),lty=c(1,2),col=c("orange","purple")) # "black","steelblue"

######################################################################
## Grafico 2 - Modelo do artigo:

modelo_resumo_BR <- modelo_resumo_BR %>% filter(!is.na(delta_D))
modelo_resumo_BR$ano_nasc <- as.numeric(modelo_resumo_BR$ano_nasc)

par(mar=c(5, 4, 4, 6) + 0.1)

# criar o primeiro eixo Y
plot(x = modelo_resumo_BR$ano_nasc, y = 100*(modelo_resumo_BR$delta_F), lty=1, axes=TRUE, 
     xlab="", ylab="", type="l", lwd = 3,
     col="orange", main = "Displacements and Distance") # grafico para a variavel X, orange
axis(col="black", las=1) # etiqueta horizontal
mtext("% of Displacements", side=2, line=2.5) # nomear o primeiro eixo Y 
box() # criar uma caixa para o grafico

# criar o segundo eixo Y
par(new=TRUE)
plot(x = modelo_resumo_BR$ano_nasc, y = modelo_resumo_BR$delta_D, 
     lty=2,  xlab="", ylab="", 
     axes=FALSE, type="l", lwd = 3, col="purple") # purple
#mtext("Average Distance (Km)",side=4,col="purple",line=4) 
axis(4, ylim=modelo_resumo_BR$delta_D, 
     col="purple",col.axis="purple",las=1)
#axis(1, at = seq(1,22,1))
# adicionar uma legenda ao grafico

par(new=TRUE)
plot(x = modelo_resumo_BR$ano_nasc, y = modelo_resumo_BR$delta_C, lty=3,  xlab="", ylab="", 
     axes=FALSE, type="l", lwd = 3, col="blue") # blue
mtext("Average Distance (Km)",side=4,col="black",line=4) 
axis(4, ylim=modelo_resumo_BR$delta_C, col="black",col.axis="black",las=1)
#axis(1, at = seq(1,22,1))
# adicionar uma legenda ao grafico
legend("bottomleft",legend=c("Displacements","Distance","Distance (Displacements)"),
       text.col=c("orange","purple","blue"),lty=c(1,2),col=c("orange","purple","blue")) # "black","steelblue"

## ###################################

dado_1 <- modelo_resumo_BR %>% select(BR, ano_nasc, ref_cal, delta_F)
dado_1$ref <- "delta_F"
dado_1 <- rename(dado_1, "valor_plot" = "delta_F")

dado_2 <- modelo_resumo_BR %>% select(BR, ano_nasc, ref_cal, delta_C)
dado_2$ref <- "delta_C"
dado_2 <- rename(dado_2, "valor_plot" = "delta_C")

dado_3 <- modelo_resumo_BR %>% select(BR, ano_nasc, ref_cal, delta_D)
dado_3$ref <- "delta_D"
dado_3 <- rename(dado_3, "valor_plot" = "delta_D")

dado_4 <- modelo_resumo_BR %>% select(BR, ano_nasc, ref_cal, formula_delta_D)
dado_4$ref <- "formula_delta_D"
dado_4 <- rename(dado_4, "valor_plot" = "formula_delta_D")

dados_plot_BR <- rbind(dado_1, dado_2, dado_3, dado_4)
rm(dado_1, dado_2, dado_3, dado_4)

# delta_F, delta_C, delta_D, formula_delta_D

dados_plot_BR$ref_cal <- factor(dados_plot_BR$ref_cal,
                                levels = c("Delta: 7-6",
                                           "Delta: 8-7",
                                           "Delta: 9-8",
                                           "Delta: 10-9",
                                           "Delta: 11-10",
                                           "Delta: 12-11",
                                           "Delta: 13-12",
                                           "Delta: 14-13",
                                           "Delta: 15-14",
                                           "Delta: 16-15",
                                           "Delta: 17-16"))

ggplot(dados_plot_BR) +
  aes(x = ref_cal, fill = ref, colour = ref, weight = valor_plot) +
  geom_bar() +
  scale_fill_manual(
    values = c(delta_C = "#440154",
               delta_D = "#31688E",
               delta_F = "#35B779",
               formula_delta_D = "#FDE725")
  ) +
  scale_color_manual(
    values = c(delta_C = "#440154",
               delta_D = "#31688E",
               delta_F = "#35B779",
               formula_delta_D = "#FDE725")
  ) +
  theme_minimal() +
  facet_wrap(vars(ref), scales = "free_y")

## ###################################

# Grafico para o BR - diferenca entre 2017 e 2007 (10 anos)
dado_1 <- modelo_2_resumo_BR %>% select(BR, ano_nasc, ref_cal, 
                                        pp_nasc_fora, lag_pp_nasc_fora, delta_F)
dado_1$ref <- "delta_F"
dado_1 <- rename(dado_1, "valor_plot" = "delta_F")
dado_1 <- rename(dado_1, "vf" = "pp_nasc_fora")
dado_1 <- rename(dado_1, "vi" = "lag_pp_nasc_fora")
dado_1 <- dado_1 %>% mutate(vr_pp = (vf-vi)/(vi))

dado_2 <- modelo_2_resumo_BR %>% select(BR, ano_nasc, ref_cal, 
                                        dist_rota_deslocou, lag_dist_rota_deslocou, delta_C)
dado_2$ref <- "delta_C"
dado_2 <- rename(dado_2, "valor_plot" = "delta_C")
dado_2 <- rename(dado_2, "vf" = "dist_rota_deslocou")
dado_2 <- rename(dado_2, "vi" = "lag_dist_rota_deslocou")
dado_2 <- dado_2 %>% mutate(vr_pp = (vf-vi)/(vi))

dado_3 <- modelo_2_resumo_BR %>% select(BR, ano_nasc, ref_cal, 
                                        dist_rota, lag_dist_rota,delta_D)
dado_3$ref <- "delta_D"
dado_3 <- rename(dado_3, "valor_plot" = "delta_D")
dado_3 <- rename(dado_3, "vf" = "dist_rota")
dado_3 <- rename(dado_3, "vi" = "lag_dist_rota")
dado_3 <- dado_3 %>% mutate(vr_pp = (vf-vi)/(vi))

dados_plot_BR <- rbind(dado_1, dado_2, dado_3)
rm(dado_1, dado_2, dado_3)

# delta_F, delta_C, delta_D, formula_delta_D

dados_plot_BR <- dados_plot_BR %>% filter(ref_cal == "Delta: 17-7" & !is.na(vr_pp))
dados_plot_BR$ref <- ifelse(dados_plot_BR$ref == "delta_C", "Delta C",
                            ifelse(dados_plot_BR$ref == "delta_F", "Delta F", "Delta D"))
dados_plot_BR$ref <- factor(dados_plot_BR$ref,
                            levels = c("Delta D","Delta F","Delta C"))

ggplot(dados_plot_BR) +
  aes(x = ref, fill = ref, colour = ref, weight = vr_pp) +
  geom_bar() +
  scale_fill_manual(
    values = c("Delta C" = "#BA55D3", "Delta D" = "#87CEEB", "Delta F" = "#FF6347")) +
  scale_color_manual(
    values = c("Delta C" = "#BA55D3", "Delta D" = "#87CEEB", "Delta F" = "#FF6347")) +
  labs(
    x = "Variables", y = "Percentage Change", title = "Percentage Change",
    subtitle = "", caption = "", fill = "Variable:", color = "Variable:") +
  theme_minimal() + theme(axis.text = element_text(size = 12),
                          axis.title = element_text(size = 15))

## colors: https://flaviocopes.com/rgb-color-codes/

######################################################################
## Grafico 3 - Mapas do Modelo do Artigo:

lista_regioes <- c("res_NOME_UF")

for(i in lista_regioes){
  
  ## merge com shapefile
  
  end_shape <- "E:/Documentos/Banco de Dados/Dados GeoBR/"
  
  GEO_REGIAO <- readRDS(paste(end_shape,"region_shape_2020.rds",sep = ""))
  GEO_REGIAO$name_region <- ifelse(GEO_REGIAO$name_region == "Centro Oeste", "Centro-Oeste", GEO_REGIAO$name_region)
  GEO_UF <- readRDS(paste(end_shape,"state_shape_2020.rds",sep = ""))
  
  if(i == "res_NOME_UF"){uf_grafico <- left_join(GEO_UF, modelo_resumo_UF, by = c("abbrev_state" = "res_SIGLA_UF"))}
  
  bordas_regiao <- GEO_REGIAO # gambiarra abaixo para juntar as bases:
  bordas_regiao <- rbind(bordas_regiao,bordas_regiao,bordas_regiao,bordas_regiao,bordas_regiao,
                         bordas_regiao,bordas_regiao,bordas_regiao,bordas_regiao,bordas_regiao,
                         bordas_regiao)
  bordas_regiao <- bordas_regiao %>% group_by(code_region) %>% mutate(ano_nasc = c(2007:2017))
  # bordas_regiao <- bordas_regiao %>% filter(ano_nasc == "2007" | ano_nasc == "2012" | ano_nasc == "2017")
  
  dados_grafico <- uf_grafico %>% filter(!is.na(delta_C) & ano_nasc != "2006")
  
  # DELTA C
  seq_breaks <- c(round(as.numeric(quantile(dados_grafico$delta_C, 
                                            c(.125,.25,.375,.50,.625,.75,.875), na.rm = TRUE)),2))
  grafico <- dados_grafico %>% ggplot() + 
    geom_sf(aes(fill = delta_C), size=.01) + 
    geom_sf(data = bordas_regiao$geom, fill = "transparent", size=0.4) +
    scale_fill_fermenter(breaks = seq_breaks, palette = "Oranges", direction = 1) +
    facet_wrap(vars(ano_nasc)) + 
    theme_void(base_size = 10) + theme(legend.position = 'right') +
    labs(title = "Displacement x Distances", subtitle = "", fill = "Delta C: ")
  
  Endereco_saida_dados <- "E:/Documentos/Data Paper - Distancias SINASC/Paper Adjustments/New Maps/"
  
  ggsave(filename = paste(Endereco_saida_dados,"Dist_Displa_Delta_C_",i,".png", sep =""),
         plot = grafico, width = 30, height = 15, units = c("cm"), dpi = 600, limitsize = TRUE)
  
  # DELTA F
  dados_grafico$delta_F <- 100*(dados_grafico$delta_F)
  
  seq_breaks <- c(round(as.numeric(quantile(dados_grafico$delta_F, 
                                            c(.125,.25,.375,.50,.625,.75,.875), na.rm = TRUE)),2))
  grafico <- dados_grafico %>% ggplot() + 
    geom_sf(aes(fill = delta_F), size=.01) + 
    geom_sf(data = bordas_regiao$geom, fill = "transparent", size=0.4) +
    scale_fill_fermenter(breaks = seq_breaks, palette = "Oranges", direction = 1) +
    facet_wrap(vars(ano_nasc)) + 
    theme_void(base_size = 10) + theme(legend.position = 'right') +
    labs(title = "Displacement x Distances", subtitle = "", fill = "Delta F: ")
  
  ggsave(filename = paste(Endereco_saida_dados,"Dist_Displa_Delta_F_",i,".png", sep =""),
         plot = grafico, width = 30, height = 15, units = c("cm"), dpi = 600, limitsize = TRUE)
  
  # DELTA D
  seq_breaks <- c(round(as.numeric(quantile(dados_grafico$delta_D, 
                                            c(.125,.25,.375,.50,.625,.75,.875), na.rm = TRUE)),2))
  grafico <- dados_grafico %>% ggplot() + 
    geom_sf(aes(fill = delta_D), size=.01) + 
    geom_sf(data = bordas_regiao$geom, fill = "transparent", size=0.4) +
    scale_fill_fermenter(breaks = seq_breaks, palette = "Oranges", direction = 1) +
    facet_wrap(vars(ano_nasc)) + 
    theme_void(base_size = 10) + theme(legend.position = 'right') +
    labs(title = "Displacement x Distances", subtitle = "", fill = "Delta D: ")
  
  ggsave(filename = paste(Endereco_saida_dados,"Dist_Displa_Delta_D_",i,".png", sep =""),
         plot = grafico, width = 30, height = 15, units = c("cm"), dpi = 600, limitsize = TRUE)
  
  # DELTA D - Formula
  seq_breaks <- c(round(as.numeric(quantile(dados_grafico$formula_delta_D, 
                                            c(.125,.25,.375,.50,.625,.75,.875), na.rm = TRUE)),2))
  grafico <- dados_grafico %>% ggplot() + 
    geom_sf(aes(fill = formula_delta_D), size=.01) + 
    geom_sf(data = bordas_regiao$geom, fill = "transparent", size=0.4) +
    scale_fill_fermenter(breaks = seq_breaks, palette = "Oranges", direction = 1) +
    facet_wrap(vars(ano_nasc)) + 
    theme_void(base_size = 10) + theme(legend.position = 'right') +
    labs(title = "Displacement x Distances", subtitle = "", fill = "Delta D: ")
  
  ggsave(filename = paste(Endereco_saida_dados,"Dist_Displa_Formula_Delta_D_",i,".png", sep =""),
         plot = grafico, width = 30, height = 15, units = c("cm"), dpi = 600, limitsize = TRUE)
  
  rm(grafico)
  gc()
}

######################################################################
## Grafico 4 - Mapas do Modelo do Artigo - somente para diferenca de 2017-2007:

library(cowplot) # juntar graficos

lista_regioes <- c("res_NOME_UF")

for(i in lista_regioes){
  
  ## merge com shapefile
  
  end_shape <- "E:/Documentos/Banco de Dados/Dados GeoBR/"
  
  GEO_REGIAO <- readRDS(paste(end_shape,"region_shape_2020.rds",sep = ""))
  GEO_REGIAO$name_region <- ifelse(GEO_REGIAO$name_region == "Centro Oeste", "Centro-Oeste", GEO_REGIAO$name_region)
  GEO_UF <- readRDS(paste(end_shape,"state_shape_2020.rds",sep = ""))
  
  if(i == "res_NOME_UF"){uf_grafico <- left_join(GEO_UF, modelo_2_resumo_UF, by = c("abbrev_state" = "res_SIGLA_UF"))}
  
  bordas_regiao <- GEO_REGIAO # gambiarra abaixo para juntar as bases:
  bordas_regiao <- rbind(bordas_regiao,bordas_regiao)
  bordas_regiao <- bordas_regiao %>% group_by(code_region) %>% mutate(ano_nasc = c(2007,2017))
  # bordas_regiao <- bordas_regiao %>% filter(ano_nasc == "2007" | ano_nasc == "2012" | ano_nasc == "2017")
  
  dados_grafico <- uf_grafico %>% filter(!is.na(delta_C) & ano_nasc != "2007")
  
  # DELTA C
  seq_breaks <- c(round(as.numeric(quantile(dados_grafico$delta_C, 
                                            c(.125,.25,.375,.50,.625,.75,.875), na.rm = TRUE)),2))
  grafico_c <- dados_grafico %>% ggplot() + 
    geom_sf(aes(fill = delta_C), size=.01) + 
    geom_sf(data = bordas_regiao$geom, fill = "transparent", size=0.4) +
    scale_fill_fermenter(breaks = seq_breaks, palette = "Oranges", direction = 1) +
    #facet_wrap(vars(ano_nasc)) + 
    theme_void(base_size = 10) + theme(legend.position = 'right') +
    labs(title = "Displacement x Distances (2017-2007)", subtitle = "", fill = "Delta C: ")
  
  Endereco_saida_dados <- "E:/Documentos/Data Paper - Distancias SINASC/Paper Adjustments/New Maps/"
  
  #ggsave(filename = paste(Endereco_saida_dados,"Dist_Displa_Delta_C_",i,".png", sep =""),
  #       plot = grafico, width = 30, height = 15, units = c("cm"), dpi = 600, limitsize = TRUE)
  
  # DELTA F
  dados_grafico$delta_F <- 100*(dados_grafico$delta_F)
  
  seq_breaks <- c(round(as.numeric(quantile(dados_grafico$delta_F, 
                                            c(.125,.25,.375,.50,.625,.75,.875), na.rm = TRUE)),2))
  grafico_f <- dados_grafico %>% ggplot() + 
    geom_sf(aes(fill = delta_F), size=.01) + 
    geom_sf(data = bordas_regiao$geom, fill = "transparent", size=0.4) +
    scale_fill_fermenter(breaks = seq_breaks, palette = "Oranges", direction = 1) +
    #facet_wrap(vars(ano_nasc)) + 
    theme_void(base_size = 10) + theme(legend.position = 'right') +
    labs(fill = "Delta F: ")
  
  #ggsave(filename = paste(Endereco_saida_dados,"Dist_Displa_Delta_F_",i,".png", sep =""),
  #       plot = grafico, width = 30, height = 15, units = c("cm"), dpi = 600, limitsize = TRUE)
  
  # DELTA D
  seq_breaks <- c(round(as.numeric(quantile(dados_grafico$delta_D, 
                                            c(.125,.25,.375,.50,.625,.75,.875), na.rm = TRUE)),2))
  grafico_d <- dados_grafico %>% ggplot() + 
    geom_sf(aes(fill = delta_D), size=.01) + 
    geom_sf(data = bordas_regiao$geom, fill = "transparent", size=0.4) +
    scale_fill_fermenter(breaks = seq_breaks, palette = "Oranges", direction = 1) +
    #facet_wrap(vars(ano_nasc)) + 
    theme_void(base_size = 10) + theme(legend.position = 'right') +
    labs(fill = "Delta D: ")
  
  #ggsave(filename = paste(Endereco_saida_dados,"Dist_Displa_Delta_D_",i,".png", sep =""),
  #       plot = grafico, width = 30, height = 15, units = c("cm"), dpi = 600, limitsize = TRUE)
  
  # DELTA D - Formula
  seq_breaks <- c(round(as.numeric(quantile(dados_grafico$formula_delta_D, 
                                            c(.125,.25,.375,.50,.625,.75,.875), na.rm = TRUE)),2))
  grafico_fd <- dados_grafico %>% ggplot() + 
    geom_sf(aes(fill = formula_delta_D), size=.01) + 
    geom_sf(data = bordas_regiao$geom, fill = "transparent", size=0.4) +
    scale_fill_fermenter(breaks = seq_breaks, palette = "Oranges", direction = 1) +
    #facet_wrap(vars(ano_nasc)) + 
    theme_void(base_size = 10) + theme(legend.position = 'right') +
    labs(fill = "Cal. D: ")
  
  
  grafico <- plot_grid(grafico_c, grafico_f, grafico_d, ncol = 3)
  
  ggsave(filename = paste(Endereco_saida_dados,"Displacement_Distances_FCD_1_",i,".png", sep =""),
         plot = grafico, width = 30, height = 12, units = c("cm"), dpi = 600, limitsize = TRUE)
  
  grafico <- plot_grid(grafico_c, grafico_f, grafico_d, ncol = 1)
  
  ggsave(filename = paste(Endereco_saida_dados,"Displacement_Distances_FCD_2_",i,".png", sep =""),
         plot = grafico, width = 12, height = 30, units = c("cm"), dpi = 600, limitsize = TRUE)
  
  
  rm(grafico, grafico_c, grafico_f, grafico_d, grafico_fd)
  gc()
}
