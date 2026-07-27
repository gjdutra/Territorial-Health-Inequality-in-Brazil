##################################################################################################
##################################################################################################

## PROJETO: GCE - Descritivo das Distancias

rm(list = ls(all.names=TRUE)) # limpar area de trabalho do R

# pacotes iniciais

library(meta)
library(tidyverse)

Endereco_dados <- "E:/Documentos/Data Paper - Distancias SINASC/"

# 1 - Importando os dados ------
dados_ano <- readRDS(paste(Endereco_dados,"resumo_BR_ano_grafico_pp_v3.rds",sep = ""))
dados_completos <- readRDS(paste(Endereco_dados,"resumo_BR_base_grafico_pp_v3.rds",sep = ""))

dados_completos <- dados_completos %>% mutate(
  ref = ifelse(variavel_ref == 0 & ref == "obito_infantil", "Birth",
               ifelse(variavel_ref == 1000 & ref == "obito_infantil", "Newborn Death", ref)),
  
  ref = ifelse(variavel_ref == 0 & ref == "apgar_1_0_7", "Other",
               ifelse(variavel_ref == 1 & ref == "apgar_1_0_7", "Apgar1 8-", ref)),
  ref = ifelse(variavel_ref == 0 & ref == "apgar_1_8_10", "Apgar1 8+",
               ifelse(variavel_ref == 1 & ref == "apgar_1_8_10", "Apgar1 8+", ref)),
  
  ref = ifelse(variavel_ref == 0 & ref == "apgar_5_0_7", "Other",
               ifelse(variavel_ref == 1 & ref == "apgar_5_0_7", "Apgar5 8-", ref)),
  ref = ifelse(variavel_ref == 0 & ref == "apgar_5_8_10", "Apgar5 8+",
               ifelse(variavel_ref == 1 & ref == "apgar_5_8_10", "Apgar5 8+", ref)),
  
  ref = ifelse(variavel_ref == 0 & ref == "baixo_peso", "Low weight",
               ifelse(variavel_ref == 1 & ref == "baixo_peso", "Low weight", ref)),
  ref = ifelse(variavel_ref == 0 & ref == "muito_baixo_peso", "Other",
               ifelse(variavel_ref == 1 & ref == "muito_baixo_peso", "Very Low weight", ref)),
  
  ref = ifelse(variavel_ref == 0 & ref == "escol_ensfunda", ref,
               ifelse(variavel_ref == 1 & ref == "escol_ensfunda", "Elementary School", ref)),
  ref = ifelse(variavel_ref == 0 & ref == "escol_ensmedio", ref,
               ifelse(variavel_ref == 1 & ref == "escol_ensmedio", "High School", ref)),
  ref = ifelse(variavel_ref == 0 & ref == "escol_enssuper", ref,
               ifelse(variavel_ref == 1 & ref == "escol_enssuper", "Univeristy Education", ref)),
  ref = ifelse(variavel_ref == 0 & ref == "escol_ensmedio_mais", "High School +",
               ifelse(variavel_ref == 1 & ref == "escol_ensmedio_mais", "High School +", ref)),
  
  ref = ifelse(variavel_ref == 0 & ref == "getacao_abaixo_22sema", "Other",
               ifelse(variavel_ref == 1 & ref == "getacao_abaixo_22sema", "Pregnancy 23- weeks", ref)),
  ref = ifelse(variavel_ref == 0 & ref == "getacao_de_23_36sema", "Other",
               ifelse(variavel_ref == 1 & ref == "getacao_de_23_36sema", "Pregnancy 23 to 36 weeks", ref)),
  ref = ifelse(variavel_ref == 0 & ref == "getacao_acima_37sema", "Pregnancy 37+ weeks",
               ifelse(variavel_ref == 1 & ref == "getacao_acima_37sema", "Pregnancy 37+ weeks", ref)),
  
  ref = ifelse(variavel_ref == 0 & ref == "lrfb2", "Low Risk Pregnancy",
               ifelse(variavel_ref == 1 & ref == "lrfb2", "Low Risk Pregnancy", ref)),
  ref = ifelse(variavel_ref == 0 & ref == "parto_cesareo", "Cesarean",
               ifelse(variavel_ref == 1 & ref == "parto_cesareo", "Cesarean", ref)),
  ref = ifelse(variavel_ref == 0 & ref == "racacor_rn_branca", "White Newborn",
               ifelse(variavel_ref == 1 & ref == "racacor_rn_branca", "White Newborn", ref)),
  ref = ifelse(variavel_ref == 0 & ref == "gravidez_multi", "Multiple Pregnancy",
               ifelse(variavel_ref == 1 & ref == "gravidez_multi", "Multiple Pregnancy", ref)),
  
  ref = ifelse(variavel_ref == 0 & ref == "numero_consul_pre", "Prenatal 7+",
               ifelse(variavel_ref == 1 & ref == "numero_consul_pre", "Prenatal 7+", ref)),
  )

# Meta-analysis to estimate proportion of male infections

dados_completos <- dados_completos %>% filter(!is.na(ref) & (ref == "Apgar1 8+" |
                                                ref == "Apgar5 8+" |
                                                ref == "Cesarean" |
                                                ref == "High School +" | 
                                                ref == "Low Risk Pregnancy" |
                                                ref == "Pregnancy 37+ weeks" |
                                                ref == "White Newborn" | ref == "Low weight" |
                                                ref == "Multiple Pregnancy" | 
                                                ref == "Prenatal 7+"))

dados_completos2 <- dados_completos
dados_completos2$pp <- 100*(dados_completos2$numero_deslocou/dados_completos2$numero_nasc)
dados_completos2$Group <- ifelse(dados_completos2$variavel_ref == 0, "No", "Yes")
table(dados_completos2$ref, dados_completos2$Group)

dados_completos2$ref <- factor(dados_completos2$ref, 
                               levels = c("High School +", "Prenatal 7+", "Pregnancy 37+ weeks", 
                                          "Multiple Pregnancy", "Low Risk Pregnancy", 
                                          "Cesarean", "Low weight", "Apgar1 8+", "Apgar5 8+",
                                          "White Newborn"))

dados_completos2$ref <- factor(dados_completos2$ref, levels = rev(levels(dados_completos2$ref)))

dados_completos2 <- dados_completos2 %>% filter(ref != "White Newborn" &  ref != "Not White Newborn")

## Ex 1
dados_completos2 %>% ggplot() +
  aes(x = ref, y = as.factor(round(pp,0)), fill = factor(Group), colour = factor(Group)) +
  #geom_boxplot(shape = "circle") + 
  geom_jitter(size = 8) +
  scale_fill_manual(values = c("No" = "orange", "Yes" = "purple")) +
  scale_color_manual(values = c("No" = "orange", "Yes" = "purple")) +
  coord_flip() + theme_bw() +
  labs(title = "Displacement", x = "Variable", y = "% of displacement",
       colour = "Groups", fill = "Groups")

## Ex 2
ggplot(dados_completos2) +
  aes(x = ref, fill = Group, colour = Group, weight = pp) +
  geom_bar(position = "dodge") +
  scale_fill_manual(values = c(No = "#440154",Yes = "#FDE725")) +
  scale_color_manual(values = c(No = "#440154", Yes = "#FDE725")) +
  theme_minimal() + 
  labs(title = "Displacement", x = "Variable", y = "% of displacement",
       colour = "Groups", fill = "Groups")

## Ex 3
ggplot(dados_completos2) +
  aes(x = ref, fill = Group, colour = Group,  y = as.factor(round(pp,0))) +
  geom_jitter(width = 0.2, size = 10) +
  scale_fill_manual(values = c(No = "#440154",Yes = "#FDE725")) +
  scale_color_manual(values = c(No = "#440154", Yes = "#FDE725")) +
  theme_minimal() +
  labs(title = "Displacement", x = "Variable", y = "% of displacement",
       colour = "Groups", fill = "Groups") + coord_flip()

## Ex 4

dados_completos2$ref <- factor(dados_completos2$ref, levels = rev(levels(dados_completos2$ref)))

dados_completos2$pp <- as.numeric(round(dados_completos2$pp,0))

library(ggpubr)
ggdotchart(dados_completos2, x = "ref", y = "pp",
           color = "Group", palette = c("purple", "orange"), size = 7, # palette = "jco"
           #add = "segment", 
           #add.params = list(color = "transparent", size = 1.5),
           position = position_dodge(0.7),
           ggtheme = theme_bw() + theme(axis.text = element_text(size = 12),
                                        axis.title = element_text(size = 15)
                                        ),
           title = "Displacement",
           xlab = "Variable",
           ylab = "% of displacement",
           #label = TRUE, lab.pos = "out", lab.col = "black",
           #font.label = list(size = 14, face = "bold", color = "black"),
           ) + coord_flip()


