#####################################################################################################

################### PARTE 6 #######################

#### GRAFICOS:
rm(list = ls(all.names=TRUE)) # Limpar area de trabalho
gc()

pacotes = c("tidyverse", "dplyr", "readxl", "sf", "readr") 
lapply(pacotes, require, character.only = TRUE)

packages = c("tidyverse", "dplyr", "readr", "geobr", "readxl", "AER", "stats", 
             "jtools", "summarytools", "stargazer", "fixest", "modelsummary")
lapply(packages, require, character.only = T)

## Bases resumo SINASC - Modelo Econometrico:
resumo_CODMUNRES <- readRDS("E:/Documentos/Data Paper - Distancias SINASC/resumo_MUNIC2.rds")
resumo_CODMUNRES$CODMUNRES <- as.numeric(resumo_CODMUNRES$CODMUNRES)
resumo_CODMUNRES$ano_nasc <- as.numeric(resumo_CODMUNRES$ano_nasc)

ATLAS <- read_excel("E:/Documentos/Data Paper - Distancias SINASC/atlas2013_dadosbrutos_pt.xlsx", 
                    sheet = "MUN 91-00-10")

# Pegar um conjunto de variaveis no nivel de municipios, padroniza-las e regredir uma-a-uma 
# contra as distancias no nivel municipal. Plotar os betas como na figura anterior (pp que viajou)
# Outra opcao seria fazer um grafico de correlacoes.

ATLAS <- ATLAS %>% select(ANO, UF, Codmun6, ESPVIDA, FECTOT, MORT1, MORT5, T_ANALF18M,
                          T_FBFUND, T_FBMED, T_FBPRE, T_FBSUPER, GINI, PIND, PINDCRI, 
                          PMPOB, PMPOBCRI, RDPC, EMP, T_DES18M, T_BANAGUA, T_LUZ, 
                          AGUA_ESGOTO, IDHM, IDHM_E, IDHM_L, IDHM_R)

ATLAS <- ATLAS %>% filter(ANO == 2010) #%>% arrange(UF, Codmun6)
resumo_CODMUNRES <- resumo_CODMUNRES %>% filter(ano_nasc == 2010)

ATLAS <- left_join(ATLAS, resumo_CODMUNRES[,c(1,2,13)], by = c("Codmun6" = "CODMUNRES"))

# Normalized Data
# normalized = (x-min(x))/(max(x)-min(x))

ATLAS <- ATLAS %>% mutate(
  N_ESPVIDA = (ESPVIDA - min(ESPVIDA))/(max(ESPVIDA)-min(ESPVIDA)), 
  N_FECTOT = (FECTOT - min(FECTOT))/(max(FECTOT)-min(FECTOT)), 
  N_MORT1 = (MORT1 - min(MORT1))/(max(MORT1)-min(MORT1)), 
  N_MORT5 = (MORT5 - min(MORT5))/(max(MORT5)-min(MORT5)), 
  N_T_ANALF18M = (T_ANALF18M - min(T_ANALF18M))/(max(T_ANALF18M)-min(T_ANALF18M)), 
  N_T_FBFUND = (T_FBFUND - min(T_FBFUND))/(max(T_FBFUND)-min(T_FBFUND)), 
  N_T_FBMED = (T_FBMED - min(T_FBMED))/(max(T_FBMED)-min(T_FBMED)), 
  N_T_FBPRE = (T_FBPRE - min(T_FBPRE))/(max(T_FBPRE)-min(T_FBPRE)), 
  
  N_T_FBSUPER = (T_FBSUPER - min(T_FBSUPER))/(max(T_FBSUPER)-min(T_FBSUPER)), 
  N_GINI = (GINI - min(GINI))/(max(GINI)-min(GINI)), 
  N_PIND = (PIND - min(PIND))/(max(PIND)-min(PIND)), 
  N_PINDCRI = (PINDCRI - min(PINDCRI))/(max(PINDCRI)-min(PINDCRI)), 
  N_PMPOB = (PMPOB - min(PMPOB))/(max(PMPOB)-min(PMPOB)), 
  N_PMPOBCRI = (PMPOBCRI - min(PMPOBCRI))/(max(PMPOBCRI)-min(PMPOBCRI)), 
  N_RDPC = (RDPC - min(RDPC))/(max(RDPC)-min(RDPC)), 
  N_EMP = (EMP - min(EMP))/(max(EMP)-min(EMP)),
  
  N_T_DES18M = (T_DES18M - min(T_DES18M))/(max(T_DES18M)-min(T_DES18M)),
  N_T_BANAGUA = (T_BANAGUA - min(T_BANAGUA))/(max(T_BANAGUA)-min(T_BANAGUA)),
  N_T_LUZ = (T_LUZ - min(T_LUZ))/(max(T_LUZ)-min(T_LUZ)),
  N_AGUA_ESGOTO = (AGUA_ESGOTO - min(AGUA_ESGOTO))/(max(AGUA_ESGOTO)-min(AGUA_ESGOTO)),
  N_IDHM = (IDHM - min(IDHM))/(max(IDHM)-min(IDHM)),
  N_IDHM_E = (IDHM_E - min(IDHM_E))/(max(IDHM_E)-min(IDHM_E)),
  N_IDHM_L = (IDHM_L - min(IDHM_L))/(max(IDHM_L)-min(IDHM_L)),
  N_IDHM_R = (IDHM_R - min(IDHM_R))/(max(IDHM_R)-min(IDHM_R))
)

# Regressoes:

# Ex 1:
reg <- lm(dist_rota ~ IDHM, data = ATLAS[ATLAS$ano_nasc == 2010,])
summary(reg)

# Loop de reg:
var_dependentes <- colnames(ATLAS[,c(30:53)]) # Lista de Y Complementares
resultados <- list()
gc() # limpar cache de memoria

for(i in var_dependentes){
  
  for (j in c(2010)) { # lista de anos para as regressoes
    ols_fe0 <- as.formula(paste("dist_rota ~ ",i))
    resultados[[paste0(j," ",i," - OLS")]] <- lm(ols_fe0, data = ATLAS[ATLAS$ano_nasc == j,])
  }
  
  if(i == "N_ESPVIDA"){
    coef_data <- as.data.frame(resultados[[paste0(j," ",i," - OLS")]]$coefficients[c(0,0)])
    }
  
  coef_data <- rbind(coef_data, resultados[[paste0(j," ",i," - OLS")]]$coefficients[c(1,2)])
  
  if(i == "N_IDHM_R"){
    coef_data <- rename(coef_data, "Intercept" = "X59.4610895373191")
    coef_data <- rename(coef_data, "Coefficients" = "X.39.8243506745961")
    
    for (m in 1:length(var_dependentes)) { # para criar a var indicador no final
      if(m == 1){coef_data$indicador <- 0}
      coef_data$indicador[m] <- var_dependentes[m]
    }
  }

  rm(ols_fe0) # limpar memoria do R
  print(paste(i,j,sep = "_")) # andamento das regressoes
  
}
# ver resultados:
#msummary(resultados, statistic = 'p.value', stars = c('*' = .1, '**' = .05, '***' = .01))

## Graph:

#ESPVIDA	Esperança de vida ao nascer 
#FECTOT	Taxa de fecundidade total
#MORT1	Mortalidade infantil
#MORT5	Mortalidade até 5 anos de idade
#T_ANALF18M	Taxa de analfabetismo - 18 anos ou mais 
#T_FBFUND	Taxa de frequência bruta ao fundamental
#T_FBMED	Taxa de frequência bruta ao médio
#T_FBPRE	Taxa de frequência bruta à pré-escola
#T_FBSUPER	Taxa de frequência bruta ao superior
#T_FUND18M	% de 18 anos ou mais com fundamental completo
#T_MED18M	% de 18 anos ou mais com médio completo
#GINI	Índice de Gini
#PIND	% de extremamente pobres
#PINDCRI	% de crianças extremamente pobres
#PMPOB	% de pobres
#PMPOBCRI	% de crianças pobres
#RDPC	Renda per capita 
#EMP	% de empregadores - 18 anos ou mais
#T_DES18M	Taxa de desocupação - 18 anos ou mais
#THEILtrab	Índice de Theil-L dos rendimentos do trabalho - 18 anos ou mais
#T_AGUA	% da população em domicílios com água encanada
#T_BANAGUA	% da população em domicílios com banheiro e água encanada
#T_LIXO	% da população em domicílios com coleta de lixo
#T_LUZ	% da população em domicílios com energia elétrica
#AGUA_ESGOTO	% de pessoas em domicílios com abastecimento de água e esgotamento sanitário inadequados
#IDHM	IDHM
#IDHM_ E	IDHM Educação
#IDHM_L	IDHM Longevidade
#IDHM_R	IDHM Renda

coef_data$indicador <- case_when(
  coef_data$indicador == "N_ESPVIDA" ~ "Life expectancy",
  coef_data$indicador == "N_FECTOT" ~ "Fertility rate",
  coef_data$indicador == "N_MORT1" ~ "Child mortality (1 year)",
  coef_data$indicador == "N_MORT5" ~ "Child mortality (5 years)",
  # education
  coef_data$indicador == "N_T_ANALF18M" ~ "Illiteracy rate (18 years +)",
  coef_data$indicador == "N_T_FBFUND" ~ "Elementary frequency rate",
  coef_data$indicador == "N_T_FBMED" ~ "High School frequency rate",
  coef_data$indicador == "N_T_FBPRE" ~ "Pre school frequency rate",
  coef_data$indicador == "N_T_FBSUPER" ~ "University frequency rate",
  
  coef_data$indicador == "N_GINI" ~ "Gini Index",
  # poor rate
  coef_data$indicador == "N_PIND" ~ "Extremely poor rate",
  coef_data$indicador == "N_PINDCRI" ~ "Extremely poor children rate",
  coef_data$indicador == "N_PMPOB" ~ "Population poor rate",
  coef_data$indicador == "N_PMPOBCRI" ~ "Children poor rate",
  
  coef_data$indicador == "N_RDPC" ~ "Per capita income",
  coef_data$indicador == "N_EMP" ~ "Employment rate (18 years +)",
  coef_data$indicador == "N_T_DES18M" ~ "Unemployment rate (18 years +)",
  coef_data$indicador == "N_T_BANAGUA" ~ "Population rate with bathroom and water",
  coef_data$indicador == "N_T_LUZ" ~ "Population rate with with electric power",
  coef_data$indicador == "N_AGUA_ESGOTO" ~ "Population with with inadequate water supply",
  
  coef_data$indicador == "N_IDHM" ~ "HDI",
  coef_data$indicador == "N_IDHM_E" ~ "HDI Education",
  coef_data$indicador == "N_IDHM_L" ~ "HDI longevity",
  coef_data$indicador == "N_IDHM_R" ~ "HDI Income",
)

coef_data$Relationship <- ifelse(coef_data$Coefficients < 0, "-", "+")
coef_data$Coefficients <- ifelse(coef_data$Coefficients < 0,
                                 coef_data$Coefficients*(-1),
                                 coef_data$Coefficients)

coef_data <- coef_data %>% filter(indicador != "HDI" & 
                                    indicador != "Unemployment rate (18 years +)" &
                                    indicador != "Child mortality (1 year)" &
                                    indicador != "Child mortality (5 years)" &
                                    indicador != "Elementary frequency rate" &
                                    indicador != "High School frequency rate" &
                                    indicador != "Pre school frequency rate" &
                                    indicador != "University frequency rate")
# Graph 1:
library(ggpubr)
ggdotchart(coef_data, x = "indicador", y = "Coefficients",
           color = "Relationship", palette = c("lightblue", "purple"), size = 7,
           position = position_dodge(0.7), sorting = c("ascending"),
           ggtheme = theme_bw() + theme(axis.text = element_text(size = 12),
                                        axis.title = element_text(size = 15)
           ),
           title = "Relationship between socioeconomic factors and distance traveled",
           xlab = "Variable",
           ylab = "Coefficients",
) + coord_flip()

# Graph 2:
ggdotchart(coef_data[coef_data$Relationship=="-",], x = "indicador", y = "Coefficients",
           color = "Relationship", palette = c("purple", "orange"), size = 7,
           position = position_dodge(0.7), sorting = c("descending"),
           ggtheme = theme_bw() + theme(axis.text = element_text(size = 12),
                                        axis.title = element_text(size = 15)
           ),
           title = "Relationship between socioeconomic factors and distance traveled",
           xlab = "Variable",
           ylab = "Coefficients",
) + coord_flip()

# Graph 3:
ggdotchart(coef_data[coef_data$Relationship=="+",], x = "indicador", y = "Coefficients",
           color = "Relationship", palette = c("orange"), size = 7,
           position = position_dodge(0.7), sorting = c("descending"),
           ggtheme = theme_bw() + theme(axis.text = element_text(size = 12),
                                        axis.title = element_text(size = 15)
           ),
           title = "Relationship between socioeconomic factors and distance traveled",
           xlab = "Variable",
           ylab = "Coefficients",
) + coord_flip()


############################################################################################

# Parte 2 - Normalizacao por Desvio Padrao

## Bases resumo SINASC - Modelo Econometrico:
resumo_CODMUNRES <- readRDS("E:/Documentos/Data Paper - Distancias SINASC/resumo_MUNIC2.rds")
resumo_CODMUNRES$CODMUNRES <- as.numeric(resumo_CODMUNRES$CODMUNRES)
resumo_CODMUNRES$ano_nasc <- as.numeric(resumo_CODMUNRES$ano_nasc)

ATLAS <- read_excel("E:/Documentos/Data Paper - Distancias SINASC/atlas2013_dadosbrutos_pt.xlsx", 
                    sheet = "MUN 91-00-10")

ATLAS <- ATLAS %>% select(ANO, UF, Codmun6, ESPVIDA, FECTOT, MORT1, MORT5, T_ANALF18M,
                          T_FBFUND, T_FBMED, T_FBPRE, T_FBSUPER, GINI, PIND, PINDCRI, 
                          PMPOB, PMPOBCRI, RDPC, EMP, T_DES18M, T_BANAGUA, T_LUZ, 
                          AGUA_ESGOTO, IDHM, IDHM_E, IDHM_L, IDHM_R)

ATLAS <- ATLAS %>% filter(ANO == 2010) #%>% arrange(UF, Codmun6)
resumo_CODMUNRES <- resumo_CODMUNRES %>% filter(ano_nasc == 2010)

ATLAS <- left_join(ATLAS, resumo_CODMUNRES[,c(1,2,13)], by = c("Codmun6" = "CODMUNRES"))

# fazer a outra normalizacao, (var - mean) / (desvio-padrao)

ATLAS <- ATLAS %>% mutate(
  N_ESPVIDA = (ESPVIDA - mean(ESPVIDA))/(sd(ESPVIDA)), 
  N_FECTOT = (FECTOT - mean(FECTOT))/(sd(FECTOT)), 
  N_MORT1 = (MORT1 - mean(MORT1))/(sd(MORT1)), 
  N_MORT5 = (MORT5 - mean(MORT5))/(sd(MORT5)), 
  N_T_ANALF18M = (T_ANALF18M - mean(T_ANALF18M))/(sd(T_ANALF18M)), 
  N_T_FBFUND = (T_FBFUND - mean(T_FBFUND))/(sd(T_FBFUND)), 
  N_T_FBMED = (T_FBMED - mean(T_FBMED))/(sd(T_FBMED)), 
  N_T_FBPRE = (T_FBPRE - mean(T_FBPRE))/(sd(T_FBPRE)), 
  
  N_T_FBSUPER = (T_FBSUPER - mean(T_FBSUPER))/(sd(T_FBSUPER)), 
  N_GINI = (GINI - mean(GINI))/(sd(GINI)), 
  N_PIND = (PIND - mean(PIND))/(sd(PIND)), 
  N_PINDCRI = (PINDCRI - mean(PINDCRI))/(sd(PINDCRI)), 
  N_PMPOB = (PMPOB - mean(PMPOB))/(sd(PMPOB)), 
  N_PMPOBCRI = (PMPOBCRI - mean(PMPOBCRI))/(sd(PMPOBCRI)), 
  N_RDPC = (RDPC - mean(RDPC))/(sd(RDPC)), 
  N_EMP = (EMP - mean(EMP))/(sd(EMP)),
  
  N_T_DES18M = (T_DES18M - mean(T_DES18M))/(sd(T_DES18M)),
  N_T_BANAGUA = (T_BANAGUA - mean(T_BANAGUA))/(sd(T_BANAGUA)),
  N_T_LUZ = (T_LUZ - mean(T_LUZ))/(sd(T_LUZ)),
  N_AGUA_ESGOTO = (AGUA_ESGOTO - mean(AGUA_ESGOTO))/(sd(AGUA_ESGOTO)),
  N_IDHM = (IDHM - mean(IDHM))/(sd(IDHM)),
  N_IDHM_E = (IDHM_E - mean(IDHM_E))/(sd(IDHM_E)),
  N_IDHM_L = (IDHM_L - mean(IDHM_L))/(sd(IDHM_L)),
  N_IDHM_R = (IDHM_R - mean(IDHM_R))/(sd(IDHM_R))
)

# Regressoes:

# Loop de reg:
var_dependentes <- colnames(ATLAS[,c(30:53)]) # Lista de Y Complementares
resultados <- list()
gc() # limpar cache de memoria

for(i in var_dependentes){
  
  for (j in c(2010)) { # lista de anos para as regressoes
    ols_fe0 <- as.formula(paste("dist_rota ~ ",i))
    resultados[[paste0(j," ",i," - OLS")]] <- lm(ols_fe0, data = ATLAS[ATLAS$ano_nasc == j,])
  }
  
  if(i == "N_ESPVIDA"){
    coef_data <- as.data.frame(resultados[[paste0(j," ",i," - OLS")]]$coefficients[c(0,0)])
  }
  
  coef_data <- rbind(coef_data, resultados[[paste0(j," ",i," - OLS")]]$coefficients[c(1,2)])
  
  if(i == "N_IDHM_R"){
    coef_data <- rename(coef_data, "Intercept" = "X36.208339275026")
    coef_data <- rename(coef_data, "Coefficients" = "X.8.00282788818421")
    
    for (m in 1:length(var_dependentes)) { # para criar a var indicador no final
      if(m == 1){coef_data$indicador <- 0}
      coef_data$indicador[m] <- var_dependentes[m]
    }
  }
  
  rm(ols_fe0) # limpar memoria do R
  print(paste(i,j,sep = "_")) # andamento das regressoes
  
}

#msummary(resultados, statistic = 'p.value', stars = c('*' = .1, '**' = .05, '***' = .01))

coef_data$indicador <- case_when(
  coef_data$indicador == "N_ESPVIDA" ~ "Life expectancy",
  coef_data$indicador == "N_FECTOT" ~ "Fertility rate",
  coef_data$indicador == "N_MORT1" ~ "Child mortality (1 year)",
  coef_data$indicador == "N_MORT5" ~ "Child mortality (5 years)",
  # education
  coef_data$indicador == "N_T_ANALF18M" ~ "Illiteracy rate (18 years +)",
  coef_data$indicador == "N_T_FBFUND" ~ "Elementary frequency rate",
  coef_data$indicador == "N_T_FBMED" ~ "High School frequency rate",
  coef_data$indicador == "N_T_FBPRE" ~ "Pre school frequency rate",
  coef_data$indicador == "N_T_FBSUPER" ~ "University frequency rate",
  
  coef_data$indicador == "N_GINI" ~ "Gini Index",
  # poor rate
  coef_data$indicador == "N_PIND" ~ "Extremely poor rate",
  coef_data$indicador == "N_PINDCRI" ~ "Extremely poor children rate",
  coef_data$indicador == "N_PMPOB" ~ "Population poor rate",
  coef_data$indicador == "N_PMPOBCRI" ~ "Children poor rate",
  
  coef_data$indicador == "N_RDPC" ~ "Per capita income",
  coef_data$indicador == "N_EMP" ~ "Employment rate (18 years +)",
  coef_data$indicador == "N_T_DES18M" ~ "Unemployment rate (18 years +)",
  coef_data$indicador == "N_T_BANAGUA" ~ "Population rate with bathroom and water",
  coef_data$indicador == "N_T_LUZ" ~ "Population rate with with electric power",
  coef_data$indicador == "N_AGUA_ESGOTO" ~ "Population rate with inadequate water supply",
  
  coef_data$indicador == "N_IDHM" ~ "HDI",
  coef_data$indicador == "N_IDHM_E" ~ "HDI Education",
  coef_data$indicador == "N_IDHM_L" ~ "HDI longevity",
  coef_data$indicador == "N_IDHM_R" ~ "HDI Income",
)

coef_data$Relationship <- ifelse(coef_data$Coefficients < 0, "-", "+")
coef_data$Coefficients <- ifelse(coef_data$Coefficients < 0,
                                 coef_data$Coefficients*(-1),
                                 coef_data$Coefficients)

coef_data <- coef_data %>% filter(indicador != "HDI" & 
                                    indicador != "Unemployment rate (18 years +)" &
                                    indicador != "Child mortality (1 year)" &
                                    indicador != "Child mortality (5 years)" &
                                    indicador != "Elementary frequency rate" &
                                    indicador != "High School frequency rate" &
                                    indicador != "Pre school frequency rate" &
                                    indicador != "University frequency rate")
# Graph 1:
library(ggpubr)
ggdotchart(coef_data, x = "indicador", y = "Coefficients",
           color = "Relationship", palette = c("lightblue", "purple"), size = 7,
           position = position_dodge(0.7), sorting = c("ascending"),
           ggtheme = theme_bw() + theme(axis.text = element_text(size = 12),
                                        axis.title = element_text(size = 15)
           ),
           title = "Relationship between socioeconomic factors and distance traveled",
           xlab = "Variable",
           ylab = "Coefficients",
) + coord_flip()
