source("rdocs/source/packages.R")
pacman::p_load(ggcorrplot, knitr,showtext, kableExtra, data.table, tidyr,SnowballC,
               wordcloud,tm,stringr,gridExtra,DescTools)


# ---------------------------------------------------------------------------- #

#        ______   _____  ________      ________ 
#      |  ____| / ____| |__   __| /\  |__   __|
#     | |__    | (___     | |   /  \    | |   
#    |  __|    \___ \    | |  / /\ \   | |   
#   | |____   ____) |   | |  /____ \  | |   
#  |______   |_____/   |_| /_/    \_\|_|   
#  
#         Consultoria estatística 
#

# ---------------------------------------------------------------------------- #
# ############################## README ###################################### #
# Consultor, favor utilizar este arquivo .R para realizar TODAS as análises
# alocadas a você neste projeto pelo gerente responsável, salvo instrução 
# explícita do gerente para mudança.
#
# Escreva seu código da forma mais clara e legível possível, eliminando códigos
# de teste depreciados, ou ao menos deixando como comentário. Dê preferência
# as funções dos pacotes contidos no Tidyverse para realizar suas análises.
# ---------------------------------------------------------------------------- #



#### Deputados - Índice 1 ----

banco_Brasil <- read.csv("banco\\TA\\IC_deputados.csv",sep=";",header = T)

banco_Argentina <- read.xlsx("banco\\TA\\ECI_dep_AR_latest version 26Jul.xlsx", header = T, sheetIndex = 1)
banco_Argentina <- na.omit(banco_Argentina)

#### Auto-Correlações ----
pacman::p_load(Mcomp, forecast, tseries)

# Brasil
Autocor_ECI.gov <- Autocor_ECI.dep <- n1 <- n2 <- pvalor1 <- pvalor2 <- numeric()

UF <- character()
j=0
for (i in unique(banco_Brasil$SG_UF)) {
  j=j+1
  UF[i] <- i
  banco_Estado <- banco_Brasil %>% filter(SG_UF ==i)
  X <-ts(as.numeric(gsub(",", ".", gsub("\\.", "", banco_Estado$ECI.gov))))
  Y <-ts(as.numeric(gsub(",", ".", gsub("\\.", "", banco_Estado$ECI.dep))))
  #ndif1[j] <- ndiffs(X)
  #ndif2[j] <- ndiffs(Y)
  Autocor_ECI.gov[j] <- acf(X)$acf[2]
  #Autocor_ECI.govAlt[j] <- ifelse(ndif1[j]==1,acf(diff(X))$acf[2],Autocor_ECI.gov[j])
  n1[j] <- acf(X)$n.used
  pvalor1[j] <- 2-2*pnorm(abs(Autocor_ECI.gov[j]*sqrt(n1[j])))
  Autocor_ECI.dep[j] <- acf(Y)$acf[2]
  #Autocor_ECI.depAlt[j] <- ifelse(ndif2[j]==1,acf(diff(Y))$acf[2],Autocor_ECI.dep[j])
  n2[j] <- acf(Y)$n.used
  pvalor2[j] <- 2-2*pnorm(abs(Autocor_ECI.dep[j]*sqrt(n2[j])))
}
AutoCor_Brasil <- data.frame(UF,Autocor_ECI.gov,n1,pvalor1,Autocor_ECI.dep,n2,pvalor2)
write.csv(AutoCor_Brasil,"resultados\\bancos\\TA\\Autocorrelação Brasil.csv")

rm(UF,Autocor_ECI.gov, Autocor_ECI.dep, n1, n2, pvalor1, pvalor2)

# Argentina
Autocor_ECI.gov <- Autocor_ECI.dep <- n1 <- n2 <- pvalor1 <- pvalor2 <- numeric()

Province <- character()
j=0
for (i in unique(banco_Argentina$Province)) {
  j=j+1
  Province[i] <- i
  banco_Province <- banco_Argentina %>% filter(Province ==i)
  X <-ts(na.omit(as.numeric(banco_Province$ECI.gov)))
  Y <-ts(as.numeric(banco_Province$ECI.dep))
  #ndif1[j] <- ndiffs(X)
  #ndif2[j] <- ndiffs(Y)
  Autocor_ECI.gov[j] <- acf(X)$acf[2]
  #Autocor_ECI.govAlt[j] <- ifelse(ndif1[j]==1,acf(diff(X))$acf[2],Autocor_ECI.gov[j])
  n1[j] <- acf(X)$n.used
  pvalor1[j] <- 2-2*pnorm(abs(Autocor_ECI.gov[j]*sqrt(n1[j])))
  Autocor_ECI.dep[j] <- acf(Y)$acf[2]
  #Autocor_ECI.depAlt[j] <- ifelse(ndif2[j]==1,acf(diff(Y))$acf[2],Autocor_ECI.dep[j])
  n2[j] <- acf(Y)$n.used
  pvalor2[j] <- 2-2*pnorm(abs(Autocor_ECI.dep[j]*sqrt(n2[j])))
}
AutoCor_Argentina <- data.frame(Province,Autocor_ECI.gov,n1,pvalor1,Autocor_ECI.dep,n2,pvalor2)
write.csv(AutoCor_Argentina,"resultados\\bancos\\TA\\Autocorrelação Argentina.csv")
