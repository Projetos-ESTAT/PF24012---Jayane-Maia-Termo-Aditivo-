source("rdocs/source/packages.R")

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

# Calculo do ECI pro deputadosAR

banco_deputados1 <- Base_deputados_provinciais_AR_1983_2023_1_ %>% group_by(yr,cst_n) %>%
  summarise(Oi = sum(ifelse(gov_oppos=="O",pv1,0)),
            Oi2 = sum((ifelse(gov_oppos=="O",pv1,0))^2),
            O=Oi2/Oi,
            Gi = sum(ifelse(gov_oppos=="G",pv1,0)),
            Gi2 = sum((ifelse(gov_oppos=="G",pv1,0))^2),
            G=Gi2/Gi,
            "ECI dep" = 1- abs(O-G)/100,
            ENP = 1/(sum((pv1/100)^2)),) 
banco_deputados1 <- banco_deputados1 %>%
  rename(Year = yr, Province = cst_n)

banco_deputados1 <- na.omit(banco_deputados1)
write.csv2(banco_deputados1)  

# Calculo dos indices banco governadores

Base_modificada1<- Base_governadores_certa %>%
  filter(!is.na(first_second))


banco_governadores1 <- Base_modificada1 %>% group_by(Year,Province) %>%
  summarise(Oi = sum(ifelse(gov_oppos=="O",`Votes party (percent)`,0)),
            Oi2 = sum((ifelse(gov_oppos=="O",`Votes party (percent)`,0))^2),
            O = Oi2/Oi,
            Gi = sum(ifelse(gov_oppos=="G",`Votes party (percent)`,0)),
            Gi2 = sum((ifelse(gov_oppos=="G",`Votes party (percent)`,0))^2),
            G = ifelse(Gi==0,0,Gi2/Gi),
            "ECI gov/opos" = 1- abs(O-G)/100,
            p1 = sum(ifelse(first_second=="first",`Votes party (percent)`/100,0)), #índice 2
            p2 = sum(ifelse(first_second=="second",`Votes party (percent)`/100,0)), #índice 2
            "ECI 1st/2nd" = ifelse(1-(p1-p2)>1,1,1-(p1-p2)), #índice 2
            ENP = 1/(sum((`Votes party (percent)`/100)^2)),
  )

write.csv2(banco_governadores1) 


# Teste de correlação entre ECI dep e ECI gov (1st/2nd)

banco_deputados1 <- Base_deputados_provinciais_AR_1983_2023_1_ %>% group_by(yr,cst_n) %>%
  summarise(Oi = sum(ifelse(gov_oppos=="O",pv1,0)),
            Oi2 = sum((ifelse(gov_oppos=="O",pv1,0))^2),
            O=Oi2/Oi,
            Gi = sum(ifelse(gov_oppos=="G",pv1,0)),
            Gi2 = sum((ifelse(gov_oppos=="G",pv1,0))^2),
            G=Gi2/Gi,
            "ECI dep" = 1- abs(O-G)/100,
            ENP = 1/(sum((pv1/100)^2)),) 
banco_deputados1 <- banco_deputados1 %>%
  rename(Year = yr, Province = cst_n)

banco_deputados1 <- na.omit(banco_deputados1)

banco_governadores1 <- Base_modificada1 %>% group_by(Year,Province) %>%
  summarise(Oi = sum(ifelse(gov_oppos=="O",`Votes party (percent)`,0)),
            Oi2 = sum((ifelse(gov_oppos=="O",`Votes party (percent)`,0))^2),
            O = Oi2/Oi,
            Gi = sum(ifelse(gov_oppos=="G",`Votes party (percent)`,0)),
            Gi2 = sum((ifelse(gov_oppos=="G",`Votes party (percent)`,0))^2),
            G = ifelse(Gi==0,0,Gi2/Gi),
            "ECI gov/opos" = 1- abs(O-G)/100,
            p1 = sum(ifelse(first_second=="first",`Votes party (percent)`/100,0)), #índice 2
            p2 = sum(ifelse(first_second=="second",`Votes party (percent)`/100,0)), #índice 2
            "ECI 1st/2nd" = ifelse(1-(p1-p2)>1,1,1-(p1-p2)), #índice 2
            ENP = 1/(sum((`Votes party (percent)`/100)^2)),
  )

banco_governadores1 <- banco_governadores1 %>% select(Year,Province,"ECI gov/opos","ECI 1st/2nd", "ENP")

banco_deputados21 <- left_join(banco_deputados1,banco_governadores1[,c(1,2,4)],by = c('Year','Province'))

banco_deputados21 <- banco_deputados21 %>% select(Year,Province,"ECI dep","ECI 1st/2nd", ENP)

banco_deputados21 <- rename(banco_deputados21, "ECI gov" = "ECI 1st/2nd")

banco_deputados21 <- na.omit(banco_deputados21)


banco_deputadosCorrelacao <- banco_deputados21 %>% group_by(Province) %>%
  do(tidy(cor.test(.$`ECI dep`, .$ `ECI gov`)))

write.csv2(banco_deputadosCorrelacao)

#Teste de correlação entre os dois índices de competitividade calculados para o banco governadores, isto é, ECI gov/opos e ECI first/second. 

banco_governadoresCorrelacao <- banco_governadores1 %>%
  group_by(Province) %>%
  do(tidy(cor.test(.$`ECI gov/opos`, .$ `ECI 1st/2nd`)))


write.csv2(banco_governadoresCorrelacao)








