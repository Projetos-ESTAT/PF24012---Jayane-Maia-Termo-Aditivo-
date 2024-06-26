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
            IC = 1- abs(O-G)/100) 
banco_deputados1

banco_deputados1 <- na.omit(banco_deputados1)
banco_deputados1 <- banco_deputados1 %>%
  select(yr, cst_n,IC)
write.csv2(banco_deputados1)  

# Calculo dos indices banco governadores

Base_modificada1<- Base_modificada1 %>%
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
            "ECI 1st/2nd" = ifelse(1-(p1-p2)>1,1,1-(p1-p2)) #índice 2
  )

banco_governadores1 <- banco_governadores1 %>% select(Year,Province,"ECI gov/opos","ECI 1st/2nd")
banco_governadores1

write.csv2(banco_governadores1) 


