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

calcular_ENP <- function(Base_deputados_AR) {
  votos_validos <- Base_deputados_AR$pv1
  enp <- enp(votos_validos)
  return(enp)
}

bancoDeputados <- Base_deputados_AR %>%
  group_by(yr, cst_n) %>%
  mutate(ENP = calcular_ENP(cur_data()))


banco_deputados1 <- bancoDeputados %>%
  group_by(yr, cst_n,ENP) %>%
  summarise(Oi = sum(coalesce(ifelse(gov_oppos == "O", pv1, 0), 0)),
            Oi2 = sum(coalesce(ifelse(gov_oppos == "O", pv1, 0), 0)^2),
            O = Oi2 / Oi,
            Gi = sum(coalesce(ifelse(gov_oppos == "G", pv1, 0), 0)),
            Gi2 = sum(coalesce(ifelse(gov_oppos == "G", pv1, 0), 0)^2),
            G = Gi2 / Gi,
            `ECI dep` = 1 - abs(O - G) / 100)
          
banco_deputados1 <- banco_deputados1 %>%
  rename(Year = yr, Province = cst_n)


banco_deputados1$id <- ifelse(banco_deputados1$Province == "Buenos Aires", 1,
                              ifelse(banco_deputados1$Province == "Catamarca", 2,
                                     ifelse(banco_deputados1$Province == "Chaco", 3,
                                            ifelse(banco_deputados1$Province == "Corrientes", 4,
                                                   ifelse(banco_deputados1$Province == "Formosa", 5,
                                                          ifelse(banco_deputados1$Province == "Jujuy", 6,
                                                                 ifelse(banco_deputados1$Province == "La Rioja", 7,
                                                                        ifelse(banco_deputados1$Province == "Mendoza", 8,
                                                                               ifelse(banco_deputados1$Province == "Misiones", 9,
                                                                                      ifelse(banco_deputados1$Province == "Salta", 10,
                                                                                             ifelse(banco_deputados1$Province == "San Luis", 11,
                                                                                                    ifelse(banco_deputados1$Province == "Santa Cruz", 12,
                                                                                                           ifelse(banco_deputados1$Province == "Tucumán", 13,
                                                                                                                  ifelse(banco_deputados1$Province == "Chubut", 14,
                                                                                                                         ifelse(banco_deputados1$Province == "Córdoba", 15,
                                                                                                                                ifelse(banco_deputados1$Province == "Entre Ríos", 16,
                                                                                                                                       ifelse(banco_deputados1$Province == "La Pampa", 17,
                                                                                                                                              ifelse(banco_deputados1$Province == "Neuquén", 18,
                                                                                                                                                     ifelse(banco_deputados1$Province == "Río Negro", 19,
                                                                                                                                                            ifelse(banco_deputados1$Province == "Santa Fe", 20,
                                                                                                                                                                   ifelse(banco_deputados1$Province == "Santiago del Estero", 21,
                                                                                                                                                                          ifelse(banco_deputados1$Province == "San Juan", 22,
                                                                                                                                                                                 ifelse(banco_deputados1$Province == "Tierra del Fuego", 23,
                                                                                                                                                                                        ifelse(banco_deputados1$Province == "Capital Federal", 24, 0))))))))))))))))))))))))


banco_deputados1 <- na.omit(banco_deputados1)


# Calculo dos indices banco governadores

calcular_ENP_gov <- function(Base_governadores_AR_xlsx_Planilha2) {
  votos_validos <- Base_governadores_AR_xlsx_Planilha2$`Votes party (percent)`
  enp <- enp(votos_validos)
  return(enp)
}

bancoGovernadores <- Base_governadores_AR_xlsx_Planilha2 %>%
  group_by(Year, Province) %>%
  mutate(ENP = calcular_ENP_gov(cur_data()))

banco_governadores1 <- bancoGovernadores %>%
  group_by(Year, Province,ENP) %>%
  summarise(Oi = sum(coalesce(ifelse(gov_oppos == "O", `Votes party (percent)`, 0), 0)),
            Oi2 = sum(coalesce(ifelse(gov_oppos == "O", `Votes party (percent)`, 0), 0)^2),
            O = ifelse(Oi == 0, 0, Oi2 / Oi), 
            Gi = sum(coalesce(ifelse(gov_oppos == "G", `Votes party (percent)`, 0), 0)),
            Gi2 = sum(coalesce(ifelse(gov_oppos == "G", `Votes party (percent)`, 0), 0)^2),
            G = ifelse(Gi == 0, 0, Gi2 / Gi), 
            `ECI gov/opos` = 1 - abs(O - G) / 100,
            p1 = sum(coalesce(ifelse(!is.na(first_second) & first_second == "first", `Votes party (percent)` / 100, 0), 0)),
            p2 = sum(coalesce(ifelse(!is.na(first_second) & first_second == "second", `Votes party (percent)` / 100, 0), 0)),
            `ECI 1st/2nd` = (1 - (p1 - p2))) %>%
  ungroup() %>%
  filter(if_all(everything(), ~ . != 0))

banco_governadores1$id <- ifelse(banco_governadores1$Province == "Buenos Aires", 1,
                              ifelse(banco_governadores1$Province == "Catamarca", 2,
                                     ifelse(banco_governadores1$Province == "Chaco", 3,
                                            ifelse(banco_governadores1$Province == "Corrientes", 4,
                                                   ifelse(banco_governadores1$Province == "Formosa", 5,
                                                          ifelse(banco_governadores1$Province == "Jujuy", 6,
                                                                 ifelse(banco_governadores1$Province == "La Rioja", 7,
                                                                        ifelse(banco_governadores1$Province == "Mendoza", 8,
                                                                               ifelse(banco_governadores1$Province == "Misiones", 9,
                                                                                      ifelse(banco_governadores1$Province == "Salta", 10,
                                                                                             ifelse(banco_governadores1$Province == "San Luis", 11,
                                                                                                    ifelse(banco_governadores1$Province == "Santa Cruz", 12,
                                                                                                           ifelse(banco_governadores1$Province == "Tucumán", 13,
                                                                                                                  ifelse(banco_governadores1$Province == "Chubut", 14,
                                                                                                                         ifelse(banco_governadores1$Province == "Córdoba", 15,
                                                                                                                                ifelse(banco_governadores1$Province == "Entre Ríos", 16,
                                                                                                                                       ifelse(banco_governadores1$Province == "La Pampa", 17,
                                                                                                                                              ifelse(banco_governadores1$Province == "Neuquén", 18,
                                                                                                                                                     ifelse(banco_governadores1$Province == "Río Negro", 19,
                                                                                                                                                            ifelse(banco_governadores1$Province == "Santa Fe", 20,
                                                                                                                                                                   ifelse(banco_governadores1$Province == "Santiago del Estero", 21,
                                                                                                                                                                          ifelse(banco_governadores1$Province == "San Juan", 22,
                                                                                                                                                                                 ifelse(banco_governadores1$Province == "Tierra del Fuego", 23,
                                                                                                                                                                                        ifelse(banco_governadores1$Province == "Capital Federal", 24, 0))))))))))))))))))))))))

write.csv2(banco_governadores1) 
                           
# Banco ECI deputados AR

banco_governadores2 <- banco_governadores1 %>% select(Year,Province,"ECI gov/opos","ECI 1st/2nd", "ENP", "id")

banco_deputados21 <- left_join(banco_deputados1,banco_governadores2[,c(1,2,4,6)],by = c('Year','Province','id'))

banco_deputados21 <- rename(banco_deputados21, "ECI gov" = "ECI 1st/2nd")

write.csv2(banco_deputados21)


# Teste de correlação entre ECI dep e ECI gov (1st/2nd)


banco_deputados21 <- banco_deputados21 %>% select(Year,Province,"ECI dep","ECI gov", ENP,id)

banco_deputados21 <- na.omit(banco_deputados21)

banco_deputadosCorrelacao <- banco_deputados21 %>% group_by(Province) %>%
  summarise(Coeficiente = cor(`ECI dep`,`ECI gov`), `P-valor` = cor.test(`ECI dep`, `ECI gov`)[[3]], Resultado = if (`P-valor`< 0.05) 'Rejeita' else 'Não rejeita')
  

write.csv2(banco_deputadosCorrelacao)

#Teste de correlação entre os dois índices de competitividade calculados para o banco governadores, isto é, ECI gov/opos e ECI first/second. 

banco_governadoresCorrelacao <- banco_governadores1 %>%
  group_by(Province) %>%
  summarise(Coeficiente = cor(`ECI gov/opos`,`ECI 1st/2nd`), `P-valor` = cor.test(`ECI gov/opos`, `ECI 1st/2nd`)[[3]], Resultado = if (`P-valor`< 0.05) 'Rejeita' else 'Não rejeita')


write.csv2(banco_governadoresCorrelacao)








