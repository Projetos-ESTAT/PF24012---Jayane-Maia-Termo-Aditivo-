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

# carregando pacotes
pacman::p_load(readxl)

# importando bancos
deputados <- read_excel("banco/ECI_deputados_AR (5).xlsx")
governadores <- read_excel("banco/ECI-GOV-Ar.xlsx")

## ordem dos anos
ordem_anos <- as.character(c(1985:2024))

# correlacoes por Spearman -----

## deputados -----

dep_corr_spearman <- deputados %>% 
  select(Province,`ECI dep`,`ECI gov`) %>% 
  mutate(`ECI gov` = as.numeric(`ECI gov`)) %>% 
  na.omit() %>% 
  group_by(Province) %>%
  summarise(Coeficiente = cor(`ECI dep`, `ECI gov`, method = 'spearman'),
            'P-valor' = cor.test(`ECI dep`, `ECI gov`, method = 'spearman')[[3]],
            Resultado = if (`P-valor` < 0.05) 'Rejeita' else 'Não rejeita',
            Coef_det = Coeficiente^2)

write.csv2(dep_corr_spearman, 
           "resultados/bancos/Teste_correlacao_deputados_spearman.csv",
           fileEncoding = 'latin1')

### normalidade 
dep_n <- deputados %>% 
  select(`ECI dep`,`ECI gov`) %>% 
  mutate(`ECI gov` = as.numeric(`ECI gov`)) %>% 
  na.omit()
shapiro.test(dep_n$`ECI dep`)
shapiro.test(dep_n$`ECI gov`)

## governadores -----

gov_corr_spearman <- governadores %>% 
  select(Province,`ECI gov/opos`,`ECI 1st/2nd`) %>% 
  group_by(Province) %>%
  summarise(Coeficiente = cor(`ECI gov/opos`,`ECI 1st/2nd`, method = 'spearman'),
            'P-valor' = cor.test(`ECI gov/opos`,`ECI 1st/2nd`, method = 'spearman')[[3]],
            Resultado = if (`P-valor` < 0.05) 'Rejeita' else 'Não rejeita',
            Coef_det = Coeficiente^2)

write.csv2(gov_corr_spearman, 
           "resultados/bancos/Teste_correlacao_governadores_spearman.csv",
           fileEncoding = 'latin1')

### normalidade 
gov_n <- governadores %>% 
  select(`ECI gov/opos`,`ECI 1st/2nd`) %>% 
  na.omit()
shapiro.test(gov_n$`ECI gov/opos`)
shapiro.test(gov_n$`ECI 1st/2nd`)

# ENP ----
## selecionando variaveis de interesse
dep <- deputados %>% 
  select('Year', 'Province', 'ENP')%>%
  na.omit() %>% 
  mutate(Year = as.character(Year))

gov <- governadores %>% 
  select('Year', 'Province', 'ENP')%>%
  na.omit() %>% 
  mutate(Year = as.character(Year))

## adicionando variavel identificadora 
dep$'tipo' <- 'dep'
gov$'tipo' <- 'gov'

## juntando os bancos
df <- bind_rows(dep, gov)

## criando chave para separar os anos
keys <- data.frame(Year=c(1985:2024))
keys$i <- 1:nrow(keys)
keys$panel <- cut(keys$i,
                  breaks = c(-Inf,quantile(keys$i,0.5),Inf),
                  include.lowest = T,right = T,
                  labels = c('Parte 1','Parte 2'))

## adicionando variavel no banco
df$panel <- keys[match(df$Year,keys$Year),"panel"]

## box-plot ----
ggplot(df) +
  aes(
    x = factor(Year, levels = ordem_anos),
    y = ENP,
    fill = tipo
  ) +
  geom_boxplot(width = 0.5) +
  labs(x = "Election year", y = "ENP", fill = '') +
  coord_cartesian(ylim = c(0,20))  +
  theme_estat() +
  scale_fill_manual(values=c("white", "grey"), 
                    labels = c('State-dep','State-gov'))
#ggsave("resultados/Nath/box_anos_ENP_comp.pdf", width = 250, height = 150, units = "mm")
#ggsave("resultados/Nath/box_anos_ENP_comp.jpeg", width = 250, height = 150, units = "mm")


# facetado por metade dos anos
ggplot(df) +
  aes(
    x = factor(Year, levels = ordem_anos),
    y = ENP,
    fill = tipo
  ) +
  geom_boxplot(width = 0.5) +
  labs(x = "Election year", y = "ENP", fill = '') +
  coord_cartesian(ylim = c(0,20))  +
  theme_estat() +
  scale_fill_manual(values=c("white", "grey"), 
                    labels = c('State-dep','State-gov')) +
  facet_wrap(~panel, scales = "free_x", nrow = 4) +
  theme(strip.background = element_blank(),
        strip.text.x = element_blank())
#ggsave("resultados/Nath/box_anos_ENP_div.pdf", width = 250, height = 150, units = "mm")
#ggsave("resultados/Nath/box_anos_ENP_div.jpeg", width = 250, height = 150, units = "mm")

# ECI dep e ECI gov 1st/ 2nd  ----
## selecionando variaveis de interesse
dep2 <- deputados %>% 
  select('Year', 'Province', `ECI dep`)%>%
  na.omit() %>% 
  mutate(Year = as.character(Year))

gov2 <- governadores %>% 
  select('Year', 'Province', `ECI 1st/2nd`)%>%
  na.omit() %>% 
  mutate(Year = as.character(Year))

## adicionando variavel identificadora 
dep2$'tipo' <- 'dep'
gov2$'tipo' <- 'gov'

## juntando os bancos
df2 <- bind_rows(dep2, gov2) %>%
  mutate(ECI = coalesce(`ECI dep`, `ECI 1st/2nd`))

## adicionando variavel no banco
df2$panel <- keys[match(df2$Year,keys$Year),"panel"]

## box-plot ----
ggplot(df2) +
  aes(
    x = factor(Year, levels = ordem_anos),
    y = ECI,
    fill = tipo
  ) +
  geom_boxplot(width = 0.5) +
  labs(x = "Election year", y = "Competitiveness", fill = '') +
  coord_cartesian(ylim = c(0.15,1))  +
  theme_estat() +
  scale_fill_manual(values=c("white", "grey"), 
                    labels = c('ECI dep','ECI gov 1st/2nd'))
#ggsave("resultados/Nath/box_anos_ECI_comp.pdf", width = 250, height = 150, units = "mm")
#ggsave("resultados/Nath/box_anos_ECI_comp.jpeg", width = 250, height = 150, units = "mm")


# facetado por metade dos anos
ggplot(df2) +
  aes(
    x = factor(Year, levels = ordem_anos),
    y = ECI,
    fill = tipo
  ) +
  geom_boxplot(width = 0.5) +
  labs(x = "Election year", y = "Competitiveness", fill = '') +
  coord_cartesian(ylim = c(0.15,1))  +
  theme_estat() +
  scale_fill_manual(values=c("white", "grey"), 
                    labels = c('ECI dep','ECI gov 1st/2nd')) +
  facet_wrap(~panel, scales = "free_x", nrow = 4) +
  theme(strip.background = element_blank(),
        strip.text.x = element_blank())
#ggsave("resultados/Nath/box_anos_ECI.pdf", width = 250, height = 150, units = "mm")
#ggsave("resultados/Nath/box_anos_ECI.jpeg", width = 250, height = 150, units = "mm")

# relacao entre ECI e subtype_cand - banco eleitos -----

# carregando pacotes
pacman::p_load(readxl, DescTools)

# importando banco 
eleitos <- read_excel("banco/Base_governadores eleitos AR_1983-2023.xlsx")
deputados <- read_excel("banco/ECI_deputados_AR (5).xlsx")

# manipulacoes
eleitos <-  eleitos %>% 
  filter(subtype_cand!="NA")


#Verificando umas coisas
table(eleitos$type_winning_cand)
table(eleitos$type_party_candidate)

table(eleitos$type_party_candidate[eleitos$type_winning_cand=="exogeneous"])
table(eleitos$type_party_candidate[eleitos$type_winning_cand=="governing"])
table(eleitos$type_party_candidate[eleitos$type_winning_cand=="incumbent"])
table(eleitos$type_party_candidate[eleitos$type_winning_cand=="semi governing"])
# vai q precisa

eleitos$ANO_ELEICAO <- as.numeric(eleitos$ANO_ELEICAO)

## metodos 2 ----
banco_eleitos <- left_join(deputados,eleitos[,c(1,3,20,23,24)],by = c("Year"="ANO_ELEICAO","Province"="PROVINCIA"))

# verificando de novo
table(banco_eleitos$subtype_cand)

# filtrando os q contem 1 obs
banco_eleitos <- banco_eleitos %>% 
  filter(!subtype_cand %in% c('exog/same cand', 'semigov/same cand', 
                              'semigov/same cand*', 'semigov/vice'))

# criando a diferenca
banco_eleitos <- banco_eleitos %>%
  mutate(Diff = as.numeric(`ECI gov`) - `ECI dep`,
         subtype_cand_code = case_when(
           subtype_cand == 'exog/all diff' ~ '1',
           subtype_cand == 'gov/all diff' ~ '2',
           subtype_cand == 'incumb/same family' ~ '3',
           subtype_cand == 'incumb/diff cand' ~ '4',
           subtype_cand == 'incumb/same cand' ~ '5',
           subtype_cand == 'incumb/same cand*' ~ '6',
           subtype_cand == 'incumb/vice' ~ '7',
           subtype_cand == 'semigov/all diff' ~ '8'
         ))

banco_eleitos$subtype_cand_code <- factor(banco_eleitos$subtype_cand_code,
                                          levels = c("1","2","3","4","5","6","7","8"))
banco_eleitos <- banco_eleitos %>% na.omit(Diff)

# verificando de novo
table(banco_eleitos$subtype_cand_code)


## boxplot
ggplot(banco_eleitos) +  aes(x = subtype_cand_code,y = Diff) +
  geom_boxplot(fill = c("white"), width = 0.5) +
  labs(x = "Code", y = "Difference") +
  theme_estat() +
  scale_x_discrete(labels = function(x) str_wrap(x,width = 20))
#ggsave("resultados/Nath/box_eleitos_ECI.pdf", width = 158, height = 93, units = "mm")
#ggsave("resultados/Nath/box_eleitos_ECI.jpeg", width = 158, height = 93, units = "mm")


## teste de comparacao de media
anova1 <- aov(Diff ~ factor(subtype_cand), data = banco_eleitos)
summary(anova1)
shapiro.test(anova1$residuals)

# nao parametrico
kruskal.test(banco_eleitos$Diff, as.factor(banco_eleitos$subtype_cand))

## teste de comparacoes multiplas
ConoverTest(banco_eleitos$Diff,as.factor(banco_eleitos$subtype_cand), method = "holm")
ConoverTest(banco_eleitos$Diff,as.factor(banco_eleitos$subtype_cand_code), method = "holm")


# TA do TA - testando exploratoria -----

# carregando pacotes
source("rdocs/source/packages.R")
pacman::p_load(readxl)

## importando bancos

deputados <- read_excel("banco/ECI_deputados_AR (5).xlsx")
deputados$`ECI gov` <- as.numeric(deputados$`ECI gov`)

## ECI gov/ 1st/2nd ----
### tabela 2.2 - summary statistics para ECI gov/ 1st/2nd ----

estatis_gov <- deputados %>%
  group_by(Province) %>%
  na.omit(`ECI gov`) %>% 
  summarize(
    Mean = mean(`ECI gov`),
    `Std. dev.` = sd(`ECI gov`),
    Min = min(`ECI gov`),
    Max = max(`ECI gov`),  
    N = n() 
  )



### tabela 2.2 - summary statistics para ECI gov/ 1st/2nd em On-Schedule e Full-data ----

### filtrando
on_schedule_gov <- deputados %>%
  filter(!(Province == "Catamarca" & Year == 1988),
         !(Province == "Corrientes" & (Year %in% c(1993, 1997, 2001, 2005, 2009, 2013, 2017, 2021))),
         !(Province == "Córdoba" & Year == 1998),
         !(Province == "Capital Federal" & Year == 2000),
         !(Province == "Santiago del Estero" & (Year %in% c(2002, 2005, 2008, 2013, 2017, 2021))))

full_data_gov <- deputados %>%
  filter(!(Province %in% c("Catamarca", "Corrientes", "Córdoba", "Capital Federal", 
                           "Santiago del Estero", "Tierra del Fuego")))

# criando dataframe
estatis_on_schedule_gov <- on_schedule_gov %>%
  group_by(Year) %>%
  na.omit(`ECI gov`) %>%
  summarise(
    Mean = mean(`ECI gov`, na.rm = TRUE),
    `Std. dev.` = sd(`ECI gov`, na.rm = TRUE),
    N = n()
)

estatis_full_data_gov <- full_data_gov %>%
  group_by(Year) %>%
  na.omit(`ECI gov`) %>%
  summarise(
    Mean = mean(`ECI gov`, na.rm = TRUE),
    `Std. dev.` = sd(`ECI gov`, na.rm = TRUE),
    N = n()
  )


## ECI dep ----
### tabela 2.2 - summary statistics para ECI dep ----

estatis_dep <- deputados %>%
  group_by(Province) %>%
  summarize(
    Mean = mean(`ECI dep`),
    `Std. dev.` = sd(`ECI dep`),
    Min = min(`ECI dep`),
    Max = max(`ECI dep`),  
    N = n() 
  )

### tabela 2.2 - summary statistics para ECI dep em On-Schedule e Full-data ----

### filtrando
on_schedule_dep <- deputados %>%
  filter(!(Province == "Capital Federal" & Year == 2000),
         !(Province == "Santiago del Estero" & (Year %in% c(2002, 2008))))

full_data_dep <- deputados %>%
  filter(!(Province %in% c("Capital Federal", "Santiago del Estero", "La Rioja",
                           "Salta", "San Luis", "Tierra del Fuego")))

# criando dataframe
estatis_on_schedule_dep <- on_schedule_dep %>%
  group_by(Year) %>%
  summarise(
    Mean = mean(`ECI dep`, na.rm = TRUE),
    `Std. dev.` = sd(`ECI dep`, na.rm = TRUE),
    N = n()
  )

estatis_full_data_dep <- full_data_dep %>%
  group_by(Year) %>%
  summarise(
    Mean = mean(`ECI dep`, na.rm = TRUE),
    `Std. dev.` = sd(`ECI dep`, na.rm = TRUE),
    N = n()
  )













