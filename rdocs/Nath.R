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
            Resultado = if (`P-valor` < 0.05) 'Rejeita' else 'Não rejeita')

write.csv2(dep_corr_spearman, 
           "resultados/bancos/Teste_correlacao_deputados_spearman.csv")

## governadores -----

gov_corr_spearman <- governadores %>% 
  select(Province,`ECI gov/opos`,`ECI 1st/2nd`) %>% 
  group_by(Province) %>%
  summarise(Coeficiente = cor(`ECI gov/opos`,`ECI 1st/2nd`, method = 'spearman'),
            'P-valor' = cor.test(`ECI gov/opos`,`ECI 1st/2nd`, method = 'spearman')[[3]],
            Resultado = if (`P-valor` < 0.05) 'Rejeita' else 'Não rejeita')

write.csv2(gov_corr_spearman, 
           "resultados/bancos/Teste_correlacao_governadores_spearman.csv")


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
  theme_estat() +
  scale_fill_manual(values=c("white", "grey"), 
                    labels = c('State-dep','State-gov'))
#ggsave("resultados/Nath/box_anos_ENP1.pdf", width = 240, height = 120, units = "mm")
#ggsave("resultados/Nath/box_anos_ENP.jpeg", width = 158, height = 93, units = "mm")


# facetado por metade dos anos
ggplot(df) +
  aes(
    x = factor(Year, levels = ordem_anos),
    y = ENP,
    fill = tipo
  ) +
  geom_boxplot(width = 0.5) +
  labs(x = "Election year", y = "ENP", fill = '') +
  theme_estat() +
  scale_fill_manual(values=c("white", "grey"), 
                    labels = c('State-dep','State-gov')) +
  facet_wrap(~panel, scales = "free_x", nrow = 4) +
  theme(strip.background = element_blank(),
        strip.text.x = element_blank())
#ggsave("resultados/Nath/box_anos_ENP2.pdf", width = 240, height = 120, units = "mm")
#ggsave("resultados/Nath/box_anos_ENP.jpeg", width = 158, height = 93, units = "mm")

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
  theme_estat() +
  scale_fill_manual(values=c("white", "grey"), 
                    labels = c('ECI dep','ECI gov 1st/2nd'))
#ggsave("resultados/Nath/box_anos_ECI.pdf", width = 158, height = 93, units = "mm")
#ggsave("resultados/Nath/box_anos_ECI.jpeg", width = 158, height = 93, units = "mm")


# facetado por metade dos anos
ggplot(df2) +
  aes(
    x = factor(Year, levels = ordem_anos),
    y = ECI,
    fill = tipo
  ) +
  geom_boxplot(width = 0.5) +
  labs(x = "Election year", y = "Competitiveness", fill = '') +
  theme_estat() +
  scale_fill_manual(values=c("white", "grey"), 
                    labels = c('ECI dep','ECI gov 1st/2nd')) +
  facet_wrap(~panel, scales = "free_x", nrow = 4) +
  theme(strip.background = element_blank(),
        strip.text.x = element_blank())
#ggsave("resultados/Nath/box_anos_ECI.pdf", width = 158, height = 93, units = "mm")
#ggsave("resultados/Nath/box_anos_ECI.jpeg", width = 158, height = 93, units = "mm")

# facetado por metade dos anos e tirando legenda
ggplot(df2) +
  aes(
    x = factor(Year, levels = ordem_anos),
    y = ECI,
    fill = tipo
  ) +
  geom_boxplot(width = 0.5) +
  labs(x = "Election year", y = "Competitiveness", fill = '') +
  theme_estat() +
  scale_fill_manual(values=c("white", "grey"), 
                    labels = c('ECI dep','ECI gov 1st/2nd'),
                    guide = "none") +
  facet_wrap(~panel, scales = "free_x", nrow = 4) +
  theme(strip.background = element_blank(),
        strip.text.x = element_blank())
#ggsave("resultados/Nath/box_anos_ECI.pdf", width = 158, height = 93, units = "mm")
#ggsave("resultados/Nath/box_anos_ECI.jpeg", width = 158, height = 93, units = "mm")




