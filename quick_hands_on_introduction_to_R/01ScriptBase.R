
# Setup do Script ---------------------------------------------------------

library(tidyverse)
library(broom)
library(forcats)
library(ggthemes)
source("00FuncoesProntas.R")


# Importar Dados ----------------------------------------------------------

instituicoes <- as_tibble(read.csv("Dados/institutions.csv",sep = ";"))

corrupcao <- as_tibble(read.csv("Dados/cpi.csv",sep = ";"))



# Limpar Dados ------------------------------------------------------------

## Arrumando nome das colunas
colnames(instituicoes) <- c("Pais","Variavel",2011,2013:2017)

colnames(corrupcao) <- c("Sigla","Pais",2011:2017)


## Tirando os NAs
instituicoes <- filtro(instituicoes)

corrupcao <- filtro(corrupcao)


# Transformar Dados -------------------------------------------------------

## Variaveis Como colunas e observações como linhas
instituicoes <- instituicoes %>% 
  gather(key = Ano,value = valor,-c(Pais,Variavel)) %>% 
  spread(key = Variavel,value = valor)


corrupcao <- corrupcao %>% 
  gather(key = Ano,value = ipc,-c(Sigla,Pais))


## Juntando Dados e Renomenado Variaveis

datasetFinal <- instituicoes %>% 
  inner_join(corrupcao,
             c("Pais","Ano")) %>% 
  select(-c(`2`,`3`)) %>% 
  rename(politico = `1.1.`,
         regulatorio = `1.2.`,
         negocio = `1.3.`,
         sof_mercado = `4`,
         sof_negocio = `5`)


# Modelar -----------------------------------------------------------------

## Regressao linear basica
rg <- lm(formula = ipc~politico+regulatorio+
           negocio+sof_mercado+sof_negocio,data = datasetFinal)
sumario <- summary(rg)

tidy(sumario)



## Regressao Linear por ano
porAno <- split(x = datasetFinal,
                f = datasetFinal$Ano)


regressaoPorAno <- function(df){
  ano <- df$Ano[1]
  rl <- lm(ipc~politico+regulatorio+negocio+sof_mercado+sof_negocio,data = df)
  sumario <- tidy(summary(rl)) %>% 
    mutate(ano = ano)
  return(sumario)
}

regressoesPorAno <- map(porAno,regressaoPorAno)

## Regressao Linear por Pais
porPais <- split(x = datasetFinal,
                 f = datasetFinal$Pais)

regressaoPorPais <- function(df){
  pais <- df$Pais[1]
  rl <- lm(ipc~politico+regulatorio,data = df)
  sumario <- tidy(summary(rl)) %>% 
    mutate(pais=pais)
  return(sumario)
}

regressoesPorPais <- map(porPais,regressaoPorPais)
regressoesPorPais

datasetFinal


# Visualização ------------------------------------------------------------



# Histograma
datasetFinal %>% 
  ggplot(aes(x=politico))+
  geom_histogram()+
  theme_minimal()




# Densidade
datasetFinal %>% 
  ggplot(aes(x=negocio, fill=Pais))+
  geom_density()

# Dispersão
datasetFinal %>% 
  ggplot(aes(x=negocio, y=ipc, color=Ano,
             shape=Ano))+
  geom_point(size=2)+theme_minimal()+
  geom_smooth(method = "lm")





# boxplot
datasetFinal %>% 
  ggplot(aes(y=ipc, x=Ano, fill=Ano))+
  geom_boxplot()+theme_stata()


SumarioPorAno <- bind_rows(regressoesPorAno)
SumarioPorAno %>% 
  group_by(term,ano) 
  summarise(
    media = mean(estimate)
  )










## Visualizando Resultado do Modelo
## Preparando Dado para visualizar
SumarioPorAno <- bind_rows(regressoesPorAno)


linhaPVALOR <- tibble(
  term=as_factor(c("estimate","p.value")),
  Valor = c(NA,0.05))

SumarioPorAno %>% 
  filter(term != "(Intercept)") %>%
  select(term,ano,estimate,p.value) %>% 
  mutate(
    term = fct_reorder(term,estimate)
  ) %>% 
  gather(Variavel,Valor,c(estimate,p.value)) %>% 
  ggplot(aes(x=term,y=Valor))+
  geom_bar(stat="identity")+
  coord_flip()+
  facet_grid(rows=vars(ano),cols=vars(Variavel))+
  theme_minimal()+
  geom_hline(data = linhaPVALOR,
             aes(yintercept=Valor))



SumarioPorAno %>% 
  filter(term != "(Intercept)") %>%
  select(term,ano,estimate,p.value) %>% 
  mutate(
    term = fct_reorder(term,estimate)
  ) %>% 
  gather(Variavel,Valor,-c(term,ano)) %>% 
  ggplot(aes(x=term,y=Valor))+
  geom_bar(position='dodge',stat="identity")+
  coord_flip()+
  facet_grid(rows=vars(ano),cols=vars(Variavel))+
  theme_minimal()+geom_hline(data = linhaPVALOR,
                             aes(yintercept = Valor))









# Grafico com Coeficiente Estimado e p. Valor lado a lado
linhaPVALOR <- tibble(Variavel=as_factor(c("estimate","p.value")),
                      Valor = c(NA,0.05))
SumarioPorAno %>% 
  filter(term != "(Intercept)") %>%
  select(term,ano,estimate,p.value) %>% 
  mutate(
    term = fct_reorder(term,estimate)
  ) %>% 
  gather(Variavel,Valor,-c(term,ano)) %>% 
  ggplot(aes(x=term,y=Valor))+
  geom_bar(position='dodge',stat="identity")+
  coord_flip()+
  facet_grid(rows=vars(ano),cols=vars(Variavel))+
  theme_minimal()+geom_hline(data = linhaPVALOR,
                             aes(yintercept = Valor,
                                 x=term))















