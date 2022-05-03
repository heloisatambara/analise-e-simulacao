# Lista 3 - MAE0399
library(dplyr)
#### Leitura dos dados ####
setwd('E:/usp/estudos/ESTAT/mae399')
dados = read.csv('201807-citibike-tripdata.csv')
dados = data.frame(dados, stringsAsFactors = FALSE)

#### outliers ####
# exercicio 1
quantile(dados$tripduration, c(.01, .25, .5, .75, .99))

dados = dados[dados$tripduration >= 103 & dados$tripduration <= 4157,]

# exercicio 2
bikes = unique(dados$bikeid) #10667 bikes

#### dias de uso ####
# separar starttime em data e hora
startdate <- substr(dados$starttime, 1, 10)
dados$startdate <- startdate

# contar quantas vezes cada id aparece
bikeperday = unique(dados[c('bikeid', 'startdate')]) # une ocorrencias de uma bike no dia
daysperbike = data.frame(table(bikeperday$bikeid))  # quantos dias aparece a bike

# exercicio 3
usedeveryday = daysperbike[daysperbike$Freq == 31,] # 292 obs

# exercicio 4
use_onde_day = daysperbike[daysperbike$Freq == 1,] # 100 obs

#### top 10, bottom 10 ####

# antes, somar o tempo de cada bicicleta
dadosbike_tempo = data.frame(dados$tripduration, dados$bikeid)
dadosbike_tempo = dadosbike_tempo[order(dadosbike_tempo$dados.bikeid),]
dadosbike_tempo$dados.bikeid = as.factor(dadosbike_tempo$dados.bikeid)

t_cada_bike = dadosbike_tempo %>% 
  group_by(across(where(is.factor))) %>% 
  summarise(across(everything(), sum))

# exercicio 5
top10 = t_cada_bike[order(t_cada_bike$dados.tripduration, decreasing = TRUE)[1:10],]

# exercicio 6
bottom10 = t_cada_bike[order(t_cada_bike$dados.tripduration)[1:10],]


#### exercicio 7 ####
# dividir o tempo de uso de cada bike pelo tempo total do mes:
tempo_total = 60*60*24*31

t_cada_bike$dados.tripduration <- t_cada_bike$dados.tripduration/tempo_total
top10 = t_cada_bike[order(t_cada_bike$dados.tripduration, decreasing = TRUE)[1:10],]
bottom10 = t_cada_bike[order(t_cada_bike$dados.tripduration)[1:10],]

summary(t_cada_bike$dados.tripduration)

# reduzir o df para somente as bikes usadas todos os dias
bue = usedeveryday$Var1
usedeveryday = dados[dados$bikeid %in% bue,]
# somar o tempo de cada bicicleta
dadosbike_tempo = data.frame(usedeveryday$tripduration, usedeveryday$bikeid)
dadosbike_tempo = dadosbike_tempo[order(dadosbike_tempo$usedeveryday.bikeid),]
dadosbike_tempo$usedeveryday.bikeid = as.factor(dadosbike_tempo$usedeveryday.bikeid)
t_cada_bike = dadosbike_tempo %>% 
  group_by(across(where(is.factor))) %>% 
  summarise(across(everything(), sum))

#melhor:
# base_por_bike=base %>% group_by(bikeid) %>% summarise(tempo_de_uso=sum(tripduration),
#                                                      dias_utilizada=n_distinct(dia_comeco))



# recomecando a analise
t_cada_bike$usedeveryday.tripduration <- t_cada_bike$usedeveryday.tripduration/tempo_total
top10 = t_cada_bike[order(t_cada_bike$usedeveryday.tripduration, decreasing = TRUE)[1:10],]
bottom10 = t_cada_bike[order(t_cada_bike$usedeveryday.tripduration)[1:10],]

summary(t_cada_bike$usedeveryday.tripduration)
