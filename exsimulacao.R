#### iniciais ####
# importacoes
library(geosphere)
library(dplyr)

# leitura dos dados
setwd('E:/usp/estudos/ESTAT/mae399')
dados = read.csv('202102-citibike-tripdata.csv')
dados = data.frame(dados)

ipar = par()

#### EXERCICIO 1 ####
# c) colocar graficos lado a lado
par(mfrow=c(1,2)) 

# a) histograma direto da exponencial
x1 = rexp(10000,10) # lambda = 10
hist(x1, main="Histograma de distribuição rexp", xlab="", ylab="Frequência", breaks=200)
axis(side=1, at=seq(0,1.2,by=0.1))

# b) histograma da exponencial por transformada inversa
x2 = (-1/10)*log(runif(10000, 0, 1),base=exp(1))
hist(x2, main="Histograma de distribuição Transf. Inversa", xlab="", ylab="Frequência", breaks=200)
axis(side=1, at=seq(0,1.2,by=0.1))




#### EXERCICIO 2 ####
par(ipar)
# DADOS REAIS
## le as distancias
dados$distancia=distGeo(
  data.frame(dados$start_lat, dados$start_lng),
  data.frame(dados$end_lat, dados$end_lng))/1000

## arrumar datas
dados$started_at = strptime(dados$started_at, "%Y-%m-%d %H:%M:%S" )
dados$ended_at = strptime(dados$ended_at, "%Y-%m-%d %H:%M:%S" )

## tempo de uso
tempo = (as.numeric(dados$ended_at - dados$started_at))/60
tempo_de_uso = data.frame(tempo) # em minutos
dados$tempo <- tempo

## a) e b) tirar valores acima de 60 min e abaixo de 78,75s (tempo de 350m em menos de 16km/h)
dados = dados[dados$tempo < 60 & dados$tempo > 1.3125, ]

## c) gera o histograma
hist(dados$distancia, main="Histograma de distância aparado", xlab="Distância (km)", ylab="Frequência", breaks=200)
axis(side=1, at=seq(0,12,by=1))

## d) porcentagem de 0s
table = table(dados$distancia == 0)
p = as.numeric(table[2]/table[1])

## e) media das distancias
dist = dados[dados$distancia > 0 & !is.na(dados$distancia), 'distancia'] # distancias sem 0s e NAs
media = mean(dist)


# DADOS GERADOS
par(mfrow=c(1,2))
## gera valores numa distribuição
U = runif(10000, 0, 1)
Up = U[U<=p]
UP = U[U>p]
x = (-1/media)*log(UP, base=exp(1))
x = append(x, rep(0, length(Up)))
hist(x, main="Histograma de distância (modelo)", xlab="", ylab="Frequência", breaks=200)
hist(dados$distancia, main="Histograma de distância aparado", xlab="Distância (km)", ylab="Frequência", breaks=200)
