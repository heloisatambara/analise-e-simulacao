# importacoes
library(geosphere)
library(dplyr)

# le os dados
setwd('E:/usp/estudos/ESTAT/mae399')
dados = read.csv('202102-citibike-tripdata.csv')
dados = data.frame(dados)


# le as distancias
dados$distancia=distGeo(
  data.frame(dados$start_lat, dados$start_lng),
  data.frame(dados$end_lat, dados$end_lng))/1000

# gera o histograma real
hist(dados$distancia, main="Histograma de distância REAL", xlab="Distância (km)", ylab="Frequência", breaks=200)
axis(side=1, at=seq(0,12,by=1))

# gera o modelo direto da exp
x = rexp(1000,10)
hist(x, main="Histograma de distância MODELO1", xlab="Distância (km)", ylab="Frequência", breaks=200)
axis(side=1, at=seq(0,1.2,by=1))

# gera o modelo da uniforme e passa pra exp
x = (-1/10)*log(runif(1000, 0, 1),base=exp(1))
hist(x, main="Histograma de distância MODELO2", xlab="Distância (km)", ylab="Frequência", breaks=200)
axis(side=1, at=seq(0,1.2,by=1))

  
#DESAFIO
# media das distancias
dist = dados[dados$distancia > 0 & !is.na(dados$distancia), 'distancia'] # distancias sem 0s e NAs
media = mean(dist)


# porcentagem de 0s
table = table(dados$distancia == 0)
p = as.numeric(table[2]/table[1])


# gera valores numa distribuição
U = runif(1000, 0, 1)
Up = U[U<=p]
UP = U[U>p]
x = (-1/media)*log(UP, base=exp(1))
x = append(x, rep(0, length(Up)))
hist(x, main="Histograma de distância DESAFIO", xlab="Distância (km)", ylab="Frequência", breaks=200)
axis(side=1, at=seq(0,1.2,by=1))
