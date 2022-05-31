# LISTA 5
# Heloisa Tambara 12556819

#### exercicio a) ####
# get the data
df = data.frame(read.csv('E:/usp/estudos/estat/mae399/201806-citibike-tripdata.csv'))

# get the msot frequent start station
mostFreqStart = names(sort(table(df$start.station.id), decreasing=T)[1])

# data frame with only the most frequent station
df2 = df[df$start.station.id == mostFreqStart, c('start.station.id', 'end.station.id')]

# get the 9 most frequent end stations
mostFreqEnd = names(sort(table(df2$end.station.id), decreasing=T)[1:9])

# data frame with only the 10 selected stations
df2 = df[df$start.station.id == mostFreqStart | df$start.station.id == mostFreqEnd, c('start.station.id', 'end.station.id')]
df2 = df2[df2$end.station.id == mostFreqEnd | df2$end.station.id == mostFreqStart,]



#### exercicio b ####
# cria a matriz de transicao
a = matrix(table(df2$start.station.id, df2$end.station.id), nrow = 10)
colnames(a) = stations
rownames(a) = stations


#### might use this ####

# lista das estacoes
stations = sort(c(mostFreqEnd, mostFreqStart))

# pegar pares igual na P1
transitions = c(paste(as.character(df3$start.station.id),'to', as.character(df3$end.station.id),sep=" "))
summary(as.factor(transitions))
substr(transitions[1], 1, 3)
trans.matrix <- function(X, prob=T)
{
  tt <- table( c(X[,-ncol(X)]), c(X[,-1]) )
  if(prob) tt <- tt / rowSums(tt)
  tt
}
library('markovchain')

cbind()