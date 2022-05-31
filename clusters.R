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

# lista das estacoes
stations = sort(c(mostFreqEnd, mostFreqStart))

# data frame with only the 10 selected stations
df2 = df[df$start.station.id %in% stations, c('start.station.id', 'end.station.id')]
df2 = df2[df2$end.station.id %in% stations,]

#### exercicio b) ####

# cria a matriz de transicao
cluster1 = matrix(prop.table(table(df2$start.station.id, df2$end.station.id)), nrow = 10)
colnames(cluster1) = stations
rownames(cluster1) = stations
cluster1


#### exercicio c) ####
# quantidade de estacoes
length(unique(df$start.station.id))

# retirar ocorrencias diferentes das stations
`%!in%` <- Negate(`%in%`)
df3 = df[df$start.station.id %!in% stations & df$end.station.id %!in% stations , c('start.station.id', 'end.station.id')]

# refazer tudo para novo cluster
# c.a
mostFreqStart = names(sort(table(df3$start.station.id), decreasing=T)[1])
df2 = df3[df3$start.station.id == mostFreqStart, c('start.station.id', 'end.station.id')]
mostFreqEnd = names(sort(table(df2$end.station.id), decreasing=T)[1:10]) # aqui peguei 10 pois a mostFreqStart está em mostFreqEnd
stations = sort(c(mostFreqEnd)) # somente o mostFreqEnd pois contem mosFreqStart
df2 = df3[df3$start.station.id %in% stations, c('start.station.id', 'end.station.id')]
df2 = df2[df2$end.station.id %in% stations,]
# c.b
cluster2 = matrix(prop.table(table(df2$start.station.id, df2$end.station.id)), nrow = 10)
colnames(cluster2) = stations
rownames(cluster2) = stations
cluster2
# c.c
df3 = df3[df3$start.station.id %!in% stations & df3$end.station.id %!in% stations , c('start.station.id', 'end.station.id')]
# rodar c.a de novo
cluster3 = matrix(prop.table(table(df2$start.station.id, df2$end.station.id)), nrow = 10)
colnames(cluster3) = stations
rownames(cluster3) = stations
cluster3

cluster4 = matrix(prop.table(table(df2$start.station.id, df2$end.station.id)), nrow = 10)
colnames(cluster4) = stations
rownames(cluster4) = stations
cluster4

cluster5 = matrix(prop.table(table(df2$start.station.id, df2$end.station.id)), nrow = 10)
colnames(cluster5) = stations
rownames(cluster5) = stations
cluster5

# quantos clusters tem que fazer?

cbind()
