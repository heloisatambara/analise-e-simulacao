# LISTA 5
# Heloisa Tambara 12556819

#### exercicio a) ####
# get the data
df = data.frame(read.csv('E:/usp/estudos/estat/mae399/201806-citibike-tripdata.csv'))
df = df[, c('start.station.id', 'end.station.id', 'start.station.latitude', 'start.station.longitude', 'end.station.latitude','end.station.longitude' )]

# get the msot frequent start station
mostFreqStart = names(sort(table(df$start.station.id), decreasing=T)[1])

# data frame with only the most frequent station
df2 = df[df$start.station.id == mostFreqStart,]

# get the 9 most frequent end stations
mostFreqEnd = names(sort(table(df2$end.station.id), decreasing=T)[1:9])

# lista das estacoes
stations = sort(c(mostFreqEnd, mostFreqStart))

# data frame with only the 10 selected stations
df2 = df[df$start.station.id %in% stations,]
df2 = df2[df2$end.station.id %in% stations,]
long1 = unique(df2$start.station.longitude)
lat1 = unique(df2$start.station.latitude) 

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
df3 = df[df$start.station.id %!in% stations & df$end.station.id %!in% stations,]

# refazer tudo para novo cluster
# c.a
mostFreqStart = names(sort(table(df3$start.station.id), decreasing=T)[1])
df2 = df3[df3$start.station.id == mostFreqStart,]
mostFreqEnd = names(sort(table(df2$end.station.id), decreasing=T)[1:10]) # aqui peguei 10 pois a mostFreqStart está em mostFreqEnd
stations = sort(c(mostFreqEnd)) # somente o mostFreqEnd pois contem mosFreqStart
df2 = df3[df3$start.station.id %in% stations,]
df2 = df2[df2$end.station.id %in% stations,]
long2 = unique(df2$start.station.longitude)
lat2 = unique(df2$start.station.latitude) 
# c.b
cluster2 = matrix(prop.table(table(df2$start.station.id, df2$end.station.id)), nrow = 10)
colnames(cluster2) = stations
rownames(cluster2) = stations
cluster2

#### c.c mais clusters ####
df3 = df3[df3$start.station.id %!in% stations & df3$end.station.id %!in% stations,]
# rodar linha 57 e c.a a cada novo cluster
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


#### exercicio d) ####
library(mapview)

# maybe get unique long and lat

FirstCluster = data.frame(longitude =long1, latitude = lat1, cluster = 1)
SecondCluster = data.frame(longitude = long2, latitude = lat2, cluster = 2)
clusters = rbind(FirstCluster, SecondCluster)
mapview(clusters, xcol = "longitude", ycol = "latitude", grid = FALSE, crs = 4269, zcol = 'cluster')
  
