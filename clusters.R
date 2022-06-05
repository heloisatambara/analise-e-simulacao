# LISTA 5
# Heloisa Tambara 12556819

#### exercicio a) ####
# get the data
df = data.frame(read.csv('E:/usp/estudos/estat/mae399/201806-citibike-tripdata.csv'))

# aparar
a = as.integer(quantile(df$tripduration, c(.01, .99))[1])
b = as.integer(quantile(df$tripduration, c(.01, .99))[2])

df = df[df$tripduration >= a & df$tripduration <= b,]

df = df[, c('start.station.id', 'end.station.id', 'start.station.latitude', 'start.station.longitude', 'end.station.latitude','end.station.longitude' )]


# get the most frequent start station
mostFreqStart = names(sort(table(df$start.station.id), decreasing=T)[1])

# data frame with only the most frequent station
df2 = df[df$start.station.id == mostFreqStart,]

# get the 9 most frequent end stations
mostFreqEnd = names(sort(table(df2$end.station.id), decreasing=T)[1:9])

# station list
stations = sort(c(mostFreqEnd, mostFreqStart))

# data frame with only the 10 selected stations
df2 = df[df$start.station.id %in% stations,]
df2 = df2[df2$end.station.id %in% stations,]

# this part will be used in exercise d
long1 = unique(df2$start.station.longitude)
lat1 = unique(df2$start.station.latitude) 

#### exercicio b) ####

# cria a matriz de transicao
cluster1 = matrix(prop.table(table(df2$start.station.id, df2$end.station.id)), nrow = 10)
colnames(cluster1) = stations
rownames(cluster1) = stations
cluster1


#### exercicio c) ####

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


#### exercicio d) ####
library(mapview)
FirstCluster = data.frame(longitude =long1, latitude = lat1, cluster = 1)
SecondCluster = data.frame(longitude = long2, latitude = lat2, cluster = 2)
clusters = rbind(FirstCluster, SecondCluster, ThirdCluster, FourthCluster, FifthCluster)
mapview(clusters, xcol = "longitude", ycol = "latitude", grid = FALSE, crs = 4269, zcol = 'cluster')




#### exercicio e) ####
# how many stations
aux = length(unique(df$start.station.id))

# rodar esse entre cada stations e cluster
df2 = df[df$start.station.id %in% stations,]
df2 = df2[df2$end.station.id %in% stations,]
long1 = unique(df2$start.station.longitude)
lat1 = unique(df2$start.station.latitude)
cluster1 = matrix(prop.table(table(df2$start.station.id, df2$end.station.id)), nrow = length(stations))
colnames(cluster1) = stations
rownames(cluster1) = stations
cluster1


#
stations = names(sort(table(df$start.station.id), decreasing=T)[1:153])
FirstCluster = data.frame(longitude =long1, latitude = lat1, cluster = 1)

stations = names(sort(table(df$start.station.id), decreasing=T)[153:306])
SecondCluster = data.frame(longitude =long1, latitude = lat1, cluster = 2)


stations = names(sort(table(df$start.station.id), decreasing=T)[306:459])
ThirdCluster = data.frame(longitude =long1, latitude = lat1, cluster = 3)


stations = names(sort(table(df$start.station.id), decreasing=T)[459:612])
FourthCluster = data.frame(longitude =long1, latitude = lat1, cluster = 4)



stations = names(sort(table(df$start.station.id), decreasing=T)[612:aux])
FifthCluster = data.frame(longitude = df2$start.station.longitude, latitude = df2$start.station.latitude, cluster = 5)


clusters = rbind(FirstCluster, SecondCluster, ThirdCluster, FourthCluster, FifthCluster)
mapview(clusters, xcol = "longitude", ycol = "latitude", grid = FALSE, crs = 4269, zcol = 'cluster')




#### might use this ####

