#### ler o arquivo ####

dados = read.csv('E:/usp/estudos/ESTAT/mae399/202102-citibike-tripdata.csv')
dados = data.frame(dados)
# arrumar datas
dados$started_at = strptime(dados$started_at, "%Y-%m-%d %H:%M:%S" )
dados$ended_at = strptime(dados$ended_at, "%Y-%m-%d %H:%M:%S" )

#### tempo de uso ####
tempo = (as.numeric(dados$ended_at - dados$started_at))/60
tempo_de_uso = data.frame(tempo) # em minutos
dados$tempo <- tempo
dados = dados[dados$tempo > 2 & dados$tempo < 1440, ]


# retirar outliers das quantitativas
# retirar valores negativos e valores acima de 24h - casos especiais
tempo1 = as.numeric(tempo_de_uso[tempo_de_uso$tempo > 2 & tempo_de_uso$tempo <= 30,]) # ate 30 min
tempo2 = as.numeric(tempo_de_uso[tempo_de_uso$tempo > 30 & tempo_de_uso$tempo <= 60,]) # 30 min a 60
tempo3 = as.numeric(tempo_de_uso[tempo_de_uso$tempo > 60 & tempo_de_uso$tempo <= 120,]/60) # 1 a 2h
tempo4 = as.numeric(tempo_de_uso[tempo_de_uso$tempo > 120 & tempo_de_uso$tempo <= 1440,]/60) # 2 a 7h
tempo_de_uso1 = data.frame(tempo1)
tempo_de_uso2 = data.frame(tempo2)
tempo_de_uso3 = data.frame(tempo3)
tempo_de_uso4 = data.frame(tempo4)


#### regiões ####
summary(dados$start_lat) 
summary(dados$start_lng) # variação de 0,14 na longitude
summary(dados$end_lat)# variação de 0,21 na latitude
summary(dados$end_lng)

# brooklin: lat < 40.725
# manhattan: lat > 40.725
brkstart = dados[dados$start_lat < 40.725,]
manstart = dados[dados$start_lat > 40.725,]
brkend = dados[dados$end_lat < 40.725,]
manend = dados[dados$end_lat > 40.725,]


plot(table(manend$start_lat))

# pela diferença nos graficos de brk, nota-se um movimento de saida do norte e ida para o sul
# o movimento contrário em manhattan


#### periodo ####
# mais movimento de tarde, depois de manha e por ultimo de noite:
manha = dados[format(dados$started_at, '%H:%M:%S') < '12:00:00',] 
tarde = dados[format(dados$started_at, '%H:%M:%S') > '12:00:01' & format(dados$started_at, '%H:%M:%S') < '19:00:00'  ,] 
noite = dados[format(dados$started_at, '%H:%M:%S') > '19:00:01',]

#### membros ####
casual = dados[dados$member_casual == 'casual',]
member = dados[dados$member_casual == 'member',]


#### estatisticas basicas ####
summary(tempo_de_uso)
summary(strptime(format(dados$started_at, '%H:%M:%S'), '%H:%M:%S'))


plot(table(dados$start_lat), ylab = 'frequência', xlab = 'latitude')
plot(table(dados$start_lng), ylab = 'frequência', xlab = 'longitude')
plot(table(dados$end_lat), ylab = 'frequência', xlab = 'latitude')
plot(table(dados$end_lng), ylab = 'frequência', xlab = 'longitude')

# freq tempo de uso

plot(table(as.numeric(strptime(format(dados$started_at, '%H:%M:%S'), '%H:%M:%S'))), xlab = '00:00 - 23h59', ylab = 'frequência', xaxt = "n", yaxt = "n")

#### bivariadas ####
## membro x periodo
memberm = dados[dados$member_casual == 'member'&format(dados$started_at, '%H:%M:%S') < '12:00:00',]
membert = dados[dados$member_casual == 'member'&format(dados$started_at, '%H:%M:%S') > '12:00:01' & format(dados$started_at, '%H:%M:%S') < '19:00:00',]
membern = dados[dados$member_casual == 'member'&format(dados$started_at, '%H:%M:%S') > '19:00:01',]

casualm = dados[dados$member_casual == 'casual'&format(dados$started_at, '%H:%M:%S') < '12:00:00',]
casualt = dados[dados$member_casual == 'casual'&format(dados$started_at, '%H:%M:%S') > '12:00:01' & format(dados$started_at, '%H:%M:%S') < '19:00:00',]
casualn = dados[dados$member_casual == 'casual'&format(dados$started_at, '%H:%M:%S') > '19:00:01',]


## membro x tempo
summary(member$tempo)
var(member$tempo)
summary(casual$tempo)
var(casual$tempo)
member = member[member$tempo <60, ]
casual = casual[casual$tempo <60, ]
plot(table(casual$tempo))
plot(table(member$tempo))


