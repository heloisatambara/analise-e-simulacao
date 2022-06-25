# Heloisa Tambara
# 12556819

#### Ruina do Jogador ####

#### Exercicio 1 ####

# Formula
P = c(0,0,0,0)
for (i in 1:4) {
  P[i] = (1 - (q/p)^X[i])/(1-(q/p)^50)
}
plot(c(1,2,3,4),P )

# a.
## Parametros iniciais
X = c(10, 20, 30, 40)
p = 18/38
q = 1-p

## Random walk
x = X[1]
soma = c(0,0,0,0)
soma2 = 0
bool = 1
for (i in 1:4){
  for (j in 1:1000) {
    x = X[i]
    bool = 1
    while (bool) {
      result = sample(c(-1,1), 1, replace = TRUE, prob = c(p, q))
      x = x + result
      if (x==0) {
        soma2 = soma2 + 1 # para conferir
        bool = 0
      } 
      if (x==50) {
        soma[i] = soma[i] + 1
        bool = 0
      }
    }
  }
}
soma = soma/1000
i # conferir se chegou em 1000
plot(c(1,2,3,4), soma)
# b
# rodar tudo de novo com
p=1/2

#### Exercicio 2 ####
# a
N = c(10, 20, 30, 40, 50, 60, 70, 80, 90, 100)
eventos = 0
tempo = 0
for (n in N) {
  x = dexp(1:n,1)
  a = runif(1000,x)
  tempo = tempo + T
  eventos = eventos + 1
} 
dpois(x, 1)

plot(c(1:10),x)


# b
## i e ii
color1 = c(rgb(1,0,0,.5),
          rgb(0,1,0,.5),
          rgb(0,0,1,.5),
          rgb(1,1,0,.5),
          rgb(1,0,1,.5))

color = c(rgb(1,0,0,1),
           rgb(0,1,0,1),
           rgb(0,0,1,1),
           rgb(1,1,0,1),
           rgb(1,0,1,1))

plot(0,0, xlim=c(0,100), ylim=c(0,100),type="n")
for (i in 1:5) {
  x = c()
  tempo = 0 
  count = 0
  eventos = 0
  while (tempo < 100) {
    count = count + 1
    Ti = rexp(1,1)
    tempo = tempo + Ti
    eventos = eventos + 1
    x[count] = tempo
  }
  y1 = x + 3 * sqrt(x)
  y2 = x - 3 * sqrt(x)
  y = c(1:eventos)
  lines(x,y1, col = color1[i])
  lines(x,y2, col = color1[i])
  lines(x,y, col = color[i])
}
