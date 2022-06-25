# Heloisa Tambara
# 12556819

#### Ruina do Jogador ####

#### Exercicio 1 ####

# a.
## Parametros iniciais
X = c(10, 20, 30, 40)
p = 18/38
q = 1-p


# Formula
P = c(0,0,0,0)
for (i in 1:4) {
  P[i] = (1 - (q/p)^X[i])/(1-(q/p)^50)
}

## Random walk
x = X[4]
soma = c(0,0,0,0)
bool = 1
for (i in 1:4){
  for (j in 1:1000) {
    x = X[i]
    bool = 1
    while (bool) {
      prob = runif(1,0,1)
      if (prob <= p) {result = 1}
      else {result = -1}
      x = x + result
      if (x==0) { bool = 0 } 
      if (x==50) {
        soma[i] = soma[i] + 1
        bool = 0
      }
    }
  }
}
soma = soma/1000
plot(c(10,20,30,40), soma, type = "n")
lines(c(10,20,30,40), P, col = "Red")
lines(c(10,20,30,40), soma, col = "Blue")
P
soma
# b
# rodar tudo de novo com
p=1/2
P = c(0,0,0,0)
for (i in 1:4) {
  P[i] = p*X[i]/25
}

#### Exercicio 2 ####
# a
N = c(10, 20, 30, 40, 50, 60, 70, 80, 90, 100)
for (n in N){
  x = c()
  count = 0
  for (i in 1:1000) {
    tempo = 0 
    eventos = 0
    while (eventos < n) {
      Ti = rexp(1,1)
      tempo = tempo + Ti
      eventos = eventos + 1
    }
    count = count +1
    x[count] = tempo
    
  }
  print(quantile(x, c(.05, .95))) 
}

n = 10
for (n in N){
  vetor = pnorminv(.05, n, sqrt(n))
  print(vetor)
}
?dnorm

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
  y1 = x + 3 * sqrt(x) # ii
  y2 = x - 3 * sqrt(x) # ii
  y = c(1:eventos)
  lines(x,y1, col = color1[i]) # ii
  lines(x,y2, col = color1[i]) # ii
  lines(x,y, col = color[i])
}
