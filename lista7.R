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
tempo = 0 
eventos = 0
for (i in 1:5) {
  while (tempo < 100) {
    T = rexp(1,1)
    tempo = tempo + T
    eventos = eventos + 1
  }
}
?runif
