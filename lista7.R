# Heloisa Tambara
# 12556819

#### Ruina do Jogador ####

#### Exercicio 1 ####

# Formula
1 - (q/p)


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
      result = sample(c(-1,1), 1, replace = TRUE, prob = c(20/38, 18/38))
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

i # conferir se chegou em 10000


