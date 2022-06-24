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
soma = 0
for (i in 1000) {
  while (0 > x & x > 50) {
    result = sample(c(-1,1), 1, replace = TRUE, prob = c(20/38, 18/38))
    x = x + result
  }
  if (x == 50) {
    soma = soma + 1
    }
}
