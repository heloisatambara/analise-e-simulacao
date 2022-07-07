# P2 - MAE0399
# Heloisa Tambara 

#### Exercicio 2 ####
# a.
## Parametros iniciais
X = c(50, 15, 15)
matriculas = X
pa = .6
pr = .25
pd = .15

sa = .7
sr = .2
sd = .1


## Walk
var = c(0)
nalunos1 = c(X[1])
nalunos2 = c(X[2])
nalunos3 = c(X[3])
bool = 3
anos = 1
while (bool) { 
  x1=X[1]
  x2=X[2]
  x3=X[3]
  
  X[1] = X[1]*pr + matriculas[1]
  X[2] = X[1]*pa + X[2]*sr + matriculas[2]
  X[3] = X[2]*sa + X[3]*sr + matriculas[3]
  
  if (X[3] * .05 > abs(X[3]-x3)) {
    bool = bool-1
  }
  var = append(var, abs(X[3]-x3)/X[3])
  anos = anos + 1
  nalunos1 = append(nalunos1, X[1])
  nalunos2 = append(nalunos2, X[2])
  nalunos3 = append(nalunos3, X[3])
}


plot(c(1:anos), nalunos3, type="n")

lines(c(1:anos), nalunos1,col = "Red")
lines(c(1:anos),nalunos2, col = "Blue")
lines(c(1:anos), nalunos3,col = "Yellow")

anos-3 # ano que converge

# b.
nalunos1 # 66 alunos
nalunos2 # 68 alunos
nalunos3 # 78 alunos

# c.
matriculas = c(runif(1, 45, 55), runif(1, 10, 20), runif(1, 10, 20))


