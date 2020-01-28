data = read.csv("~/projects/reg/bodyfatmen.csv")
Y = as.matrix(data$density)
X = as.matrix(data[,c(3,4,6,8,9, 7)])

all_regressors = function(Y,X) {
  a = 1:length(X[1,])
  r2s = c(0)
  combs = list()
  combs_names = c()
  index = 1
  
  for (i in 1:length(X[1,])) {
    combinations = as.matrix(combn(a,i))
    
    for (comb in 1:length(combinations[1,])){
      vars = as.matrix(combinations[,comb])
      model = lm(Y ~ X[,vars])
      r2adj = summary(model)$adj.r.squared
      r2s = c(r2s, r2adj) 
      combs[index] = list(vars)
      combs_names[index] = toString(index)
      index = index + 1
    }
    
  }
  names(combs) = combs_names
  output <- list(r2s, combs)
  names(output) = c( "R2", "combs")
  output
}

results = all_regressors(Y, X)
R2s = results$R2
print(R2s)
combs = results$combs
sorted = order(R2s)

print(combs[sorted[(length(sorted) - 5):length(sorted)]])
print(combs$"52")
plot(R2s)
