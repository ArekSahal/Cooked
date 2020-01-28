data = read.csv("~/projects/reg/bodyfatmen.csv")
Y = data$density
X = as.matrix(data[-1])

n = length(Y)
p = length(X[1,]) + 1

model = lm(Y ~ X)
summary(model)
plot(data$abdomen, Y)
residuals = resid(model)
plot(residuals)

qqnorm(residuals)
qqline(residuals)

#### Cook's D
H = X%*%solve(t(X)%*%X)%*%t(X)
s2 = as.double(t(residuals)%*%residuals / (n - p))
D = (residuals**2/(p*s2))*(diag(H)/((1 - diag(H))**2))
data[D > 0.1,]
data[4,]



stud_res = studres(model)
plot(stud_res)
qqnorm(stud_res)
qqline(stud_res)

#### Cook's D
H = X%*%solve(t(X)%*%X)%*%t(X)
s2 = as.double(t(stud_res)%*%stud_res / (n - p))
D = (stud_res**2/(p*s2))*(diag(H)/((1 - diag(H))**2))
plot(D)
data[D > 0.1,]
data[4,]

clean_Y = Y[abs(stud_res) < 2]
clean_X = X[abs(stud_res) < 2,]
clean_model = lm(clean_Y ~ clean_X)
summary(model)
summary(clean_model)

stud_res = studres(clean_model)
plot(stud_res)
qqnorm(stud_res)
qqline(stud_res)

#### Cook's D
H = clean_X%*%solve(t(clean_X)%*%clean_X)%*%t(clean_X)
s2 = as.double(t(stud_res)%*%stud_res / (n - p))
D = (stud_res**2/(p*s2))*(diag(H)/((1 - diag(H))**2))
plot(D)
data[D > 0.1,]
data[4,]




#### Partial F-test
Fs = c()
for ( feature in 1:length(clean_X[1,])) {
  partial_model = lm(clean_Y ~ clean_X[,-feature])
  
  F0 = anova(partial_model, clean_model)$F[2]
  print(summary(F0))
  
  Fs = c(Fs, F0)
  
}

plot(Fs)
lines(c(1,13), c(qf(0.95, n - p, p - 1), qf(0.95, n - p, p - 1)))


#### Variable selection

vars = c()
sorted_fs = order(Fs)
r2s = c()

for (i in 1:length(Fs)) {
  vars = c(vars, sorted_fs[length(Fs) - i + 1])
  new_model = lm(clean_Y ~ clean_X[,vars])
  r2adj = summary(new_model)$adj.r.squared
  r2s = c(r2s, r2adj)
  
}

plot(r2s)


### Another Partial F-Test
good_variables
good_variables = sorted_fs[(length(sorted_fs) - 6 + 1):length(sorted_fs)]
part_f = anova(lm(clean_Y ~ clean_X[,good_variables]), lm(clean_Y ~ clean_X[,good_variables[-3]]))
summary(part_f)
qf(0.95, (n-7),( 6))


look = 1
final_X = as.matrix(clean_X)

final_model = lm(clean_Y ~ clean_X[,good_variables])
Beta = final_model$coefficients

plot(clean_X[,good_variables[look]], cbind(1, final_X[,good_variables])%*%Beta)
plot(model)




