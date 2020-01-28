fatdata <- read.csv("~/Documents/functionfinder/Rfolder/bodyfatmen.csv")
fatdata[1,]

X <- as.matrix(fatdata)

colmean <- colMeans(X)
colvar <- diag(var(X))
X
for(i in 1:length(X[,2])) {
  X[i,] <- (X[i,] - colmean)/colvar
}
Y <- X[,1]
X[,1] = 1
X
typeof(X)

H <- X%*%solve(t(X)%*%X)%*%t(X)
dim(H)
Beta <- solve(t(X)%*%X)%*%t(X)%*%Y
C <- solve(t(X)%*%X)
Y
Xplot <- X
means <- colMeans(X) 
study_parameter <- 2
for( i in 2:length(X[2,])) {
  if (i != study_parameter) {
    Xplot[,i] <- means[i]
  }
}
Xplot
Beta
plot(X[,study_parameter],Y, type = "p", lty = 1)
lines(X[,study_parameter],Xplot%*%Beta,type = "l",lty = 1)
