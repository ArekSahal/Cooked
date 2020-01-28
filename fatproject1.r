fatdata <- read.csv("~/Documents/functionfinder/Rfolder/bodyfatmen.csv")
fatdata[1,]
study_parameter <- 5
X <- as.matrix(fatdata[,study_parameter])

mean <- mean(X)
var <- var(X)


Y <- as.matrix(fatdata[,1])
Ymean <- mean(Y)
Sxy = sum((X-mean)*(Y-Ymean))
Sxx = sum((X-mean)*(X-mean))
Beta1 <- Sxy/Sxx
Beta0 <- Ymean - mean*Beta1

plot(X,Y, type = "p", lty = 1)
lines(X,X*Beta1+Beta0,type = "l",lty = 1)

%%%%  F-test
SSR = sum(((X*Beta1+Beta0)-Ymean)**2 )
SSR
SSReg = sum((Y-(X*Beta1+Beta0))**2)
SSReg
F0 = (SSReg/1)/(SSR/(length(X)-1-1))
F0
F005 = 3.84

%%%% R2-test
R2 = SSReg/(SSR+SSReg)
R2
%%%% Confidence interval of mean of y
sigma2 = SSR/(length(X)-1-1)
sigma2
expYmean = (X*Beta1+Beta0)
varYmean = sigma2*(1/length(X)+(X-mean)**2/Sxx)
varYmean
lines(X,expYmean + 196*varYmean,type = "l",lty = 1)
lines(X,expYmean - 196*varYmean,type = "l",lty = 1)
%%%% Confidence interval of a new prediction
expY = (X*Beta1+Beta0)
varY = sigma2*(1+1/length(X)+(X-mean)**2/Sxx)
lines(X,expY + 196*varYmean,type = "x",lty = 1)
lines(X,expY - 196*varYmean,type = "x",lty = 1)