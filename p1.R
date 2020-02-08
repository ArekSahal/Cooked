data = read.csv("~/projects/reg/bodyfatmen.csv")
#### Model assesment (via data exploration)

            #### Plotting respons against all variables 

par(mfrow=c(4,4), mar=c(2,2,2,2))
for (i in 2:14) {
  plot( data[,i],data[,1],main=colnames(data)[i])
}
par(mfrow=c(1,1))

            #### correlations with the respons variable

barplot(abs(cor(data$density, data[-1])), main="Correlation coefficient between respons and variable")

            #### Distribution of our respons (need transformation?)

hist(data$density, main="Density distribution")




#### Model creation

model = lm(density ~ age + weight + height + neck + chest + abdomen + hip + thigh + knee + ankle + biceps + forearm + wrist, data=data)


#### Model Diagnostics

            #### Model summary (Partial f-tests)

suma = summary(model)
print(suma)

                      #### Higher values mean higher probability that the coefficients should be zero
barplot(suma$coefficients[,4], main="p-vlue for zero hypothesis")
lines(c(0,length(suma$coefficients)), c(0.05,0.05), col="red", lty=2)
lines(c(0,length(suma$coefficients)), c(0.1,0.1), col="blue", lty=2)
legend(12.5,0.8, legend=c("5%", "10%"), col=c("red", "blue"), lty=c(2,2))


            #### vif

barplot(vif(model))
lines(c(0,15), c(10,10), col="red")


#### Residual Analysis

res = rstandard(model)
par(mfrow=c(3,1))
plot(res, main="Residuals")
hist(res, main="Distribution of residuals")
qqnorm(res)
qqline(res)
