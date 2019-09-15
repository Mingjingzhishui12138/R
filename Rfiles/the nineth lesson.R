############Regression Analysis

#######ps: qq plot is used for make sure the residues comform to normal distributions
#######1.simple linear regression
par(mfrow=c(2,2))
fit <- lm(weight~height, data = women)
fit
residuals(fit)
plot(women$height,women$weight,
     main = "Regression",
     xlab = "height",
     ylab = "weight",
     col = c("red","green","black"))
abline(fit)
lines(women$height,women$weight)
fitted(fit)

residuals(fit)

#########2.mpolynomial regression(power = 2)
fit2 <- lm(weight~height + I(height^2),data = women)
fit2
summary(fit2)

plot(women$height, women$weight,
     xlab = "height",
     ylab = "weight",
     main = "Regression2",
     col = c("purple","white"),
     pch = 17
     )
lines(women$height, women$weight,lty = 10)

lines(women$height, fitted(fit2),col = "green")
#abline(fit2)
#########3. polynomial regression (power = 3)
fit3 <- lm(weight~height + I(height^2) + I(height^3), data = women)
fit3
summary(fit3)
#plot(fit3)
plot(women$height,women$weight,
     xlab = "height",
     ylab = "weight",
     main = "Regression3",
     col = c("green","yellow")
     )
lines(women$height, fitted(fit3),col = "red")

##########4.using scatterplot function to plot scatter plot
library(carData)
library(car)
scatterplot(weight~height,data=women,
            spread = FALSE,
            smooth.args=list(Ity=2),pch=19,
            main = "Women Age 30~39",
            xlab = "Height(Inches)",
            ylab = "Weight (Lbs)")

###########5. multi-factor regression--- with interaction
states <- as.data.frame(state.x77[,c("Murder", "Population","Illiteracy", "Income", "Frost")])
cor(states)
library(carData)
library(car)
scatterplotMatrix(states,spread=FALSE,smoother.arg = list(lty = 2),
                  main = "Scatter Plot Matrix")

###########6. multi-factor linear regression---with interaction

fit4<- lm(mpg~hp + wt+ wt:hp, data = mtcars)
summary(fit4)
plot(mtcars$hp,mtcars$mpg,
     xlab = "hp",
     ylab = "mpg",
     main = "regression4")
abline(fit4)
lines(mtcars$hp,fitted(fit4))

#use effect function to see the interaction effects on regression plot
library(carData)
library(effects)
plot(effect("hp:wt",fit4,,list(wt = c(2.2,3.2,4.2))),multiline = TRUE)


###########6. use confit function to analyse regression
states <- as.data.frame(state.x77[,c("Murder", "Population","Illiteracy", "Income", "Frost")])
fit5 <- lm(Murder~Population + Illiteracy + Income + Frost, data=states)
confint(fit5)
plot(fit5)


############7. testing regression 
fit6 <- lm(weight~height, data = women)
par(mfrow = c(2,2))
plot(fit6)
fit7 <- lm(weight~height + I(height^2),data = women)
par(mfrow = c(2,2))
plot(fit7)
# as the 13th and 15th observed points seem like distracting point, we can delete them
newfit7 <- lm(weight~height + I(height^2), data = women[-c])
# my test 
newstates <- as.data.frame(state.x77[,c("Murder", "Population","Illiteracy", "Income", "Frost")])
fit8 <- lm(Murder~Population+Illiteracy +Income + Frost, data = newstates)
par(mfrow = c(2,2))
plot(fit8)
plot(fitted(fit8), newstates$Murder,
     xlab = "a",
     ylab = "b",
     main = "main")

############8. strengthened regression and fitting
#testing homoscedasticity by using qqPlot Function(qqPlot, not qqplot)
library(carData)
library(car)
states <- as.data.frame(state.x77[,c("Murder", "Population","Illiteracy", "Income", "Frost")])
fit9 <- lm(Murder~ Population + Illiteracy + Income + Frost, data = states)
qqPlot(fit9,
       labels = row.names(states),
       id.method = "identify",
       simulate = TRUE,
       main = "Q-Q Plot"
)

