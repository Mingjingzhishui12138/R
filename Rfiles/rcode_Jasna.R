#load the data
Data=read.csv('customer_analysis.csv')

# Dummy Variables are -
# * Qualitative variables taken from independent variable categorical values.
# * The number of dummy variables is always N-1, where N is the number of possible categorical values for the qualitative independent variable.
# * Dummy variables can take only 2 numeric values 0 or 1

#lets have a quick look at the data
str(Data)
#gender,age,city_Category, Stay_In_Current_City_Years are all colums of category type. And we need to convert them into dummy variables. While Marital_Status has already been a dummy variable, we can use it directly.
DUM_Data=data.frame(model.matrix(~Gender+Age+City_Category+Stay_In_Current_City_Years+Marital_Status+Purchase,Data))
#notice that if we have n categories for a variable, we need to use n-1 dummy variables to avoid the poblem of multicolinearity. The new variables created by function "model.matrix" could satisfy this after checking.
#Then we can build the regression model
#baseline model
cus_reg=lm(Purchase~.-X.Intercept.,data=DUM_Data)
summary(cus_reg)
# Residual plots
par(mfrow=c(2,2))
plot(cus_reg)
#We check D-W test value to see if the model satisfy regression assumptions.
require('lmtest')
require('car')
dwtest(cus_reg)
vif(cus_reg)
#Assumptions
#1.Error should follow normal distribution: Normal Q-Q plot looks linear. So this assumption is satisfied.
#2.No heteroscedasticity - Residual plot shows scattered plot. Hence, this assumption is met.
#3. No multicollinearity - The VIF values of all predictor variables are less than 5 so this assumption satisfied.
#4. No autocorrelation - D-W test value is 2.056 which is very close to 2. Hence, this assumption is also satisfied.

# alternative model
# here we try to use the all-subsets regression to find the best model
require('leaps')
leaps<-regsubsets(Purchase~.-X.Intercept.,data=DUM_Data, nbest=2)
plot(leaps, scale='adjr2')
#we can find that if we include variables corresponding to the black parts of the top row, we could achive largest adjr2: GenderM,Age18.25,Age26.35,Age36.45,Age46.50,City_CategoryC,Stay_In_Current_City_Years1 and Stay_In_Current_City_Years2, along with a intercept term.
#We can get conclusions here that: 1.if one is older than 50, whether he is older than 55 will not affect a lot. 2.Apart from city C, city A and City B will not affect a lot. 3. After staying in a city for more than 2 years, more specific years of living will not be important. 4 status of marital is not important
#Then the new model will be:
cus_reg_new=lm(Purchase~GenderM+Age18.25+Age26.35+Age36.45+Age46.50+City_CategoryC+Stay_In_Current_City_Years1+Stay_In_Current_City_Years2,data=DUM_Data)
summary(cus_reg_new)
# Residual plots
par(mfrow=c(2,2))
plot(cus_reg_new)
#We check D-W test value to see if the model satisfy regression assumptions.
require('lmtest')
require('car')
dwtest(cus_reg_new)
vif(cus_reg_new)
#Assumptions
#1.Error should follow normal distribution: Normal Q-Q plot looks linear. So this assumption is satisfied.
#2.No heteroscedasticity - Residual plot shows scattered plot. Hence, this assumption is met.
#3. No multicollinearity - The VIF values of all predictor variables are less than 5 so this assumption satisfied.
#4. No autocorrelation - D-W test value is 2.056 which is very close to 2. Hence, this assumption is also satisfied.
AIC(cus_reg,cus_reg_new)
#we can find that the new model could achive smaller AIC with using less variables, so the new model is not doubt better. This is coherent with our conclusion before.
summary(cus_reg_new)
#最后根据这个SUMMARY的结果分析就好了，比如几个负数的，说明C城人买的少，住的小于两年的人买的少，其他都是正。还可以同一组里面比较，比如都在年龄里面比较，46岁到50的会买的最多，相比其他年龄