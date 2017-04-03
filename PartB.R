B = read.csv("B1refined.csv")  # read csv file 

plot(mydata$IV, mydata$DV)

fit1 = lm(DV ~ log(IV), data = B)
fit2 = lm(DV ~ factor(IV), data = B)

anova(fit1, fit2)
