##########################################################################################
# Part A

# Read files
x = read.csv("A 1 1 .csv", header = T , na.string = "NULL")
y = read.csv("A 1 2 .csv", header = T, na.string = "NULL")
A = merge(x,y,by="Patientno")

# List wise deletion
A = na.omit(A)

# anova test
anovaA = aov(formula = A$DV ~ A$IV)

# Output results
summary(anovaA)

##########################################################################################
# Part B

# Read the file
#B = read.csv("B1refined.csv")  # read csv file 
B = read.csv("B 1 .csv")

# Plot
par(mfrow=c(1,2))

plot(B$IV, B$DV, col="black", main="Scatter plot of IV vs DV")
lines(lowess(B$IV, B$DV), col=2)

plot(log(B$IV), B$DV, col="blue", main="Scatter plot of log(IV) vs DV")
lines(lowess(log(B$IV), B$DV), col=2)

# Linear model for log and factor
fitB = lm(DV ~ (IV), data = B)
lnfitB = lm(DV ~ log(IV), data = B)
log10fitB = lm(DV ~ log10(IV), data = B)
log2fitB = lm(DV ~ log2(IV), data = B)
log1000fitB = lm(DV ~ log(IV, base = 1000), data = B)
sqrtfitB = lm(DV ~ sqrt(IV), data = B)
fitB1 = lm(DV ~ factor(IV), data = B)
fitB2 = lm(DV ~ factor(log(IV)), data = B)
fitB3 = lm(DV ~ factor(sqrt(IV)), data = B)

#anova(fitB1)
#anova(fitB1, fitB2)

#par(mfrow=c(3,1))
#plot(B$IV, fitB$residuals, main="IV vs Residual")
#plot(log(B$IV), lnfitB$residuals, main="log(IV) vs Residual")
#plot(sqrt(B$IV), sqrtfitB$residuals, main="sqrt(IV) vs Residual")

par(mfrow=c(2,2))
##########################################################################################
# Reset the plot dimension
par(mfrow=c(1,1))

lnlnfitB = lm(log(DV) ~ log(IV), data = B)
lnlnfitB2 = lm(log(DV) ~ log(IV) + log(log(IV)), data = B)
lnlnfitB3 = lm(log(DV) ~ log(IV) + log(log(IV)) + log(log(log(IV))), data = B)

lnlnfitB4 = lm(log(DV) ~ factor(log(IV) + log(log(IV) + log(log(log(IV))))), data = B)
