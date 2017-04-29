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

# Calculate confidence interval
confint(fitA, level = 0.99)

##########################################################################################
# Part B

# Read the file
B = read.csv("B 1 .csv")  # read csv file 

# Plot
par(mfrow=c(1,2))

plot(B$IV, B$DV, col="black", main="Scatter plot of IV vs DV")
lines(lowess(B$IV, B$DV), col=2)

plot(log(B$IV), B$DV, col="blue", main="Scatter plot of log(IV) vs DV")
lines(lowess(log(B$IV), B$DV), col=2)

lnfit1 = lm(DV ~ log(IV), data = B)
lnfit2 = lm(DV ~ factor(IV), data = B)

# Analyze
aovB = aov(DV ~ log(IV), data = B)
anova(lnfit1, lnfit2)

# Output results
summary(aovB)

# Calculate confidence interval
confint(lnfit1, level = 0.99)

##########################################################################################
