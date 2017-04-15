##########################################################################################
# Part A

# Read files
x = read.csv("A 1 1 .csv", header = T , na.string = "NULL")
y = read.csv("A 1 2 .csv", header = T, na.string = "NULL")
A = merge(x,y,by="Patientno")

# List wise deletion
A = na.omit(A)

# Linear Model
fitA = lm(formula = A$DV ~ A$IV)

# Output results
summary(fitA)
anova(fitA)

##########################################################################################
# Part B

# Read the file
B = read.csv("B 1 .csv")  # read csv file 

# Plot
plot(B$IV, B$DV)

# Linear model for log and factor
lnfit1 = lm(DV ~ log(IV), data = B)
lnfit2 = lm(DV ~ factor(IV), data = B)

# Analyze
aovB = aov(DV ~ log(IV), data = B)
anova(lnfit1, lnfit2)

# Output results
summary(aovB)

##########################################################################################