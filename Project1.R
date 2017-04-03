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
B = read.csv("B1refined.csv")  # read csv file 

# Plot
plot(mydata$IV, mydata$DV)

# Linear model for log and factor
fitB1 = lm(DV ~ log(IV), data = B)
fitB2 = lm(DV ~ factor(IV), data = B)

anova(fit1, fit2)
##########################################################################################