##########################################################################################
# Part A

# Read files
x = read.csv("A 1 1 .csv", header = T , na.string = "NULL")
y = read.csv("A 1 2 .csv", header = T, na.string = "NULL")
A = merge(x,y,by="Patientno")

# List wise deletion
total_modified = na.omit(A)

# Linear Model
linearAnalysis = lm(formula = total_modified$DV ~ total_modified$IV)

# Output results
summary(linearAnalysis)
anova(linearAnalysis)

##########################################################################################
# Part B

# Read the file
B = read.csv("B1refined.csv")  # read csv file 

# Plot
plot(mydata$IV, mydata$DV)

# Linear model for log and factor
fit1 = lm(DV ~ log(IV), data = B)
fit2 = lm(DV ~ factor(IV), data = B)

anova(fit1, fit2)
##########################################################################################