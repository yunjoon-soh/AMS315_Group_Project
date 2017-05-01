##########################################################################################
# Project 2
input = read.csv("Group1.csv", header = T)
df = data.frame(input)

# Generate a big plot
cnames = colnames(input)

par(mfrow=c(5,4))

for(i in 1:20){
  # plot it
  plot(input[[i]], input[[21]])
  
  # Create title for each plot
  ti=paste("DV vs ", cnames[i])
  
  # set title
  title(main=ti, xlab=str(i))
}

cvc = cov(df)

anova(lm(Y ~ E1 + E2 + E3 + E4 + E5 + G1 + G2 + G3 + G4 + G5 + G6 + G7 + G8 + G9 + G10 + G11 + G12 + G13 + G14 + G15, data = df))

require("MASS")
require("leaps")

reg1 = regsubsets(Y ~ E1 + E2 + E3 + E4 + E5 + G1 + G2 + G3 + G4 + G5 + G6 + G7 + G8 + G9 + G10 + G11 + G12 + G13 + G14 + G15, data = df)
summary(reg1)

reg2 = regsubsets(Y ~ (E3 + E4 + E5 + G1 + G2 + G6 + G10 + G13)^2, data=df)
summary(reg2)

reg3 = regsubsets(Y ~ (E1 + E2 + E3 + E4 + E5 + G1 + G2 + G3 + G4 + G5 + G6 + G7 + G8 + G9 + G10 + G11 + G12 + G13 + G14 + G15)^2, data = df, really.big = T)
summary(reg3)
