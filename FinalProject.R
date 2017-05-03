##########################################################################################
"Function to draw plot of IV vs DV"
drawPlot <- function(){
  #column names
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

  par(mfrow=c(1,1))
}
##########################################################################################
# Data creation
# Read data, box-cox transformation, add interaction varaibles

# Read input
input = read.csv("Group1.csv", header = T)
df = data.frame(input)

# Box-cox transformation exp(Y-45.7)
tY = exp(df$Y -45.7)
df = cbind(df, tY)

# Shapiro test for normality
shapiro.test(df$Y) # before transformation
shapiro.test(df$tY) # after transformation

# Generate new variable columns
cnames = colnames(input)

# Gene-Env
index = 23
for(i in 1:5){
  for(j in 6:20){
    toAdd = input[[i]] * input[[j]]
    ti = paste(cnames[i], paste("*", cnames[j]))
    df = cbind(df, toAdd)
    colnames(df)[index] = ti
    index=index+1
  }
}

# Gene-Gene
for(i in 6:19){
  for(j in (i+1):20){
    toAdd = input[[i]] * input[[j]]
    ti = paste(cnames[i], paste("*", cnames[j]))
    df = cbind(df, toAdd)
    colnames(df)[index] = ti
    index=index+1
  }
}

rm(i)
rm(j)
rm(index)
rm(toAdd)
rm(tY)
rm(ti)
rm(cnames)

# End of Data Creation
################################################################################
# Multiple Regression

# One big linear model
cnames = colnames(df)

# Generate string of the following format: tY ~ E1 + E2 + .. + G15 + E1 * G1 + ... + E5 * G15 + G1 * G2 + ... + G14 * G15
formulaStr = "tY ~ E1"
for(i in 2:length(cnames)){
  if(!grepl(cnames[i], "Y") && !grepl(cnames[i], "tY")){ # ignore Y and tY
    formulaStr=paste(formulaStr, paste("+", cnames[i]))
  }
}

# Linear fit
fit = lm(as.formula(formulaStr), data = df)

# Reduce the number of variables
require("MASS")
sAIC = stepAIC(fit)

rm(cnames)
rm(i)

"
Step:  AIC=-10188.31
tY ~ E2 + E3 + E4 + E5 + G1 + G2 + G4 + G5 + G6 + G7 + G8 + G9 + 
G10 + G11 + G12 + G13 + G14 + G15 + E2:G1 + E2:G7 + E2:G10 + 
E3:G1 + E3:G2 + E3:G13 + E4:G11 + E4:G12 + E5:G15 + G1:G12 + 
G2:G4 + G4:G10 + G4:G11 + G4:G14 + G4:G15 + G5:G7 + G5:G9 + 
G6:G7 + G8:G13 + G9:G15 + G10:G13 + G11:G15 + G12:G13 + G12:G14

Df Sum of Sq    RSS    AIC
<none>                 25.292 -10188
- G6:G7    1   0.02547 25.317 -10188
- G12:G13  1   0.02572 25.317 -10188
- G8:G13   1   0.02610 25.318 -10188
- E5:G15   1   0.02855 25.320 -10188
- G4:G11   1   0.02873 25.320 -10188
- G4:G14   1   0.02932 25.321 -10188
- E4:G11   1   0.03071 25.323 -10188
- G11:G15  1   0.03100 25.323 -10188
- G2:G4    1   0.03164 25.323 -10188
- E2:G7    1   0.03486 25.327 -10187
- E4:G12   1   0.03841 25.330 -10187
- G4:G15   1   0.04275 25.335 -10186
- G9:G15   1   0.04715 25.339 -10186
- G5:G7    1   0.04821 25.340 -10186
- G10:G13  1   0.04846 25.340 -10186
- G12:G14  1   0.04967 25.341 -10186
- E3:G2    1   0.05366 25.346 -10186
- G4:G10   1   0.05798 25.350 -10185
- E3:G1    1   0.06067 25.352 -10185
- G1:G12   1   0.06429 25.356 -10184
- E2:G10   1   0.07281 25.365 -10184
- E2:G1    1   0.08451 25.376 -10183
- G5:G9    1   0.09070 25.383 -10182
- E3:G13   1   1.40692 26.699 -10067

Call:
lm(formula = tY ~ E2 + E3 + E4 + E5 + G1 + G2 + G4 + G5 + G6 + 
G7 + G8 + G9 + G10 + G11 + G12 + G13 + G14 + G15 + E2:G1 + 
E2:G7 + E2:G10 + E3:G1 + E3:G2 + E3:G13 + E4:G11 + E4:G12 + 
E5:G15 + G1:G12 + G2:G4 + G4:G10 + G4:G11 + G4:G14 + G4:G15 + 
G5:G7 + G5:G9 + G6:G7 + G8:G13 + G9:G15 + G10:G13 + G11:G15 + 
G12:G13 + G12:G14, data = df)

Coefficients:
(Intercept)           E2           E3           E4           E5           G1           G2           G4  
1.772e+00   -9.093e-06   -5.600e-05    1.335e-04   -5.785e-05   -6.722e-04   -4.338e-04   -9.220e-04  
G5           G6           G7           G8           G9          G10          G11          G12  
-1.429e-04    6.330e-04    1.134e-03    3.741e-04   -1.496e-03    2.440e-03   -1.154e-03   -7.914e-04  
G13          G14          G15        E2:G1        E2:G7       E2:G10        E3:G1        E3:G2  
2.476e-04   -4.593e-04   -1.459e-03    7.722e-08    4.667e-08   -6.564e-08   -6.907e-08    5.446e-08  
E3:G13       E4:G11       E4:G12       E5:G15       G1:G12        G2:G4       G4:G10       G4:G11  
3.319e-07    4.856e-08   -5.056e-08    5.456e-08    8.299e-07    4.911e-07   -7.932e-07    5.757e-07  
G4:G14       G4:G15        G5:G7        G5:G9        G6:G7       G8:G13       G9:G15      G10:G13  
-4.764e-07    7.348e-07   -7.005e-07    9.381e-07   -5.377e-07   -5.616e-07    8.108e-07   -8.179e-07  
G11:G15      G12:G13      G12:G14  
7.321e-07    5.134e-07    6.217e-07  
"

################################################################################
fit = lm(tY ~ E1 + E2 + E3 + E4 + E5 + G1 + G2 + G3 + G4 + G5 + G6 + G7 + G8 + G9 + G10 + G11 + G12 + G13 + G14 + G15, data = df)

fit_sq = lm(Y ~ (E1 + E2 + E3 + E4 + E5 + G1 + G2 + G3 + G4 + G5 + G6 + G7 + G8 + G9 + G10 + G11 + G12 + G13 + G14 + G15)^2, data = df)

anova(fit)

require("VIF")


require("MASS")
require("leaps")
require("car") # for loading the vif

# Find subset of all linear model
reg1 = regsubsets(Y ~ E1 + E2 + E3 + E4 + E5 + G1 + G2 + G3 + G4 + G5 + G6 + G7 + G8 + G9 + G10 + G11 + G12 + G13 + G14 + G15, data = df)
summary(reg1)

fit_all_lin1 = lm(Y ~ (E3 + E4 + E5 + G1 + G2 + G6 + G10 + G13), data=df)
fit_all_lin2 = lm(Y ~ (E3 + E4 +      G1 + G2 + G6 + G10 + G13), data=df)
fit_all_lin3 = lm(Y ~ (E3 + E4 +      G1 +      G6 + G10 + G13), data=df) # maybe, maybe not
fit_all_lin4 = lm(Y ~ (E3 + E4 +                G6 + G10 + G13), data=df) # accept H0
fit_all_lin5 = lm(Y ~ (E3 + E4 +                     G10 + G13), data=df) # accept H0
fit_all_lin6 = lm(Y ~ (E3 + E4 +                     G10      ), data=df) # accept H0
fit_all_lin7 = lm(Y ~ (E3 +                          G10      ), data=df) # accept H0
fit_all_lin8 = lm(Y ~ (E3                                     ), data=df) # accept H0

# Find subset of non-linear model
# reasonable, i.e., < 10
mult_fit = lm(Y ~ (E3 * E4 * E5), data = df)
vif(mult_fit) # E3, E4, E5, E3:E4, E3:E5, E4:E5(~10), E3:E4:E5(~10)
mult_fit = lm(Y ~ (E3 * E4 * G1), data = df)
vif(mult_fit) # G1

# following is unreasonable
mult_fit = lm(Y ~ (E3 * E4 * G2), data = df)
vif(mult_fit)
mult_fit = lm(Y ~ (E3 * E4 * G6), data = df)
vif(mult_fit)
mult_fit = lm(Y ~ (E3 * E4 * G10), data = df)
vif(mult_fit)
mult_fit = lm(Y ~ (E3 * E4 * G13), data = df)
vif(mult_fit)

# E4 * E5
# Maybe (~ 10)
mult_fit = lm(Y ~ (E4 * E5 * G1), data = df)
vif(mult_fit)

# following is unreasonable
mult_fit = lm(Y ~ (E4 * E5 * G2), data = df)
vif(mult_fit)
mult_fit = lm(Y ~ (E4 * E5 * G6), data = df)
vif(mult_fit)
mult_fit = lm(Y ~ (E4 * E5 * G10), data = df)
vif(mult_fit)
mult_fit = lm(Y ~ (E4 * E5 * G13), data = df)
vif(mult_fit)

# E5 * G1
# following is unreasonable
mult_fit = lm(Y ~ (E5 * G1 * G2), data = df)
vif(mult_fit)
mult_fit = lm(Y ~ (E5 * G1 * G6), data = df)
vif(mult_fit)
mult_fit = lm(Y ~ (E5 * G1 * G10), data = df)
vif(mult_fit)
mult_fit = lm(Y ~ (E5 * G1 * G13), data = df)
vif(mult_fit)

# G1 * G2
# following is unreasonable
mult_fit = lm(Y ~ (G1 * G2 * G6), data = df)
vif(mult_fit)
mult_fit = lm(Y ~ (G1 * G2 * G10), data = df)
vif(mult_fit)
mult_fit = lm(Y ~ (G1 * G2 * G13), data = df)
vif(mult_fit)

# G2 * G6
# following is unreasonable
mult_fit = lm(Y ~ (G2 * G6 * G10), data = df)
vif(mult_fit)
mult_fit = lm(Y ~ (G2 * G6 * G13), data = df)
vif(mult_fit)

# G6 * G10
# following is unreasonable
mult_fit = lm(Y ~ (G6 * G10 * G13), data = df)
vif(mult_fit)

fit_all_lin1 = lm(Y ~ (E3 + E4 + E5 + G1 + G2 + G6 + G10 + G13)^2, data=df)
