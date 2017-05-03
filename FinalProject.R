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
"

Call:
lm(formula = tY ~ E2 + E3 + E4 + E5 + G1 + G2 + G4 + G5 + G6 + 
G7 + G8 + G9 + G10 + G11 + G12 + G13 + G14 + G15 + E2:G1 + 
E2:G7 + E2:G10 + E3:G1 + E3:G2 + E3:G13 + E4:G11 + E4:G12 + 
E5:G15 + G1:G12 + G2:G4 + G4:G10 + G4:G11 + G4:G14 + G4:G15 + 
G5:G7 + G5:G9 + G6:G7 + G8:G13 + G9:G15 + G10:G13 + G11:G15 + 
G12:G13 + G12:G14, data = df)

Residuals:
Min       1Q   Median       3Q      Max 
-0.37431 -0.07364 -0.00047  0.07114  0.34555 

Coefficients:
Estimate Std. Error t value Pr(>|t|)    
(Intercept)  1.772e+00  7.997e-01   2.216 0.026775 *  
E2          -9.093e-06  2.694e-05  -0.337 0.735785    
E3          -5.600e-05  2.975e-05  -1.882 0.059947 .  
E4           1.335e-04  3.745e-05   3.565 0.000371 ***
E5          -5.785e-05  3.770e-05  -1.535 0.125008    
G1          -6.722e-04  3.993e-04  -1.684 0.092382 .  
G2          -4.338e-04  1.675e-04  -2.590 0.009664 ** 
G4          -9.220e-04  6.180e-04  -1.492 0.135846    
G5          -1.429e-04  3.330e-04  -0.429 0.667794    
G6           6.330e-04  2.626e-04   2.411 0.015991 *  
G7           1.134e-03  5.372e-04   2.111 0.034913 *  
G8           3.741e-04  2.280e-04   1.641 0.100944    
G9          -1.496e-03  4.912e-04  -3.046 0.002345 ** 
G10          2.440e-03  3.093e-04   7.889 4.72e-15 ***
G11         -1.154e-03  5.244e-04  -2.200 0.027885 *  
G12         -7.914e-04  2.616e-04  -3.025 0.002517 ** 
G13          2.476e-04  5.281e-04   0.469 0.639231    
G14         -4.593e-04  3.753e-04  -1.224 0.221231    
G15         -1.459e-03  4.731e-04  -3.084 0.002070 ** 
E2:G1        7.722e-08  2.823e-08   2.735 0.006283 ** 
E2:G7        4.667e-08  2.657e-08   1.757 0.079101 .  
E2:G10      -6.564e-08  2.585e-08  -2.539 0.011191 *  
E3:G1       -6.907e-08  2.980e-08  -2.318 0.020563 *  
E3:G2        5.446e-08  2.499e-08   2.179 0.029401 *  
E3:G13       3.319e-07  2.974e-08  11.160  < 2e-16 ***
E4:G11       4.856e-08  2.945e-08   1.649 0.099328 .  
E4:G12      -5.056e-08  2.742e-08  -1.844 0.065302 .  
E5:G15       5.456e-08  3.432e-08   1.590 0.112012    
G1:G12       8.299e-07  3.479e-07   2.386 0.017134 *  
G2:G4        4.911e-07  2.935e-07   1.674 0.094335 .  
G4:G10      -7.931e-07  3.501e-07  -2.265 0.023579 *  
G4:G11       5.757e-07  3.610e-07   1.595 0.110930    
G4:G14      -4.764e-07  2.957e-07  -1.611 0.107319    
G4:G15       7.349e-07  3.778e-07   1.945 0.051863 .  
G5:G7       -7.005e-07  3.391e-07  -2.066 0.038956 *  
G5:G9        9.381e-07  3.311e-07   2.834 0.004643 ** 
G6:G7       -5.377e-07  3.581e-07  -1.502 0.133343    
G8:G13      -5.616e-07  3.695e-07  -1.520 0.128618    
G9:G15       8.108e-07  3.969e-07   2.043 0.041164 *  
G10:G13     -8.179e-07  3.949e-07  -2.071 0.038456 *  
G11:G15      7.321e-07  4.419e-07   1.657 0.097716 .  
G12:G13      5.134e-07  3.402e-07   1.509 0.131442    
G12:G14      6.217e-07  2.965e-07   2.097 0.036120 *  
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.1063 on 2239 degrees of freedom
Multiple R-squared:  0.8421,	Adjusted R-squared:  0.8391 
F-statistic: 284.3 on 42 and 2239 DF,  p-value: < 2.2e-16

"

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
