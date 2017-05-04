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

for(i in 1:20){
  toAdd = input[[i]] * input[[i]]
  ti = paste(cnames[i], paste("*", cnames[i]))
  df = cbind(df, toAdd)
  colnames(df)[index] = ti
  index=index+1
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

# Residual standard error: 0.1078 on 2274 degrees of freedom
# Multiple R-squared:  0.8351,	Adjusted R-squared:  0.8346
# E3 + E4 + G6 + G10 +            E3 * G13 + G1 + G13
fit_mini = lm(tY~E3 + E4 + G6 + G10 + E3:G13 + G1 + G13, data=df)

# Residual standard error: 0.1078 on 2273 degrees of freedom
# Multiple R-squared:  0.8351,	Adjusted R-squared:  0.8345 
#      E4 + G6 + G10 +            E3 * G13 + G1 * G6
fit_sas = lm(tY~ E4 + G6 + G10 + E3:G13 + G1:G6, data=df)

# Residual standard error: 0.1076 on 2270 degrees of freedom
# Multiple R-squared:  0.8361,	Adjusted R-squared:  0.8353
# E3 + G10 + E4 + G13 + G6 + G1 + G2 + E3:G13 + E3:G1 + G10:G13 + E3:G2
fit_r = step(lm(tY~1, data=df), method = "forward", scope = as.formula(formulaStr))

# Residual standard error: 0.1078 on 2274 degrees of freedom
# Multiple R-squared:  0.8351,	Adjusted R-squared:  0.8346 
# E3 + E4 + G6 + G10 + G13 + G1 + E3 * G13
fit_r2 = lm(tY ~ E3 + E4 + G6 + G10 + G13 + G1 + E3:G13, data=df)

rm(cnames)
rm(i)
################################################################################
