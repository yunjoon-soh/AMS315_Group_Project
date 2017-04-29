##########################################################################################
# Project 2
input = read.csv("Group1.csv", header = T)
df = data.frame(input)

# Generate a big plot
cnames = colnames(input)
par(mfrow=c(5,4))
for(i in 1:20){
  #ti=paste("DV vs ", cnames[i])
 # plot(input[[i]], input[[21]])
#  title(main=ti, xlab=str(i))
  var(input[[i]], input$Y)
}


for(i in 1:20){
  print(var(input[[i]], input$Y))
}

k <- ncol(M) #number of variables
n <- nrow(M) #number of subjects

#create means for each column
M_mean <- matrix(data=1, nrow=n) %*% cbind(mean(a),mean(b),mean(c),mean(d),mean(e)) 

#creates a difference matrix
D <- M - M_mean

#creates the covariance matrix
C <- (n-1)^-1 t(D) %*% D