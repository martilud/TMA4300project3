z = read.table('z.txt', header = FALSE, sep = "", dec = ".")
u = read.table('u.txt', header = FALSE, sep = "", dec = ".")
n = length(z[,])
lambda_0_next <- function(lambda_0_prev){
  sum = 0
  for (i in 1:n){
    if (u[i,] == 1){
      sum = sum + z[i,]
    }
    else {
      sum = sum + 1.0/lambda_0_prev - z[i,]/(exp(lambda_0_prev * z[i,])-1)
    }
  }
  return(n/sum)
}
lambda_1_next <- function(lambda_1_prev){
  sum = 0
  for (i in 1:n){
    if (u[i,] == 0){
      sum = sum + z[i,]
    }
    else {
      sum = sum + 1.0/lambda_1_prev - z[i,]/(exp(lambda_1_prev * z[i,])-1)
    }
  }
  return(n/sum)
}
MAXITER = 10
lambda_0_list = numeric(MAXITER)
lambda_1_list = numeric(MAXITER)
lambda_0 = 1.0
lambda_1 = 1.0
lambda_0_list[1] = lambda_0
lambda_1_list[1] = lambda_1
for (j in 1:(MAXITER-1)){
    lambda_0_list[j+1] = lambda_0_next(lambda_0_list[j])
    lambda_1_list[j+1] = lambda_1_next(lambda_1_list[j])
}

plot(lambda_1_list, type="b", main="", xlab = "Iterations", ylab="", col="red")
lines(lambda_0_list, type="b", col="blue")
#legend("topleft", main=c(expression(lambda[1]), expression(lambda[0])), col = c("red","blue"), type=c("b","b"))
  