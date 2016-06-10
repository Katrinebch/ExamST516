#' @export
#' @title Bootstrap
#' @usage bootstrap(n, x, y)
#' @keywords bootstrap, standard error, correlation
#' @description This function uses the boostrap method to estimate the correlation, standard error, bias and confidence interval. The function takes a .txt, which contains the x and y variables, as an input argument.
#' @return Correlation between x and y \cr
#' The bootstrap estimate of correlation \cr
#' The bootstrap estimation of standard error\cr
#' Bias \cr
#' Confidence interval
#' @param n number of bootstrap replicates
#' @param x variable
#' @param y variable
#' @author Katrine Eriksen and Katrine Bach \cr
#' Department of mathematics and computer science (IMADA) \cr
#' University of Southern Denmark \cr
#' \email{kater13@student.sdu.dk} and \email{kabac13@student.sdu.dk}




bootstrap <- function(n,x,y){
  #Warnings and stop
  if(n<1){stop('n must be larger or equal to 1')}
  if(!is.numeric(n)){stop("n must be numeric")}
  if(length(x)<2 ){stop("x must be a single row vector")}
  if(length(y)<2 ){stop("y must be a single row vector")}
  if(length(x)!=length(y)){stop("x and y must have the same length")}

  #Calculation the correlation between the variables og interest.
  theta.hat <- cor(x,y)

  #Standard Error
  set.seed(516)
  N <- nrow(data) # sample size (number of rows)
  storage <- numeric(n) #Store the variables
  for (i in 1:n) {
    k <- sample(1:N, size = N, replace = TRUE) # random indice
    storage[i] <- cor(x[k],y[k])
  }
  se <- sd(storage) #standard error
  hist(storage, probability = TRUE)

  #bias of sample correlation
  theta.hat.boot <- mean(storage)
  bias <- theta.hat.boot - theta.hat

  #95% confidence interval
  alfa = 0.05
  plus <- theta.hat.boot + qnorm(1-(alfa/2))*se
  minus <- theta.hat.boot - qnorm(1-(alfa/2))*se
  return(list("exact_corr"=theta.hat,"boot_corr"=theta.hat.boot, "se"=se, "bias"=bias, "confidence_interval"=c(plus, minus)))
}
