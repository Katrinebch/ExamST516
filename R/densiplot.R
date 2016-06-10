#' @export
#' @title Plot of Denisty Estimation
#' @usage densiplot(x,n=500, method="naive", from= min(x)-(sd(x)/3), to=max(x)+(sd(x)/3))
#' @keywords density
#' @description This function calls the function densi(x,d=NULL,h=NULL, method="naive") and creates a density plot based on the specified method.
#' @param x numeric value
#' @param n number of points to be used for plotting the density function.
#' @param method the method for density estimation. It can be "naive" or "kernel".
#' @param from specifies from which point the method should begin.
#' @param to specifies where the plot should stop.
#' @return A density plot based on the specified method.
#' @examples densiplot(faithful$eruptions)
#' @author Katrine Eriksen and Katrine Bach \cr
#' Department of mathematics and computer science (IMADA) \cr
#' University of Southern Denmark \cr
#' \email{kater13@student.sdu.dk} and \email{kabac13@student.sdu.dk}


densiplot <- function(x,n=500, method="naive", from= min(x)-(sd(x)/3), to=max(x)+(sd(x)/3)){
  #warnings/stops
  if (!is.numeric(x)){stop("x must be numeric")}
  if (length(x)<1){stop("The length of the x cannot be negative")}
  if (length(x)==0){warning("The input must exist")}

  if (!is.numeric(n)){stop("n must be numeric")}
  if (n<1){stop("The number of points to be plottet must be positive")}

  if (method=="naive"){
  }else if (method=="kernel"){
  }else {stop("The method does not exist for this program")}

  if (!is.numeric(from)){stop("The point must be numeric")}
  if (!is.numeric(to)){stop("The point must be numeric")}


  happy<-c()
  for (d in seq(from,to,length.out = n)) {
    happy1<-densi(x,d=d,method=method)
    happy<-c(happy,happy1)
  }
  plot(seq(from,to,length.out = n),happy, type = "l")
}
