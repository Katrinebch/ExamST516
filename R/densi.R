#' @export
#' @title Estimation of Density
#' @usage densi(x,d=NULL,h=NULL, method="naive")
#' @keywords density
#' @description This function estimates density using the naive estimator or gaussian kernel.
#' @param x numeric value
#' @param d a point that is evaluatet to obtain its density
#' @param h bin width
#' @param method the method for density estimation. It can be "naive" or "kernel".
#' @author Katrine Eriksen and Katrine Bach \cr
#' Department of mathematics and computer science (IMADA) \cr
#' University of Southern Denmark \cr
#' \email{kater13@student.sdu.dk} and \email{kabac13@student.sdu.dk}
#' @examples densi(faithful$eruptions)



densi <- function(x,d=NULL,h=NULL, method="naive"){
  #warnings/stops
  if (!is.numeric(x)){stop("x must be numeric")}
  if (length(x)<1){stop("The length of the x cannot be negative")}
  if (length(x)==0){warning("The input must exist")}

  if (method=="naive"){
  }else if (method=="kernel"){
  }else {stop("The method does not exist for this program")}


  n<-length(x)

  #Generates h
  if(length(h)==0){#Generates h if there is no input value
    if (method=="kernel"){#Generates h according to Silverman.
      h <- 0.9*min(c(IQR(x)/1.34,sd(x)))*n^(-1/5)
    }else{
      h<- (max(x)-min(x))/(1+log2(n))#Generates h according to Struges.
    }
  }else{

    h <- h
  }
#Generates d
  if(length(d)==0){# Generates d to be the minimum, maksimum, mean and all the quantiles.
    minimum<-densi(x,min(x),h,method)
    maksimum<-densi(x,max(x),h, method)

    quantile1<-quantile(x,names=FALSE)[2]
    q1<-densi(x,quantile1,h,method)

    quantile3<-quantile(x,names=FALSE)[4]
    q3<-densi(x,quantile3,h,method)

    medians <- densi(x,median(x),h,method)
    means <- densi(x,mean(x),h,method)
  #Creates the table
    smoke <- matrix(c(min(x),minimum,quantile1,q1,median(x),medians,mean(x),means,quantile3,q3,max(x),maksimum), ncol = 2, byrow = TRUE)
    colnames(smoke) <- c("x","y")
    rownames(smoke) <- c("Min","1st Quantile","Median", "Mean", "3dr Quantile", "Max")
    smoke <- as.table(smoke)
    return(smoke)

  }else{#If the input argument is not NULL then the d value is used.
    d=d
  }

  #Implementation of Gaussian kernel according to theory.
  if(method=="kernel"){
    kern<-0
    for (i in 1:n) {
      kern[i] <-(1/h)*dnorm((d-x[i])/h)
    }
    summ<- sum(kern)
    f<-(1/n)*summ
    return(f)
  }else{# Implementation of naive method according to theory.
    nav <- 0
    for(i in 1:n){
      if(abs((d-x[i])/h)<1){
        w <- 1/2
      }else{
        w <- 0
      }
      nav[i] <-(1/h)*w
    }
    summ <- sum(nav)
    f <- (1/n)*summ
    return(f)
  }

}


