#' @export
#' @title Goodness of Fit
#' @usage GoodnessOfFit(x,p)
#' @keywords GOF. goodness-of-fit
#' @description This function perfomes chi-squared test and goodness-of-fit test.
#' @param x observed values
#' @param p the expected probability for all outcomes
#' @author Katrine Eriksen and Katrine Bach \cr
#' Department of mathematics and computer science (IMADA) \cr
#' University of Southern Denmark \cr
#' \email{kater13@student.sdu.dk} and \email{kabac13@student.sdu.dk}



GoodnessOfFit <- function(x,p=c(rep(1/length(x),length(x)))){
  #Warning/stops
  if(sum(p)!=1){stop("p must sum to 1 and be a vector of probabilities")}
  if(any(p<0)){stop("you can't have a negative probability")}
  if(length(x)!=length(p)){stop("x and p must have the same length")}

  set.seed(13)
  n<-301
  Exp<-n*p
  Pearson<-((x-Exp)^2/Exp)
  GOF<-sum(Pearson,na.rm = FALSE)
  return(GOF)

}
