#' @export
#' @title Goodness of Fit
#' @usage GoodnessOfFit(x,p)
#' @keywords GOF. goodness-of-fit
#' @description This function perfomes chi-squared test and goodness-of-fit test.
#' @param x Snak med cille bille
#' @param p the expected probability for all outcomes
#' @author Katrine Eriksen and Katrine Bach \cr
#' Department of mathematics and computer science (IMADA) \cr
#' University of Southern Denmark \cr
#' \email{kater13@student.sdu.dk} and \email{kabac13@student.sdu.dk}



GoodnessOfFit <- function(x,p=c(rep(1/length(x),length(x)))){
  set.seed(13)
  n<-301
  Exp<-n*p
  Pearson<-((x-Exp)^2/Exp)
  GOF<-sum(Pearson,na.rm = FALSE)
  return(GOF)
}
