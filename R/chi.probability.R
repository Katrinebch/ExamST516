#' @export
#' @title Chi square probability
#' @usage chi.probability(x, df, n)
#' @keywords bootstrap
#' @description This function applies the Monte Carlo method to generate a chi-squared distribution.
#' @param x the point where we want to calculate the probability.
#' @param df degree of freedom
#' @param n number of random numbers for each variable.
#' @author Katrine Eriksen and Katrine Bach \cr
#' Department of mathematics and computer science (IMADA) \cr
#' University of Southern Denmark \cr
#' \email{kater13@student.sdu.dk} and \email{kabac13@student.sdu.dk}
#' @examples chi.probability(10,4,100)


chi.probability <-function(x, df, n){
  set.seed(13)
  summ <- 0
  Y <- 0
  for (i in 1:df) {
    Y <- rnorm(n, mean=0, sd=1)
    Yianden <- Y^2
    summ <- summ + Yianden
  }
  blup <- ecdf(summ)
  plot(blup)
  p <-1-blup(x)
  return(p)

}
