#' @export
#' @title Buffon's Neddle experiment
#' @usage buffon(N,l,d)
#' @keywords buffon
#' @description This function estimates the probability of a needle to hit a line, P(hit), and pi by performing Buffon's experiment.
#' @param N number of needles to be thrown on the floor.
#' @param l length of the needle.
#' @param d distance between the stripes on the floor.
#' @return P(hit)\cr
#' pi
#' @author Katrine Eriksen and Katrine Bach \cr
#' Department of mathematics and computer science (IMADA) \cr
#' University of Southern Denmark \cr
#' \email{kater13@student.sdu.dk} and \email{kabac13@student.sdu.dk}
#' @examples buffon(1000,1,1)


buffon <- function(N,l,d){
  a <- 0
  b <- pi
  U <- runif(N)
  theta <- (b-a)*U+a
  f <- l*sin(theta)
  integralet <- sum((f/N))*(b-a) #the estiamtion
  Phit<- integralet/(pi*d)
  pie <- (2*l)/(Phit*d)
  return(pie)
}
