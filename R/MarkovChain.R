#' @export
#' @title Markov Chain
#' @usage MarkovChain(p,k,n)
#' @keywords Markov
#' @description This function simulates the stationary distribution of a Markov Chain.
#' @return Returns a table of the stationary probabilities of each state.
#' @param p Transition probability matrix
#' @param k integer indicating the initial state of the chain
#' @param n number of simulated steps
#' @author Katrine Eriksen and Katrine Bach \cr
#' Department of mathematics and computer science (IMADA) \cr
#' University of Southern Denmark \cr
#' \email{kater13@student.sdu.dk} and \email{kabac13@student.sdu.dk}




MarkovChain <- function(p,k,n){
  #Warning/stops
  if (!is.matrix(p)){stop("The transition probability matrix must be a matrix")}
  if (dim(p)[1]!=dim(p)[2]){stop("The transition probability matrix must be a square matrix")}
  
  if(!is.numeric(n)){stop("n must be numeric")}
  if(n==0){warning("Must be positive and larger than 0")}
  if(n<0){stop("Must be positive")}
  
  if(!is.numeric(k)){stop("The inital state must be numeric")}
  if(k>dim(p)[1]){stop("The initial state must exist")}
  if (k<1){stop("The inital state must be positive and larger than zero")}
  
  set.seed(12)# Set seed
  x <- 0
  Y<- 0
  i<-k
  x[1]<-i
  t <- 1
  U <- runif(1)
  
  #Generate inital Y
  for (j in 1:dim(p)[1]) {#Generates a for loop from 1 to the dimension of p
    if (U<= p[i,1]){ #If U is less than or equal to the probability in the first column
      Y[i] <- 1
    }else if( sum(p[i,1:j-1])<U && U<= sum(p[i,1:j])) {#Otherwise U must be between the sum
      #of probabilities in the row until j-1 and the sum of probabilities in the rows until j
      Y[i]=j
    }
  }
  #Generate X2 from Y
  x[2]<-Y[i]
  
  # Generate Y as above
  while (t<n){
    i<-x[t]
    U<-runif(1)
    for (j in 1:dim(p)[1]) {
      if (U<= p[i,1]){
        Y[i] <- 1
      }else if( sum(p[i,1:j-1])<U && U<= sum(p[i,1:j])) {
        Y[i]=j
      }
    }
    t=t+1
    #Set X at the given time equal to Y
    x[t]<-Y[i]
    
  }#End while
  
  return(prop.table(table(as.matrix(x)))) #Returns a table
  
}#End function



