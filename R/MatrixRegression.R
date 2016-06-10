#' @export
#' @title Matrix Regression
#' @usage MatrixRegression(formula)
#' @keywords matrix regression
#' @description This function uses matrix regression to fit a simple or multiple linear regression model.
#' @return Returns a linear regression model
#' @param formula an object of class "formula": a symbolic description of the model to be fitted.
#' @author Katrine Eriksen and Katrine Bach \cr Department of mathematics and
#'   computer science (IMADA) \cr University of Southern Denmark \cr
#'   \email{kater13@student.sdu.dk} and \email{kabac13@student.sdu.dk}



MatrixRegression = function(formula){
  
  #Get the matrix so it can be used for evaluations
  call <- match.call()
  matri <- match.call() #Use to make matrix and so the code can be run like the lm function
  matri[[1L]] <- quote(stats::model.frame)
  matri <- eval(matri, parent.frame())
  
  #Dimension of matrix
  widthmatri <- dim(matri)[2] #Width=p+1
  heightmatri <- dim(matri)[1] #Height = n
  Y <- matri[,1] # Setting Y up
  X <- rep(1,heightmatri) #Start X with olumn of 1's.
  
  for(i in 2:widthmatri){
    X <- cbind(X,matri[,i]) # Add other variables to X.
  }
  
  #Calculates the parameters as defined in the theory
  Estimate = as.vector(solve(t(X)%*%X)%*%(t(X)%*%Y))#beta values
  yhat = X%*%Estimate
  ehat = as.vector(Y-X%*%Estimate)
  RSS = sum((ehat)^2)
  SST = sum((Y-mean(Y))^2)
  SSreg = sum((yhat-mean(Y))^2)
  R2=1-(RSS)/(SST)
  df= heightmatri-widthmatri#Generates the degrees of freedom
  
  Radj = 1- (RSS/(heightmatri-widthmatri))/(SST/(heightmatri-1))
  Ftest = (SSreg/(widthmatri-1))/(RSS/(heightmatri-widthmatri))
  pvalue = 1-pf(Ftest, heightmatri- widthmatri,widthmatri-1)
  S2 = RSS/(heightmatri-widthmatri)
  ResidualStandardError = sd(ehat)
  StandardError = sqrt(diag(S2*solve(t(X)%*%X)))
  Tvalue = Estimate/StandardError
  Pr = 1-pt(Tvalue,df)
  print(data.frame(Estimate, StandardError, Tvalue, Pr))
  
  #Generates the table
  cat((sprintf("Residual standard error %.4f on %i degrees of freedom \n",ResidualStandardError, df)),
      (sprintf("R-squared:%4f \n", R2 )),
      (sprintf("Adjusted R-squared: %4f\n", Radj )),
      (sprintf("F-statistics %2f with the cooresponding p value %4f\n", Ftest, pvalue )))
  
}#ends function