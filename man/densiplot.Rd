% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/densiplot.R
\name{densiplot}
\alias{densiplot}
\title{Plot of Denisty Estimation}
\usage{
densiplot(x,n=500, method="naive", from= min(x)-(sd(x)/3), to=max(x)+(sd(x)/3))
}
\arguments{
\item{x}{numeric value}

\item{n}{number of points to be used for plotting the density function.}

\item{method}{the method for density estimation. It can be "naive" or "kernel".}

\item{from}{specifies from which point the method should begin.}

\item{to}{specifies where the plot should stop.}
}
\value{
A density plot based on the specified method.
}
\description{
This function calls the function densi(x,d=NULL,h=NULL, method="naive") and creates a density plot based on the specified method.
}
\examples{
densiplot(faithful$eruptions)
}
\author{
Katrine Eriksen and Katrine Bach \cr
Department of mathematics and computer science (IMADA) \cr
University of Southern Denmark \cr
\email{kater13@student.sdu.dk} and \email{kabac13@student.sdu.dk}
}
\keyword{density}

