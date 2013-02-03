\name{gpcm}
\alias{gpcm}
\title{Generalised Partial Credit Model Derived Probabilities}
\description{Calculate vector of probabilities of success from person and item parameters under the Generalised Partial Credit Model.}
\usage{
  gpcm(theta=NULL,alpha=NULL,delta=NULL,n=NULL)
}
\arguments{
          \item{theta}{Theta parameter}
          \item{alpha}{Alpha parameter}
          \item{delta}{Vector of delta parameters}
          \item{n}{Number of item categories}
}          

\details{The delta parameters are the intersection points of adjacent category information functions. There should be one less delta parameter than categories. Assumes that the location parameter is zero}
\value{Vector of probabilities of success}
          
\examples{

## Generalized Partial Credit Model
## Item parameters from Embretson & Reise (2000, p. 112) item 5
theta <- 1
alpha <- .683
delta <- c(-3.513,-.041,.182)
n <- 4
gpcm(theta,alpha,delta,n)

}