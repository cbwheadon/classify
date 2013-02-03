\name{pcm}
\alias{pcm}
\title{Partial Credit Model Derived Probabilities}
\description{Calculate vector of probabilities of success from person and item parameters under the Partial Credit Model.}
\usage{
  pcm(theta=NULL, delta=NULL, n=NULL)
}
\arguments{
          \item{theta}{Theta parameter}
          \item{delta}{Vector of delta parameters}
          \item{n}{Number of item categories}
}          

\details{The delta parameters are the intersection points of adjacent category information functions. There should be one less delta parameter than categories.}
\value{Vector of probabilities of success}
          
\examples{

# Example from The Theory and Practice of Item Response Theory
# By Rafael Jaime De Ayala
# p.204
theta <- 0
n <- 3
d <- c(-1,1)

pcm(theta,d,n)

#0.2119416 0.5761169 0.2119416
}