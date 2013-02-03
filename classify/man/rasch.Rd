\name{rasch}
\alias{rasch}
\title{Rasch Derived Probabilities}
\description{Calculate vector of probabilities of success from person and item parameters under the Rasch model.}
\usage{
  rasch(theta=NULL, delta=NULL)
}
\arguments{
          \item{theta}{Vector of theta parameters}
          \item{delta}{Vector of delta parameters}
}          

\details{Calculates vector of probabilities of success from person and item parameters under the Rasch model.}
\value{Vector of probabilities of success, persons in columns, items in rows}
          
\examples{

theta <- c(-5:5)
delta <- c(-5:5)
rasch(theta,delta)

}