\name{tpl}
\alias{tpl}
\title{Two Parameter IRT Model Derived Probabilities}
\description{Calculate vector of probabilities of success from person and item parameters under the 2pl IRT model.}
\usage{
  tpl(theta=NULL, beta=NULL, alpha=NULL)
}
\arguments{
          \item{theta}{Vector of theta parameters}
          \item{beta}{Vector of beta parameters}
          \item{alpha}{Vector of alpha parameters}
}          

\details{Calculates vector of probabilities of success from person and item parameters under the 2pl IRT model.}
\value{Vector of probabilities of success, persons in columns, items in rows}
          
\examples{

theta <- c(-2:2)
beta <- rep(0,5)
alpha <- seq(from=0.2,by=0.2,to=1)
tpl(theta,beta,alpha)

}