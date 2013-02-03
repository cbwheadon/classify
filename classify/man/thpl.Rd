\name{thpl}
\alias{thpl}
\title{Three Parameter IRT Model Derived Probabilities}
\description{Calculate vector of probabilities of success from person and item parameters under the 3pl IRT model.}
\usage{
  thpl(theta=NULL,beta=NULL,alpha=NULL,eta=NULL)
}
\arguments{
          \item{theta}{Vector of theta parameters}
          \item{beta}{Vector of beta parameters}
          \item{alpha}{Vector of alpha parameters}
          \item{eta}{Vector of eta parameters}
}          

\details{Three Parameter IRT Model Derived Probabilities}
\value{Vector of probabilities of success, persons in columns, items in rows}
          
\examples{

theta <- c(-2:2)
beta <- rep(0,5)
alpha <- rep(1,5)
eta <- seq(from=0.2,by=0.2,to=1)
thpl(theta,beta,alpha,eta)

}