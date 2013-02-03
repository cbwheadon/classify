\name{expected.rc}
\alias{expected.rc}
\title{Expected scores under the PCM or the GPCM.}
\description{Calculates expected scores under the PCM or the GPCM.}
\usage{
  expected.rc(beta=NULL,theta=NULL,cats=NULL,alpha=NULL)
}
\arguments{
          \item{beta}{Matrix of Beta parameters}
          \item{theta}{Vector of Theta parameters}
          \item{cats}{Vector Item category parameters}
          \item{alpha}{Vector of Alpha parameters (optional)}
}          

\details{The beta parameters are the intersection points of adjacent category information functions. There should be one less delta parameter than categories. Assumes that the location parameter is zero. If no alpha parameters are supplied it assumes the PCM.}
\value{Vector of expected scores}

\examples{

#One item with three categories
beta <- matrix(c(0,-1.586,-3.798),nrow=1,ncol=3)
theta <- 0.674
cats <- 3
alpha <- 1
expected.rc(beta,theta,cats,alpha)

}
