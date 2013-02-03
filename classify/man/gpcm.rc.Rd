\name{gpcm.rc}
\alias{gpcm.rc}
\title{IRT Derived Predicted Conditional Number Correct Score Distribution}
\description{Obtains the predicted number-correct score distribution from parameters estimated under the Generalised Partial Credit Model.}
\usage{
  gpcm.rc(beta=NULL,theta=NULL,cats=NULL,alpha=NULL)
}
\arguments{
		  \item{beta}{Item threshold parameters. These should be a matrix, with rows for items and columns for categories. Following Muraki, the first column should be zero.}
          \item{theta}{Theta parameters}
          \item{cats}{Vector of item categories. A dichotomous item is specified as two categories.}
          \item{alpha}{Discrimination parameters. If none are specified the model will default to the Partial Credit Model.}
}          

\details{The beta parameters are defined as the intersection points of adjacent category information functions. There should be the same number of beta parameters as categories, with the first column, following Muraki, specified as zero.}
\value{Vector of probabilities of achieving any item score}
\examples{

beta <- matrix(c(0,-1.586,-3.798),nrow=1,ncol=3)
theta <- 0.674
cats <- 3
alpha <- 1
gpcm.rc(beta,theta,cats,alpha)

}