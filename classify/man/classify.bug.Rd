\name{classify.bug}
\alias{classify.bug}
\title{
Classification Accuracy and Consistency from Bugs Replicate Parameters
}
\description{
Calculates classification statistics under IRT models in bugs. 
}
\usage{
classify.bug(sims, scores, bnds, lbls=NULL)
}
\arguments{
  \item{sims}{bugs sims matrix.}
  \item{scores}{\code{\linkS4class{scores}}}
  \item{bnds}{Numeric vector of grade boundaries, monotonically increasing.}
  \item{lbls}{Character vector of grade labels. Optional.}
}
\details{Calculates classification statistics under IRT models in bugs.}
\value{
\code{\linkS4class{classification}} 
}