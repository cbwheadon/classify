\name{classify}
\alias{classify}
\title{Calculate Classification Statistics}
\description{Internal function to calculate classification statistics.}
\usage{classify(cssd, expected, bnds, cats, lbls=NULL)}

\arguments{
  \item{cssd}{Conditional Summed Score Distribution}
  \item{expected}{Numeric vector of Expected Scores}
  \item{bnds}{Numeric vector of grade boundaries, specified in ascending order, including the minimum and maximum mark on the test.}
  \item{cats}{Numeric vector of item categories.}
  \item{lbls}{Character vector of grade labels. Optional.}
}
\value{
	List of classification statistics: \cr
	Candidate level accuracy \cr
	Candidate level false negatives \cr
	Candidate level false positives \cr
	Summary of consistency \cr
	Matrix of grade probability combinations \cr
	Kappa \cr
	Candidate level classification consistency \cr
	Summary of accuracy by grade \cr
	Summary of false positives by grade \cr
	Summary of false negatives by grade \cr
	Summary of consistency by grade
}