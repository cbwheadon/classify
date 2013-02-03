\name{plot Classification}
\alias{plot.scores}
\alias{plot.classification}
\title{Plot Methods for Classification and Scores s4 objects}
\description{
 Produces various plots of score distributions and classification statistics.
}

\usage{
\method{plot}{scores}(x, type = c("exp","cond","qq"),alpha = 0.05, \dots)
\method{plot}{classification}(x, type = c("acc", "kappa", "density"), \dots)
}

\arguments{
  \item{x}{ an object inheriting either from class \code{scores} or class \code{classification}}
  \item{type}{ the type of plot: \cr
	"exp": Expected summed scores compared to observed \cr
	"cond": Conditional summed scores compared to observed \cr
	"qq": QQ plot of conditional summed scores compared to observed \cr
	"acc": Classification accuracy \cr
	"kappa": Kappa \cr
	"density" Density \cr
  }
  \item{alpha}{Alpha value for points on qq plot.}
  \item{\dots}{ extra graphical parameters }
}	 

\details{Produces various plots of score distributions and classification statistics.}