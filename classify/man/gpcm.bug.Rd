\name{gpcm.bug}
\alias{gpcm.bug}
\title{
Extract IRT Model Parameters from Bugs Models
}
\description{
Internal function which draws heavily on Curtis, S.M.(2010).
}
\usage{
gpcm.bug(v, cats, mdl, gibbs=c("bugs","jags"))
}
\arguments{
  \item{v}{Bugs sims matrix}
  \item{cats}{Numeric vector of item categories}
  \item{mdl}{Bugs file: Partial Credit Model - "pcm.bug" or Generalised Partial Credit Model - "gpcm.bug" or Rasch model - "rasch.bug" or 2pl model "tpl.bug"}
  \item{gibbs}{Gibbs sampler: "bugs" or "jags"}
}
\details{Extracts IRT Model Parameters from Bugs Models}
\value{
	List with theta and beta parameters
}
\references{
Curtis, S.M.(2010) BUGS Code for Item Response Theory, \emph{Journal of Statistical Software}, Code Snippets, \bold{36(1)},1--34
}