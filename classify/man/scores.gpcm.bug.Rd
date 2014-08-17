\name{scores.gpcm.bug}
\alias{scores.gpcm.bug}
\title{Expected and Conditional Summed Score Distributions}
\description{
Obtains the predicted number-correct score distribution from the IRT models specified in the bugs files pcm.bug, gpcm.bug, rasch.bug, tpl.bug.
}
\usage{
scores.gpcm.bug(item.scores,sims,
  mdl = c("rasch.bug", "pcm.bug", "tpl.bug","gpcm.bug"),
  gibbs = c("bugs","jags"))
}
\arguments{
  \item{item.scores}{Matrix of item scores, one candidate per row.}
  \item{sims}{Winbugs sims matrix}
  \item{mdl}{Bugs file: "rasch.bug", "pcm.bug", "tpl.bug", "gpcm.bug"}
  \item{gibbs}{Gibbs sampler: "bugs" or "jags"}
}
\value{an object of class \code{\linkS4class{scores}} }