\name{wlord}
\alias{wlord}
\title{
Lord and Wingersky Recursion Formula
}
\description{
The Lord and Wingersky Recursion Formula allows for efficient computation of the predicted number-correct score distribution (also known as the expected score distribution) given probabilities of correct responses to items.
}
\usage{
wlord(probs=NULL,cats=NULL)	
}
\arguments{
	\item{probs}{Probability matrix specifying the probability of achieving each category. There should be one row per item and one column per category. Where the number of categories differs between items, the matrix should be padded with zeros.}
	\item{cats}{Numeric vector specifying the number of categories in each item. A dichotomous item, for example, has two categories.}
}
\details{
The Lord and Wingersky Recursion Formula is a particularly useful short-cut in computations with the probabilities derived from IRT models. The algorithm simplifies the process of calculating the compound probabilities involved when the probability of achieving any score on an assessment instrument is required. It is essentially an elegant solution to combining the probabilities of responding in any particular category with the multiple ways in which any test score can be achieved through responses to different categories on different items.
}
\value{
A vector of probabilities of achieving every test score
}
\references{
Kolen MJ, Brennan RL (2004). Test Equating, Scaling, and Linking. Statistics in Social Science and Public Policy. Springer, New York.\cr
Lord F, Wingersky M (1984). Comparison of IRT true-score and equipercentile observed-score equatings. Applied Psychological Measurement, 8, 452-461.
}
\examples{
#This reproduces the example on page 183 of Kolen & Brennan (2004)
probs <- matrix(c(.74,.73,.82,.26,.27,.18),nrow=3,ncol=2, byrow = FALSE)
cats <- c(2,2,2)
wlord(probs,cats)
}
