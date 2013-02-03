\name{classify-package}
\alias{classify-package}
\docType{package}
\title{
Classification Accuracy and Consistency under IRT models.
}
\description{
IRT classification uses the probability that candidates of a given ability, will answer correctly questions of a specified difficulty to calculate the probability of their achieving every possible score in a test. Due to the IRT assumption of conditional independence (that is every answer given is assumed to depend only on the latent trait being measured) the probability of candidates achieving these potential scores can be expressed by multiplication of probabilities for item responses for a given ability. Once the true score and the probabilities of achieving all other scores have been determined for a candidate the probability of their score lying in the same category as that of their true score (classification accuracy), or the probability of consistent classification in a category over administrations (classification consistency), can be calculated.
}
\details{
\tabular{ll}{
Package: \tab classify\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2012-04-30\cr
License: \tab GPL (>= 2)\cr
Depends: \tab Rcpp (>= 0.9.10), plyr, ggplot2, R2WinBUGS, lattice, reshape2,
methods, R2jags\cr
LinkingTo: \tab Rcpp\cr
Packaged: \tab 2012-06-08 12:43:25 UTC; cwheadon\cr
Built: \tab R 2.15.0; i386-pc-mingw32; 2012-06-08 12:43:33 UTC; windows\cr
}

Index:
\preformatted{
beta.list               Extracts Beta Values from Bugs Sims File
biology                 Polytomous Responses from 200 Candidates to 31
                        Questions
classification-class    Class '"classification"'
classify                Calculate Classification Statistics
classify-package        Classification Accuracy and Consistency under
                        IRT Models
classify.bug            Classification Accuracy and Consistency from
                        Bugs Replicate Parameters
gpcm                    Generalised Partial Credit Model Derived
                        Probabilities
gpcm.bug                Extracts IRT Model Parameters from Bugs Models
gpcm.rc                 IRT Derived Predicted Conditional Number
                        Correct Score Distribution
pcm                     Partial Credit Model Derived Probabilities
physics                 Dichotomous Responses from 200 Candidates to 25
                        Questions
plot.scores             Plot Methods for Classification and Scores
                        Objects
rasch                   Rasch Derived Probabilities
scores-class            Class '"scores"'
scores.gpcm.bug         Expected and Conditional Summed Score
                        Distributions
summary-methods         Summary Stats for Classification Accuracy and
                        Consistency
thpl                    Three Parameter IRT Model Derived Probabilities
tpl                     Two Parameter IRT model Derived Probabilities
wlord                   Lord and Wingersky Recursion Formula
}
}
\author{
Dr Chris Wheadon and Dr Ian Stockford

Maintainer: Dr Chris Wheadon <chris.wheadon@gmail.com>
}
\references{
Curtis, S.M.(2010) BUGS Code for Item Response Theory, \emph{Journal of Statistical Software}, Code Snippets, \bold{36(1)},1--34.

Hanson, B.A., Beguin, A.A.(2002) Obtaining a common scale for item response theory item parameters using separate versus concurrent estimation in the common-item equating design. \emph{Applied Psychological Measurement}, \bold{26}, 3--24.

Kolen M.J., Brennan, R.L. (2004) \emph{Test Equating, Scaling, and Linking. Statistics in Social Science and Public Policy}.

Lee, W. (2008) Classification consistency and accuracy for complex assessments using item response theory, (No. 27) \emph{CASMA Research Report} Iowa City, IA: Center for Advanced Studies in Measurement and Assessment, University of Iowa.

Lord, F., Wingersky, M. (1984) Comparison of IRT true-score and equipercentile observed-score equatings, \emph{Applied Psychological Measurement}, \bold{8}, 452--461.

Lunn, D.J., Thomas, A., Best, N., Spiegelhalter, D. (2000) WinBUGS, A Bayesian modelling framework: concepts, structure, and extensibility. \emph{Statistics and Computing},
\bold{10}, 325--337.

Plummer, M (2012) Just Another Gibbs Sampler, version 3.2.0 http://mcmcjags.sourceforge.net/

Swaminathan, H., Hambleton, R. K.,  Rogers, H. J. (2007) Assessing the fit of Item Response Theory models. In C. R. Rao, S. Sinharay (Eds.), \emph{Handbook of statistics, Vol. 26}, 683--718.

Wheadon, C., Stockford, I., (2011) Classification accuracy and consistency in GCSE and A level examinations offered by the Assessment and Qualifications Alliance (AQA) November 2008 to June 2009, \emph{Ofqual's Reliability Compendium}. Office of Qualification and Examinations Regulation.
}
\keyword{ package }
\keyword{ irt }
\keyword{ winbugs }
\keyword{ bugs }
\keyword{ jags }
\keyword{ classification }
\keyword{ reliability }
\examples{
\dontrun{

## Rasch or 2pl Model

# Data should be a numeric matrix, with one row per candidate, one column per item
data(physics)
# If reading from csv, the following is recommended:
# physics<-read.csv("physics.csv", header=TRUE, sep=",", na.strings = "-")
# physics<-physics[complete.cases(physics),]
# physics<-physics[sample(1:nrow(physics), 200, replace=FALSE),]
# physics<-as.matrix(physics)

n <- nrow(physics)
p <- ncol(physics)

# Boundary marks in ascending order
bnds <- c(9,11,13,15,18,21)

# Labels for boundaries (one more boundary than label)
lbls <- c("U","E","D","C","B","A","A*")

# Specify bugs file - included in the classify/bugs directory
# 2 pl model
mdl <- "tpl.bug"
# Rasch model
# mdl <- "rasch.bug"

# Item marks
Y <- physics

# Mean and standard deviation of delta
m.delta <- 0.0
s.delta <- 1.0

# Mean and standard deviation of alpha, comment out for Rasch model
m.alpha <- 0.0
s.alpha <- 1.0

# Data set for the 2 pl model
data <- list("Y", "n", "p", "m.delta", "s.delta", "m.alpha", "s.alpha")

# Parameters to monitor for the 2 pl model
monitor <- c("delta", "theta", "alpha")

# Rasch model
# data <- list("Y", "n", "p", "m.delta", "s.delta")
# monitor <- c("delta", "theta")

# Set to location of bug file
jags.file <- file.path(getwd(), "R/R-2.15.0/library/classify/bugs" ,mdl)

# JAGS
# may require set.seed(1234) depending on version of R
system.time(jagsout <- jags(data=data, inits=NULL, parameters.to.save=monitor,
                            model.file=jags.file,
                            n.iter=2000, n.thin=10, n.burnin=1000))

sims <- jagsout$BUGSoutput$sims.matrix

# Bugs
# Change this to your bugs directory
# bugs.directory = "C:/Program Files/WinBUGS14"
# system.time(bugsout <- bugs(data=data, inits=NULL, parameters.to.save=monitor,
#                            model.file=jags.file,
#                            n.iter=2000, n.thin=10, n.burnin=1000))

# sims <- bugsout$sims.matrix

# Estimate conditional score and expected score distributions
scores <- scores.gpcm.bug(Y,sims,mdl)
# Plots 
plot(scores)
# Save plot
# ggsave("expected.pdf")
plot(scores,type="cond")
plot(scores,type="qq",alpha=0.5)

# Estimate accuracy statistics
accs <- classify.bug(sims,scores,bnds,lbls)
summary(accs)
plot(accs)
plot(accs,type="kappa")
plot(accs,type="density")

##############################################
##############################################

## PCM or GPCM models
# Data should be a numeric matrix, with one row per candidate, one column per item
data(biology)
# If reading from csv, the following is recommended:
# biology<-read.csv("biology.csv", header=TRUE, sep=",", na.strings = "-")
# biology<-biology[complete.cases(biology),]
# biology<-biology[sample(1:nrow(biology), 200, replace=FALSE),]
# biology<-as.matrix(biology)

n <- nrow(biology)
p <- ncol(biology)

# Boundary marks in ascending order
bnds <- c(26,30,35,40,45)

# Labels for boundaries (one more boundary than label)
lbls <- c("U","E","D","C","B","A")

# Specify bugs file - included in the classify/bugs directory
# GPCM
mdl <- "gpcm.bug"
# PCM
#mdl <- "pcm.bug"

# Bugs polytomous models require polytomous scores as categories
Y <-  biology + 1
# Specify response categories
K <- as.numeric(apply(Y,2,max,na.rm = TRUE))

# Mean and standard deviation of alpha and beta parameters

m.beta <- 0.0
s.beta <- 1.0

# Comment out for PCM
m.alpha <- 0.0
s.alpha <- 1.0

# GPCM
data <- list("Y", "n", "p", "K",
		"m.beta", "s.beta", "m.alpha", "s.alpha")
monitor <- c("beta", "theta", "alpha")

# PCM
#data <- list("Y", "n", "p", "K",
#		"m.beta", "s.beta")
#monitor <- c("beta", "theta")

# Initial values for beta set to 0, matrix padded with NA
beta <- t(sapply(1:p, function(j) c(rep(NA, K[j]), rep(0.0, max(K) - K[j]))))
data <- c(data, "beta")

# Change this to bugs file directory
jags.file <- file.path(getwd(), "R/R-2.15.0/library/classify/bugs" ,mdl)

# Simulations and sampling
iter <- 2000
burnin <- 1000
thin <- 10

## JAGS
estimation <- "jags"
# may require set.seed(1234) depending on version of R
system.time(jagsout <- jags(data=data, inits=NULL, parameters.to.save=monitor,
                            model.file=jags.file,
                            n.iter=iter, n.thin=thin, n.burnin=burnin))
sims <- jagsout$BUGSoutput$sims.matrix


## Bugs
#estimation <- "bugs"
# Change this to your bugs directory
#bugs.directory = "C:/Program Files/WinBUGS14"
#system.time(bugsout <- jags(data=data, inits=NULL, parameters.to.save=monitor,
#                            model.file=jags.file,
#                            n.iter=iter, n.thin=thin, n.burnin=burnin))
#sims <- bugsout$sims.matrix

# Estimate conditional score and expected score distributions
scores <- scores.gpcm.bug(biology,sims,mdl)
# Plots 
plot(scores)
# Save plot
#ggsave("expected.pdf")
plot(scores,type="cond")
plot(scores,type="qq",alpha=0.5)

# Estimate accuracy statistics
accs <- classify.bug(sims,scores,bnds,lbls)
summary(accs)
plot(accs)
plot(accs,type="kappa")
plot(accs,type="density")

}
}
