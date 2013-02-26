require(devtools)
load_all("classify")

## Not run:
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
jags.file <- file.path(getwd(), "classify/inst/bugs" ,mdl)
# JAGS
# may require set.seed(1234) depending on version of R
system.time(jagsout <- jags(data=data, inits=NULL, parameters.to.save=monitor,
model.file=jags.file,
n.iter=2000, n.thin=10, n.burnin=1000))
sims <- jagsout$BUGSoutput$sims.matrix


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

