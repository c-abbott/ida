library(maxLik)
# Loading in data
load('dataex2.Rdata')
# Log likelihood function set to maximized
get_log_likelihood = function(param, data) {
  mu = param
  x = data[,1]; r = data[,2]
  return(sum(r*dnorm(x, mean=mu, sd=1.5, log=TRUE) + 
               (1 - r)*pnorm(x, mean=mu, sd=1.5, log.p=TRUE)))
}
# Get MLE
mle = maxLik(logLik = get_log_likelihood, data = dataex2, start = c(mu=1))
# Present results
summary(mle)