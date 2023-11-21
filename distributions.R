# Load packages
library(survival);  # Fit Survival Models
library(EnvStats);  # Q-Q Plot for censored data

#time = time between start follow-up and first event
#event = follow-up moment

# Distributions considered
dist.considered <- c("exponential", 
                     "weibull", 
                     "logistic", 
                     "lognormal");

# Data for first-line
df.RWD <- df;

# Select distribution
dist.performance <- sapply(dist.considered, function(dist) {
  
  # Fit survival model for distribution
  model <- survreg(formula=Surv(time=, event=) ~ 1, data=df.RWD, dist=dist);
  
  # Return the likelihood
  return(model$loglik[1]);
  
});

# Print the results and which distribution would be selected based on the likelihood
dist.selected <- dist.considered[which.max(dist.performance)];
print(dist.performance);

# Plot the survival curves
for(i in 1:length(dist.considered)) {
  
  # Get distribution type
  dist <- dist.considered[i];
  
  # Fit survival model for distribution
  model <- survreg(formula=Surv(time=, event=) ~ 1, data=df.RWD, dist=dist);
  
  # If it is the first distribution considered, plot kaplan-meier
  if(i==1) plot(survfit(Surv(time=, event=) ~ 1, data=df.RWD), conf.int=F, xlab="Time to follow-up event (days)", lwd=2);
  
  # Plot fitted distribution
  lines(y=seq(.99, .01, by=-.01), x=predict(model, type="quantile", p=seq(.01, .99, by=.01))[1,], col=rainbow(length(dist.considered))[i], lwd=2)
  
  # Add the legend if it is the last distribution
  if(i==length(dist.considered)) legend("right", legend=dist.considered, col=rainbow(length(dist.considered)), lty=1, lwd=2);
  
} 

# Plot the Q-Q Plots curves
par(mfrow=c(2,2));
for(i in 1:length(dist.considered)) {
  
  # Get distribution type
  dist <- dist.considered[i];
  
  # Fit survival model for distribution
  model <- survreg(formula=Surv(time=, event=) ~ 1, data=df.RWD, dist=dist);
  
  # Plot the Q-Q Plot
  if(dist=="exponential") qqPlotCensored(x=df.RWD$ , censored=df.RWD$ ==0, censoring.side="right", 
                                         equal.axes=T, main=paste0("Q-Q Plot for the ", dist, " distribution"),
                                         distribution="exp", param.list=list(rate=1/exp(model$coefficients)));
  if(dist=="weibull") qqPlotCensored(x=df.RWD$ , censored=df.RWD$ ==0, censoring.side="right", 
                                     equal.axes=T, main=paste0("Q-Q Plot for the ", dist, " distribution"), 
                                     distribution="weibull", param.list=list(shape=1/model$scale, scale=exp(model$coefficients)));
  if(dist=="logistic") qqPlotCensored(x=df.RWD$ , censored=df.RWD$ ==0, censoring.side="right", 
                                      equal.axes=T, main=paste0("Q-Q Plot for the ", dist, " distribution"), 
                                      distribution="logis", param.list=list(location=model$coefficients, scale=model$scale));
  if(dist=="lognormal") qqPlotCensored(x=df.RWD$ , censored=df.RWD$ ==0, censoring.side="right", 
                                       equal.axes=T, main=paste0("Q-Q Plot for the ", dist, " distribution"), 
                                       distribution="lnorm", param.list=list(meanlog=model$coefficients, sdlog=model$scale));
}