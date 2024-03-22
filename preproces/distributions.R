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

#VOLUME DOUBLING TIME
library(fitdistrplus)
CDF_values <- read_excel("~/AMICUS/Stukken/4_Simulation model/model/CDF_values.xlsx", 
                         +     sheet = "Sheet3", range = "H1:M17")

ggplot(CDF_values, aes(x=VDT, y=CP)) + geom_point() + xlab("Volume Doubling Time (days)") + ylab("Cumulative Probability") + geom_errorbar(aes(xmin=min, xmax=max), width=0.01) #+ xlim(0,800)
#+ geom_linerange(aes(xmin=min, xmax=max), colour="blue")

# Choose candidate distributions
candidate_distributions <- c("norm", "lnorm", "exp", "weibull")

# Fit distributions
fit_results <- lapply(candidate_distributions, function(dist) {
  fitdist(CDF_values$VDT, dist)
})

descdist(CDF_values$VDT, discrete = FALSE, boot = 1000) 

M_fit_w <- fitdist(CDF_values$VDT, "weibull")
M_fit_g <- fitdist(CDF_values$VDT, "gamma")
M_fit_ln <- fitdist(CDF_values$VDT, "lnorm")
M_fit_exp <- fitdist(CDF_values$VDT, "exp")  
M_fit_norm <- fitdist(CDF_values$VDT, "norm")

par(mfrow = c(1, 1))
plot.legend <- c("Weibull", "lognormal", "gamma", "exp", "norm")
#denscomp(list(M_fit_w, M_fit_ln, M_fit_g, M_fit_exp, M_fit_norm ), legendtext = plot.legend)
#qqcomp(list(M_fit_w, M_fit_ln, M_fit_g, M_fit_exp, M_fit_norm ), legendtext = plot.legend)
cdfcomp(list(M_fit_w, M_fit_ln, M_fit_g, M_fit_exp, M_fit_norm ), legendtext = plot.legend)
#ppcomp(list(M_fit_w, M_fit_ln, M_fit_g, M_fit_exp, M_fit_norm ), legendtext = plot.legend)

#weibull
wei_shape <- M_fit_w$estimate[1]
wei_scale <- M_fit_w$estimate[2]

#norm
norm_mean <- M_fit_norm$estimate[1]
norm_sd <- M_fit_norm$estimate[2]

ggplot(CDF_values, aes(x=VDT, y=CP)) + geom_point() + xlab("Volume Doubling Time (days)") + ylab("Cumulative Probability") + geom_errorbar(aes(xmin=min, xmax=max), width=0.01) +xlim(0,800)+
  stat_function(
    fun = function(x) pweibull(x, shape = wei_shape, scale = wei_scale),
    aes(color = "Weibull"),
    linetype = "dashed") + 
  stat_function(
    fun = function(x) pnorm(x, mean = norm_mean, sd = norm_sd),
    aes(color = "Normal"),
    linetype = "twodash") + 
  stat_function(
    fun = function(x) pexp(x, rate = M_fit_exp$estimate[1]),
    aes(color = "Exponential"),
    linetype = "solid") +
  stat_function(
    fun = function(x) pgamma(x, shape = M_fit_g$estimate[1], scale = 1/M_fit_g$estimate[2]),
    aes(color = "Gamma"),
    linetype = "dotted") +
  stat_function(
    fun = function(x) plnorm(x, meanlog = M_fit_ln$estimate[1], sdlog = M_fit_ln$estimate[2]),
    aes(color = "Lognormal"),
    linetype = "dotdash") 
  scale_color_manual(values = c("blue", "red", "green", "orange", "pink")) 
  
  
# Draw a single random sample from the normal distribution
random_sample <- rnorm(1, mean = mean_normal, sd = sd_normal)

# Draw 100 random samples from the normal distribution
random_samples <- rnorm(100, mean = mean_normal, sd = sd_normal)

# Plot a histogram of the random samples
hist(random_samples, main = "Histogram of Random Samples from Normal Distribution", xlab = "Volume Doubling Time (days)", ylab = "Frequency")


