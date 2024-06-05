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
library(ggplot2)
library(survival)
#library(gganimate)
#library(gifski)
#library(png)

CDF_values <- read_excel("~/AMICUS/Stukken/4_Simulation model/model/CDF_values.xlsx", 
                         +     sheet = "Sheet3", range = "H1:M17")

ggplot(CDF_values, aes(x=VDT, y=CP)) + geom_point() + xlab("Volume Doubling Time (days)") + ylab("Cumulative Probability") + geom_errorbar(aes(xmin=min, xmax=max), width=0.01) + xlim(0,800)
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
    linetype = "solid") + 
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
    linetype = "dotdash") +
  scale_color_manual(values = c("red", "blue", "green", "orange", "pink")) 

#p + transition_states(color)  
  
# Draw a single random sample from the normal distribution
random_sample <- rnorm(1, mean = mean_normal, sd = sd_normal)

# Draw 100 random samples from the normal distribution
random_samples <- rnorm(100, mean = mean_normal, sd = sd_normal)

# Plot a histogram of the random samples
hist(random_samples, main = "Histogram of Random Samples from Normal Distribution", xlab = "Volume Doubling Time (days)", ylab = "Frequency")

###### HAZARD RATE RECURRENCE AND DISTANT METASTASIS #########
rtruncnorm <- function(n, mu, sigma, low, high) {
  # find quantiles that correspond the the given low and high levels.
  p_low <- pnorm(low, mu, sigma)
  p_high <- pnorm(high, mu, sigma)
  
  # draw quantiles uniformly between the limits and pass these
  # to the relevant quantile function.
  qnorm(runif(n, p_low, p_high), mu, sigma)
}
# Define time intervals for analysis (e.g., within 1 year, within 5 years)
time_intervals <- c(12, 60)  # Months

#HORAN 2022
# Create a data frame with the provided information
horan <- data.frame(
  detection_mode = c(rep("Symptomatic", 65), rep("Routine", 75)),
  time_to_recurrence = c(rtruncnorm(65, 23, 9.2, low = 2, high = 58), rtruncnorm(75, 33, 13.2, low = 2, high = 60))
)
#sd
#A common approach to estimating the standard deviation from the range is to use a coefficient of variation (CV), which is the ratio of the standard deviation to the mean. 
#assuming a coefficient of variation typical for such data (which could vary depending on the specific context but is often around 0.3 to 0.5 for biological and medical data)
#cv = sd/mean
#sd = cv*mean = 0.4*23 = 9.2
#samples <- rtruncnorm(1000, 23, 9.2, low = 2, high = 60)
#hist(samples) looks cool

# Subset data for patients diagnosed symptomatically within 5 years
horan_subset <- subset(horan, detection_mode == "Symptomatic" & time_to_recurrence <= time_intervals[2])

# Fit a parametric survival model (e.g., exponential)
#fit_exponential <- survreg(Surv(time_to_recurrence) ~ 1, data = horan_subset, dist = "exponential")

# Fit parametric survival models (exponential, Weibull, log-normal)
h.fit.exp <- survreg(Surv(time_to_recurrence) ~ 1, data = horan_subset, dist = "exponential")
h.fit.wei <- survreg(Surv(time_to_recurrence) ~ 1, data = horan_subset, dist = "weibull")
h.fit.lognorm <- survreg(Surv(time_to_recurrence) ~ 1, data = horan_subset, dist = "lognormal")

# Plot Kaplan-Meier survival curve for observed data
h.surv.observed <- survfit(Surv(time_to_recurrence) ~ 1, data = horan_subset)
plot(h.surv.observed, col = "black", main = "Fitted parametric survival curves", xlab = "Time (Months)", ylab = "Symptomatic LRR Probability")
lines(h.surv.observed[1], lwd=1)
pct <- seq(.01,.99,by=.01)

lines(predict(h.fit.exp,type="quantile",p=pct)[1,],1-pct,col="red",lwd=1)
lines(predict(h.fit.wei,type="quantile",p=pct)[1,],1-pct,col="blue",lwd=1)
lines(predict(h.fit.lognorm,type="quantile",p=pct)[1,],1-pct,col="green",lwd=1)

legend("topright", legend = c("'Observed Data' Horan et al.", "Exponential", "Weibull", "Log-Normal"), col = c("black","red", "blue", "green"), lty = 1:4, bty = "n")

# Extract estimated parameter (hazard rate) from the exponential model
#hazard_rate_exponential <- summary(fit_exponential)$coef[1]
# Print the estimated hazard rate
#print(paste("Estimated hazard rate (Exponential model):", hazard_rate_exponential))

#EIJKELBOOM 2020
#Given the IQR, you can estimate the standard deviation (assuming normality) using the relationship between the standard deviation and the IQR. 
#For normally distributed data, the relationship between the standard deviation and the IQR is approximately:
#sd = IQR/1.349
#In a normal distribution, approximately 68% of the data falls within one standard deviation of the mean, and approximately 50% of the data falls within the IQR. 
#Since the IQR covers roughly half of the distribution and the standard deviation covers roughly two-thirds, their ratio can be estimated as:
#sd/IQR = 2/3*1/0.5 = 1.349
#DFI defined from moment of diagnosis = -3 months from surgery
#sd = (46-17)/1.349 = 21.49741
#mean = 2.6 years = 31 months - 3 = 28 months

eijkelboom <- data.frame(
  detection_mode = c(rep("Interval", 113), rep("Routine", 109)),
  time_to_recurrence = c(rtruncnorm(113, 28, 21.5, low = 0, high = 70), rtruncnorm(109, 32, 26.6, low = 0, high = 70))
)

# Subset data for patients diagnosed symptomatically within 5 years
eijkelboom_subset <- subset(eijkelboom, detection_mode == "Interval" & time_to_recurrence <= time_intervals[2])

# Fit parametric survival models (exponential, Weibull, log-normal)
e.fit.exp <- survreg(Surv(time_to_recurrence) ~ 1, data = eijkelboom_subset, dist = "exponential")
e.fit.wei <- survreg(Surv(time_to_recurrence) ~ 1, data = eijkelboom_subset, dist = "weibull")
e.fit.lognorm <- survreg(Surv(time_to_recurrence) ~ 1, data = eijkelboom_subset, dist = "lognormal")

# Plot Kaplan-Meier survival curve for observed data
e.surv.observed <- survfit(Surv(time_to_recurrence) ~ 1, data = eijkelboom_subset)
plot(e.surv.observed, col = "black", main = "Fitted parametric survival curves", xlab = "Time (Months)", ylab = "Symptomatic LRR Probability")
lines(e.surv.observed[1], lwd=1)
pct <- seq(.01,.99,by=.01)

lines(predict(e.fit.exp,type="quantile",p=pct)[1,],1-pct,col="red",lwd=1)
lines(predict(e.fit.wei,type="quantile",p=pct)[1,],1-pct,col="blue",lwd=1)
lines(predict(e.fit.lognorm,type="quantile",p=pct)[1,],1-pct,col="green",lwd=1)

legend("topright", legend = c("'Observed Data' Eijkelboom et al.", "Exponential", "Weibull", "Log-Normal"), col = c("black","red", "blue", "green"), lty = 1:4, bty = "n")

## COMBINED
he.combi <- data.frame(
  detection_mode = c(rep("Symptomatic", 113), rep("Symptomatic", 65)),
  time_to_recurrence = c(rtruncnorm(113, 28, 21.5, low = 0, high = 70), rtruncnorm(65, 23, 9.2, low = 2, high = 58))
)

# Subset data for patients diagnosed symptomatically within 5 years
combi.subset <- subset(he.combi, detection_mode == "Symptomatic" & time_to_recurrence <= time_intervals[2])

# Fit parametric survival models (exponential, Weibull, log-normal)
he.fit.exp <- survreg(Surv(time_to_recurrence) ~ 1, data = combi.subset, dist = "exponential")
he.fit.wei <- survreg(Surv(time_to_recurrence) ~ 1, data = combi.subset, dist = "weibull")
he.fit.lognorm <- survreg(Surv(time_to_recurrence) ~ 1, data = combi.subset, dist = "lognormal")

# Plot Kaplan-Meier survival curve for observed data
he.surv.observed <- survfit(Surv(time_to_recurrence) ~ 1, data = combi.subset)
plot(he.surv.observed, col = "black", main = "Fitted parametric survival curves", xlab = "Time (Months)", ylab = "Asymptomatic LRR Probability")
lines(he.surv.observed[1], lwd=1)
pct <- seq(.01,.99,by=.01)

#lines(predict(he.fit.exp,type="quantile",p=pct)[1,],1-pct,col="red",lwd=1)
lines(predict(he.fit.wei,type="quantile",p=pct)[1,],1-pct,col="blue",lwd=1)
#lines(predict(he.fit.lognorm,type="quantile",p=pct)[1,],1-pct,col="green",lwd=1)

legend("topright", legend = c("Combined data LRR", "Exponential", "Weibull", "Log-Normal"), col = c("black","red", "blue", "green"), lty = 1:4, bty = "n")

#WEIBULL appears to fit best
# Extract estimated parameter (hazard rate) from the weibull model
hazard.rate.weibull <- summary(he.fit.wei)$coef[1]
# Print the estimated hazard rate
print(paste("Estimated hazard rate (Weibull model, per month):", hazard.rate.weibull))
print(paste("Estimated hazard rate (Weibull model, per day):", (hazard.rate.weibull/30.44)))

##### CHAT WEIBULL
n_patients <- 200  # Number of patients in the model
random_times <- rweibull(n_patients, shape = he.fit.wei$scale, scale = exp(he.fit.wei$coefficients))

# Print the randomly generated times
print(random_times)

weibull <- data.frame(
  detection_mode = rep("Symptomatic", 200),
  time_to_recurrence =  rweibull(n_patients, shape = 1/(he.fit.wei$scale), scale = exp(he.fit.wei$coefficients))
)
surv.observed.w <- survfit(Surv(time_to_recurrence) ~ 1, data = weibull)
lines(surv.observed.w[1], lwd=1, col='pink')

####### DATA AE ##############
load("~/AMICUS/Stukken/4_Simulation model/model/des_model/preproces/data_AE.RData")

# Yes, with the information you provided, including the known distribution of V(t) and the dataset containing patients' recurrence data, disease-free interval,
#detection method, and tumor size at detection, you have sufficient information to proceed with the analysis. 
#You can use R to analyze the data and estimate the hazard function h(V(t)) of the time to symptomatic detection Tdet
# Load required packages
library(survival)
library(ggplot2)
library(ggfortify)
library(survminer)

library(dplyr)
df_patient <- df_patient %>% 
  mutate(symp = na_if(symp, 99))

#remove row twelve
df_patient <- df_patient[-c(12), ]

#if pt3 = 1 (7) <= 20, pt3=2 (5) 20-50, pt3=3 (1) > 50
df_patient$size <- ifelse(df_patient$pt3 == 1 & is.na(df_patient$size), round(runif(7, 0, 20)), 
                          ifelse(df_patient$pt3 == 2 & is.na(df_patient$size), round(runif(5, 21, 50)),
                                 ifelse(df_patient$pt3 == 3 & is.na(df_patient$size), round(runif(1, 51, 85)), df_patient$size)))  

# #Use Cox proportional hazards regression to model the relationship between tumor size at detection and the hazard of symptomatic recurrence.
# # Fit Cox proportional hazards model
# #cox_model <- coxph(Surv(dfi, symp) ~ rorilrr + size, data = df_patient)
# cox_model <- coxph(Surv(dfi, symp) ~ size, data = df_patient)
# 
# survival_curves <- survfit(cox_model)
# autoplot(survival_curves)
# 
# # Summarize the model
# summary(cox_model)
# 
# # Check proportional hazards assumption
# cox.zph(cox_model)
# 
# # Find minimum tumor size, ignoring NA values
# min_size <- min(df_patient$size, na.rm = TRUE)
# max_size <- max(df_patient$size, na.rm = TRUE)
# 
# # Create new data for prediction
# new_data <- data.frame(size = seq(min_size, max_size, length.out = 100))
# 
# # Predict hazard function
# predicted_hazard <- predict(cox_model, newdata = new_data, type = "risk")
# 
# # Plot the predicted hazard function
# plot(new_data$size, predicted_hazard, type = "l", xlab = "Tumor Size at Detection (mm)", ylab = "Hazard Function", main = "Hazard Function of Time to Symptomatic Detection")

############## baseline hazard
# Fit Cox proportional hazards model
cox_model <- coxph(Surv(dfi, symp) ~ size, data = df_patient)

# Extract baseline hazard function
baseline_hazard <- basehaz(cox_model)

# Plot the baseline hazard function
plot(baseline_hazard$time, baseline_hazard$hazard, type = "l", 
     xlab = "Time", ylab = "Hazard", main = "Baseline Hazard Function")

# Calculate cumulative hazard function
cumulative_hazard <- -log(1 - baseline_hazard$hazard)

# Calculate survival function
survival_function <- exp(-cumulative_hazard)

# Calculate probability of becoming symptomatic
prob_symptomatic <- 1 - survival_function

# Plot probability of becoming symptomatic by time
plot(baseline_hazard$time, prob_symptomatic, type = "l",
     xlab = "Time", ylab = "Probability of Becoming Symptomatic",
     main = "Probability of Becoming Symptomatic by Time")

# Define size intervals
size_intervals <- c(0, 15, 25, Inf)
interval_labels <- c("0-15", "15-25", "25+")

# Create a new column in the dataframe indicating the size interval for each tumor
df_patient$size_interval <- cut(df_patient$size, breaks = size_intervals, labels = interval_labels, right = FALSE)

# Plot probability of becoming symptomatic by time for each size interval
par(mfrow=c(1,1)) # Resetting plotting parameters
colors <- rainbow(length(interval_labels))

# Plotting
for (i in 1:length(interval_labels)) {
  subset_data <- df_patient[df_patient$size_interval == interval_labels[i], ]
  cox_model_subset <- coxph(Surv(dfi, symp) ~ 1, data = subset_data)
  baseline_hazard_subset <- basehaz(cox_model_subset)
  cumulative_hazard_subset <- -log(1 - baseline_hazard_subset$hazard)
  survival_function_subset <- exp(-cumulative_hazard_subset)
  prob_symptomatic_subset <- 1 - survival_function_subset
  
  # Plotting
  lines(baseline_hazard_subset$time, prob_symptomatic_subset, type = "l", col = colors[i], lwd = 2,
        xlab = "Time", ylab = "Probability of Becoming Symptomatic",
        main = "Probability of Becoming Symptomatic by Tumor Size Interval")
}

# Adding legend
legend("topright", legend = interval_labels, col = colors, lty = 1, lwd = 2, cex = 0.8)


#### 
sfit <- survfit(Surv(dfi, symp) ~ size_interval, data = df_patient)
ggsurvplot(sfit)

## add V_t
df_patient <- df_patient %>%
  mutate(V_t = ((4/3) * pi * (size/2)^3) / 1000)
#add VDT
df_patient <- df_patient %>%
  mutate(vdt = dfi * log(2) / log(V_t / V_0))
df_patient <- na.omit(df_patient)
save(df_patient,file="data_AEP.Rda")

## o determine the likelihood of a recurrence becoming symptomatic given a past time
# Fit Cox proportional hazards model
#cox_model <- coxph(Surv(dfi, symp) ~ vdt, data = df_patient)

# Define predictor values for a specific patient
e.vdt <- 100
e.dfi <- 360

# # Estimate survival probability at dfi for the specific patient
# survival_prob <- survfit(cox_model, newdata = data.frame(vdt = e.vdt, dfi = e.dfi))
# 
# # Find the row index corresponding to the closest time point to dfi
# closest_time_index <- which.min(abs(survival_prob$time - e.dfi))
# 
# # Extract survival probability at the closest time point to dfi
# survival_prob_at_dfi <- survival_prob$surv[closest_time_index]
# 
# # Calculate likelihood of symptomatic recurrence at dfi
# likelihood_symptomatic <- 1 - survival_prob_at_dfi
# 
# # Print likelihood
# print(likelihood_symptomatic)

##### instead of closest time index, use survival models to approximate ----
# Subset data for patients diagnosed symptomatically within 5 years
subset <- subset(df_patient, symp == 1 & dfi <= 2000)

# Fit Cox proportional hazards model
cox_model <- coxph(Surv(dfi, symp) ~ vdt, data = subset)

# Extract original survival probabilities
original_survival <- survfit(cox_model)

# Fit Weibull survival model
weibull_model <- survreg(Surv(dfi, symp) ~ vdt, data = subset, dist = "weibull")

# Fit Exponential survival model
exp_model <- survreg(Surv(dfi, symp) ~ vdt + dfi, data = subset, dist = "exponential")

# Fit Log-Normal survival model
lognormal_model <- survreg(Surv(dfi, symp) ~ vdt + dfi, data = subset, dist = "lognormal")

# Plot survival functions
plot(original_survival, col = "black", lty = 1, xlab = "Disease Free interval (days)", ylab = "Probability of remaining asymptomatic", conf.int = FALSE, xlim = c(0, 1900))
pct <- seq(.01,.99,by=.01)
# Add survival functions for parametric models
lines(predict(weibull_model, type = "quantile",p=pct)[1,],1-pct, col = "blue", lty = 2)
lines(predict(exp_model, type = "quantile",p=pct)[1,],1-pct, col = "red", lty = 3)
lines(predict(lognormal_model, type = "quantile",p=pct)[1,],1-pct, col = "green", lty = 4)

# Add legend
legend("topright", legend = c("Original Data", "Weibull", "Exponential", "Log-Normal"), col = c("black", "blue", "red", "green"), lty = 1:4)

# Compare model fits
aic_values <- AIC(weibull_model, exp_model, lognormal_model)
best_model <- names(aic_values)[which.min(aic_values)]

# Estimate survival probability at dfi using the best-fitting model
survival_prob <- predict(weibull_model, type = "response", newdata = data.frame(vdt = 30, dfi = 1100))

# Calculate likelihood of symptomatic recurrence at dfi
likelihood_symptomatic <- 1 - pweibull(survival_prob, shape = 1/(weibull_model$scale), scale = exp(weibull_model$coefficients))

# Print likelihood
print(likelihood_symptomatic*100)




###
#https://cran.r-project.org/web/packages/contsurvplot/vignettes/introduction.html
#install.packages("contsurvplot")
library(contsurvplot)
library(ggplot2)
library(dplyr)
library(rlang)
library(riskRegression)
library(survival)
library(pammtools) #not installed
library(gganimate)
library(transformr)
library(plotly) #not installed
library(reshape2) 
library(knitr)
library(rmarkdown)

model <- coxph(Surv(time = dfi, event = symp) ~ vdt, data=df_patient, x=T)
curve_cont(data=df_patient, variable="vdt", model=model,
           horizon=c(10, 50, 120), times=c(500, 1500))
#horizon: A numeric vector containing a range of values of variable for which the survival curves should be calculated
#times: A numeric vector containing points in time at which the survival probabilities should be calculated


plot_surv_at_t(time="dfi",
               status="symp",
               variable="vdt",
               data=subset,
               model=model,
               t=850)

plot_surv_at_t(time="dfi",
               status="symp",
               variable="vdt",
               data=df_patient,
               model=model,
               t=c(400, 700, 900, 1200, 1500))

plot_surv_quantiles(time="dfi",
                    status="symp",
                    variable="vdt",
                    data=df_patient,
                    model=model,
                    p=0.5)

plot_surv_quantiles(time="dfi",
                    status="symp",
                    variable="vdt",
                    data=df_patient,
                    model=model,
                    p=c(0.1, 0.25, 0.5, 0.75, 0.9))

#single curves different vdt
plot_surv_lines(time="dfi",
                    status="symp",
                    variable="vdt",
                    data=df_patient,
                    model=model,
                    horizon=c(50))

#survival area
plot_surv_area(time="dfi",
                status="symp",
                variable="vdt",
                data=df_patient,
                model=model)
#survival area discrete
plot_surv_area(time="dfi",
               status="symp",
               variable="vdt",
               data=df_patient,
               model=model,
               discrete=T,
               bins=5,
               start_color = "lightgrey",
               end_color="black")
#survival heatmap
plot_surv_heatmap(time="dfi",
               status="symp",
               variable="vdt",
               data=df_patient,
               model=model,
               start_color = "blue",
               end_color="red")
#survival heatmap with contour lines
plot_surv_heatmap(time="dfi",
                  status="symp",
                  variable="vdt",
                  data=df_patient,
                  model=model,
                  contour_lines = T,
                  start_color = "blue",
                  end_color="red")
#survival contours
plot_surv_contour(time="dfi",
                  status="symp",
                  variable="vdt",
                  data=df_patient,
                  model=model,
                  bins=5)
#survival Matrix
plot_surv_matrix(time="dfi",
                  status="symp",
                  variable="vdt",
                  data=df_patient,
                  model=model)

#restricted mean survival time
plot_surv_rmst(time="dfi",
               status="symp",
               variable="vdt",
               data=df_patient,
               model=model,
               tau=c(1100))


#### Not based on VDT but size a diagnosis
model2 <- coxph(Surv(time = dfi, event = symp) ~ size, data=subset, x=T)

#restricted mean survival time
plot_surv_rmst(time="dfi",
               status="symp",
               variable="size",
               data=subset,
               model=model2,
               tau=c(365, 730, 1095, 1460, 1825))

##
symp.prob <- function(vdt,dfi){
  p <- as.numeric(curve_cont(data=df_patient, variable="vdt", model=model, horizon=c(vdt), times=c(dfi))[2])
  return(p)
}


a <- plot_surv_3Dsurface(time="dfi",
                    status="symp",
                    variable="vdt",
                    data=df_patient,
                    model=model,
                    interactive=T)

#survival heatmap with contour lines
p <- plot_surv_heatmap(time="dfi",
                  status="symp",
                  variable="vdt",
                  data=df_patient,
                  model=model,
                  contour_lines = T,
                  start_color = "blue",
                  end_color="red")
p + ggtitle("Survival Heatmap") + labs(y = "Volume Doubling Time (days)", x = "Disease Free Interval (days)")













