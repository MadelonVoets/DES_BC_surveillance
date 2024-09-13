# life_table_long <- tidyr::pivot_longer(oligo, cols = c(surv.prob.oligo, surv.prob.dm),
#                                        names_to = "group", values_to = "survival_prob")
# 
# # Plot the survival curves
# ggplot(life_table_long, aes(x = time, y = survival_prob, color = group)) +
#   geom_line(size = 1) +
#   labs(x = "Time (years)", y = "Survival Probability", 
#        title = "Survival Curves", color = "Group") +
#   theme_minimal()

################################################
# OLIGO (<3 DM)
################################################
oligo <- read_excel("oligo.xlsx", sheet = "Sheet4")

surv.observed.o <- survfit(Surv(time_to_death, event) ~ 1, data = oligo)
fit.wei.o <- survreg(Surv(time_to_death, event) ~ 1, data = oligo, dist = "weibull")
fit.exp <- survreg(Surv(time_to_death, event) ~ 1, data = oligo, dist = "exponential")
fit.log <- survreg(Surv(time_to_death, event) ~ 1, data = oligo, dist = "lognormal")
fit.norm <- survreg(Surv(time_to_death, event) ~ 1, data = oligo, dist = "gaussian")

plot(surv.observed.o, col = "black", main = "Fitted parametric survival curve", xlab = "Time (days)", ylab = "Survival probability oligo DM")
lines(surv.observed[1], lwd=1)
pct <- seq(.01,.99,by=.01)

lines(predict(fit.wei,type="quantile",p=pct)[1,],1-pct,col="blue",lwd=1)
lines(predict(fit.exp,type="quantile",p=pct)[1,],1-pct,col="red",lwd=1)
lines(predict(fit.log,type="quantile",p=pct)[1,],1-pct,col="green",lwd=1)
lines(predict(fit.norm,type="quantile",p=pct)[1,],1-pct,col="purple",lwd=1)
legend("topright", legend = c("OLIGO-3", "Weibull", "Exponential", "Log-Normal", "Normal"), col = c("black","blue", "red", "green", "purple"), lty = 1:5, bty = "n")
#best fit
aic_values <- AIC(fit.wei, fit.exp, fit.log, fit.norm)

##### CHAT WEIBULL
n_patients <- 200  # Number of patients in the model
random_times <- rweibull(n_patients, shape = fit.wei.o$scale, scale = exp(fit.wei.o$coefficients))
#fit.wei$scale = 0.9248594
#fit.wei$coefficients = 7.434267 

# Print the randomly generated times
print(random_times)

weibull.0 <- data.frame(
  detection_mode = rep("event", 200),
  time_to_death =  rweibull(n_patients, shape = 1/(fit.wei.o$scale), scale = exp(fit.wei.o$coefficients))
)
surv.observed.w <- survfit(Surv(time_to_death) ~ 1, data = weibull.o)
lines(surv.observed.w[1], lwd=1, col='pink')

################################################
# DM
################################################

dm <- read_excel("oligo.xlsx", sheet = "Sheet5")

surv.observed.dm <- survfit(Surv(time_to_death, event) ~ 1, data = dm)
fit.wei <- survreg(Surv(time_to_death, event) ~ 1, data = dm, dist = "weibull")
fit.exp <- survreg(Surv(time_to_death, event) ~ 1, data = dm, dist = "exponential")
fit.log <- survreg(Surv(time_to_death, event) ~ 1, data = dm, dist = "lognormal")
fit.norm <- survreg(Surv(time_to_death, event) ~ 1, data = dm, dist = "gaussian")

plot(surv.observed.dm, col = "black", main = "Fitted parametric survival curves", xlab = "Time (days)", ylab = "Survival probability >3 DM")
lines(surv.observed[1], lwd=1)
pct <- seq(.01,.99,by=.01)

lines(predict(fit.wei,type="quantile",p=pct)[1,],1-pct,col="blue",lwd=1)
lines(predict(fit.exp,type="quantile",p=pct)[1,],1-pct,col="red",lwd=1)
lines(predict(fit.log,type="quantile",p=pct)[1,],1-pct,col="green",lwd=1)
lines(predict(fit.norm,type="quantile",p=pct)[1,],1-pct,col="purple",lwd=1)
legend("topright", legend = c("OLIGO-3", "Weibull", "Exponential", "Log-Normal", "Normal"), col = c("black","blue", "red", "green", "purple"), lty = 1:5, bty = "n")
#best fit
aic_values <- AIC(fit.wei, fit.exp, fit.log, fit.norm)

##### CHAT WEIBULL
n_patients <- 200  # Number of patients in the model
random_times <- rweibull(n_patients, shape = fit.wei$scale, scale = exp(fit.wei$coefficients))
#fit.wei$scale = 0.8820885
#fit.wei$coefficients = 7.089314  

weibull.dm <- data.frame(
  detection_mode = rep("event", 200),
  time_to_death =  rweibull(n_patients, shape = 1/(fit.wei$scale), scale = exp(fit.wei$coefficients))
)
surv.observed.w.dm <- survfit(Surv(time_to_death) ~ 1, data = weibull.dm)
lines(surv.observed.w.dm[1], lwd=1, col='pink')

# Plot together
plot(surv.observed.w, col = "blue", main = "Fitted parametric survival curves", xlab = "Time since diagnosis (days)", ylab = "Survival probability", conf.int = F)
lines(surv.observed.w[1], lwd=2, col = "blue", conf.int = F)
lines(surv.observed.o[1], lwd=1, col='black', conf.int = F, linetype="dashed")
lines(surv.observed.w.dm[1], lwd=2, col='red', conf.int = F)
lines(surv.observed.dm[1], lwd=1, col='black', conf.int = F)
legend("topright", legend = c("Oligo metastatic", "Observed", ">3 metastases"), col = c("blue", "Black", "red"), lty = 1:2, bty = "n")

