## 6. MODEL CALIBRATION ----
library(ggstatsplot)
library(tidyr)
library(ggplot2)

#Test population
test <- fn_gen_pt(100000)
#save(test, file= "test.Rda")
#load("test.Rda")
sub.test <- subset(test, select = -c(ID, age, age.grp, t.DM, t.LRR, vdt_lrr, vdt_dm)) 
test.list <- as.list(sub.test)

cali.test <- list(
  c(nkr_variables[1,"p.gr1"], nkr_variables[1,"p.gr2"], nkr_variables[1,'p.gr3']), # grade
  c(nkr_variables[1,"p.st1"], nkr_variables[1,"p.st2"], nkr_variables[1,'p.st3']), # stage
  c(nkr_variables[1,"p.n0"], nkr_variables[1,"p.n1"], nkr_variables[1,"p.n2"], nkr_variables[1,'p.n3']), # nstatus
  c(nkr_variables[1,"p.multi.n"], nkr_variables[1,"p.multi.y"]), #multifocality
  c(nkr_variables[1,"p.bcs"], nkr_variables[1,"p.mst"]), #surgery
  c(nkr_variables[1,"p.chemo.n"], nkr_variables[1,"p.chemo.y"]), #chemotherapy
  c(nkr_variables[1,"p.rt.n"], nkr_variables[1,"p.rt.y"]), #radiotherapy
  c(nkr_variables[1,"p.hr.n"], nkr_variables[1,"p.hr.y.ther.n"], nkr_variables[1,'p.hr.y.ther.y']), #hormonal therapy
  c(nkr_variables[1,"p.her2.n"], nkr_variables[1,"p.her2.y.ther.n"], nkr_variables[1,'p.her2.y.ther.y']) #targeted therapy
)
  
#histogram for age
hist(test$age)
#grade
nkr_variables[5:7]
prop.table(table(test$grade))
#stage
nkr_variables[23:25]
prop.table(table(test$stage))
#nstatus
nkr_variables[17:20]
prop.table(table(test$nstatus))
#multifocality
nkr_variables[15:16]
prop.table(table(test$multif))
#surgery
nkr_variables[c(2,14)]
prop.table(table(test$sur))
#chemotherapy
nkr_variables[3:4]
prop.table(table(test$chemo))
#radiotherapy
nkr_variables[21:22]
prop.table(table(test$radio))
#hormonal therapy
nkr_variables[11:13]
prop.table(table(test$horm))
#targeted therapy
nkr_variables[8:10]
prop.table(table(test$antiher2))

#compare nkr variables (rwe) and test patients to assess differences
check_calibration <- function(test_data_list, calibration_probs_list) {
  # Iterate over each parameter
  for (i in seq_along(test_data_list)) {
    test <- test_data_list[[i]]
    calibration_probs <- calibration_probs_list[[i]]
    
    # Calculate the proportions in the test population
    n <- length(test)
    test_proportions <- sapply(sort(unique(test)), function(x) sum(test == x) / n)
    
    # Check if the proportions are within 10% of the calibration parameters
    check_results <- mapply(function(test_prop, cal_prob) {
      abs(test_prop - cal_prob) <= 0.02 #* cal_prob
    }, test_proportions, calibration_probs)
    
    # Print the appropriate message based on the checks
    if (all(check_results)) {
      print(paste("Parameter", i, ": Distributions within 2%"))
    } else {
      for (j in seq_along(check_results)) {
        if (!check_results[j]) {
          print(paste("Parameter", i, ": Proportion of value", j, "is off"))
        }
      }
    }
  }
}

# Run the check
check_calibration(test.list, cali.test)

## RISK OF RECURRENCES
risk.list <- apply(test, 1, function(row) {
  vector <- c(row["age.grp"], row["grade"], row["stage"], row["nstatus"], row["multif"], row["sur"], row["chemo"], row["radio"], row["horm"], row["antiher2"])
  risk <- fn_risk(vector, inf_matrix, rec=1)
  return(risk)
  
})


risks <- as.data.frame(do.call(rbind,risk.list))
#risk.index <- rownames(risks)
risks.bef$con_1 <- risks.bef$c_lrr1
risks.bef$con_2 <- risks.bef$c_lrr2 - risks.bef$c_lrr1
risks.bef$con_3 <- risks.bef$c_lrr3 - risks.bef$c_lrr2
risks.bef$con_4 <- risks.bef$c_lrr4 - risks.bef$c_lrr3
risks.bef$con_5 <- risks.bef$c_lrr5 - risks.bef$c_lrr4

#conditional
plot_df <- pivot_longer(risks, cols = 6:10)
plot_df$name <- factor(plot_df$name)
plot_df$values <- plot_df$value*100

##ggstatsplot fancy violin plot
plt <- ggbetweenstats(
  data = plot_df,
  x = name,
  y = values,
  pairwise.comparisons = FALSE,
  pairwise.display = "none",
  bf.message = FALSE,
  results.subtitle = FALSE
)

plt <- plt +
  #add labels and titles
  labs(
    x = "Year",
    y = "Conditional Annual Risk Calibration(%)"
  ) + 
  #customizations
  theme( 
    text = element_text(family = "Roboto", size = 8, color = "black"),
    plot.title = element_text(
      family = "Lobster Two",
      size = 20,
      face = "bold",
      color = "#2a475e"
    ),
    #statistical annotations below the main title
    plot.subtitle = element_text(
      family = "Lobster Two",
      size = 15,
      face = "bold",
      color = "#1b2838"
    ),
    plot.title.position = "plot",
    axis.text = element_text(size =10, color="black"),
    axis.title = element_text(size = 12)
  )

plt <- plt +
  theme(
    axis.ticks = element_blank(),
    axis.line = element_line(colour = "grey50"),
    panel.grid = element_line(color = "#b4aea9"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(linetype = "dashed")
  )

plt 

inf_ref <- data.frame(
  Year = factor(1:5),
  Mean = c(0.26, 1.05, 0.51, 0.40, 0.33),        # Replace these with your actual mean values
  LowerIQR = c(0.11, 0.7, 0.35, 0.2, 0.23),      # Replace these with your actual lower IQR values
  UpperIQR = c(0.55, 1.8, 0.8, 0.6, 0.48)     # Replace these with your actual upper IQR values
)
#ggplot(inf_ref, aes(x = Year, y = Mean)) +
#  geom_errorbar(aes(ymin = LowerIQR, ymax = UpperIQR), width = 0.2) +
#  geom_point(size = 3, color = 'blue') +
#  labs(title = "Boxplot for 5 Years", x = "Year", y = "Value") +
#  theme_minimal()

inf_pred <- data.frame(
  Year = factor(1:5),
  Mean = c(0.446, 0.982, 0.834, 0.794, 0.492),        # Replace these with your actual mean values
  LowerIQR = c(0.151, 0.414, 0.578, 0.451, 0.330),      # Replace these with your actual lower IQR values
  UpperIQR = c(0.456, 1.193, 1.019, 0.957, 0.602)     # Replace these with your actual upper IQR values
)

inf_ref$Group <- 'Observed'
inf_pred$Group <- 'Predicted'

# Combine the two dataframes
inf_comb <- rbind(inf_ref, inf_pred)

#VISUALIZATION OF RISKS
#from: Applying Risk-Based Follow-Up Strategies on the Dutch Breast Cancer Population: Consequences for Care and Costs (Draeger VIH 2020)
ggplot(inf_comb, aes(x = Year, y = Mean, color = Group)) +
  geom_errorbar(aes(ymin = LowerIQR, ymax = UpperIQR), width = 0.2, position = position_dodge(0.3)) +
  geom_point(position = position_dodge(0.3), size = 3) +
  labs(title = "Combined Plot of Two Datasets for LRR", x = "Year", y = "Value") +
  theme_minimal() +
  scale_color_manual(values = c("Observed" = "blue", "Predicted" = "red"))
# YEAR 4 LIJKT WAT HOOG


##
prop.table(table(test$t.DM))
prop.table(table(test$t.LRR))

## VDT // SYMPTOMATIC CALIBRATION 
#test$t_det <- ifelse(test$vdt_lrr == 0, 0, fn_days_symptomatic(vdt = test$vdt_lrr, data = df_patient, model = symp_cox_model))

#time to symptoms
test$t_symp <- mapply(function(vdt) {
  if (vdt == 0) {
    return(0)
  } else {
    return(fn_days_symptomatic(vdt, data = df_patient, model = symp_cox_model))
  }
}, test$vdt_lrr)

#time to pass detection threshold
test$dfi <- mapply(function(vdt) {
  if (vdt == 0) {
    return(0)
  } else {
    return(fn_t(V_d, V_0, vdt))
  }
}, test$vdt_lrr)

#SYMPTOMATIC OR ROUTINE VISIT
# Define the scheduled hospital visit times
visit_times <- c(0, 365, 730, 1095, 1460, 1825)

# DYNAMIC ROUTINE VISITS (ADHERENCE)
# Original static visit times
routine_visit_times <- c(0, 365, 730, 1095, 1460, 1825)

# Generate dynamic visit times
dynamic_visit_times <- fn_dynamic_visits(routine_visit_times)

# Apply the function to each row in the dataframe
test$detection_stat <- mapply(fn_detection_type_static, test$dfi, test$t_symp) #, routine_visit_times)

## VOLUME DOUBLING TIME
ggplot(CDF_values, aes(x=VDT, y=CP)) + geom_point() + xlab("Volume Doubling Time (days)") + ylab("Cumulative Probability") + geom_errorbar(aes(xmin=min, xmax=max), width=0.01) +xlim(0,800)+
  stat_function(
    fun = function(x) pnorm(x, mean = mean.norm.vdt, sd = sd.norm.vdt),
    aes(color = "Fitted"),
    linetype = "solid") +
  scale_color_manual(values = c("red", "blue", "green", "orange", "pink"))

#subset for vdt > 0, sorted ascending with added cumulative probability
sub.vdt <- subset(test, t.LRR > 0) 
sub.vdt$n <- 1
sub.vdt <- sub.vdt[order(sub.vdt$vdt_lrr),]
sub.vdt <- sub.vdt %>%
  mutate(
    #'Count CDF' = cumsum(Counts) / sum(Counts),
    'CP' = cumsum(n) / sum(n)
  )

#plot pred vdt (max =107) against fitted vdt
ggplot(data.frame(x = c(0, 120)), aes(x = x)) + xlim(c(0, 120)) + ylim(0, 1) +
  stat_function(
    fun = function(x) pnorm(x, mean = mean.norm.vdt, sd = sd.norm.vdt),
    aes(color = "Fitted"),
    linetype = "solid") +
  geom_point(data = sub.vdt, 
           mapping = aes(x = vdt_lrr, y = CP))


## RISK OF DISTANT METASTASIS
risk.list.dm <- apply(test, 1, function(row) {
  vector <- c(row["age.grp"], row["grade"], row["stage"], row["nstatus"], row["multif"], row["sur"], row["chemo"], row["radio"], row["horm"], row["antiher2"])
  risk <- fn_risk(vector, inf_matrix, rec=2)
  return(risk)
  
})

#conditional risk
risks.dm <- as.data.frame(do.call(rbind,risk.list.dm))
#risk.index <- rownames(risks)
risks.dm$con_1 <- risks.dm$c_DM1
risks.dm$con_2 <- risks.dm$c_DM2 - risks.dm$c_DM1
risks.dm$con_3 <- risks.dm$c_DM3 - risks.dm$c_DM2
risks.dm$con_4 <- risks.dm$c_DM4 - risks.dm$c_DM3
risks.dm$con_5 <- risks.dm$c_DM5 - risks.dm$c_DM4

inf_pred_dm <- data.frame(
  Year = factor(1:5),
  Mean = c(0.6858, 1.3696, 1.4887, 1.2726, 0.9723),        # Replace these with your actual mean values
  LowerIQR = c(0.2500, 0.5101, 0.5691, 0.5065, 0.906),      # Replace these with your actual lower IQR values
  UpperIQR = c(0.8389, 1.7098, 1.8924, 1.6421, 1.2742)     # Replace these with your actual upper IQR values
)

inf_pred_dm$Group <- 'Predicted'

ggplot(inf_pred_dm, aes(x = Year, y = Mean, color = Group)) +
  geom_errorbar(aes(ymin = LowerIQR, ymax = UpperIQR), width = 0.2, position = position_dodge(0.3)) +
  geom_point(position = position_dodge(0.3), size = 3) +
  labs(title = "Risks of DM for 100.000 simulated patients per year of follow-up", x = "Year", y = "Conditional Annual Risk of DM") +
  theme_minimal() #+
  #scale_color_manual(values = c("Observed" = "blue", "Predicted" = "red"))

## BC DEATH 
#check "oligo.R"
sub.dm <- subset(test, t.DM > 0)
sub.dm$type <- ifelse(runif(nrow(sub.dm)) < p.oligo, 1, 2) #1 oligo , 2 dm

sub.dm$ttod <- apply(sub.dm,1, function(row) fn_day_death_bc(row["type"]))

surv.pred.o <- survfit(Surv(ttod) ~ 1, data = sub.dm[sub.dm$type == 1,])
surv.pred.dm <- survfit(Surv(ttod) ~ 1, data = sub.dm[sub.dm$type == 2,])


# Plot together
plot(surv.observed.dm, col = "black", xlab = "Time since diagnosis (days)", ylab = "Survival probability", conf.int = F, xlim=c(0,7000), cex.lab=1.5, cex.axis=1.4)
pct <- seq(.01,.99,by=.01)
lines(surv.observed.dm, lwd=2, col='black', conf.int = F) #observed >3 DM
lines(predict(fit.wei,type="quantile",p=pct)[1,],1-pct,col="red",lwd=2) #fit weibull dm
lines(surv.observed.o, lwd=2, col='black', conf.int = F) #observed oligo
lines(predict(fit.wei.o,type="quantile",p=pct)[1,],1-pct,col="red",lwd=2) #fit weibull oligo
lines(surv.pred.dm, lwd=2, col='orange', conf.int = F) #based on test
lines(surv.pred.o, lwd=2, col='orange', conf.int = F) #based on test
legend("topright", legend = c("Fitted Oligo DM", "Observed", "Fitted >3 DM", "Simulated >3 DM", "Simulated Oligo DM"), col = c("red", "black", "red", "orange", "orange"), lty = 1, bty = "n")


#lines(surv.observed.w, lwd=2, col = "red", conf.int = F)
#lines(surv.observed.w.dm, lwd=2, col='red', conf.int = F)
#lines(surv.observed.dm, lwd=2, col='black', conf.int = F)



# Plot together
png(file="saving_plot7.png",
    height = 400,
    width = 600)

plot(surv.observed.o, col='black', xlab = "Time since diagnosis (days)", ylab = "Survival probability", conf.int = F, xlim=c(0,7000), cex.lab=1.5, cex.axis=1.4)
lines(surv.observed.o, lwd=3, col='black', conf.int = F)
lines(surv.observed.dm, lwd=3, col='black', conf.int = F)

lines(surv.pred.dm, lwd=3, col='#D788AD', conf.int = F)
lines(surv.pred.o, lwd=3, col='#D788AD', conf.int = F)

legend("topright", legend = c("Observed Survival", "Simulated Survival"), col = c("black", "#D788AD"), lty = 1:1, bty = "n")

dev.off()

#shape = fit.wei$scale, scale = exp(fit.wei$coefficients))
sub.dm$event <- 1
fit.o <- survreg(Surv(ttod, event) ~ 1,data = sub.dm[sub.dm$type == 1,], dist = "weibull")
fit.dm <- survreg(Surv(ttod, event) ~ 1,data = sub.dm[sub.dm$type == 2,], dist = "weibull")


plot(surv.observed.o, col = "black", main = "Fitted parametric survival curves", xlab = "Time since diagnosis (days)", ylab = "Survival probability")
lines(surv.observed.o[1], lwd=1)
lines(surv.observed.dm, lwd=1, col='black', conf.int = T)
pct <- seq(.01,.99,by=.01)
lines(predict(fit.o,type="quantile",p=pct)[1,],1-pct,col="red",lwd=1)
lines(predict(fit.dm,type="quantile",p=pct)[1,],1-pct,col="blue",lwd=1)

#Komogorov-Smirnov tests
ks_test1 <- ks.test(oligo$time_to_death, "pweibull", shape = fit.o$scale, scale = exp(fit.o$coefficients))
ks_test2 <- ks.test(dm$time_to_death, "pweibull", shape = fit.dm$scale, scale = exp(fit.dm$coefficients))

## SYMPTOMATIC
sub.vdt$vdt_lrr <- ifelse(sub.vdt$vdt_lrr == 0, fn_trnorm(1, mean.norm.vdt, sd.norm.vdt,1,21.55), sub.vdt$vdt_lrr)
# Apply the function to each row in the dataframe
sub.vdt$detection_stat <- mapply(fn_detection_type_static, sub.vdt$dfi, sub.vdt$t_symp) #, routine_visit_times)

## LRR SUBSET
test.lrr <- subset(test, t.LRR > 0)
test.lrr <- test.lrr %>%
  mutate(jr = t.LRR / 365)

ggplot(test.lrr) + aes(x = jr) + geom_bar()
#combined plot risk and incidence??


## DM SUBSET
test.dm <- subset(test, t.DM > 0)
ggplot(test.dm) + aes(x = t.DM) + geom_bar()


###################################### Calibration 100 000 pt annual conditional risks
summary(risks.bef$con_1)*100
summary(risks.bef$con_2)*100
summary(risks.bef$con_3)*100
summary(risks.bef$con_4)*100
summary(risks.bef$con_5)*100


#reference - influence 2 cohort
inf_ref <- data.frame(
  Year = factor(1:5),
  Mean = c(0.4943, 0.8969, 0.8380, 0.7083, 0.5022),        # Replace these with your actual mean values
  LowerIQR = c(0.1614, 0.3990, 0.5937, 0.4662, 0.3517),      # Replace these with your actual lower IQR values
  UpperIQR = c(0.5367, 1.0859, 1.0188, 0.7423, 0.5929)     # Replace these with your actual upper IQR values
)

#before calibration
inf_bef <- data.frame(
  Year = factor(1:5),
  Mean = c(0.4501, 1.03554, 1.0010, 0.9680, 0.5895),        # Replace these with your actual mean values
  LowerIQR = c(0.150, 0.4563, 0.6873, 0.6026, 0.3950),      # Replace these with your actual lower IQR values
  UpperIQR = c(0.460, 1.22706, 1.2069, 1.0646, 0.7158)     # Replace these with your actual upper IQR value
)

#inf_aft <- data.frame(
#  Year = factor(1:5),
#  Mean = c(0.4501, 0.7654, 0.6224, 0.4651, 0.3930),        # Replace these with your actual mean values
#  LowerIQR = c(0.1502, 0.3375, 0.4339, 0.2802, 0.2636),      # Replace these with your actual lower IQR values
#  UpperIQR = c(0.4603, 0.9241, 0.7518, 0.5084, 0.4772)     # Replace these with your actual upper IQR values
#)

inf_aft <- data.frame(
  Year = factor(1:5),
  Mean = c(0.4501, 0.9005, 0.8117, 0.7165, 0.4913),        # Replace these with your actual mean values
  LowerIQR = c(0.1502, 0.3987, 0.5639, 0.4420, 0.3292),      # Replace these with your actual lower IQR values
  UpperIQR = c(0.4603, 1.0701, 0.9764, 0.7806, 0.5965)     # Replace these with your actual upper IQR values
)

inf_ref$Group <- 'Observed cohort'
inf_bef$Group <- 'Before calibration'
inf_aft$Group <- 'After calibration'


# Combine the two dataframes
inf_comb <- rbind(inf_aft, inf_ref, inf_bef)

#from: Applying Risk-Based Follow-Up Strategies on the Dutch Breast Cancer Population: Consequences for Care and Costs (Draeger VIH 2020)
plot <- ggplot(inf_comb, aes(x = Year, y = Mean, color = Group)) +
  #geom_line(aes(group = Group)) +
  geom_errorbar(aes(ymin = LowerIQR, ymax = UpperIQR), width = 0.3, position = position_dodge(0.3)) +
  geom_point(position = position_dodge(0.3), size = 5) +
  labs(title = " ", x = "Surveillance Year", y = "Conditional Risk") +
  theme_minimal() +
  #theme(legend.position="none") +
  theme(text = element_text(size = 20), 
        panel.grid.major.x = element_blank()) +
  scale_color_manual(values = c("Before calibration" = "red", "Observed cohort" = "blue", "After calibration" = "darkgreen"))

ggsave(
  filename = paste0("Obs_risks_", format(Sys.time(), "%Y-%m-%d_%H-%M"), ".png"),
  plot = plot,
  path = "manuscript",
  width = 13,
  height = 10,
  dpi = 400,
  device = "png"
)

#VISUALIZATION OF RISKS
#from: Applying Risk-Based Follow-Up Strategies on the Dutch Breast Cancer Population: Consequences for Care and Costs (Draeger VIH 2020)
ggplot(inf_comb, aes(x = Year, y = Mean, color = Group)) +
  geom_errorbar(aes(ymin = LowerIQR, ymax = UpperIQR), width = 0.2, position = position_dodge(0.3)) +
  geom_point(position = position_dodge(0.3), size = 3) +
  labs(title = "Combined Plot of Two Datasets for LRR", x = "Year", y = "Value") +
  theme_minimal() +
  scale_color_manual(values = c("Observed" = "blue", "Predicted" = "red"))


###########
png("survival_plot.png", width = 7, height = 5, units = "in", res = 300)
# Plot with x-axis in years
plot(surv.observed.dm, col = "white", xlab = "Time since diagnosis (years)", 
     ylab = "Survival probability", conf.int = FALSE, 
     xlim = c(0, 7000/365), cex.lab = 1.5, cex.axis = 1.4, xaxt = "n")

# Add custom x-axis in years
axis(1, at = seq(0, 7000/365, by = 2), labels = seq(0, 7000/365, by = 2), cex.axis = 1.4)

# Define percentiles for quantile predictions
pct <- seq(.01, .99, by = .01)

# Overlay survival and prediction lines, transforming time from days to years
lines(surv.observed.dm$time/365, surv.observed.dm$surv, lwd=2, col='black') # Observed >3 DM
lines(predict(fit.wei, type="quantile", p=pct)[1,]/365, 1-pct, col="red", lwd=2) # Fitted Weibull DM

lines(surv.observed.o$time/365, surv.observed.o$surv, lwd=2, col='black') # Observed Oligo
lines(predict(fit.wei.o, type="quantile", p=pct)[1,]/365, 1-pct, col="red", lwd=2) # Fitted Weibull Oligo

lines(surv.pred.dm$time/365, surv.pred.dm$surv, lwd=2, col='orange') # Simulated >3 DM
lines(surv.pred.o$time/365, surv.pred.o$surv, lwd=2, col='orange') # Simulated Oligo DM

# Add legend
legend("topright", legend = c("Fitted Oligo DM", "Observed", "Fitted >3 DM", 
                              "Simulated >3 DM", "Simulated Oligo DM"), 
       col = c("red", "black", "red", "orange", "orange"), 
       lty = 1, bty = "n")

dev.off()

