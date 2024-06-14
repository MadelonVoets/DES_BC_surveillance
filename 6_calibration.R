## 6. MODEL CALIBRATION ----
library(ggstatsplot)
library(tidyr)

#Test population
test <- fn_gen_pt(5000)
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
      abs(test_prop - cal_prob) <= 0.05 #* cal_prob
    }, test_proportions, calibration_probs)
    
    # Print the appropriate message based on the checks
    if (all(check_results)) {
      print(paste("Parameter", i, ": Distributions within 5%"))
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
risk.index <- rownames(risks)
risks$con_1 <- risks$c_lrr1
risks$con_2 <- risks$c_lrr2 - risks$c_lrr1
risks$con_3 <- risks$c_lrr3 - risks$c_lrr2
risks$con_4 <- risks$c_lrr4 - risks$c_lrr3
risks$con_5 <- risks$c_lrr5 - risks$c_lrr4

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
    y = "Cumulative Risk Calibration(%)"
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
  Mean = c(0.445, 0.983, 0.839, 0.787, 0.496),        # Replace these with your actual mean values
  LowerIQR = c(0.154, 0.423, 0.594, 0.450, 0.336),      # Replace these with your actual lower IQR values
  UpperIQR = c(0.453, 1.19, 1.018, 0.958, 0.610)     # Replace these with your actual upper IQR values
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
  labs(title = "Combined Plot of Two Datasets", x = "Year", y = "Value") +
  theme_minimal() +
  scale_color_manual(values = c("Observed" = "blue", "Predicted" = "red"))
# YEAR 4 LIJKT WAT HOOG


##
prop.table(table(test$t.DM))
prop.table(table(test$t.LRR))

## BC DEATH 
#check "oligo.R"

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
test$detection_type <- mapply(fn_detection_type, test$dfi, test$t_symp) #, routine_visit_times)























