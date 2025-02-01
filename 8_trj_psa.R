# Plot
library(tidyverse)
library(hrbrthemes)
library(viridis)
library(dplyr)
library(tidyverse)
library(ggplot2)

# mean.norm.vdt 
# sd.norm.vdt
#                                                                                                   
#   
# d_t <- 5
# V_d <-((4/3)*pi*(d_t/2)^3)/1000 
# 
# #
# d_vdt_est <- c(mean = 178.85, sd = 75.75)
# d_vdt_vcov = matrix(data = c(358.6385, 0.0000, 0.0000, 179.3145),
#                     nrow = 2, dimnames = list(c('mean', 'sd'), c('mean', 'sd')))
# library(MASS)
# d_vdt_coef <- mvrnorm(n = 1, mu = d_vdt_est, Sigma = d_vdt_vcov)



# Function to get multiple attributes
getAttributes <- function(attributes, output) {
  output %>%
    filter(key %in% attributes) %>%
    group_by(name, key) %>%
    summarise(value = last(value), .groups = "drop") %>%
    pivot_wider(names_from = key, values_from = value) %>%
    as.data.frame()
}
######### VDT ##############
# vdt_lrr_df <- data.frame(name = character(0), vdt_lrr_values = numeric(0), t_LRR_values = numeric(0))
# 
# # Loop through each dataframe in the list
# for (i in 1:length(sim_results_psa)) {
# #for (i in 1:length(sim_results)) {
#   # Access the dataframe attributes
#   df <- sim_results_psa[[i]]$attributes
#   
#   # Group by 'name' and filter where 'key' is 'vdt_lrr' and 'vdt_lrr' is greater than 0
#   df_filtered <- df[df$key == "vdt_lrr" & df$value > 0, ]
#   
#   # Loop through the filtered dataframe (by name)
#   for (j in 1:nrow(df_filtered)) {
#     name <- df_filtered$name[j]  # Get the 'name'
#     vdt_lrr_value <- df_filtered$value[j]  # Get the 'vdt_lrr' value
#     
#     # Extract corresponding t_LRR value for the same individual
#     t_LRR_value <- df$value[df$name == name & df$key == "t_LRR"]
#     
#     # Append the name, vdt_lrr value, and t_LRR value to the dataframe
#     if (length(t_LRR_value) > 0) {
#       vdt_lrr_base <- rbind(vdt_lrr_base, 
#                           data.frame(name = name, vdt_lrr_values = vdt_lrr_value, t_LRR_values = t_LRR_value))
#     }
#   }
# }
# 
# vdt_lrr_df_filtered <- vdt_lrr_df %>%
#   filter(t_LRR_values != 0)
# 
# vdt_lrr_base_filtered <- vdt_lrr_base %>%
#   filter(t_LRR_values != 0)
# 
# ## SCATTERPLOT
# vdt_lrr_base_filtered$source <- "base"
# vdt_lrr_df_filtered$source <- "psa"
# 
# combined_data <- rbind(vdt_lrr_base_filtered, vdt_lrr_df_filtered)
# # Create the scatter plot
# ggplot(combined_data, aes(x = vdt_lrr_values, y = t_LRR_values, color = source)) +
#   geom_point() +  # Add points to the scatter plot
#   labs(title = "Scatterplot of vdt_lrr_values vs t_LRR_values",
#        x = "vdt_lrr Values",
#        y = "t_LRR Values") +
#   scale_color_manual(values = c("df" = "blue", "base" = "red")) +  # Assign colors to sources
#   theme_minimal()  # Use a clean, minimal theme
# 

######
#Initialize an empty list to store the results
vdt_vdt_psa_risk <- list()
t_vdt_psa_risk <- list()

vdt_dt_psa_risk <- list()
t_dt_psa_risk <- list()

# Loop through each dataframe in the list
for (i in 1:length(sim_psa_risk_dt_100)) {
  # Access the dataframe attributes
  df <- sim_psa_risk_dt_100[[i]]$attributes
  
  # Filter the row where 'key' is 'vdt_lrr' and the 'value' is greater than 0
  #value <- df$value[df$key == "vdt_lrr" & df$value > 0]
  value <- df$value[df$key == "t_LRR" & df$value > 0]
  
  # Only store the value if it exists and is greater than 0
  if (length(value) > 0) {
    #vdt_lrr_psa_risk[[i]] <- value
    t_dt_psa_risk[[i]] <- value
  }
}

# DATAFRAME FOR SCATTERPLOT
# Initialize an empty dataframe to store the results
df_psa_vdt <- data.frame(simulation_id = numeric(0), 
                                 mean_vdt_lrr = numeric(0), 
                                 lower_IQR = numeric(0), 
                                 upper_IQR = numeric(0))

# Loop through each list (each simulation's vdt_lrr values)
for (i in 1:length(vdt_lrr_psa)) {
  # Extract the vdt_lrr values for the current simulation
  vdt_lrr_sim <- unlist(vdt_lrr_psa[[i]])
  
  # Calculate the mean, lower IQR (25th percentile), and upper IQR (75th percentile)
  mean_value <- mean(vdt_lrr_sim, na.rm = TRUE)
  lower_IQR_value <- quantile(vdt_lrr_sim, 0.25, na.rm = TRUE)
  upper_IQR_value <- quantile(vdt_lrr_sim, 0.75, na.rm = TRUE)
  
  # Add the results to the dataframe
  df_psa_vdt <- rbind(df_psa_vdt, 
                              data.frame(simulation_id = i,
                                         mean_vdt_lrr = mean_value, 
                                         lower_IQR = lower_IQR_value, 
                                         upper_IQR = upper_IQR_value))
}
row.names(df_psa_vdt) <- NULL

ggplot(df_psa_vdt, aes(x = simulation_id, y = mean_vdt_lrr)) +
  geom_errorbar(aes(ymin = lower_IQR, ymax = upper_IQR), width = 0.2) +
  geom_point(position = position_dodge(0.3), size = 3) +
  theme_minimal() 

####### T_LRR SUMMARY
# Initialize an empty dataframe to store the results
t_dt_summary_risk <- data.frame(simulation_id = numeric(0), 
                            total = numeric(0),
                            year_1 = numeric(0), 
                            year_2 = numeric(0),
                            year_3 = numeric(0), 
                            year_4 = numeric(0), 
                            year_5 = numeric(0))

# Loop through each simulation in t_lrr_psa
for (i in 1:length(t_dt_psa_risk)) {
  # Extract the t_LRR values for the current simulation
  t_lrr_sim <- unlist(t_dt_psa_risk[[i]])
  
  # Calculate the total number of values
  total_count <- length(t_lrr_sim)
  
  # Count how many times each specific value (365, 730, 1095, 1460, 1825) appears
  year_1 <- sum(t_lrr_sim == 365)
  year_2 <- sum(t_lrr_sim == 730)
  year_3 <- sum(t_lrr_sim == 1095)
  year_4 <- sum(t_lrr_sim == 1460)
  year_5 <- sum(t_lrr_sim == 1825)
  
  # Add the results to the dataframe
  t_dt_summary_risk <- rbind(t_dt_summary_risk, 
                         data.frame(simulation_id = i, 
                                    total= total_count, 
                                    year_1 = year_1, 
                                    year_2 = year_2,
                                    year_3 = year_3, 
                                    year_4 = year_4, 
                                    year_5 = year_5))
}

t_dt_summary_risk <- t_dt_summary_risk %>%
  mutate(prop_1 = (year_1 / total)*100,
         prop_2 = (year_2 / total)*100,
         prop_3 = (year_3 / total)*100,
         prop_4 = (year_4 / total)*100,
         prop_5 = (year_5 / total)*100,
         rate_1 = (year_1 / 10000)*100,
         rate_2 = (year_2 / 10000)*100,
         rate_3 = (year_3 / 10000)*100,
         rate_4 = (year_4 / 10000)*100,
         rate_5 = (year_5 / 10000)*100,)

### BOXPLOT 
psa_concat_risk_dt <- t_dt_summary_risk %>%
  subset(select = c("simulation_id", 'rate_1', 'rate_2', 'rate_3', 'rate_4', 'rate_5')) %>%
  pivot_longer(cols = starts_with("rate_"), 
               names_to = "name", 
               values_to = "value")


# # Violin basic
# psa_concat_risk %>%
#   ggplot( aes(x=name, y=value, fill=name)) +
#   geom_violin() +
#   scale_fill_viridis(discrete = TRUE, alpha=0.6, option="A") +
#   theme_ipsum() +
#   theme(
#     legend.position="none",
#     plot.title = element_text(size=11)
#   ) +
#   #ggtitle("Violin chart") +
#   xlab("")
# 
# sample_size = psa_concat %>% group_by(name) %>% summarize(num=n())
# 
# # Plot
# psa_concat %>%
#   left_join(sample_size) %>%
#   mutate(myaxis = paste0(name, "\n", "n=", num)) %>%
#   ggplot( aes(x=myaxis, y=value, fill=name)) +
#   geom_violin(width=1.4) +
#   geom_boxplot(width=0.1, color="grey", alpha=0.2) +
#   scale_fill_viridis(discrete = TRUE) +
#   theme_ipsum() +
#   theme(
#     legend.position="none",
#     plot.title = element_text(size=11)
#   ) +
#   ggtitle("A Violin wrapping a boxplot") +
#   xlab("")

#boxplot with jitter
points_df <- data.frame(
  name = c("rate_1", "rate_2", "rate_3", "rate_4", "rate_5"),
  value = c(0.403, 0.720, 0.545, 0.441, 0.443),
  LowerIQR = c(0.403*0.9, 0.720*0.9, 0.545*0.9, 0.441*0.9, 0.443*0.9),     
  UpperIQR = c(0.403*1.1, 0.720*1.1, 0.545*1.1, 0.441*1.1, 0.443*1.1)
)

violin_risk_dt <- psa_concat_risk_dt %>%
  ggplot(aes(x=name, y=value)) +
  geom_violin(trim=FALSE, fill='white', color="red", linewidth = 1.0) + #D788AD
  stat_summary(fun=mean, geom="point", shape=23, size=4, color='red') +
  #scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  #geom_jitter(color="black", size=1.4, alpha=0.9) +
  theme_minimal() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  #ggtitle("A boxplot with jitter") +
  #xlab("Surveillance year") +
  scale_x_discrete(
    "Surveillance Year",
    labels = c(
      "rate_1" = "Year 1",
      "rate_2" = "Year 2",
      "rate_3" = "Year 3",
      "rate_4" = "Year 4",
      "rate_5" = "Year 5"
    )
  )  +
  theme(text = element_text(size = 20), 
        panel.grid.major.x = element_blank()) +
  ylab("Rate of recurrence") +
  # Add points for the individual years
  geom_point(data=points_df, aes(x=name, y=value), color="blue", size=4) +
  geom_errorbar(data=points_df, aes(x=name, ymin = LowerIQR, ymax = UpperIQR), width = 0.4, position = position_dodge(0.3))  

#violin
violin_risk_dt

ggsave(
  filename = paste0("violin_100_dt_", format(Sys.time(), "%Y-%m-%d_%H-%M"), ".png"),
  plot = violin_risk_dt,
  path = "output",
  width = 15,
  height = 10,
  dpi = 400,
  device = "png"
)

#### VDT
violin_risk_vdt <- psa_concat_risk_vdt %>%
  ggplot(aes(x=name, y=value)) +
  geom_violin(trim=FALSE, fill='white', color="red", linewidth = 1.0) + ##D788AD
  stat_summary(fun=mean, geom="point", shape=23, size=4, color='red') +
  #scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  #geom_jitter(color="black", size=1.4, alpha=0.9) +
  theme_minimal() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  #ggtitle("A boxplot with jitter") +
  #xlab("Surveillance year") +
  scale_x_discrete(
    "Surveillance Year",
    labels = c(
      "rate_1" = "Year 1",
      "rate_2" = "Year 2",
      "rate_3" = "Year 3",
      "rate_4" = "Year 4",
      "rate_5" = "Year 5"
    )
  )  +
  theme(text = element_text(size = 20), 
        panel.grid.major.x = element_blank()) +
  ylab("Rate of recurrence") +
  # Add points for the individual years
  geom_point(data=points_df, aes(x=name, y=value), color="blue", size=4) +
  geom_errorbar(data=points_df, aes(x=name, ymin = LowerIQR, ymax = UpperIQR), width = 0.4, position = position_dodge(0.3))  

#violin
violin_risk_vdt

ggsave(
  filename = paste0("violin_100_vdt_", format(Sys.time(), "%Y-%m-%d_%H-%M"), ".png"),
  plot = violin_risk_vdt,
  path = "output",
  width = 15,
  height = 10,
  dpi = 400,
  device = "png"
)


########## COMBINATION PLOT ###########
# Add a new column to each dataset to identify them
psa_concat_risk_vdt$dataset <- "VDT"
psa_concat_risk_dt$dataset <- "DT"
points_df$dataset <- "Points"
# Combine the two datasets
combined_data <- rbind(psa_concat_risk_vdt, psa_concat_risk_dt)

# Modify your plot to include the color aesthetic for the combined data
violin_risk <- combined_data %>%
  ggplot(aes(x=name, y=value, color=dataset)) +  # Map color to the 'dataset' column
  geom_violin(alpha=0.5, position=position_dodge(0.9)) +
  stat_summary(fun=mean, geom="point", shape=23, size=2) +
  geom_jitter(size=1, alpha=0.9) +
  theme_minimal() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  scale_x_discrete(
    "Surveillance Year",
    labels = c(
      "rate_1" = "Year 1",
      "rate_2" = "Year 2",
      "rate_3" = "Year 3",
      "rate_4" = "Year 4",
      "rate_5" = "Year 5"
    )
  ) +
  ylab("Rate of recurrence") +
  scale_color_manual(values = c("VDT" = "pink", "DT" = "green")) +
  # Add points for the individual years
  geom_point(data=points_df, aes(x=name, y=value), color="blue", size=3) +
  geom_errorbar(data=points_df, aes(x=name, ymin = LowerIQR, ymax = UpperIQR), width = 0.2, position = position_dodge(0.3)) 
  

# Display the plot
violin_risk



########### DT #############
#Initialize an empty list to store the results
vdt_lrr_psa_dt <- list()
t_lrr_psa_dt <- list()

# Loop through each dataframe in the list
for (i in 1:length(sim_results_psa)) {
  # Access the dataframe attributes
  df <- sim_results_psa[[i]]$attributes
  
  # Filter the row where 'key' is 'vdt_lrr' and the 'value' is greater than 0
  value <- df$value[df$key == "t_LRR" & df$value > 0]
  
  # Only store the value if it exists and is greater than 0
  if (length(value) > 0) {
    #vdt_lrr_psa[[i]] <- value
    t_lrr_psa_dt[[i]] <- value
  }
}

dt_psa_dt <- list()

for (i in 1:length(sim_results_psa)) {
  # Access the dataframe attributes
  df <- sim_results_psa[[i]]$attributes
  
  # Find the value where 'key' is 't_LRR' and its value is greater than 0
  value <- df$value[df$key == "t_LRR" & df$value > 0]
  
  # Only proceed if t_LRR_value exists and is greater than 0
  if (length(value) > 0) {
    # Now grab the value associated with 'd_t'
    d_t_value <- df$value[df$key == "d_t"]
    
    # Only store the value of 'd_t' if it exists
    if (length(d_t_value) > 0) {
      dt_psa_dt[[i]] <- d_t_value
    }
  }
}




















