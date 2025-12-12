profiles <- data.frame(
  group = c("Male_0_3", "Male_3_6", "Male_6_12", "Male_12_16", "Male_16_18", "Male_19_65", "Female_0_3", "Female_3_6", "Female_6_12","Female_12_16", "Female_16_18", "Female_19_65"),
#Tea consumption data: archived from TFDA public survey from 2017 to 2020
  meanTea = c(158.47, 225.05, 376.35, 524.73, 556.15, 843.99, 126.63, 172.28, 305.67, 379.94, 461.17, 565.50),  
  sdTea = c(116.96, 173.68, 236.96, 346.89, 408.43, 836.87, 91.73, 154.33, 228.84, 256.29, 345.76, 580.28),
#Body weight data: archived from TFDA public survey from 2017 to 2020
  meanW = c(13.46, 20.52, 37.91, 61.75, 67.12, 73.19, 12.27, 19.59, 35.18, 53.45, 58.11, 59.97),
  sdW = c(3.27, 5.04, 13.37, 16.31, 14.19, 14.29, 3.01, 4.51, 11.98, 11.91, 13.99, 11.68)
)

#Benzo[a]pyrene concentrations in 51 tea samples 
meanbap <- 0.128
sdbap <- 0.077

n_iter <- 10000
results <- data.frame()

set.seed(123)
for (i in 1:nrow(profiles)) {
  # Extract parameters
  meanTea <- profiles$meanTea[i]
  sdTea <- profiles$sdTea[i]
  meanW <- profiles$meanW[i]
  sdW <- profiles$sdW[i]
  group <- profiles$group[i]
  
  # Simulate consumption (lognormal distribution)
  simTea <- rlnorm(n_iter, 
                   meanlog = log(meanTea^2 / sqrt(sdTea^2 + meanTea^2)),
                   sdlog = sqrt(log(1 + (sdTea^2 / meanTea^2)))) / 1000
  
  # Simulate body weight (lognormal distribution)
  simW <- rlnorm(n_iter, 
                 meanlog = log(meanW^2 / sqrt(sdW^2 + meanW^2)),
                 sdlog = sqrt(log(1 + (sdW^2 / meanW^2))))
  
  # Simulate concentration (lognormal distribution)
  simbap <- rlnorm(n_iter, 
                   meanlog = log(meanbap^2 / sqrt(sdbap^2 + meanbap^2)),
                   sdlog = sqrt(log(1 + (sdbap^2 / meanbap^2))))
  
  # Calculate DI and MOE
  DI <- (simbap * simTea) / simW
  MOE <- 70 / DI #BMDL10 values of 0.07 mg/kg bw/day (EFSA, 2008)
  
  # Store percentiles
  q <- quantile(MOE, probs = c(0.025, 0.5, 0.975))
  results <- rbind(results, data.frame(group = group, p2.5 = q[1], p50 = q[2], p97.5 = q[3]))
}
rownames(results) <- NULL
print(results)

#Visualisation
library(ggplot2)
library(dplyr)
library(tidyr)

# Split group into Gender and Age_Group
data_long <- results %>%
  separate(group, into = c("Gender", "Age_Group"), sep = "_") %>%
  pivot_longer(
    cols = c(p2.5, p50, p97.5),
    names_to = "Percentile",
    values_to = "MOE"
  ) %>%
  mutate(
    Gender = factor(Gender, levels = c("Female", "Male")),
    Percentile = recode(Percentile,
                        "p2.5" = "2.5th",
                        "p50" = "50th",
                        "p97.5" = "97.5th"),
    Age_Group = factor(Age_Group, levels = unique(Age_Group)),
    Position = as.numeric(Age_Group) + ifelse(Gender == "Female", -0.2, 0.2)
  )

# Visualisation
ggplot(data_long, aes(x = Position, y = MOE)) +
  geom_hline(yintercept = 10000, linetype = "dashed", color = "red", linewidth = 1) +
  geom_point(aes(shape = Percentile, color = Gender), size = 3) +
  scale_shape_manual(values = c("2.5th" = 1, "50th" = 16, "97.5th" = 17)) +
  scale_color_manual(values = c("Female" = "red", "Male" = "blue")) +
  scale_y_log10(
    breaks = 10^(3:7),
    labels = c("1,000", "10,000", "100,000", "1,000,000", "10,000,000")
  ) +
  scale_x_continuous(
    breaks = 1:6,
    labels = unique(data_long$Age_Group)
  ) +
  labs(
    title = "BaP Exposure Margins in Taiwanese RTD Tea Consumers",
    x = "Age Group", y = "MOE"
  ) +
  theme_minimal()