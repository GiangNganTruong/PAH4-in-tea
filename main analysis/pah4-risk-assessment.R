library(ggplot2)
library(dplyr)
library(tidyr)

# Population group data
pah4_profiles <- data.frame(
  group = c("Male_0_3", "Male_3_6", "Male_6_12", "Male_12_16", "Male_16_18", "Male_19_65", 
            "Female_0_3", "Female_3_6", "Female_6_12","Female_12_16", "Female_16_18", "Female_19_65"),
  # Tea consumption data (g/day)
  meanTea = c(158.47, 225.05, 376.35, 524.73, 556.15, 843.99, 
              126.63, 172.28, 305.67, 379.94, 461.17, 565.50),  
  sdTea = c(116.96, 173.68, 236.96, 346.89, 408.43, 836.87, 
            91.73, 154.33, 228.84, 256.29, 345.76, 580.28),
  # Body weight data (kg)
  meanW = c(13.46, 20.52, 37.91, 61.75, 67.12, 73.19, 
            12.27, 19.59, 35.18, 53.45, 58.11, 59.97),
  sdW = c(3.27, 5.04, 13.37, 16.31, 14.19, 14.29, 
          3.01, 4.51, 11.98, 11.91, 13.99, 11.68)
)

# PAH4 concentration data (μg/kg): For a conservative estimation, the upper-bound estimates were used for assessment. Upper-bound estimates assume that the amounts in non-detected/non-quantified samples are equal Limit of Detection/Limit of Quantitation
meanpah4 <- 0.422
sdpah4 <- 0.217

# Monte Carlo simulation
n_iter <- 10000
pah4_results <- data.frame()

set.seed(123)  

for (i in 1:nrow(pah4_profiles)) {
  # Extract parameters for current group
  meanTea <- pah4_profiles$meanTea[i]
  sdTea <- pah4_profiles$sdTea[i]
  meanW <- pah4_profiles$meanW[i]
  sdW <- pah4_profiles$sdW[i]
  group <- pah4_profiles$group[i]
  
  # Simulate tea consumption (lognormal)
  simTea <- rlnorm(n_iter, 
                   meanlog = log(meanTea^2 / sqrt(sdTea^2 + meanTea^2)),
                   sdlog = sqrt(log(1 + (sdTea^2 / meanTea^2)))) / 1000
  
  # Simulate body weight (lognormal)
  simW <- rlnorm(n_iter, 
                 meanlog = log(meanW^2 / sqrt(sdW^2 + meanW^2)),
                 sdlog = sqrt(log(1 + (sdW^2 / meanW^2))))
  
  # Simulate PAH4 concentration (lognormal)
  simpah4 <- rlnorm(n_iter, 
                    meanlog = log(meanpah4^2 / sqrt(sdpah4^2 + meanpah4^2)),
                    sdlog = sqrt(log(1 + (sdpah4^2 / meanpah4^2))))
  
  # Calculate daily intake and MOE
  DI <- (simpah4 * simTea) / simW  # μg/kg bw/day
  MOE <- 0.34 * 1000 / DI  # Convert BMDL10 from mg to μg
  
  # Store percentiles
  q <- quantile(MOE, probs = c(0.025, 0.5, 0.975))
  pah4_results <- rbind(pah4_results, data.frame(group = group, 
                                                 p2.5 = q[1], 
                                                 p50 = q[2], 
                                                 p97.5 = q[3]))
}

rownames(pah4_results) <- NULL
print(pah4_results)

# Identify groups below safety threshold
cat("\n**Groups with MOE < 10,000 at median:**\n")
pah4_risk_groups <- pah4_results[pah4_results$p50 < 10000, ]
print(pah4_risk_groups)

# Prepare data for visualization
data_long <- pah4_results %>%
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

# Create plot
ggplot(data_long, aes(x = Position, y = MOE)) +
  geom_hline(yintercept = 10000, linetype = "dashed", 
             color = "red", linewidth = 1, alpha = 0.7) +
  geom_point(aes(shape = Percentile, color = Gender), size = 3) +
  scale_shape_manual(values = c("2.5th" = 1, "50th" = 16, "97.5th" = 17)) +
  scale_color_manual(values = c("Female" = "#E41A1C", "Male" = "#377EB8")) +
  scale_y_log10(
    breaks = 10^(3:7),
    labels = c("1,000", "10,000", "100,000", "1,000,000", "10,000,000"),
    limits = c(1000, 10^7)
  ) +
  scale_x_continuous(
    breaks = 1:6,
    labels = c("0-3", "3-6", "6-12", "12-16", "16-18", "19-65")
  ) +
  labs(
    title = "PAH4 Exposure Margins in Taiwanese RTD Tea Consumers",
    x = "Age Group (years)", 
    y = "Margin of Exposure (MOE, log scale)",
    shape = "Percentile",s
    color = "Gender"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.title = element_text(size = 12)
  )