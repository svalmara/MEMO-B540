################################################

## Detection Bias

# Installation and loading necessary packages

install.packages("readxl")
install.packages("tidyr")
install.packages("dplyr")
install.packages("ggplot2")

library(readxl)
library(tidyr)
library(dplyr)
library(ggplot2)

# Insert data

data <- read_excel("C:\\Users\\GogoFifiLolous\\Desktop\\ULB 2021 - 2022\\Mémoire\\R Studio - Metafor\\Detection Bias.xlsx")

# Reshaping data

data <- data %>%
  mutate(across(c(peer_reviewed, asd_exp, funding, same_setting, design_type, incomplete_outcome_data,
                  reporters_familiarity, boundedness, proximity_distality), as.character))

data_long <- data %>%
  pivot_longer(
    cols = c(peer_reviewed, asd_exp, funding, same_setting, design_type, incomplete_outcome_data,
             reporters_familiarity, boundedness, proximity_distality), 
    names_to = "Indicators", 
    values_to = "Risks"
  )

data_long <- data_long %>%
  mutate(Risks = case_when(
    Risks == "1" ~ "Low risk of bias",
    Risks == "0" ~ "High risk of bias",
    Risks == "?" ~ "Unclear risk of bias",
    TRUE ~ "Other"
  ))

# Create the plot
total_plot <- ggplot(data_long, aes(x = Indicators, fill = Risks)) +
  geom_bar(stat = "count", position = "fill") +
  scale_fill_manual(values = c("Low risk of bias" = "chartreuse3", 
                               "Unclear risk of bias" = "darkgoldenrod1", 
                               "High risk of bias" = "brown1")) +
  labs(title = "Detection Bias for all Included Studies", x = NULL, y = NULL) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  coord_flip()

# Print the plot
print(total_plot)

# Save the plot as a png file
ggsave("Total_Detection_Bias_Plot.png", plot = total_plot, width = 10, height = 8, dpi = 300)

# Summary statistics for the total plot
total_statistics <- data_long %>%
  group_by(Indicators, Risks) %>%
  summarise(Count = n(), .groups = 'drop') %>%
  mutate(Proportion = Count / sum(Count))

# Print the summary statistics
print(total_statistics)

# Excel file with statistics
write.csv(total_statistics, "Total_Plot_Statistics.csv", row.names = FALSE)

################


## Generating a separate plot for each outcome type

# Expanding the Outcome column
data_long <- data_long %>%
  separate_rows(Outcome, sep = ";")

# List of plots, one for each outcome type

# Defining each outcome type and generating plot

titles <- c("1" = "Detection Bias for Cognitive Outcomes",
            "2" = "Detection Bias for Adaptive Functioning Outcomes",
            "3" = "Detection Bias for Social-Communication Outcomes",
            "4" = "Detection Bias for Linguistic Outcomes")

plots <- lapply(unique(data_long$Outcome), function(outcome) {
  subset_data <- data_long[data_long$Outcome == outcome,]
  ggplot(subset_data, aes(x = Indicators, fill = Risks)) +
    geom_bar(stat = "count", position = "fill") +
    scale_fill_manual(values = c("Low risk of bias" = "chartreuse3", 
                                 "Unclear risk of bias" = "darkgoldenrod1", 
                                 "High risk of bias" = "brown1")) +
    labs(title = titles[outcome],
         x = NULL,
         y = NULL) +
    theme_minimal() +
    theme(legend.position = "bottom") +
    coord_flip()
})

# Print plots for verification
if (!is.null(plots)) {
  for (plot in plots) {
    print(plot)
  }
}

# Save png plots
if (!is.null(plots)) {
  lapply(seq_along(plots), function(i) {
    ggsave(paste0("Outcome_Plot_Type_", i, ".png"), plots[[i]], width = 10, height = 8)
  })
}

# Calculate summary statistics for each outcome
outcome_statistics_per_outcome <- lapply(unique(data_long$Outcome), function(outcome) {
  subset_data <- data_long[data_long$Outcome == outcome, ]
  stats <- subset_data %>%
    group_by(Indicators, Risks) %>%
    summarise(Count = n(), .groups = 'drop') %>%
    mutate(Proportion = Count / sum(Count))
  
# Optionally, return a list containing the outcome name and its stats
  list(Outcome = outcome, Statistics = stats)
})

# Print for verification

print(outcome_statistics_per_outcome)

# Excel file with statistics

lapply(outcome_statistics_per_outcome, function(outcome_stat) {
  filename <- paste0("Outcome_", outcome_stat$Outcome, "_Statistics.csv")
  write.csv(outcome_stat$Statistics, filename, row.names = FALSE)
})
