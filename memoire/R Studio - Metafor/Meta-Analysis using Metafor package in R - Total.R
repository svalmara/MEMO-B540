# install and load the metafor package
install.packages("metafor")
library(metafor)

# Import data from CSV
data <- read.csv("C:\\Users\\GogoFifiLolous\\Desktop\\ULB 2021 - 2022\\Mémoire\\R Studio - Metafor\\Coding Guide and effect sizes.csv", header=TRUE, fileEncoding="UTF-8")

# Examine the data structure
str(data)

# Convert cohens_d to numeric
data$Cohens_d <- as.numeric(as.character(data$Cohens_d))

# Calculate the standard error of Cohen's d
data$SE_d <- sqrt((data$N1 + data$N2) / (data$N1 * data$N2) + (data$Cohens_d^2) / (2 * (data$N1 + data$N2)))

# Filter data for specified treatment and control groups
filtered_data <- data[data$Treatment %in% c("ASDB", "ASDBH", "ASDBSEQ", "ASDBSIM", "ASDBL") & data$Control == "ASDM", ]

# Perform the meta-analysis on the filtered dataset
result <- rma(yi = Cohens_d, sei = SE_d, data = filtered_data, method = "REML")

# Print summary of the meta-analysis results
summary(result)

# Create a forest plot for the filtered data
forest(result, main="Forest Plot for ASD Treatments vs. Control", cex=0.8, cex.axis=0.7)

# Optional: Generate a more detailed plot using ggplot2
library(ggplot2)
dat <- with(filtered_data, data.frame(study=1:nrow(filtered_data), effect=Cohens_d, se=SE_d))
dat$lower <- with(dat, effect - 1.96 * se)
dat$upper <- with(dat, effect + 1.96 * se)

ggplot(dat, aes(y=study, xmin=lower, xmax=upper, x=effect)) +
  geom_vline(xintercept = 0, linetype="dotted") +
  geom_errorbarh(height=0, color="blue4", size=0.7) +
  geom_point(size=0.9, color="red4") +
  labs(title="Forest Plot of ASDB compared to ASDM Effect Sizes", x="Effect Size (Cohen's d)", y="Study") +
  theme_minimal()

#########################################################

library(metafor)

# Import data from CSV
data <- read.csv("C:\\Users\\GogoFifiLolous\\Desktop\\ULB 2021 - 2022\\Mémoire\\R Studio - Metafor\\Coding Guide and effect sizes.csv", header=TRUE, fileEncoding="UTF-8")

# Examine the data structure
str(data)

# Convert cohens_d to numeric
data$Cohens_d <- as.numeric(as.character(data$Cohens_d))

# Calculate the standard error of Cohen's d
data$SE_d <- sqrt((data$N1 + data$N2) / (data$N1 * data$N2) + (data$Cohens_d^2) / (2 * (data$N1 + data$N2)))

# Filter data for specified treatment and control groups
filtered_data <- data[data$Treatment %in% c("ASDB", "ASDBH", "ASDBSEQ", "ASDBSIM", "ASDBL") & data$Control == "TDB", ]

# Perform the meta-analysis on the filtered dataset
result <- rma(yi = Cohens_d, sei = SE_d, data = filtered_data, method = "REML")

# Print summary of the meta-analysis results
summary(result)

# Create a forest plot for the filtered data
forest(result, main="Forest Plot for ASD Treatments vs. Control", cex=0.8, cex.axis=0.7)

# Optional: Generate a more detailed plot using ggplot2
library(ggplot2)
dat <- with(filtered_data, data.frame(study=1:nrow(filtered_data), effect=Cohens_d, se=SE_d))
dat$lower <- with(dat, effect - 1.96 * se)
dat$upper <- with(dat, effect + 1.96 * se)

ggplot(dat, aes(y=study, xmin=lower, xmax=upper, x=effect)) +
  geom_vline(xintercept = 0, linetype="dotted") +
  geom_errorbarh(height=0, color="blue4", size=0.7) +
  geom_point(size=0.9, color="red4") +
  labs(title="Forest Plot of ASDB compared to TDB Effect Sizes", x="Effect Size (Cohen's d)", y="Study") +
  theme_minimal()
