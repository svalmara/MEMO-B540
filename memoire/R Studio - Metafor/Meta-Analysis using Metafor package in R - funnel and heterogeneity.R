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

# Perform the meta-analysis
results_combined <- rma(yi = Cohens_d, sei = SE_d,
                             data = data[data$Treatment %in% c("ASDB", "ASDBH", "ASDBSEQ", "ASDBSIM", "ASDBL") & data$Control == "ASDM",], 
                             method = "REML")

# Print summary of the meta-analysis results
summary(results_combined)

# Create a forest plot of the meta-analysis results
forest(results_combined)

# Create a funnel plot to check for publication bias
funnel(results_combined)

# Perform a test for funnel plot asymmetry (Egger's test)
regtest(results_combined, model="lm")


