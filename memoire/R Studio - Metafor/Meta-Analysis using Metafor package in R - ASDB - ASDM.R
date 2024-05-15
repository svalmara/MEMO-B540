############################################################################

# install the metafor package

install.packages("metafor")

# load the metafor package
library(metafor)

# Import data from CSV
data <- read.csv("C:\\Users\\GogoFifiLolous\\Desktop\\ULB 2021 - 2022\\Mémoire\\R Studio - Metafor\\Coding Guide and effect sizes.csv", header=TRUE, fileEncoding="UTF-8")

# Data Structure
str(data)

# Convert cohens_d to numeric
data$Cohens_d <- as.numeric(as.character(data$Cohens_d))

# Calculating the standard error of Cohen's d
data$SE_d <- sqrt((data$N1 + data$N2) / (data$N1 * data$N2) + (data$Cohens_d^2) / (2 * (data$N1 + data$N2)))

############################################################################


# Filter by group all types of ASDB versus asdm - results_asdb_combined#

results_asdb_combined <- rma(yi = Cohens_d, sei = SE_d,
                             data = data[data$Treatment %in% c("ASDB", "ASDBH", "ASDBSEQ", "ASDBSIM", "ASDBL") & data$Control == "ASDM",], 
                             method = "REML")
summary(results_asdb_combined) # Summarise results

# Define a specific subset for ASDB vs. asdm
subset_asdb_asdm <- data[data$Treatment %in% c("ASDB", "ASDBH", "ASDBSEQ", "ASDBSIM", "ASDBL") & data$Control == "ASDM",]

# Filter data for each outcome and run separate meta-analyses
results_cognitive <- rma(yi=Cohens_d, sei=SE_d, data=subset_asdb_asdm[subset_asdb_asdm$Cognitive == 1,], method="REML")
results_adaptive <- rma(yi=Cohens_d, sei=SE_d, data=subset_asdb_asdm[subset_asdb_asdm$Adaptive.functioning == 1,], method="REML")
results_social <- rma(yi=Cohens_d, sei=SE_d, data=subset_asdb_asdm[subset_asdb_asdm$Social.Communication == 1,], method="REML")
results_linguistic <- rma(yi=Cohens_d, sei=SE_d, data=subset_asdb_asdm[subset_asdb_asdm$Linguistic == 1,], method="REML")

# Print summary of results
summary(results_cognitive)
summary(results_adaptive)
summary(results_social)
summary(results_linguistic)


# Meta-regression with outcomes as moderators - how different outcomes affect the effect sizes for ASDB vs asdm
results_meta_reg <- rma(yi=Cohens_d, sei=SE_d, mods = ~ Cognitive + Adaptive.functioning + Social.Communication + Linguistic, data=subset_asdb_asdm, method="REML")
summary(results_meta_reg)

# Forest plots for each analysis
forest(results_cognitive, main="Effects on Cognitive Outcomes")
forest(results_adaptive, main="Effects on Adaptive Functioning")
forest(results_social, main="Effects on Social-Communication")
forest(results_linguistic, main="Effects on Linguistic Outcomes")

###
# Combining study identifiers into a single column
subset_asdb_asdm$StudyLabel <- paste(subset_asdb_asdm$ID, subset_asdb_asdm$year, subset_asdb_asdm$Outcome.term, sep="        ")

#################### Cognitive ####################
# Running the meta-analysis (for cognitive outcomes)
results_cognitive <- rma(yi=Cohens_d, sei=SE_d, data=subset_asdb_asdm[subset_asdb_asdm$Cognitive == 1,], method="REML")

##
# Filter the data for cognitive outcomes
cognitive_data <- subset_asdb_asdm[subset_asdb_asdm$Cognitive == 1,]

# Ensure that the labels correspond only to the filtered data
cognitive_data$StudyLabel <- paste(cognitive_data$ID, cognitive_data$year, cognitive_data$Outcome.term, sep="        ")

# Run the meta-analysis for cognitive outcomes
results_cognitive <- rma(yi=Cohens_d, sei=SE_d, data=cognitive_data, method="REML")

# Check summary for any potential issues
summary(results_cognitive)

# Generate the forest plot with matching labels
forest(results_cognitive, main="Effects on Cognitive Outcomes", slab=cognitive_data$StudyLabel, cex=0.5)

#################### Adaptive Functioning ######################

# Running the meta-analysis
results_Adaptive.functioning <- rma(yi=Cohens_d, sei=SE_d, data=subset_asdb_asdm[subset_asdb_asdm$Adaptive.functioning == 1,], method="REML")

# Filter the data for cognitive outcomes
Adaptive.functioning_data <- subset_asdb_asdm[subset_asdb_asdm$Adaptive.functioning == 1,]

# Ensure that the labels correspond only to the filtered data
Adaptive.functioning_data$StudyLabel <- paste(Adaptive.functioning_data$ID, Adaptive.functioning_data$year, Adaptive.functioning_data$Outcome.term, sep="        ")

# Run the meta-analysis for cognitive outcomes
results_Adaptive.functioning <- rma(yi=Cohens_d, sei=SE_d, data=Adaptive.functioning_data, method="REML")

# Check summary for any potential issues
summary(results_Adaptive.functioning)

# Generate the forest plot with matching labels
forest(results_Adaptive.functioning, main="Effects on Adaptive Functioning Outcomes", slab=Adaptive.functioning_data$StudyLabel, cex=0.5)

#################### Social Communication ######################

# Running the meta-analysis
results_Social.Communication <- rma(yi=Cohens_d, sei=SE_d, data=subset_asdb_asdm[subset_asdb_asdm$Social.Communication == 1,], method="REML")

# Filter the data for cognitive outcomes
Social.Communication_data <- subset_asdb_asdm[subset_asdb_asdm$Social.Communication == 1,]

# Ensure that the labels correspond only to the filtered data
Social.Communication_data$StudyLabel <- paste(Social.Communication_data$ID, Social.Communication_data$year, Social.Communication_data$Outcome.term, sep="        ")

# Run the meta-analysis for cognitive outcomes
results_Social.Communication <- rma(yi=Cohens_d, sei=SE_d, data=Social.Communication_data, method="REML")

# Check summary for any potential issues
summary(results_Social.Communication)

# Generate the forest plot with matching labels
forest(results_Social.Communication, main="Effects on Social Communication Outcomes", slab=Social.Communication_data$StudyLabel, cex=0.5)

#################### Linguistic ######################

# Running the meta-analysis
results_Linguistic <- rma(yi=Cohens_d, sei=SE_d, data=subset_asdb_asdm[subset_asdb_asdm$Linguistic == 1,], method="REML")

# Filter the data for cognitive outcomes
Linguistic_data <- subset_asdb_asdm[subset_asdb_asdm$Linguistic == 1,]

# Ensure that the labels correspond only to the filtered data
Linguistic_data$StudyLabel <- paste(Linguistic_data$ID, Linguistic_data$year, Linguistic_data$Outcome.term, sep="        ")

# Run the meta-analysis for cognitive outcomes
results_Linguistic <- rma(yi=Cohens_d, sei=SE_d, data=Linguistic_data, method="REML")

# Check summary for any potential issues
summary(results_Linguistic)

# Generate the forest plot with matching labels
forest(results_Linguistic, main="Effects on Linguistic Outcomes", slab=Linguistic_data$StudyLabel, cex=0.5)
