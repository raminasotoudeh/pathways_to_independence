# Final Analyses for Pathways to Independence Study
# This script performs the main analyses for the pathways to independence research
# using PSID longitudinal data to examine transitions to adulthood

rm(list = ls())

# Load required packages
library(igraph)
library(reshape2)
library(dplyr)
library(reshape2)
library(ggplot2)
library(tidyr)
library(broom)
library(nnet)
library(survival)
library(coxme)


# Read in 2005-2019
data = readRDS('psid_long_with_ta_and_ind_v8_w_cds.RDS')

# Fix UID
data$psid68_UID = paste0(as.numeric(data$psid68_FID), "_", as.numeric(data$psid68_IID))

# Add covariates
data = data %>% mutate(fam_income_quintiles = ntile(average_parental_income, 5))
data$fam_income_quintiles = paste0("Q", data$fam_income_quintiles)
data$fam_income_quintiles = factor(data$fam_income_quintiles, levels = c("Q1", "Q2", "Q3", "Q4", "Q5"))

data = data %>% mutate(wealth_quintiles = ntile(average_parental_wealth_w_equity, 5))
data$wealth_quintiles = paste0("Q", data$wealth_quintiles)
data$wealth_quintiles = factor(data$wealth_quintiles, levels = c("Q1", "Q2", "Q3", "Q4", "Q5"))

data$age = ifelse(data$year == 2005, data$ER33804_age_of_individual_2005, NA)
data$age = ifelse(data$year == 2007, data$ER33904_age_of_individual_2007, data$age)
data$age = ifelse(data$year == 2009, data$ER34004_age_of_individual_2009, data$age)
data$age = ifelse(data$year == 2011, data$ER34104_age_of_individual_2011, data$age)
data$age = ifelse(data$year == 2013, data$ER34204_age_of_individual_2013, data$age)
data$age = ifelse(data$year == 2015, data$ER34305_age_of_individual_2015, data$age)
data$age = ifelse(data$year == 2017, data$ER34504_age_of_individual_2017, data$age)
data$age = ifelse(data$year == 2019, data$ER34704_age_of_individual_2019, data$age)
data$age = ifelse(data$year == 2021, data$ER34904_age_of_individual_2021, data$age)
data$age = as.numeric(data$age)
data$age = ifelse(data$age == 16, NA, data$age)

## mean imutation for gap
mean_value <- mean(data$standardized_gpa, na.rm = TRUE)
data$standardized_gpa_imp = data$standardized_gpa
data$standardized_gpa_imp[is.na(data$standardized_gpa_imp)] <- mean_value

# Imputation for earnings from work
data$earnings_work_log_imp = data$earnings_work_log
data$earnings_work_log_imp = ifelse(is.na(data$earnings_work_log_imp), 0, data$earnings_work_log_imp)

# Convert age to factor with 17 as reference
data$age_factor <- relevel(as.factor(data$age), ref = "17")

# Feels close to family members
data$kid_feels_close_mom = ifelse(data$kid_feels_close_mom == 0, NA, data$kid_feels_close_mom)
data$kid_feels_close_dad = ifelse(data$kid_feels_close_dad == 0, NA, data$kid_feels_close_dad)
data$kid_feels_close_stepmom = ifelse(data$kid_feels_close_stepmom == 0, NA, data$kid_feels_close_stepmom)
data$kid_feels_close_stepdad = ifelse(data$kid_feels_close_stepdad == 0, NA, data$kid_feels_close_stepdad)
data$kid_feels_close_parents = rowMeans(data[,c("kid_feels_close_mom",                                
                                              "kid_feels_close_dad",                                 
                                              "kid_feels_close_stepmom",                             
                                              "kid_feels_close_stepdad")], na.rm = T)

# Parents currently married as inverse of divorced
data$parents_currently_married = 1-data$currently_divorced

# Construct inputs for seq analysis
# 1.	In school, single
data$SCH = ifelse(data$in_school_or_coll == 1 , 1, 0)

# 3.	NW NS, single
data$NWS = ifelse(data$no_school_no_work == 1, 1, 0)

# 5.	Working, single
data$W = ifelse(data$working_now_no_school == 1 , 1, 0)

# 6.	Working, married/cohab
data$M = ifelse(data$married_binary == 1 , 1, 0)

data$C = ifelse(data$cohab_binary == 1 , 1, 0)

data$S = ifelse(data$marriage_cohabitation_binary == 0 , 1, 0)


# Categorical state school work
school.states = c("SCH", "NWS", "W")

# State for school
data$final_school_state = NA
for ( i in school.states ) {
  data$final_school_state = ifelse(data[,i] == 1, i, data$final_school_state)
}

# Categorical state for marriage cohabitation
marital.states = c("M", "C", "S")

data$final_marital_state = NA
for ( i in marital.states ) {
  data$final_marital_state = ifelse(data[,i] == 1, i, data$final_marital_state)
}

# Independence
# 1.	In school, single, totally independent 
data$TI = ifelse(data$totally_ind == 1, 1, 0)

# 2.	In school, single, F dependent, R independent
data$FS = ifelse(data$F_dep_R_ind == 1, 1, 0)

# 3.	In school, single, F independent, R dependent
data$RS = ifelse(data$F_ind_R_dep == 1, 1, 0)

# 4.	In school, single, totally dependent 
data$RS_FS = ifelse(data$totally_dep == 1, 1, 0)

# Categorical state for independence
ind.states = c("TI", "FS", "RS", "RS_FS")

data$final_ind_state = NA
for ( i in ind.states ) {
  data$final_ind_state = ifelse(data[,i] == 1, i, data$final_ind_state)
}

# Transpose data from long to wide
wide_data = data[,c("psid68_UID", "year", "final_ind_state", "final_marital_state", "final_school_state")]
wide_data = wide_data[order(wide_data$year),]
wide_data = reshape(wide_data, idvar = "psid68_UID", timevar = "year", direction = "wide", sep = "_")

# Grab first year that each person shows up in TAS
library(dplyr)
first_year <- data %>%
  group_by(psid68_UID) %>%
  summarise(join_year = min(year)) %>%
  ungroup()

# Grab last year that each person shows up in TAS
last_year <- data %>%
  group_by(psid68_UID) %>%
  summarise(end_year = max(year)) %>%
  ungroup()

# Check counts of first year
join_counts <- first_year %>%
  group_by(join_year) %>%
  summarise(count = n()) %>%
  ungroup()

# Add first and last years to wide data
wide_data$join_year = first_year$join_year[match(wide_data$psid68_UID, first_year$psid68_UID)]
wide_data$end_year = last_year$end_year[match(wide_data$psid68_UID, last_year$psid68_UID)]

# Create binary vars for whether a person was in TAS anytime during recession or pandemic years
# Extended recession indicator to include onset (2007) and peak (2008)
# Anyone active during 2007 OR 2008
wide_data$recession_year = ifelse(wide_data$join_year <= 2009, 1, 0)
wide_data$pandemic_year = ifelse(wide_data$join_year >= 2015, 1, 0)

# Subset the data to include only complete sequences (5 waves in the data)
complete = subset(wide_data, join_year %in% c(2005, 2007, 2009, 2011, 2013))

# Grab all sequence states across three domains
mar_cols = c(paste0("final_marital_state_", seq(2005, 2021, by = 2)))
sch_cols = c(paste0("final_school_state_", seq(2005, 2021, by = 2)))
ind_cols = c(paste0("final_ind_state_", seq(2005, 2021, by = 2)))

# Subset wide data for seq analysis
wide_data_mar = complete[,c("psid68_UID", mar_cols)]
wide_data_sch = complete[,c("psid68_UID",sch_cols)]
wide_data_ind = complete[,c("psid68_UID",ind_cols)]

# Find people who have empty sequences in the three domains
empty_sequences_mar = which(rowSums(is.na(wide_data_mar[,mar_cols])) == 9)
empty_sequences_sch = which(rowSums(is.na(wide_data_sch[,sch_cols])) == 9)
empty_sequences_ind = which(rowSums(is.na(wide_data_ind[,ind_cols])) == 9)

# Combine all empty identifiers into one vector that identifies people with empty states in any of the domains
empty_sequences = unique(c(empty_sequences_mar, empty_sequences_sch, empty_sequences_ind))

# Drop those people across all domains
for_seq_data_mar = wide_data_mar[-empty_sequences,]
for_seq_data_sch = wide_data_sch[-empty_sequences,]
for_seq_data_ind = wide_data_ind[-empty_sequences,]

# Align sequences so that we effectively ignore year
align_sequences = function(seq_data) {
  aligned = list()
  # Loop through data
  for ( i in 1:nrow(seq_data)){
    # Grab person
    person = seq_data[i,]
    # Grab states
    person_states = person[,grep('final_', colnames(person))]
    # See what waves they have data for
    viable_waves = which(!is.na(person_states))
    # Eligible waves
    all_waves = seq(from = 2005, to = 2021, by = 2)
    # First state is minimum wave they are in data
    first_state = min(viable_waves)
    start_state_year = all_waves[first_state]
    # Last state is maximum wave they are in data
    last_state = max(viable_waves)
    end_state_year = all_waves[last_state]
    # Possible set of states
    all_states = first_state:last_state
    # Total number of waves they are in data
    total_states = length(all_states)
    # Data set to save results
    person_aligned = data.frame(psid68_UID = person$psid68_UID, state1 = NA, state2 = NA, state3 = NA,
                                state4 = NA, state5 = NA, state6 = NA, state7 = NA, state8 = NA, 
                                state1_year = NA, state2_year = NA,  state3_year = NA,
                                state4_year = NA, state5_year = NA, state6_year = NA, state7_year = NA, state8_year = NA, 
                                state9_year = NA)
    # Loop through and assign results to data
    for ( j in 1:total_states ) {
      person_aligned[,paste0('state', j)] = person_states[all_states[j]]
      person_aligned[,paste0('state', j, '_year')] = all_waves[all_states[j]]
    }
    person_aligned$total_states = total_states
    person_aligned$start_wave = start_state_year
    person_aligned$end_wave = end_state_year
    
    aligned[[length(aligned) + 1]] = person_aligned
  }
  # bind all the results into a new data frame and return
  aligned_sequences = do.call("rbind", aligned)
  return(aligned_sequences)
}

# Apply to each domain
aligned_mar = align_sequences(for_seq_data_mar)
aligned_sch = align_sequences(for_seq_data_sch)
aligned_ind = align_sequences(for_seq_data_ind)

# Data is ready for sequency analysis


library(TraMineR) # Load in package for sequence analysis

# Limit to first five states (most people are only in five waves of the data or less)
mar_to_seq = aligned_mar[,c('state1', 'state2', 'state3', 'state4', 'state5')]
sch_to_seq = aligned_sch[,c('state1','state2', 'state3', 'state4', 'state5')]
ind_to_seq = aligned_ind[,c('state1','state2', 'state3', 'state4', 'state5')]

# Find people which are not missing sequences
to_keep = which(rowSums(is.na(mar_to_seq)) == 0 & rowSums(is.na(sch_to_seq)) == 0 & rowSums(is.na(ind_to_seq)) == 0)

# Keep only those people
mar_to_seq_nona = mar_to_seq[to_keep,]
sch_to_seq_nona = sch_to_seq[to_keep,]
ind_to_seq_nona = ind_to_seq[to_keep,]

# Project everyone who are missing 1 or 2 states in any domain 
to_project = which(rowSums(is.na(mar_to_seq)) <= 2 & rowSums(is.na(mar_to_seq)) <= 2 & rowSums(is.na(mar_to_seq)) <= 2)
to_project = to_project[!to_project %in% to_keep]

mar_to_seq_project = mar_to_seq[to_project,]
sch_to_seq_project = sch_to_seq[to_project,]
ind_to_seq_project = ind_to_seq[to_project,]

# Define sequences for the complete cases
marital.seq = seqdef(mar_to_seq_nona)
school.seq = seqdef(sch_to_seq_nona)
ind.seq = seqdef(ind_to_seq_nona)

# Visualize       
par(mfrow=c(1,1))
seqdplot(marital.seq, xtlab = c('Wave 1', 'Wave 2', 'Wave 3', 'Wave 4', 'Wave 5'), 
         ylab = "Relative Frequency", cpal = c("#8b4513", "#fbe3c2", "#9db88f"),
         cex.lab = 1.5,
         cex.axis = 1.4,
         las = 1,
         mgp = c(2.5, 1, 0),
         border = TRUE,
         lwd = 0.5, line = 3.2)

seqdplot(school.seq, xtlab = c('Wave 1', 'Wave 2', 'Wave 3', 'Wave 4', 'Wave 5'), 
         ylab = "Relative Frequency", cpal = c("#ffb3ba", "#5d4e75", "#a8dadc"),
         cex.lab = 1.5,
         cex.axis = 1.4,
         las = 1,
         mgp = c(2.5, 1, 0),
         border = TRUE,
         lwd = 0.5, line = 3.2)

par(mar=c(5, 6, 4, 2) + 0.1)
seqdplot(ind.seq, xtlab = c('Wave 1', 'Wave 2', 'Wave 3', 'Wave 4', 'Wave 5'), 
         ylab = "Relative Frequency", cpal = c("#a84532", "#208cc0", "#f1af3a", "#cf5e4e" ), 
         cex.lab = 1.5,
         cex.axis = 1.4,
         las = 1,
         mgp = c(2.5, 1, 0),
         border = TRUE,
         lwd = 0.5, line = 3.2)

      

# Begin multi-channel sequence analysis
channels = list(School= school.seq, Marr= marital.seq, Ind=ind.seq)

# Calculate distace metric across all channels
mcdist <- seqdistmc(channels, method="OM", sm =list("TRATE", "TRATE", "TRATE"), what = "diss")

## BRIEF DETOUR ##
# Prepare for dyadic sibling models 
rownames(mcdist) = aligned_mar$psid68_UID[to_keep]
colnames(mcdist) = aligned_mar$psid68_UID[to_keep]

library(reshape2)
melted_mcdist = melt(mcdist)
colnames(melted_mcdist) = c('person1', 'person2', 'distance')
## DETOUR ENDS ##


# Cluster distance matrix to identify sequence clusters
library(cluster)
biofam.clusterward = agnes(mcdist, diss = T, method = "ward")

# Choose best clustering solution
plot(sort(biofam.clusterward$height, decreasing=TRUE)[1:20], type="s", xlab="Number of clusters", ylab="Inertia")

# More metrics to evaluate clustering solutions
library(WeightedCluster)
wardRange <- as.clustrange(biofam.clusterward, diss=as.dist(mcdist))
summary(wardRange, max.rank=2)

# Cut at 3 groups based on above metrics
biofam.c3 <- cutree(biofam.clusterward, k = 3)

# Visualizations by group

seqdplot(marital.seq, group=biofam.c3, xtlab = c('Wave 1', 'Wave 2', 'Wave 3', 'Wave 4', 'Wave 5'), 
         ylab = "Relative Frequency", cpal = c("#8b4513", "#fbe3c2", "#9db88f"),
         cex.lab = 1.5,
         cex.axis = 1.4,
         las = 1,
         mgp = c(2.5, 1, 0),
         border = TRUE,
         lwd = 0.5, line = 3.2)

seqdplot(school.seq, group=biofam.c3, xtlab = c('Wave 1', 'Wave 2', 'Wave 3', 'Wave 4', 'Wave 5'), 
         ylab = "Relative Frequency", cpal = c("#ffb3ba", "#5d4e75", "#a8dadc"),
         cex.lab = 1.5,
         cex.axis = 1.4,
         las = 1,
         mgp = c(2.5, 1, 0),
         border = TRUE,
         lwd = 0.5, line = 3.2)

par(mar=c(5, 6, 4, 2) + 0.1)
seqdplot(ind.seq, group=biofam.c3, xtlab = c('Wave 1', 'Wave 2', 'Wave 3', 'Wave 4', 'Wave 5'), 
         ylab = "Relative Frequency", cpal = c("#a84532", "#208cc0", "#f1af3a", "#cf5e4e" ),
         cex.lab = 1.5,
         cex.axis = 1.4,
         las = 1,
         mgp = c(2.5, 1, 0),
         border = TRUE,
         lwd = 0.5, line = 3.2)


# Color scheme for reference
color_scheme = data.frame(state = c("FS", "RS", "RS_FS", "TI",
                                    "C", "M", "S",
                                    "NWS", "SCH", "W"), 
                          color = c("#a84532", "#208cc0", "#f1af3a", "#cf5e4e" ,
                                    "#8b4513", "#fbe3c2", "#9db88f",
                                    "#ffb3ba", "#5d4e75", "#a8dadc"))

# PROJECTION

# Create a simple data set of non-projected people to their assigned clusters
nonprojected_individuals = aligned_mar[to_keep,]
nonprojected_individuals$seq_cluster_3 = biofam.c3
to_match = nonprojected_individuals[,c('psid68_UID', 'seq_cluster_3')]

# Define sequences for everybody including people who need to be projected
marital.seq.all = seqdef(mar_to_seq)
school.seq.all = seqdef(sch_to_seq)
ind.seq.all = seqdef(ind_to_seq)

# Calculate distance metric
channels.all = list(School= school.seq.all, Marr= marital.seq.all, Ind=ind.seq.all)
mcdist.all <- seqdistmc(channels.all, method="OM", sm =list("TRATE", "TRATE", "TRATE"), what = "diss")

# Set row and column names
rownames(mcdist.all) = aligned_mar$psid68_UID
colnames(mcdist.all) = aligned_mar$psid68_UID

# Make distance matrix long form for easier use
melted_mcdist_all = melt(mcdist.all)
colnames(melted_mcdist_all) = c('person1', 'person2', 'distance')

# Assign cluster of ego and alter
melted_mcdist_all$cluster_ego = to_match$seq_cluster_3[match(melted_mcdist_all$person1, to_match$psid68_UID)]
melted_mcdist_all$cluster_alter = to_match$seq_cluster_3[match(melted_mcdist_all$person2, to_match$psid68_UID)]

# Limit to rows where ego is missing (i.e. needs to be projected) and alter is present (i.e. has a complete sequence and has been assigned a cluster)
melted_mcdist_to_project = melted_mcdist_all[melted_mcdist_all$person1 %in% aligned_mar$psid68_UID[to_project] & melted_mcdist_all$person2 %in% aligned_mar$psid68_UID[to_keep],]

# For each person requiring projection, calculate their average distance to people in each cluster
avg_cluster_dists = aggregate(distance ~ person1 + cluster_alter, data = melted_mcdist_to_project, FUN = function(x) mean(x))

# Find the cluster that they have the smallest average distance to and assign as their cluster
smallest_dist = avg_cluster_dists %>% 
  group_by(person1) %>% 
  arrange(desc(distance)) %>% 
  top_n(-1)

# Create simple data set of projected people to their projected clusters
projected_individuals = smallest_dist[,c('person1', 'cluster_alter')]
colnames(projected_individuals) = c('psid68_UID', 'seq_cluster_3')

# Set type to projected (for diagnostics and so we have record)
projected_individuals$type = 'projected'

# Set type to fit (for diagnostics and so we have record)
to_match$type = 'fit'

# Combine both projected and non-projected individuals into a single data set for later
final_for_regs = rbind(to_match, projected_individuals[!projected_individuals$psid68_UID %in% to_match$psid68_UID,])

# Read in LCA
LCA_results = readRDS("LCA_results_06_03_24.RDS")

# Add in LCA results to final_for_regs data set
final_for_regs$LCA_TwoClass = LCA_results$TwoClass[match(final_for_regs$psid68_UID, LCA_results$UID_2)]
final_for_regs$LCA_ThreeClass = LCA_results$ThreeClass[match(final_for_regs$psid68_UID, LCA_results$UID_2)]

# Make LCA and seq cluster variables factors and give each level an interpretable name
final_for_regs$LCA_ThreeClass_Cat = c('Diligents', 'Strugglers', 'Middles')[final_for_regs$LCA_ThreeClass ]
final_for_regs$seq_cluster_cat = c('School Dependents', 'RD Workers', 'Independence via marriage')[final_for_regs$seq_cluster_3]

# Look at cross-tabs between LCA and seq clusters
# Cross-tab post-projection
round(prop.table(table(final_for_regs$LCA_ThreeClass_Cat, final_for_regs$seq_cluster_cat), 2), 2)
round(prop.table(table(final_for_regs$LCA_ThreeClass_Cat, final_for_regs$seq_cluster_cat), 1), 2)

# Only fitted individuals
final_for_regs_fit = subset(final_for_regs, type == 'fit')
round(prop.table(table(final_for_regs_fit$LCA_ThreeClass_Cat, final_for_regs_fit$seq_cluster_cat), 1), 2)

## REGRESSIONS
# Match clusters into main data

# We already loaded in main data
psid_long_all = data

# Need to construct fam income quintiles (forgot above)
psid_long_all = psid_long_all %>% mutate(fam_income_quintiles = ntile(average_parental_income, 5))
psid_long_all$fam_income_quintiles = paste0("Q", psid_long_all$fam_income_quintiles)
psid_long_all$fam_income_quintiles = factor(psid_long_all$fam_income_quintiles, levels = c("Q1", "Q2", "Q3", "Q4", "Q5"))

# Match in clusters
psid_long_all$seq_cluster_3 = final_for_regs$seq_cluster_3[match(psid_long_all$psid68_UID, final_for_regs$psid68_UID)]

# Create binaries
psid_long_all$cluster_1 = ifelse(psid_long_all$seq_cluster_3 == 1, 1, 0)
psid_long_all$cluster_2 = ifelse(psid_long_all$seq_cluster_3 == 2, 1, 0)
psid_long_all$cluster_3 = ifelse(psid_long_all$seq_cluster_3 == 3, 1, 0)

# Match in LCA
psid_long_all$LCA_ThreeClass_Cat = final_for_regs$LCA_ThreeClass_Cat[match(psid_long_all$psid68_UID, final_for_regs$psid68_UID)]

# Create LCA binaries
psid_long_all$Strugglers = ifelse(psid_long_all$LCA_ThreeClass_Cat == "Strugglers", 1, 0)
psid_long_all$Diligents = ifelse(psid_long_all$LCA_ThreeClass_Cat == "Diligents", 1, 0)
psid_long_all$Middles = ifelse(psid_long_all$LCA_ThreeClass_Cat == "Middles", 1, 0)

# Read in sibling data
psid_long_sib = readRDS("psid_long_with_ta_and_ind_v9.RDS")

# Income quintiles again
psid_long_sib = psid_long_sib %>% mutate(fam_income_quintiles = ntile(average_parental_income, 5))
psid_long_sib$fam_income_quintiles = paste0("Q", psid_long_sib$fam_income_quintiles)
psid_long_sib$fam_income_quintiles = factor(psid_long_sib$fam_income_quintiles, levels = c("Q1", "Q2", "Q3", "Q4", "Q5"))

# Match in UIDs and clusters
psid_long_sib$psid68_UID = paste0(as.numeric(psid_long_sib$psid68_FID), '_',  as.numeric(psid_long_sib$psid68_IID))
psid_long_sib$seq_cluster_3 = final_for_regs$seq_cluster_3[match(psid_long_sib$psid68_UID, final_for_regs$psid68_UID)]

# Binarize
psid_long_sib$cluster_1 = ifelse(psid_long_sib$seq_cluster_3 == 1, 1, 0)
psid_long_sib$cluster_2 = ifelse(psid_long_sib$seq_cluster_3 == 2, 1, 0)
psid_long_sib$cluster_3 = ifelse(psid_long_sib$seq_cluster_3 == 3, 1, 0)

# LCA
psid_long_sib$LCA_ThreeClass_Cat = final_for_regs$LCA_ThreeClass_Cat[match(psid_long_sib$psid68_UID, final_for_regs$psid68_UID)]

# Binarize
psid_long_sib$Strugglers = ifelse(psid_long_sib$LCA_ThreeClass_Cat == "Strugglers", 1, 0)
psid_long_sib$Diligents = ifelse(psid_long_sib$LCA_ThreeClass_Cat == "Diligents", 1, 0)
psid_long_sib$Middles = ifelse(psid_long_sib$LCA_ThreeClass_Cat == "Middles", 1, 0)

# DATA IS READY FOR REGRESSIONS

###########################
# MULTINOMIAL LOGISTIC REG:
###########################

# Select relevant between family vars
for_reg = psid_long_all[,c('psid68_UID', 'age_factor', 'female', 'race_cat', 
           'mother_edu', 'parents_currently_married', 'number_children_mother', 
            'fam_income_quintiles', 'wealth_quintiles', 'metro_psid', 
            'in_college_now', 'earnings_work_log_imp', 'working_now', 
            'marriage_cohabitation_cat', 'health_level', 'year',
            'seq_cluster_3', 'cluster_1', 'cluster_2', 'cluster_3')]

# Drop people with missing data
for_reg = na.omit(for_reg)

# Use model.matrix to turn all of the factors into binaries at once
for_reg = cbind(for_reg, model.matrix( ~ fam_income_quintiles - 1 + race_cat - 1 + age_factor - 1 + wealth_quintiles -1 + marriage_cohabitation_cat-1, data=for_reg ))

# Data is long, but here we're predicting sequence outcome (which is known only at the end of their time in TAS)
# So limit to just their first year in the TAS so that the covariates are preceding the realization of their sequence cluster
for_reg = for_reg %>%
  group_by(psid68_UID) %>%
  filter(year == min(year)) %>%
  ungroup()


# Standardize variables
for_reg[,c('female', 'mother_edu', 'parents_currently_married', 'number_children_mother', 
          'metro_psid', 'in_college_now', 'earnings_work_log_imp', 'working_now', 
          'marriage_cohabitation_catmarried or cohab', 'health_level', 'year',
          "fam_income_quintilesQ1", "fam_income_quintilesQ2", "fam_income_quintilesQ3", "fam_income_quintilesQ4", "fam_income_quintilesQ5",
          "race_catAsian", "race_catBlack", "race_catHispanic", "race_catOther", "age_factor18", "age_factor19", "age_factor20", "age_factor21",
          "age_factor22", "age_factor23", "age_factor24", "age_factor25", "age_factor26", "age_factor27", "age_factor28",
          "wealth_quintilesQ2", "wealth_quintilesQ3", "wealth_quintilesQ4", "wealth_quintilesQ5")] = sapply(for_reg[,c('female', 
           'mother_edu', 'parents_currently_married', 'number_children_mother', 
          'metro_psid', 'in_college_now', 'earnings_work_log_imp', 'working_now', 
          'marriage_cohabitation_catmarried or cohab', 'health_level', 'year',
          "fam_income_quintilesQ1", "fam_income_quintilesQ2", "fam_income_quintilesQ3", "fam_income_quintilesQ4", "fam_income_quintilesQ5",
          "race_catAsian", "race_catBlack", "race_catHispanic", "race_catOther", "age_factor18", "age_factor19", "age_factor20", "age_factor21",
          "age_factor22", "age_factor23", "age_factor24", "age_factor25", "age_factor26", "age_factor27", "age_factor28",
          "wealth_quintilesQ2", "wealth_quintilesQ3", "wealth_quintilesQ4", "wealth_quintilesQ5")], scale)

# Abbreviate sequence cluster names for ease of interpretation
abbreviated_names = c("ssfs", "rsw", "mi")
for_reg$seq_cluster_cat = c("ssfs", "rsw", "mi")[for_reg$seq_cluster_3]
for_reg$seq_cluster_cat = factor(for_reg$seq_cluster_cat, levels = abbreviated_names)

# RUN MULTINOMIAL LOGISTIC REGRESSION
for_reg$recession_year = wide_data$recession_year[match(for_reg$psid68_UID, wide_data$psid68_UID)]

library(nnet)
test_btw <- multinom(seq_cluster_cat ~  age_factor18 + age_factor19 + age_factor20 + age_factor21 + age_factor22 + age_factor23 + age_factor24 +
                                    age_factor25 + age_factor26 + age_factor27 + female + 
                                    race_catAsian + race_catBlack + race_catHispanic + race_catOther +  
                                    mother_edu + parents_currently_married + number_children_mother + 
                                    fam_income_quintilesQ2 + fam_income_quintilesQ3 + fam_income_quintilesQ4 + fam_income_quintilesQ5 + 
                                    wealth_quintilesQ2 + wealth_quintilesQ3 + wealth_quintilesQ4 + wealth_quintilesQ5 + metro_psid + 
                                    year + recession_year * wealth_quintilesQ2 + recession_year * wealth_quintilesQ3 + 
                                    recession_year * wealth_quintilesQ4 + recession_year * wealth_quintilesQ5, 
                                    data = for_reg)

# Prepare result for visualization
tt_btw <- broom::tidy(test_btw,conf.int=TRUE)
tt_btw <- dplyr::filter(tt_btw, term!="(Intercept)")
tt_btw <- dplyr::filter(tt_btw, term!="race_catOther")
tt_btw$Cluster = ifelse(tt_btw$y.level == 'mi', 'Married & Independent', 'Residentially Supported Workers')

# Data frame to fix variable names to be more sensible / visually appealing
variable_fixer = data.frame(original = c('female',
                                         'race_catAsian',
                                          'race_catBlack',
                                         'race_catHispanic',
                                         'race_catOther',
                                         'mother_edu',
                                         'parents_currently_married',
                                         'number_children_mother',
                                         paste0('fam_income_quintilesQ', 1:5),
                                         paste0('wealth_quintilesQ', 1:5),
                                         'metro_psid',
                                         'in_college_now',
                                         'earnings_work_log_imp',
                                         'working_now',
                                         'marriage_cohabitation_catmarried or cohab',
                                         'year', 
                                         'recession_year'), 
                            new = c('Female',
                                    'Race: Asian',
                                    'Race: Black',
                                    'Race: Hispanic',
                                    'Race: Other',
                                    'Maternal Education',
                                    'Parents currently married',
                                    'Num. Siblings',
                                    paste0('Income Quintile: ', 1:5),
                                    paste0('Wealth Quintile: ', 1:5),
                                    'Metro',
                                    'In college',
                                    'Earnings from work (logged)',
                                    'Currently Working',
                                    'Married or Cohabitating',
                                    'Year Entered TAS',
                                    'Recession Year'))


# match in fixed variable names
tt_btw$Variable = variable_fixer$new[match(tt_btw$term, variable_fixer$original)]

# drop age from the viz (error bars are wide and make it difficult to see results)
tt_btw_final = subset(tt_btw, !grepl('age_', tt_btw$term))

# Set the desired order for variables (customize as needed)
desired_order <- c("Year Entered TAS", "Pandemic Year", "Recession Year",
                   "Female", "Race: Asian", "Race: Black", "Race: Hispanic", 
                   "Maternal Education", "Parents currently married", 
                   "Num. Siblings", "Metro",
                   paste0('Income Quintile: ', 1:5),
                   paste0('Wealth Quintile: ', 2:5))  # Note: only 2:5 since Q1 is reference

# Apply the factor ordering
tt_btw_final$Variable <- factor(tt_btw_final$Variable, levels = desired_order)


####################################
# MULTIMONIAL LOGISTIC REGRESSIONS
####################################

library(nnet)
library(broom)
library(dplyr)
library(openxlsx)  # For Excel output

# First, create a single outcome variable with all clusters
for_reg$cluster_outcome <- NA
for_reg$cluster_outcome[for_reg$cluster_1 == 1] <- "Cluster 1"
for_reg$cluster_outcome[for_reg$cluster_2 == 1] <- "Cluster 2" 
for_reg$cluster_outcome[for_reg$cluster_3 == 1] <- "Cluster 3"

# Convert to factor (important!)
for_reg$cluster_outcome <- as.factor(for_reg$cluster_outcome)

# Center year around a meaningful value
for_reg <- for_reg %>%
  mutate(year_centered = year - 2010)

# Run multinomial logistic regression
multinom_model <- multinom(cluster_outcome ~ female + race_cat +
                             mother_edu + parents_currently_married + number_children_mother +
                             fam_income_quintiles + wealth_quintiles + metro_psid +
                             year_centered + recession_year,
                           data = for_reg)

summary(multinom_model)

# Define new cluster names
cluster_names <- c("In School & Financially Supported", "Coresident Workers", "Married & Independent")

# Function to create comprehensive results table
create_multinom_excel_table <- function(model, cluster_names, for_reg) {
  
  # Get results with Cluster 1 as reference
  results1 <- broom::tidy(model, exponentiate = TRUE, conf.int = TRUE)
  
  # Refit with Cluster 2 as reference for Cluster 3 vs Cluster 2 comparison
  for_reg_temp <- for_reg
  for_reg_temp$cluster_outcome <- relevel(for_reg_temp$cluster_outcome, ref = "Cluster 2")
  model_ref2 <- multinom(cluster_outcome ~ female + race_cat +
                           mother_edu + parents_currently_married + number_children_mother +
                           fam_income_quintiles + wealth_quintiles + metro_psid +
                           year_centered + recession_year,
                         data = for_reg_temp)
  results2 <- broom::tidy(model_ref2, exponentiate = TRUE, conf.int = TRUE)
  
  # Format function with significance stars
  format_results <- function(data, comp_name) {
    data %>%
      mutate(
        OR = round(estimate, 3),
        CI_lower = round(conf.low, 3),
        CI_upper = round(conf.high, 3),
        
        # Add significance stars
        stars = case_when(
          p.value < 0.001 ~ "***",
          p.value < 0.01 ~ "**",
          p.value < 0.05 ~ "*",
          TRUE ~ ""
        ),
        
        # OR with stars BEFORE CI, and comma instead of dash
        OR_CI = paste0(OR, stars, " (", CI_lower, ",", CI_upper, ")"),
        
        # Formatted p-value
        p_value = case_when(
          p.value < 0.001 ~ "<0.001",
          p.value < 0.01 ~ sprintf("%.3f", p.value),
          TRUE ~ sprintf("%.3f", p.value)
        ),
        comparison = comp_name
      ) %>%
      select(term, comparison, OR, CI_lower, CI_upper, OR_CI, p_value, stars)
  }
  
  # Create each comparison
  comp1 <- results1 %>%
    filter(y.level == "Cluster 2") %>%
    format_results(paste(cluster_names[2], "vs", cluster_names[1]))
  
  comp2 <- results1 %>%
    filter(y.level == "Cluster 3") %>%
    format_results(paste(cluster_names[3], "vs", cluster_names[1]))
  
  comp3 <- results2 %>%
    filter(y.level == "Cluster 3") %>%
    format_results(paste(cluster_names[3], "vs", cluster_names[2]))
  
  # Combine all comparisons
  all_results <- bind_rows(comp1, comp2, comp3)
  
  # Create wide format table for Excel
  wide_table <- all_results %>%
    select(term, comparison, OR_CI, p_value) %>%
    pivot_wider(names_from = comparison, 
                values_from = c(OR_CI, p_value),
                names_sep = "_") %>%
    # Clean up column names
    rename_with(~gsub("OR_CI_", "", .x), starts_with("OR_CI_")) %>%
    rename_with(~paste0(.x, "_p"), starts_with("p_value_"))
  
  return(list(long_format = all_results, wide_format = wide_table))
}

# Create the tables
results_tables <- create_multinom_excel_table(multinom_model, cluster_names, for_reg)

# Create Excel workbook
wb <- createWorkbook()

# Add worksheets
addWorksheet(wb, "Pairwise Comparisons")
addWorksheet(wb, "Long Format")
addWorksheet(wb, "Model Summary")

# Write wide format table (main results)
writeData(wb, "Pairwise Comparisons", results_tables$wide_format, 
          startRow = 2, startCol = 1, colNames = TRUE)

# Add title for wide format
writeData(wb, "Pairwise Comparisons", 
          "Multinomial Logistic Regression: Pairwise Comparisons (OR with 95% CI)", 
          startRow = 1, startCol = 1)

# Write long format table
writeData(wb, "Long Format", results_tables$long_format, 
          startRow = 2, startCol = 1, colNames = TRUE)

# Add title for long format
writeData(wb, "Long Format", 
          "Multinomial Logistic Regression: All Results (Long Format)", 
          startRow = 1, startCol = 1)

# Calculate model fit statistics (CLEANED UP - no duplicates)
null_model <- multinom(cluster_outcome ~ 1, data = for_reg)
mcfadden_r2 <- 1 - (logLik(multinom_model) / logLik(null_model))

# Calculate LR statistics safely
lr_chisq <- 2 * (as.numeric(logLik(multinom_model)) - as.numeric(logLik(null_model)))
lr_df <- length(coef(multinom_model)) * ncol(coef(multinom_model)) - ncol(coef(multinom_model))
lr_pvalue <- if(lr_chisq > 0 & lr_df > 0) {
  sprintf("%.3f", 1 - pchisq(lr_chisq, df = lr_df))
} else {
  "N/A"
}

# Simpler model summary to avoid the error
model_summary <- data.frame(
  Metric = c("Reference Category", 
             "Sample Size", 
             "Number of Predictors",
             "AIC", 
             "BIC",
             "Log-Likelihood"),
  Value = c(cluster_names[1],
            nrow(multinom_model$fitted.values),
            length(coef(multinom_model)[1,]),
            round(AIC(multinom_model), 2),
            round(BIC(multinom_model), 2),
            round(as.numeric(logLik(multinom_model)), 2))
)

# ADD THESE MISSING LINES:
writeData(wb, "Model Summary", model_summary, 
          startRow = 2, startCol = 1, colNames = TRUE)

writeData(wb, "Model Summary", 
          "Model Information", 
          startRow = 1, startCol = 1)

# Format the Excel file
bold_style <- createStyle(textDecoration = "bold")
addStyle(wb, "Pairwise Comparisons", bold_style, rows = 1:2, cols = 1:ncol(results_tables$wide_format), gridExpand = TRUE)
addStyle(wb, "Long Format", bold_style, rows = 1:2, cols = 1:ncol(results_tables$long_format), gridExpand = TRUE)
addStyle(wb, "Model Summary", bold_style, rows = 1:2, cols = 1:2, gridExpand = TRUE)

# Auto-size columns
setColWidths(wb, "Pairwise Comparisons", cols = 1:ncol(results_tables$wide_format), widths = "auto")
setColWidths(wb, "Long Format", cols = 1:ncol(results_tables$long_format), widths = "auto")
setColWidths(wb, "Model Summary", cols = 1:2, widths = "auto")

# Save the Excel file
saveWorkbook(wb, "multinomial_regression_results.xlsx", overwrite = TRUE)

# Print confirmation
cat("Results saved to 'multinomial_regression_results.xlsx'\n")
cat("The file contains three sheets:\n")
cat("1. 'Pairwise Comparisons': Main results in wide format\n")
cat("2. 'Long Format': All results in long format\n")
cat("3. 'Model Summary': Basic model information\n")

# Optional: Also create a simple CSV for the main results
write.csv(results_tables$wide_format, "multinomial_results_wide.csv", row.names = FALSE)
cat("Also saved main results to 'multinomial_results_wide.csv'\n")


########################
# USING MARGINAL EFFECTS
########################

library("marginaleffects")  # This is newer and more reliable than 'margins'

# Get marginal effects - this will include ALL categories
mfx <- marginaleffects(test_btw)

# Convert to data frame and plot
mfx_df <- as.data.frame(mfx)

# Calculate average marginal effects by term and group
avg_mfx <- mfx_df %>%
  group_by(term, group) %>%
  summarise(
    avg_effect = mean(estimate),
    se = sd(estimate) / sqrt(n()),
    conf.low = mean(conf.low),
    conf.high = mean(conf.high),
    .groups = 'drop'
  )

avg_mfx <- dplyr::filter(avg_mfx, term!="(Intercept)")
avg_mfx <- dplyr::filter(avg_mfx, term!="race_catOther")

avg_mfx$Cluster = 'Residentially Supported Workers'
avg_mfx$Cluster = ifelse(avg_mfx$group == 'mi', 'Married & Independent', avg_mfx$Cluster)
avg_mfx$Cluster = ifelse(avg_mfx$group == 'ssfs', 'In School & Financially Supported', avg_mfx$Cluster)

avg_mfx$Cluster = factor(avg_mfx$Cluster, levels = c('In School & Financially Supported', 'Residentially Supported Workers', 'Married & Independent'))

# Data frame to fix variable names to be more sensible / visually appealing
variable_fixer = data.frame(original = c('female',
                                         'race_catAsian',
                                          'race_catBlack',
                                         'race_catHispanic',
                                         'race_catOther',
                                         'mother_edu',
                                         'parents_currently_married',
                                         'number_children_mother',
                                         paste0('fam_income_quintilesQ', 1:5),
                                         paste0('wealth_quintilesQ', 1:5),
                                         'metro_psid',
                                         'in_college_now',
                                         'earnings_work_log_imp',
                                         'working_now',
                                         'marriage_cohabitation_catmarried or cohab',
                                         'year', 
                                         'recession_year'), 
                            new = c('Female',
                                    'Race: Asian',
                                    'Race: Black',
                                    'Race: Hispanic',
                                    'Race: Other',
                                    'Maternal Education',
                                    'Parents currently married',
                                    'Num. Siblings',
                                    paste0('Income Quintile: ', 1:5),
                                    paste0('Wealth Quintile: ', 1:5),
                                    'Metro',
                                    'In college',
                                    'Earnings from work (logged)',
                                    'Currently Working',
                                    'Married or Cohabitating',
                                    'Year Entered TAS',
                                    'Recession Year'))


# match in fixed variable names
avg_mfx$Variable = variable_fixer$new[match(avg_mfx$term, variable_fixer$original)]

# drop age from the viz (error bars are wide and make it difficult to see results)
tt_btw_final = subset(avg_mfx, !grepl('age_', avg_mfx$term))

# Set the desired order for variables (customize as needed)
desired_order <- rev(c("Female", "Race: Asian", "Race: Black", "Race: Hispanic", "Race: Other",
                   "Maternal Education", "Parents currently married", 
                   "Num. Siblings", "Metro",
                   paste0('Income Quintile: ', 2:5),
                   paste0('Wealth Quintile: ', 2:5), "Year Entered TAS", "Pandemic Year", "Recession Year"))  # Note: only 2:5 since Q1 is reference

# Apply the factor ordering
tt_btw_final$Variable <- factor(tt_btw_final$Variable, levels = desired_order)

# Create the plot
model_colors <- c("#138808", "#3E2F5B",  "#FF8C00") 

ggplot(tt_btw_final, aes(x = Variable, y = avg_effect, color = Cluster)) +
  geom_point(position = position_dodge(width = 0.6), size = 10) +  # INCREASED from 6 to 8
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), 
                width = 0.7, position = position_dodge(width = 0.6), lwd = 2.5) +  # INCREASED width from 0.9 to 1.1, lwd from 1.5 to 2
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.7) +
  coord_flip() +
  labs(title = " ",
       y = "Marginal Effect on Probability",
       x = "Predictors",
       color = " ") +
  scale_color_manual(values = model_colors) +
  scale_x_discrete(expand = expansion(add = c(0.5, 0.5))) +
  theme_minimal(base_size = 30) +
  theme(
    axis.title.x = element_text(face = "bold", size = 30, margin = margin(t = 10)),
    axis.title.y = element_text(face = "bold", size = 30, margin = margin(r = 10)),
    axis.text.y = element_text(size = 36, lineheight = 1.5),  # INCREASED from 32 to 36
    axis.text.x = element_text(size = 27),
    legend.title = element_text(face = "bold", size = 20),
    legend.text = element_text(size = 30),
    legend.position = "top",
    legend.justification = "left",
    legend.margin = margin(l = -250, r = 0, t = 0, b = 10),
    panel.grid.major = element_line(size = 0.5, linetype = 'solid', color = "grey80"),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    panel.spacing = unit(2, "lines")
  )

ggsave('between_family_cluster_predictors_all_cat.pdf',
       plot = last_plot(),
       device = "pdf",
       height = 35,
       width = 22)

#####################################
## CLUSTER MEANS DESCRIPTIVE TABLES 
#####################################

# Load required libraries
library(dplyr)
library(tidyr)
library(openxlsx)

# Convert binary 0/1 variables to factors BEFORE running the analysis

# Convert female
to_mean_df$female <- factor(to_mean_df$female, 
                            levels = c(0, 1), 
                            labels = c("Male", "Female"))

# Convert first_born  
to_mean_df$first_born <- factor(to_mean_df$first_born, 
                                levels = c(0, 1), 
                                labels = c("Not First Born", "First Born"))

# Convert parents_currently_married
to_mean_df$parents_currently_married <- factor(to_mean_df$parents_currently_married, 
                                               levels = c(0, 1), 
                                               labels = c("Not Married", "Married"))

# Convert metro_psid
to_mean_df$metro_psid <- factor(to_mean_df$metro_psid, 
                                levels = c(0, 1), 
                                labels = c("Non-Metro", "Metro"))

# Convert kid_feels_close_parents (if it's 0/1)
if(all(to_mean_df$kid_feels_close_parents %in% c(0, 1, NA))) {
  to_mean_df$kid_feels_close_parents <- factor(to_mean_df$kid_feels_close_parents, 
                                               levels = c(0, 1), 
                                               labels = c("Not Close", "Feels Close"))
}

# Convert pandemic/recession variables
to_mean_df$no_pandemic_recession <- factor(to_mean_df$no_pandemic_recession, 
                                           levels = c(0, 1), 
                                           labels = c("Experienced P/R", "No Pandemic/Recession"))

to_mean_df$pandemic_year <- factor(to_mean_df$pandemic_year, 
                                   levels = c(0, 1), 
                                   labels = c("No Pandemic", "Pandemic Year"))

to_mean_df$recession_year <- factor(to_mean_df$recession_year, 
                                    levels = c(0, 1), 
                                    labels = c("No Recession", "Recession Year"))

# Check the conversions
print("Converted variables:")
print(str(to_mean_df[c('female', 'first_born', 'parents_currently_married', 
                       'metro_psid', 'pandemic_year', 'recession_year', 
                       'no_pandemic_recession')]))

# Define all variables to analyze
all_vars <- c('fam_income_quintiles', 'wealth_quintiles', 'race_cat',
              'female', 'number_children_mother', 'mother_edu', 
              'parents_currently_married', 'health_level', 'first_born', 
              'metro_psid', 'standardized_gpa_imp', 'age', 
              'LCA_ThreeClass_Cat', 'kid_feels_close_parents', 
              'join_year', 'no_pandemic_recession', 'pandemic_year', 
              'recession_year')

# Automatically detect variable types
numeric_vars <- names(to_mean_df)[sapply(to_mean_df, is.numeric)]
numeric_vars <- numeric_vars[numeric_vars %in% all_vars]

categorical_vars <- names(to_mean_df)[sapply(to_mean_df, function(x) is.factor(x) | is.character(x))]
categorical_vars <- categorical_vars[categorical_vars %in% all_vars]

print("Numeric variables:")
print(numeric_vars)
print("Categorical variables:")
print(categorical_vars)

# Initialize results list
all_results <- list()

# Function to perform t-test and return results
analyze_numeric <- function(var_name, cluster_num) {
  reference <- to_mean_df %>% filter(seq_cluster_3 == cluster_num) %>% pull(!!var_name)
  other <- to_mean_df %>% filter(seq_cluster_3 != cluster_num) %>% pull(!!var_name)
  
  # Remove missing values
  reference <- reference[!is.na(reference)]
  other <- other[!is.na(other)]
  
  # Check if enough observations
  if(length(reference) < 2 || length(other) < 2) return(NULL)
  
  # Calculate statistics
  reference_mean <- mean(reference, na.rm = TRUE)
  reference_sd <- sd(reference, na.rm = TRUE)
  
  # T-test
  tryCatch({
    t_out <- t.test(reference, other)
    t_score <- t_out$estimate[1] - t_out$estimate[2]
    p_value <- t_out$p.value
    
    return(data.frame(
      Variable = var_name,
      Cluster = cluster_num,
      Variable_Type = "Numeric",
      Category = NA,
      Value = reference_mean,
      SD = reference_sd,
      Test_Statistic = t_score,
      P_value = p_value,
      stringsAsFactors = FALSE
    ))
  }, error = function(e) {
    return(NULL)
  })
}

# Function to analyze categorical variables
analyze_categorical <- function(var_name, cluster_num) {
  # Get data for this cluster and variable
  cluster_data <- to_mean_df %>% filter(seq_cluster_3 == cluster_num)
  other_data <- to_mean_df %>% filter(seq_cluster_3 != cluster_num)
  
  # Get the variable values
  cluster_var <- cluster_data[[var_name]]
  other_var <- other_data[[var_name]]
  
  # Remove missing values
  cluster_var <- cluster_var[!is.na(cluster_var)]
  other_var <- other_var[!is.na(other_var)]
  
  if(length(cluster_var) == 0 || length(other_var) == 0) return(NULL)
  
  # Get unique categories
  all_categories <- unique(c(cluster_var, other_var))
  
  results_list <- list()
  
  for(category in all_categories) {
    # Calculate proportions
    cluster_prop <- mean(cluster_var == category, na.rm = TRUE)
    other_prop <- mean(other_var == category, na.rm = TRUE)
    
    # Chi-square test for this category vs others
    tryCatch({
      # Create 2x2 table: this category vs others, cluster vs other clusters
      cluster_this <- sum(cluster_var == category)
      cluster_other_cats <- sum(cluster_var != category)
      other_this <- sum(other_var == category)
      other_other_cats <- sum(other_var != category)
      
      # Only do chi-square if we have sufficient counts
      if(all(c(cluster_this, cluster_other_cats, other_this, other_other_cats) >= 5)) {
        chi_table <- matrix(c(cluster_this, cluster_other_cats, 
                              other_this, other_other_cats), 
                            nrow = 2, byrow = TRUE)
        chi_test <- chisq.test(chi_table)
        p_value <- chi_test$p.value
        chi_stat <- chi_test$statistic
      } else {
        # Use Fisher's exact test for small counts
        fisher_test <- fisher.test(matrix(c(cluster_this, cluster_other_cats, 
                                            other_this, other_other_cats), 
                                          nrow = 2, byrow = TRUE))
        p_value <- fisher_test$p.value
        chi_stat <- NA
      }
      
      results_list[[length(results_list) + 1]] <- data.frame(
        Variable = var_name,
        Cluster = cluster_num,
        Variable_Type = "Categorical",
        Category = as.character(category),
        Value = cluster_prop,
        SD = NA,
        Test_Statistic = chi_stat,
        P_value = p_value,
        stringsAsFactors = FALSE
      )
    }, error = function(e) {
      # If test fails, just return proportions without test
      results_list[[length(results_list) + 1]] <- data.frame(
        Variable = var_name,
        Cluster = cluster_num,
        Variable_Type = "Categorical",
        Category = as.character(category),
        Value = cluster_prop,
        SD = NA,
        Test_Statistic = NA,
        P_value = NA,
        stringsAsFactors = FALSE
      )
    })
  }
  
  return(do.call(rbind, results_list))
}

# Analyze all numeric variables
for(var in numeric_vars) {
  for(i in 1:3) {
    result <- analyze_numeric(var, i)
    if(!is.null(result)) {
      all_results[[length(all_results) + 1]] <- result
    }
  }
}

# Analyze all categorical variables
for(var in categorical_vars) {
  for(i in 1:3) {
    result <- analyze_categorical(var, i)
    if(!is.null(result)) {
      all_results[[length(all_results) + 1]] <- result
    }
  }
}

# Combine all results
if(length(all_results) > 0) {
  final_results <- do.call(rbind, all_results)
  
  # Add cluster names
  cluster_names <- c('In School & Financially Supported', 'Residentially Supported Workers', 'Married & Independent')
  final_results$Cluster_Name <- cluster_names[final_results$Cluster]
  
  # Add significance stars
  final_results$stars <- ""
  final_results$stars[final_results$P_value <= 0.05 & !is.na(final_results$P_value)] <- "*"
  final_results$stars[final_results$P_value <= 0.01 & !is.na(final_results$P_value)] <- "**"
  final_results$stars[final_results$P_value <= 0.001 & !is.na(final_results$P_value)] <- "***"
  
  # Create variable labels
  final_results$Variable_Label <- paste0(
    final_results$Variable,
    ifelse(is.na(final_results$Category), "", paste0(": ", final_results$Category))
  )
  
  # Format values for display
  final_results$Display_Value <- ifelse(
    final_results$Variable_Type == "Numeric",
    paste0(round(final_results$Value, 3), " (", round(final_results$SD, 3), ")", final_results$stars),
    paste0(round(final_results$Value, 3), final_results$stars)
  )
  
  print("Analysis completed successfully!")
  print(paste("Total results:", nrow(final_results)))
  
} else {
  print("No results generated - check your data!")
}

# CREATE BOLD/ITALIC FORMATTING FOR HIGH/LOW VALUES
# Enhanced formatting with bold/italic instead of symbols

enhance_results_formatted <- function(final_results) {
  
  enhanced_results <- final_results %>%
    group_by(Variable, Category) %>%
    mutate(
      # Calculate overall mean across all clusters
      Overall_Mean = weighted.mean(Value, 
                                   table(to_mean_df$seq_cluster_3)[Cluster], 
                                   na.rm = TRUE),
      
      # Determine if this cluster is above or below overall mean
      Above_Mean = Value > Overall_Mean,
      
      # Create formatted display values
      Display_Value_Formatted = case_when(
        Value > Overall_Mean ~ paste0("**", Display_Value, "**"),  # Bold for higher
        Value < Overall_Mean ~ paste0("*", Display_Value, "*"),    # Italic for lower  
        TRUE ~ Display_Value                                        # Normal for average
      )
    ) %>%
    ungroup()
  
  return(enhanced_results)
}

# Apply the new formatting
enhanced_final_results_formatted <- enhance_results_formatted(final_results)

# Create wide table with bold/italic formatting
wide_table_formatted <- enhanced_final_results_formatted %>%
  select(Variable_Label, Cluster_Name, Display_Value_Formatted) %>%
  pivot_wider(names_from = Cluster_Name, values_from = Display_Value_Formatted) %>%
  rename(Variable = Variable_Label) %>%
  arrange(Variable)

# Create Excel file with actual bold/italic formatting
create_formatted_excel <- function(data, filename) {
  
  # Create workbook
  wb <- createWorkbook()
  addWorksheet(wb, "Formatted_Table")
  
  # Write the basic data first (without markdown symbols)
  basic_data <- enhanced_final_results_formatted %>%
    select(Variable_Label, Cluster_Name, Display_Value) %>%
    pivot_wider(names_from = Cluster_Name, values_from = Display_Value) %>%
    rename(Variable = Variable_Label) %>%
    arrange(Variable)
  
  writeData(wb, "Formatted_Table", basic_data)
  
  # Now apply formatting based on the enhancement data
  for(i in 1:nrow(enhanced_final_results_formatted)) {
    row_data <- enhanced_final_results_formatted[i, ]
    
    # Find the row and column in the Excel sheet
    excel_row <- which(basic_data$Variable == row_data$Variable_Label) + 1  # +1 for header
    excel_col <- which(names(basic_data) == row_data$Cluster_Name)
    
    if(length(excel_row) > 0 && length(excel_col) > 0) {
      if(row_data$Value > row_data$Overall_Mean) {
        # Make bold for higher values
        addStyle(wb, "Formatted_Table", 
                 style = createStyle(textDecoration = "bold"),
                 rows = excel_row, cols = excel_col)
      } else if(row_data$Value < row_data$Overall_Mean) {
        # Make italic for lower values  
        addStyle(wb, "Formatted_Table",
                 style = createStyle(textDecoration = "italic"),
                 rows = excel_row, cols = excel_col)
      }
    }
  }
  
  # Add detailed results sheet
  addWorksheet(wb, "Detailed_Results")
  writeData(wb, "Detailed_Results", enhanced_final_results_formatted)
  
  # Add legend
  addWorksheet(wb, "Legend")
  legend_data <- data.frame(
    Formatting = c("Bold text", "Italic text", "Normal text", "", "Statistical Tests:", "", "", "", "Significance Levels:", "", "", ""),
    Meaning = c("Higher than average across clusters", 
                "Lower than average across clusters",
                "About average",
                "",
                "Numeric: Two-sample t-tests",
                "Categorical: Chi-square or Fisher's exact tests", 
                "",
                "",
                "* p < 0.05",
                "** p < 0.01", 
                "*** p < 0.001",
                "")
  )
  writeData(wb, "Legend", legend_data)
  
  # Save workbook
  saveWorkbook(wb, filename, overwrite = TRUE)
  
  return(wb)
}

# Print the table with markdown formatting (for viewing in R)
print("=== TABLE WITH BOLD/ITALIC FORMATTING ===")
print("**text** = Bold (higher than average)")
print("*text* = Italic (lower than average)")
print("normal text = About average")
print(wide_table_formatted)

# Create Excel file with actual bold/italic formatting
create_formatted_excel(enhanced_final_results_formatted, "Formatted_Descriptive_Statistics.xlsx")

# Also create the basic wide table for reference
wide_table_basic <- final_results %>%
  select(Variable_Label, Cluster_Name, Display_Value) %>%
  pivot_wider(names_from = Cluster_Name, values_from = Display_Value) %>%
  rename(Variable = Variable_Label) %>%
  arrange(Variable)

print("\n=== BASIC TABLE (no formatting) ===")
print(wide_table_basic)

print("\nFiles created:")
print("1. 'Formatted_Descriptive_Statistics.xlsx' - With bold/italic formatting")
print("   Bold = Higher than average, Italic = Lower than average")
print("2. Use the markdown table above for copy/paste with formatting indicators")


#####################################
## SIBLING ANALYSES
#####################################

# Average sequence distance between siblings #

# Match in sibling groups to distance matrix
melted_mcdist_all$sib_group_ego = psid_long_sib$sib_group[match(melted_mcdist_all$person1, psid_long_sib$psid68_UID)]
melted_mcdist_all$sib_group_alter = psid_long_sib$sib_group[match(melted_mcdist_all$person2, psid_long_sib$psid68_UID)]

# Match in clusters
melted_mcdist_all$cluster_ego = final_for_regs$seq_cluster_3[match(melted_mcdist_all$person1, final_for_regs$psid68_UID)]
melted_mcdist_all$cluster_alter = final_for_regs$seq_cluster_3[match(melted_mcdist_all$person2, final_for_regs$psid68_UID)]

# Remove self ties
melted_mcdist_all_no_self = subset(melted_mcdist_all, person1 != person2)

# Create dyad var
melted_mcdist_all_no_self$dyad = paste0(melted_mcdist_all_no_self$person1, "_", melted_mcdist_all_no_self$person2)

# Create sibling pairs
mcdist_sibs = subset(melted_mcdist_all_no_self, sib_group_ego == sib_group_alter)

# Remove sibs from original sample
melted_mcdist_no_sibs = subset(melted_mcdist_all_no_self, !dyad %in% mcdist_sibs$dyad)

# Check distances between siblings vs. non-siblings
# Siblings have significantly more similar sequences
t.test(mcdist_sibs$distance, melted_mcdist_no_sibs$distance)

# Check likilihood of same cluster if sib vs. non-sib
prop.table(table(melted_mcdist_no_sibs$cluster_ego == melted_mcdist_no_sibs$cluster_alter))
prop.table(table(mcdist_sibs$cluster_ego == mcdist_sibs$cluster_alter))

# Mark which pairs are sibships and not
melted_mcdist_no_sibs$same_sib_group = 0
mcdist_sibs$same_sib_group = 1

# Combine
for_chi_sq = rbind(melted_mcdist_no_sibs, mcdist_sibs)
for_chi_sq$same_cluster = ifelse(for_chi_sq$cluster_ego == for_chi_sq$cluster_alter, 1, 0)

# Run Chi-Squared
chisq.test(table(for_chi_sq$same_sib_group, for_chi_sq$same_cluster))
# 59% of siblings have same cluster vs. 39% chance of sharing cluster with any other person in the data

# Add in wealth, SES, income
mcdist_sibs$income = psid_long_sib$average_parental_income[match(mcdist_sibs$person1, psid_long_sib$psid68_UID)]
mcdist_sibs$wealth = psid_long_sib$inflatedNetWorthWithHomeRecalc_as_2019[match(mcdist_sibs$person1, psid_long_sib$psid68_UID)]
mcdist_sibs$mother_edu = psid_long_sib$mother_edu[match(mcdist_sibs$person1, psid_long_sib$psid68_UID)]

mcdist_sibs_income_upper = subset(mcdist_sibs, mcdist_sibs$income > median(psid_long_all$average_parental_income, na.rm = T))
mcdist_sibs_income_lower = subset(mcdist_sibs, mcdist_sibs$income < median(psid_long_all$average_parental_income, na.rm = T))

mcdist_sibs_wealth_upper = subset(mcdist_sibs, mcdist_sibs$wealth > median(psid_long_all$inflatedNetWorthWithHomeRecalc_as_2019, na.rm = T))
mcdist_sibs_wealth_lower = subset(mcdist_sibs, mcdist_sibs$wealth < median(psid_long_all$inflatedNetWorthWithHomeRecalc_as_2019, na.rm = T))

mcdist_sibs_mother_edu_upper = subset(mcdist_sibs, mcdist_sibs$mother_edu > median(psid_long_all$mother_edu, na.rm = T))
mcdist_sibs_mother_edu_lower = subset(mcdist_sibs, mcdist_sibs$mother_edu < median(psid_long_all$mother_edu, na.rm = T))

# Compare values for income, wealth, education upper and lower halves
iu = prop.table(table(mcdist_sibs_income_upper$cluster_ego == mcdist_sibs_income_upper$cluster_alter))
il = prop.table(table(mcdist_sibs_income_lower$cluster_ego == mcdist_sibs_income_lower$cluster_alter))

wu = prop.table(table(mcdist_sibs_wealth_upper$cluster_ego == mcdist_sibs_wealth_upper$cluster_alter))
wl = prop.table(table(mcdist_sibs_wealth_lower$cluster_ego == mcdist_sibs_wealth_lower$cluster_alter))

eu = prop.table(table(mcdist_sibs_mother_edu_upper$cluster_ego == mcdist_sibs_mother_edu_upper$cluster_alter))
el = prop.table(table(mcdist_sibs_mother_edu_lower$cluster_ego == mcdist_sibs_mother_edu_lower$cluster_alter))

########### 
# Same models as above (predicting cluster membership) but with sibling fixed effects
psid_long_sib$standardized_gpa_imp = psid_long_all$standardized_gpa_imp[match(psid_long_sib$psid68_UID, psid_long_all$psid68_UID)]
psid_long_sib$kid_feels_close_parents = psid_long_all$kid_feels_close_parents[match(psid_long_sib$psid68_UID, psid_long_all$psid68_UID)]

psid_long_sib$LCA_ThreeClass_Cat_Relevel = ifelse(psid_long_sib$LCA_ThreeClass_Cat == 'Difficults', 'Strugglers', psid_long_sib$LCA_ThreeClass_Cat)
psid_long_sib$LCA_ThreeClass_Cat_Relevel <- relevel(factor(psid_long_sib$LCA_ThreeClass_Cat_Relevel), ref = "Strugglers")


# Multinom with sibs 
for_reg = psid_long_sib[,c('psid68_UID', 'sib_group', 'year', 
                           'kid_feels_close_parents',  'standardized_gpa_imp',
                           'female', 'first_born',
                           'seq_cluster_3', 'cluster_1', 'cluster_2', 'cluster_3', 'LCA_ThreeClass_Cat_Relevel')]

for_reg = na.omit(for_reg)

# Limit to min year
for_reg = for_reg %>%
  group_by(psid68_UID) %>%
  filter(year == min(year)) %>%
  ungroup()

# Add in time period data
for_reg$pandemic_year = wide_data$pandemic_year[match(for_reg$psid68_UID, wide_data$psid68_UID)]
for_reg$recession_year = wide_data$recession_year[match(for_reg$psid68_UID, wide_data$psid68_UID)]
for_reg$no_pandemic_recession = ifelse(for_reg$pandemic_year == 0 & for_reg$recession_year == 0, 1, 0)

for_reg$LCA_ThreeClass_Cat_RelevelDiligents = ifelse(for_reg$LCA_ThreeClass_Cat_Relevel == 'Diligents', 1, 0)
for_reg$LCA_ThreeClass_Cat_RelevelMiddles = ifelse(for_reg$LCA_ThreeClass_Cat_Relevel == 'Middles', 1, 0)

# Grab variables 
for_reg[,c('female', 
           'kid_feels_close_parents', 'standardized_gpa_imp',
           'LCA_ThreeClass_Cat_RelevelDiligents', 'LCA_ThreeClass_Cat_RelevelMiddles',
          'first_born')] = sapply(for_reg[,c('female', 
           'kid_feels_close_parents', 'standardized_gpa_imp',
           'LCA_ThreeClass_Cat_RelevelDiligents', 'LCA_ThreeClass_Cat_RelevelMiddles',
           'first_born')], scale)

abbreviated_names = c("ssfs", "rsw", "mi")
for_reg$seq_cluster_cat = c("ssfs", "rsw", "mi")[for_reg$seq_cluster_3]
for_reg$seq_cluster_cat = factor(for_reg$seq_cluster_cat, levels = abbreviated_names)

# Multinomial regression 
test_wi <- multinom(seq_cluster_cat ~  female + standardized_gpa_imp + first_born +
                      kid_feels_close_parents + LCA_ThreeClass_Cat_RelevelDiligents + LCA_ThreeClass_Cat_RelevelMiddles + year + recession_year + as.factor(sib_group),
                    data = for_reg, MaxNWts = 4000, maxit = 500)


# Complete function that gets all three pairwise comparisons
create_multinom_excel_table_sibs_complete <- function(model, cluster_names, for_reg_data) {
  
  # Get results with ssfs as reference
  results1 <- broom::tidy(model, exponentiate = TRUE, conf.int = TRUE)
  
  # Refit with rsw as reference to get mi vs rsw comparison
  for_reg_temp <- for_reg_data
  for_reg_temp$seq_cluster_cat <- relevel(factor(for_reg_temp$seq_cluster_cat), ref = "rsw")
  
  model_ref2 <- multinom(seq_cluster_cat ~ female + standardized_gpa_imp + first_born +
                           kid_feels_close_parents + LCA_ThreeClass_Cat_RelevelDiligents + 
                           LCA_ThreeClass_Cat_RelevelMiddles + year + recession_year + 
                           as.factor(sib_group),
                         data = for_reg_temp, MaxNWts = 4000, maxit = 500)
  
  results2 <- broom::tidy(model_ref2, exponentiate = TRUE, conf.int = TRUE)
  
  # Format function
  format_results <- function(data, comp_name) {
    data %>%
      filter(!grepl("Intercept|sib_group", term)) %>%
      mutate(
        OR = round(estimate, 3),
        CI_lower = round(conf.low, 3),
        CI_upper = round(conf.high, 3),
        
        stars = case_when(
          p.value < 0.001 ~ "***",
          p.value < 0.01 ~ "**",
          p.value < 0.05 ~ "*",
          TRUE ~ ""
        ),
        
        OR_CI = paste0(OR, stars, " (", CI_lower, ",", CI_upper, ")"),
        
        p_value = case_when(
          p.value < 0.001 ~ "<0.001",
          p.value < 0.01 ~ sprintf("%.3f", p.value),
          TRUE ~ sprintf("%.3f", p.value)
        ),
        comparison = comp_name
      ) %>%
      select(term, comparison, OR, CI_lower, CI_upper, OR_CI, p_value, stars)
  }
  
  # Create all three comparisons
  comp1 <- results1 %>%
    filter(y.level == "rsw") %>%
    format_results(paste(cluster_names[2], "vs", cluster_names[1]))
  
  comp2 <- results1 %>%
    filter(y.level == "mi") %>%
    format_results(paste(cluster_names[3], "vs", cluster_names[1]))
  
  comp3 <- results2 %>%
    filter(y.level == "mi") %>%
    format_results(paste(cluster_names[3], "vs", cluster_names[2]))
  
  # Combine all comparisons
  all_results <- bind_rows(comp1, comp2, comp3)
  
  # Add variable names
  variable_fixer_sib = data.frame(
    original = c('female', 'standardized_gpa_imp', 'first_born', 'kid_feels_close_parents',
                 'LCA_ThreeClass_Cat_RelevelDiligents', 'LCA_ThreeClass_Cat_RelevelMiddles',
                 'year', 'recession_year'),
    new = c('Female', 'Standardized GPA', 'First Born', 'Feels close to parents',
            'Diligent (vs Strugglers)', 'Middle (vs Strugglers)', 'Year', 'Recession Year')
  )
  
  all_results$Variable = variable_fixer_sib$new[match(all_results$term, variable_fixer_sib$original)]
  all_results$Variable = ifelse(is.na(all_results$Variable), all_results$term, all_results$Variable)
  
  # Create wide format table
  wide_table <- all_results %>%
    select(Variable, comparison, OR_CI, p_value) %>%
    pivot_wider(names_from = comparison, 
                values_from = c(OR_CI, p_value),
                names_sep = "_") %>%
    rename_with(~gsub("OR_CI_", "", .x), starts_with("OR_CI_")) %>%
    rename_with(~paste0(.x, "_p"), starts_with("p_value_"))
  
  return(list(long_format = all_results, wide_format = wide_table))
}

# Define cluster names
cluster_names_sib <- c("In School & Financially Supported", "Residentially Supported Workers", "Married & Independent")

# Use the complete function
results_tables_sibs <- create_multinom_excel_table_sibs_complete(test_wi, cluster_names_sib, for_reg)


library(openxlsx)

# Create Excel workbook for sibling analysis
wb_sibs <- createWorkbook()

# Add worksheets
addWorksheet(wb_sibs, "Pairwise Comparisons")
addWorksheet(wb_sibs, "Long Format")
addWorksheet(wb_sibs, "Model Summary")

# Write wide format table (main results)
writeData(wb_sibs, "Pairwise Comparisons", results_tables_sibs$wide_format, 
          startRow = 3, startCol = 1, colNames = TRUE)

# Add title and significance legend
writeData(wb_sibs, "Pairwise Comparisons", 
          "Within-Sibling Multinomial Logistic Regression: Pairwise Comparisons (OR with 95% CI)", 
          startRow = 1, startCol = 1)

writeData(wb_sibs, "Pairwise Comparisons", 
          "Significance: * p<0.05, ** p<0.01, *** p<0.001", 
          startRow = 2, startCol = 1)

# Write long format table
writeData(wb_sibs, "Long Format", results_tables_sibs$long_format, 
          startRow = 3, startCol = 1, colNames = TRUE)

writeData(wb_sibs, "Long Format", 
          "Within-Sibling Multinomial Logistic Regression: All Results (Long Format)", 
          startRow = 1, startCol = 1)

writeData(wb_sibs, "Long Format", 
          "Significance: * p<0.05, ** p<0.01, *** p<0.001", 
          startRow = 2, startCol = 1)

# Model summary for within-sibling analysis
model_summary_sibs <- data.frame(
  Metric = c("Reference Category", 
             "Sample Size", 
             "Number of Predictors (excluding sibling FE)",
             "Number of Sibling Groups",
             "Total Parameters (including sibling FE)",
             "AIC", 
             "BIC",
             "Log-Likelihood"),
  Value = c(cluster_names_sib[1],
            nrow(test_wi$fitted.values),
            8,  # Your main predictors: female, gpa, first_born, close_parents, 2 LCA dummies, year, recession
            length(unique(for_reg$sib_group)),
            length(coef(test_wi)[1,]) * nrow(coef(test_wi)),
            round(AIC(test_wi), 2),
            round(BIC(test_wi), 2),
            round(as.numeric(logLik(test_wi)), 2))
)

writeData(wb_sibs, "Model Summary", model_summary_sibs, 
          startRow = 2, startCol = 1, colNames = TRUE)

writeData(wb_sibs, "Model Summary", 
          "Within-Sibling Model Information (with Sibling Fixed Effects)", 
          startRow = 1, startCol = 1)

# Format the Excel file
bold_style <- createStyle(textDecoration = "bold")
addStyle(wb_sibs, "Pairwise Comparisons", bold_style, rows = 1:3, cols = 1:ncol(results_tables_sibs$wide_format), gridExpand = TRUE)
addStyle(wb_sibs, "Long Format", bold_style, rows = 1:3, cols = 1:ncol(results_tables_sibs$long_format), gridExpand = TRUE)
addStyle(wb_sibs, "Model Summary", bold_style, rows = 1:2, cols = 1:2, gridExpand = TRUE)

# Auto-size columns
setColWidths(wb_sibs, "Pairwise Comparisons", cols = 1:ncol(results_tables_sibs$wide_format), widths = "auto")
setColWidths(wb_sibs, "Long Format", cols = 1:ncol(results_tables_sibs$long_format), widths = "auto")
setColWidths(wb_sibs, "Model Summary", cols = 1:2, widths = "auto")

# Save the Excel file
saveWorkbook(wb_sibs, "within_sibling_multinomial_results.xlsx", overwrite = TRUE)

# Print confirmation
cat("Within-sibling results saved to 'within_sibling_multinomial_results.xlsx'\n")
cat("The file contains three sheets:\n")
cat("1. 'Pairwise Comparisons': Main results with significance stars\n")
cat("2. 'Long Format': All results in long format\n")
cat("3. 'Model Summary': Model information including sibling fixed effects\n")


##################
# MARGINAL EFFECTS SIBS
##################

library(marginaleffects)  # This is newer and more reliable than 'margins'

vars_of_interest <- c("year", "standardized_gpa_imp", "recession_year",
                   "health_level", "kid_feels_close_parents",
                   "first_born", "female", 'LCA_ThreeClass_Cat_RelevelDiligents', 'LCA_ThreeClass_Cat_RelevelMiddles')

# Get marginal effects - this will include ALL categories
mfx <- marginaleffects(test_wi, variables = vars_of_interest, type = "latent")

# Convert to data frame and plot
mfx_df <- as.data.frame(mfx)
mfx_df = na.omit(mfx_df)
# Calculate average marginal effects by term and group
avg_mfx <- mfx_df %>%
  group_by(term, group) %>%
  summarise(
    avg_effect = mean(estimate),
    se = sd(estimate) / sqrt(n()),
    conf.low = mean(conf.low),
    conf.high = mean(conf.high),
    .groups = 'drop'
  )

avg_mfx$Cluster = 'Residentially Supported Workers'
avg_mfx$Cluster = ifelse(avg_mfx$group == 'mi', 'Married & Independent', avg_mfx$Cluster)
avg_mfx$Cluster = ifelse(avg_mfx$group == 'ssfs', 'In School & Financially Supported', avg_mfx$Cluster)
avg_mfx$Cluster = factor(avg_mfx$Cluster, levels = c('In School & Financially Supported', 'Residentially Supported Workers', 'Married & Independent'))

# Data frame to fix variable names to be more sensible / visually appealing
variable_fixer = data.frame(
  original = c('in_tutoring', 'disobedient', 'helped_sib', 'kid_feels_close_parents',
               'year', 'earnings_work_log', 'first_born', 'health_level', 'female',
               'in_college_now', 'standardized_gpa_imp', paste0('age_factor', 18:28),
               'keeping_home', 'pandemic_year', 'recession_year', 'LCA_ThreeClass_Cat_RelevelDiligents', 'LCA_ThreeClass_Cat_RelevelMiddles'),
  new = c('In tutoring', 'Disobedient', 'Helped sibling', 'Feels close to parents',
          'Year Entered TAS', 'Earnings from work (logged)', 'First Born', 'Health Level', 'Female',
          'In college', 'Standardized GPA', paste0('Age: ', 18:28),
          'Keeping Home', 'Pandemic Year', 'Recession Year', 'LCA: Diligents', 'LCA: Middles'))


# match in fixed variable names
avg_mfx$Variable = variable_fixer$new[match(avg_mfx$term, variable_fixer$original)]

# Set the desired order for variables (customize as needed)
desired_order <- c("Year Entered TAS", "Standardized GPA", "Recession Year",
                   'LCA: Diligents', 'LCA: Middles', "Feels close to parents",
                   "First Born", "Female", "Helped sibling")

# Apply the factor ordering
avg_mfx$Variable <- factor(avg_mfx$Variable, levels = desired_order)


# Create the plot
model_colors <- c("#138808", "#3E2F5B",  "#FF8C00") 


# Create coefficient plot
ggplot(avg_mfx, aes(x = Variable, y = avg_effect, color = Cluster)) +
  geom_point(position = position_dodge(width = 0.6), size = 6) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), 
                width = 0.9, position = position_dodge(width = 0.6), lwd = 2) +
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.7) +
  coord_flip() +
  labs(title = "Average Marginal Effects by Outcome Category",
       y = "Marginal Effect on Probability",
       x = "Predictors",
       color = " ") +
  scale_color_manual(values = model_colors) +
  theme_minimal(base_size = 30) +
  theme(
    axis.title.x = element_text(face = "bold", size = 20, margin = margin(t = 10)),
    axis.title.y = element_text(face = "bold", size = 20, margin = margin(r = 10)),
    axis.text = element_text(size = 24),
    legend.title = element_text(face = "bold", size = 20),
    legend.text = element_text(size = 24),
    legend.position = "top",
    panel.grid.major = element_line(size = 0.5, linetype = 'solid', color = "grey80"),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )

ggsave('multinomial_within_family_fixed_macro_all_cat.pdf',
       plot = last_plot(),
       device = "pdf",
       height = 12,
       width = 20) 

#######################
# COX MODELS #
#######################

# Between family EHA
library(survival)
library(coxme)

last_year <- psid_long_all %>%
  group_by(UID_2) %>%
  summarise(final_year = max(year)) %>%
  ungroup()

# Overall measure of having children, issue is that it doesn't really have temporal resolution
# So I limited to situations where birth data is up-to-date by the person's last wave
psid_long_all$final_year = last_year$final_year[match(psid_long_all$UID_2, last_year$UID_2)]
psid_long_all$last_year_birth_info = as.numeric(psid_long_all$ER32021_year_birth_info_most_recently_updated__all)
psid_long_all$last_year_birth_info = ifelse(psid_long_all$last_year_birth_info > 2023, NA, psid_long_all$last_year_birth_info)
valid_entries = psid_long_all$last_year_birth_info >= psid_long_all$final_year

psid_long_all$live_births = ifelse(valid_entries, psid_long_all$ER32022__live_births_to_this_individual__all, NA)
psid_long_all$live_births = ifelse(psid_long_all$live_births > 9, NA, psid_long_all$live_births)
psid_long_all$live_births = as.numeric(psid_long_all$live_births)

# Arrange by person + year
psid_long_all = psid_long_all %>%
  arrange(psid68_UID, year)

psid_long_all = psid_long_all %>%
  group_by(psid68_UID) %>%
  mutate(
    lagged_births = dplyr::lag(number_births, order_by = year)
  ) %>%
  ungroup()

psid_long_all$lagged_births = ifelse(psid_long_all$number_births == 0 & is.na(psid_long_all$lagged_births), 0, psid_long_all$lagged_births)
psid_long_all$transition_to_childbearing = ifelse(psid_long_all$lagged_births == 0 & psid_long_all$number_births > 0, 1, 0)
psid_long_all$had_child_between_waves = ifelse(psid_long_all$number_births > psid_long_all$lagged_births, 1, 0)
psid_long_all$is_parent = ifelse(psid_long_all$number_births > 0, 1, 0)

psid_long_all$residentially_independent = ifelse(psid_long_all$FS == 1 | psid_long_all$TI == 1, 1, 0)
psid_long_all$financially_independent = ifelse(psid_long_all$RS == 1 | psid_long_all$TI == 1, 1, 0)

# Create transition variables
eha_data = psid_long_all %>%
  group_by(psid68_UID) %>%
  mutate(
    first_obs = row_number() == 1,
    transition_to_residential_independence = case_when(
      first_obs & residentially_independent == 1 ~ 1,
      !first_obs & residentially_independent == 1 & dplyr::lag(residentially_independent) == 0 ~ 1,
      TRUE ~ 0
    ),
    transition_to_financial_independence = case_when(
      first_obs & financially_independent == 1 ~ 1,
      !first_obs & financially_independent == 1 & dplyr::lag(financially_independent) == 0 ~ 1,
      TRUE ~ 0
    ),
    transition_to_total_dependence = case_when(
      first_obs & RS_FS == 1 ~ 1,
      !first_obs & RS_FS == 1 & dplyr::lag(RS_FS) == 0 ~ 1,
      TRUE ~ 0
    ),
    transition_to_total_independence = case_when(
      first_obs & TI == 1 ~ 1,
      !first_obs & TI == 1 & dplyr::lag(TI) == 0 ~ 1,
      TRUE ~ 0
    ),
    transition_out_total_independence = case_when(
      !first_obs & TI == 0 & dplyr::lag(TI) == 1 ~ 1,
      TRUE ~ 0
    ),
    transition_out_total_dependence = case_when(
      !first_obs & RS_FS == 0 & dplyr::lag(RS_FS) == 1 ~ 1,
      TRUE ~ 0
    ),
    transition_to_marriage = case_when(
      first_obs & M == 1 ~ 1,
      !first_obs & M == 1 & dplyr::lag(M) == 0 ~ 1,
      TRUE ~ 0
    ),
    transition_to_single = case_when(
      first_obs & S == 1 ~ 1,
      !first_obs & S == 1 & dplyr::lag(S) == 0 ~ 1,
      TRUE ~ 0
    ),
    transition_to_cohab = case_when(
      first_obs & C == 1 ~ 1,
      !first_obs & C == 1 & dplyr::lag(C) == 0 ~ 1,
      TRUE ~ 0
    ),
    transition_to_working = case_when(
      first_obs & W == 1 ~ 1,
      !first_obs & W == 1 & dplyr::lag(W) == 0 ~ 1,
      TRUE ~ 0
    ),
    transition_to_notworking = case_when(
      first_obs & NWS == 1 ~ 1,
      !first_obs & NWS == 1 & dplyr::lag(NWS) == 0 ~ 1,
      TRUE ~ 0
    ),
    transition_to_school = case_when(
      first_obs & S == 1 ~ 1,
      !first_obs & S == 1 & dplyr::lag(S) == 0 ~ 1,
      TRUE ~ 0
    ),
    transition_out_school = case_when(
      !first_obs & S == 0 & dplyr::lag(S) == 1 ~ 1,
      TRUE ~ 0
    )
  ) %>%
  ungroup()

# Create lagged variables
eha_data_w_lagged = eha_data %>%
  group_by(psid68_UID) %>%
  mutate(
    lagged_TI = dplyr::lag(TI, order_by = year),
    lagged_RS_FS = dplyr::lag(RS_FS, order_by = year),
    lagged_FS = dplyr::lag(FS, order_by = year),
    lagged_RS = dplyr::lag(RS, order_by = year),
    lagged_M = dplyr::lag(M, order_by = year),
    lagged_C = dplyr::lag(C, order_by = year),
    lagged_S = dplyr::lag(S, order_by = year),
    lagged_W = dplyr::lag(W, order_by = year),
    lagged_SCH = dplyr::lag(SCH, order_by = year),
    lagged_NWS = dplyr::lag(NWS, order_by = year),
    lagged_parent = dplyr::lag(is_parent, order_by = year),
    lagged_res_dep = dplyr::lag(residentially_dep, order_by = year),
    lagged_fin_dep = dplyr::lag(financially_dep, order_by = year)
  ) %>%
  ungroup()

# CONDITIONAL ON MARRIAGE, LIKELIHOOD OF TRANSITIONING INTO TI
# CONDITIONAL ON TI, LIKELIHOOD OF TRANSITIONING INTO MARRIAGE

# Transitions into total independence
eha_data_w_lagged$age_squared = eha_data_w_lagged$age * eha_data_w_lagged$age
eha_data_w_lagged$pandemic_year = ifelse(eha_data_w_lagged$year == 2021, 1, 0)
eha_data_w_lagged$recession_year = ifelse(eha_data_w_lagged$year %in% c(2007, 2009), 1, 0)


ti_sum = aggregate(transition_to_total_independence ~ psid68_UID, data = eha_data_w_lagged, FUN = function(x) sum(x, na.rm = T))
(sum(ti_sum$transition_to_total_independence > 1)/nrow(ti_sum)) * 100

fi_sum = aggregate(transition_to_financial_independence ~ psid68_UID, data = eha_data_w_lagged, FUN = function(x) sum(x, na.rm = T))
(sum(fi_sum$transition_to_financial_independence > 1)/nrow(fi_sum)) * 100

ri_sum = aggregate(transition_to_residential_independence ~ psid68_UID, data = eha_data_w_lagged, FUN = function(x) sum(x, na.rm = T))
(sum(ri_sum$transition_to_residential_independence > 1)/nrow(ri_sum)) * 100

marr_sum = aggregate(transition_to_marriage ~ psid68_UID, data = eha_data_w_lagged, FUN = function(x) sum(x, na.rm = T))
(sum(marr_sum$transition_to_marriage > 1)/nrow(marr_sum)) * 100

# Only 6.7% transitioned into residential independence more than once, 9.6% transitioned to financial independence more than once, 4.4% transitioned to total independence more than once, and 0.028% transitioned into marriage more than once.

library(dplyr)

prepare_first_event_data <- function(data, outcome) {
  # First, remove NA values for the specific outcome
  data_clean <- data %>%
    filter(!is.na(!!sym(outcome)))

  data_clean = data_clean %>%
    arrange(psid68_UID, year)
  
  first_event <- data_clean %>%
    group_by(psid68_UID) %>%
    summarise(
      event_year = if(sum(!!sym(outcome)) > 0) min(year[!!sym(outcome) == 1]) else Inf,
      .groups = "drop"
    )
  
  data_first_event <- data_clean %>%
    left_join(first_event, by = "psid68_UID") %>%
    group_by(psid68_UID) %>%
    filter(year <= event_year) %>%
    mutate(
      event = ifelse(year == event_year & event_year != Inf, 1, 0),
      time = year - min(year) + 1
    ) %>%
    ungroup()
  
  return(data_first_event)
}

# Usage
outcomes <- c(
  "transition_to_residential_independence",
  "transition_to_financial_independence",
  "transition_to_total_independence",
  "transition_to_marriage"
)

# Prepare data for each outcome
eha_data_list <- lapply(outcomes, function(outcome) {
  prepare_first_event_data(eha_data_w_lagged, outcome)
})

names(eha_data_list) <- outcomes

covars_btw_fam = c('wealth_quintiles', 
                   'race_cat',
                   'parents_currently_married',
                   'number_children_mother',
                   'mother_edu',
                   'metro_psid',
                   'female',
                   'fam_income_quintiles', 
                   'age_factor',
                   'standardized_gpa_imp',
                   'pandemic_year',
                   'recession_year')

covars_cds = c('working_now',
           'wealth_quintiles', 
           'standardized_gpa_imp', 
           'race_cat',
           'parents_currently_married',
           'number_children_mother',
           'mother_edu',
           'metro_psid',
           'marriage_cohabitation_cat',
           'LCA_ThreeClass_Cat',
           'kid_feels_close_parents',
           'join_year',
           'in_college_now',
           'helped_sib',
           'health_level',
           'first_born',
           'female',
           'fam_income_quintiles', 
           'earnings_work_log_imp',
           'age_factor',
           'pandemic_year')

final_covars_btw = c('psid68_UID', 'age', 'lagged_res_dep', 'lagged_fin_dep', 'TI', 'transition_to_total_independence', 'transition_to_residential_independence', 'transition_to_financial_independence', 'lagged_M', 'lagged_C', 'lagged_W', 'lagged_NWS', 'lagged_SCH', 'lagged_FS', 'lagged_RS', 'lagged_TI', 'lagged_RS_FS', covars_btw_fam)

# TRANSITION MODELS:
for_reg_df = eha_data_list[[1]]

df1 = for_reg_df[,c('psid68_UID', 'age', 'TI', 'transition_to_total_independence', 'transition_to_residential_independence', 'transition_to_financial_independence',  'lagged_M', 'lagged_C', 'lagged_W', 'lagged_NWS', 'lagged_SCH', 'lagged_FS', 'lagged_RS', 'lagged_TI', 'lagged_RS_FS', covars_btw_fam)]
df1 = na.omit(df1)
df1$age2 = df1$age + 1

# PURE BETWEEN FAMILY
# Residential independence
mixed_model1_res <- coxme(Surv(time = age, time2 = age2, transition_to_residential_independence) ~ female + race_cat + 
                        mother_edu + parents_currently_married + number_children_mother + 
                        fam_income_quintiles + wealth_quintiles + metro_psid + pandemic_year + recession_year +
                        lagged_M + lagged_C + lagged_NWS + lagged_SCH + 
                        (1 | psid68_UID), data = df1)


summary(mixed_model1_res)

# WITH INT
mixed_model1_res_int <- coxme(Surv(time = age, time2 = age2, transition_to_residential_independence) ~ female * lagged_M + 
                        race_cat + mother_edu + parents_currently_married + number_children_mother + 
                        fam_income_quintiles + wealth_quintiles + metro_psid + pandemic_year + recession_year +
                        lagged_M + lagged_C + lagged_NWS + lagged_SCH + 
                        (1 | psid68_UID), data = df1)

summary(mixed_model1_res_int)

# Financial independence
for_reg_df = eha_data_list[[2]]

df1_fin = for_reg_df[,c('psid68_UID', 'age', 'TI', 'transition_to_total_independence', 'transition_to_residential_independence', 'transition_to_financial_independence',  'lagged_M', 'lagged_C', 'lagged_W', 'lagged_NWS', 'lagged_SCH', 'lagged_FS', 'lagged_RS', 'lagged_TI', 'lagged_RS_FS', covars_btw_fam)]
df1_fin = na.omit(df1_fin)
df1_fin$age2 = df1_fin$age + 1

mixed_model1_fin <- coxme(Surv(time = age, time2 = age2, transition_to_financial_independence) ~ female + race_cat + 
                        mother_edu + parents_currently_married + number_children_mother + 
                        fam_income_quintiles + wealth_quintiles + metro_psid + pandemic_year + recession_year +
                        lagged_M + lagged_C + lagged_NWS + lagged_SCH + 
                        (1 | psid68_UID), data = df1_fin)

summary(mixed_model1_fin)

# WITH INT
mixed_model1_fin_int <- coxme(Surv(time = age, time2 = age2, transition_to_financial_independence) ~ female * lagged_M + 
                        race_cat + mother_edu + parents_currently_married + number_children_mother + 
                        fam_income_quintiles + wealth_quintiles + metro_psid + pandemic_year + recession_year +
                        lagged_M + lagged_C + lagged_NWS + lagged_SCH + 
                        (1 | psid68_UID), data = df1_fin)

summary(mixed_model1_fin_int)


# Total Independence
for_reg_df = eha_data_list[[3]]

df1_total = for_reg_df[,c('psid68_UID', 'age', 'TI', 'transition_to_total_independence', 'transition_to_residential_independence', 'transition_to_financial_independence',  'lagged_M', 'lagged_C', 'lagged_W', 'lagged_NWS', 'lagged_SCH', 'lagged_FS', 'lagged_RS', 'lagged_TI', 'lagged_RS_FS', covars_btw_fam)]
df1_total = na.omit(df1_total)
df1_total$age2 = df1_total$age + 1

mixed_model1 <- coxme(Surv(time = age, time2 = age2, transition_to_total_independence) ~ female + race_cat + 
                        mother_edu + parents_currently_married + number_children_mother + 
                        fam_income_quintiles + wealth_quintiles + metro_psid + pandemic_year + recession_year +
                        lagged_M + lagged_C + lagged_NWS + lagged_SCH + 
                        (1 | psid68_UID), data = df1_total)

summary(mixed_model1)

# WITH INT
mixed_model1_int <- coxme(Surv(time = age, time2 = age2, transition_to_total_independence) ~ female * lagged_M + 
                        race_cat + mother_edu + parents_currently_married + number_children_mother + 
                        fam_income_quintiles + wealth_quintiles + metro_psid + pandemic_year + recession_year +
                        lagged_M + lagged_C + lagged_NWS + lagged_SCH + 
                        (1 | psid68_UID), data = df1_total)

summary(mixed_model1_int)

# TRANSITION TO MARRIAGE
for_reg_df = eha_data_list[[4]]

df1_marr = for_reg_df[,c('psid68_UID', 'age', 'transition_to_marriage', 'lagged_M', 'lagged_C', 'lagged_W', 'lagged_NWS', 'lagged_SCH', 'lagged_FS', 'lagged_RS', 'lagged_TI', 'lagged_RS_FS', 'lagged_res_dep', 'lagged_fin_dep', covars_btw_fam)]
df1_marr = na.omit(df1_marr)
df1_marr$age2 = df1_marr$age + 1

mixed_model1_marr <- coxme(Surv(time = age, time2 = age2, transition_to_marriage) ~ female + 
                            race_cat + mother_edu + parents_currently_married + number_children_mother + 
                            fam_income_quintiles + wealth_quintiles + metro_psid +  pandemic_year + recession_year +
                            lagged_C + lagged_NWS + lagged_SCH + 
                            lagged_FS + lagged_RS + lagged_TI +
                            (1 | psid68_UID), data = df1_marr)

summary(mixed_model1_marr)

mixed_model1_marr_int <- coxme(Surv(time = age, time2 = age2, transition_to_marriage) ~ female * lagged_TI +
                             race_cat + mother_edu + parents_currently_married + number_children_mother + 
                             fam_income_quintiles + wealth_quintiles + metro_psid + pandemic_year + recession_year +
                             lagged_C + lagged_NWS + lagged_SCH + 
                             lagged_FS + lagged_RS + lagged_TI +
                             (1 | psid68_UID), data = df1_marr)

summary(mixed_model1_marr_int)


# TRANSITION TO CHILDBEARING
df1_cb = eha_data_w_lagged[,c('psid68_UID', 'age', 'transition_to_childbearing', 'lagged_M', 'lagged_C', 'lagged_W', 'lagged_NWS', 'lagged_SCH', 'lagged_FS', 'lagged_RS', 'lagged_TI', 'lagged_RS_FS', 'lagged_res_dep', 'lagged_fin_dep',covars_btw_fam)]
df1_cb = na.omit(df1_cb)
df1_cb$age2 = df1_cb$age + 1

mixed_model1_cb <- coxme(Surv(time = age, time2 = age2, transition_to_childbearing) ~ female + 
                             race_cat + mother_edu + parents_currently_married + number_children_mother + 
                             fam_income_quintiles + wealth_quintiles + metro_psid + pandemic_year + recession_year +
                             lagged_M + lagged_C + lagged_NWS + lagged_SCH + 
                             lagged_FS + lagged_RS + lagged_TI +
                             (1 | psid68_UID), data = df1_cb)

summary(mixed_model1_cb)

mixed_model1_cb_int <- coxme(Surv(time = age, time2 = age2, transition_to_childbearing) ~ female * lagged_TI + 
                               race_cat + mother_edu + parents_currently_married + number_children_mother + 
                               fam_income_quintiles + wealth_quintiles + metro_psid + pandemic_year + recession_year +
                               lagged_M + lagged_C + lagged_NWS + lagged_SCH + 
                               lagged_FS + lagged_RS + lagged_TI +
                               (1 | psid68_UID), data = df1_cb)

summary(mixed_model1_cb_int)

mixed_model1_cb_int2 <- coxme(Surv(time = age, time2 = age2, transition_to_childbearing) ~ female * lagged_M + 
                               race_cat + mother_edu + parents_currently_married + number_children_mother + 
                               fam_income_quintiles + wealth_quintiles + metro_psid + pandemic_year + recession_year +
                               lagged_M + lagged_C + lagged_NWS + lagged_SCH + 
                               lagged_FS + lagged_RS + lagged_TI +
                               (1 | psid68_UID), data = df1_cb)

summary(mixed_model1_cb_int2)


### SUMMARIZE MODELS:
coxme_stargazer = function(x, dep.var = "Transition into Independence"){
  # Extract coefficients and standard errors
  coefs <- coef(summary(x))
  coefs_df <- as.data.frame(coefs)
  
  # Create a data frame for stargazer
  model_results <- data.frame(
    term = rownames(coefs_df),
    estimate = coefs_df[, "coef"],
    std.error = coefs_df[, "se(coef)"],
    p.value = coefs_df[, "p"]
  )
  
  model_results$stars = ifelse(model_results$p.value < 0.05, '*', '')
  model_results$stars = ifelse(model_results$p.value < 0.01, '**', model_results$stars)
  model_results$stars = ifelse(model_results$p.value < 0.001, '***', model_results$stars)
  
  # Convert term names to match what stargazer expects
  rownames(model_results) <- NULL
  
  stargazer(
    as.data.frame(model_results),
    type = "text",
    summary = FALSE,
    title = dep.var,
    digits = 3,
    dep.var.labels = dep.var,
    coef = list(model_results$estimate),
    se = list(model_results$std.error),
    p = list(model_results$p.value)
  )
}

# COXME RESULTS
# Without CDS
coxme_stargazer(mixed_model1_res, "Transition to Independence")
coxme_stargazer(mixed_model1_res_int, "Transition to Independence")
coxme_stargazer(mixed_model1_fin, "Transition to Independence")
coxme_stargazer(mixed_model1_fin_int, "Transition to Independence")
coxme_stargazer(mixed_model1, "Transition to Independence")
coxme_stargazer(mixed_model1_int, "Transition to Independence")


coxme_stargazer(mixed_model1_marr, "Transition to Marriage")
coxme_stargazer(mixed_model1_marr_int, "Transition to Marriage")

coxme_stargazer(mixed_model1_cb, "Transition to Child-bearing")
coxme_stargazer(mixed_model1_cb_int, "Transition to Child-bearing")



# TRANSITION TO INDEPENDENCE WITH CDS
df2 = eha_data_w_lagged[,c('psid68_UID', 'age', 'TI', 'transition_to_total_independence', 'lagged_M', 'lagged_C', 'lagged_W', 'lagged_NWS', 'lagged_SCH', 'lagged_FS', 'lagged_RS', 'lagged_TI', 'lagged_RS_FS', covars_cds)]
df2 = na.omit(df2)
df2$age2 = df2$age + 1

mixed_model2 <- coxme(Surv(time = age, time2 = age2, transition_to_total_independence) ~ female + race_cat + 
                        mother_edu + parents_currently_married + number_children_mother + 
                        fam_income_quintiles + wealth_quintiles + metro_psid + 
                        standardized_gpa_imp + LCA_ThreeClass_Cat + kid_feels_close_parents +
                        helped_sib + first_born + earnings_work_log_imp + 
                        lagged_M + lagged_C + lagged_W + lagged_SCH + pandemic_year + 
                        (1 | psid68_UID), data = df2)
summary(mixed_model2)


mixed_model2_int <- coxme(Surv(time = age, time2 = age2, transition_to_total_independence) ~ female * lagged_M + race_cat + 
                        mother_edu + parents_currently_married + number_children_mother + 
                        fam_income_quintiles + wealth_quintiles + metro_psid + 
                        standardized_gpa_imp + LCA_ThreeClass_Cat + kid_feels_close_parents +
                        helped_sib + first_born + earnings_work_log_imp + 
                        lagged_M + lagged_C + lagged_W + lagged_SCH + pandemic_year + 
                        (1 | psid68_UID), data = df2)

summary(mixed_model2_int)

# Marriage w CDS
df2_marr = eha_data_w_lagged[,c('psid68_UID', 'age', 'TI', 'transition_to_marriage', 'lagged_M', 'lagged_C', 'lagged_W', 'lagged_NWS', 'lagged_SCH', 'lagged_FS', 'lagged_RS', 'lagged_TI', 'lagged_RS_FS', covars_cds)]
df2_marr = na.omit(df2_marr)
df2_marr$age2 = df2_marr$age + 1

mixed_model2_marr <- coxme(Surv(time = age, time2 = age2, transition_to_marriage) ~ female + 
                             race_cat + mother_edu + parents_currently_married + number_children_mother + 
                             fam_income_quintiles + wealth_quintiles + metro_psid + 
                             standardized_gpa_imp + LCA_ThreeClass_Cat + kid_feels_close_parents +
                             helped_sib + first_born + earnings_work_log_imp + 
                             lagged_TI + lagged_C + lagged_W + lagged_SCH + pandemic_year + 
                             (1 | psid68_UID), data = df2_marr)

summary(mixed_model2_marr)

mixed_model2_marr_int <- coxme(Surv(time = age, time2 = age2, transition_to_marriage) ~ female * lagged_TI + 
                                 race_cat + mother_edu + parents_currently_married + number_children_mother + 
                                 fam_income_quintiles + wealth_quintiles + metro_psid + 
                                 standardized_gpa_imp + LCA_ThreeClass_Cat + kid_feels_close_parents +
                                 helped_sib + first_born + earnings_work_log_imp + 
                                 lagged_TI + lagged_C + lagged_W + lagged_SCH + pandemic_year + 
                                 (1 | psid68_UID), data = df2_marr)

summary(mixed_model2_marr_int)


# Childrearing w CDS
df2_cb = eha_data_w_lagged[,c('psid68_UID', 'age', 'TI', 'transition_to_childbearing', 'lagged_M', 'lagged_C', 'lagged_W', 'lagged_NWS', 'lagged_SCH', 'lagged_FS', 'lagged_RS', 'lagged_TI', 'lagged_RS_FS', covars_cds)]
df2_cb = na.omit(df2_cb)
df2_cb$age2 = df2_cb$age + 1

mixed_model2_cb <- coxme(Surv(time = age, time2 = age2, transition_to_childbearing) ~ female + 
                             race_cat + mother_edu + parents_currently_married + number_children_mother + 
                             fam_income_quintiles + wealth_quintiles + metro_psid + 
                             standardized_gpa_imp + LCA_ThreeClass_Cat + kid_feels_close_parents +
                             helped_sib + first_born + earnings_work_log_imp + 
                             lagged_TI + lagged_M + lagged_C + lagged_W + lagged_SCH +  pandemic_year + 
                             (1 | psid68_UID), data = df2_cb)

summary(mixed_model2_cb)

mixed_model2_cb_int <- coxme(Surv(time = age, time2 = age2, transition_to_childbearing) ~ female * lagged_TI + 
                                 race_cat + mother_edu + parents_currently_married + number_children_mother + 
                                 fam_income_quintiles + wealth_quintiles + metro_psid + 
                                 standardized_gpa_imp + LCA_ThreeClass_Cat + kid_feels_close_parents +
                                 helped_sib + first_born + earnings_work_log_imp + 
                                 lagged_TI + lagged_M + lagged_C + lagged_W + lagged_SCH +  pandemic_year + 
                                 (1 | psid68_UID), data = df2_cb)

summary(mixed_model2_cb_int)


mixed_model2_cb_int2 <- coxme(Surv(time = age, time2 = age2, transition_to_childbearing) ~ female * lagged_M + 
                                 race_cat + mother_edu + parents_currently_married + number_children_mother + 
                                 fam_income_quintiles + wealth_quintiles + metro_psid + 
                                 standardized_gpa_imp + LCA_ThreeClass_Cat + kid_feels_close_parents +
                                 helped_sib + first_born + earnings_work_log_imp + 
                                 lagged_TI + lagged_M + lagged_C + lagged_W + lagged_SCH +  pandemic_year + 
                                 (1 | psid68_UID), data = df2_cb)

summary(mixed_model2_cb_int)


# With CDS
coxme_stargazer(mixed_model2, "Transition to Independence")
coxme_stargazer(mixed_model2_int, "Transition to Independence")
coxme_stargazer(mixed_model2_marr, "Transition to Marriage")
coxme_stargazer(mixed_model2_marr_int, "Transition to Marriage")
coxme_stargazer(mixed_model2_cb, "Transition to Child-bearing")
coxme_stargazer(mixed_model2_cb_int, "Transition to Child-bearing")



# transitions out
df3 = subset(df1, lagged_TI == 1)
mixed_model3 <- coxme(Surv(year, transition_out_total_independence) ~ age + age_squared + female + first_born + health_level + earnings_work_log + standardized_gpa_imp +  race_cat + average_parental_income_log + mother_edu + lagged_M + lagged_C + lagged_W + lagged_NWS + (1 | psid68_UID), data = df3)
summary(mixed_model3)

df4 = subset(df2, lagged_TI == 1)
mixed_model4 <- coxme(Surv(year, transition_out_total_independence) ~ age + age_squared + female + first_born + health_level + earnings_work_log + standardized_gpa_imp +  race_cat + average_parental_income_log + mother_edu + childhood_divorce + disobedient + helped_sib + in_tutoring + kid_feels_close_parents + lagged_M + lagged_C + lagged_W + lagged_NWS + (1 | psid68_UID), data = df4)
summary(mixed_model4)



# Within family
all = readRDS("psid_long_with_ta_and_ind_v9.RDS")

# create income quintiles
all = all %>% mutate(fam_income_quintiles = ntile(average_parental_income, 5))
all$fam_income_quintiles = paste0("Q", all$fam_income_quintiles)
all$fam_income_quintiles = factor(all$fam_income_quintiles, levels = c("Q1", "Q2", "Q3", "Q4", "Q5"))

# create wealth quintiles
all = all %>% mutate(wealth_quintiles = ntile(average_parental_wealth_w_equity, 5))
all$wealth_quintiles = paste0("Q", all$wealth_quintiles)
all$wealth_quintiles = factor(all$wealth_quintiles, levels = c("Q1", "Q2", "Q3", "Q4", "Q5"))

# create mother edu quintiles
all = all %>% mutate(mother_edu_quintiles = ntile(mother_edu, 5))
all$mother_edu_quintiles = paste0("Q", all$mother_edu_quintiles)
all$mother_edu_quintiles = factor(all$mother_edu_quintiles, levels = c("Q1", "Q2", "Q3", "Q4", "Q5"))

all$residentially_dep = abs(all$residentially_ind - 1)
all$financially_dep = abs(all$financially_ind - 1)

all$age = ifelse(all$year == 2005, all$ER33804_age_of_individual_2005, NA)
all$age = ifelse(all$year == 2007, all$ER33904_age_of_individual_2007, all$age)
all$age = ifelse(all$year == 2009, all$ER34004_age_of_individual_2009, all$age)
all$age = ifelse(all$year == 2011, all$ER34104_age_of_individual_2011, all$age)
all$age = ifelse(all$year == 2013, all$ER34204_age_of_individual_2013, all$age)
all$age = ifelse(all$year == 2015, all$ER34305_age_of_individual_2015, all$age)
all$age = ifelse(all$year == 2017, all$ER34504_age_of_individual_2017, all$age)
all$age = ifelse(all$year == 2019, all$ER34704_age_of_individual_2019, all$age)
all$age = ifelse(all$year == 2021, all$ER34904_age_of_individual_2021, all$age)
all$age = as.numeric(all$age)
all$age = ifelse(all$age == 16, NA, all$age)

## mean imutation for gap
mean_value <- mean(all$standardized_gpa, na.rm = TRUE)
all$standardized_gpa_imp = all$standardized_gpa
all$standardized_gpa_imp[is.na(all$standardized_gpa_imp)] <- mean_value

all$earnings_work_log_imp = all$earnings_work_log
all$earnings_work_log_imp = ifelse(is.na(all$earnings_work_log_imp), 0, all$earnings_work_log_imp)

all$age_factor <- relevel(as.factor(all$age), ref = "17")

all$kid_feels_close_mom = ifelse(all$kid_feels_close_mom == 0, NA, all$kid_feels_close_mom)
all$kid_feels_close_dad = ifelse(all$kid_feels_close_dad == 0, NA, all$kid_feels_close_dad)
all$kid_feels_close_stepmom = ifelse(all$kid_feels_close_stepmom == 0, NA, all$kid_feels_close_stepmom)
all$kid_feels_close_stepdad = ifelse(all$kid_feels_close_stepdad == 0, NA, all$kid_feels_close_stepdad)
all$kid_feels_close_parents = rowMeans(all[,c("kid_feels_close_mom",                                
                                              "kid_feels_close_dad",                                 
                                              "kid_feels_close_stepmom",                             
                                              "kid_feels_close_stepdad")], na.rm = T)

# 1.  In school, single
all$SCH = ifelse(all$in_school_or_coll == 1 , 1, 0)

# 3.  NW NS, single
all$NWS = ifelse(all$no_school_no_work == 1, 1, 0)

# 5.  Working, single
all$W = ifelse(all$working_now_no_school == 1 , 1, 0)

# 6.  Working, married/cohab
all$M = ifelse(all$married_binary == 1 , 1, 0)

all$C = ifelse(all$cohab_binary == 1 , 1, 0)

all$S = ifelse(all$marriage_cohabitation_binary == 0 , 1, 0)

# Final state school work
school.states = c("SCH", "NWS", "W")

all$final_school_state = NA
for ( i in school.states ) {
  all$final_school_state = ifelse(all[,i] == 1, i, all$final_school_state)
}

# final state for marriage cohabitation
marital.states = c("M", "C", "S")

all$final_marital_state = NA
for ( i in marital.states ) {
  all$final_marital_state = ifelse(all[,i] == 1, i, all$final_marital_state)
}

# 1.  In school, single, totally independent 
all$TI = ifelse(all$totally_ind == 1, 1, 0)

# 2.  In school, single, F dependent, R independent
all$FS = ifelse(all$F_dep_R_ind == 1, 1, 0)

# 3.  In school, single, F independent, R dependent
all$RS = ifelse(all$F_ind_R_dep == 1, 1, 0)

# 4.  In school, single, totally dependent 
all$RS_FS = ifelse(all$totally_dep == 1, 1, 0)

# Final state
ind.states = c("TI", "FS", "RS", "RS_FS")

all$final_ind_state = NA
for ( i in ind.states ) {
  all$final_ind_state = ifelse(all[,i] == 1, i, all$final_ind_state)
}

last_year <- all %>%
  group_by(UID_2) %>%
  summarise(final_year = max(year)) %>%
  ungroup()

# Overall measure of having children, issue is that it doesn't really have temporal resolution
# So I limited to situations where birth data is up-to-date by the person's last wave
all$final_year = last_year$final_year[match(all$UID_2, last_year$UID_2)]
all$last_year_birth_info = as.numeric(all$ER32021_year_birth_info_most_recently_updated__all)
all$last_year_birth_info = ifelse(all$last_year_birth_info > 2023, NA, all$last_year_birth_info)
valid_entries = all$last_year_birth_info >= all$final_year

all$live_births = ifelse(valid_entries, all$ER32022__live_births_to_this_individual__all, NA)
all$live_births = ifelse(all$live_births > 9, NA, all$live_births)
all$live_births = as.numeric(all$live_births)

# Arrange by person + year
all = all %>%
  arrange(psid68_UID, year)

all = all %>%
  group_by(psid68_UID) %>%
  mutate(
    lagged_births = lag(number_births, order_by = year)
  ) %>%
  ungroup()

all$lagged_births = ifelse(all$number_births == 0 & is.na(all$lagged_births), 0, all$lagged_births)
all$transition_to_childbearing = ifelse(all$lagged_births == 0 & all$number_births > 0, 1, 0)
all$had_child_between_waves = ifelse(all$number_births > all$lagged_births, 1, 0)
all$is_parent = ifelse(all$number_births > 0, 1, 0)

all$parents_currently_married = 1-all$currently_divorced


eha_sibs = all %>%
  arrange(psid68_UID, year)

eha_sibs = eha_sibs %>%
  group_by(psid68_UID) %>%
  mutate(
    transition_to_total_dependence = if_else(RS_FS == 1 & lag(RS_FS) == 0, 1, 0),
    transition_to_total_independence = if_else(TI == 1 & lag(TI) == 0, 1, 0),
    transition_out_total_independence = if_else(TI == 0 & lag(TI) == 1, 1, 0),
    transition_out_total_dependence = if_else(RS_FS == 0 & lag(RS_FS) == 1, 1, 0),
    transition_to_marriage = if_else(M == 1 & lag(M) == 0, 1, 0),
    transition_to_single = if_else(S == 1 & lag(S) == 0, 1, 0),
    transition_to_cohab = if_else(C == 1 & lag(C) == 0, 1, 0),
    transition_to_working = if_else(W == 1 & lag(W) == 0, 1, 0),
    transition_to_notworking = if_else(NWS == 1 & lag(NWS) == 0, 1, 0),
    transition_to_school = if_else(S == 1 & lag(S) == 0, 1, 0),
    transition_to_resdep = if_else(residentially_dep == 1 & lag(residentially_dep) == 0, 1, 0),
    transition_to_findep = if_else(financially_dep == 1 & lag(financially_dep) == 0, 1, 0),
  ) %>%
  ungroup()

eha_sibs_w_lagged = eha_sibs %>%
  group_by(psid68_UID) %>%
  mutate(
    lagged_TI = lag(TI, order_by = year),
    lagged_RS_FS = lag(RS_FS, order_by = year),
    lagged_FS = lag(FS, order_by = year),
    lagged_RS = lag(RS, order_by = year),
    lagged_res_dep = lag(residentially_dep, order_by = year),
    lagged_fin_dep = lag(financially_dep, order_by = year),
    lagged_M = lag(M, order_by = year),
    lagged_C = lag(C, order_by = year),
    lagged_S = lag(S, order_by = year),
    lagged_W = lag(W, order_by = year),
    lagged_SCH = lag(SCH, order_by = year),
    lagged_NWS = lag(NWS, order_by = year),
    lagged_parent = lag(is_parent, order_by = year)
  ) %>%
  ungroup()

# Transitions into total independence
eha_sibs_w_lagged$age_squared = eha_sibs_w_lagged$age * eha_sibs_w_lagged$age
eha_sibs_w_lagged$pandemic_year = ifelse(eha_sibs_w_lagged$year == 2021, 1, 0)

covars_btw_fam = c('wealth_quintiles', 
                   'race_cat',
                   'parents_currently_married',
                   'number_children_mother',
                   'mother_edu',
                   'metro_psid',
                   'female',
                   'fam_income_quintiles', 
                   'age_factor',
                   'unemployed',
                   'pandemic_year')

covars_cds = c('working_now',
           'wealth_quintiles', 
           'standardized_gpa_imp', 
           'race_cat',
           'parents_currently_married',
           'number_children_mother',
           'mother_edu',
           'metro_psid',
           'marriage_cohabitation_cat',
           'LCA_ThreeClass_Cat',
           'kid_feels_close_parents',
           'join_year',
           'in_college_now',
           'helped_sib',
           'health_level',
           'first_born',
           'female',
           'fam_income_quintiles', 
           'earnings_work_log_imp',
           'age_factor',
           'pandemic_year')

final_covars_btw = c('psid68_UID', 'age', 'TI', 'transition_to_total_independence', 'transition_out_total_independence', 'lagged_M', 'lagged_C', 'lagged_W', 'lagged_NWS', 'lagged_SCH', 'lagged_FS', 'lagged_RS', 'lagged_TI', 'lagged_RS_FS', 'lagged_res_dep', 'lagged_fin_dep', 'transition_to_findep', 'transition_to_resdep', covars_btw_fam)

# TRANSITION MODELS:
df1 = eha_sibs_w_lagged[,c('psid68_UID', 'sib_group', 'age', 'TI', 'transition_to_total_independence', 'transition_out_total_independence','transition_to_findep', 'transition_to_resdep', 'lagged_M', 'lagged_C', 'lagged_W', 'lagged_NWS', 'lagged_SCH', 'lagged_FS', 'lagged_RS', 'lagged_res_dep', 'lagged_fin_dep', 'lagged_TI', 'lagged_RS_FS', covars_btw_fam)]
df1 = na.omit(df1)
df1$age2 = df1$age + 1

complete_sibgroups = table(df1$sib_group)
good_sibgroups = names(complete_sibgroups)[complete_sibgroups > 1]
df1 = subset(df1, sib_group %in% good_sibgroups)

# PURE BETWEEN FAMILY
# TRANSITION TO INDEP
mixed_model1 <- coxme(Surv(time = age, time2 = age2, transition_to_total_independence) ~ female + race_cat + 
                        mother_edu + parents_currently_married + number_children_mother + 
                        fam_income_quintiles + wealth_quintiles + metro_psid + pandemic_year + 
                        lagged_M + lagged_C + lagged_NWS + lagged_SCH +
                        (1|sib_group/psid68_UID), data = df1)

summary(mixed_model1)

# WITH INT
mixed_model1_int <- coxme(Surv(time = age, time2 = age2, transition_to_total_independence) ~ female * lagged_M + 
                        race_cat + mother_edu + parents_currently_married + number_children_mother + 
                        fam_income_quintiles + wealth_quintiles + metro_psid + 
                        lagged_M + lagged_C + lagged_NWS + lagged_SCH + pandemic_year + 
                        (1 | sib_group/psid68_UID), data = df1)

summary(mixed_model1_int)

mixed_model1_int2 <- coxme(Surv(time = age, time2 = age2, transition_to_total_independence) ~ female * lagged_SCH + 
                        race_cat + mother_edu + parents_currently_married + number_children_mother + 
                        fam_income_quintiles + wealth_quintiles + metro_psid + 
                        lagged_M + lagged_C + lagged_NWS + lagged_SCH + pandemic_year + 
                        (1 | sib_group/psid68_UID), data = df1)

summary(mixed_model1_int2)


mixed_model1_int3 <- coxme(Surv(time = age, time2 = age2, transition_to_total_independence) ~ mother_edu * lagged_M + female + 
                        race_cat + mother_edu + parents_currently_married + number_children_mother + 
                        fam_income_quintiles + wealth_quintiles + metro_psid + 
                        lagged_M + lagged_C + lagged_NWS + lagged_SCH + pandemic_year + 
                        (1 | sib_group/psid68_UID), data = df1)

summary(mixed_model1_int3)


mixed_model1_int4 <- coxme(Surv(time = age, time2 = age2, transition_to_total_independence) ~ mother_edu * lagged_SCH + female + 
                        race_cat + mother_edu + parents_currently_married + number_children_mother + 
                        fam_income_quintiles + wealth_quintiles + metro_psid + 
                        lagged_M + lagged_C + lagged_NWS + lagged_SCH + pandemic_year + 
                        (1 | sib_group/psid68_UID), data = df1)

summary(mixed_model1_int4)


mixed_model1_int5 <- coxme(Surv(time = age, time2 = age2, transition_to_total_independence) ~ wealth_quintiles * lagged_M + female + 
                        race_cat + mother_edu + parents_currently_married + number_children_mother + 
                        fam_income_quintiles + wealth_quintiles + metro_psid + 
                        lagged_M + lagged_C + lagged_NWS + lagged_SCH + pandemic_year + 
                        (1 | sib_group/psid68_UID), data = df1)

summary(mixed_model1_int5)


mixed_model1_int6 <- coxme(Surv(time = age, time2 = age2, transition_to_total_independence) ~ wealth_quintiles * lagged_SCH + female + 
                        race_cat + mother_edu + parents_currently_married + number_children_mother + 
                        fam_income_quintiles + wealth_quintiles + metro_psid + 
                        lagged_M + lagged_C + lagged_NWS + lagged_SCH + pandemic_year + 
                        (1 | sib_group/psid68_UID), data = df1)

summary(mixed_model1_int6)

mixed_model1_int7 <- coxme(Surv(time = age, time2 = age2, transition_to_total_independence) ~ fam_income_quintiles * lagged_M + female + 
                        race_cat + mother_edu + parents_currently_married + number_children_mother + 
                        fam_income_quintiles + wealth_quintiles + metro_psid + 
                        lagged_M + lagged_C + lagged_NWS + lagged_SCH + pandemic_year + 
                        (1 | sib_group/psid68_UID), data = df1)

summary(mixed_model1_int7)

mixed_model1_int8 <- coxme(Surv(time = age, time2 = age2, transition_to_total_independence) ~ fam_income_quintiles * lagged_SCH + female + 
                        race_cat + mother_edu + parents_currently_married + number_children_mother + 
                        fam_income_quintiles + wealth_quintiles + metro_psid + 
                        lagged_M + lagged_C + lagged_NWS + lagged_SCH + pandemic_year + 
                        (1 | sib_group/psid68_UID), data = df1)

summary(mixed_model1_int8)


mixed_model1_res <- coxme(Surv(time = age, time2 = age2, transition_to_resdep) ~ female + race_cat + 
                        mother_edu + parents_currently_married + number_children_mother + 
                        fam_income_quintiles + wealth_quintiles + metro_psid + pandemic_year + 
                        lagged_M + lagged_C + lagged_NWS + lagged_SCH +
                        (1|sib_group/psid68_UID), data = df1)

mixed_model1_fin <- coxme(Surv(time = age, time2 = age2, transition_to_findep) ~ female + race_cat + 
                        mother_edu + parents_currently_married + number_children_mother + 
                        fam_income_quintiles + wealth_quintiles + metro_psid + pandemic_year + 
                        lagged_M + lagged_C + lagged_NWS + lagged_SCH +
                        (1|sib_group/psid68_UID), data = df1)

summary(mixed_model1)


# TRANSITION TO MARRIAGE
df1_marr = eha_sibs_w_lagged[,c('psid68_UID', 'sib_group', 'age', 'transition_to_marriage', 'lagged_fin_dep', 'lagged_res_dep', 'lagged_M', 'lagged_C', 'lagged_W', 'lagged_NWS', 'lagged_SCH', 'lagged_FS', 'lagged_RS', 'lagged_TI', 'lagged_RS_FS', covars_btw_fam)]
df1_marr = na.omit(df1_marr)
df1_marr$age2 = df1_marr$age + 1

complete_sibgroups = table(df1_marr$sib_group)
good_sibgroups = names(complete_sibgroups)[complete_sibgroups > 1]
df1_marr = subset(df1_marr, sib_group %in% good_sibgroups)


mixed_model1_marr <- coxme(Surv(time = age, time2 = age2, transition_to_marriage) ~ female + 
                            race_cat + mother_edu + parents_currently_married + number_children_mother + 
                            fam_income_quintiles + wealth_quintiles + metro_psid + 
                            lagged_TI + lagged_C + lagged_NWS + lagged_SCH +  pandemic_year + 
                            (1 | sib_group/psid68_UID), data = df1_marr)

summary(mixed_model1_marr)

mixed_model1_marr_int <- coxme(Surv(time = age, time2 = age2, transition_to_marriage) ~ female * lagged_TI + 
                             race_cat + mother_edu + parents_currently_married + number_children_mother + 
                             fam_income_quintiles + wealth_quintiles + metro_psid + 
                             lagged_TI + lagged_C + lagged_NWS + lagged_SCH + pandemic_year + 
                             (1 | sib_group/psid68_UID), data = df1_marr)

summary(mixed_model1_marr_int)

mixed_model1_marr2 <- coxme(Surv(time = age, time2 = age2, transition_to_marriage) ~ female + 
                            race_cat + mother_edu + parents_currently_married + number_children_mother + 
                            fam_income_quintiles + wealth_quintiles + metro_psid + 
                            lagged_fin_dep + lagged_res_dep + lagged_C + lagged_NWS + lagged_SCH +  pandemic_year + 
                            (1 | sib_group/psid68_UID), data = df1_marr)


# TRANSITION TO CHILDBEARING
df1_cb = eha_sibs_w_lagged[,c('psid68_UID', 'sib_group', 'age', 'transition_to_childbearing', 'lagged_fin_dep', 'lagged_res_dep', 'lagged_M', 'lagged_C', 'lagged_W', 'lagged_NWS', 'lagged_SCH', 'lagged_FS', 'lagged_RS', 'lagged_TI', 'lagged_RS_FS', covars_btw_fam)]
df1_cb = na.omit(df1_cb)
df1_cb$age2 = df1_cb$age + 1

complete_sibgroups = table(df1_cb$sib_group)
good_sibgroups = names(complete_sibgroups)[complete_sibgroups > 1]
df1_cb = subset(df1_cb, sib_group %in% good_sibgroups)

mixed_model1_cb <- coxme(Surv(time = age, time2 = age2, transition_to_childbearing) ~ female + 
                             race_cat + mother_edu + parents_currently_married + number_children_mother + 
                             fam_income_quintiles + wealth_quintiles + metro_psid + 
                             lagged_TI + lagged_M + lagged_C + lagged_NWS + lagged_SCH +  pandemic_year + 
                             (1 | sib_group/psid68_UID), data = df1_cb)

summary(mixed_model1_cb)

mixed_model1_cb_int <- coxme(Surv(time = age, time2 = age2, transition_to_childbearing) ~ female * lagged_TI + 
                               race_cat + mother_edu + parents_currently_married + number_children_mother + 
                               fam_income_quintiles + wealth_quintiles + metro_psid + 
                               lagged_TI + lagged_M + lagged_C + lagged_NWS + lagged_SCH +  pandemic_year + 
                               (1 | sib_group/psid68_UID), data = df1_cb)

summary(mixed_model1_cb_int)

mixed_model1_cb_int2 <- coxme(Surv(time = age, time2 = age2, transition_to_childbearing) ~ female * lagged_M + 
                               race_cat + mother_edu + parents_currently_married + number_children_mother + 
                               fam_income_quintiles + wealth_quintiles + metro_psid + 
                               lagged_TI + lagged_M + lagged_C + lagged_NWS + lagged_SCH +  pandemic_year + 
                               (1 | sib_group/psid68_UID), data = df1_cb)

summary(mixed_model1_cb_int2)

mixed_model1_cb2 <- coxme(Surv(time = age, time2 = age2, transition_to_childbearing) ~ female + 
                             race_cat + mother_edu + parents_currently_married + number_children_mother + 
                             fam_income_quintiles + wealth_quintiles + metro_psid + 
                             lagged_fin_dep + lagged_res_dep + lagged_M + lagged_C + lagged_NWS + lagged_SCH +  pandemic_year + 
                             (1 | sib_group/psid68_UID), data = df1_cb)

summary(mixed_model1_cb2)


coxme_stargazer(mixed_model1, "Transition to Independence")
coxme_stargazer(mixed_model1_int, "Transition to Independence")
coxme_stargazer(mixed_model1_int2, "Transition to Independence")
coxme_stargazer(mixed_model1_int3, "Transition to Independence")
coxme_stargazer(mixed_model1_int4, "Transition to Independence")
coxme_stargazer(mixed_model1_int5, "Transition to Independence")
coxme_stargazer(mixed_model1_int6, "Transition to Independence")
coxme_stargazer(mixed_model1_int7, "Transition to Independence")
coxme_stargazer(mixed_model1_int8, "Transition to Independence")

coxme_stargazer(mixed_model1_marr, "Transition to Marriage")
coxme_stargazer(mixed_model1_marr_int, "Transition to Marriage")

coxme_stargazer(mixed_model1_cb, "Transition to Child-bearing")
coxme_stargazer(mixed_model1_cb_int, "Transition to Child-bearing")
coxme_stargazer(mixed_model1_cb_int2, "Transition to Child-bearing")



### SUMMARIZE MODELS:
coxme_stargazer = function(x, dep.var = "Transition into Independence"){
  # Extract coefficients and standard errors
  coefs <- coef(summary(x))
  coefs_df <- as.data.frame(coefs)
  
  # Create a data frame for stargazer
  model_results <- data.frame(
    term = rownames(coefs_df),
    estimate = coefs_df[, "coef"],
    std.error = coefs_df[, "se(coef)"],
    p.value = coefs_df[, "p"]
  )
  
  model_results$stars = ifelse(model_results$p.value < 0.05, '*', '')
  model_results$stars = ifelse(model_results$p.value < 0.01, '**', model_results$stars)
  model_results$stars = ifelse(model_results$p.value < 0.001, '***', model_results$stars)
  
  # Convert term names to match what stargazer expects
  rownames(model_results) <- NULL
  
  stargazer(
    as.data.frame(model_results),
    type = "text",
    summary = FALSE,
    title = dep.var,
    digits = 3,
    dep.var.labels = dep.var,
    coef = list(model_results$estimate),
    se = list(model_results$std.error),
    p = list(model_results$p.value)
  )
}

# COXME RESULTS
coxme_stargazer(mixed_model1_sibs)
coxme_stargazer(mixed_model2_sibs)
coxme_stargazer(mixed_model3_sibs, "Transition out of Independence")
coxme_stargazer(mixed_model4_sibs, "Transition out of Independence")
