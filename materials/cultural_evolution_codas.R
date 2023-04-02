####### An analysis of sperm whale codas relative to the neutral model of cultural transmission #######

# Based on a dataset of sperm whale codas from the Pacific Ocean 
# From: Evidence from sperm whale clans of symbolic marking in non-human cultures (Hersh et al., 2022)
# Available at: https://osf.io/ae6pd/

# Updated by David Schwitzgebel, 01-04-2023
# Previous versions by Axel Molina & Olivier Morin

##### 0. Loading packages & data #####

library(dplyr)
library(poweRlaw)

pacific_coda_data <- load(file="pacific_coda_identity.Rdata") %>%
  filter(!is.na(coda_type)) # exclude unknown coda types

###### 1. Preparing & plotting rank frequency distributions for each clan ######

coda_reg <- pacific_coda_data %>%
  filter(clan_name == "REG")

coda_po <- pacific_coda_data %>%
  filter(clan_name == "PO")

coda_sh <- pacific_coda_data %>%
  filter(clan_name == "SH")

coda_ri <- pacific_coda_data %>%
  filter(clan_name == "RI")

coda_pali <- pacific_coda_data %>%
  filter(clan_name == "PALI")

coda_fp <- pacific_coda_data %>%
  filter(clan_name == "FP")

coda_si <- pacific_coda_data %>%
  filter(clan_name == "SI")

# REG

reg_coda_total_n <- sum(coda_reg %>%
                                summarize(n = n()) %>%
                                select(n))

reg_coda_rankf <- coda_reg %>%
  group_by(coda_type) %>%
  summarize(n = n()) %>%
  mutate(frequency = n/reg_coda_total_n) %>% 
  mutate(rank = as.integer(rank(frequency)))

plot(reg_coda_rankf$frequency, reg_coda_rankf$rank)
plot(log(reg_coda_rankf$frequency), reg_coda_rankf$rank)

# PO

po_coda_total_n <- sum(coda_po %>%
                          summarize(n = n()) %>%
                          select(n))

po_coda_rankf <- coda_po %>%
  group_by(coda_type) %>%
  summarize(n = n()) %>%
  mutate(frequency = n/po_coda_total_n) %>% 
  mutate(rank = as.integer(rank(frequency)))

plot(po_coda_rankf$frequency, po_coda_rankf$rank)
plot(log(po_coda_rankf$frequency), po_coda_rankf$rank)

# SH

sh_coda_total_n <- sum(coda_sh %>%
                         summarize(n = n()) %>%
                         select(n))

sh_coda_rankf <- coda_sh %>%
  group_by(coda_type) %>%
  summarize(n = n()) %>%
  mutate(frequency = n/sh_coda_total_n) %>% 
  mutate(rank = as.integer(rank(frequency)))

plot(sh_coda_rankf$frequency, sh_coda_rankf$rank)
plot(log(sh_coda_rankf$frequency), sh_coda_rankf$rank)

# RI

ri_coda_total_n <- sum(coda_ri %>%
                         summarize(n = n()) %>%
                         select(n))

ri_coda_rankf <- coda_ri %>%
  group_by(coda_type) %>%
  summarize(n = n()) %>%
  mutate(frequency = n/ri_coda_total_n) %>% 
  mutate(rank = as.integer(rank(frequency)))

plot(ri_coda_rankf$frequency, ri_coda_rankf$rank)
plot(log(ri_coda_rankf$frequency), ri_coda_rankf$rank)

# PALI

pali_coda_total_n <- sum(coda_pali %>%
                         summarize(n = n()) %>%
                         select(n))

pali_coda_rankf <- coda_pali %>%
  group_by(coda_type) %>%
  summarize(n = n()) %>%
  mutate(frequency = n/pali_coda_total_n) %>% 
  mutate(rank = as.integer(rank(frequency)))

plot(pali_coda_rankf$frequency, pali_coda_rankf$rank)
plot(log(pali_coda_rankf$frequency), pali_coda_rankf$rank)

# FP

fp_coda_total_n <- sum(coda_fp %>%
                           summarize(n = n()) %>%
                           select(n))

fp_coda_rankf <- coda_fp %>%
  group_by(coda_type) %>%
  summarize(n = n()) %>%
  mutate(frequency = n/fp_coda_total_n) %>% 
  mutate(rank = as.integer(rank(frequency)))

plot(fp_coda_rankf$frequency, fp_coda_rankf$rank)
plot(log(fp_coda_rankf$frequency), fp_coda_rankf$rank)

# SI

si_coda_total_n <- sum(coda_si %>%
                         summarize(n = n()) %>%
                         select(n))

si_coda_rankf <- coda_si %>%
  group_by(coda_type) %>%
  summarize(n = n()) %>%
  mutate(frequency = n/si_coda_total_n) %>% 
  mutate(rank = as.integer(rank(frequency)))

plot(si_coda_rankf$frequency, si_coda_rankf$rank)
plot(log(si_coda_rankf$frequency), si_coda_rankf$rank)

###### 2. Preparing & plotting composite rank frequency distribution ######

composite_coda_total_n <- sum(pacific_coda_data %>%
                                summarize(n = n()) %>%
                                select(n))

composite_coda_rankf <- pacific_coda_data %>%
  group_by(coda_type) %>%
  summarize(n = n()) %>%
  mutate(frequency = n/composite_coda_total_n) %>% 
  mutate(rank = as.integer(rank(frequency)))

plot(composite_coda_rankf$frequency, composite_coda_rankf$rank)
plot(log(composite_coda_rankf$frequency), composite_coda_rankf$rank)

###### 3. Preparing & plotting rank frequency distribution for identity & non-identity codas ######

coda_identity <- pacific_coda_data %>%
  filter(identity == 1)

coda_non_identity <- pacific_coda_data %>%
  filter(identity == 0)

# Identity

identity_coda_total_n <- sum(coda_identity %>%
                         summarize(n = n()) %>%
                         select(n))

identity_coda_rankf <- coda_identity %>%
  group_by(coda_type) %>%
  summarize(n = n()) %>%
  mutate(frequency = n/identity_coda_total_n) %>% 
  mutate(rank = as.integer(rank(frequency)))

plot(identity_coda_rankf$frequency, identity_coda_rankf$rank)
plot(log(identity_coda_rankf$frequency), identity_coda_rankf$rank)

#Non-identity

non_identity_coda_total_n <- sum(coda_non_identity %>%
                               summarize(n = n()) %>%
                               select(n))

non_identity_coda_rankf <- coda_non_identity %>%
  group_by(coda_type) %>%
  summarize(n = n()) %>%
  mutate(frequency = n/non_identity_coda_total_n) %>% 
  mutate(rank = as.integer(rank(frequency)))

plot(non_identity_coda_rankf$frequency, non_identity_coda_rankf$rank)
plot(log(non_identity_coda_rankf$frequency), non_identity_coda_rankf$rank)

###### 4. Hypothesis testing for goodness-of-fit with simulated power law distribution ###### 

# The package poweRlaw allows us to investigate how well a dataset corresponds to a simulated
# model of the power law distribution (bootstrapped from the estimated parameters of the input). 
# It provides an estimated p-value based on the hypothesis test where H0: the distribution conforms
# to the power law model; H1: the distribution does not conform to the power law model

# So if p > .05, we cannot conclude that the dataset does not conform to a power law distribution
# (so it plausibly follows the power law distribution); if p < .05, we conclude that the
# dataset does not conform to a power law distribution. 

reg_dist <- displ$new(as.integer(sort(reg_coda_rankf$n, decreasing = TRUE)))
po_dist <- displ$new(as.integer(sort(po_coda_rankf$n, decreasing = TRUE)))
sh_dist <- displ$new(as.integer(sort(sh_coda_rankf$n, decreasing = TRUE)))
ri_dist <- displ$new(as.integer(sort(ri_coda_rankf$n, decreasing = TRUE)))
pali_dist <- displ$new(as.integer(sort(pali_coda_rankf$n, decreasing = TRUE)))
fp_dist <- displ$new(as.integer(sort(fp_coda_rankf$n, decreasing = TRUE)))
si_dist <- displ$new(as.integer(sort(si_coda_rankf$n, decreasing = TRUE)))
composite_dist <- displ$new(as.integer(sort(composite_coda_rankf$n, decreasing = TRUE)))
identity_dist <- displ$new(as.integer(sort(identity_coda_rankf$n, decreasing = TRUE)))
non_identity_dist <- displ$new(as.integer(sort(non_identity_coda_rankf$n, decreasing = TRUE)))

reg_test <- bootstrap_p(reg_dist)
po_test <- bootstrap_p(po_dist)
sh_test <- bootstrap_p(sh_dist)
ri_test <- bootstrap_p(ri_dist)
pali_test <- bootstrap_p(pali_dist)
fp_test <- bootstrap_p(fp_dist)
si_test <- bootstrap_p(si_dist)
composite_test <- bootstrap_p(composite_dist)
identity_test <- bootstrap_p(identity_dist)
non_identity_test <- bootstrap_p(non_identity_dist)

reg_test$p # p > .05
po_test$p # p > .05 
sh_test$p # p > .05 
ri_test$p # p > .05 
pali_test$p # p > .05 
fp_test$p # p > .05
si_test$p # p > .05 
composite_test$p # p > .05 
identity_test$p # p > .05
non_identity_test$p # p > .05 

###### 5. duration-frequency analysis ######

# Under the assumptions of the neutral model, cultural transmission of a given
# trait is unbiased: when encountered, all traits are equally likely to be adopted
# (so exposure is the only factor that matters). Therefore, under the neutral model,
# there should *not* be a relationship between whale coda frequency and duration.

# I hypothesized that lower-duration codas might have increased frequency, because they
# would be easier to communicate (and therefore expend less energy). That would be a 
# violation of the neutral model, because it would mean a factor other than mere exposure
# influences transmission. 

# Main analysis

composite_coda_duration_frequency <- pacific_coda_data %>%
  group_by(coda_type) %>%
  summarize(n = n(), duration = mean(duration))

composite_coda_duration_frequency <- composite_coda_duration_frequency %>%
  mutate(frequency = log(n/composite_coda_total_n)) # Using the log-transformed data in order to avoid violating the assumptions of a linear model 

summary(lm(frequency ~ duration, data=composite_coda_duration_frequency))
# p < .001

plot(composite_coda_duration_frequency$duration, composite_coda_duration_frequency$frequency)

abline(lm(frequency ~ duration, data=composite_coda_duration_frequency),
       col = "red")

# Outlier exclusions

sd_duration <- sd(composite_coda_duration_frequency$duration)*3
sd_frequency <- sd(composite_coda_duration_frequency$frequency)*3
max_duration = mean(composite_coda_duration_frequency$duration) + sd_duration
min_duration = mean(composite_coda_duration_frequency$duration) - sd_duration
max_frequency = mean(composite_coda_duration_frequency$frequency) + sd_frequency
min_frequency = mean(composite_coda_duration_frequency$frequency) - sd_frequency

corrected_coda_duration_frequency <- composite_coda_duration_frequency %>%
  filter(duration < max_duration & duration > min_duration) %>%
  filter(frequency < max_frequency & frequency > min_frequency)
  
summary(lm(frequency ~ duration, data=corrected_coda_duration_frequency))
# p < .01

plot(corrected_coda_duration_frequency$duration, corrected_coda_duration_frequency$frequency)

abline(lm(frequency ~ duration, data=corrected_coda_duration_frequency),
       col = "red")

# Identity analysis

identity_coda_duration_frequency <- pacific_coda_data %>%
  group_by(coda_type, identity) %>%
  summarize(n = n(), duration = mean(duration))

identity_coda_duration_frequency <- identity_coda_duration_frequency %>%
  mutate(frequency = log(n/composite_coda_total_n)) %>%
  filter(duration < max_duration & duration > min_duration) %>%
  filter(frequency < max_frequency & frequency > min_frequency)

summary(lm(frequency ~ duration*identity, data=identity_coda_duration_frequency))
# p < .001 for main effect of duration
# p > .05 for main effect of identity
# p < .05 for duration*identity interaction

identity_coda_duration_frequency_id <- identity_coda_duration_frequency %>%
  filter(identity == 1)

summary(lm(frequency ~ duration, data=identity_coda_duration_frequency_id))
# p > .05 selectively for identity codas

identity_coda_duration_frequency_non <- identity_coda_duration_frequency %>%
  filter(identity == 0)

summary(lm(frequency ~ duration, data=identity_coda_duration_frequency_non))
# p < .001 selectively for non-identity codas

plot(identity_coda_duration_frequency_id$duration, identity_coda_duration_frequency_id$frequency)

abline(lm(frequency ~ duration, data=identity_coda_duration_frequency_id),
       col = "red")

plot(identity_coda_duration_frequency_non$duration, identity_coda_duration_frequency_non$frequency)

abline(lm(frequency ~ duration, data=identity_coda_duration_frequency_non),
       col = "red")





