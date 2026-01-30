#All r2 household-level exposure data with r2 individual-level vax data (India & Peru)
#1/30/26
#Author: Brittany Shea
#Data: Boyden, J. (2025). Young Lives: an International Study of Childhood Poverty: 
  #Rounds 1-5 Constructed Files, 2002-2016. [data collection]. 5th Edition. UK Data Service. SN: 7483, DOI: http://doi.org/10.5255/UKDA-SN-7483-5

#Load the libraries
library(tidyverse)
library(haven)
library(dplyr)
library(ggplot2)
library(scales)
library(readxl)
library(janitor)
library(knitr)
library(glmmTMB)
library(lme4)
library(GGally)
library(reshape2)
library(car)
library(correlation)
library(qgraph)
library(lavaan)
library(ggeffects)
library(DHARMa)
library(forcats)
library(marginaleffects)

#Format for plots
knitr::opts_chunk$set(
  echo = TRUE,
  warning = FALSE,
  fig.width = 8, 
  fig.height = 6,
  out.width = "90%"
)

options(
  ggplot2.continuous.colour = "viridis",
  ggplot2.continuous.fill = "viridis"
)

scale_colour_discrete = scale_colour_viridis_d
scale_fill_discrete = scale_fill_viridis_d

theme_set(theme_minimal() + theme(legend.position = "bottom"))

#Load the data
#r1
r1_nd_obe_ne <- read.csv("./code/cleaning/ych_main/data_ych/r1_pluspolio_nd_obe_ne.csv")

#r2
r2_expandout_ind_in_ych <- read.csv("./code/cleaning/ych_main/data_ych/r2individualexposure_r2outcome_in_ych.csv")
r2_expandout_ind_pe_ych <- read.csv("./code/cleaning/ych_main/data_ych/r2individualexposure_r2outcome_pe_ych.csv")

#-------------------------
#India
#exposed group = exposed to extreme weather events
#unexposed group = unexposed to extreme weather events (remove individuals exposed
#to other types of natural disasters that are not also exposed to extreme weather events)

#clean df/create exposure variable
r2_in = r2_expandout_ind_in_ych %>% 
  mutate(exposure = case_when(
    event24 == 1 | event25 == 1 | event26 == 1 | event27 == 1 ~ 1,
    event24 == 0 & event25 == 0 & event26 == 0 & event27 == 0 ~ 0,
    TRUE ~ NA_real_)) %>% 
  distinct(childid, vax_type, .keep_all = TRUE) %>% 
  mutate(country = substr(childid, 1, 2)) %>%
  mutate(clustid = if_else(is.na(clustid), as.character(clustid), paste(country, clustid, sep = "_"))) %>% 
  mutate(region = if_else(is.na(region), as.character(region), paste(country, region, sep = "_"))) %>% 
  #filter(!is.na(vax_status)) %>%
  #filter(!is.na(exposure)) %>% 
  select(round, childid, country, region, clustid, commid, exposure, vax_status, 
         vax_type, wi, agechild, biomum, primum, event24, event25, event26, event27, event28,
         event29, event30, event31)

#table of childid by exposure/unexposed
r2_in %>%
  group_by(exposure) %>%
  summarise(n = n_distinct(childid))

#childids exposure versus not exposure by vax_timing
r2_in %>%
  group_by(vax_status, vax_type, exposure) %>%
  summarise(n = n()) %>%
  ungroup()

#-------------------------
#Peru
#clean df/create exposure variable
r2_pe = r2_expandout_ind_pe_ych %>% 
  mutate(exposure = case_when(
    event24 == 1 | event25 == 1 | event26 == 1 | event27 == 1 ~ 1,
    event24 == 0 & event25 == 0 & event26 == 0 & event27 == 0 ~ 0,
    TRUE ~ NA_real_)) %>%
  distinct(childid, vax_type, .keep_all = TRUE) %>%
  mutate(country = substr(childid, 1, 2)) %>%
  mutate(clustid = if_else(is.na(clustid), as.character(clustid), paste(country, clustid, sep = "_"))) %>% 
  mutate(region = if_else(is.na(region), as.character(region), paste(country, region, sep = "_"))) %>% 
  #filter(!is.na(vax_status)) %>%
  select(round, childid, country, region, clustid, commid, exposure, vax_status, 
         vax_type, wi, agechild, biomum, primum, event24, event25, event26, event27, event58, event59,
         event28, event29, event30, event31, event14)

#table of childid by exposure/unexposed
r2_pe %>%
  group_by(exposure) %>%
  summarise(n = n_distinct(childid))

#count exposure rows 
r2_pe %>%
  summarise(n = sum(as.numeric(exposure == "1"), na.rm = TRUE))

#childids exposed versus not exposed by vax_timing
r2_pe %>%
  group_by(vax_status, vax_type, exposure) %>%
  summarise(n = n()) %>%
  ungroup()

#-------------------------
#Combine the 2 country data
r2_join <- bind_rows(r2_in, r2_pe)

r2_join = r2_join %>% 
  select(round, country, childid, clustid, commid, exposure, vax_status, vax_type, wi, 
         agechild, event24, event25, event26, event27, event58, event59, everything())

#filter hvb and hib vaccinations since not widely introduced & hvb only asked about in Peru
r2_join_filtered = r2_join %>% 
  filter(vax_type != "vax_hvb" & vax_type != "vax_hib")

#-------------------------
#join r2 to r1 (r1_nd_obe_ne) so childid not considered unvaccinated if
#vax received in r1 but not r2 & control for r1 exposure and vaccination status

#filter r1 nd_obe_ne to only india & peru
r1_nd_obe_ne_inpe <- r1_nd_obe_ne %>% 
  filter(country == "pe" | country == "in") %>% 
  mutate(round = as.numeric(round))

#perform join
r2_join_filtered$vax_status <- as.numeric(r2_join_filtered$vax_status)
r1_nd_obe_ne_inpe$vax_status <- as.numeric(r1_nd_obe_ne_inpe$vax_status)

r2_join_filtered$exposure <- as.numeric(r2_join_filtered$exposure)
r1_nd_obe_ne_inpe$exposure <- as.numeric(r1_nd_obe_ne_inpe$exposure)

combined_df <- bind_rows(r1_nd_obe_ne_inpe, r2_join_filtered)

#keep only children with 2 rounds of data, remove vax_status=NA, remove applicable 
#vax_types if there is no round 1 data
combined_df <- combined_df %>% 
  group_by(childid) %>%
  filter(n_distinct(round) == 2) %>%
  ungroup() %>% 
  group_by(childid, vax_type) %>%
  filter(
    (
      vax_type %in% c("vax_bcg", "vax_measles", "vax_polio") &
        (
          any(round == 1 & vax_status == 1) |
            (any(round == 1 & vax_status == 0) & any(round == 2 & !is.na(vax_status)))    
          )
    ) |
      !(vax_type %in% c("vax_bcg", "vax_measles", "vax_polio")) 
  ) %>% 
  filter(!is.na(vax_status)) %>% 
  ungroup() %>% 
  select(round, childid, country, region, clustid, commid, exposure, vax_status,
         vax_type, wi, agechild, everything())

#create a vax_status_prev variable that indicates previous vax_status for each childid/vax_type
combined_df <- combined_df %>%
  arrange(childid, vax_type, round) %>%
  group_by(childid, vax_type) %>%
  mutate(vax_status_prev = lag(vax_status)) %>%
  ungroup()

#create vax_status for r's 1 & 2
combined_df <- combined_df %>%
  group_by(childid, vax_type) %>% 
  mutate(
    vax_status_r1 = case_when(
      round == 1 & vax_status == 1 & vax_type %in% c("vax_inject", "vax_bcg", "vax_measles", "vax_polio") ~ "1",
      round == 2 ~ NA,
      TRUE ~ "0"),
    vax_status_r2 = case_when(
      round == 2 & vax_status == 1 & vax_status_prev != 1 & vax_type %in% c("vax_bcg", "vax_measles", "vax_polio") ~ "1",
      round == 2 & vax_status %in% c(0, 1) & vax_status_prev == 1 & vax_type %in% c("vax_bcg", "vax_measles", "vax_polio") ~ "1",
      round == 2 & vax_status == 0 & vax_status_prev == 0 & vax_type %in% c("vax_bcg", "vax_measles", "vax_polio") ~ "0",
      round == 2 & vax_status == 1 & vax_type %in% c("vax_dpt") ~ "1",
      round == 1 ~ NA,
      TRUE ~ "0",
      )) %>%
  ungroup() %>% 
    mutate(
      exposure_r1 = ifelse(round == 1, exposure, NA),
      exposure_r2 = ifelse(round == 2, exposure, NA)
      ) %>% 
  select(-exposure, -vax_status)

#combine vax_status_r1 and r2 into 1 column and exposure_r1 and r2 into one column
combined_df <- combined_df %>%
  arrange(childid, round) %>%
  mutate(
    vax_status = case_when(
      round == 1 ~ as.character(vax_status_r1),
      round == 2 ~ as.character(vax_status_r2),
      TRUE ~ NA_character_
  ),
    exposure = case_when(
      round == 1 ~ exposure_r1,
      round == 2 ~ exposure_r2,
      TRUE ~ NA_real_
  )
  ) 

#create column for lagged exposure  
exposure_r1 <- combined_df %>%
  filter(round == 1) %>%
  group_by(childid) %>%
  summarise(
    r1_exposure =  first(na.omit(as.numeric(as.character(exposure))))
    )
combined_df <- combined_df %>%
  left_join(exposure_r1, by = "childid") %>%
  mutate(
    lagged_exposure = if_else(round == 1, 0, r1_exposure)
  ) %>%
  select(-r1_exposure)

#create column for baseline vax status
baseline_data <- combined_df %>%
  filter(round == 1) %>%
  select(childid, vax_type, vax_status) %>%
  rename(r1_status = vax_status)
combined_df <- combined_df %>%
  left_join(baseline_data, by = c("childid", "vax_type")) %>%
  mutate(
    temp_status = as.numeric(as.character(r1_status)),
    baseline = if_else(round == 1, 0, temp_status)
  ) %>%
  select(-r1_status, -temp_status)

#convert variables to factors before running models
combined_df <- combined_df %>% 
  mutate(across(c(round, childid, country, commid, clustid, region, vax_status, baseline, 
                  lagged_exposure, exposure, vax_type), factor))

#-----------------------------------------
#run model for both countries with both rounds as exposure/outcome and controlling for baseline exposure/outcome
#filter out vax_inject bc only in r1 and dpt bc only in r2; make bcg reference group; filter exposure na's

no_dpt_df = combined_df %>% 
  filter(vax_type %in% c("vax_bcg", "vax_measles", "vax_polio")) %>% 
  mutate(vax_type = fct_relevel(vax_type, "vax_bcg")) %>% 
  mutate(agechild_centered = agechild - mean(agechild, na.rm = TRUE)) %>% 
  mutate(wi_centered = wi - mean(wi, na.rm = TRUE)) %>% 
  mutate(
    wi_centered = scale(wi_centered),
    agechild_centered = scale(agechild_centered)
  ) %>% 
  filter(!is.na(exposure))

model <- glmer(vax_status ~ exposure * vax_type + round + baseline + lagged_exposure + wi_centered +
                 agechild_centered + 
                 (1 | clustid),
               data = no_dpt_df, family = binomial, nAGQ=1, 
               control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000)))

summary(model)

##run model with only round 2 exposure/outcome and control for baseline exposure/outcome
r2_df = combined_df %>% 
  filter(round == "2") %>% 
  filter(vax_type %in% c("vax_bcg", "vax_measles", "vax_polio")) %>% 
  mutate(vax_type = fct_relevel(vax_type, "vax_bcg")) %>% 
  mutate(agechild_centered = agechild - mean(agechild, na.rm = TRUE)) %>% 
  mutate(wi_centered = wi - mean(wi, na.rm = TRUE)) %>% 
  mutate(
    wi_centered = scale(wi_centered),
    agechild_centered = scale(agechild_centered)
  ) %>% 
  filter(!is.na(exposure))

model <- glmer(vax_status ~ exposure * vax_type + baseline + lagged_exposure +
                 wi_centered + agechild_centered +
                 (1 | country/clustid/childid),
               data = r2_df, family = binomial, nAGQ=1, 
               control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000)))

summary(model)

#AIC to check model fit (lower AIC values indicate a better-fitting model)
AIC(model)

#DHARMa for residual diagnostics and model fit checks
sim_res <- simulateResiduals(model)
plot(sim_res)
testDispersion(sim_res)
testOutliers(sim_res)
testQuantiles(sim_res)
sim_res_grouped <- recalculateResiduals(sim_res, group = no_dpt_df$clustid)
plot(sim_res_grouped)

#compute average predicted outcome for each combination of exposure & moderator 
avg_predictions(model, by = c("exposure", "vax_type"))

#plot "average predicted probability that Y=1 for different values of moderator & exposure"
plot_predictions(model, by = c("vax_type", "exposure"))

#compute average counterfactual comparison
avg_comparisons(model, variables = "exposure")

#check for heterogeneity to see if effect of X on Y depends on Moderator 
avg_comparisons(model, variables = "exposure", by = "vax_type")

plot_comparisons(model, variables = "exposure", by = "vax_type") +
  labs(y = "Marginal Effect", x = "Vaccine Type") +
  scale_x_discrete(labels = c("vax_bcg" = "BCG", 
                              "vax_measles" = "Measles",
                              "vax_polio" = "Polio")) +
  theme(text = element_text(size = 14)) +
  aes(color = vax_type) + 
  scale_color_brewer(type = "qual", palette = "Dark2") +
  geom_hline(yintercept = 0, color = "darkblue", linewidth = 0.8, linetype = "dashed", inherit.aes = FALSE)

#are the differences btw moderator groups statistically significant? 
avg_comparisons(model,
                variables = "exposure",
                by = "vax_type",
                hypothesis = "b3 - b1 = 0")

#get marginal effects for covariates
avg_slopes(model, variables = "agechild_centered")
avg_slopes(model, variables = "wi_centered")
avg_comparisons(model, variables = "round")
avg_comparisons(model, variables = "baseline")
avg_comparisons(model, variables = "lagged_exposure")

#-----------------------------------------
#run models for dpt only
dpt_df = combined_df %>% 
  filter(vax_type == "vax_dpt") %>% 
  mutate(agechild_centered = agechild - mean(agechild, na.rm = TRUE)) %>% 
  mutate(wi_centered = wi - mean(wi, na.rm = TRUE)) %>% 
  mutate(
    wi_centered = scale(wi_centered),
    agechild_centered = scale(agechild_centered)
  ) %>% 
  filter(!is.na(exposure))

model <- glmer(vax_status ~ exposure + lagged_exposure + wi_centered + agechild_centered + country +
                 (1 | clustid),
               data = dpt_df, family = binomial, nAGQ=1, 
               control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000)))

summary(model)

AIC(model)

#DHARMa for residual diagnostics and model fit checks
sim_res <- simulateResiduals(model)
plot(sim_res)
testDispersion(sim_res)
testOutliers(sim_res)
testQuantiles(sim_res)

#plot "average predicted probability that Y=1 for different values of moderator & exposure"
plot_predictions(model, by = c("vax_type", "exposure"))

#compute average counterfactual comparison
avg_comparisons(model, variables = "exposure")

plot_comparisons(model, variables = "exposure", by = "vax_type") +
  labs(y = "Marginal Effect", x = "Vaccine Type") +
  scale_x_discrete(labels = c("vax_dpt" = "3 courses of DPT vaccine")) +
  theme(text = element_text(size = 14)) +
  aes(color = vax_type) + 
  scale_color_brewer(type = "qual", palette = "Dark2") +
  geom_hline(yintercept = 0, color = "darkblue", linewidth = 0.8, linetype = "dashed", inherit.aes = FALSE)

#-----------------------------------------
#run models for individual countries

#india

in_no_dpt_df = no_dpt_df %>% 
  filter(country == "in")

model <- glmer(vax_status ~ exposure * vax_type + round + baseline + lagged_exposure + wi_centered + 
                 agechild_centered + 
                 (1 | clustid),
               data = in_no_dpt_df, family = binomial, nAGQ=1, 
               control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000)))
summary(model)

AIC(model)

avg_comparisons(model, variables = "exposure", by = "vax_type")

plot_comparisons(model, variables = "exposure", by = "vax_type") +
  labs(y = "Marginal Effect", x = "Vaccine Type") +
  scale_x_discrete(labels = c("vax_bcg" = "BCG", 
                              "vax_measles" = "Measles",
                              "vax_polio" = "Polio")) +
  theme(text = element_text(size = 14)) +
  aes(color = vax_type) + 
  scale_color_brewer(type = "qual", palette = "Dark2") +
  geom_hline(yintercept = 0, color = "darkblue", linewidth = 0.8, linetype = "dashed", inherit.aes = FALSE)

#peru
pe_no_dpt_df = no_dpt_df %>% 
  filter(country == "pe") 

model <- glmer(vax_status ~ exposure * vax_type + round + wi_centered + agechild_centered + 
                 (1 | clustid),
               data = pe_no_dpt_df, family = binomial, nAGQ=1, 
               control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000)))
summary(model)

#peru with earthquake & forest fire added to exposure
eff_pe_no_dpt_df = pe_no_dpt_df %>% 
  mutate(exposure = case_when(
    event24 == 1 | event25 == 1 | event26 == 1 | event27 == 1 | event58 == 1 | event59 == 1 ~ 1,
    event24 == 0 & event25 == 0 & event26 == 0 & event27 == 0 | event58 == 0 | event59 == 0 ~ 0,
    TRUE ~ NA_real_)) %>%
  mutate(
    exposure_r1 = ifelse(round == 1, exposure_r1, NA),
    exposure_r2 = ifelse(round == 2, exposure_r2, NA)
  ) %>% 
  select(-exposure)

eff_pe_no_dpt_df <- eff_pe_no_dpt_df %>%
  arrange(childid, round) %>%
  mutate(
    exposure = case_when(
      round == 1 ~ exposure_r1,
      round == 2 ~ exposure_r2,
      TRUE ~ NA_real_
    ))
pe <- eff_pe_no_dpt_df %>%
  filter(round == 1) %>%
  group_by(childid) %>%
  summarise(
    r1_exposure =  first(na.omit(as.numeric(as.character(exposure))))
  )
eff_pe_no_dpt_df <- eff_pe_no_dpt_df %>%
  left_join(pe, by = "childid") %>%
  mutate(
    lagged_exposure = if_else(round == 1, 0, r1_exposure)
  ) %>%
  select(-r1_exposure)
  
model <- glmer(vax_status ~ exposure * vax_type + round + baseline + wi_centered + agechild_centered + 
                 (1 | clustid),
               data = eff_pe_no_dpt_df, family = binomial, nAGQ=1, 
               control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000)))
summary(model)

#DHARMa for residual diagnostics and model fit checks
sim_res <- simulateResiduals(model)
plot(sim_res)
testDispersion(sim_res)
testOutliers(sim_res)
testQuantiles(sim_res)

avg_comparisons(model, variables = "exposure", by = "vax_type")

plot_comparisons(model, variables = "exposure", by = "vax_type") +
  labs(y = "Marginal Effect", x = "Vaccine Type") +
  scale_x_discrete(labels = c("vax_bcg" = "BCG", 
                              "vax_measles" = "Measles",
                              "vax_polio" = "Polio")) +
  theme(text = element_text(size = 14)) +
  aes(color = vax_type) + 
  scale_color_brewer(type = "qual", palette = "Dark2") +
  geom_hline(yintercept = 0, color = "darkblue", linewidth = 0.8, linetype = "dashed", inherit.aes = FALSE)

#-------------------------
#data exploration/visualization 

#2x2 tables for round 2 vaccines
table(r2_df$exposure, r2_df$vax_status)

addmargins(table(r2_df$exposure, r2_df$vax_status))

table(r2_df$exposure, r2_df$vax_status, r2_df$vax_type) 

table(combined_df$exposure, combined_df$vax_status, combined_df$vax_type) #to find dpt #'s

table(combined_df$exposure, combined_df$vax_status, combined_df$vax_type, combined_df$round) 

#find out which childids are in both r1 and r2 analysis for India & Peru  
identical(sort(r1_nd_obe_ne_inpe$childid), sort(no_dpt_df$childid))

#in df1 but not in df2 (dropped)
missing_in_r2 <- setdiff(r1_nd_obe_ne_inpe$childid, no_dpt_df$childid)
length(missing_in_r2)
head(missing_in_r2)  

#in df1 but not in df2 (0)
missing_in_r1 <- setdiff(no_dpt_df$childid, r1_nd_obe_ne_inpe$childid)
length(missing_in_r1)

#childids in both
both <- intersect(r1_nd_obe_ne_inpe$childid, no_dpt_df$childid)
length(both)

data.frame(
  in_r1 = length(unique(r1_nd_obe_ne_inpe$childid)),
  in_r2 = length(unique(no_dpt_df$childid)),
  in_both = length(both),
  dropped = length(missing_in_df2),
  added = length(missing_in_df1)
)

#make a dropped out variable in df
r1_nd_obe_ne_inpe <- r1_nd_obe_ne_inpe %>% mutate(dropped_out = !childid %in% no_dpt_df$childid)
table(r1_nd_obe_ne_inpe$dropped_out)

###

#find out how many childids have multiple clustids 
combined_df %>%
  group_by(childid) %>%
  summarise(num_clustid = n_distinct(clustid)) %>%
  filter(num_clustid > 1)

#count of clustids by country
combined_df %>% 
  group_by(country) %>% 
  summarise(unique = n_distinct(clustid))

#number/percentage of childids exposed by round
exposure_plot <- combined_df %>%
  mutate(exposure_num = as.numeric(as.character(exposure))) %>%
  group_by(childid, round) %>%
  summarise(exposure_round = max(exposure_num, na.rm = TRUE), .groups = "drop")
exposure_cat <- exposure_plot %>%
  pivot_wider(names_from = round, values_from = exposure_round, names_prefix = "round") %>%
  mutate(
    category = case_when(
      round1 == 1 & round2 == 1 ~ "Both rounds",
      round1 == 1 & (is.na(round2) | round2 == 0) ~ "Round 1 only",
      (is.na(round1) | round1 == 0) & round2 == 1 ~ "Round 2 only",
      TRUE ~ "Neither"
    )
  ) 
exposure_cat <- exposure_cat %>%
  mutate(category = factor(category, levels = c("Round 1 only", "Round 2 only", "Both rounds", "Neither")))
exposure_summary <- exposure_cat %>%
  count(category) %>%
  mutate(
    percent = n / sum(n) * 100
  )
ggplot(exposure_summary, aes(x = category, y = n, fill = category)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(percent, 1), "%")), 
            vjust = -0.5) + 
  labs(
    x = "Exposure category",
    y = "Number of mother/child pairs",
    title = "Number/Percent of mother/child pairs exposed to natural disaster by round"
  ) +
  theme_minimal() +
  theme(
    axis.text.x  = element_text(size = 12),
    axis.text.y  = element_text(size = 15),
    axis.title.x = element_text(size = 13),
    axis.title.y = element_text(size = 13),
    plot.title   = element_text(size = 18, face = "bold"),
    legend.title = element_text(size = 13),
    legend.text  = element_text(size = 11)
  )

#plot disaster type
ndtype_plot <- combined_df %>%
  group_by(childid) %>%
  summarise(
    Drought = max(event24, na.rm = TRUE),
    Too_much_rain_or_flooding = max(event25, na.rm = TRUE),
    Erosion_or_landslides = max(event26, na.rm = TRUE),
    Frosts_or_hailstorms = max(event27, na.rm = TRUE)
  )
ndtype_plot <- ndtype_plot %>%
  pivot_longer(
    cols = c(Drought, Too_much_rain_or_flooding, Erosion_or_landslides, Frosts_or_hailstorms),
    names_to = "type",
    values_to = "y"
  )
ggplot(ndtype_plot, aes(x = type, fill = factor(y))) +
  geom_bar(position = "fill") +
  geom_text(aes(label = after_stat(count)), 
            stat = "count", 
            position = position_fill(vjust = 0.5),
            size = 5,
            color = "black") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("0" = "steelblue", "1" = "coral")) +
  labs(
    fill = "Exposed",
    x = "Natural Disaster Type Household Affected By",
    y = "Percent"
  )

#find vax_status by vax_type by exposure for round 1 (included in round 1+2 analysis)
combined_df %>%
  filter(round == 1)  %>%
  group_by(vax_type, exposure, vax_status) %>% 
  filter(vax_type %in% c("vax_bcg", "vax_measles", "vax_polio")) %>% 
  summarise(count = n())
summarized_data <- combined_df %>%
  filter(round == 1)  %>%
  filter(vax_type %in% c("vax_bcg", "vax_measles", "vax_polio")) %>% 
  group_by(vax_type, exposure, vax_status) %>%
  summarise(count = n()) %>% 
  mutate(percentage = count / sum(count) * 100)
ggplot(summarized_data, aes(x = vax_type, y = count, fill = factor(vax_status))) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = paste0(round(percentage, 1), "%")), 
            position = position_dodge(width = 0.9), vjust = -0.5,
            hjust = -0.05, size = 4) +
  facet_grid(factor(exposure)~ .) +
  coord_flip() +
  labs(title = "Vaccination Status by Vaccine Type & Natural Disaster Exposure, Round 1",
       x = "Vaccine Type",
       y = "Count",
       fill = "Vaccination Status") +
  scale_x_discrete(labels = c("vax_polio" = "Polio", "vax_measles" = "Measles",
    "vax_dpt" = "3 Courses of DPT","vax_bcg" = "BCG")) +
  theme_minimal() +
  theme(
    axis.text.x  = element_text(size = 12),
    axis.text.y  = element_text(size = 15),
    axis.title.x = element_text(size = 13),
    axis.title.y = element_text(size = 13),
    plot.title   = element_text(size = 18, face = "bold"),
    legend.title = element_text(size = 13),
    legend.text  = element_text(size = 11)
  )

#Count the number of exposed childids to each natural disaster type
combined_df %>% 
  group_by(childid) %>% 
  filter(event27 == 1) %>%
  slice_head(n = 1)

#create tables of vax_status counts for each vax_type and round
combined_df %>%
  filter(vax_type == "vax_dpt") %>%
  group_by(round, vax_status) %>%
  summarise(count = n(), .groups = 'drop') %>%
  arrange(round, vax_status)

#plot percentage of phychnge by clustid
#ensure each child is counted once per country by taking the maximum phychnge value
clean_data <- combined_df %>%
  filter(country == "in", round == 2) %>% 
  group_by(clustid, childid) %>%
  summarise(exposure = max(as.character(exposure))) %>%
  ungroup()
#calculate the percentage by clustid
clean_data <- clean_data %>%
  group_by(clustid, exposure) %>%
  summarize(count = n()) %>%
  mutate(total = sum(count),
         percentage = count / total) %>%
  ungroup()
#create bar plot
ggplot(clean_data, aes(x = factor(clustid), y = percentage, fill = factor(exposure))) +
  geom_bar(stat = "identity", position = "dodge", color = "NA") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_viridis_d(labels = c("0" = "Unexposed", "1" = "Exposed")) +
  theme(axis.text = element_text(size = 16), legend.text = element_text(size = 16)) +
  labs(title = "",
       x = "Sentinel Site",
       y = "Percentage",
       fill = "Natural disaster exposure") +
  theme_minimal() +
  theme(legend.position = "bottom")

##find out which childids are listed as different vax_status in different rounds
#filter for vax_type, round 1, & vax_status
round1_status <- combined_df %>%
  filter(vax_type == "vax_polio", round == 1, vax_status == 1) %>%
  select(childid)
#filter for vax_type, round 2, & vax_status
round2_status <- combined_df %>%
  filter(vax_type == "vax_polio", round == 2, vax_status == 0) %>%
  select(childid)
#find common childids between the two dfs
common_childids <- inner_join(round1_status, round2_status, by = "childid")