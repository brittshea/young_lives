# File: Ethiopia main younger children survey r1 & r2 
# Author: Brittany Shea
# Date: 12-11-25

#-------------------------

#install libraries:
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

#-------------------------

#load data:
r1_et_ych = read_dta("./data/raw/ych_main/r1_et_ych.dta")
r2_et_ych = read_dta("./data/raw/ych_main/r2_et_ych.dta")

#-------------------------
#Round 1
#-------------------------

#clean round 1 child data; filter timelive >= 3; if antnata = 0, vax_inject = 0; 
#NAs for measles response if agechild <= 9; filtered if at least 1 vax response is not NA

#measles_under10 = sub1_r1_et_ych %>% 
 # filter(agechild <= 9)

sub1_r1_et_ych = r1_et_ych %>% 
  select(agechild, childid, commid, clustid, region, timelive, bio1, relcare, head,
         longterm, sex, caredep, badevent, phychnge, worsevnt, wi, antnata, bplace, inject, bcg, 
         measles) %>% 
  mutate(round = 1) %>%
  mutate(across(where(is.character), tolower)) %>%
  rename_with(tolower) %>% 
  mutate(measles = ifelse(agechild <= 9, NA_real_, measles)) %>%
  filter(timelive >= 3) %>%
  mutate(sex = as.numeric(sex)) %>% 
  mutate(sex = ifelse(sex == 1, 1,0)) %>% #0=female, 1=male
  mutate(antnata = as.numeric(antnata)) %>% 
  mutate(antnata = ifelse(antnata == 1, 1,0)) %>% 
  mutate(inject = as.numeric(inject)) %>% 
  rename(vax_inject = inject) %>% 
  mutate(vax_inject = ifelse(vax_inject == 1, 1,0)) %>% 
  mutate(vax_inject = if_else(antnata == 0, 0, vax_inject)) %>%
  mutate(bcg = as.numeric(bcg)) %>% 
  rename(vax_bcg = bcg) %>% 
  mutate(vax_bcg = ifelse(vax_bcg == 1, 1,0)) %>% 
  mutate(measles = as.numeric(measles)) %>% 
  rename(vax_measles = measles) %>% 
  mutate(vax_measles = ifelse(vax_measles == 1, 1,0)) %>% 
  mutate(bio1 = as.numeric(bio1)) %>% 
  rename(biomum = bio1) %>% 
  mutate(biomum = ifelse(biomum == 1, 1,0)) %>%
  mutate(longterm = as.numeric(longterm)) %>% 
  mutate(longterm = ifelse(longterm == 1, 1,0)) %>%
  mutate(relcare = as.numeric(relcare)) %>% 
  mutate(badevent = as.numeric(badevent)) %>% 
  mutate(badevent = ifelse(badevent == 1, 1,0)) %>%
  mutate(head = as.numeric(head)) %>% 
  mutate(caredep = as.numeric(caredep)) %>%
  mutate(phychnge = ifelse(phychnge == 1, 1,0)) %>%
  filter(!is.na(vax_inject) | !is.na(vax_bcg) | !is.na(vax_measles)) %>% 
  pivot_longer(cols = starts_with("vax"), 
               names_to = "vax_type", 
               values_to = "vax_status")

#save sub1_r1_et_ych to file
write.csv(sub1_r1_et_ych, "./code/cleaning/ych_main/data_ych/r1individualexposure_r1outcome_et_ych.csv")

#---------------------------------------
#sub-analysis to look at other bad events (obe)

obe_r1_et_ych <- r1_et_ych %>% 
  select(childid, commid, clustid, region, agechild, wi, timelive, badevent, phychnge,
        hhfood, hhlstck, hhcrps, hhlstl, hhcstl, hhdeath, hhjob,
        hhill, hhcrime, hhdiv, hhbirth, edu, hhmove, hhoth, inject, bcg, measles) %>% 
  mutate(round = 1) %>%
  mutate(across(where(is.character), tolower)) %>%
  rename_with(tolower) %>% 
  mutate(measles = ifelse(agechild <= 9, NA_real_, measles)) %>%
  filter(timelive >= 3) %>% 
  rename(vax_inject = inject) %>% 
  mutate(vax_inject = ifelse(vax_inject == 1, 1,0)) %>% 
  rename(vax_bcg = bcg) %>% 
  mutate(vax_bcg = ifelse(vax_bcg == 1, 1,0)) %>% 
  rename(vax_measles = measles) %>% 
  mutate(vax_measles = ifelse(vax_measles == 1, 1,0)) %>% 
  mutate(badevent = ifelse(badevent == 1, 1,0)) %>%
  mutate(phychnge = ifelse(phychnge == 1, 1,0)) %>%
  mutate(hhfood = ifelse(hhfood == 1, 1,0)) %>%
  mutate(hhlstck = ifelse(hhlstck == 1, 1,0)) %>%
  mutate(hhcrps = ifelse(hhcrps == 1, 1,0)) %>%
  mutate(hhlstl = ifelse(hhlstl == 1, 1,0)) %>%
  mutate(hhcstl = ifelse(hhcstl == 1, 1,0)) %>%
  mutate(hhdeath = ifelse(hhdeath == 1, 1,0)) %>%
  mutate(hhjob = ifelse(hhjob == 1, 1,0)) %>%
  mutate(hhill = ifelse(hhill == 1, 1,0)) %>%
  mutate(hhcrime = ifelse(hhcrime == 1, 1,0)) %>%
  mutate(hhdiv = ifelse(hhdiv == 1, 1,0)) %>%
  mutate(hhbirth = ifelse(hhbirth == 1, 1,0)) %>%
  mutate(edu = ifelse(edu == 1, 1,0)) %>%
  mutate(hhmove = ifelse(hhmove == 1, 1,0)) %>%
  mutate(hhoth = ifelse(hhoth == 1, 1,0)) %>%
  filter(!is.na(vax_inject) | !is.na(vax_bcg) | !is.na(vax_measles)) %>% 
  pivot_longer(cols = starts_with("vax"), 
               names_to = "vax_type", 
               values_to = "vax_status")

#save obe_r1_et_ych to file
write.csv(obe_r1_et_ych, "./code/cleaning/ych_main/data_ych/obe_et_ych.csv")

#plot percentage of individuals exposed to obe types
obe_r1_et_ych %>%
  distinct(childid, phychnge, hhfood, hhlstck, hhcrps, hhlstl, hhcstl, hhdeath,
           hhjob, hhjob, hhill, hhcrime, hhdiv, hhbirth, edu, hhmove, hhoth) %>%
  pivot_longer(cols = c(phychnge, hhfood, hhlstck, hhcrps, hhlstl, hhcstl, hhdeath,
                        hhjob, hhjob, hhill, hhcrime, hhdiv, hhbirth, edu, hhmove,
                        hhoth), 
               names_to = "column", 
               values_to = "value") %>%
  group_by(column) %>%
  summarise(
    n_childid = sum(value == 1, na.rm = TRUE),
    total_childid = n(),
    pct_childid = (n_childid / total_childid) * 100
  ) %>%
  ggplot(aes(x = column, y = pct_childid)) +
  geom_col(fill = "royalblue", alpha = 0.7) +
  labs(x = "Type of Bad Event", y = "Percentage of Mother/Child Pairs (%)")

#plot count of individuals by obe combinations
obe_plot = obe_r1_et_ych %>%
  distinct(childid, .keep_all = TRUE) %>%
  group_by(phychnge, hhfood, hhlstck, hhcrps, hhlstl, hhcstl, hhdeath,
           hhjob, hhill, hhcrime, hhdiv, hhbirth, edu, hhmove, hhoth) %>%
  summarise(count = n(), .groups = 'drop') %>%
  mutate(
    exposure_combos = case_when(
      phychnge == 1 & hhfood == 0 & hhlstck == 0 & hhcrps == 0 & hhlstl == 0 &
        hhcstl == 0 & hhdeath == 0 & hhjob == 0 & hhill == 0 & 
        hhcrime == 0 & hhdiv == 0 & hhbirth == 0 & edu == 0 & hhmove == 0 & 
        hhoth == 0 ~ "ND Only",
      phychnge == 1 & hhfood == 1 ~ "ND & Decrease in food availability",
      phychnge == 1 & hhlstck == 1 ~ "ND & Livestock died", 
      phychnge == 1 & hhcrps == 1 ~ "ND & Crops failed", 
      phychnge == 1 & hhlstl == 1 ~ "ND & Livestock stolen", 
      phychnge == 1 & hhcstl == 1 ~ "ND & Crops stolen", 
      phychnge == 1 & hhdeath == 1 ~ "ND & Death/reduction in household members", 
      phychnge == 1 & hhjob == 1 ~ "ND & Job loss/loss of income source/family enterprise", 
      phychnge == 1 & hhill == 1 ~ "ND & Severe illness or injury", 
      phychnge == 1 & hhcrime == 1 ~ "ND & Victim of crime", 
      phychnge == 1 & hhdiv == 1 ~ "ND & Divorce or separation", 
      phychnge == 1 & hhbirth == 1 ~ "ND & Birth/new household member", 
      phychnge == 1 & edu == 1 ~ "ND & Paying for child's education", 
      phychnge == 1 & hhmove == 1 ~ "ND & Moved/migrated/fled", 
      phychnge == 1 & hhoth == 1 ~ "ND & Other bad event", 
      TRUE ~ "Other"
      ),
  ) %>% 
  filter(exposure_combos != "Other") %>%
  group_by(exposure_combos) %>%
  summarise(total_count = sum(count), .groups = 'drop')
ggplot(obe_plot, aes(x = total_count, y = reorder(exposure_combos, total_count))) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(
    title = "Count of Individuals by Bad Event Combination",
    x = "Number of Individuals",
    y = "Bad Event Combination"
  ) +
  theme_minimal() +
  geom_text(aes(label = total_count), hjust = -0.1, size = 3)

#formatted table with counts of obe combinations
obe_plot %>%
  arrange(desc(total_count)) %>%
  knitr::kable(col.names = c("Bad Event Combinations", "Count"))

#run model for obe
model_a <- glmer(vax_status ~ hhcrime + vax_type + wi + agechild + 
                   (1 | region/commid/childid) ,
                 data = obe_r1_et_ych, family = binomial, nAGQ = 1, 
                 control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000)))

summary(model_a)