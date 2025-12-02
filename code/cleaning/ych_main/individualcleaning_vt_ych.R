# File: Vietnam main younger children survey r1 & r2
# Author: Brittany Shea
# Date: 1-24-25

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
r1_vt_ych = read_dta("./data/raw/ych_main/r1_vt_ych.dta")
r2_vt_ych = read_dta("./data/raw/ych_main/r2_vt_ych.dta")

#-------------------------
#Round 1
#-------------------------

#clean round 1 child data; filtered timelive >= 3
sub1_r1_vt_ych = r1_vt_ych %>% 
  select(agechild, CHILDID, COMMID, CLUSTID, REGION, TIMELIVE, BIO1, HEAD,
         LONGTERM, CHLDETH, CHLDREL, SEX, caredep, RELCARE, BADEVENT, WORSEVNT, PHYCHNGE, wi, INJECT, BCG, 
         MEASLES) %>% 
  mutate(round = 1) %>%
  mutate(across(where(is.character), tolower)) %>%
  rename_with(tolower) %>% 
  mutate(measles = ifelse(agechild <= 9, NA_real_, measles)) %>%
  filter(timelive >= 3) %>% 
  mutate(across(everything(), ~ {
    if (is.numeric(.)) {
      . <- na_if(., 99)
      . <- na_if(., 88)
    } else if (is.character(.)) {
      . <- na_if(., "99")
      . <- na_if(., "88")
    }
    .
  })) %>%
  mutate(sex = as.numeric(sex)) %>% 
  mutate(sex = ifelse(sex == 1, 1,0)) %>% #0=female; 1=male
  mutate(inject = as.numeric(inject)) %>% 
  rename(vax_inject = inject) %>% 
  mutate(vax_inject = ifelse(vax_inject == 1, 1,0)) %>% 
  mutate(bcg = as.numeric(bcg)) %>% 
  rename(vax_bcg = bcg) %>% 
  mutate(vax_bcg = ifelse(vax_bcg == 1, 1,0)) %>% 
  mutate(measles = as.numeric(measles)) %>% 
  rename(vax_measles = measles) %>% 
  mutate(vax_measles = ifelse(vax_measles == 1, 1,0)) %>% 
  mutate(bio1 = as.numeric(bio1)) %>% 
  rename(biomum = bio1) %>% 
  mutate(biomum = ifelse(biomum == 1, 1,0)) %>%
  mutate(relcare = as.numeric(relcare)) %>% 
  mutate(longterm = as.numeric(longterm)) %>% 
  mutate(longterm = ifelse(longterm == 1, 1,0)) %>%
  mutate(badevent = as.numeric(badevent)) %>% 
  mutate(badevent = ifelse(badevent == 1, 1,0)) %>%
  mutate(phychnge = as.numeric(phychnge)) %>% 
  mutate(phychnge = ifelse(phychnge == 1, 1,0)) %>%
  mutate(head = as.numeric(head)) %>% 
  mutate(caredep = as.numeric(caredep)) %>%
  mutate(chldrel = case_when(
    chldrel == 1 ~ "christian",
    chldrel == 3 ~ "buddhist",
    chldrel == 6 ~ "protestant",
    chldrel == 11 ~ "ancestor worship",
    chldrel == 13 ~ "cao dai",
    chldrel == 14 ~ "none",
    chldrel == 15 ~ "other",
    chldrel == 99 ~ NA_character_,
    TRUE ~ as.character(chldrel))) %>% 
  mutate(chldeth = case_when(
    chldeth == 10 ~ "other",
    chldeth == 41 ~ "kinh",
    chldeth == 42 ~ "h'mong",
    chldeth == 44 ~ "ede",
    chldeth == 45 ~ "ba na",
    chldeth == 46 ~ "nung",
    chldeth == 47 ~ "tay",
    chldeth == 48 ~ "dao",
    TRUE ~ as.character(chldeth))) %>% 
  mutate(commid = str_replace(commid, "^vn", "vt")) %>%
  mutate(childid = str_replace(childid, "^vn", "vt")) %>%
  filter(!is.na(vax_inject) | !is.na(vax_bcg) | !is.na(vax_measles)) %>% 
  pivot_longer(cols = starts_with("vax"), 
               names_to = "vax_type", 
               values_to = "vax_status")

#save sub1_r1_vt_ych to file
write.csv(sub1_r1_vt_ych, "./code/cleaning/ych_main/data_ych/r1individualexposure_r1outcome_vt_ych.csv")

sub2_r1_vt_ych <- sub1_r1_vt_ych %>% 
  select(childid, round, commid, clustid, agechild, vax_type, vax_status, wi, chldeth, chldrel,
         longterm, caredep, head, relcare, badevent, phychnge)
