# File: Peru main survey with younger children round 1 & 2 
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
library(lme4)
library(glmmTMB)
library(GGally)
library(reshape2)
library(car)
library(qgraph)
library(lavaan)
library(ggeffects)
library(insight)
library("performance")

#-------------------------

#load data:
r1_pe_ych = read_dta("./data/raw/ych_main/r1_pe_ych.dta")
r2_pe_ych = read_dta("./data/raw/ych_main/r2_pe_ych.dta")

#-------------------------
#Round 1
#-------------------------

#clean round 1 child data; filtered timelive >= 3; if antnata = 0, vax_inject = 0; 
#filter agechild for measles

sub1_r1_pe_ych = r1_pe_ych %>% 
  select(agechild, childid, placeid, clustid, region, timelive, bio1, relcare, head, 
         longterm, sex, caredep, badevent, phychnge, worsevnt, wi, antnata, inject, bcg, polio, 
         measles) %>% 
  mutate(round = 1) %>%
  mutate(measles = ifelse(agechild <= 12, NA_real_, measles)) %>%
  filter(timelive >= 3) %>% 
  rename(commid = placeid) %>% 
  mutate(sex = as.numeric(sex)) %>% 
  mutate(sex = ifelse(sex == 1, 1,0)) %>% #0=female; 1=male
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
  mutate(polio = as.numeric(polio)) %>% 
  rename(vax_polio = polio) %>% 
  mutate(vax_polio = ifelse(vax_polio == 1, 1,0)) %>% 
  mutate(relcare = as.numeric(relcare)) %>% 
  mutate(longterm = as.numeric(longterm)) %>% 
  mutate(longterm = ifelse(longterm == 1, 1,0)) %>%
  mutate(phychnge = as.numeric(phychnge)) %>% 
  mutate(phychnge = ifelse(phychnge == 1, 1,0)) %>%
  mutate(badevent = as.numeric(badevent)) %>% 
  mutate(badevent = ifelse(badevent == 1, 1,0)) %>%
  mutate(caredep = as.numeric(caredep)) %>% 
  mutate(head = as.numeric(head)) %>% 
  mutate(bio1 = as.numeric(bio1)) %>% 
  rename(biomum = bio1) %>% 
  mutate(biomum = ifelse(biomum == 1, 1,0)) %>%
  filter(!is.na(vax_inject) | !is.na(vax_bcg) | !is.na(vax_measles) | !is.na(vax_polio)) %>% 
  pivot_longer(cols = starts_with("vax"), 
               names_to = "vax_type", 
               values_to = "vax_status")
sub1_r1_pe_ych$commid <- tolower(sub1_r1_pe_ych$commid)
sub1_r1_pe_ych$childid <- tolower(sub1_r1_pe_ych$childid)

#save sub1_r1_pe_ych to file
write.csv(sub1_r1_pe_ych, "./code/cleaning/ych_main/data_ych/r1individualexposure_r1outcome_pe_ych.csv")

#-------------------------
# Round 2
#-------------------------

#clean round 2 child data
sub1_r2_pe_ych = r2_pe_ych %>% 
  select(childid, agechild, clustid, region, placeid, primum, wi, chldeth, carelive,
         longterm, event24, event25, event26, event27, event28, event29, event30, 
         event31, event14, event58, event59, bcg, measles, dpt, opv, hib, pehvb) %>%
  mutate(round = 2) %>%
  filter(carelive == -66 | carelive >= 4) %>%
  mutate(bcg = as.numeric(bcg)) %>% 
  rename(vax_bcg = bcg) %>% 
  mutate(measles = as.numeric(measles)) %>% 
  rename(vax_measles = measles) %>% 
  mutate(dpt = as.numeric(dpt)) %>% 
  rename(vax_dpt = dpt) %>% 
  mutate(opv = as.numeric(opv)) %>% 
  rename(vax_polio = opv) %>% 
  mutate(hib = as.numeric(hib)) %>%
  rename(vax_hib = hib) %>% 
  mutate(pehvb = as.numeric(pehvb)) %>% 
  rename(vax_hvb = pehvb) %>% 
  filter(!is.na(vax_bcg) | !is.na(vax_measles) | !is.na(vax_dpt) | !is.na(vax_polio) 
         | !is.na(vax_hib) | !is.na(vax_hvb)) %>% 
  pivot_longer(cols = starts_with("vax"), 
               names_to = "vax_type", 
               values_to = "vax_status") %>% 
  rename(commid = placeid)
sub1_r2_pe_ych$commid <- tolower(sub1_r2_pe_ych$commid)
sub1_r2_pe_ych$childid <- tolower(sub1_r2_pe_ych$childid)

#save sub1_r2_in_ych to file
write.csv(sub1_r2_pe_ych, "./code/cleaning/ych_main/data_ych/r2individualexposure_r2outcome_pe_ych.csv")

sub2_r2_pe_ych <- sub1_r2_pe_ych %>% 
  select(childid, round, commid, clustid, region, agechild, vax_type, vax_status, wi, longterm)
