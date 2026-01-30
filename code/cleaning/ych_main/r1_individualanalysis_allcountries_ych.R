#All young lives r1 household-level exposure data with round 1 vax data
#Author: Brittany Shea
#Date: 12/27/25
#Data: Huttly, S., & Jones, N. (2014). Young Lives: An International Study of Childhood Poverty 2002, Round 1 [Data set]. World Bank, Development Data Group. https://doi.org/10.48529/3CAG-R297
#Boyden, J. (2025). Young Lives: an International Study of Childhood Poverty: Rounds 1-5 Constructed Files, 2002-2016. [data collection]. 5th Edition. UK Data Service. SN: 7483, DOI: http://doi.org/10.5255/UKDA-SN-7483-5


####If respondent not biomum, NA for antnata and vax_inject
####If vax_inject not known, NA (et,in, pe) or NK (vt)

#Load the libraries
library(tidyverse)
library(viridis)
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
library(broom.mixed)
library(sjPlot)
library(DHARMa)
library(forcats)
library(marginaleffects)
library(patchwork)
library(purrr)

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
r1_expandout_ind_et_ych <- read.csv("./code/cleaning/ych_main/data_ych/r1individualexposure_r1outcome_et_ych.csv")
r1_expandout_ind_in_ych <- read.csv("./code/cleaning/ych_main/data_ych/r1individualexposure_r1outcome_in_ych.csv")
r1_expandout_ind_pe_ych <- read.csv("./code/cleaning/ych_main/data_ych/r1individualexposure_r1outcome_pe_ych.csv")
r1_expandout_ind_vt_ych <- read.csv("./code/cleaning/ych_main/data_ych/r1individualexposure_r1outcome_vt_ych.csv")

#Combine the data by common columns
common_columns <- Reduce(intersect, list(names(r1_expandout_ind_et_ych), names(r1_expandout_ind_in_ych), names(r1_expandout_ind_pe_ych), names(r1_expandout_ind_vt_ych)))

r1_expandout_ind_all_ych <- rbind(r1_expandout_ind_et_ych[, common_columns], 
                            r1_expandout_ind_in_ych[, common_columns], 
                            r1_expandout_ind_pe_ych[, common_columns], 
                            r1_expandout_ind_vt_ych[, common_columns])

#create df to examine effect of exposure to natural disaster (ND) v. other bad event (OBE); remove polio;
#convert categorical variables to factors; mean center agechild & wi
nd_obe = r1_expandout_ind_all_ych %>% 
  mutate(country = substr(childid, 1, 2)) %>% 
  mutate(clustid = paste(country, clustid, sep = "_")) %>% 
  mutate(region = if_else(is.na(region), as.character(region), paste(country, region, sep = "_"))) %>% 
  mutate(exposure = case_when(
    badevent == 1 & phychnge == 1 ~ 1,
    badevent == 1 & (phychnge == 0 | is.na(phychnge)) ~ 0,
    TRUE ~ NA_real_
  )) %>% 
  mutate(agechild_centered = agechild - mean(agechild, na.rm = TRUE)) %>% 
  mutate(wi_centered = wi - mean(wi, na.rm = TRUE)) %>% 
  mutate(
    round = as.factor(round),
    country = as.factor(country),
    childid = as.factor(childid),
    commid = as.factor(commid),
    badevent = as.factor(badevent),
    phychnge = as.factor(phychnge),
    exposure = as.factor(exposure),
    sex = as.factor(sex),
    vax_type = as.factor(vax_type),
    vax_status = as.factor(vax_status), 
    region = as.factor(region),
    caredep = as.factor(caredep),
    clustid = as.factor(clustid)) %>%
  filter(vax_type != "vax_polio") %>% 
  mutate(vax_type = fct_relevel(vax_type, "vax_inject")) %>% #relevel vax_type
  filter(!is.na(exposure)) %>% 
  filter(!is.na(wi_centered)) %>% 
  filter(!is.na(vax_status)) %>% 
  select(-X) %>% 
  select(round, childid, country, region, clustid, commid, exposure, vax_status, vax_type, antnata, phychnge, badevent, wi_centered, agechild_centered, everything())

##create pluspolio_nd_obe df that includes polio 
pluspolio_nd_obe = r1_expandout_ind_all_ych %>% 
  mutate(country = substr(childid, 1, 2)) %>% 
  mutate(clustid = paste(country, clustid, sep = "_")) %>% 
  mutate(region = if_else(is.na(region), as.character(region), paste(country, region, sep = "_"))) %>% 
  mutate(exposure = case_when(
    badevent == 1 & phychnge == 1 ~ 1,
    badevent == 1 & (phychnge == 0 | is.na(phychnge)) ~ 0,
    TRUE ~ NA_real_
  )) %>% 
  mutate(agechild_centered = agechild - mean(agechild, na.rm = TRUE)) %>% 
  mutate(wi_centered = wi - mean(wi, na.rm = TRUE)) %>% 
  mutate(
    round = as.factor(round),
    country = as.factor(country),
    childid = as.factor(childid),
    commid = as.factor(commid),
    badevent = as.factor(badevent),
    phychnge = as.factor(phychnge),
    exposure = as.factor(exposure),
    sex = as.factor(sex),
    vax_type = as.factor(vax_type),
    vax_status = as.factor(vax_status), 
    region = as.factor(region),
    caredep = as.factor(caredep),
    clustid = as.factor(clustid)) %>%
  mutate(vax_type = fct_relevel(vax_type, "vax_inject")) %>% #relevel vax_type
  filter(!is.na(exposure)) %>% 
  filter(!is.na(wi_centered)) %>% 
  filter(!is.na(vax_status)) %>% 
  select(-X) %>% 
  select(round, childid, country, region, clustid, commid, exposure, vax_status, vax_type, antnata, phychnge, badevent, wi_centered, agechild_centered, everything())

#create df to examine effect of exposure to natural disaster (ND) v. no event (NE); remove polio;
#convert categorical variables to factors; mean center agechild & wi
nd_ne = r1_expandout_ind_all_ych %>% 
  mutate(country = substr(childid, 1, 2)) %>% 
  mutate(clustid = paste(country, clustid, sep = "_")) %>% 
  mutate(region = if_else(is.na(region), as.character(region), paste(country, region, sep = "_"))) %>% 
  mutate(exposure = case_when(
    badevent == 1 & phychnge == 1 ~ 1,
    badevent == 0 ~ 0,
    TRUE ~ NA_real_
  )) %>% 
  mutate(agechild_centered = agechild - mean(agechild, na.rm = TRUE)) %>% 
  mutate(wi_centered = wi - mean(wi, na.rm = TRUE)) %>% 
  mutate(
    round = as.factor(round),
    country = as.factor(country),
    childid = as.factor(childid),
    commid = as.factor(commid),
    badevent = as.factor(badevent),
    phychnge = as.factor(phychnge),
    exposure = as.factor(exposure),
    sex = as.factor(sex),
    vax_type = as.factor(vax_type),
    vax_status = as.factor(vax_status), 
    region = as.factor(region),
    caredep = as.factor(caredep),
    clustid = as.factor(clustid)) %>%
  filter(vax_type != "vax_polio") %>% 
  mutate(vax_type = fct_relevel(vax_type, "vax_inject")) %>% 
  filter(!is.na(exposure)) %>% 
  filter(!is.na(wi_centered)) %>% 
  filter(!is.na(vax_status)) %>% 
  select(-X) %>% 
  select(round, childid, country, region, clustid, commid, exposure, vax_status, vax_type, antnata, phychnge, badevent, wi_centered, agechild_centered, everything())

#create pluspolio_nd_ne df that includes polio
pluspolio_nd_ne = r1_expandout_ind_all_ych %>% 
  mutate(country = substr(childid, 1, 2)) %>% 
  mutate(clustid = paste(country, clustid, sep = "_")) %>% 
  mutate(region = if_else(is.na(region), as.character(region), paste(country, region, sep = "_"))) %>% 
  mutate(exposure = case_when(
    badevent == 1 & phychnge == 1 ~ 1,
    badevent == 0 ~ 0,
    TRUE ~ NA_real_
  )) %>% 
  mutate(agechild_centered = agechild - mean(agechild, na.rm = TRUE)) %>% 
  mutate(wi_centered = wi - mean(wi, na.rm = TRUE)) %>% 
  mutate(
    round = as.factor(round),
    country = as.factor(country),
    childid = as.factor(childid),
    commid = as.factor(commid),
    badevent = as.factor(badevent),
    phychnge = as.factor(phychnge),
    exposure = as.factor(exposure),
    sex = as.factor(sex),
    vax_type = as.factor(vax_type),
    vax_status = as.factor(vax_status), 
    region = as.factor(region),
    caredep = as.factor(caredep),
    clustid = as.factor(clustid)) %>%
  mutate(vax_type = fct_relevel(vax_type, "vax_inject")) %>% 
  filter(!is.na(exposure)) %>% 
  filter(!is.na(wi_centered)) %>% 
  filter(!is.na(vax_status)) %>% 
  select(-X) %>% 
  select(round, childid, country, region, clustid, commid, exposure, vax_status, vax_type, antnata, phychnge, badevent, wi_centered, agechild_centered, everything())

#create df to examine effect of exposure to natural disaster (ND) v. other bad event (OBE) AND no event (NE); remove polio;
#convert categorical variables to factors; mean center agechild & wi
nd_obe_ne = r1_expandout_ind_all_ych %>% 
  mutate(country = substr(childid, 1, 2)) %>% 
  mutate(clustid = paste(country, clustid, sep = "_")) %>% 
  mutate(region = if_else(is.na(region), as.character(region), paste(country, region, sep = "_"))) %>% 
  mutate(exposure = case_when(
    is.na(badevent) ~ NA_real_,
    badevent == 1 & phychnge == 1 ~ 1,
    badevent == 1 & (phychnge == 0 | is.na(phychnge)) ~ 0,
    badevent == 0 & is.na(phychnge) ~ 0,
    TRUE ~ NA_real_
  )) %>% 
  mutate(agechild_centered = agechild - mean(agechild, na.rm = TRUE)) %>% 
  mutate(wi_centered = wi - mean(wi, na.rm = TRUE)) %>% 
  mutate(
    round = as.factor(round),
    country = as.factor(country),
    childid = as.factor(childid),
    commid = as.factor(commid),
    badevent = as.factor(badevent),
    phychnge = as.factor(phychnge),
    exposure = as.factor(exposure),
    sex = as.factor(sex),
    vax_type = as.factor(vax_type),
    vax_status = as.factor(vax_status), 
    region = as.factor(region),
    caredep = as.factor(caredep),
    clustid = as.factor(clustid)) %>%
  filter(vax_type != "vax_polio") %>% 
  mutate(vax_type = fct_relevel(vax_type, "vax_inject")) %>% 
  filter(!is.na(exposure)) %>% 
  filter(!is.na(wi_centered)) %>% 
  filter(!is.na(vax_status)) %>% 
  select(-X) %>% 
  select(round, childid, country, region, clustid, commid, exposure, vax_status, vax_type, antnata, phychnge, badevent, wi_centered, agechild_centered, everything())

#percent not biomum
nd_obe_ne %>%
  distinct(childid, biomum) %>%
  summarise(percent = mean(biomum == 0) * 100)

#count number of NA's for exposure per childid (before removed NAs)
#nd_obe_ne %>%
  #group_by(childid) %>%
  #summarize(na_count = sum(is.na(exposure))) %>%
  #filter(na_count > 0) %>% 
  #print(n = 100)

#count number of NA's for wi_centered per childid (before removed NAs)
#nd_obe_ne %>%
  #group_by(childid) %>%
  #summarize(na_count = sum(is.na(wi_centered))) %>%
  #filter(na_count > 0) %>% 
  #print(n = 100)

#count number of NAs for vax_status (before removed NAs)
#nd_obe_ne %>%
  #summarize(na_count = sum(is.na(vax_status))) 

#create pluspolio_nd_obe_ne df that includes polio
pluspolio_nd_obe_ne = r1_expandout_ind_all_ych %>% 
  mutate(country = substr(childid, 1, 2)) %>% 
  mutate(clustid = paste(country, clustid, sep = "_")) %>% 
  mutate(region = if_else(is.na(region), as.character(region), paste(country, region, sep = "_"))) %>% 
  mutate(exposure = case_when(
    is.na(badevent) ~ NA_real_,
    badevent == 1 & phychnge == 1 ~ 1,
    badevent == 1 & (phychnge == 0 | is.na(phychnge)) ~ 0,
    badevent == 0 & is.na(phychnge) ~ 0,
    TRUE ~ NA_real_
  )) %>% 
  mutate(agechild_centered = agechild - mean(agechild, na.rm = TRUE)) %>% 
  mutate(wi_centered = wi - mean(wi, na.rm = TRUE)) %>% 
  mutate(
    round = as.factor(round),
    country = as.factor(country),
    childid = as.factor(childid),
    commid = as.factor(commid),
    badevent = as.factor(badevent),
    phychnge = as.factor(phychnge),
    exposure = as.factor(exposure),
    sex = as.factor(sex),
    vax_type = as.factor(vax_type),
    vax_status = as.factor(vax_status), 
    region = as.factor(region),
    caredep = as.factor(caredep),
    clustid = as.factor(clustid)) %>%
  mutate(vax_type = fct_relevel(vax_type, "vax_inject")) %>% 
  filter(!is.na(exposure)) %>% 
  filter(!is.na(wi_centered)) %>% 
  filter(!is.na(vax_status)) %>% 
  select(-X) %>% 
  select(round, childid, country, region, clustid, commid, exposure, vax_status, vax_type, antnata, phychnge, badevent, wi_centered, agechild_centered, everything())

#save r1 nd_ne_obe to file
write.csv(pluspolio_nd_obe_ne, "./code/cleaning/ych_main/data_ych/r1_pluspolio_nd_obe_ne.csv")

#create dfs for analysis
et_nd_obe = nd_obe %>% 
  filter(country == "et")

et_nd_ne = nd_ne %>% 
  filter(country == "et") 

et_nd_obe_ne = nd_obe_ne %>% 
  filter(country == "et")

in_nd_obe = pluspolio_nd_obe %>% 
  filter(country == "in")

in_nd_ne = pluspolio_nd_ne %>% 
  filter(country == "in") 

in_nd_obe_ne = pluspolio_nd_obe_ne %>% 
  filter(country == "in")

vi_nd_obe = nd_obe %>% 
  filter(country == "vt") 

vi_nd_ne = nd_ne %>% 
  filter(country == "vt")

vi_nd_obe_ne = nd_obe_ne %>% 
  filter(country == "vt")

#-----------------------------------------
#it’s ok for there to be an imbalance in the outcome (it’s about 1/3rd) 
#only 1 childid missing response for badevent, but only a few exposure=1 for Peru
#check for missingness for other variables (wi)

#check if missingness is random for wi
#missing_model <- glm(is.na(wi_centered) ~ exposure + vax_type + agechild_centered, 
                     #data = nd_obe, family = binomial)
#summary(missing_model)
#exp(coef(missing_model))
#exp(confint(missing_model))  

#-----------------------------------------
#summary statistics

#count unique children by country
nd_obe_ne %>% 
  group_by(country) %>% 
  summarise(unique = n_distinct(commid))

#count of total rows 
nrow(nd_obe_ne)

#summary statistics
nd_obe_ne %>% 
  filter(country == "vt") %>% 
  distinct(childid, .keep_all = TRUE) %>%
  select(-vax_status, -vax_type) %>% 
  summary()

#table of unexposed/exposed mother/child pairs and percentage exposed by country
nd_obe_ne %>% 
  distinct(childid, .keep_all = TRUE) %>% 
  count(country, exposure) %>% 
  pivot_wider(names_from = exposure, values_from = n, values_fill = 0) %>% 
  mutate(total = `0` + `1`,
         pct_exposed = round(`1` / total * 100, 1))

#plot age
nd_obe_ne %>% 
  distinct(childid, agechild) %>%
  ggplot(aes(x = agechild)) +
  geom_density(fill = "blue", alpha = 0.5) +
  xlab("Age in months") +
  ylab("Density") +
  ggtitle("Age Density Plot")

#plot vaccine probability by age by vax_type
nd_obe_ne %>%
  filter(vax_type == "vax_measles") %>% 
  mutate(
    vax_status = as.numeric(as.character(vax_status)),
    age_bin = cut(agechild, breaks = 10)
  ) %>%
  group_by(age_bin) %>%
  summarise(
    vax_prob = mean(vax_status),
    median_age = median(agechild),
    n = n()
  ) %>%
  ggplot(aes(x = median_age, y = vax_prob)) +
  geom_point(aes(size = n)) +
  geom_smooth(method = "loess", se = TRUE) +
  xlab("Child age in months") +
  ylab("Vaccination Probability")

#plot wi
nd_obe_ne %>% 
  filter(country == "et") %>% 
  distinct(childid, wi) %>% 
  ggplot(aes(x = wi)) +
  geom_density(fill = "darkgreen", alpha = 0.5) +
  xlab("Wealth Index") +
  ylab("Density") +
  ggtitle("Wealth Index Density Plot")

#density plot for wi by clustid
nd_obe_ne %>% 
  filter(country == "et") %>% 
  distinct(childid, clustid, wi) %>% 
  ggplot(aes(x = wi)) +
  geom_density(fill = "#440154", alpha = 0.5) +
  facet_wrap(~ clustid, scales = "free_y") + 
  xlab("Wealth Index") +
  ylab("Density") +
  ggtitle("Wealth Index Density Plot by Sentinel Site") +
  theme(strip.text = element_text(size = 8)) 

#count of NA for wi
nd_obe_ne %>%
  distinct(childid, .keep_all = TRUE) %>% 
  summarise(count_na = sum(is.na(wi_centered)))

#exposure proportion by clustid (sentinel site)
nd_obe_ne %>% 
  filter(country == "in") %>% 
  distinct(childid, clustid, exposure) %>%
  ggplot(aes(x = clustid, fill = factor(exposure))) +
  geom_bar(position = "fill") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  xlab("Clustid (Sentinel Site)") +
  ylab("Proportion") +
  ggtitle("Exposure Proportion by Clustid (Sentinel Site)") +
  labs(fill = "Exposure") 

#find percentage of 1's in a column (binary) by country:
nd_obe_ne %>% 
  #filter(country == "vt") %>%
  distinct(childid, biomum) %>%
  summarise(percent = mean(biomum == 0, na.rm = TRUE) * 100)

#find percentage of 1's in a column (not binary) by country:
nd_obe_ne %>% 
  filter(country == "pe") %>%
  distinct(childid, bplace) %>%
  summarise(percent = mean(bplace == 1, na.rm = TRUE) * 100)

#find counts/percentages of vaccinations by type and country 
nd_obe_ne %>% 
  group_by(vax_type, vax_status, country) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count) * 100) %>% 
  print(n = 30)

#find counts/percentages of vaccinations by country for individual vax types
nd_obe_ne %>%
  filter(vax_type == "vax_measles") %>%
  group_by(country) %>%
  summarise(
    filtered_count = sum(vax_status == 1, na.rm = TRUE),
    total_count = n(),
    percentage = round(filtered_count / total_count * 100, 1)
  )

#find total count/percentage for individual vax types
nd_obe_ne %>%
  filter(vax_type == "vax_measles") %>%
  summarise(
    filtered_count = sum(vax_status == 1, na.rm = TRUE),
    total_count = n(),
    percentage = round(filtered_count / total_count * 100, 2)
  )

#find vax_status by vax_type by exposure 
nd_obe_ne %>%
  group_by(vax_type, exposure, vax_status) %>%
  summarise(count = n())
#plot this data
summarized_data <- nd_obe_ne %>%
  group_by(vax_type, exposure, vax_status) %>%
  summarise(count = n()) %>% 
  mutate(percentage = count / sum(count) * 100)
ggplot(summarized_data, aes(x = vax_type, y = count, fill = factor(vax_status))) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = paste0(round(percentage, 1), "%")), 
            position = position_dodge(width = 0.9), vjust = -0.5) +
  facet_grid(factor(exposure)~ .) +
  coord_flip() +
  labs(title = "Vaccination Status by Vaccine Type & Natural Disaster Exposure",
       x = "Vaccine Type",
       y = "Count",
       fill = "Vaccination Status") +
  theme_minimal()

#plot vax_status by vax_type (calculate percentages)
df_percent <- nd_obe_ne %>%
  mutate(country = str_to_title(country)) %>% 
  group_by(country, vax_type, vax_status) %>%
  summarise(count = n()) %>% 
  mutate(percentage = count / sum(count) * 100) %>% 
  print(n=40)

ggplot(df_percent, aes(x = vax_type, y = count, fill = as.factor(vax_status))) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_cartesian(clip = "off") +
  geom_text(aes(label = paste0(round(percentage, 1), "%")), 
            position = position_dodge(width = 0.9), vjust = -0.5) +
  labs(title = "Vaccination Status by Vaccine Type and Country",
       x = "Vaccine Type",
       y = "Count",
       fill = "Vaccination Status") +
  scale_x_discrete(labels = c("vax_bcg" = "BCG", "vax_inject" = "Prenatal Tetanus", "vax_measles" = "Measles", "vax_polio" = "Polio")) +
  labs(fill = "") +
  scale_fill_discrete(labels = c("0" = "Unvaccinated", "1" = "Vaccinated")) +
  theme_minimal() +
  facet_wrap(~ country, scales = "free_x")

#plot percentage of phychnge by clustid
#ensure each child is counted once per country by taking the maximum phychnge value
clean_data <- nd_obe_ne %>%
  filter(country == "in") %>% 
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

#plot percentage of phychnge by country
clean_data <- nd_obe_ne %>%
  group_by(country, childid) %>%
  summarise(exposure = max(as.character(exposure))) %>%
  ungroup()
#calculate the percentage by clustid
clean_data <- clean_data %>%
  group_by(country, exposure) %>%
  summarize(count = n()) %>%
  mutate(total = sum(count),
         percentage = count / total) %>%
  ungroup()
#create bar plot
ggplot(clean_data, aes(x = factor(country), y = percentage, fill = factor(exposure))) +
  geom_bar(stat = "identity", position = "dodge", color = "NA") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_viridis_d(labels = c("0" = "Unexposed", "1" = "Exposed")) +
  theme(axis.text = element_text(size = 16), legend.text = element_text(size = 16)) +
  labs(title = "",
       x = "Country",
       y = "Percentage",
       fill = "Natural disaster exposure") +
  theme_minimal() +
  theme(legend.position = "bottom")

#plot counts for vax_type by vax_status and exposure:
filtered_data <- nd_obe_ne[nd_obe_ne$vax_status == 1 & nd_obe_ne$exposure == 1, ]
ggplot(filtered_data, aes(x = vax_type)) +
  geom_bar() +
  labs(x = "vax_type", y = "Count", title = "Counts by vax_type")

#plot wi by vaccination status for round 1
clean_data <- nd_obe_ne %>%
  drop_na(wi)
#create histogram plot
ggplot(clean_data, aes(x = wi, fill = factor(vax_status))) +
  geom_histogram(aes(y = after_stat(count)/sum(after_stat(count))), binwidth = 0.07, position = "stack", color = "NA") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("0" = "darkorange1", "1" = "green4"), labels = c("0" = "Unvaccinated", "1" = "Vaccinated")) +
  labs(title = "Individual wealth index by vaccination status",
       x = "Wealth index",
       y = "Percentage",
       fill = "Vaccination status") +
  theme_minimal() +
  theme(legend.position = "bottom")

#----------------------------------------
#run models for all vaccination and countries combined: nd_obe, nd_ne, nd_obe_ne

#nd_obe

model_a <- glmer(vax_status ~ exposure + vax_type + wi_centered + agechild_centered + 
                   (1 | country/region/commid/childid) ,
                 data = nd_obe, family = binomial, nAGQ = 1, 
                 control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000)))

summary(model_a)

#get odds ratio and confidence interval using broom.mixed 
exp(fixef(model_a))
tidy(model_a, conf.int = TRUE, exponentiate = TRUE)

#AIC to check model fit (lower AIC values indicate a better-fitting model)
AIC(model_a)

#DHARMa for residual diagnostics and model fit checks
sim_res <- simulateResiduals(model_a)
plot(sim_res)
testDispersion(sim_res)
testOutliers(sim_res)

#nd_ne

model_b <- glmer(vax_status ~ exposure + vax_type + wi_centered + agechild_centered +
                   (1 | country/region/commid/childid) ,
                 data = nd_ne, family = binomial, nAGQ = 1, 
                 control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000)))

summary(model_b)

#AIC to check model fit (lower AIC values indicate a better-fitting model)
AIC(model_b)

#DHARMa for residual diagnostics and model fit checks
sim_res <- simulateResiduals(model_b)
plot(sim_res)
testDispersion(sim_res)
testOutliers(sim_res)

#nd_ne_obe

model_c <- glmer(vax_status ~ exposure + vax_type + wi_centered + agechild_centered +
                   (1 | country/region/clustid/childid) ,
                 data = nd_obe_ne, family = binomial, nAGQ = 1, 
                 control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000)))

summary(model_c)

#AIC to check model fit (lower AIC values indicate a better-fitting model)
AIC(model_c)

#DHARMa for residual diagnostics and model fit checks
sim_res <- simulateResiduals(model_c)
plot(sim_res)
testDispersion(sim_res)
testOutliers(sim_res)

#----------------------------
#run interaction models

### sensitivity analysis #1 ##

#nd_obe

model_d <- glmer(vax_status ~ exposure * vax_type + wi_centered + agechild_centered + 
                   (1 | region/clustid/childid),
                 data = nd_obe, family = binomial, nAGQ = 1, 
                 control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000)))

summary(model_d)

#AIC to check model fit (lower AIC values indicate a better-fitting model)
AIC(model_d)

#DHARMa for residual diagnostics and model fit checks
sim_res <- simulateResiduals(model_d)
plot(sim_res)
testDispersion(sim_res)
testOutliers(sim_res)

#compute average predicted outcome for each combination of exposure & moderator 
avg_predictions(model_d, by = c("exposure", "vax_type"))

#plot "average predicted probability that Y=1 for different values of moderator & exposure"
plot_predictions(model_d, by = c("vax_type", "exposure"))

#compute average counterfactual comparison
avg_comparisons(model_d, variables = "exposure")

#check for heterogeneity to see if effect of X on Y depends on Moderator 
avg_comparisons(model_d, variables = "exposure", by = "vax_type")

#plot marginal effects 
plot_comparisons(model_d, variables = "exposure", by = "vax_type") +
  labs(y = "Marginal Effect", x = "Vaccine Type") +
  scale_x_discrete(labels = c("vax_inject" = "≥ 2 doses of antenatal tetanus", 
                              "vax_bcg" = "BCG", 
                              "vax_measles" = "Measles")) +
  theme(text = element_text(size = 14)) +
  aes(color = vax_type) + 
  scale_color_brewer(type = "qual", palette = "Dark2") +
  geom_hline(yintercept = 0, color = "darkblue", linewidth = 0.8, linetype = "dashed", inherit.aes = FALSE)

#are the differences btw moderator groups statistically significant? 
avg_comparisons(model_d,
                variables = "exposure",
                by = "vax_type",
                hypothesis = "b3 - b1 = 0")

#get marginal effects for covariates
avg_slopes(model_d, variables = "agechild_centered")
avg_slopes(model_d, variables = "wi_centered")

### sensitivity analysis #2 ##
#nd_ne

model_e <- glmer(vax_status ~ exposure * vax_type + wi_centered + agechild_centered +
                   (1 | country/clustid/childid),
                 data = nd_ne, family = binomial, nAGQ = 1, 
                 control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000)))

summary(model_e)

#run model without peru
noperu_nd_ne = nd_ne %>% 
  filter(country != "pe")

model_noperu <- glmer(vax_status ~ exposure * vax_type + wi_centered + agechild_centered +
                        (1 | country/region/clustid/childid),
                      data = noperu_nd_ne, family = binomial, nAGQ = 1, 
                      control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000)))

summary(model_noperu)

avg_comparisons(model_noperu, variables = "exposure", by = "vax_type")

plot_comparisons(model_noperu, variables = "exposure", by = "vax_type") +
  labs(y = "Marginal Effect", x = "Vaccine Type") +
  scale_x_discrete(labels = c("vax_inject" = "≥ 2 doses of antenatal tetanus", 
                              "vax_bcg" = "BCG", 
                              "vax_measles" = "Measles")) +
  theme(text = element_text(size = 14)) +
  aes(color = vax_type) + 
  scale_color_brewer(type = "qual", palette = "Dark2")

#AIC to check mo your del fit (lower AIC values indicate a better-fitting model)
AIC(model_e)

#DHARMa for residual diagnostics and model fit checks
sim_res <- simulateResiduals(model_e)
plot(sim_res)
testDispersion(sim_res)
testOutliers(sim_res)

#use marginaleffects pkg to find predicted probabilities & marginal predictions (https://marginaleffects.com/chapters/interactions.html)

#compute average predicted outcome for each combination of exposure & moderator 
avg_predictions(model_e, by = c("exposure", "vax_type"))

#plot "average predicted probability that Y=1 for different values of moderator & exposure"
plot_predictions(model_e, by = c("vax_type", "exposure"))

#compute average counterfactual comparison ("on average, moving from 0 to 1 on 
avg_comparisons(model_e, variables = "exposure")

#check for heterogeneity to see if effect of X on Y depends on Moderator 
avg_comparisons(model_e, variables = "exposure", by = "vax_type")

#plot marginal effects 
plot_comparisons(model_e, variables = "exposure", by = "vax_type") +
  labs(y = "Marginal Effect", x = "Vaccine Type") +
  scale_x_discrete(labels = c("vax_inject" = "≥ 2 doses of antenatal tetanus", 
                              "vax_bcg" = "BCG", 
                              "vax_measles" = "Measles")) +
  theme(text = element_text(size = 14)) +
  aes(color = vax_type) + 
  scale_color_brewer(type = "qual", palette = "Dark2") +
  geom_hline(yintercept = 0, color = "darkblue", linewidth = 0.8, linetype = "dashed", inherit.aes = FALSE)


#are the differences btw moderator groups statistically significant? 
avg_comparisons(model_e,
                variables = "exposure",
                by = "vax_type",
                hypothesis = "b3 - b1 = 0")

#get marginal effects for covariates
avg_slopes(model_e, variables = "agechild_centered")
avg_slopes(model_e, variables = "wi_centered")

#nd_ne_obe

model_f <- glmer(vax_status ~ exposure * vax_type + wi_centered + agechild_centered +
                   (1 | country/clustid/childid),
                 data = nd_obe_ne, family = binomial, nAGQ = 1, 
                 control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000)))

summary(model_f)

#sjplot for OR, CI, p-value, and random effects
tab_model(model_f)

#AIC to check model fit
AIC(model_f)

#DHARMa for residual diagnostics and model fit checks
sim_res <- simulateResiduals(model_f)
plot(sim_res)
testDispersion(sim_res)
testOutliers(sim_res)
testQuantiles(sim_res)
sim_res_grouped <- recalculateResiduals(sim_res, group = nd_obe_ne$clustid)
plot(sim_res_grouped)

#compute average predicted outcome for each combination of exposure & moderator 
avg_predictions(model_f, by = c("exposure", "vax_type"))

#plot "average predicted probability that Y=1 for different values of moderator & exposure"
plot_predictions(model_f, by = c("vax_type", "exposure")) +
labs(y = "Predicted Probability", x = "Vaccine Type") +
  scale_x_discrete(labels = c("vax_inject" = "≥ 2 doses of antenatal tetanus", 
                              "vax_bcg" = "BCG", 
                              "vax_measles" = "Measles")) +
  theme(text = element_text(size = 14)) +
  aes(color = exposure) + 
  scale_color_brewer(type = "qual", palette = "Set2") 

#compute average counterfactual comparison
avg_comparisons(model_f, variables = "exposure")

#Find average marginal effects, i.e. the comparison between exposure = 1 vs exposure = 0)
#it's the average percentage‑point differences in the predicted probability of the outcome between exposed and unexposed, calculated separately within each level of the moderator.
#checks for heterogeneity to see if effect of X on Y depends on Moderator 
avg_comparisons(model_f, variables = "exposure", by = "vax_type")

#plot marginal effects 
plot_comparisons(model_f, variables = "exposure", by = "vax_type") +
  labs(y = "Marginal Effect", x = "Vaccine Type") +
  scale_x_discrete(labels = c("vax_inject" = "≥ 2 doses of antenatal tetanus", 
                              "vax_bcg" = "BCG", 
                              "vax_measles" = "Measles")) +
  theme(text = element_text(size = 14)) +
  aes(color = vax_type) + 
  scale_color_brewer(type = "qual", palette = "Dark2") +
  geom_hline(yintercept = 0, color = "darkblue", linewidth = 0.8, linetype = "dashed", inherit.aes = FALSE)

#forest plot marginal effects
comps <- avg_comparisons(model_f, variables = "exposure", by = "vax_type")
ggplot(comps, aes(y = vax_type, x = estimate)) +
  geom_point() +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high)) 

#are the differences btw moderator groups statistically significant? 
avg_comparisons(model_f,
                variables = "exposure",
                by = "vax_type",
                hypothesis = "b3 - b2 = 0")

#get marginal effects for covariates
avg_slopes(model_f, variables = "agechild_centered")
print(avg_slopes(model_f, variables = "wi_centered"), digits = 5)

### sensitivity analysis #3 ###

##run model without ethiopia##
noet_nd_obe_ne = nd_obe_ne %>% 
  filter(country != "et")

model_noet <- glmer(vax_status ~ exposure * vax_type + wi_centered + agechild_centered +
                        (1 | country/clustid/childid),
                      data = noet_nd_obe_ne, family = binomial, nAGQ = 1, 
                      control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000)))

summary(model_noet)

avg_comparisons(model_noet, variables = "exposure", by = "vax_type")

plot_comparisons(model_noet, variables = "exposure", by = "vax_type") +
  labs(y = "Marginal Effect", x = "Vaccine Type") +
  scale_x_discrete(labels = c("vax_inject" = "≥ 2 doses of antenatal tetanus", 
                              "vax_bcg" = "BCG", 
                              "vax_measles" = "Measles")) +
  theme(text = element_text(size = 14)) +
  aes(color = vax_type) + 
  scale_color_brewer(type = "qual", palette = "Dark2") +
  geom_hline(yintercept = 0, color = "darkblue", linewidth = 0.8, linetype = "dashed", inherit.aes = FALSE) +
  scale_y_continuous(limits = c(-0.2, 0.2))

avg_slopes(model_noet, variables = "agechild_centered")
avg_slopes(model_noet, variables = "wi_centered")

##run model without india##
noin_nd_obe_ne = nd_obe_ne %>% 
  filter(country != "in")

model_noin <- glmer(vax_status ~ exposure * vax_type + wi_centered + agechild_centered +
                      (1 | country/clustid/childid),
                    data = noin_nd_obe_ne, family = binomial, nAGQ = 1, 
                    control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000)))

summary(model_noin)

avg_comparisons(model_noin, variables = "exposure", by = "vax_type")

plot_comparisons(model_noin, variables = "exposure", by = "vax_type") +
  labs(y = "Marginal Effect", x = "Vaccine Type") +
  scale_x_discrete(labels = c("vax_inject" = "≥ 2 doses of antenatal tetanus", 
                              "vax_bcg" = "BCG", 
                              "vax_measles" = "Measles")) +
  theme(text = element_text(size = 14)) +
  aes(color = vax_type) + 
  scale_color_brewer(type = "qual", palette = "Dark2") +
  geom_hline(yintercept = 0, color = "darkblue", linewidth = 0.8, linetype = "dashed", inherit.aes = FALSE) +
  scale_y_continuous(limits = c(-0.2, 0.2))

avg_slopes(model_noin, variables = "agechild_centered")
avg_slopes(model_noin, variables = "wi_centered")

##run model without peru##
nope_nd_obe_ne = nd_obe_ne %>% 
  filter(country != "pe")

model_nope <- glmer(vax_status ~ exposure * vax_type + wi_centered + agechild_centered +
                      (1 | country/clustid/childid),
                    data = nope_nd_obe_ne, family = binomial, nAGQ = 1, 
                    control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000)))

summary(model_nope)

avg_comparisons(model_nope, variables = "exposure", by = "vax_type")

plot_comparisons(model_nope, variables = "exposure", by = "vax_type") +
  labs(y = "Marginal Effect", x = "Vaccine Type") +
  scale_x_discrete(labels = c("vax_inject" = "≥ 2 doses of antenatal tetanus", 
                              "vax_bcg" = "BCG", 
                              "vax_measles" = "Measles")) +
  theme(text = element_text(size = 14)) +
  aes(color = vax_type) + 
  scale_color_brewer(type = "qual", palette = "Dark2") +
  geom_hline(yintercept = 0, color = "darkblue", linewidth = 0.8, linetype = "dashed", inherit.aes = FALSE) +
  scale_y_continuous(limits = c(-0.2, 0.2))

avg_slopes(model_nope, variables = "agechild_centered")
avg_slopes(model_nope, variables = "wi_centered")

##run model without vietnam##
novt_nd_obe_ne = nd_obe_ne %>% 
  filter(country != "vt")

model_novt <- glmer(vax_status ~ exposure * vax_type + wi_centered + agechild_centered +
                      (1 | country/clustid/childid),
                    data = novt_nd_obe_ne, family = binomial, nAGQ = 1, 
                    control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000)))

summary(model_novt)

avg_comparisons(model_novt, variables = "exposure", by = "vax_type")

plot_comparisons(model_novt, variables = "exposure", by = "vax_type") +
  labs(y = "Marginal Effect", x = "Vaccine Type") +
  scale_x_discrete(labels = c("vax_inject" = "≥ 2 doses of antenatal tetanus", 
                              "vax_bcg" = "BCG", 
                              "vax_measles" = "Measles")) +
  theme(text = element_text(size = 14)) +
  aes(color = vax_type) + 
  scale_color_brewer(type = "qual", palette = "Dark2") +
  geom_hline(yintercept = 0, color = "darkblue", linewidth = 0.8, linetype = "dashed", inherit.aes = FALSE) +
  scale_y_continuous(limits = c(-0.2, 0.2))

avg_slopes(model_novt, variables = "agechild_centered")
avg_slopes(model_novt, variables = "wi_centered")

#make a 4-panel plot showing results when leaving each country out
models <- list(
  "Results of Model Without Ethiopia" = model_noet,
  "Results of Model Without India" = model_noin,
  "Results of Model Without Peru" = model_nope,
  "Results of Model Without Vietnam" = model_novt
)

df <- imap_dfr(models, ~{
  plot_comparisons(.x, variables = "exposure", by = "vax_type", draw = FALSE) %>%
    mutate(country = .y)
})

ylims <- range(df$estimate, df$conf.low, df$conf.high, na.rm = TRUE)

ggplot(df, aes(x = vax_type, y = estimate, color = vax_type)) +
  geom_point(position = position_dodge(width = 0.4)) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                width = 0.2, position = position_dodge(width = 0.4)) +
  geom_hline(yintercept = 0, color = "darkblue", linewidth = 0.8, linetype = "dashed") +
  scale_color_brewer(type = "qual", palette = "Dark2") +
  labs(y = "Marginal Effect", x = "Vaccine Type") +
  scale_x_discrete(labels = c("vax_inject" = "≥ 2 doses of antenatal tetanus", 
                              "vax_bcg" = "BCG", 
                              "vax_measles" = "Measles")) +
  facet_wrap(~country, ncol = 2, scales = "fixed") +
  scale_y_continuous(limits = ylims) +
  theme(text = element_text(size = 14))

### sensitivity analysis #4 ##

#run model with only biological mothers as respondents

biomum_df = nd_obe_ne %>% 
  filter(biomum == 1)

biomum_model <- glmer(vax_status ~ exposure * vax_type + wi_centered + agechild_centered +
                   (1 | country/clustid/childid),
                 data = biomum_df, family = binomial, nAGQ = 1, 
                 control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000)))

summary(biomum_model)

#AIC to check model fit
AIC(biomum_model)

#DHARMa for residual diagnostics and model fit checks
sim_res <- simulateResiduals(biomum_model)
plot(sim_res)
testDispersion(sim_res)
testOutliers(sim_res)
testQuantiles(sim_res)
sim_res_grouped <- recalculateResiduals(sim_res, group = biomum_df$clustid)
plot(sim_res_grouped)

#compute average predicted outcome for each combination of exposure & moderator 
avg_predictions(biomum_model, by = c("exposure", "vax_type"))

#plot "average predicted probability that Y=1 for different values of moderator & exposure"
plot_predictions(biomum_model, by = c("vax_type", "exposure")) +
  labs(y = "Predicted Probability", x = "Vaccine Type") +
  scale_x_discrete(labels = c("vax_inject" = "≥ 2 doses of antenatal tetanus", 
                              "vax_bcg" = "BCG", 
                              "vax_measles" = "Measles")) +
  theme(text = element_text(size = 14)) +
  aes(color = exposure) + 
  scale_color_brewer(type = "qual", palette = "Set2") 

#compute average counterfactual comparison
avg_comparisons(biomum_model, variables = "exposure")

avg_comparisons(biomum_model, variables = "exposure", by = "vax_type")

#plot marginal effects 
plot_comparisons(biomum_model, variables = "exposure", by = "vax_type") +
  labs(y = "Marginal Effect", x = "Vaccine Type") +
  scale_x_discrete(labels = c("vax_inject" = "≥ 2 doses of antenatal tetanus", 
                              "vax_bcg" = "BCG", 
                              "vax_measles" = "Measles")) +
  theme(text = element_text(size = 14)) +
  aes(color = vax_type) + 
  scale_color_brewer(type = "qual", palette = "Dark2") +
  geom_hline(yintercept = 0, color = "darkblue", linewidth = 0.8, linetype = "dashed", inherit.aes = FALSE)

#run model with only only other primary caregivers as respondents

opc_df = nd_obe_ne %>% 
  filter(biomum == 0)

opc_model <- glmer(vax_status ~ exposure * vax_type + wi_centered + agechild_centered +
                        (1 | clustid/childid),
                      data = opc_df, family = binomial, nAGQ = 1, 
                      control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000)))

#DHARMa for residual diagnostics and model fit checks
sim_res <- simulateResiduals(opc_model)
plot(sim_res)
testDispersion(sim_res)
testOutliers(sim_res)
testQuantiles(sim_res)

#compute average predicted outcome for each combination of exposure & moderator 
avg_predictions(opc_model, by = c("exposure", "vax_type"))

#plot "average predicted probability that Y=1 for different values of moderator & exposure"
plot_predictions(opc_model, by = c("vax_type", "exposure")) +
  labs(y = "Predicted Probability", x = "Vaccine Type") +
  scale_x_discrete(labels = c("vax_bcg" = "BCG", 
                              "vax_measles" = "Measles")) +
  theme(text = element_text(size = 14)) +
  aes(color = exposure) + 
  scale_color_brewer(type = "qual", palette = "Set2") 

#compute average counterfactual comparison
avg_comparisons(opc_model, variables = "exposure")

avg_comparisons(opc_model, variables = "exposure", by = "vax_type")

#plot marginal effects 
plot_comparisons(opc_model, variables = "exposure", by = "vax_type") +
  labs(y = "Marginal Effect", x = "Vaccine Type") +
  scale_x_discrete(labels = c("vax_bcg" = "BCG", 
                              "vax_measles" = "Measles")) +
  theme(text = element_text(size = 14)) +
  aes(color = vax_type) + 
  scale_color_brewer(type = "qual", palette = "Dark2") +
  geom_hline(yintercept = 0, color = "darkblue", linewidth = 0.8, linetype = "dashed", inherit.aes = FALSE)


#--------------------------
#model individual countries with all vaccinations together: nd_obe/nd_ne/nd_obe/ne

#ethiopia

#nd_obe

model_g <- glmer(vax_status ~ exposure + vax_type + wi_centered + agechild_centered + 
                   (1 | region/clustid/childid) ,
                 data = et_nd_obe, family = binomial, nAGQ = 1, 
                 control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000)))

summary(model_g)

#AIC to check model fit
AIC(model_g)

#DHARMa for residual diagnostics and model fit checks
sim_res <- simulateResiduals(model_g)
plot(sim_res)
testDispersion(sim_res)
testOutliers(sim_res)

#interaction nd_obe

model_h <- glmer(vax_status ~ exposure * vax_type + wi_centered + agechild_centered + 
                   (1 | commid/childid) ,
                 data = et_nd_obe, family = binomial, nAGQ = 1, 
                 control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000)))

summary(model_h)

#AIC to check model fit
AIC(model_h)

#DHARMa for residual diagnostics and model fit checks
sim_res <- simulateResiduals(model_h)
plot(sim_res)
testDispersion(sim_res)
testOutliers(sim_res)

#nd_ne

model_i <- glmer(vax_status ~ exposure + vax_type + wi_centered + agechild_centered + 
                   (1 | region/commid/childid) ,
                 data = et_nd_ne, family = binomial, nAGQ = 1, 
                 control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000)))

summary(model_i)

#AIC to check model fit
AIC(model_i)

#DHARMa for residual diagnostics and model fit checks
sim_res <- simulateResiduals(model_i)
plot(sim_res)
testDispersion(sim_res)
testOutliers(sim_res)

#interaction nd_ne

model_j <- glmer(vax_status ~ exposure * vax_type + wi_centered + agechild_centered + 
                   (1 | commid) ,
                 data = et_nd_ne, family = binomial, nAGQ = 1, 
                 control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000)))

summary(model_j)

#AIC to check model fit
AIC(model_j)

#DHARMa for residual diagnostics and model fit checks
sim_res <- simulateResiduals(model_j)
plot(sim_res)
testDispersion(sim_res)
testOutliers(sim_res)

#compute average predicted outcome for each combination of exposure & moderator 
avg_predictions(model_j, by = c("exposure", "vax_type"))

#plot "average predicted probability that Y=1 for different values of moderator & exposure"
plot_predictions(model_j, by = c("vax_type", "exposure"))

#compute average counterfactual comparison ("on average, moving from 0 to 1 on 
avg_comparisons(model_j, variables = "exposure")

#check for heterogeneity to see if effect of X on Y depends on Moderator 
avg_comparisons(model_j, variables = "exposure", by = "vax_type")

#are the differences btw moderator groups statistically significant? 
avg_comparisons(model_j,
                variables = "exposure",
                by = "vax_type",
                hypothesis = "b3 - b1 = 0")

#nd_obe_ne

model_k <- glmer(vax_status ~ exposure + vax_type + wi_centered + agechild_centered + 
                   (1 | region/clustid/childid) ,
                 data = et_nd_obe_ne, family = binomial, nAGQ = 1, 
                 control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000)))

summary(model_k)

#AIC to check model fit
AIC(model_k)

#DHARMa for residual diagnostics and model fit checks
sim_res <- simulateResiduals(model_k)
plot(sim_res)
testDispersion(sim_res)
testOutliers(sim_res)

#interaction nd_obe_ne

model_l <- glmer(vax_status ~ exposure * vax_type + wi_centered + agechild_centered + 
                   (1 | clustid/childid) ,
                 data = et_nd_obe_ne, family = binomial, nAGQ = 1, 
                 control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000)))

summary(model_l)

#AIC to check model fit
AIC(model_l)

#DHARMa for residual diagnostics and model fit checks
sim_res <- simulateResiduals(model_l)
plot(sim_res)
testDispersion(sim_res)
testOutliers(sim_res)

#compute average predicted outcome for each combination of exposure & moderator 
avg_predictions(model_l, by = c("exposure", "vax_type"))

#plot "average predicted probability that Y=1 for different values of moderator & exposure"
plot_predictions(model_l, by = c("vax_type", "exposure"))

#compute average counterfactual comparison ("on average, moving from 0 to 1 on 
avg_comparisons(model_l, variables = "exposure")

#check for heterogeneity to see if effect of X on Y depends on Moderator 
avg_comparisons(model_l, variables = "exposure", by = "vax_type")

#are the differences btw moderator groups statistically significant? 
avg_comparisons(model_l,
                variables = "exposure",
                by = "vax_type",
                hypothesis = "b3 - b1 = 0")

#india

#nd_obe

model_m <- glmer(vax_status ~ exposure + vax_type + wi_centered + agechild_centered + chldeth +
                   (1| commid) ,
                 data = in_nd_obe, family = binomial, nAGQ = 1, 
                 control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000)))

summary(model_m)

#AIC to check model fit
AIC(model_m)

#DHARMa for residual diagnostics and model fit checks
sim_res <- simulateResiduals(model_m)
plot(sim_res)
testDispersion(sim_res)
testOutliers(sim_res)

#interaction nd_obe

model_n <- glmer(vax_status ~ exposure * vax_type + wi_centered + agechild_centered + chldeth +
                   (1 | commid) ,
                 data = in_nd_obe, family = binomial, nAGQ = 1, 
                 control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000)))

summary(model_n)

#AIC to check model fit
AIC(model_n)

#DHARMa for residual diagnostics and model fit checks
sim_res <- simulateResiduals(model_n)
plot(sim_res)
testDispersion(sim_res)
testOutliers(sim_res)

#nd_ne

model_o <- glmer(vax_status ~ exposure + vax_type + wi_centered + agechild_centered + chldeth +
                   (1 | commid) ,
                 data = in_nd_ne, family = binomial, nAGQ = 1, 
                 control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000)))

summary(model_o)

#AIC to check model fit
AIC(model_o)

#DHARMa for residual diagnostics and model fit checks
sim_res <- simulateResiduals(model_o)
plot(sim_res)
testDispersion(sim_res)
testOutliers(sim_res)

#interaction nd_ne

model_p <- glmer(vax_status ~ exposure * vax_type + wi_centered + agechild_centered + chldeth +
                   (1 | commid) ,
                 data = in_nd_ne, family = binomial, nAGQ = 1, 
                 control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000)))

summary(model_p)

#AIC to check model fit
AIC(model_p)

#DHARMa for residual diagnostics and model fit checks
sim_res <- simulateResiduals(model_p)
plot(sim_res)
testDispersion(sim_res)
testOutliers(sim_res)

#compute average predicted outcome for each combination of exposure & moderator 
avg_predictions(model_p, by = c("exposure", "vax_type"))

#plot "average predicted probability that Y=1 for different values of moderator & exposure"
plot_predictions(model_p, by = c("vax_type", "exposure"))

#compute average counterfactual comparison ("on average, moving from 0 to 1 on 
avg_comparisons(model_p, variables = "exposure")

#check for heterogeneity to see if effect of X on Y depends on Moderator 
avg_comparisons(model_p, variables = "exposure", by = "vax_type")

#are the differences btw moderator groups statistically significant? 
avg_comparisons(model_p,
                variables = "exposure",
                by = "vax_type",
                hypothesis = "b3 - b1 = 0")

#nd_obe_ne

model_q <- glmer(vax_status ~ exposure + vax_type + wi_centered + agechild_centered + chldeth +
                   (1 | commid) ,
                 data = in_nd_obe_ne, family = binomial, nAGQ = 1, 
                 control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000)))

summary(model_q)

#AIC to check model fit
AIC(model_q)

#DHARMa for residual diagnostics and model fit checks
sim_res <- simulateResiduals(model_q)
plot(sim_res)
testDispersion(sim_res)
testOutliers(sim_res)

#interaction nd_obe_ne

model_r <- glmer(vax_status ~ exposure * vax_type + wi_centered + agechild_centered +
                   (1 | commid/childid),
                 data = in_nd_obe_ne, family = binomial, nAGQ = 1, 
                 control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000)))

summary(model_r)

#AIC to check model fit
AIC(model_r)

#DHARMa for residual diagnostics and model fit checks
sim_res <- simulateResiduals(model_r)
plot(sim_res)
testDispersion(sim_res)
testOutliers(sim_res)

#compute average predicted outcome for each combination of exposure & moderator 
avg_predictions(model_r, by = c("exposure", "vax_type"))

#plot "average predicted probability that Y=1 for different values of moderator & exposure"
plot_predictions(model_r, by = c("vax_type", "exposure"))

#compute average counterfactual comparison ("on average, moving from 0 to 1 on 
avg_comparisons(model_r, variables = "exposure")

#check for heterogeneity to see if effect of X on Y depends on Moderator 
avg_comparisons(model_r, variables = "exposure", by = "vax_type")

#are the differences btw moderator groups statistically significant? 
avg_comparisons(model_r,
                variables = "exposure",
                by = "vax_type",
                hypothesis = "b3 - b1 = 0")

#vietnam

#nd_obe

model_s <- glmer(vax_status ~ exposure + vax_type + wi_centered + agechild_centered +
                   (1 | region/clustid/childid),
                 data = vi_nd_obe, family = binomial, nAGQ = 1, 
                 control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000)))

summary(model_s)

#AIC to check model fit
AIC(model_s)

#DHARMa for residual diagnostics and model fit checks
sim_res <- simulateResiduals(model_s)
plot(sim_res)
testDispersion(sim_res)
testOutliers(sim_res)

#interaction nd_obe

model_t <- glmer(vax_status ~ exposure * vax_type + wi_centered + agechild_centered + 
                   (1 | region/commid/childid) ,
                 data = vi_nd_obe, family = binomial, nAGQ = 1, 
                 control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000)))

summary(model_t)

#AIC to check model fit
AIC(model_t)

#DHARMa for residual diagnostics and model fit checks
sim_res <- simulateResiduals(model_t)
plot(sim_res)
testDispersion(sim_res)
testOutliers(sim_res)

#nd_ne

model_u <- glmer(vax_status ~ exposure + vax_type + wi_centered + agechild_centered + 
                   (1 | region/commid/childid) ,
                 data = vi_nd_ne, family = binomial, nAGQ = 1, 
                 control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000)))

summary(model_u)

#AIC to check model fit
AIC(model_u)

#DHARMa for residual diagnostics and model fit checks
sim_res <- simulateResiduals(model_u)
plot(sim_res)
testDispersion(sim_res)
testOutliers(sim_res)

#interaction nd_ne

model_v <- glmer(vax_status ~ exposure * vax_type + wi_centered + agechild_centered + 
                   (1 | region/commid/childid) ,
                 data = vi_nd_ne, family = binomial, nAGQ = 1, 
                 control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000)))

summary(model_v)

#AIC to check model fit
AIC(model_v)

#DHARMa for residual diagnostics and model fit checks
sim_res <- simulateResiduals(model_v)
plot(sim_res)
testDispersion(sim_res)
testOutliers(sim_res)

#compute average predicted outcome for each combination of exposure & moderator 
avg_predictions(model_v, by = c("exposure", "vax_type"))

#plot "average predicted probability that Y=1 for different values of moderator & exposure"
plot_predictions(model_v, by = c("vax_type", "exposure"))

#compute average counterfactual comparison ("on average, moving from 0 to 1 on 
avg_comparisons(model_v, variables = "exposure")

#check for heterogeneity to see if effect of X on Y depends on Moderator 
avg_comparisons(model_v, variables = "exposure", by = "vax_type")

#are the differences btw moderator groups statistically significant? 
avg_comparisons(model_v,
                variables = "exposure",
                by = "vax_type",
                hypothesis = "b3 - b1 = 0")

#nd_obe_ne

model_w <- glmer(vax_status ~ exposure + vax_type + wi_centered + agechild_centered + 
                   (1 | region/clustid/childid) ,
                 data = vi_nd_obe_ne, family = binomial, nAGQ = 1, 
                 control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000)))

summary(model_w)

#AIC to check model fit
AIC(model_w)

#DHARMa for residual diagnostics and model fit checks
sim_res <- simulateResiduals(model_w)
plot(sim_res)
testDispersion(sim_res)
testOutliers(sim_res)

#interaction nd_obe_ne

model_x <- glmer(vax_status ~ exposure * vax_type + wi_centered + agechild_centered +
                   (1 | region/clustid/childid),
                 data = vi_nd_obe_ne, family = binomial, nAGQ = 1, 
                 control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000)))

summary(model_x)

#AIC to check model fit
AIC(model_x)

#DHARMa for residual diagnostics and model fit checks
sim_res <- simulateResiduals(model_x)
plot(sim_res)
testDispersion(sim_res)
testOutliers(sim_res)

#compute average predicted outcome for each combination of exposure & moderator 
avg_predictions(model_x, by = c("exposure", "vax_type"))

#plot "average predicted probability that Y=1 for different values of moderator & exposure"
plot_predictions(model_x, by = c("vax_type", "exposure"))

#compute average counterfactual comparison ("on average, moving from 0 to 1 on 
avg_comparisons(model_x, variables = "exposure")

#check for heterogeneity to see if effect of X on Y depends on Moderator 
avg_comparisons(model_x, variables = "exposure", by = "vax_type")

#are the differences btw moderator groups statistically significant? 
avg_comparisons(model_x,
                variables = "exposure",
                by = "vax_type",
                hypothesis = "b3 - b1 = 0")

#----------------------------------------
#run models for each vaccination type individually & all countries combined: nd_obe/nd_ne/nd_obe_ne

#nd_obe

#2+ prenatal tetanus

pretet_nd_obe = nd_obe %>% 
  filter(vax_type == "vax_inject")

model_1 <- glmer(vax_status ~ exposure + wi_centered + agechild_centered +
                   (1 | country/region/commid),
               data = pretet_nd_obe, family = binomial, nAGQ = 1, 
               control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000)))

summary(model_1)

#AIC to check model fit (lower AIC values indicate a better-fitting model)
AIC(model_1)

#DHARMa for residual diagnostics and model fit checks (distribution of residuals, 
#link function by plotting observed v. predicted values, & overdispersion)
sim_res <- simulateResiduals(model_1)
plot(sim_res)
testDispersion(sim_res)
testOutliers(sim_res)

#bcg

bcg_nd_obe = nd_obe %>% 
  filter(vax_type == "vax_bcg")

model_2 <- glmer(vax_status ~ exposure + wi_centered + agechild_centered +
                   (1 | country/region/commid) ,
                 data = bcg_nd_obe, family = binomial, nAGQ = 1, 
                 control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000)))

summary(model_2)

#check model fit
AIC(model_2)

#check assumptions of model with DHARMa
sim_res <- simulateResiduals(model_2)
plot(sim_res)
testDispersion(sim_res)
testOutliers(sim_res)

#measles

meas_nd_obe = nd_obe %>% 
  filter(vax_type == "vax_measles")

model_3 <- glmer(vax_status ~ exposure + wi_centered + agechild_centered + 
                   (1 | country/region/commid) ,
                 data = meas_nd_obe, family = binomial, nAGQ = 1, 
                 control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000)))

summary(model_3)

#check model fit
AIC(model_3)

#check assumptions of model with DHARMa
sim_res <- simulateResiduals(model_3)
plot(sim_res)
testDispersion(sim_res)
testOutliers(sim_res)

#polio

polio_nd_obe = pluspolio_nd_obe %>% 
  filter(vax_type == "vax_polio")

model_4 <- glmer(vax_status ~ exposure + wi_centered + agechild_centered + 
                    (1 | country/region/clustid) ,
                 data = polio_nd_obe, family = binomial, nAGQ = 1, 
                 control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000)))

summary(model_4)

#check model fit
AIC(model_4)

#check assumptions of model with DHARMa
sim_res <- simulateResiduals(model_4)
plot(sim_res)
testDispersion(sim_res)
testOutliers(sim_res)

#--------------------------
#nd_ne models for each vaccine type individually & all countries combined 

#2+ prenatal tetanus

pretet_nd_ne = nd_ne %>% 
  filter(vax_type == "vax_inject")

model_5 <- glmer(vax_status ~ exposure + wi_centered + agechild_centered +
                   (1 | country/region/commid) ,
                 data = pretet_nd_ne, family = binomial, nAGQ = 1, 
                 control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000)))

summary(model_5)

#AIC to check model fit (lower AIC values indicate a better-fitting model)
AIC(model_5)

#DHARMa for residual diagnostics and model fit checks
sim_res <- simulateResiduals(model_5)
plot(sim_res)
testDispersion(sim_res)
testOutliers(sim_res)

#bcg

bcg_nd_ne = nd_ne %>% 
  filter(vax_type == "vax_bcg")

model_6 <- glmer(vax_status ~ exposure + wi_centered + agechild_centered + 
                   (1 | country/region/commid) ,
                 data = bcg_nd_ne, family = binomial, nAGQ = 1, 
                 control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000)))

summary(model_6)

#check model fit
AIC(model_6)

#check assumptions of model with DHARMa
sim_res <- simulateResiduals(model_6)
plot(sim_res)
testDispersion(sim_res)
testOutliers(sim_res)

#measles

meas_nd_ne = nd_ne %>% 
  filter(vax_type == "vax_measles")

model_7 <- glmer(vax_status ~ exposure + wi_centered + agechild_centered + 
                   (1 | country/region/commid) ,
                 data = meas_nd_ne, family = binomial, nAGQ = 1, 
                 control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000)))

summary(model_7)

#check model fit
AIC(model_7)

#check assumptions of model with DHARMa
sim_res <- simulateResiduals(model_7)
plot(sim_res)
testDispersion(sim_res)
testOutliers(sim_res)

#polio

polio_nd_ne = pluspolio_nd_ne %>% 
  filter(vax_type == "vax_polio")

model_8 <- glmer(vax_status ~ exposure + wi_centered + agechild_centered + 
                 (1 | country/region/clustid) ,
                 data = polio_nd_ne, family = binomial, nAGQ = 1, 
                 control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000)))

summary(model_8)

#check model fit
AIC(model_8)

#check assumptions of model with DHARMa
sim_res <- simulateResiduals(model_8)
plot(sim_res)
testDispersion(sim_res)
testOutliers(sim_res)

#--------------------------
#nd_obe_ne models for each vaccine type individually & all countries combined 

#2+ prenatal tetanus

pretet_nd_obe_ne = nd_obe_ne %>% 
  filter(vax_type == "vax_inject")

model_9 <- glmer(vax_status ~ exposure + wi_centered + agechild_centered + 
                   (1 | country/region/clustid) ,
                 data = pretet_nd_obe_ne, family = binomial, nAGQ = 1, 
                 control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000)))

summary(model_9)

#AIC to check model fit (lower AIC values indicate a better-fitting model)
AIC(model_9)

#DHARMa for residual diagnostics and model fit checks
sim_res <- simulateResiduals(model_9)
plot(sim_res)
testDispersion(sim_res)
testOutliers(sim_res)

#bcg

bcg_nd_obe_ne = nd_obe_ne %>% 
  filter(vax_type == "vax_bcg")

model_10 <- glmer(vax_status ~ exposure + wi_centered + agechild_centered + 
                   (1 | country/region/commid) ,
                 data = bcg_nd_obe_ne, family = binomial, nAGQ = 1, 
                 control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000)))

summary(model_10)

#check model fit
AIC(model_10)

#check assumptions of model with DHARMa
sim_res <- simulateResiduals(model_10)
plot(sim_res)
testDispersion(sim_res)
testOutliers(sim_res)

#measles

meas_nd_obe_ne = nd_obe_ne %>% 
  filter(vax_type == "vax_measles")

model_11 <- glmer(vax_status ~ exposure + wi_centered + agechild_centered + 
                   (1 | country/region/commid) ,
                 data = meas_nd_obe_ne, family = binomial, nAGQ = 1, 
                 control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000)))

summary(model_11)

#check model fit
AIC(model_11)

#check assumptions of model with DHARMa
sim_res <- simulateResiduals(model_11)
plot(sim_res)
testDispersion(sim_res)
testOutliers(sim_res)

#polio

polio_nd_obe_ne = pluspolio_nd_obe_ne %>% 
  filter(vax_type == "vax_polio")

model_12 <- glmer(vax_status ~ exposure + wi_centered + agechild_centered + 
                   (1 | country/region/clustid) ,
                 data = polio_nd_obe_ne, family = binomial, nAGQ = 1, 
                 control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000)))

summary(model_12)

#check model fit
AIC(model_12)

#check assumptions of model with DHARMa
sim_res <- simulateResiduals(model_12)
plot(sim_res)
testDispersion(sim_res)
testOutliers(sim_res)

#-----------------------------------------
#run models for individual countries
#-----------------------------------------

#ethiopia##

#nd_obe#

#2+ prenatal tetanus

et_pretet_nd_obe = et_nd_obe %>% 
  filter(vax_type == "vax_inject")

model_19 <- glmer(vax_status ~ exposure + wi_centered + agechild_centered + 
                    (1 | region/commid) ,
                  data = et_pretet_nd_obe, family = binomial, nAGQ = 1, 
                  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000)))

summary(model_19)

#AIC to check model fit (lower AIC values indicate a better-fitting model)
AIC(model_19)

#DHARMa for residual diagnostics and model fit checks
sim_res <- simulateResiduals(model_19)
plot(sim_res)
testDispersion(sim_res)
testOutliers(sim_res)

#bcg

et_bcg_nd_obe = et_nd_obe %>% 
  filter(vax_type == "vax_bcg")

model_20 <- glmer(vax_status ~ exposure + wi_centered + agechild_centered + 
                    (1 | clustid),
                  data = et_bcg_nd_obe, family = binomial, nAGQ = 1, 
                  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000)))

summary(model_20)

#check model fit
AIC(model_20)

#check assumptions of model with DHARMa
sim_res <- simulateResiduals(model_20)
plot(sim_res)
testDispersion(sim_res)
testOutliers(sim_res)

#measles

et_meas_nd_obe = et_nd_obe %>% 
  filter(vax_type == "vax_measles")

model_21 <- glmer(vax_status ~ exposure + wi_centered + agechild_centered + 
                    (1 | commid) ,
                  data = et_meas_nd_obe, family = binomial, nAGQ = 1, 
                  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000)))

summary(model_21)

#check model fit
AIC(model_21)

#check assumptions of model with DHARMa
sim_res <- simulateResiduals(model_21)
plot(sim_res)
testDispersion(sim_res)
testOutliers(sim_res)

#nd_ne

#2+ prenatal tetanus

et_pretet_nd_ne = et_nd_ne %>% 
  filter(vax_type == "vax_inject")

model_16 <- glmer(vax_status ~ exposure + wi_centered + agechild_centered + 
                    (1 | region) ,
                  data = et_pretet_nd_ne, family = binomial, nAGQ = 1, 
                  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000)))

summary(model_16)

#AIC to check model fit (lower AIC values indicate a better-fitting model)
AIC(model_16)

#DHARMa for residual diagnostics and model fit checks
sim_res <- simulateResiduals(model_16)
plot(sim_res)
testDispersion(sim_res)
testOutliers(sim_res)

#bcg

et_bcg_nd_ne = et_nd_ne %>% 
  filter(vax_type == "vax_bcg")

model_17 <- glmer(vax_status ~ exposure + wi_centered + agechild_centered + 
                    (1 | clustid) ,
                  data = et_bcg_nd_ne, family = binomial, nAGQ = 1, 
                  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000)))

summary(model_17)

#check model fit
AIC(model_17)

#check assumptions of model with DHARMa
sim_res <- simulateResiduals(model_17)
plot(sim_res)
testDispersion(sim_res)
testOutliers(sim_res)

#measles

et_meas_nd_ne = et_nd_ne %>% 
  filter(vax_type == "vax_measles")

model_18 <- glmer(vax_status ~ exposure + wi_centered + agechild_centered + 
                    (1 | commid) ,
                  data = et_meas_nd_ne, family = binomial, nAGQ = 1, 
                  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000)))

summary(model_18)

#check model fit
AIC(model_18)

#check assumptions of model with DHARMa
sim_res <- simulateResiduals(model_18)
plot(sim_res)
testDispersion(sim_res)
testOutliers(sim_res)

#nd_obe_ne

#2+ prenatal tetanus

et_pretet_nd_obe_ne = et_nd_obe_ne %>% 
  filter(vax_type == "vax_inject")

model_13 <- glmer(vax_status ~ exposure + wi_centered + agechild_centered + 
                    (1 | region) ,
                  data = et_pretet_nd_obe_ne, family = binomial, nAGQ = 1, 
                  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000)))

summary(model_13)

#AIC to check model fit (lower AIC values indicate a better-fitting model)
AIC(model_13)

#DHARMa for residual diagnostics and model fit checks
sim_res <- simulateResiduals(model_13)
plot(sim_res)
testDispersion(sim_res)
testOutliers(sim_res)

#bcg

et_bcg_nd_obe_ne = et_nd_obe_ne %>% 
  filter(vax_type == "vax_bcg")

model_14 <- glmer(vax_status ~ exposure + wi_centered + agechild_centered + region +
                    (1 | clustid) ,
                  data = et_bcg_nd_obe_ne, family = binomial, nAGQ = 1, 
                  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000)))

summary(model_14)

#check model fit
AIC(model_14)

#check assumptions of model with DHARMa
sim_res <- simulateResiduals(model_14)
plot(sim_res)
testDispersion(sim_res)
testOutliers(sim_res)

#measles

et_meas_nd_obe_ne = et_nd_obe_ne %>% 
  filter(vax_type == "vax_measles")

model_15 <- glmer(vax_status ~ exposure + wi_centered + agechild_centered + 
                    (1 | commid) ,
                  data = et_meas_nd_obe_ne, family = binomial, nAGQ = 1, 
                  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000)))

summary(model_15)

#check model fit
AIC(model_15)

#check assumptions of model with DHARMa
sim_res <- simulateResiduals(model_15)
plot(sim_res)
testDispersion(sim_res)
testOutliers(sim_res)

#---------------------------
#india

#nd_obe

#2+ prenatal tetanus

in_pretet_nd_obe = in_nd_obe %>% 
  filter(vax_type == "vax_inject")

model_22 <- glmer(vax_status ~ exposure + wi_centered + agechild_centered +
                   (1 | region) ,
                  data = in_pretet_nd_obe, family = binomial, nAGQ = 1, 
                  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000)))

summary(model_22)

#AIC to check model fit (lower AIC values indicate a better-fitting model)
AIC(model_22)

#DHARMa for residual diagnostics and model fit checks
sim_res <- simulateResiduals(model_22)
plot(sim_res)
testDispersion(sim_res)
testOutliers(sim_res)

#bcg

in_bcg_nd_obe = in_nd_obe %>% 
  filter(vax_type == "vax_bcg")

model_23 <- glmer(vax_status ~ exposure + wi_centered + agechild_centered + 
                    (1 | clustid) ,
                  data = in_bcg_nd_obe, family = binomial, nAGQ = 1, 
                  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000)))

summary(model_23)

#check model fit
AIC(model_23)

#check assumptions of model with DHARMa
sim_res <- simulateResiduals(model_23)
plot(sim_res)
testDispersion(sim_res)
testOutliers(sim_res)

#measles

in_meas_nd_obe = in_nd_obe %>% 
  filter(vax_type == "vax_measles")

model_24 <- glmer(vax_status ~ exposure + wi_centered + agechild_centered + 
                    (1 | region) ,
                  data = in_meas_nd_obe, family = binomial, nAGQ = 1, 
                  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000)))

summary(model_24)

#check model fit
AIC(model_24)

#check assumptions of model with DHARMa
sim_res <- simulateResiduals(model_24)
plot(sim_res)
testDispersion(sim_res)
testOutliers(sim_res)

#polio

in_pol_nd_obe = in_nd_obe %>% 
  filter(vax_type == "vax_polio")

model_33 <- glmer(vax_status ~ exposure + wi_centered + agechild_centered + 
                    (1 | region) ,
                  data = in_pol_nd_obe, family = binomial, nAGQ = 1, 
                  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000)))

summary(model_33)

#check model fit
AIC(model_33)

#check assumptions of model with DHARMa
sim_res <- simulateResiduals(model_33)
plot(sim_res)
testDispersion(sim_res)
testOutliers(sim_res)

#nd_ne

#2+ prenatal tetanus

in_pretet_nd_ne = in_nd_ne %>% 
  filter(vax_type == "vax_inject")

model_25 <- glmer(vax_status ~ exposure + wi_centered + agechild_centered + 
                    (1 | region) ,
                  data = in_pretet_nd_ne, family = binomial, nAGQ = 1, 
                  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000)))

summary(model_25)

#AIC to check model fit (lower AIC values indicate a better-fitting model)
AIC(model_25)

#DHARMa for residual diagnostics and model fit checks
sim_res <- simulateResiduals(model_25)
plot(sim_res)
testDispersion(sim_res)
testOutliers(sim_res)

#bcg

in_bcg_nd_ne = in_nd_ne %>% 
  filter(vax_type == "vax_bcg")

model_26 <- glmer(vax_status ~ exposure + wi_centered + agechild_centered + 
                    (1 | clustid) ,
                  data = in_bcg_nd_ne, family = binomial, nAGQ = 1, 
                  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000)))

summary(model_26)

#check model fit
AIC(model_26)

#check assumptions of model with DHARMa
sim_res <- simulateResiduals(model_26)
plot(sim_res)
testDispersion(sim_res)
testOutliers(sim_res)

#measles

in_meas_nd_ne = in_nd_ne %>% 
  filter(vax_type == "vax_measles")

model_27 <- glmer(vax_status ~ exposure + wi_centered + agechild_centered + 
                    (1 | commid) ,
                  data = in_meas_nd_ne, family = binomial, nAGQ = 1, 
                  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000)))

summary(model_27)

#check model fit
AIC(model_27)

#check assumptions of model with DHARMa
sim_res <- simulateResiduals(model_27)
plot(sim_res)
testDispersion(sim_res)
testOutliers(sim_res)

#polio

in_pol_nd_ne = in_nd_ne %>% 
  filter(vax_type == "vax_polio")

model_32 <- glmer(vax_status ~ exposure + wi_centered + agechild_centered + 
                    (1 | commid) ,
                  data = in_pol_nd_ne, family = binomial, nAGQ = 1, 
                  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000)))

summary(model_32)

#check model fit
AIC(model_32)

#check assumptions of model with DHARMa
sim_res <- simulateResiduals(model_32)
plot(sim_res)
testDispersion(sim_res)
testOutliers(sim_res)

#nd_obe_ne

#2+ prenatal tetanus

in_pretet_nd_obe_ne = in_nd_obe_ne %>% 
  filter(vax_type == "vax_inject")

model_28 <- glmer(vax_status ~ exposure + wi_centered + agechild_centered + 
                    (1 | clustid) ,
                  data = in_pretet_nd_obe_ne, family = binomial, nAGQ = 1, 
                  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000)))

summary(model_28)

#AIC to check model fit (lower AIC values indicate a better-fitting model)
AIC(model_28)

#DHARMa for residual diagnostics and model fit checks
sim_res <- simulateResiduals(model_28)
plot(sim_res)
testDispersion(sim_res)
testOutliers(sim_res)

#bcg

in_bcg_nd_obe_ne = in_nd_obe_ne %>% 
  filter(vax_type == "vax_bcg")

model_29 <- glmer(vax_status ~ exposure + wi_centered + agechild_centered + 
                  (1 | commid) ,
                  data = in_bcg_nd_obe_ne, family = binomial, nAGQ = 1, 
                  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000)))

summary(model_29)

#check model fit
AIC(model_29)

#check assumptions of model with DHARMa
sim_res <- simulateResiduals(model_29)
plot(sim_res)
testDispersion(sim_res)
testOutliers(sim_res)

#measles

in_meas_nd_obe_ne = in_nd_obe_ne %>% 
  filter(vax_type == "vax_measles")

model_30 <- glmer(vax_status ~ exposure + wi_centered + agechild_centered + 
                    (1 | commid) ,
                  data = in_meas_nd_obe_ne, family = binomial, nAGQ = 1, 
                  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000)))

summary(model_30)

#check model fit
AIC(model_30)

#check assumptions of model with DHARMa
sim_res <- simulateResiduals(model_30)
plot(sim_res)
testDispersion(sim_res)
testOutliers(sim_res)

#polio

in_pol_nd_obe_ne = in_nd_obe_ne %>% 
  filter(vax_type == "vax_polio")

model_31 <- glmer(vax_status ~ exposure + wi_centered + agechild_centered + 
                    (1 | clustid) ,
                  data = in_pol_nd_obe_ne, family = binomial, nAGQ = 1, 
                  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000)))

summary(model_31)

#check model fit
AIC(model_31)

#check assumptions of model with DHARMa
sim_res <- simulateResiduals(model_31)
plot(sim_res)
testDispersion(sim_res)
testOutliers(sim_res)


#---------------------------
#vietnam

#nd_obe

#2+ prenatal tetanus

vi_pretet_nd_obe = vi_nd_obe %>% 
  filter(vax_type == "vax_inject")

model_46 <- glmer(vax_status ~ exposure + wi_centered + agechild_centered + 
                    (1 | region/clustid) ,
                  data = vi_pretet_nd_obe, family = binomial, nAGQ = 1, 
                  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000)))

summary(model_46)

#AIC to check model fit (lower AIC values indicate a better-fitting model)
AIC(model_46)

#DHARMa for residual diagnostics and model fit checks
sim_res <- simulateResiduals(model_46)
plot(sim_res)
testDispersion(sim_res)
testOutliers(sim_res)

#bcg

vi_bcg_nd_obe = vi_nd_obe %>% 
  filter(vax_type == "vax_bcg")

model_46 <- glmer(vax_status ~ exposure + wi_centered + agechild_centered + 
                    (1 | region/clustid) ,
                  data = vi_bcg_nd_obe, family = binomial, nAGQ = 1, 
                  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000)))

summary(model_46)

#check model fit
AIC(model_46)

#check assumptions of model with DHARMa
sim_res <- simulateResiduals(model_46)
plot(sim_res)
testDispersion(sim_res)
testOutliers(sim_res)

#measles

vi_meas_nd_obe = vi_nd_obe %>% 
  filter(vax_type == "vax_measles")

model_47 <- glmer(vax_status ~ exposure + wi_centered + agechild_centered +
                  (1 | region/commid) ,
                  data = vi_meas_nd_obe, family = binomial, nAGQ = 1, 
                  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000)))

summary(model_47)

#check model fit
AIC(model_47)

#check assumptions of model with DHARMa
sim_res <- simulateResiduals(model_47)
plot(sim_res)
testDispersion(sim_res)
testOutliers(sim_res)

#nd_ne

#2+ prenatal tetanus

vi_pretet_nd_ne = vi_nd_ne %>% 
  filter(vax_type == "vax_inject")

model_48 <- glmer(vax_status ~ exposure + wi_centered + agechild_centered + 
                  (1 | region/clustid) ,
                  data = vi_pretet_nd_ne, family = binomial, nAGQ = 1, 
                  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000)))

summary(model_48)

#AIC to check model fit (lower AIC values indicate a better-fitting model)
AIC(model_48)

#DHARMa for residual diagnostics and model fit checks
sim_res <- simulateResiduals(model_48)
plot(sim_res)
testDispersion(sim_res)
testOutliers(sim_res)

#bcg

vi_bcg_nd_ne = vi_nd_ne %>% 
  filter(vax_type == "vax_bcg")

model_49 <- glmer(vax_status ~ exposure + wi_centered + agechild_centered + 
                  (1 | commid) ,
                  data = vi_bcg_nd_ne, family = binomial, nAGQ = 1, 
                  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000)))

summary(model_49)

#check model fit
AIC(model_49)

#check assumptions of model with DHARMa
sim_res <- simulateResiduals(model_49)
plot(sim_res)
testDispersion(sim_res)
testOutliers(sim_res)

#measles

vi_meas_nd_ne = vi_nd_ne %>% 
  filter(vax_type == "vax_measles")

model_50 <- glmer(vax_status ~ exposure + wi_centered + agechild_centered + 
                   (1 | commid) ,
                  data = vi_meas_nd_ne, family = binomial, nAGQ = 1, 
                  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000)))

summary(model_50)

#check model fit
AIC(model_50)

#check assumptions of model with DHARMa
sim_res <- simulateResiduals(model_50)
plot(sim_res)
testDispersion(sim_res)
testOutliers(sim_res)

#nd_obe_ne

#2+ prenatal tetanus

vi_pretet_nd_obe_ne = vi_nd_obe_ne %>% 
  filter(vax_type == "vax_inject")

model_51 <- glmer(vax_status ~ exposure + wi_centered + agechild_centered +
                      (1 | clustid) ,
                  data = vi_pretet_nd_obe_ne, family = binomial, nAGQ = 1, 
                  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000)))

summary(model_51)

#AIC to check model fit (lower AIC values indicate a better-fitting model)
AIC(model_51)

#DHARMa for residual diagnostics and model fit checks
sim_res <- simulateResiduals(model_51)
plot(sim_res)
testDispersion(sim_res)
testOutliers(sim_res)

#bcg

vi_bcg_nd_obe_ne = vi_nd_obe_ne %>% 
  filter(vax_type == "vax_bcg")

model_52 <- glmer(vax_status ~ exposure + wi_centered + agechild_centered + 
                    (1 | clustid) ,
                  data = vi_bcg_nd_obe_ne, family = binomial, nAGQ = 1, 
                  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000)))

summary(model_52)

#check model fit
AIC(model_52)

#check assumptions of model with DHARMa
sim_res <- simulateResiduals(model_52)
plot(sim_res)
testDispersion(sim_res)
testOutliers(sim_res)

#measles

vi_meas_nd_obe_ne = vi_nd_obe_ne %>% 
  filter(vax_type == "vax_measles")

model_53 <- glmer(vax_status ~ exposure + wi_centered + agechild_centered + 
                 (1 | region/commid) ,
                  data = vi_meas_nd_obe_ne, family = binomial, nAGQ = 1, 
                  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000)))

summary(model_53)

#check model fit
AIC(model_53)

#check assumptions of model with DHARMa
sim_res <- simulateResiduals(model_53)
plot(sim_res)
testDispersion(sim_res)
testOutliers(sim_res)

#---------------------------
#create table with OR and CI's

results <- tidy(model, effects = "fixed", conf.int = TRUE, exponentiate = TRUE) %>%
  select(term, estimate, std.error, conf.low, conf.high, p.value) %>%
  rename(
    Predictor = term,
    `Odds Ratio` = estimate,
    SE = std.error,
    `95% CI Lower` = conf.low,
    `95% CI Upper` = conf.high,
    `p-value` = p.value
  )

print(results)

#-------------------------------
#plot confidence intervals

tidy_model <- tidy(model_1, effects = "fixed", conf.int = TRUE, exponentiate = TRUE)

#plot confidence intervals
ciplot <- ggplot(tidy_model, aes(x = term, y = estimate)) +
  geom_point() +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
  geom_hline(yintercept=1, linetype="dashed", color="red") + 
  labs(title = "Model Coefficients with Confidence Intervals",
       x = "Predictor",
       y = "Odds Ratio") +
  theme_minimal() +
  coord_flip()

ggsave("ciplot.png", plot = ciplot, width = 6, height = 3.5, units = "in", dpi = 350)

#------------------------------------
#False Discovery Rate (FDR) test/Benjamini-Hochberg procedure

#extract p-values from the model summary
p_values <- coef(summary(model_h))[, "Pr(>|z|)"]

#apply the BH procedure
p_adjusted <- p.adjust(p_values, method = "fdr")

#create df with the results
results <- data.frame(
  Term = rownames(coef(summary(model_h))),
  Estimate = coef(summary(model_h))[, "Estimate"],
  Std_Error = coef(summary(model_h))[, "Std. Error"],
  z_value = coef(summary(model_h))[, "z value"],
  p_value = p_values,
  p_adjusted = p_adjusted
)

#determine significance from adjusted p-values
results$Significant <- results$p_adjusted < 0.05

print(results)

#------------------------------------
#check if random effects are normally distributed

re <- ranef(model_1)$commid
hist(re[,1], main = "Histogram of Random Effects", xlab = "Random Effect Value")
qqnorm(re[,1])
qqline(re[,1])

#------------------------------------
#check for multicollinearity by fitting a glm model without random effects

glm_model <- glm(vax_status ~ exposure + wi_centered + agechild_centered + vax_type, data = nd_obe, family = binomial)
vif(glm_model)
#there is no multicollinearity among predictors bc all VIFs (Variance Inflation Factors) are ~1

#------------------------------------
#compare findings from models w/ interaction, without interaction, and stratified with only prenatal tetanus

#2x2 table for prenatal tetanus
pretet_nd_obe %>%
  group_by(exposure, vax_status) %>%
  summarise(count = n())

#2x2 table for nd_obe
nd_obe %>%
  filter(vax_type != "vax_polio") %>% 
  group_by(exposure, vax_status) %>%
  summarise(count = n())

#run model without covariates
test <- glm(vax_status ~ exposure, 
            family = binomial(), 
            data = pretet_nd_obe)
summary(test)

test <- glm(vax_status ~ exposure * vax_type, 
            family = binomial(), 
            data = nd_obe)
summary(test)

#Compare average effect of exposure in interaction model vs. exposure coefficient in model w/out interaction
avg_slopes(model_a, variables = "exposure")

avg_slopes(model_d, variables = "exposure", by = "vax_type")
avg_slopes(model_d, variables = "exposure")

#-------------------------
#test if relationship between age and vaccination is linear - not a significant difference btw categorical and linear model
model_f1 <- glmer(vax_status ~ exposure * vax_type + wi_centered + agechild_centered +
                    (1 | country/clustid/childid),
                  data = nd_obe_ne, family = binomial, nAGQ = 1, 
                  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000)))

model_f2 <- glmer(vax_status ~ exposure * vax_type + wi_centered + cut(agechild, breaks = 10) +
                    (1 | country/clustid/childid),
                  data = nd_obe_ne, family = binomial, nAGQ = 1, 
                  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000)))

anova(model_f1, model_f2, test = "Chisq")
