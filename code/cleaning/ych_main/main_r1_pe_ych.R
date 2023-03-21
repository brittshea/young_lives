# Install libraries:
library(tidyverse)
library(haven)
library(dplyr)
library(ggplot2)
library(plotly)
library(scales)
library(readxl)
library(janitor)

# Figure preferences:
knitr::opts_chunk$set(
  fig.width = 6,
  fig.asp = .6,
  out.width = "90%"
)

theme_set(theme_minimal() + theme(legend.position = "bottom"))

options(
  ggplot2.continuous.colour = "viridis",
  ggplot2.continuous.fill = "viridis"
)

scale_colour_discrete = scale_colour_viridis_d
scale_fill_discrete = scale_fill_viridis_d

#helpful

skimr::skim(main_r1_pe_ych)  

### Read in data: ###
r1_yc_per <- read_dta("r1/stata/stata13/r1_yc/peru/pechildlevel1yrold.dta") %>%
  janitor::clean_names() 

r1_comm_per <- read_dta("r1/stata/stata13/r1_comm/peru/pe_r1_community_stblsec1disasters.dta") %>%
  janitor::clean_names() 

r2_yc_per <- read_dta("r2/stata/stata13_se/r2_yc/peru/pechildlevel5yrold.dta") %>%
  janitor::clean_names() 

r2_oc_per <- read_dta("r2/stata/stata13_se/r2_oc/peru/pechildlevel12yrold.dta") %>%
  janitor::clean_names() 

r2_comm_per <- read_dta("r2/stata/stata13_se/r2_comm/peru/pe_r2_community_stblnaturaldisaster.dta") %>%
  janitor::clean_names()

r3_yc_per <- read_dta("r3/stata/stata13/r3_yc/peru/pe_yc_householdlevel.dta") %>%
  janitor::clean_names()

r3_oc_per <- read_dta("r3/stata/stata13/r3_oc/peru/pe_oc_householdlevel.dta") %>%
  janitor::clean_names()

r3_comm_per <- read_dta("r3/stata/stata13/r3_comm/peru/pe_r3_community_subnaturaldisaster.dta") %>%
  janitor::clean_names()

r5_coc_per <- read_dta("r4/stata/stata13/peru_r5/pe_oc_ch_anon/pe_r5_occh_childofchild.dta") %>%
  janitor::clean_names()

### Peru comm r1
# filter by any disaster (cyclone, drought, flood) - 14/89
r1_comm_per %>% 
  filter(anydis == 2 | anydis == 3 | anydis == 6) %>% 
  summarise(counted = n())

plot_1 = r1_comm_per %>% 
  filter(szdis != "888" & szdis != "88" & szdis != "8") %>% 
  filter(disid == 2 | disid == 3 | disid == 6)

ggplot(plot_1) + geom_point(aes(x = disid, y = szdis), color = "blue")

### Peru yc r1 
r1_yc_per %>% 
  filter(agechild >= 6) %>% 
  filter(polio == 1) %>% 
  count()

# Filter by vacc >= 12 mos
r1_yc_per %>% 
  filter(agechild >= 12) %>% 
  filter(bcg == 2 | measles == 2 | polio == 2) %>% 
  summarise(counted = n())

# Filter by vacc and badevent - climate shock >= 12 mos
r1_yc_per %>% 
  filter(agechild >= 12) %>% 
  filter(bcg == 2 | measles == 2 | polio == 2) %>% 
  filter(badevent == 1) %>% 
  filter(phychnge == 1 | hhlstck == 1 | hhcrps == 1) %>%
  summarise(counted = n())

# Filter by vacc and badevent - moved/migrated/fled or decrease in food 
r1_yc_per %>% 
  filter(agechild >= 12) %>% 
  filter(bcg == 2 | measles == 2 | polio == 2) %>% 
  filter(badevent == 1) %>% 
  filter(hhmove == 1 | hhfood == 1) %>% 
  summarise(counted = n())

# Filter by vacc & worsevent (natural disaster or crops failed)
r1_yc_per %>% 
  filter(agechild >= 12) %>% 
  filter(bcg == 2 | measles == 2 | polio == 2) %>% 
  filter(badevent == 1) %>% 
  filter(worsevnt == 1 | worsevnt == 4) %>% 
  summarise(counted = n())

### Peru comm r2
# filter by any disaster (cyclone, drought, floods, forest fire, crop failure) - 51/187
r2_comm_per %>% 
  filter(disaster == 2 | disaster == 3 | disaster == 6 | disaster == 8 | disaster == 10) %>% 
  summarise(counted = n())

### Peru yc r2
# Filter by no vacc
r2_yc_per %>% 
  filter(pehvb == 1) %>% 
  count() 

# Filter by vacc 
r2_yc_per %>% 
  filter(bcg == 0 | measles == 0 | dpt == 0 | opv == 0 | hib == 0 | pehvb == 0) %>% 
  summarise(counted = n())

# Filter by vacc and badevent - climate shock 
r2_yc_per %>% 
  filter(bcg == 0 | measles == 0 | dpt == 0 | opv == 0 | hib == 0 | pehvb == 0) %>% 
  filter(event24 == 1 | event25 == 1 | event26 == 1 | event28 == 1 | event29 == 1 | event30 == 1 | event31 == 1 | event59 == 1 | event14 == 1) %>% 
  summarise(counted = n())

# No vac and how many affected by disaster
r2_yc_per %>% 
  filter(bcg == 0 | measles == 0 | dpt == 1 | opv == 1 | hib == 0 | pehvb == 0) %>% 
  filter(wide24 == 3 | wide25 == 3 | wide26 == 3 | wide28 == 3 | wide29 == 3 | wide30 == 3 | wide31 == 3) %>% 
  summarise(counted = n())
  
# No vacc and response to too much rain or flooding
r2_yc_per %>% 
  filter(bcg == 0 | measles == 0 | dpt == 1 | opv == 1 | hib == 0 | pehvb == 0) %>% 
  filter(resp125 == 1 | resp125 == 2 | resp125 == 3) %>% 
  summarise(counted = n())

### Peru oc r2
r2_oc_per %>% 
  filter(bcg == 1) %>% 
  count() 

### Peru comm r3
# filter by any disaster (cyclone, drought, avalanche, floods)
r3_comm_per %>% 
  filter(disaster == 2 | disaster == 3 | disaster == 4 | disaster == 6 | disaster == 8 | disaster == 10) %>% 
  summarise(counted = n())

### Peru yl r3
# Filter by vacc 
r3_yc_per %>% 
  filter(rqjnr307 == 2 | rqjnr307 == 3 | rqjnr307 == 4) %>% 
  count()
  
### Peru oc r3
# Filter by vacc 
r3_oc_per %>% 
  filter(rqjnr307 == 2 | rqjnr307 == 3 | rqjnr307 == 4) %>% 
  count() 

### Peru coc r5
r5_coc_per %>% 
  filter(agemntr5 >= 6) %>% 
  filter(vacpolr5 == 1) %>% 
  summarise(counted = n())



### CONSTRUCTED DATA ###
# Read in data:
eth_yl <- read_dta("constructed/stata/ethiopia_constructed.dta") %>%
  janitor::clean_names() 

# Plot time series for measles and date of interview
eth_yl %>%
  group_by(measles, dint, round) %>% 
  filter(measles > 0) %>% 
  summarise(counted = n()) %>% 
  ggplot(aes(x = dint, y = counted)) + 
  geom_line() 

# Plot bcg versus round
eth_yl %>%
  plot_ly(
    x = ~round, y = ~bcg,
    type = "bar")

# glm
eth_fit_logistic = 
  eth_yl %>% 
  glm(measles ~ agemon + shenv9, data = ., family = binomial())

eth_fit_logistic %>% 
  broom::tidy() %>% 
  mutate(OR = exp(estimate)) %>%
  select(term, log_OR = estimate, OR, p.value) %>% 
  knitr::kable(digits = 3)
