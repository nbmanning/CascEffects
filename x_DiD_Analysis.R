# Name: x_DiD_Analysis
# Purpose: Script to actually do the DiD based on the cleaned data from the previous script ().
# Created On: 9/3/26
# Last Edited: 9/3/26
# Author: Nick Manning

# # # # # # # # # # # # # # # # # # # # # # # # 

# rm(list = ls())

# Load Libraries & Set Paths and Constants ------------------------------------
library(dplyr)

# Set Constants ------
v_startyr <- 2007
v_endyr <- 2017

## 0.1) Load CSV from previous script 
df_did <- read.csv('../Data_Derived/df_did_propalltime.csv')
df_did_source <- readRDS("../Data_Derived/df_did_propalltime_mapb_filtered.rds")
# Notes:
## The logic here is:
### Group A is our Untreated Group because it has consistently mainly domestic trade
### Group E is our Treated as it is consistently mainly international trade 
### Pre-Treatment is 2007-2011 average 
### Post-Treatment is 2013-2017 average

# Get groups in DiD format
df_did <- df_did_source %>%
  filter(group_alltime %in% c("A", "E")) %>%   # ignore NAs
  mutate(
    period = case_when(
      year >= v_startyr & year <= 2011 ~ "pre_2012",
      year == 2012 ~ "2012",
      year >= 2013 & year <= v_endyr ~ "post_2012"
    )
  )  #%>% filter(period != "2012") 

# get count of each group (pre-2012, 2012, 2012)  
summary_count_did <- df_did %>%
  filter(destination == "TOTAL") %>% 
  count(group_alltime, period) %>%
  tidyr::pivot_wider(
    names_from = period,
    values_from = n,
    values_fill = 0
  )

# 1) Basic Four-Mean DiD -------

## 1.1) Example -----
# Example from DiD Causality Video from Dr. HK: https://youtu.be/8RQWEykGAjM?si=35fj5DKrYmMAI-Wj&t=375
# library(tidyverse)
set.seed(101)

# Create our data
ex_diddata <- tibble(year = sample(2002:2010, 10000, replace = T),
                     group = sample(c('TreatedGroup', 'UntreatedGroup'), 10000, replace = T)) %>% 
  mutate(after = (year >= v_startyr)) %>% 
  # only let the treatment be applied to the treated group
  mutate(D = after*(group == "TreatedGroup")) %>% 
  mutate(Y = 2*D + .5*year + (group == 'TreatedGroup') + rnorm(10000)) # 2 is the "True Effect"

# now, get before-after differences for both groups
ex_means <- ex_diddata %>% group_by(group, after) %>% summarize(Y=mean(Y))

#before-after difference for untreated; has the time effect only 
ex_bef.aft.untreated <- filter(ex_means, group == "UntreatedGroup", after == 1)$Y - filter(ex_means, group == "UntreatedGroup", after == 0)$Y

#before-after difference for treated; has the time AND treated effect 
ex_bef.aft.treated <- filter(ex_means, group == "TreatedGroup", after == 1)$Y - filter(ex_means, group == "TreatedGroup", after == 0)$Y

#Difference-in-Difference! Take the Time+Treated effect and remove the time effect 
DID <- ex_bef.aft.treated - ex_bef.aft.untreated
DID

## 1.2) Real Attempt using Our Data --------
# now, get before-after differences for both groups
ex_means <- ex_diddata %>% group_by(group, after) %>% summarize(Y=mean(Y))

ex_means2 <- df_did_area_sumyr_mean %>% 
  mutate(group = ifelse(group_alltime == "A", "UntreatedGroup", "TreatedGroup"),
         after = ifelse(period == "pre_2012", F, T))

#before-after difference for untreated; has the time effect only 
ex_bef.aft.untreated <- filter(ex_means2, group == "UntreatedGroup", after == 1)$mean_area - filter(ex_means2, group == "UntreatedGroup", after == 0)$mean_area

#before-after difference for treated; has the time AND treated effect 
ex_bef.aft.treated <- filter(ex_means2, group == "TreatedGroup", after == 1)$mean_area - filter(ex_means2, group == "TreatedGroup", after == 0)$mean_area

#Difference-in-Difference! Take the Time+Treated effect and remove the time effect 
DID <- ex_bef.aft.treated - ex_bef.aft.untreated
DID


## 1.3) Plot Four-Mean DiD using function -------
## 3.2.0) Basic DiD Plots with function ----
plot_period_summary <- function(df, var, fun) {
  
  # Get names for labels
  df_name <- deparse(substitute(df))
  fun_name <- deparse(substitute(fun))
  
  # Summarize data
  plot_df <- df %>%
    filter(
      destination == "TOTAL",
      group_alltime %in% c("A", "E")
    ) %>%
    group_by(group_alltime, period) %>%
    summarize(
      value = fun(.data[[var]], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    filter(period != "2012") %>%
    mutate(
      period = factor(
        period,
        levels = c("pre_2012", "2012", "post_2012")
      )
    )
  
  plot_df
  
  # Plot
  ggplot(
    plot_df,
    aes(
      x = period,
      y = value,
      color = group_alltime,
      group = group_alltime
    )
  ) +
    geom_line(linewidth = 1) +
    geom_point(size = 3) +
    labs(
      x = NULL,
      y = paste0(
        stringr::str_to_title(fun_name),
        " ",
        var
      ),
      color = "Group",
      title = paste0(
        df_name,
        ": ",
        stringr::str_to_title(fun_name),
        " ",
        var,
        " by Group (Pre/Post 2012)"
      )
    ) +
    theme_minimal()+
    theme(legend.position = "none")
}

plot_period_summary(
  df_did,
  "soy_area",
  mean
)
