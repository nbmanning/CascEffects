# Name: x_DiD_Analysis
# Purpose: Script to actually do the DiD based on the cleaned data from the previous script ().
# Created On: 9/3/26
# Last Edited: 9/3/26
# Author: Nick Manning

# # # # # # # # # # # # # # # # # # # # # # # # 

rm(list = ls())

# Load Libraries & Set Paths and Constants ------------------------------------
library(dplyr)
library(ggplot2)

# for fixed effects regression
library(modelsummary)
library(fixest)
library(causaldata)

# Set Constants ------
v_startyr <- 2007
v_endyr <- 2017


## 0.1) Load CSV from previous script -----

df_did_source <- readRDS("../Data_Derived/df_did_propalltime_mapb_filtered.rds")

# Notes:
## The logic here is:
### Group A is our Untreated Group because it has consistently and primarily domestic trade
### Group E is our Treated as it is consistently and primarily international trade 
### Pre-Treatment is 2007-2011 average 
### Post-Treatment is 2013-2017 average

## 0.2) Initial Formatting ----
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

## 0.3) Calculate the Mean then Mean -----
# Muni --Mean--> Year --MEAN--> Period
# mean per year then mean
# takes care of differences in group size between E and A
# get just the relevant area and create the DiD groups 
# mean per year per export group 
df_did_area_mean_yr <- df_did %>%
  filter(destination == "TOTAL") %>%
  group_by(group_alltime, period, year) %>%
  summarise(
    total_area = mean(soy_area, na.rm = TRUE),
    .groups = "drop"
  ) 

# calculate the mean 
df_did_area_mean_meanyr <- df_did_area_mean_yr %>%
  group_by(group_alltime, period) %>%
  summarise(
    mean_area = mean(total_area, na.rm = TRUE),
    .groups = "drop"
  ) 

## 0.4) EDA Plotting with Function --------
plot_data <- function(
    x_df,
    x_y,
    x_fun,
    x_log10
) {
  
  x_fun_name <- deparse(substitute(x_fun))
  
  p <- x_df %>%
    filter(
      destination == "TOTAL",
      group_alltime %in% c("A", "E")
    ) %>%
    ggplot(
      aes(
        x = factor(year),
        y = .data[[x_y]],
        fill = group_alltime
      )
    ) +
    # violin plot of data frame
    geom_violin(
      alpha = 0.3,
      width = 0.8,
      position = position_dodge(width = 0.7)
    ) +
    
    # Summary points
    stat_summary(
      aes(color = group_alltime),
      fun = x_fun,
      geom = "point",
      size = 2.5
    ) +
    
    # Summary lines
    stat_summary(
      aes(
        color = group_alltime,
        group = group_alltime
      ),
      fun = x_fun,
      geom = "line",
      linewidth = 1
    ) + 
    
    geom_vline(
      xintercept = factor(2012),
      linetype = "dashed",
      lwd = 1.5,
      color = "black") +
    
    theme_bw() +
    labs(
      title = paste0(
        "Annual ",
        x_fun_name,
        " of ",
        x_y
      ),
      x = "",
      y = x_y,
      fill = "Group",
      color = "Group"
    )+
    theme(legend.position = "bottom")
  
  # add option to plot on log10 scale 
  if (x_log10) {
    p <- p +
      scale_y_log10(labels = scales::comma)
  }
  
  return(p)
}

plot_data(
  x_df = df_did,
  x_y = "soy_area",
  #x_y = "ha_trans_mapb",
  x_fun = mean,
  x_log10 = T
)


# 1) Basic Four-Mean DiD -------

## 1.1) Example -----
# Example from DiD Causality Video from Dr. HK: https://youtu.be/8RQWEykGAjM?si=35fj5DKrYmMAI-Wj&t=375
# library(tidyverse)
# set.seed(101)
# 
# # Create our data
# ex_diddata <- tibble(year = sample(2002:2010, 10000, replace = T),
#                      group = sample(c('TreatedGroup', 'UntreatedGroup'), 10000, replace = T)) %>% 
#   mutate(after = (year >= v_startyr)) %>% 
#   # only let the treatment be applied to the treated group
#   mutate(D = after*(group == "TreatedGroup")) %>% 
#   mutate(Y = 2*D + .5*year + (group == 'TreatedGroup') + rnorm(10000)) # 2 is the "True Effect"
# 
# # now, get before-after differences for both groups
# ex_means <- ex_diddata %>% group_by(group, after) %>% summarize(Y=mean(Y))
# 
# #before-after difference for untreated; has the time effect only 
# ex_bef.aft.untreated <- filter(ex_means, group == "UntreatedGroup", after == 1)$Y - filter(ex_means, group == "UntreatedGroup", after == 0)$Y
# 
# #before-after difference for treated; has the time AND treated effect 
# ex_bef.aft.treated <- filter(ex_means, group == "TreatedGroup", after == 1)$Y - filter(ex_means, group == "TreatedGroup", after == 0)$Y
# 
# #Difference-in-Difference! Take the Time+Treated effect and remove the time effect 
# DID <- ex_bef.aft.treated - ex_bef.aft.untreated
# DID

## 1.2.1) Analysis using Real Data & Manual --------

# manual mean-mean for soy area for comparison
df_did_area_mean_yr <- df_did %>%
  filter(destination == "TOTAL") %>%
  group_by(group_alltime, period, year) %>%
  summarise(
    total_area = mean(soy_area, na.rm = TRUE),
    .groups = "drop"
  ) 

# calculate the mean 
df_did_area_mean_meanyr <- df_did_area_mean_yr %>%
  group_by(group_alltime, period) %>%
  summarise(
    mean_area = mean(total_area, na.rm = TRUE),
    .groups = "drop"
  )
# now, get before-after differences for both groups
did1_means <- df_did_area_mean_meanyr %>% 
  filter(period != "2012") %>% 
  mutate(group = ifelse(group_alltime == "A", "UntreatedGroup", "TreatedGroup"),
         after = ifelse(period == "pre_2012", F, T))

#before-after difference for untreated; has the time effect only 
did1_bef.aft.untreated <- 
  filter(did1_means, group == "UntreatedGroup", after == 1)$mean_area - 
  filter(did1_means, group == "UntreatedGroup", after == 0)$mean_area

#before-after difference for treated; has the time AND treated effect 
did1_bef.aft.treated <- 
  filter(did1_means, group == "TreatedGroup", after == 1)$mean_area - 
  filter(did1_means, group == "TreatedGroup", after == 0)$mean_area

#Difference-in-Difference! Take the Time+Treated effect and remove the time effect 
DID1 <- did1_bef.aft.treated - did1_bef.aft.untreated
DID1

## 1.2.2) Analysis using Real Data & Functions to test many variables --------

# function to clean to mean-mean format 
clean_did_4mean <- function(df, var, fun1 = mean, fun2 = mean) {
  
  # Summary by year
  df_mean_yr <- df %>%
    filter(destination == "TOTAL") %>%
    group_by(group_alltime, period, year) %>%
    summarise(
      value = fun1({{ var }}, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Mean/sum/etc. across years
  df_mean_mean_yr <- df_mean_yr %>%
    group_by(group_alltime, period) %>%
    summarise(
      value = fun2(value, na.rm = TRUE),
      .groups = "drop"
    )
  
  return(df_mean_mean_yr)
}

did1_df <- clean_did_4mean(df = df_did, var = soy_area)

# Function to calculate DiD from Cleaned Mean-Mean df
calc_did_4mean <- function(df, var) {
  
  did1_means <- df %>%
    filter(period != "2012") %>%
    mutate(
      group = ifelse(group_alltime == "A",
                     "UntreatedGroup",
                     "TreatedGroup"),
      after = ifelse(period == "pre_2012", FALSE, TRUE)
    )
  
  # before-after difference for untreated
  did1_bef.aft.untreated <-
    did1_means %>%
    filter(group == "UntreatedGroup", after) %>%
    pull({{ var }}) -
    did1_means %>%
    filter(group == "UntreatedGroup", !after) %>%
    pull({{ var }})
  
  # before-after difference for treated
  did1_bef.aft.treated <-
    did1_means %>%
    filter(group == "TreatedGroup", after) %>%
    pull({{ var }}) -
    did1_means %>%
    filter(group == "TreatedGroup", !after) %>%
    pull({{ var }})
  
  # Difference-in-Difference
  DID1 <- did1_bef.aft.treated - did1_bef.aft.untreated
  
  cat("DiD:", DID1, "\n")
  
  return(DID1)
}

did1_did <- calc_did_4mean(test, value) 


## 1.3) Plot Four-Mean DiD using function -------
plot_did1_summary <- function(df, var, fun) {
  
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
  
  # GET & PLOT COUNTERFACTUAL
  
  # Get the values needed
  a_pre <- plot_df %>%
    filter(group_alltime == "A",
           period == "pre_2012") %>%
    pull(value)
  
  a_post <- plot_df %>%
    filter(group_alltime == "A",
           period == "post_2012") %>%
    pull(value)
  
  e_pre <- plot_df %>%
    filter(group_alltime == "E",
           period == "pre_2012") %>%
    pull(value)
  
  e_post <- plot_df %>%
    filter(group_alltime == "E",
           period == "post_2012") %>%
    pull(value)
  
  # Apply A's change to E's starting value
  e_counterfactual_post <- e_pre + (a_post - a_pre)
  
  # Create data frame for plotting
  df_counterfactual <- tibble(
    period = factor(
      c("pre_2012", "post_2012"),
      levels = c("pre_2012", "post_2012")
    ),
    value = c(
      e_pre,
      e_counterfactual_post
    ),
    group_alltime = "E Counterfactual"
  )
  
  # append to plotting data 
  plot_df_cf <- bind_rows(
    plot_df,
    df_counterfactual
  )
  
  # plot 
  ggplot(
    plot_df_cf,
    aes(
      x = period,
      y = value,
      group = group_alltime
    )
  ) +
    geom_line(
      aes(
        color = group_alltime,
        linetype = group_alltime
      ),
      linewidth = 1
    ) +
    geom_point(
      aes(color = group_alltime),
      size = 3
    ) +
    scale_color_manual(
      values = c(
        #"A" = colors_groups[["A"]],
        #"E" = colors_groups[["E"]],
        "A" = "maroon",
        "E" = "yellow",
        "E Counterfactual" = "black"
      )
    ) +
    scale_linetype_manual(
      values = c(
        "A" = "solid",
        "E" = "solid",
        "E Counterfactual" = "dashed"
      ))+
    guides(
      linetype = "none"   # removes second legend
    ) +
    labs(
      x = NULL,
      y = paste0(fun_name, " ", var),
      color = "Export Group",
      title = paste0("DiD for ", fun_name, " ", var, " by Group")
    ) +
    theme_light()
}


# Call Function
plot_did1_summary(
  df = df_did,
  var = "soy_area",
  fun = mean
)

# 2) DiD with Fixed Effects Regression ------
# SOURCE: The Effect (2018), Nick Huntington-Klein
# Link: https://theeffectbook.net/ch-DifferenceinDifference.html#two-way-fixed-effects
# library(tidyverse)

## 2.1) Example ------
# # Treatment Variable 
# od <- causaldata::organ_donations
# 
# od <- od %>% 
#   mutate(
#     Treated = State == "California" &
#       Quarter %in% c('Q32011', 'Q42011', 'Q12012'))
# 
# # cluster using vcov = ~clustervariable 
# clfe <- feols(Rate ~ Treated | State + Quarter,
#               data = od, vcov = ~State)
# 
# msummary(clfe, stars = c('*' = 0.1, '**' = 0.05, '***' = 0.01))

## 2.2) My Analysis with Municipalities ------
# Treatment variable
did2_area_allmuni <- df_did %>% 
  filter(destination == "TOTAL") %>% 
  mutate(
    # Set 'treated' flag for Group E & year > 2012
    Treated = group_alltime == "E" & period == "post_2012"
  )

#### clustering on period #######
# regression with clustering on 'vcov' 
did2_clfe_clustperiod <- feols(soy_area ~ Treated | group_alltime + period,
                           data = did2_area_allmuni, vcov = ~period)

### no clustering  #######
did2_clfe_noclust <- feols(
  soy_area ~ Treated | group_alltime + period,
  data = did2_area_allmuni
)

### clustering on municipality #######
did2_clfe_clustmuni <- feols(
  soy_area ~ Treated | group_alltime + period,
  data = did2_area_allmuni,
  vcov = ~muni_id
)

### summary #######
did2_models <- list(
  "No clustering" = did2_clfe_noclust,
  "Cluster: Period" = did2_clfe_clustperiod,
  "Cluster: Municipality" = did2_clfe_clustmuni
)

modelsummary(
  did2_models,
  stars = c('*' = .1, '**' = .05, '***' = .01)
)



# 3) Dynamic DiD ------
## 3.1) Example -----
# Example Link https://bcallaway11.github.io/did/articles/did-basics.html#examples-with-simulated-data
library(did) # manually type step-by-step!


# XX) Other Tests --------
## TWFE with annual data -----
# NOTE: not using mean-mean here, just mean (i.e. annual data at the muni-year level)
area_mean_yr <- df_did_area_mean_yr %>% 
  filter(period != "2012") %>% 
  mutate(
    Treated = group_alltime == "E" &
      period == "post_2012"
    # year > 2012
  )

clfe_area_mean_yr <- feols(total_area ~ Treated | group_alltime + period,
                           data = area_mean_yr, vcov = ~period)

msummary(clfe_area_mean_yr, stars = c('*' = 0.1, '**' = 0.05, '***' = 0.01))

## Basic linear model with interaction terms -----
# run basic linear model with interaction terms 
lm_area_mean_yr <- lm(total_area ~ group_alltime + period + Treated, data = area_mean_yr)
summary(lm_area_mean_yr)

# run linear model with indicator variables
area_mean_yr_ind <- area_mean_yr %>% 
  mutate(Ind_TreatmentGroup = if_else(group_alltime == "E", 1, 0)) %>% 
  mutate(Ind_Period = if_else(period == "post_2012", 1, 0))

lm_area_mean_yr_ind <- lm(total_area ~ Ind_TreatmentGroup + Ind_Period + Ind_TreatmentGroup*Ind_Period, data = area_mean_yr_ind)
summary(lm_area_mean_yr_ind)

msummary(lm_area_mean_yr_ind, stars = c('*' = 0.1, '**' = 0.05, '***' = 0.01))
### ^ significant treatment (**), only n=20 though 


# run linear model on mean year with indicator variables
area_allmuni_ind <- area_allmuni %>% 
  mutate(Ind_TreatmentGroup = if_else(group_alltime == "E", 1, 0)) %>% 
  mutate(Ind_Period = if_else(period == "post_2012", 1, 0))

lm_area_allmuni_ind <- lm(soy_area ~ Ind_TreatmentGroup + Ind_Period + Ind_TreatmentGroup*Ind_Period, data = area_allmuni_ind)
summary(lm_area_allmuni_ind)

msummary(lm_area_allmuni_ind, stars = c('*' = 0.1, '**' = 0.05, '***' = 0.01))

# run basic linear model with interaction terms 
lm_area_mean <- lm(soy_area ~ group_alltime + period + Treated, data = area_allmuni)
summary(lm_area_mean)

## XX) Functions to clean any variable to Mean-Mean format

# X: Function for DiD Basic -----

clean_did_4mean <- function(df, var, fun){
  
  # mean per year per export group 
  df_mean_yr <- df %>%
    filter(destination == "TOTAL") %>%
    group_by(group_alltime, period, year) %>%
    summarize(
      value = fun(.data[[var]], na.rm = TRUE),
      .groups = "drop"
    )
  
  # calculate the mean 
  df_mean_mean_yr <- df_mean_yr %>%
    group_by(group_alltime, period) %>%
    summarize(
      value = fun(.data[[var]], na.rm = TRUE),
      .groups = "drop"
    )
}