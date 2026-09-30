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
var_DiD <- "soy_area" 

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
(summary_count_did <- df_did %>%
  filter(destination == "TOTAL") %>% 
  count(group_alltime, period) %>%
  tidyr::pivot_wider(
    names_from = period,
    values_from = n,
    values_fill = 0
  ))

## 0.3) Calculate the Mean then Mean -----
# Muni --Mean--> Year --MEAN--> Period
# mean per year then mean
# takes care of differences in group size between E and A
# get just the relevant area and create the DiD groups 
# mean per year per export group 
# df_did_area_mean_yr <- df_did %>%
#   filter(destination == "TOTAL") %>%
#   group_by(group_alltime, period, year) %>%
#   summarise(
#     total_area = mean(soy_area, na.rm = TRUE),
#     .groups = "drop"
#   ) 
# 
# # calculate the mean 
# df_did_area_mean_meanyr <- df_did_area_mean_yr %>%
#   group_by(group_alltime, period) %>%
#   summarise(
#     mean_area = mean(total_area, na.rm = TRUE),
#     .groups = "drop"
#   ) 

## 0.4) EDA Plotting with Function --------

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

# ## 1.2.1) Analysis using Real Data & Manual --------
# 
# # manual mean-mean for soy area for comparison
# df_did_area_mean_yr <- df_did %>%
#   filter(destination == "TOTAL") %>%
#   group_by(group_alltime, period, year) %>%
#   summarise(
#     total_area = mean(soy_area, na.rm = TRUE),
#     .groups = "drop"
#   ) 
# 
# # calculate the mean 
# df_did_area_mean_meanyr <- df_did_area_mean_yr %>%
#   group_by(group_alltime, period) %>%
#   summarise(
#     mean_area = mean(total_area, na.rm = TRUE),
#     .groups = "drop"
#   )
# # now, get before-after differences for both groups
# did1_means <- df_did_area_mean_meanyr %>% 
#   filter(period != "2012") %>% 
#   mutate(group = ifelse(group_alltime == "A", "UntreatedGroup", "TreatedGroup"),
#          after = ifelse(period == "pre_2012", F, T))
# 
# #before-after difference for untreated; has the time effect only 
# did1_bef.aft.untreated <- 
#   filter(did1_means, group == "UntreatedGroup", after == 1)$mean_area - 
#   filter(did1_means, group == "UntreatedGroup", after == 0)$mean_area
# 
# #before-after difference for treated; has the time AND treated effect 
# did1_bef.aft.treated <- 
#   filter(did1_means, group == "TreatedGroup", after == 1)$mean_area - 
#   filter(did1_means, group == "TreatedGroup", after == 0)$mean_area
# 
# #Difference-in-Difference! Take the Time+Treated effect and remove the time effect 
# DID1_manual <- did1_bef.aft.treated - did1_bef.aft.untreated
# DID1_manual

## 1.2.2) Analysis using Real Data & Functions to test many variables --------

# function to clean to mean-mean format 
clean_did_4mean <- function(df, var, fun_muni_to_year = mean, fun_year_to_period = mean) {
  
  # Summary by year
  df_mean_yr <- df %>%
    filter(destination == "TOTAL") %>%
    group_by(group_alltime, period, year) %>%
    summarise(
      value = fun_muni_to_year(.data[[var]], na.rm = TRUE),
      .groups = "drop"
    )
  
  # Mean/sum/etc. across years
  df_mean_mean_yr <- df_mean_yr %>%
    group_by(group_alltime, period) %>%
    summarise(
      value = fun_year_to_period(value, na.rm = TRUE),
      .groups = "drop"
    )
  
  return(df_mean_mean_yr)
}

did1_df <- clean_did_4mean(
  df = df_did, 
  var = var_DiD,
  fun_muni_to_year = mean,
  fun_year_to_period = mean
  )

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

did1_did <- calc_did_4mean(did1_df, value) 


## 1.3) Plot Four-Mean DiD using function -------

### DEFINE COLORS HERE ###
colors_groups <- c(
  "E" = "firebrick4",
  "A" = "goldenrod"
)

plot_annual_summary <- function(df, var, fun = mean) {
  
  # Get function name for labels
  fun_name <- deparse(substitute(fun))
  
  # Summarize data
  plot_df <- df %>%
    filter(group_alltime %in% c("A", "E")) %>% 
    group_by(year, group_alltime) %>%
    summarize(
      value = fun(.data[[var]], na.rm = TRUE),
      .groups = "drop"
    )
  
  # Create plot
  ggplot(
    plot_df,
    aes(
      x = year,
      y = value,
      group = group_alltime,
      color = group_alltime
    )
  ) +
    geom_line() +
    geom_point(size = 3) +
    geom_vline(xintercept = 2012) +
    scale_x_continuous(
      breaks = seq(v_startyr, v_endyr, by = 1)
    ) +
    labs(
      title = paste0(
        "Annual ", str_to_title(fun_name),
        " of ", var,
        " by Group"
      ),
      x = "Year",
      y = paste0(str_to_title(fun_name), " ", var),
      color = "Group"
    )+
    theme(legend.position = "bottom")+
    scale_color_manual(
      values = c(
        "A" = colors_groups[["A"]],
        "E" = colors_groups[["E"]]
      ),
      limits = c("E", "A"))
}

# test fxn 
plot_annual_summary(
  df = df_did,
  var = var_DiD,
  fun = mean
)



# NEW: with line plot
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
        fill = group_alltime,
        color = group_alltime
      )
    ) +
    geom_violin(
      alpha = 0.3,
      width = 0.8,
      position = position_dodge(width = 0.7)
    ) +
    
    # Summary points
    stat_summary(,
      fun = x_fun,
      geom = "point",
      size = 2.5
    ) +
    
    # Summary lines
    stat_summary(
      aes(
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
    theme(legend.position = "bottom")+
    scale_color_manual(
      values = colors_groups,
      limits = c("E", "A")
    ) +
    scale_fill_manual(
      values = colors_groups,
      limits = c("E", "A")
    )
  
  
  if (x_log10) {
    p <- p +
      scale_y_log10(labels = scales::comma)
  }
  
  return(p)
}

plot_data(
  x_df = df_did,
  x_y = var_DiD,
  x_fun = mean,
  x_log10 = T
)


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
        "A" = colors_groups[["A"]],
        "E" = colors_groups[["E"]],
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
  var = var_DiD,
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
# PICK UP HERE! #######


# add indicators for treatment group and post-treatment
did2_area_allmuni <- did2_area_allmuni %>%
  mutate(
    treat = if_else(group_alltime == "E", 1, 0),
    post  = if_else(period == "post_2012", 1, 0))
# did2_1_clfe_clustperiod <- feols(
#   as.formula(
#     paste0(outcome_var, " ~ Treated | group_alltime + period")
#   )soy_area ~ Treated | group_alltime + period,
#                            data = did2_area_allmuni, vcov = ~period)
# linear model with interaction term
m_did2_1_lmNoClust <- lm(
  as.formula(paste0(
    
    var_DiD, " ~ treat:post + treat + post"  
  
    )),
  data = did2_area_allmuni)

# model with SE clustered on municipality
m_did2_1_ClustMuni <- feols(
  as.formula(paste0(
    
    var_DiD, "~ treat * post"
    
    )),
  data = did2_area_allmuni,
  vcov = ~ muni_id)

# summary
modelsummary(
  list(
    "Linear FE Model" = m_did2_1_lmNoClust,
    "FE with SE clustered on Muni" = m_did2_1_ClustMuni),
  stars = c('*' = .1, '**' = .05, '***' = .01),
  notes = paste0("Dependent Var. = ", var_DiD))


# regression with clustering on 'vcov' 
# did2_1_clfe_clustperiod <- feols(
#   as.formula(
#     paste0(outcome_var, " ~ Treated | group_alltime + period")
#   )soy_area ~ Treated | group_alltime + period,
#                            data = did2_area_allmuni, vcov = ~period)
did2_1_clfe_clustperiod <- feols(
  soy_area ~ Treated | group_alltime + period,
  data = did2_area_allmuni,
  vcov = ~ period)


### no clustering  #######
did2_1_clfe_noclust <- feols(
  soy_area ~ Treated | group_alltime + period,
  data = did2_area_allmuni
)

### clustering on municipality #######
did2_1_clfe_clustmuni <- feols(
  soy_area ~ Treated | group_alltime + period,
  data = did2_area_allmuni,
  vcov = ~muni_id
)

### summary #######
did2_1_models <- list(
  "No clustering" = did2_1_clfe_noclust,
  "Cluster: Period" = did2_1_clfe_clustperiod,
  "Cluster: Municipality" = did2_1_clfe_clustmuni
)

modelsummary(
  did2_1_models,
  stars = c('*' = .1, '**' = .05, '***' = .01)
)

## Testing with explicit textbook regression
did_2_1_textbook <- feols(
    soy_area ~
      group_alltime +
      period +
      group_alltime:period,
    data = did2_area_allmuni
  )

did_2_1_textbook_lm <- lm(
  soy_area ~
    group_alltime +
    period +
    group_alltime:period,
  data = did2_area_allmuni
)

did_2_1_textbook_clust_muni <- feols(
  soy_area ~
    group_alltime +
    period +
    group_alltime:period,
  data = did2_area_allmuni,
  vcov = ~muni_id
)

modelsummary(
  list(
    "FEOLS" = did_2_1_textbook, 
    "LM" = did_2_1_textbook_lm,
    "FEOLS Clustered Muni" = did_2_1_textbook_clust_muni),
  stars = c('*' = .1, '**' = .05, '***' = .01)
)

### Testing why there are differences between models


## 2.3) My Analysis with Municipalities & Land Conversion ------
# Treatment variable

#### clustering on period #######
# regression with clustering on 'vcov' 
did2_2_clfe_clustperiod <- feols(soy_area ~ Treated | group_alltime + period,
                               data = did2_area_allmuni, vcov = ~period)

### no clustering  #######
did2_2_clfe_noclust <- feols(
  soy_area ~ Treated | group_alltime + period,
  data = did2_area_allmuni
)

### clustering on municipality #######
did2_2_clfe_clustmuni <- feols(
  soy_area ~ Treated | group_alltime + period,
  data = did2_area_allmuni,
  vcov = ~muni_id
)

### summary #######
did2_models <- list(
  "No clustering" = did2_2_clfe_noclust,
  "Cluster: Period" = did2_2_clfe_clustperiod,
  "Cluster: Municipality" = did2_2_clfe_clustmuni
)

modelsummary(
  did2_models,
  stars = c('*' = .1, '**' = .05, '***' = .01)
)

### why not the same?? ###

did2_area_allmuni <- did2_area_allmuni %>%
  mutate(
    treat = if_else(group_alltime == "E", 1, 0),
    post  = if_else(period == "post_2012", 1, 0)
  )

m1 <- feols(
  soy_area ~ treat * post,
  data = did2_area_allmuni,
  vcov = ~ muni_id
)

# m2 <- feols(
#   soy_area ~ I(treat * post) |
#     treat + post,
#   data = did2_area_allmuni,
#   vcov = ~ muni_id
# )

# add indicators for treatment group and post-treatment
did2_area_allmuni <- did2_area_allmuni %>%
  mutate(
    treat = if_else(group_alltime == "E", 1, 0),
    post  = if_else(period == "post_2012", 1, 0))

# linear model with interaction term
m1_NoClust <- lm(
  soy_area ~ treat:post + treat + post,
  data = did2_area_allmuni)

# model with SE clustered on municipality
m1_ClustMuni <- feols(
  soy_area ~ treat * post,
  data = did2_area_allmuni,
  vcov = ~ muni_id)

# summary
modelsummary(
  list(
    "m1" = m1_NoClust, 
    "m1 Clustered SE on Muni" = m1_ClustMuni),
  stars = c('*' = .1, '**' = .05, '***' = .01))


# 3) Dynamic DiD ------

# NOTE: this assumaes no-anticipation and parallel trends!

# ## 3.1) Example -----
# # Example Link https://bcallaway11.github.io/did/articles/did-basics.html#examples-with-simulated-data
# library(did) # manually type step-by-step!
# 
# # set seed for reproducibility
# set.seed(1814)
# 
# # generate dataset with 4 time periods
# sp <- reset.sim()
# 
# sp$te <- 0
# time.periods <- 4
# 
# # add dynamics effects
# sp$te.e <- 1:time.periods
# 
# # generate data with these parameters
# # here, we dropped all units who are treated in time period 1 as they do not help us recover ATT(g,t)'s.
# dta <- build_sim_dataset(sp)
# 
# # how many observations remained after dropping 'Always Treated'?
# nrow(dta)
# head(dta)
# 
# # estimate group-time treatment effects with 'att_gt'
# att_gt_example <- att_gt(
#   yname = "Y",
#   tname = "period",
#   idname = "id",
#   gname = "G",
#   xformla = ~X,
#   data = dta
# )
# 
# # get summary
# summary(att_gt_example)
# 
# # plot results
# ggdid(att_gt_example)
# 
# ### Dynamic Effects & Event Studies (test this with real data!) ###
# agg.es <- aggte(att_gt_example, type = "dynamic")
# summary(agg.es)
# 
# ggdid(agg.es)
# 
# # ^ NOTES from webpage:
# 
# # In this figure, the x-axis is the length of exposure to the treatment. Length of exposure equal to 0 provides the average effect of participating in the treatment across groups in the time period when they first participate in the treatment (instantaneous treatment effect). Length of exposure equal to -1 corresponds to the time period before groups first participate in the treatment, and length of exposure equal to 1 corresponds to the first time period after initial exposure to the treatment.
# 
# # As we would expect based on the data that we generated, it looks like parallel trends holds in pre-treatment periods and the effect of participating in the treatment is increasing with length of exposure of the treatment.
# 
# # The Overall ATT here averages the average treatment effects across all lengths of exposure to the treatment.
# 
# ## 3.1.1) Example with Real Data --------
# data("mpdta")
# 
# head(mpdta)
# 
# # estimate group-time avaerage treatment effect w/o covariates
# attgt.mw <- att_gt(
#   yname = "lemp",
#   gname = "first.treat",
#   idname = "countyreal",
#   tname = "year",
#   xformla = ~1,
#   data = mpdta
# )
# 
# # get summary
# summary (attgt.mw)
# 
# ## aggregate the group-time average treatment effects (this makes the most sense for my case!)
# attgt.mw.dyn <- aggte(attgt.mw, type = "dynamic")
# summary(attgt.mw.dyn)
# ggdid(attgt.mw.dyn)
# #, ylim = c(-.3, .3)

## 3.2) Analysis with My Data ---------

### 3.2.0) Clean data to get in the same format as 'mpdta' -----
# names(mpdta)
# year = year
# countyreal = muni_id
# lpop = potential covariate
# lemp = var of interest, probably 'soy_area'
# first.treat = 2012 for all
# treat = 1 in Group E and >= 2012 (2012 will be 0)

names(df_did)

# get columns 
# test_attgt <- df_did %>%
#   filter(destination == "TOTAL") %>% 
#   select(year, muni_id, soy_area, group_alltime, period) %>% 
#   mutate(
#     treat = case_when(group_alltime == "E" & period %in% c("post_2012", "2012") ~ 1, .default = 0),
#     first.treat = case_when(treat == 1 ~ 2012, .default = 0),
#     groupname_alltime = if_else(group_alltime == "E", 2012, 0)
#   ) #%>% 
#   #filter(year > 2012)

test_df_attgt <- df_did %>%
  filter(destination == "TOTAL") %>%
  #select(year, muni_id, soy_area, group_alltime) %>%
  mutate(
    first.treat = if_else(group_alltime == "E", 2012, 0)
  )

# estimate group-time average treatment effects without covariates
test_attgt <- att_gt(
  #yname = "soy_area",
  yname = var_DiD,
  gname = "first.treat",
  #gname = "group_alltime",
  #gname = "groupname_alltime",
  idname = "muni_id",
  tname = "year",
  xformla = ~1, # no covariates
  data = test_df_attgt
)


# get summary
summary(test_attgt)

## 3.2.1) Dynamic -----
## aggregate the group-time average treatment effects (this makes the most sense for my case!)
test_attgt_dyn <- aggte(test_attgt, type = "dynamic")
summary(test_attgt_dyn)
ggdid(test_attgt_dyn)

######################################################################
# END ################################################################
######################################################################




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