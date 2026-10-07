# Name: x_DiD
# Purpose: Script to load in and clean TRASE data per municipality, calculate intl. & domestic exports, then perform a basic DiD calculation
# Created On: 7/6/26
# Last Edited: 7/6/26
# Author: Nick Manning

# # # # # # # # # # # # # # # # # # # # # # # # 

rm(list = ls())

# 0) Load Libraries & Set Paths and Constants ------------------------------------

## Libraries ##########

# cleaning ---
library(readxl)
library(dplyr)
library(purrr)
library(stringr)
library(janitor) # for clean_names()
library(tidyr) # for rolling sum

# plotting ---
library(sf)
library(geobr)
library(ggplot2)

## Paths ##########
folder_source <- "../Data_Source/"
folder_derived <- "../Data_Derived/"


## Constants ########## 

# years to filter to
v_treatment <- 2012
v_startyr <- 2007
v_endyr <- 2017

# variables to sum across
vars_sum <- c(
  "def_exp",
  "em_net_def_exp",
  "em_gross_def_exp",
  "trade_volume",
  "trade_value",
  "soy_area"
)

# Description of filters
desc_DiD <- 
  "Filters: \n
BOTH Groups A & E: 
3/5 Years Pre/Post; 
SD < 1Q; 
Intl Trade <0.2 (A) >0.8 (E) \n

ONLY Group E: 
Mean Intl Trade >1Q; 
Pre-Shock Big 6 Trade >0.5"


# 1) Load TRASE & Clean --------------------

## 1.0) Initial Clean -----------
# Path to Excel file
file <- paste0(folder_source, "brazil_soy_v2_6_1_composite.xlsx")

# Get all sheet names
sheets <- excel_sheets(file)

# Keep only sheets named "Year YYYY"
year_sheets <- sheets[str_detect(sheets, "^Year\\s\\d{4}$")]

# Read and combine all year sheets
soy_df_source <- map_dfr(
  year_sheets,
  ~ read_excel(file, sheet = .x)
)

# Check result
glimpse(soy_df_source)
head(soy_df_source)

# Clean soy_df
soy_df <- soy_df_source %>% janitor::clean_names()

# Clean initial soy_df
soy_df <- soy_df %>% 
  
  # select relevant columns
  select(
    # basic info
    year, biome, exporter_group,
    # geographic info
    state_of_production, municipality_of_production, municipality_of_production_trase_id, country_of_first_import, 
    # main variables of interest
    trade_volume, soy_area,
    # other variables      
    trade_value, soy_deforestation_exposure, net_emissions_from_soy_deforestation_exposure, gross_emissions_from_soy_deforestation_exposure 
  ) %>% 
  
  rename(
    state = state_of_production,
    importer = country_of_first_import,
    muni = municipality_of_production,
    muni_id = municipality_of_production_trase_id,
    def_exp = soy_deforestation_exposure,
    em_net_def_exp = net_emissions_from_soy_deforestation_exposure,
    em_gross_def_exp = gross_emissions_from_soy_deforestation_exposure
  ) %>% 
  
  # Convert TRASE municipality IDs to numeric IBGE codes
  mutate(
    muni_id = as.numeric(sub("^BR-", "", muni_id))
  ) %>%
  
  # Keep only Cerrado municipalities within study years
  filter(
    muni != "UNKNOWN",
    biome == "CERRADO",
    between(year, v_startyr, v_endyr)
  )

## 1.1) Get 2011 soy from Big 6 ------
# get proportion of soy traded in 2011 from the big 6 per municipality
soy_df_2011 <- soy_df %>% filter(year == 2011)

# Exporter groups of interest
ls_big6 <- c(
  "BUNGE",
  "COFCO",
  "CARGILL",
  "LOUIS DREYFUS",
  "ADM",
  "AMAGGI",
  "AMAGGI & LD COMMODITIES"
)

# Get proportion of municipal trade volume from these exporter groups in 2007-2011
soy_df_big6 <- soy_df %>%
  # filter to the first year and one year before the shock
  filter(between(year, v_startyr, v_treatment-1)) %>%
  group_by(
    #year,
    biome,
    state,
    muni,
    muni_id
  ) %>%
  summarise(
    exp_total_pre = sum(trade_volume, na.rm = TRUE),
    exp_big6_pre = sum(trade_volume[exporter_group %in% ls_big6], na.rm = TRUE),
    exp_prop_big6_pre = exp_big6_pre / exp_total_pre,
    .groups = "drop"
  ) %>% 
  select(muni_id, exp_total_pre, exp_big6_pre, exp_prop_big6_pre)


# get one muni-year-importer value (because we didn't select the 'importer group' column)
soy_df <- soy_df %>%
  group_by(
    year,
    biome,
    state,
    muni,
    muni_id,
    importer
  ) %>%
  # NOTE: see 'constants' section for list of what the variables are
  summarise(
    across(all_of(vars_sum), sum, na.rm = TRUE),
    .groups = "drop"
  )

# PICK UP HERE -----
# Re-join
# change soy_df_sum back to soy_df if it worked -- DONE
# Add >50% as a filter criteria later on -- DONE 
# re-run analysis with this criteria and the export volume > 1Q filter --DONE 
# add result to table - maybe make a comprehensive result slide of important info? --Eh...  not reproducible but useful for my own notes 
# re-run without the export volume filter -- IN PROGRESS
# add result to table

soy_df <- soy_df %>% left_join(soy_df_big6, by = "muni_id")

## 1.1) Add BR where missing to get proportions -----------

# Create one Brazil row for municipality-years that lack one
brazil_rows <- soy_df %>%
  group_by(year, muni, muni_id) %>%
  # make sure munis with Brazil trade are excluded
  filter(!any(importer == "BRAZIL")) %>%
  # keep one row to copy identifying information
  slice(1) %>%
  ungroup() %>%
  mutate(
    importer = "BRAZIL",
    def_exp = 0,
    em_net_def_exp = 0,
    em_gross_def_exp = 0,
    trade_volume = 0,
    trade_value = 0,
    soy_area = 0
  )

# Append new rows
soy_df <- bind_rows(soy_df, brazil_rows)

# get count of rows added; should be n_brazil = 1; if there is an n_brazil = 2 row then something went wrong
soy_df %>%
  group_by(year, muni_id) %>%
  summarise(
    n_brazil = sum(importer == "BRAZIL"),
    .groups = "drop"
  ) %>%
  count(n_brazil)

## 1.2) Calculate proportion domestic per municipality ------

# Create split Domestic/International df
#### NOTE: soy_df_split here ###############
soy_df_split <- soy_df %>%
  mutate(
    destination = if_else(
      importer == "BRAZIL",
      "DOMESTIC",
      "INTERNATIONAL"
    )
  ) %>%
  group_by(
    year,
    biome,
    state,
    muni,
    muni_id,
    destination #important! also grouping by destination (i.e. Dom or Intl) here
  ) %>%
  # get sum of the variables per municipality per year 
  summarise(
    across(all_of(vars_sum), sum, na.rm = TRUE),
    .groups = "drop"
  )


# make sure each municipality also has an international row by getting all the rows with INTL and making one for them with everything set to 0
intl_rows <- soy_df_split %>%
  group_by(year, biome, state, muni, muni_id) %>%
  filter(!any(destination == "INTERNATIONAL")) %>%
  slice(1) %>% # extra line just to make sure we only have 1 row per municipality
  ungroup() %>%
  mutate(
    destination = "INTERNATIONAL",
    def_exp = 0,
    em_net_def_exp = 0,
    em_gross_def_exp = 0,
    trade_volume = 0,
    trade_value = 0,
    soy_area = 0
  )

# make sure each municipality gets the sum of DOMESTIC + INTERNATIONAL (only for those that already have an INTL row)
total_rows <- soy_df_split %>%
  # don't include destination in the group_by() so we can get the TOTAL value
  group_by(
    year,
    biome,
    state,
    muni,
    muni_id
  ) %>%
  # calculate total
  summarise(
    destination = "TOTAL",
    across(all_of(vars_sum), sum, na.rm = TRUE),
    .groups = "drop"
  )
    
# combine df with TOTAL data rows from above and append INTL data = 0 rows for those that don't have 
soy_df_split <- bind_rows(soy_df_split, intl_rows, total_rows)

# make sure this worked; should return n = 3
soy_df_split %>%
  count(year, muni_id) %>%
  count(n)

# # check to see if each municipality has each year, if not, fill with 0's. This matters for the proportion step. 
# x_missing_years <- soy_df_split %>%
#   group_by(
#     muni_id, destination) %>% 
#   summarize(
#     n_years = n_distinct(year),
#     expected = max(year) - min(year) + 1,
#     .groups = "drop"
#   ) %>%
#   filter(n_years != expected)
# 
# # check missing years to see if it has a chance of being 0
# x_missing_years <- unique(x_missing_years$muni_id)
# x_missing_soy_df_split <- soy_df_split %>% 
#   filter(muni_id %in% x_missing_years)
# VERDICT: do NOT change missing to 0, accept them as missing from TRASE 


# add missing years here so each municipality has every year from 2007-2017
soy_df_split <- soy_df_split %>%
  group_by(
    biome, state, muni, muni_id, destination
  ) %>%
  complete(
    year = v_startyr:v_endyr,
    fill = list(
      def_exp = NA_real_,
      em_net_def_exp = NA_real_,
      em_gross_def_exp = NA_real_,
      trade_volume = NA_real_,
      trade_value = NA_real_,
      soy_area = NA_real_
    )
  ) %>%
  ungroup()

# calculate proportion international
soy_df_split <- soy_df_split %>%
  
  # Municipality-year international trade proportion per year (for std. dev. later)
  group_by(year, muni_id) %>% # include muni in the group_by
  mutate(
    prop_intl_yr = {
      
      intl_vol <- sum(trade_volume[destination == "INTERNATIONAL"], na.rm = T)
      total_vol <- sum(trade_volume[destination == "TOTAL"], na.rm = T)
      
      if_else(
        total_vol > 0,
        intl_vol / total_vol,
        NA_real_
      )
    }
  ) %>%
  ungroup() %>%
  
  # Municipality international trade proportion across entire study period (for Group A or E later)
  group_by(muni_id) %>%
  mutate(
    prop_intl_alltime = {
      
      intl_vol <- sum(trade_volume[destination == "INTERNATIONAL"],
                      na.rm = T)
      total_vol <- sum(trade_volume[destination == "TOTAL"],
                       na.rm = T)
      
      if_else(
        total_vol > 0,
        intl_vol / total_vol,
        NA_real_
      )
    }
  ) %>%
  ungroup()

# Get mean pre-shock international trade to eventually get Quartile spread
mean_intl_volume <- soy_df_split %>%
  #filter(destination == "INTERNATIONAL") %>%
  filter(destination == "INTERNATIONAL" & between(year, v_startyr, v_treatment-1)) %>%
  group_by(muni_id) %>%
  summarize(
    mean_intl_volume = mean(trade_volume, na.rm = TRUE),
    .groups = "drop"
  )

# sum(is.na(as.matrix(mean_intl_volume)))

# add mean intl. trade to df
soy_df_split <- soy_df_split %>%
  left_join(mean_intl_volume, by = "muni_id")

# calculate std. dev as a substitute for trade instability - i.e. lower SD = more stable = lower trade instability
# OLD WAY 
# trade_instability <- soy_df_split %>%
#   distinct(year, muni_id, prop_intl_yr) %>% # get just one muni per year rather than having one DOMESTIC and one INTERNATIONAL destination column
#   group_by(muni_id) %>%
#   summarise(
#     trade_instability = sd(prop_intl_yr, na.rm = T),
#     .groups = "drop"
#   )

# NEW WAY with filter before std. dev. calculation
# trade_instability <- soy_df_split %>%
#   distinct(muni_id, year, prop_intl_yr) %>%
#   group_by(muni_id) %>%
#   # Check to see if >= 6 (of a possible 11) of the years are there 
#   summarize(
#     n_valid_years = sum(!is.na(prop_intl_yr)),
#     sd_prop_intl = ifelse(
#       n_valid_years >= 6,
#       sd(prop_intl_yr, na.rm = TRUE),
#       NA
#     ),
#     .groups = "drop"
#   )

# NEW NEW WAY with 3 valid years pre- and post-shock 
trade_instability <- soy_df_split %>%
  distinct(muni_id, year, prop_intl_yr) %>%
  group_by(muni_id) %>%
  # count number of NA values per muni per year 
  summarize(
    n_pre = sum(!is.na(prop_intl_yr) & year < v_treatment),
    n_post = sum(!is.na(prop_intl_yr) & year > v_treatment),
    
    # what this does is filter out years that do not have at least 3/5 valid pre- and post-years
    # NOTE: we chose 3 here, could be two! 
    sd_prop_intl = ifelse(
      n_pre >= 3 & n_post >= 3,
      sd(prop_intl_yr, na.rm = TRUE),
      NA
    ),
    .groups = "drop"
  )

# NEW NEW NEW WAY (after KF meeting): keep all but weight them differently based on the amount of data they have (not sure about this idea)
# ^to-do (maybe...)


# report the number of missing municipalities from this filter...
trade_instability %>%
  summarize(
    min_pre = min(n_pre),
    min_post = min(n_post),
    n_pass = sum(n_pre >= 3 & n_post >= 3),
    n_fail = sum(n_pre < 3 | n_post < 3)
  )

# ... and why they are missing (i.e. not enough pre- or not enough post-data)
trade_instability %>%
  filter(n_pre < 3 | n_post < 3) %>%
  count(
    pre_ok = n_pre >= 3,
    post_ok = n_post >= 3
  )

# add std. dev. to other df
soy_df_split <- soy_df_split %>%
  left_join(trade_instability, by = "muni_id")

# way to check missing - won't work if I filter beforehand  
# soy_df_split %>%
#   distinct(muni_id, year, prop_intl_yr) %>%
#   summarize(
#     total_muni_years = n(),
#     valid_props = sum(!is.na(prop_intl_yr)),
#     missing_props = sum(is.na(prop_intl_yr))
#   )

# create groups based on da Silva et al., 2023: https://doi.org/10.1038/s41598-023-38405-1
# NOTE: right now we make this grouped by each ROW independently, i.e. by each year, however, we may want to split this by MUNICIPALITY over time based on average split per- and post-shock  
# NOTE: this is TOTAL proportion, i.e. over the entire timespan

# first, plot the distributions of standard deviations
# boxplot 
ggplot(trade_instability,
       aes(y = sd_prop_intl)) +
  geom_boxplot()

# histogram
ggplot(trade_instability,
       aes(x = sd_prop_intl)) +
  geom_histogram(bins = 30)+
  labs(
    x = "St. Dev. of Intl. Trade Proportions Per Municipality",
  )

### Re-join proportion Big 6 to be used as another filter ###
soy_df_split <- soy_df_split %>% left_join(soy_df_big6, by = "muni_id")


### Set threshold here ----------

# set threshold based on 1st quartile of data ignoring SD of 0
## "We set our threshold based on the first quartile of those municipalities with any variation in their international trade (i.e. SD != 0)"

# check proportion 
quantile(
  # trade_instability$sd_prop_intl  # <-- results in 0, so we need to filter to > 0
  trade_instability$sd_prop_intl[trade_instability$sd_prop_intl > 0],
  probs = c(0.25, 0.5, 0.75),
  na.rm = TRUE)

# assign variable based on Intl. Trade Proportion Quartile (after removing the 0 intl. trade muni's)
# v_trade_instab_limit <- 0.2 # <-- 0.2 was the old arbitrary assignment 
v_trade_inst_q1 <- round(as.numeric(quantile(trade_instability$sd_prop_intl[trade_instability$sd_prop_intl > 0], 0.25, na.rm = T)), 3)

# Pick up by removing NAs to only be left with muni's in groups A or E 
# OLD FILTER
# soy_df_split <- soy_df_split %>%
#   filter(n_valid_years>=6) %>% # OLD: ONLY 6 YEARS
#   mutate(
#     group_alltime = case_when(
#       prop_intl_alltime <= 0.20 & sd_prop_intl < v_trade_inst_q1 ~ "A",
#       prop_intl_alltime >= 0.80 & sd_prop_intl < v_trade_inst_q1 ~ "E",
#       TRUE ~ NA_character_
#     )
#   )



### Calculate threshold for minimum mean international trade volume ###
# Municipality-level data needed for separately calculating the threshold for Group E assignment
df_groupE <- soy_df_split %>%
  distinct(
    muni_id,
    prop_intl_alltime,
    sd_prop_intl,
    mean_intl_volume,
    #exp_prop_big6_pre,
    n_pre,
    n_post
  ) %>%
  filter(
    n_pre >= 3,
    n_post >= 3,
    #exp_prop_big6_pre > 0.50
    prop_intl_alltime >= 0.80,
    sd_prop_intl < v_trade_inst_q1,
  )

# calculate the threshold for Group E assignment
#x_intl_volume_threshold <- 1000
x_intl_volume_threshold <- as.numeric(quantile(df_groupE$mean_intl_volume, 0.25))

# NEW filters
soy_df_split2 <- soy_df_split %>%
  filter(
    n_pre >= 3,
    n_post >= 3
  ) %>%
  mutate(
    group_alltime = case_when(
      # Group A: consistently domestic
      prop_intl_alltime <= 0.20 &
        sd_prop_intl < v_trade_inst_q1 
      ~ "A",
      
      # Group E: consistently international and sufficiently large exporter
      prop_intl_alltime >= 0.80 &
        sd_prop_intl < v_trade_inst_q1 &
        mean_intl_volume > x_intl_volume_threshold &
        exp_prop_big6_pre > 0.50
      ~ "E",
      
      TRUE ~ NA_character_
    )
  )

# test group membership with new filters 
table(soy_df_split2$group_alltime[soy_df_split2$destination=="TOTAL" & soy_df_split2$year==2013])

## 1.3) Optional sensitivity analysis for raw intl. trade volume threshold filter ###########
thresh_max <- 200000
thresh_int <- 250
thresh_ex <- x_intl_volume_threshold

# Evaluate a range of possible thresholds
df_thresholds <- tibble(
  threshold = seq(0, thresh_max, by = thresh_int)
) %>%
  rowwise() %>%
  mutate(
    n_groupE_included = sum(
      df_groupE$mean_intl_volume > threshold,
      na.rm = TRUE
    ),
    n_groupE_excluded = sum(
      df_groupE$mean_intl_volume <= threshold,
      na.rm = TRUE
    )
  ) %>%
  ungroup()

library(plotly)

p <- ggplot(
  df_thresholds,
  aes(
    x = threshold,
    y = n_groupE_included,
    text = paste0(
      "Threshold: ", scales::comma(threshold),
      "<br>Group E Included: ", n_groupE_included,
      "<br>Group E Excluded: ", n_groupE_excluded
    )
  )
) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  geom_vline(
    xintercept = thresh_ex,
    linetype = "dashed",
    color = "red"
  ) +
  labs(
    x = "Minimum Mean International Trade Volume",
    y = "Group E Municipalities Included",
    title = "Sensitivity of Group E Sample Size to Export Threshold"
  ) +
  theme_minimal()

ggplotly(p, tooltip = "text")

# boxplot to show this 
ggplot(
  df_groupE,
  aes(x = "Group E", y = mean_intl_volume)
) +
  geom_violin(fill = "steelblue", alpha = 0.7) +
  geom_jitter(
    width = 0.1,
    alpha = 0.5
  ) +
  geom_hline(yintercept = x_intl_volume_threshold, color = "darkgreen", linetype = "dashed") +
  labs(
    x = NULL,
    y = "Mean International Trade Volume"
  ) +
  scale_y_log10()+
  theme_minimal()

# get quantile for filter instead 
quantile(
  df_groupE$mean_intl_volume,
  probs = c(0.1, 0.25, 0.5)
)

# ggplot with lines at 10% quantile, arbitrary 1000 tonnes, and 25% quantile cutoff
ggplot(df_groupE,
       aes(mean_intl_volume)) +
  geom_histogram(bins = 30) +
  geom_vline(xintercept = quantile(df_groupE$mean_intl_volume, 0.10), color = "blue") +
  geom_vline(xintercept = 1000, color = "red") +
  geom_vline(xintercept = quantile(df_groupE$mean_intl_volume, 0.25), color = "darkgreen") +
  scale_x_log10()


## 1.4) set new filters as main df ----
soy_df_split <- soy_df_split2


# 2) Plot data pre-DiD ----------


## 2.0) Download Spatial Data form geobr -------
# Get Municipalities, Mato Grosso municipalities, Mato Grosso State, and Cerrado Biome boundaries

# set year of data (necessary for 'geobr' package)
v_yr_shp <- 2013 

shp_muni <- read_municipality(
  year = v_yr_shp
)

# shp_mt_munis <- read_municipality(
#   code_muni = "MT",
#   year = v_yr_shp
# )

# Mato Grosso state boundary
shp_mt_state <- read_state(
  code_state = "MT",
  year = v_yr_shp
)

# Cerrado biome
shp_cerr <- read_biomes(
  year = 2025
) %>%
  filter(name_biome == "Cerrado")

# get munis in Cerrado
shp_muni_cerrado <- shp_muni %>%
  filter(lengths(st_intersects(geometry, shp_cerr)) > 0)

## 2.1) Clean & Join ------
# get df of alltime
df_map_alltime <- soy_df_split %>%
  filter(
    group_alltime %in% c("A", "E")
  ) %>%
  distinct(muni_id, group_alltime) %>% 
  rename(code_muni = muni_id)

sf_map_alltime_munis <- shp_muni_cerrado %>%
  left_join(
    df_map_alltime,
    by = "code_muni"
  )

## 2.3) Plot Maps-------
# color_A <- "brown"
# color_E <- "gold"



# colors_groups <- c(
#   "E" = "firebrick4",
#   "A" = "goldenrod"
# )

color_A <- "goldenrod"
color_E <- "firebrick4"

colors_groups <- c(
  "A" = color_A,
  "E" = color_E
)

### 2.3.1) Map of Groups Alltime -------

# Count municipalities by group
n_E <- sum(sf_map_alltime_munis$group_alltime == "E", na.rm = TRUE)
n_A <- sum(sf_map_alltime_munis$group_alltime == "A", na.rm = TRUE)

# Plot
ggplot() +
  # Municipalities
  geom_sf(
    data = sf_map_alltime_munis,
    aes(fill = group_alltime)#,
    #color = NA
  ) +
  
  # # Cerrado boundary
  # geom_sf(
  #   data = shp_cerr,
  #   fill = NA,
  #   color = "grey50",
  #   linewidth = 0.3
  # ) +
  
  # State outline
  geom_sf(
    data = shp_mt_state,
    fill = NA,
    color = "black",
    linewidth = 0.6
  ) +
  
  scale_fill_manual(
    values = colors_groups,
    breaks = c("E", "A"),
    na.value = "white"
  ) +
  
  labs(
    fill = "Group",
    title = paste0("Group E (<20% Domestic) and Group A (>80% Domestic)",
                   "\n",
                   "Cerrado Municipalities",
                   " (", min(soy_df_split$year), 
                   "-",
                   max(soy_df_split$year), ")",
                   
                   "\n",
                   "Trade Instability <", v_trade_inst_q1,
                   "\n",
                   "Mean 2007-2011 Intl. Trade >", 
                   round(x_intl_volume_threshold, 0),
                   " (Group E)",
                   
                   "\n",
                   "Three Valid Years from Pre (2007-2011) and Post (2013-2017) Periods",
                   
                   "\n",
                   ">50% Trade from 'Big 6' Exporters during Pre Period"
                   ),
    
    subtitle = paste0(
      "Municipality Counts: Group E = ", scales::comma(n_E),
      " | Group A = ", scales::comma(n_A)),
    
    caption = desc_DiD
  ) +
  
  theme_void()

### 2.3.2) Get Counts --------
# get counts 
sf_map_alltime_munis %>% count(group_alltime)

# rename alltime & 1-year for DiD
df_alltime <- soy_df_split %>% 
  filter(
    group_alltime %in% c("A", "E")
  )

## 2.3.3) SAVE --------- 
# Save to CSV
write.csv(
  df_alltime,
  "../Data_Derived/df_did_propalltime_filtered.csv",
  row.names = FALSE
)

# Save for future R analyses
saveRDS(
  df_alltime,
  "../Data_Derived/df_did_propalltime_filtered.rds"
)

# 3) Add MapBiomas Land Conversion Values to this ----------
## NOTE: maybe use Conversion intervals >1?

## GOAL: get to 'df_cerr' by:
# 1) loading & filtering to specific above geocodes and 
# 2) filtering to from/to levels with soybeans and RVCs

### aka the land change values from relevant vegetation classes (RVCs) to soybean per year per municipality. 
### need this to be able to filter by municipality categories A and E

## 3.1) Load in MapBiomas Transition ------

# NOTE: this is from 'MSU\TC_SIMPLEG_USBR_Zenodo_v1.1\TC_SIMPLEG_USBR_Zenodo\Data_Derived'
# Generated using 'C:\Users\Nick Manning\OneDrive - Michigan State University\Desktop\'MSU\TC_SIMPLEG_USBR_Zenodo_v1.1\TC_SIMPLEG_USBR_Zenodo\Code\3c_MapBiomas.R'
load(file = paste0(folder_derived, "mapb_col8_clean_long.Rdata"))
df_mapb_der <- df

# NOTE: THIS INCLUDES ALL 

## 3.2) Filter this down to relevant from/to classes ------
# set relevant vegetation class (RVCs) categories
rvc_from_lvl3 <- c("Forest Formation", "Savanna Formation", "Wetland",
                   "Grassland", "Pasture", "Forest Plantation",
                   "Mosaic of Agriculture and Pasture",
                   "Magrove", "Flooded Forest",
                   "Shrub Restinga", "Other Non Forest Natural Formation", "Wooded Restinga",
                   "Perennial Crops")
# even fewer RVCs
classes_few <- c(
  #"Temporary Crops", 
  "Forest Formation", "Mosaic of Agriculture and Pasture",
  "Pasture", "Savanna Formation", "Grassland")

# filter Mapbiomas data to only focus on transitions to "Soybeans" & From-To's that do not stay the same
df_rvc <- df %>%
  filter(to_level_4 == "Soy Beans") %>%
  filter(to_level_4 != from_level_4) %>% 
  filter(from_level_3 %in% rvc_from_lvl3)

## 3.3) Filter this down to relevant biomes/muni's -------
muni_codes_cerr <- shp_muni_cerrado$code_muni


# filter to only municipalities in Cerrado
df_rvc <- df_rvc %>%
  filter(geocode %in% muni_codes_cerr) %>%
  filter(biome == "Cerrado") %>%
  rename(muni_id = geocode)

# group_by 
df_rvc_agg <- df_rvc %>%
  aggregate(ha ~ year + muni_id, sum) %>%
  mutate(
    biome = "CERRADO",
    from_level_3 = "Sum of RVCs",
    to_level_4 = "Soy Beans",
    year = as.numeric(year),
    years = paste0(year-1,"-",year)
  )

## 3.4) Get 3-Year Rolling Sum ----------
# CHECK if each has municipality has all the years
df_rvc_agg %>%
  group_by(
    muni_id, biome,
    from_level_3, to_level_4
  ) %>%
  summarize(
    n_years = n_distinct(year),
    expected = max(year) - min(year) + 1,
    .groups = "drop"
  ) %>%
  filter(n_years != expected)

# add missing years so that we can get the 3-year rolling sum for transition values
# NOTE: we use 0 ha instead of NA here because with MapBiomas algorithm we expect there to be data present for each year. If not, then 0, not NA.
# NOTE (cont.): Plus, 0's in missing years won't really mess with rolling sums
df_rvc_agg_full <- df_rvc_agg %>%
  group_by(
    muni_id, biome,
    from_level_3, to_level_4
  ) %>%
  complete(year = min(year):max(year),
           fill = list(ha = 0)) %>%
  ungroup()

# get the 3 year rolling sum
df_3yr <- df_rvc_agg_full %>%
  arrange(year.by_group = TRUE) %>%
  group_by(
    muni_id, biome,
    from_level_3, to_level_4
  ) %>%
  # coalesce gets first non-missing value, so, here, it double-checks there are no NA values before summing
  mutate(
    ha_3yr =
      coalesce(ha, 0) +
      coalesce(lag(ha, 1), 0) +
      coalesce(lag(ha, 2), 0)
  ) %>%
  ungroup()

# double-check manually 
# df_3yr %>%
#   filter(
#     muni_id == 5200050,
#   ) %>%
#   select(year, ha, ha_3yr)
 
# select only those columns necessary for joining
df_3yr <- df_3yr %>% select(year, muni_id, ha_3yr)

# select down to only two columns: 'muni_id' & 'ha' to make joining seamless
df_mapb <- df_rvc_agg_full %>% 
  left_join(df_3yr, by = c('year', 'muni_id')) %>% 
  filter(year >= v_startyr & year <= v_endyr) %>% 
  select('year', 'muni_id', 'ha', 'ha_3yr') %>% 
  rename(
    ha_trans_mapb = ha,
    ha_3yr_trans_mapb = ha_3yr
  )

## 3.5) Merge df from DiD with df of RVCs to filter land change per category pre-post  ------

# double-check df_alltime has all years 
df_alltime %>%
  group_by(
    muni_id, destination) %>% 
  summarize(
    n_years = n_distinct(year),
    expected = max(year) - min(year) + 1,
    .groups = "drop"
  ) %>%
  filter(n_years != expected)

# make 'df_alltime' wide with domestic, intl, total as their own columns
df_alltime_mapb <- left_join(df_alltime, df_mapb, by = c('year', 'muni_id'))

# merge on df_alltime INTO df_cerr on 'year' and 'muni_id'
## result should be one row = one muni_id per one year per one "To-Soybean" Transition
glimpse(df_alltime_mapb)
head(df_alltime_mapb)

## 3.6) SAVE --------- 
# Save to CSV
write.csv(
  df_alltime_mapb,
  "../Data_Derived/df_did_propalltime_mapb_filtered.csv",
  row.names = FALSE
)

# Save for future R analyses
saveRDS(
  df_alltime_mapb,
  "../Data_Derived/df_did_propalltime_mapb_filtered.rds")