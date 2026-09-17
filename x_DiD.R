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

# get one source file with just the Cerrado and 2012-2013 to test
# soy_df_source_cerrado_2012013 <- soy_df_source %>% 
#   filter(Biome == "CERRADO") %>% 
#   filter(Year == 2012 | Year == 2013)

# Clean soy_df
soy_df <- soy_df_source %>% janitor::clean_names()

# Clean initial soy_df
soy_df <- soy_df %>% 
  
  # select relevant columns
  select(
    # basic info
    year, biome, 
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

# get soy_df before summarizing - only 2012 and 2013
# soy_df_presummary <- soy_df %>% 
#   filter(biome == "CERRADO") %>% 
#   filter(year == 2012 | year == 2013)

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

# soy_df_split %>%
#   group_by(biome, state, muni, muni_id, destination) %>%
#   summarize(
#     n_years = n_distinct(year),
#     missing = max(year) - min(year) + 1 - n_years,
#     .groups = "drop"
#   ) %>%
#   summarize(
#     n_groups_missing = sum(missing > 0),
#     total_missing_years = sum(missing)
#   )

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

### TO-DO: filter for municipalities exporting less than X? -----
## Come back to this - need to figure out the groups first then I can filter
mean_intl_volume <- soy_df_split %>%
  filter(destination == "INTERNATIONAL") %>%
  group_by(muni_id) %>%
  summarize(
    mean_intl_volume = mean(trade_volume, na.rm = TRUE),
    .groups = "drop"
  )

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

### Set threshold here ----------

# set threshold based on 1st quartile of data ignoring SD of 0
## "We set our threshold based on the first quartile of those municipalities with any variation in their international trade (i.e. SD != 0)"

# check proportion 
quantile(
  # trade_instability$sd_prop_intl  # <-- results in 0, so we need to filter to > 0
  trade_instability$sd_prop_intl[trade_instability$sd_prop_intl > 0],
  probs = c(0.25, 0.5, 0.75),
  na.rm = TRUE)

# v_trade_instab_limit <- 0.2
# v_trade_inst_q1 <- round(as.numeric(quantile(trade_instability$sd_prop_intl[trade_instability$sd_prop_intl > 0], 0.25, na.rm = T)), 5)

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

# Threshold for minimum mean international trade volume
x_intl_volume_threshold <- 1000

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
        mean_intl_volume > x_intl_volume_threshold 
      ~ "E",
      
      TRUE ~ NA_character_
    )
  )

# test group membership with new filters 
table(soy_df_split2$group_alltime[soy_df_split2$destination=="TOTAL" & soy_df_split2$year==2013])

# set new filters as main df
soy_df_split <- soy_df_split2


# ### check 2013 group E values for filtering above -----
# x_2013 <- soy_df_split %>% 
#   filter(year == 2013 & group_alltime == "E" & destination != "DOMESTIC")
# 
# x_testval <- 1000
# x_2013_intl <- x_2013 %>% filter(destination=="INTERNATIONAL") %>% filter(trade_volume < x_testval)
# 
# # histogram
# ggplot(x_2013_intl,
#        aes(x = trade_volume)) +
#   geom_histogram(bins = 50)+
#   labs(
#     x = "Intl. Trade Group E Muni's in 2013",
#     title = paste0("Intl. Trade Group E 2013 Trade Volume filtered to < ", x_testval, 
#                    "\n", "n = ", length(x_2013_intl))
#   )


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
color_A <- "brown"
color_E <- "gold"

colors_groups <- c(
  "A" = color_A,
  "E" = color_E
)

### 2.3.1) Map of Groups Alltime -------

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
                   "Mean Intl. Trade >", x_intl_volume_threshold," (Group E)",
                   "\n",
                   "Three Valid Years from Pre (2007-2011) and Post (2013-2017) Periods"
                   )
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

# 4) Basic DiD -----------

# Notes:
## The logic here is:
### Group A is our Untreated Group because it has consistently mainly domestic trade
### Group E is our Treated as it is consistently mainly international trade 
### Pre-Treatment is 2007-2011 average 
### Post-Treatment is 2013-2017 average

# Get groups in DiD format
df_did <- df_alltime_mapb %>%
  filter(group_alltime %in% c("A", "E")) %>%   # ignore NAs
  mutate(
    period = case_when(
      year >= v_startyr & year <= 2011 ~ "pre_2012",
      year == 2012 ~ "2012",
      year >= 2013 & year <= v_endyr ~ "post_2012"
    )
  ) 

summary_count_did <- df_did %>%
  filter(destination == "TOTAL") %>% 
  count(group_alltime, period) %>%
  tidyr::pivot_wider(
    names_from = period,
    values_from = n,
    values_fill = 0
  )
## 3.1.0) TEST fxn for plotting -------
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
    )
}

# test fxn 
plot_annual_summary(
  df = df_did,
  var = "trade_volume",
  fun = sum
)

# now try with df_alltime_mapb 
names(df_alltime_mapb)

plot_annual_summary(
  df_alltime_mapb,
  "ha_3yr_trans_mapb",
  sum
)


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
  "ha_3yr_trans_mapb",
  mean
)

## 3.3) Manual Plotting -------
### 3.3.1) Sum then Mean (OLD) -----
# Muni --SUM--> Year --MEAN--> Period
# sum per year then mean
# get just the relevant area and create the DiD groups 

# sum per year per export group 
df_did_area_sum_yr <- df_did %>%
  filter(destination == "TOTAL") %>%
  group_by(group_alltime, period, year) %>%
  summarise(
    total_area = sum(soy_area, na.rm = TRUE),
    .groups = "drop"
  ) 

# calculate the mean -- doesn't actually do anything??
df_did_area_mean_sumyr <- df_did_area_sum_yr %>%
  group_by(group_alltime, period) %>%
  summarise(
    mean_area = mean(total_area, na.rm = TRUE),
    .groups = "drop"
  ) 

# get into the right format for easy plotting  
df_did_area_mean_sumyr_plot <- df_did_area_mean_sumyr %>% 
  filter(period != "2012") %>% 
  mutate(
    period = factor(
      period,
      levels = c("pre_2012", "2012", "post_2012")
    )
  ) 

## Plot mean of sumyr
ggplot(
  df_did_area_mean_sumyr_plot,
  aes(
    x = period,
    y = mean_area,
    color = group_alltime,
    group = group_alltime
  )
) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  scale_color_manual(values = colors_groups, breaks = c("E", "A"))+
  labs(
    x = NULL,
    y = "Mean Soy Area",
    color = "Export Group",
    title = "Sum-then-Mean Soybean Area by Group Through Time"
    #title = "Difference in Mean Annual Soybean Area by Group"
  ) +
  theme_light()

### 3.3.2) Mean then Mean -----
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

# get into the right format for easy plotting  
df_did_area_mean_meanyr_plot <- df_did_area_mean_meanyr %>% 
  filter(period != "2012") %>% 
  mutate(
    period = factor(
      period,
      levels = c("pre_2012", "2012", "post_2012")
    )
  ) 

## Plot mean of sumyr
ggplot(
  df_did_area_mean_meanyr_plot,
  aes(
    x = period,
    y = mean_area,
    color = group_alltime,
    group = group_alltime
  )
) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  scale_color_manual(values = colors_groups, breaks = c("E", "A"))+
  labs(
    x = NULL,
    y = "Mean Soy Area",
    color = "Export Group",
    # title = "Sum-then-Mean Soybean Area by Group Through Time"
    title = "Difference in Mean Annual Soybean Area by Group"
  ) +
  theme_light()

### 3.3.3) Plot Counterfactual -------
# Get the values needed
a_pre <- df_did_area_mean_meanyr_plot %>%
  filter(group_alltime == "A",
         period == "pre_2012") %>%
  pull(mean_area)

a_post <- df_did_area_mean_meanyr_plot %>%
  filter(group_alltime == "A",
         period == "post_2012") %>%
  pull(mean_area)

e_pre <- df_did_area_mean_meanyr_plot %>%
  filter(group_alltime == "E",
         period == "pre_2012") %>%
  pull(mean_area)

e_post <- df_did_area_mean_meanyr_plot %>%
  filter(group_alltime == "E",
         period == "post_2012") %>%
  pull(mean_area)

# Apply A's change to E's starting value
e_counterfactual_post <- e_pre + (a_post - a_pre)

# Create data frame for plotting
df_counterfactual <- tibble(
  period = factor(
    c("pre_2012", "post_2012"),
    levels = c("pre_2012", "post_2012")
  ),
  mean_area = c(
    e_pre,
    e_counterfactual_post
  ),
  group_alltime = "E Counterfactual"
)

# append to plotting data 
df_did_area_mean_meanyr_plot_cf <- bind_rows(
  df_did_area_mean_meanyr_plot,
  df_counterfactual
)

# plot 
ggplot(
  df_did_area_mean_meanyr_plot_cf,
  aes(
    x = period,
    y = mean_area,
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
    y = "Mean Soy Area",
    color = "Export Group",
    title = "Difference in Mean Annual Soybean Area by Group"
  ) +
  theme_light()

# xx Basic DiD Code -------
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


# Our Data for Basic DiD -----

## Most Basic Four Mean DiD -------
# now, get before-after differences for both groups
ex_means <- ex_diddata %>% group_by(group, after) %>% summarize(Y=mean(Y))

ex_means2 <- df_did_area_mean_meanyr_plot %>% 
  mutate(group = ifelse(group_alltime == "A", "UntreatedGroup", "TreatedGroup"),
         after = ifelse(period == "pre_2012", F, T))

#before-after difference for untreated; has the time effect only 
ex_bef.aft.untreated <- filter(ex_means2, group == "UntreatedGroup", after == 1)$mean_area - filter(ex_means2, group == "UntreatedGroup", after == 0)$mean_area

#before-after difference for treated; has the time AND treated effect 
ex_bef.aft.treated <- filter(ex_means2, group == "TreatedGroup", after == 1)$mean_area - filter(ex_means2, group == "TreatedGroup", after == 0)$mean_area

#Difference-in-Difference! Take the Time+Treated effect and remove the time effect 
DID <- ex_bef.aft.treated - ex_bef.aft.untreated
DID

## EX: Regression DiD -------

### PICK UP HERE ------

# clean code once more then... 
# ...Write results and send to Ken??? 
# regression results look, really interesting???

# need to check parallel trends assumption more rigorously

# SOURCE: The Effect, Huntington-Klein
# Link: https://theeffectbook.net/ch-DifferenceinDifference.html#two-way-fixed-effects
library(tidyverse)
library(modelsummary)
library(fixest)
library(causaldata)

# Treatment Variable 
od <- causaldata::organ_donations

od2 <- od %>% 
  mutate(
    Treated = State == "California" &
      Quarter %in% c('Q32011', 'Q42011', 'Q12012'))

# cluster using vcov = ~clustervariable 
clfe <- feols(Rate ~ Treated | State + Quarter,
              data = od2, vcov = ~State)

msummary(clfe, stars = c('*' = 0.1, '**' = 0.05, '***' = 0.01))

# All Muni's --------
# My Attempt with all muni's
area_allmuni <- df_did %>% 
  filter(destination == "TOTAL") %>% 
  mutate(
    Treated = group_alltime == "E" &
      period %in% "post_2012"
    # year > 2012
  )

area_allmuni <- df_did %>% 
  filter(destination == "TOTAL") %>% 
  mutate(
    Treated = group_alltime == "E" &
      period == "post_2012"
    # year > 2012
  )

clfe_area_allmuni <- feols(soy_area ~ Treated | group_alltime + period,
              data = area_allmuni, vcov = ~period)

msummary(clfe_area_allmuni, stars = c('*' = 0.1, '**' = 0.05, '***' = 0.01))

### TEST #####
lm_test <- feols(
  soy_area ~ Treated | muni_id + year,
  data = area_allmuni,
  vcov = ~muni_id
)

msummary(lm_test, stars = c('*' = 0.1, '**' = 0.05, '***' = 0.01))

# compare the regression results - pay attention to the SE section 
etable(
  feols(
    soy_area ~ Treated | group_alltime + period,
    data = area_allmuni
  ),
  feols(
    soy_area ~ Treated | group_alltime + period,
    data = area_allmuni,
    vcov = ~period
  )
)

# plot
library(ggplot2)

df_did %>%
  filter(
    destination == "TOTAL",
    group_alltime %in% c("A", "E")
  ) %>%
  ggplot(
    aes(
      x = factor(year),
      y = log(soy_area),
      color = group_alltime
    )
  ) +
  geom_jitter(
    width = 0.2,
    alpha = 0.3,
    size = 1.5
  ) +
  scale_color_manual(
    values = c(
      "A" = "#1b9e77",
      "E" = "#d95f02"
    )
  ) +
  labs(
    x = "Year",
    y = "Soy area (ha)",
    color = "Group"
  ) +
  theme_bw()

# plot 2
area_allmuni %>%
  filter(
    destination == "TOTAL",
    group_alltime %in% c("A", "E"),
  ) %>%
  ggplot(
    aes(
      x = factor(year),
      y = soy_area,
      fill = group_alltime
    )
  ) +
  geom_jitter(
    width = 0.2,
    alpha = 0.3,
    size = 1.5
  ) +
  # geom_violin(
  #   alpha = 0.5,
  #   position = position_dodge(width = 0.8)
  # ) +
  geom_point(
    stat = "summary",
    fun = mean,
    position = position_dodge(width = 0.8),
    size = 2
  )+
  #scale_y_log10()+
  theme_bw()


# test ggdist()
library(ggplot2)
library(ggdist)

area_allmuni %>%
  filter(
    destination == "TOTAL",
    group_alltime %in% c("A", "E")
  ) %>%
  ggplot(
    aes(
      x = factor(year),
      y = soy_area,
      fill = group_alltime
    )
  ) +
  stat_halfeye(
    position = position_dodge(width = 0.8),
    justification = -0.2,
    alpha = 0.6,
    width = 0.7,
    point_interval = median_qi
  ) +
  scale_y_log10(labels = scales::comma) +
  theme_bw()

# NEW: with line plot
plot_data <- function(x_df, x_y, x_fun, x_log10){
x_df %>%
  filter(
    destination == "TOTAL",
    group_alltime %in% c("A", "E")
  ) %>%
  ggplot(
    aes(
      x = factor(year),
      y = soy_area,
      fill = group_alltime
    )
  ) +
  geom_violin(
    alpha = 0.3,
    width = 0.8,
    position = position_dodge(width = 0.7)
  ) +
  
  # Summary points
  stat_summary(
    aes(color = group_alltime),
    fun = mean,
    geom = "point",
    #position = position_dodge(width = 0.8),
    size = 2.5
  ) +
  
  # Summary lines
  stat_summary(
    aes(
      group = group_alltime,
      color = group_alltime
    ),
    fun = mean,
    geom = "line",
    linewidth = 1,
   # position = position_dodge(width = 0.8)
  ) +
  
  if_else(x_log10 = T, 
          scale_y_log10(labels = scales::comma))
           +
  theme_bw()+
  labs(
    title = paste0("Annual ", x_fun, " of ", x_y,
                   ifelse(
                     x_log10 = T, "Log10"
                   ))
  )
}

plot_data(
  x_df = area_allmuni,
  x_y = "soy_area",
  x_fun = "mean",
  x_log10 = TRUE
)


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
  
  
  if (x_log10) {
    p <- p +
      scale_y_log10(labels = scales::comma)
  }
  
  return(p)
}

plot_data(
  x_df = area_allmuni,
  x_y = "soy_area",
  x_fun = mean,
  x_log10 = T
)

plot_data(
  x_df = area_allmuni,
  x_y = "ha_trans_mapb",
  x_fun = mean,
  x_log10 = T
)

### TEST END #####
# run basic linear model with interaction terms 
lm_area_mean <- lm(soy_area ~ group_alltime + period + Treated, data = area_allmuni)
summary(lm_area_mean)

## Annual -------
# My Attempt with annual data
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
# PICK UP HERE (2) ###########
# 4) Dynamic DiD Example -------- 
# Example Link https://bcallaway11.github.io/did/articles/did-basics.html#examples-with-simulated-data
library(did) # manually type step-by-step!

## 4.1 Build the Dataset ------
# set seed so everything is reproducible
set.seed(1814)

# generate dataset with 4 time periods
sp <- reset.sim()
sp$te <- 0
time.periods <- 4

# add dynamic effects
sp$te.e <- 1:time.periods

# generate data set with these parameters
# here, we dropped all units who are treated in time period 1 as they do not help us recover ATT(g,t)'s
dta <- build_sim_dataset(sp)

# How many observations remained after dropping the ``always-treated'' units
nrow(dta)

#This is what the data looks like
head(dta)

# estimate group-time average treatment effects using att_gt method
example_attgt <- att_gt(
  yname = "Y", 
  tname = "period",
  idname = "id",
  gname = "G",
  xformla = ~X,
  data = dta
)

# summarize results
summary(example_attgt)

# get real data for example
data(mpdta)
mpdta
df.mpdta <- as.data.frame(mpdta)
