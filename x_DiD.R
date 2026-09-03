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
library(janitor)

# plotting ---
library(sf)
library(geobr)
library(ggplot2)

## Paths ##########
folder_source <- "../Data_Source/"

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

# get soy_df before summarizing 
soy_df_presummary <- soy_df %>% 
  filter(biome == "CERRADO") %>% 
  filter(year == 2012 | year == 2013)

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


# make sure each municipality also has an international row by getting all the rows with INTL yet and making one for them with everything set to 0
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
  group_by(
    year,
    biome,
    state,
    muni,
    muni_id
  ) %>%
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


# calculate proportion international
soy_df_split <- soy_df_split %>%
  
  # Municipality-year international trade proportion per year (for std. dev. later)
  group_by(year, muni_id) %>% # include muni in the group_by
  mutate(
    prop_intl_yr = {
      
      intl_vol <- sum(trade_volume[destination == "INTERNATIONAL"])
      total_vol <- sum(trade_volume[destination == "TOTAL"])
      
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
      
      intl_vol <- sum(trade_volume[destination == "INTERNATIONAL"])
      total_vol <- sum(trade_volume[destination == "TOTAL"])
      
      if_else(
        total_vol > 0,
        intl_vol / total_vol,
        NA_real_
      )
    }
  ) %>%
  ungroup()

# calculate std. dev as a substitute for trade instability - i.e. lower SD = more stable = lower trade instability
trade_instability <- soy_df_split %>%
  distinct(year, muni_id, prop_intl_yr) %>% # get just one muni per year rather than having one DOMESTIC and one INTERNATIONAL destination column
  group_by(muni_id) %>%
  summarise(
    trade_instability = sd(prop_intl_yr),
    .groups = "drop"
  )

# add std. dev. to other df
soy_df_split <- soy_df_split %>%
  left_join(trade_instability, by = "muni_id")

# create groups based on da Silva et al., 2023: https://doi.org/10.1038/s41598-023-38405-1
# NOTE: right now we make this grouped by each ROW independently, i.e. by each year, however, we may want to split this by MUNICIPALITY over time based on average split per- and post-shock  
# NOTE: this is TOTAL proportion, i.e. over the entire timespan
v_trade_instab_limit <- 0.3
v_trade_inst_q1 <- round(as.numeric(quantile(trade_instability, 0.25, na.rm = T)), 2)

soy_df_split <- soy_df_split %>%
  mutate(
    group_alltime = case_when(
      prop_intl_alltime <= 0.20 & trade_instability < v_trade_inst_q1 ~ "A",
      prop_intl_alltime >= 0.80 & trade_instability < v_trade_inst_q1 ~ "E",
      TRUE ~ NA_character_
    )
  )

# 2) Plot data pre-DiD ----------

## 2.0) Download Spatial Data form geobr -------
# Get Municipalities, Mato Grosso municipalities, Mato Grosso State, and Cerrado Biome boundaries

# set year of data (necessary for 'geobr' package)
v_yr_shp <- 2013 

shp_muni <- read_municipality(
  year = v_yr_shp
)

shp_mt_munis <- read_municipality(
  code_muni = "MT",
  year = v_yr_shp
)

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
                   "Trade Instability <", v_trade_inst_q1)
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
  "../Data_Derived/df_did_propalltime.csv",
  row.names = FALSE
)

# Save for future R analyses
saveRDS(
  df_alltime,
  "../Data_Derived/df_did_propalltime.rds"
)

# # *XX) Add MapBiomas Land Conversion Values to this ----------
# ## NOTE: maybe use Conversion intervals >1? 
# 
# ## GOAL: get to 'df_cerr
# 
# ### aka the land change values from relevant vegetation classes (RVCs) to soybean per year per municipality. 
# ### need this to be able to filter by municipality categories A and E
## X.1) Load in MapBiomas Transition ------
# # Load collection 8 data in tabular form 
# csv_br_trans_m <- read.csv(paste0(folder_source, "SOURCE_transonly_col8_mapbiomas_municip.csv"), encoding = "UTF-8")
# names(csv_br_trans_m)
# 
# 
# ## X.1) Tidy -----
# 
# df <- csv_br_trans_m
# 
# # remove all accents
# df$state <- stri_trans_general(str = df$state,  id = "Latin-ASCII")
# df$biome <- stri_trans_general(str = df$biome,  id = "Latin-ASCII")
# names(df)
# 
# # select levels and years to reduce df size 
# df <- dplyr::select(df, c("state","municipality", "geocode", "biome", 
#                           "from_level_3", "to_level_3",
#                           "from_level_4", "to_level_4",
#                           #"X1985.1986", "X1986.1987", "X1987.1988", "X1988.1989", "X1989.1990", 
#                           #"X1990.1991", "X1991.1992", "X1992.1993", "X1993.1994", "X1994.1995", "X1995.1996", "X1996.1997", "X1997.1998", "X1998.1999",    
#                           "X1999.2000", "X2000.2001", "X2001.2002", "X2002.2003", "X2003.2004",    "X2004.2005",    "X2005.2006",   
#                           "X2006.2007",    "X2007.2008",    "X2008.2009",    "X2009.2010",   "X2010.2011", "X2011.2012",    "X2012.2013",   
#                           "X2013.2014",    "X2014.2015",    "X2015.2016",  "X2016.2017",    "X2017.2018",   
#                           "X2018.2019",    "X2019.2020",    "X2020.2021"))
# 
# # remove all but the last four digits of all the columns 
# names(df) <- str_sub(names(df), - 4, - 1)
# names(df)
# 
# # rename columns 
# # BEWARE HERE, this is manual for now, if you change the 'select' above then you need to change this as well 
# colnames(df)[colnames(df) %in% c("tate", "lity", "code", "iome", "el_3", "el_3",  "el_4", "el_4")] <- c("state", "municipality", "geocode", "biome", 
#                                                                                                         "from_level_3", "to_level_3",
#                                                                                                         "from_level_4", "to_level_4")
# names(df)
# 
# ## X.2) Make 'long' -----
# # gather to make into a long dataset using pivot_longer (since gather() has been replace)
# # NOTE: change the number if you changed 'select' above
# ncol(df)
# 
# df <- pivot_longer(
#   df,
#   cols = 9:ncol(df),
#   names_to = "year",
#   values_to = "ha"
# )
# 
# 
# ## X.3) Save df -----
# # save(df, file = paste0(folder_derived, "mapb_col8_clean_long.Rdata"))
# # NOTE: THIS INCLUDES ALL 
# 
# 
# 
# # X) Plot Transition Results -----
# 
# # set relevant vegetation class categories
# list_from_lv3 <- c("Forest Formation", "Savanna Formation", "Wetland",
#                    "Grassland", "Pasture", "Forest Plantation",
#                    "Mosaic of Agriculture and Pasture",
#                    "Magrove", "Flooded Forest",
#                    "Shrub Restinga", "Other Non Forest Natural Formation", "Wooded Restinga",
#                    "Perennial Crops")
# 
# # filter Mapbiomas data to only focus on transitions to "Soybeans" & From-To's that do not stay the same
# df <- df %>%
#   filter(to_level_4 == "Soy Beans") %>%
#   filter(to_level_4 != from_level_4)
# 
# ## X.1) Facet Map of Cerrado Transition ----
# 
# ### X.1.1) Prep Spatial Data ---------
# 
# # NOTE: Municipality & Cerrado Shapefiles come from 'geobr' package
# 
# # Load municipality shapefile
# # Read all municipalities in the country at a given year
# # shp_muni <- read_municipality(code_muni="all", year=2018)
# 
# # Load Other Shapefiles 
# # load(paste0(folder_derived, "shp_usbr.RData"))
# 
# # shp_cerr <- read_biomes(
# #   year = 2019,
# #   simplified = T,
# #   showProgress = T) %>%
# #   dplyr::filter(name_biome == "Cerrado")
# 
# # Old way: get municipalities that are at all within the Cerrado
# # shp_muni_in_cerr <- st_intersection(shp_muni, shp_cerr)
# 
# # New way: get municipalities that are at all within the Cerrado
# shp_muni_cerrado <- shp_muni %>%
#   filter(lengths(st_intersects(geometry, shp_cerr)) > 0)
# 
# # get just the codes column and keep as shapefile
# # shp_code_muni_in_cerr <- shp_muni_in_cerr %>%  dplyr::select(code_muni)
# shp_code_muni_in_cerr <- shp_muni_cerrado %>%  dplyr::select(code_muni)
# 
# # get territory codes for municipalities in intersection as numeric
# # muni_codes_cerr <- shp_muni_in_cerr$code_muni
# muni_codes_cerr <- shp_muni_cerrado$code_muni
# 
# # filter to only municipalities in Cerrado
# df_cerr <- df %>%
#   filter(geocode %in% muni_codes_cerr) %>%
#   filter(biome == "Cerrado") %>% 
#   rename(muni_id = geocode)

## *3.2) Merge df from DiD with df of RVCs to filter land change per category pre-post 
# make 'df_alltime' wide with domestic, intl, total as their own columns
# PICK UP HERE --------------
# merge on df_alltime INTO df_cerr on 'year' and 'muni_id'
## result should be one row = one muni_id per one year per one "To-Soybean" Transition


# 4) Basic DiD -----------

# Notes:
## The logic here is:
### Group A is our Untreated Group because it has consistently mainly domestic trade
### Group E is our Treated as it is consistently mainly international trade 
### Pre-Treatment is 2007-2011 average 
### Post-Treatment is 2013-2017 average

# Get groups in DiD format
df_did <- df_alltime %>%
  filter(group_alltime %in% c("A", "E")) %>%   # ignore NAs
  mutate(
    period = case_when(
      year >= v_startyr & year <= 2011 ~ "pre_2012",
      year == 2012 ~ "2012",
      year >= 2013 & year <= v_endyr ~ "post_2012"
    )
  ) 

summary_count_did <- df_did %>%
  count(group_alltime, period) %>%
  tidyr::pivot_wider(
    names_from = period,
    values_from = n,
    values_fill = 0
  )

## 3.1) (OMIT) Basic EXPORT plots -------

# get and plot per group per year
df_did_exp_sum_yr <- df_did %>%
  filter(destination == "TOTAL") %>%
  group_by(group_alltime, period, year) %>%
  summarise(
    total_exports = sum(trade_volume, na.rm = TRUE),
    .groups = "drop"
  ) 

df_did_exp_mean_yr <- df_did %>%
  filter(destination == "TOTAL") %>%
  group_by(group_alltime, period, year) %>%
  summarise(
    mean_exports = mean(trade_volume, na.rm = TRUE),
    .groups = "drop"
  ) 

### PLOT ###


ggplot(
  df_did_exp_sum_yr,
  aes(
    x = year,
    y = total_exports,
    group = group_alltime,
    color = group_alltime
  )
) +
  geom_line() +
  geom_point(size = 3)+
  geom_vline(xintercept = 2012)

# get just the relevant trade volume and create the DiD groups 
df_did_exp_sum <- df_did %>%
  filter(destination == "TOTAL") %>%
  group_by(group_alltime, period) %>%
  summarise(
    total_exports = sum(trade_volume, na.rm = TRUE),
    .groups = "drop"
  ) 

df_did_exp_mean <- df_did %>%
  filter(destination == "TOTAL") %>%
  group_by(group_alltime, period) %>%
  summarise(
    mean_exports = mean(trade_volume, na.rm = TRUE),
    .groups = "drop"
  )

# Test Plot

# Create DF
df_did_exp_mean_plot <- df_did_exp_mean %>% 
  filter(period != "2012") %>% 
  mutate(
    period = factor(
      period,
      levels = c("pre_2012", "2012", "post_2012")
    )
  )

df_did_exp_sum_plot <- df_did_exp_sum %>% 
  filter(period != "2012") %>% 
  mutate(
    period = factor(
      period,
      levels = c("pre_2012", "2012", "post_2012")
    )
  )
# Plot
## Plot mean
ggplot(
  df_did_exp_mean_plot,
  aes(
    x = period,
    y = mean_exports,
    color = group_alltime,
    group = group_alltime
  )
) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  labs(
    x = NULL,
    y = "Mean Trade Volume",
    color = "Group",
    title = "Mean Soybean Exports by Group Through Time"
  ) +
  theme_minimal()

## Plot sum
ggplot(
  df_did_exp_sum_plot,
  aes(
    x = period,
    y = total_exports,
    color = group_alltime,
    group = group_alltime
  )
) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  labs(
    x = NULL,
    y = "Total Trade Volume",
    color = "Group",
    title = "Total Soybean Exports by Group Through Time"
  ) +
  theme_minimal()

## 3.2) Basic SOY AREA plots -------

# get and plot per group per year
df_did_area_sum_yr <- df_did %>%
  filter(destination == "TOTAL") %>%
  group_by(group_alltime, period, year) %>%
  summarise(
    total_area = sum(soy_area, na.rm = TRUE),
    .groups = "drop"
  ) 

df_did_area_mean_yr <- df_did %>%
  filter(destination == "TOTAL") %>%
  group_by(group_alltime, period, year) %>%
  summarise(
    mean_area = mean(soy_area, na.rm = TRUE),
    .groups = "drop"
  ) 


### PLOT ###

ggplot(
  df_did_area_sum_yr,
  aes(
    x = year,
    y = total_area,
    group = group_alltime,
    color = group_alltime
  )
) +
  geom_line() +
  scale_color_manual(values = colors_groups, breaks = c("E", "A"))+
  geom_point(size = 3)+
  geom_vline(xintercept = 2012) + 
  scale_x_continuous(breaks = seq(min(df_did_area_sum_yr$year), max(df_did_area_sum_yr$year), by = 1))+
  labs(
    title = "Annual Soybean Area per Export Group",
    y = "Total Area (ha)",
    color = "Export Group"
  )+
  theme_light()

# get just the relevant trade volume and create the DiD groups 
df_did_area_sum <- df_did %>%
  filter(destination == "TOTAL") %>%
  group_by(group_alltime, period) %>%
  summarise(
    total_area = sum(soy_area, na.rm = TRUE),
    .groups = "drop"
  ) 

df_did_area_mean <- df_did %>%
  filter(destination == "TOTAL") %>%
  group_by(group_alltime, period) %>%
  summarise(
    mean_area = mean(soy_area, na.rm = TRUE),
    .groups = "drop"
  )

df_did_area_mean_sumyr <- df_did_area_sum_yr %>%
  group_by(group_alltime, period) %>%
  summarise(
    mean_area = mean(total_area, na.rm = TRUE),
    .groups = "drop"
  ) 

# sum per year then mean
# get just the relevant area and create the DiD groups 
df_did_area_sum_yr <- df_did %>%
  filter(destination == "TOTAL") %>%
  group_by(group_alltime, period) %>%
  summarise(
    total_area = sum(soy_area, na.rm = TRUE),
    .groups = "drop"
  ) 

df_did_area_mean <- df_did %>%
  filter(destination == "TOTAL") %>%
  group_by(group_alltime, period) %>%
  summarise(
    mean_area = mean(soy_area, na.rm = TRUE),
    .groups = "drop"
  )



# Test Plot

# Create DF
df_did_area_mean_plot <- df_did_area_mean %>% 
  filter(period != "2012") %>% 
  mutate(
    period = factor(
      period,
      levels = c("pre_2012", "2012", "post_2012")
    )
  )

df_did_area_sum_plot <- df_did_area_sum %>% 
  filter(period != "2012") %>% 
  mutate(
    period = factor(
      period,
      levels = c("pre_2012", "2012", "post_2012")
    )
  )

df_did_area_sumyr_plot <- df_did_area_sum_yr %>% 
  filter(period != "2012") %>% 
  mutate(
    period = factor(
      period,
      levels = c("pre_2012", "2012", "post_2012")
    )
  ) 

df_did_area_mean_sumyr_plot <- df_did_area_mean_sumyr %>% 
  filter(period != "2012") %>% 
  mutate(
    period = factor(
      period,
      levels = c("pre_2012", "2012", "post_2012")
    )
  ) 

# Plot
## Plot mean
ggplot(
  df_did_area_mean_plot,
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
    color = "Group",
    title = "Mean Soybean Area by Group Through Time"
  ) +
  theme_minimal()

## Plot sum
ggplot(
  df_did_area_sum_plot,
  aes(
    x = period,
    y = total_area,
    color = group_alltime,
    group = group_alltime
  )
) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  scale_color_manual(values = colors_groups, breaks = c("E", "A"))+
  labs(
    x = NULL,
    y = "Total Soybean Area",
    color = "Group",
    title = "Total Soybean Area by Group Through Time"
  ) +
  theme_minimal()

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
    # title = "Sum-then-Mean Soybean Area by Group Through Time"
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

ex_means2 <- df_did_area_mean_sumyr_plot %>% 
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

# My Attempt with all muni's
area_allmuni <- df_did %>% 
  mutate(
    Treated = group_alltime == "E" &
      period == "post_2012"
    # year > 2012
  )

clfe_area_allmuni <- feols(soy_area ~ Treated | group_alltime + period,
              data = area_allmuni, vcov = ~period)

msummary(clfe_area_allmuni, stars = c('*' = 0.1, '**' = 0.05, '***' = 0.01))

# My Attempt with annual data
area_mean_yr <- df_did_area_mean_yr %>% 
  filter(period != "2012") %>% 
  mutate(
    Treated = group_alltime == "E" &
      period == "post_2012"
    # year > 2012
  )

clfe_area_mean_yr <- feols(mean_area ~ Treated | group_alltime + period,
                           data = area_mean_yr, vcov = ~period)

msummary(clfe_area_mean_yr, stars = c('*' = 0.1, '**' = 0.05, '***' = 0.01))

# run basic linear model with interaction terms 
lm_area_mean_yr <- lm(mean_area ~ group_alltime + period + Treated, data = area_mean_yr)
summary(lm_area_mean_yr)

# run linear model with indicator variables
area_mean_yr_ind <- area_mean_yr %>% 
  mutate(Ind_TreatmentGroup = if_else(group_alltime == "E", 1, 0)) %>% 
  mutate(Ind_Period = if_else(period == "post_2012", 1, 0))

lm_area_mean_yr_ind <- lm(mean_area ~ Ind_TreatmentGroup + Ind_Period + Ind_TreatmentGroup*Ind_Period, data = area_mean_yr_ind)
summary(lm_area_mean_yr_ind)

msummary(lm_area_mean_yr_ind, stars = c('*' = 0.1, '**' = 0.05, '***' = 0.01))

# PICK UP HERE ###########
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
