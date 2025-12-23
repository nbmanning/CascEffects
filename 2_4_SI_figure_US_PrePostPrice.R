rm(list = ls())
# libraries
library(dplyr)
library(ggplot2)
library(tidyUSDA) # prod & price data
library(sf) 

# library(stringr) # for str_pad() & str_to_title()
# library(tigris) # for FIPS database
# library(fiftystater) # for us state map; note: had to install from GitHub for my current R, version 4.2.2 
# library(RColorBrewer)
# library(classInt) # for mapping and setting breaks 
# library(reshape2)

## key for tidyusda() - should be personal for each user
usda_key <- "34BD2DD3-9049-37A1-BC2C-D8A967E25E42"

## states of interest 
mw_st_abv <- c("ND", "SD", "NE","KS", "MO", "IA", "MN", "WI", "IL", "IN", "OH", "MI")

## 1.2: Load Base US Spatial Data --------------

# get all states 
states <- states(cb = TRUE)

## US-MW ##
# filter to US-MW 
states_mw <- states %>%
  
  filter(STUSPS %in% mw_st_abv)

# get US-MW Counties 
# NOTE: state == The two-digit FIPS code (string) of the state you want, or a vector of codes if you want multiple states. Can also be state name or state abbreviation.
states_counties <- counties(state = mw_st_abv, cb = FALSE, resolution = "500k", year = NULL)

## CONUS ##
# get conus 
states_conus <- states %>% filter(!STUSPS %in% c("HI","AK", "PR", "VI", "MP", "AS", "GU"))

# 3: SI Plot: Change in  Price --------
# set year range 
year_range <- 2011:2012

# get price at state level from USDA-QuickStats
raw_price_USMWst <- getQuickstat(
  key = usda_key,
  program = "SURVEY",
  data_item = "SOYBEANS - PRICE RECEIVED, MEASURED IN $ / BU",
  commodity = "SOYBEANS",
  geographic_level = "STATE",
  state = c("NORTH DAKOTA", "SOUTH DAKOTA", "NEBRASKA", "KANSAS", "MISSOURI",
            "IOWA", "MINNESOTA", "WISCONSIN", "ILLINOIS", "INDIANA", "OHIO", "MICHIGAN"),
  year = paste(year_range),
  geometry = F) %>%
  # keep only some of the variables
  dplyr::select(
    year, county_name, county_code, state_name, state_alpha,
    state_fips_code, short_desc, freq_desc, reference_period_desc, Value) %>%
  # rename the Value column
  dplyr::rename(price = Value) 

# filter to ANNUAL or AUGUST to compare between the two
raw_price_USMWst_annual <- raw_price_USMWst  %>% 
  filter(freq_desc == "ANNUAL")

raw_price_USMWst_aug <- raw_price_USMWst  %>% 
  filter(reference_period_desc == "AUG")

# Calculate the differences between 2012 vs. 2011 
# NOTE: have to do this separately or else we'll be comparing between ANNUAL and AUGUST

# AUGUST
df_diff_2012_price_aug <- raw_price_USMWst_aug %>%
  arrange(state_alpha, year) %>% 
  group_by(state_alpha) %>%
  mutate(
    soyDiffPctPrice = ((price - lag(price))/lag(price))*100) %>% 
  select(year, state_alpha, state_name, state_fips_code, soyDiffPctPrice) %>% na.omit() %>% 
  mutate(period = "AUGUST")

# ANNUAL
df_diff_2012_price_annual <- raw_price_USMWst_annual %>%
  arrange(state_alpha, year) %>% 
  group_by(state_alpha) %>%
  mutate(
    soyDiffPctPrice = ((price - lag(price))/lag(price))*100) %>% 
  select(year, state_alpha, state_name, state_fips_code, soyDiffPctPrice) %>% na.omit() %>% 
  mutate(period = "ANNUAL")

# rbind differences
prices <- rbind(df_diff_2012_price_aug, df_diff_2012_price_annual)

# add the shapefile and set as 'sf' type
df_price <- prices %>%
  left_join(states, by = c("state_fips_code" = "GEOID")) %>% 
  st_as_sf()

# set limits to the plot - this could be less hard-coded

# plot changes
(p_price <- ggplot(df_price)+
    geom_sf(aes(
      fill = soyDiffPctPrice),
      linewidth = 0.1)+
    theme_bw()+
    facet_wrap(~period)+
    theme(
      plot.title = element_text(hjust = 0.5, size = 14),
      
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.key.width = unit(2, "cm"),
      
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      
      strip.text.x = element_text(size = 11, colour = "black"),
      legend.text = element_text(size = 10),
      
    )+
    scale_fill_distiller(
      palette = "Greens", 
      direction = 1
    ) + 
    labs(
      title = "Change (%) in Prices from 2011-2012" ,
      fill = "" # "% Change"
    )+
    geom_sf(data = states_mw, fill = NA, color = "gray55", size = 0.25)
)

# save figure
ggsave(paste0("../Figures/USMW_CountyDiffs/",
              "gg_price_annual_facet.png"), 
       plot = p_price, dpi = 300)
