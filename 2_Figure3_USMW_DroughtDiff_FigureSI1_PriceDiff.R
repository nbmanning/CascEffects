# Section 00: Script Details ----------------

# Title: 2_Figure3_USMW_DroughtDiff_FigureSI1_PriceDiff.R
# Author: Nick Manning
# Purpose: Import US yield, production, and area harvested data from USDA and
# do some EDA and mapping to determine the best way to display this data
# Creation Date: 10/18/23
# Last Updated: December 2025

# Requires: 
## ../Data_Source/USyielddata.csv
### USDA Yield Data per county - available from USDA NASS QuickStat: https://quickstats.nass.usda.gov/
  

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #  


# 0: Import Libraries & Set Constants ---------------------------------------
rm(list = ls())

# libraries
library(dplyr)
library(ggplot2)
library(sf) 
library(stringr) # for str_pad() & str_to_title()
library(tigris) # for FIPS database
library(fiftystater) # for us state map; note: had to install from GitHub for my current R, version 4.2.2 
library(RColorBrewer)
library(classInt) # for mapping and setting breaks 
library(reshape2)
library(patchwork)

### Constants ###

## key for tidyusda() - should be personal for each user
usda_key <- "34BD2DD3-9049-37A1-BC2C-D8A967E25E42"

## paths and files 
folder_source <- "../Data_Source/"

## states of interest 
mw_st_abv <- c("ND", "SD", "NE","KS", "MO", "IA", "MN", "WI", "IL", "IN", "OH", "MI")

mw_st_full = c("NORTH DAKOTA", "SOUTH DAKOTA", "NEBRASKA", "KANSAS", "MISSOURI",
               "IOWA", "MINNESOTA", "WISCONSIN", "ILLINOIS", "INDIANA", "OHIO", "MICHIGAN")

mw_st_tigris <- str_to_title(mw_st_full)


# 1: USDA Yield Data -----------------------------------

## 1.1: Import and Tidy Data ---------------------

# import data
getwd()
df_raw <- read.csv(paste0(folder_source, "USyieldData.csv"))
df <- df_raw

# explore data
names(df)

# set "." to NA and check how many, then remove FIPS with no long/lat
df[df == "."] <- NA
sum(is.na(df$longitude))

# drop NA's in long/lat
df <- df %>% subset(!is.na(longitude))

# make it so all FIPS are 5 characters with 0 leading any 4 digit FIPS
df$fips <- as.character(df$fips)
df$fips <- str_pad(df$fips, 5, pad = "0")

# download us counties shapefile to merge here (only have points as of now)
counties <- tigris::counties(
  state = mw_st_tigris,
  # NOTE: comment out cb and resolution below for finer resolution
  cb = T,
  resolution = "5m" # could be "500k" or "20m"
) %>% select("GEOID", "STUSPS", "NAME")

# add the shapefile and set as 'sf' type
df <- df %>% left_join(counties, by = c("fips" = "GEOID"))
df <- st_as_sf(df)

# filter to only MW states
df <- df %>% filter(STUSPS %in% mw_st_abv)
colnames(df)

# transform all to numeric, they were in character
df <- df %>% rename("state" = "STUSPS", "name" = "NAME") %>% 
  mutate_at(c(
    "soybeansArea",          "soybeansAreaIrrig",     "soybeansAreaNonIrrig", 
    "soybeansAreaPlanted",   "soybeansPrice",         "soybeansProd",         
    "soybeansProdIrrig",     "soybeansProdNonIrrig",  "soybeansYield",        
    "soybeansYieldIrrig",    "soybeansYieldNonIrrig"), as.numeric)

# select only area harvested, production, and yield
df2 <- df %>% 
  select("year", "state", "name", "fips", 
         "soybeansArea", "soybeansAreaPlanted", "soybeansProd", "soybeansYield", "soybeansPrice") %>% 
  rename(
    "soybeansAreaHarvested" = "soybeansArea")

# test to see if price is viable to plot at the county level -- it is not
df_t <- df2 %>% filter(year == 2011 | year == 2012) %>% filter(state %in% mw_st_abv)
sum(is.na(df_t$soybeansPrice))


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

## 1.3: Plot Inset --------------

## Dissolve & Plot ##
# dissolve by whether each polygon is part of central area
states_conus_diss <- states_conus %>% group_by(LSAD) %>% summarize() 
states_mw_diss <- states_mw %>% group_by(LSAD) %>% summarize()

# plot to test (and to use for inset map)
ggplot(states_mw_diss)+
  geom_sf(fill = "pink", color = "gray")+
  geom_sf(data = states_conus_diss, fill = NA, color = "gray11", size = 0.25)+
  theme_void()

ggsave("../Figures/USMW_CountyDiffs/usmw_inset.png")


## 1.4 Mapping Changes in Yield, Prod, Area ---------

# set constants and filter data 
yr_one <- 2012
yr_min <- 2007
yr_range <- 2010:2013

col_border <- "lightgray"

# get one year and filter to 2007 and beyond (for 9 facets)
df2 <- df2 %>% filter(year >= yr_min)

df2_yr <- df2 %>% filter(year == yr_one)

df2_range <- df2 %>% filter(year %in% yr_range)


## 1.5: Plot Changes from 2011-2012 in corn, soy, corn/soy --------

### 1.5.1 Calculate Changes ----
df_diff <- df2_range %>% 
  group_by(state, name) %>%
  mutate(
    
    # Percent Change = ( (2012value - 2011value) / 2011value ) *100
    
    soyDiffPctYield = ((soybeansYield - lag(soybeansYield))/lag(soybeansYield))*100,
    soyDiffPctProduction = ((soybeansProd - lag(soybeansProd))/lag(soybeansProd))*100,
    soyDiffPctAreaHarvested = ((soybeansAreaHarvested - lag(soybeansAreaHarvested))/lag(soybeansAreaHarvested))*100,
    soyDiffPctAreaPlanted = ((soybeansAreaPlanted - lag(soybeansAreaPlanted))/lag(soybeansAreaPlanted))*100
  
    ) %>% 
  select(year, state, name, 
         soyDiffPctYield, soyDiffPctProduction, soyDiffPctAreaHarvested, soyDiffPctAreaPlanted#,
  ) %>% na.omit()


# get just the changes from 2011-2012
df_diff_2012 <- df_diff %>% filter(year == yr_one)
  
# Create Fxn
F_plot_gg_diffpct <- function(data, var, yr){
  
  # filter and set variables
  data <- data %>% filter(year == yr) %>% na.omit()
  y_var <- as.character(var)
  
  # get "soy" from "soyDiffPctYield
  y_var_crop <- as.character(sub("Diff.*","",y_var))
  
  # get "Yield" from soyDiffPctYield
  y_var_metric <- as.character(sub(".*Pct", "", y_var))
  
  # set classes
  class <- classIntervals(
    data[[y_var]], 
    fixedBreaks = 
      c(-100, -80, -60, -40, -20, -1,1, 20, 40, 60, 80, 100),  
    style = "fixed")
  
  # set new column with the breaks for mapping
  data$DiffCut <- cut(data[[y_var]], class$brks, include.lowest = T)
  
  # plot changes
  p <- ggplot(data)+
    geom_sf(aes(fill = DiffCut), 
            #col = "gray66", 
            col = "lightpink", 
            linewidth = 0.1)+
    theme_bw()+
    scale_fill_brewer(palette = "PiYG", direction = 1, drop = F)+
    labs(
      title = paste0(
        "% Change in ", str_to_title(y_var_metric), 
        " (", yr-1, "-", yr, ")"),
      fill = "% Change")+
    theme(
      axis.ticks = element_blank(),
      axis.text= element_blank(), 
      plot.title = element_text(hjust = 0.5)
    )+
    geom_sf(data = states_mw, fill = NA, color = "gray55", size = 0.25)
  
  # save figure
  ggsave(paste0("../Figures/USMW_CountyDiffs/",
                "gg_", y_var, "_", yr-1, yr,
                ".png"), 
         plot = p)
  
  return(p)
  
}

# get each plot to add to a grid later
(p_soy_yield <- F_plot_gg_diffpct(df_diff, "soyDiffPctYield", yr_one))
(p_soy_prod <- F_plot_gg_diffpct(df_diff, "soyDiffPctProduction", yr_one))

# 2: Arrange Plots ------------

# Plot on a grid - add legend after as three have NA's
p1 <- p_soy_yield + p_soy_prod #+ p_corn_yield + p_corn_prod

p2 <- p1 + plot_annotation(tag_levels = 'a', ) & 
  theme(plot.tag = element_text(size = 20)) 

p2 <- p2 +
  plot_layout(nrow = 2, guides = "collect") & 
  theme(legend.position = "none",
        plot.title = element_blank()) 
p2

ggsave(filename = "../Figures/USMW_CountyDiffs/USMW_Change_Soy_nolegend.png",
       p2, height = 6, width = 5, 
       dpi = 300)  


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
      title = "% Change in Prices from 2011-2012" ,
      fill = "" # "% Change"
      )+
    geom_sf(data = states_mw, fill = NA, color = "gray55", size = 0.25)
  )

# save figure
ggsave(paste0("../Figures/",
              "gg_price_annual_facet.png"), 
       plot = p_price, dpi = 300)