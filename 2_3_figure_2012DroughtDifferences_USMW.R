# Title: 2_3_Figure_2012DroughtDifferences_USMW.R
# Author: Nick Manning
# Purpose: Import US yield, production, and area harvested data from USDA and
# do some EDA and mapping to determine the best way to display this data
# Creation Date: 10/18/23
# Last Updated: October 2023

# Links: 

# Requires: 
## CSV of yield data (Data_Source\USyielddata.csv)

# Steps: 
# 0: Import Libraries & Set Constants

# 1: USDA Data
## This section imports and tidies the data, as well as produced initial maps of 
## 2011 to 2012 change 


# 2: Initial EDA
## This section plots the data in ways outside of maps 


# 3 (FUTURE): Use tidyUSDA to get data & plot (get inspo from 1_data_import.R)
## This section grabs data from the 'tidyUSDA' package instead of importing it from an external CSV

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

#library(lubridate)
#library(terra)

# constants

## paths and files 
file_path <- "../Data_Source/"

## states of interest 
mw_st_abv <- c("ND", "SD", "NE","KS", "MO", "IA", "MN", "WI", "IL", "IN", "OH", "MI")

mw_st_full = c("NORTH DAKOTA", "SOUTH DAKOTA", "NEBRASKA", "KANSAS", "MISSOURI",
               "IOWA", "MINNESOTA", "WISCONSIN", "ILLINOIS", "INDIANA", "OHIO", "MICHIGAN")

mw_st_tigris <- str_to_title(mw_st_full)


# 1: USDA Data from Iman -----------------------------------

## 1.1: Import and Tidy Data ---------------------

# import data
getwd()
df_raw <- read.csv(paste0(file_path, "USyieldData.csv"))
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
  mutate_at(c("cornArea",              "cornAreaIrrig",        
              "cornAreaNonIrrig",      "cornAreaPlanted",       "cornPrice",            
              "cornProd",              "cornProdIrrig",         "cornProdNonIrrig",     
              "cornYield",             "cornYieldIrrig",        "cornYieldNonIrrig",    
              "soybeansArea",          "soybeansAreaIrrig",     "soybeansAreaNonIrrig", 
              "soybeansAreaPlanted",   "soybeansPrice",         "soybeansProd",         
              "soybeansProdIrrig",     "soybeansProdNonIrrig",  "soybeansYield",        
              "soybeansYieldIrrig",    "soybeansYieldNonIrrig"), as.numeric)

# select only area harvested, production, and yield
df2 <- df %>% 
  select("year", "state", "name", "fips", 
         "cornArea", "cornAreaPlanted", "cornProd", "cornYield",
         "soybeansArea", "soybeansAreaPlanted", "soybeansProd", "soybeansYield") %>% 
  rename("cornAreaHarvested" = "cornArea",
         "soybeansAreaHarvested" = "soybeansArea")

# create corn/soy data by combining production and area harvest, then calculating yield 
df2 <- df2 %>% 
  mutate(
    cornSoyAreaHarvested = cornAreaHarvested + cornAreaHarvested,
    cornSoyAreaPlanted = cornAreaPlanted + soybeansAreaPlanted,
    cornSoyProd = cornProd + soybeansProd,
    cornSoyYield = cornSoyProd/cornSoyAreaPlanted
  )


## 1.2: Initial Mapping --------------
# load mw states for outline
# Stack Exchange link: https://stackoverflow.com/questions/50499363/how-to-draw-the-outline-of-multiple-us-states-in-r
# fifty_states <- fifty_states
# 
# # get sf of all fifty states
# sf_fifty <- sf::st_as_sf(fifty_states, coords = c("long", "lat")) %>% 
#   group_by(id, piece) %>% 
#   summarize(do_union = FALSE) %>%
#   st_cast("POLYGON") %>% 
#   ungroup()
# 
# # filter to US-MW
# midwest <- sf_fifty %>%
#   filter(id %in% tolower(mw_st_full))
# 
# # set CRS 
# midwest <- st_set_crs(midwest, 4269)
# 
# # get map of county outlines 
# ggplot(df2)+
#   geom_sf(aes(color = state))+
#   theme_bw()+
#   labs(title = "Counties in US-Midwest", color = "State")
# 
# # save 
# # ggsave("../Figures/USMW_CountyDiffs/counties.png")


## 1.3 Mapping Yield, Prod, Area ---------

# set constants and filter data 
yr_one <- 2012
yr_min <- 2007
yr_range <- 2010:2013

col_border <- "lightgray"

# get one year and filter to 2007 and beyond (for 9 facets)
df2 <- df2 %>% filter(year >= yr_min)

df2_yr <- df2 %>% filter(year == yr_one)

df2_range <- df2 %>% filter(year %in% yr_range)

### 1.3.1: Map Single Year Single Var --------

# Function for one year of interest and one variable
F_plot_single <- function(data, var, yr){
  
  y_var <- as.character(var)
  data <- data %>% filter(year == yr)
  
  p <- ggplot(data)+
    geom_sf(# %>% filter(year == 2010), 
      aes(fill = .data[[y_var]]), 
      #col = "lightgrey", 
      lwd = 0, col = NA)+
    #geom_sf(data = midwest, col = col_border, lwd = 1, fill = NA)+
    theme_bw()+
    theme(
      # #remove ticks
      # axis.ticks = element_blank(),
      # axis.text= element_blank(), 
      # axis.line = element_blank(),
      # panel.border = element_blank(),
      # # make transparent
      # panel.grid.major = element_line(color='transparent'),
      # panel.grid.minor = element_line(color='transparent'),
      # panel.background = element_blank(),
      # plot.background = element_rect(fill = "transparent",color='transparent'),
      
      plot.title = element_text(size=18, hjust = 0.5),
      legend.position="bottom", legend.box = "horizontal", 
      #legend.justification='right',
      #legend.title = element_text(size=13), #change legend title font size
      #legend.text = element_text(size=11)
    )+
    scale_fill_distiller(palette = "Greens", direction = 1
    )+
    labs(title = paste(yr, "US-MW", var))
  
  ggsave(paste0("../Figures/USMW_CountyDiffs/",
                y_var, "_", yr,
                ".png"), 
         plot = p)
  
  return(p)
  
}

### 1.3.1: Apply Single Year Single Var ------
F_plot_single(df2_range, "cornSoyProd", yr = yr_one)

# get prod, area, yield vars
var_names <- c("cornAreaHarvested", "cornProd", "cornYield", "cornAreaPlanted", 
               "cornAreaHarvested", "soybeansProd",  "soybeansYield", "soybeansAreaPlanted",
               "cornSoyAreaHarvested", "cornSoyProd", "cornSoyYield", "cornSoyAreaPlanted") 

# # # UNCOMMENT TO RUN OVER ALL VARIABLES # # #
##### TO-DO: Fix Legend Labels and Title ----
#lapply(var_names, F_plot_single, data = df2_range, yr = 2010)




## 1.4: Plot Changes from 2011-2012 in corn, soy, corn/soy --------

### 1.4.1 Calculate Changes ----
df_diff <- df2_range %>% 
  group_by(state, name) %>%
  mutate(
    # Difference = 2012 - 2011 per county
    # cornSoyDiffYield = cornSoyYield - lag(cornSoyYield),
    # cornSoyDiffProd = cornSoyProd - lag(cornSoyProd),
    # cornSoyDiffArea = cornSoyAreaHarvested - lag(cornSoyAreaHarvested),
    
    # Percent Change = ( (2012value - 2011value) / 2011value ) *100
    
    # cornSoy
    cornSoyDiffPctYield = ((cornSoyYield - lag(cornSoyYield))/lag(cornSoyYield))*100,
    cornSoyDiffPctProduction = ((cornSoyProd - lag(cornSoyProd))/lag(cornSoyProd))*100,
    cornSoyDiffPctAreaHarvested = ((cornSoyAreaHarvested - lag(cornSoyAreaHarvested))/lag(cornSoyAreaHarvested))*100,
    cornSoyDiffPctAreaPlanted = ((cornSoyAreaPlanted - lag(cornSoyAreaPlanted))/lag(cornSoyAreaPlanted))*100,
    
    # Corn
    cornDiffPctYield = ((cornYield - lag(cornYield))/lag(cornYield))*100,
    cornDiffPctProduction = ((cornProd - lag(cornProd))/lag(cornProd))*100,
    cornDiffPctAreaHarvested = ((cornAreaHarvested - lag(cornAreaHarvested))/lag(cornAreaHarvested))*100,
    cornDiffPctAreaPlanted = ((cornAreaPlanted - lag(cornAreaPlanted))/lag(cornAreaPlanted))*100,
    
    # Soy
    soyDiffPctYield = ((soybeansYield - lag(soybeansYield))/lag(soybeansYield))*100,
    soyDiffPctProduction = ((soybeansProd - lag(soybeansProd))/lag(soybeansProd))*100,
    soyDiffPctAreaHarvested = ((soybeansAreaHarvested - lag(soybeansAreaHarvested))/lag(soybeansAreaHarvested))*100,
    soyDiffPctAreaPlanted = ((soybeansAreaPlanted - lag(soybeansAreaPlanted))/lag(soybeansAreaPlanted))*100
  ) %>% 
  select(year, state, name, 
         #cornSoyDiffYield, cornSoyDiffProduction, cornSoyDiffArea, 
         cornDiffPctYield, cornDiffPctProduction, cornDiffPctAreaHarvested, cornDiffPctAreaPlanted,
         soyDiffPctYield, soyDiffPctProduction, soyDiffPctAreaHarvested, soyDiffPctAreaPlanted,
         cornSoyDiffPctYield, cornSoyDiffPctProduction, cornSoyDiffPctAreaHarvested, cornSoyDiffPctAreaPlanted
  ) %>% na.omit()


# get just the changes from 2011-2012
df_diff_2012 <- df_diff %>% filter(year == yr_one)

# mess with str_split
# help: https://statisticsglobe.com/extract-substring-before-or-after-pattern-in-r
# t <- "soyDiffPctYield"
# # get all before Diff
# t1 <- sub("Diff.*","",t)
# t1
# t2 <- sub(".*Pct", "", t)
# t2

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
  #class <- classIntervals(round(data[[y_var]]), n = 11, style = "fisher")
  
  class <- classIntervals(
    data[[y_var]], 
    fixedBreaks = 
      c(-100, -80, -60, -40, -20, -1,1, 20, 40, 60, 80, 100),  
    style = "fixed")
  
  # set new column with the breaks for mapping
  # data <- data %>%
  #   mutate(DiffCut = cut(y_var, class$brks, include.lowest = T))
  data$DiffCut <- cut(data[[y_var]], class$brks, include.lowest = T)
  
  # plot changes
  p <- ggplot(data)+
    geom_sf(aes(fill = DiffCut), col = "darkgray", linewidth = 0.1)+
    theme_bw()+
    scale_fill_brewer(palette = "PiYG", direction = 1, drop = F)+
    labs(
      title = paste("% Change in",
                    str_to_title(paste(
                      y_var_crop, y_var_metric,
                      "from", yr-1, "to", yr))),
      fill = "% Change"
      # fill = str_to_title(paste(
      #   y_var_metric,
      #   "% Change"))
    )
  #lines(shp_br_cerr_states, lwd = 0.8, lty = 3, col = "darkgray")
  
  # save figure
  ggsave(paste0("../Figures/USMW_CountyDiffs/",
                "gg_", y_var, "_", yr-1, yr,
                ".png"), 
         plot = p)
  
  return(p)
  
}

# test
(p_soy_yield <- F_plot_gg_diffpct(df_diff, "soyDiffPctYield", yr_one))

# get each plot to add to a grid later
(p_soy_yield <- F_plot_gg_diffpct(df_diff, "soyDiffPctYield", yr_one))
(p_soy_prod <- F_plot_gg_diffpct(df_diff, "soyDiffPctProduction", yr_one))

(p_corn_yield <- F_plot_gg_diffpct(df_diff, "cornDiffPctYield", yr_one))
(p_corn_prod <- F_plot_gg_diffpct(df_diff, "cornDiffPctProduction", yr_one))


# 2: Arrange Plots ------------
library(patchwork)

# Plot on a grid - add legend after as three have NA's
p1 <- p_soy_yield + p_soy_prod + p_corn_yield + p_corn_prod

p2 <- p1 + 
  plot_layout(nrow = 2, guides = "collect") & 
  theme(legend.position = "none")

p2

ggsave(filename = "../Figures/USMW_CountyDiffs/USMW_Change_CornAndSoy_nolegend.png",
       p2, height = 12, width = 15, 
       dpi = 300)  





### IN THE FUTURE! Spend an hour trying this! ###########

## 2.1 Yield 
#tidy_yield <- 
## 2.2 Area Harvested
## 2.3 Production










# graveyard -------------------------------------------

## FACET WRAP -------------------
### 1.3.2: facet_wrap by year ---------

# facet_wrap by year

# plotting function
F_plot_facet <- function(data, var){
  y_var <- as.character(var)
  p <- ggplot(data)+
    geom_sf(# %>% filter(year == 2010), 
      aes(fill = .data[[y_var]]), 
      #col = "lightgrey", 
      lwd = 0, col = NA)+
    #geom_sf(data = midwest, col = col_border, lwd = 1, fill = NA)+
    theme_bw()+
    theme(
      # #remove ticks
      # axis.ticks = element_blank(),
      # axis.text= element_blank(), 
      # axis.line = element_blank(),
      # panel.border = element_blank(),
      # # make transparent
      # panel.grid.major = element_line(color='transparent'),
      # panel.grid.minor = element_line(color='transparent'),
      # panel.background = element_blank(),
      # plot.background = element_rect(fill = "transparent",color='transparent'),
      
      plot.title = element_text(size=18, hjust = 0.5),
      legend.position="bottom", legend.box = "horizontal", 
      #legend.justification='right',
      #legend.title = element_text(size=13), #change legend title font size
      #legend.text = element_text(size=11)
    )+
    scale_fill_distiller(palette = "Greens", direction = 1
    )+
    labs(title = paste("US-MW", var))+
    facet_wrap("year")
  
  ggsave(paste0("../Figures/USMW_CountyDiffs/",
                y_var, "_",
                min(data$year),max(data$year),
                ".png"), 
         plot = p)
  
  return(p)
  
}

### 1.3.2: Apply Facet Wrap ------
F_plot_facet(df2_range, "soybeansProd")

# get prod, area, yield vars
var_names <- c("cornAreaHarvested", "cornProd", "cornYield", "cornAreaPlanted",
               "cornAreaHarvested", "soybeansProd",  "soybeansYield", "soybeansAreaPlanted",
               "cornSoyAreaHarvested", "cornSoyProd", "cornSoyYield", "cornSoyAreaPlanted"
) 

# # # UNCOMMENT TO RUN OVER ALL VARIABLES # # #
# lapply(var_names, F_plot_facet, data = df2_range)


### 1.4.2 Plot using facet_wrap ----
F_plot_facet(df_diff, "cornSoyDiffPctYield")
