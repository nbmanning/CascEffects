# Title: 3_extra_InsetMaps.R
# Author: Nick Manning
# Purpose: Create basic maps & inset maps in one script 
# Creation Date: 12/15/25
# Last Updated: December 2025

# Links: 

# Requires: 

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #  


# 0: Import Libraries & Set Constants ---------------------------------------
rm(list = ls())

# libraries
library(dplyr)
library(ggplot2)
library(sf) 
#library(stringr) # for str_pad() & str_to_title()
library(tigris) # for FIPS database
library(fiftystater) # for us state map; note: had to install from GitHub for my current R, version 4.2.2 
library(RColorBrewer)
#library(classInt) # for mapping and setting breaks 
#library(reshape2)
library(geobr)

### Constants ###

# ## key for tidyusda() - should be personal for each user
# usda_key <- "34BD2DD3-9049-37A1-BC2C-D8A967E25E42"
# 
# ## paths and files 
# file_path <- "../Data_Source/"

## states of interest 
mw_st_abv <- c("ND", "SD", "NE","KS", "MO", "IA", "MN", "WI", "IL", "IN", "OH", "MI")

# mw_st_full = c("NORTH DAKOTA", "SOUTH DAKOTA", "NEBRASKA", "KANSAS", "MISSOURI",
#                "IOWA", "MINNESOTA", "WISCONSIN", "ILLINOIS", "INDIANA", "OHIO", "MICHIGAN")

#mw_st_tigris <- str_to_title(mw_st_full)

color_us <- "darkgoldenrod"
color_br <- "dodgerblue4"

# 1: Plot USA Insets -------
## 1.2: Load Base US Spatial Data --------------

# get all states 
states <- states(cb = TRUE)

## US-MW ##
# filter to US-MW 
states_mw <- states %>%
  
  filter(STUSPS %in% mw_st_abv)

# get US-MW Counties 
# NOTE: state == The two-digit FIPS code (string) of the state you want, or a vector of codes if you want multiple states. Can also be state name or state abbreviation.
#states_counties <- counties(state = mw_st_abv, cb = FALSE, resolution = "500k", year = NULL)

## CONUS ##
# get conus 
states_conus <- states %>% filter(!STUSPS %in% c("HI","AK", "PR", "VI", "MP", "AS", "GU"))

## 1.3: Plot USA Inset --------------

## Dissolve & Plot ##
# dissolve by whether each polygon is part of central area
states_conus_diss <- states_conus %>% group_by(LSAD) %>% summarize() 
states_mw_diss <- states_mw %>% group_by(LSAD) %>% summarize()

### 1.3.1: Plot Basic Inset --------
ggplot(states_mw_diss)+
  geom_sf(fill = color_us, color = color_us)+
  geom_sf(data = states_conus_diss, fill = NA, color = "gray30", lwd = 2.0)+
#  coord_sf(default_crs = sf::st_crs(4326)) +
  coord_sf(crs = "EPSG:2163") +
  theme_void()

ggsave("../Figures/_inset_usmw_basic.png")

# USA
ggplot(states_conus_diss)+
  geom_sf(fill = color_us, color = color_us)+
  coord_sf(crs = "EPSG:2163") +
  #geom_sf(data = states_conus_diss, fill = NA, color = "gray30", lwd = 2.0)+
  theme_void()

ggsave("../Figures/_inset_us_basic.png")

# 1: Plot Brazil Insets -------

## 1.1: Load Shapefiles ----
### 4.2.1: Load Spatial Data ------
# Load spatial data for states and cerrado
shp_br <- read_country(
  year = 2019, 
  simplified = T
)

shp_br_cerr <- read_biomes(
  year = 2019,
  simplified = T,
  showProgress = T
) %>% dplyr::filter(name_biome == "Cerrado")

# shp_cerr_states <- read_state(
#   year = 2019, 
#   simplified = T,
#   showProgress = T 
# ) %>% 
#   dplyr::filter(abbrev_state %in% c(
#     "BA", "DF", "GO", "MA", "MT", "MS", "MG", "PR", "PI", "SP", "TO"
#   ))

## 1.2: Create Insets ------
(brcerr_inset_basic <- ggplot()+
   geom_sf(data = shp_br, color = "gray40", fill = "transparent", lwd = 2.0)+
   geom_sf(data = shp_br_cerr, color = color_br, fill = color_br, lwd = 0.5)+
   coord_sf(crs = "EPSG:5880") +
   #geom_sf(data = shp_cerr_states, color = "gray20", fill = "transparent", lwd = 0.2)+
   # geom_sf_text(data = shp_cerr_states %>% filter(abbrev_state != "DF"), 
   #              aes(label = abbrev_state), size = 3, color = "gray10") +  # Add state labels
   # geom_sf_text(data = shp_cerr_states %>% filter(abbrev_state == "DF"), 
   #              aes(label = abbrev_state), size = 1, color = "gray10") +  # Add state labels
   theme_void()
)

ggsave("../Figures/_inset_brcerr_basic.png", brcerr_inset_basic,
       dpi = 300, width = 5, height = 5)

## Brazil 
(brcerr_inset_basic <- ggplot()+
    geom_sf(data = shp_br, color = color_br, fill = color_br, lwd = 2.0)+
    coord_sf(crs = "EPSG:5880") +
    
    #geom_sf(data = shp_br_cerr, color = "darkblue", fill = "darkblue", lwd = 0.5)+
    #geom_sf(data = shp_cerr_states, color = "gray20", fill = "transparent", lwd = 0.2)+
    # geom_sf_text(data = shp_cerr_states %>% filter(abbrev_state != "DF"), 
    #              aes(label = abbrev_state), size = 3, color = "gray10") +  # Add state labels
    # geom_sf_text(data = shp_cerr_states %>% filter(abbrev_state == "DF"), 
    #              aes(label = abbrev_state), size = 1, color = "gray10") +  # Add state labels
    theme_void()
)

ggsave("../Figures/_inset_br_basic.png", brcerr_inset_basic,
       dpi = 300, width = 5, height = 5)

# GRAVEYARD: from Other scripts -------

## from CerradoMuniDiffs ------
### 4.2.2: Create Inset Map ----

(br_inset <- ggplot()+
   geom_sf(data = shp_br, color = "gray40", fill = "transparent", lwd = 0.1)+
   geom_sf(data = shp_br_cerr, color = "lightgreen", fill = "lightgreen", lwd = 0.5)+
   geom_sf(data = shp_cerr_states, color = "gray20", fill = "transparent", lwd = 0.2)+
   # geom_sf_text(data = shp_cerr_states %>% filter(abbrev_state != "DF"), 
   #              aes(label = abbrev_state), size = 3, color = "gray10") +  # Add state labels
   # geom_sf_text(data = shp_cerr_states %>% filter(abbrev_state == "DF"), 
   #              aes(label = abbrev_state), size = 1, color = "gray10") +  # Add state labels
   theme_void()
)

ggsave("../Figures/CerradoMuni/inset.png", br_inset,
       dpi = 300, width = 5, height = 5)

### 4.2.2: Create Basic Inset Map ---------
(br_inset_basic <- ggplot()+
   geom_sf(data = shp_br, color = "gray40", fill = "transparent", lwd = 2.0)+
   geom_sf(data = shp_br_cerr, color = color_br, fill = color_br, lwd = 0.5)+
   #geom_sf(data = shp_cerr_states, color = "gray20", fill = "transparent", lwd = 0.2)+
   # geom_sf_text(data = shp_cerr_states %>% filter(abbrev_state != "DF"), 
   #              aes(label = abbrev_state), size = 3, color = "gray10") +  # Add state labels
   # geom_sf_text(data = shp_cerr_states %>% filter(abbrev_state == "DF"), 
   #              aes(label = abbrev_state), size = 1, color = "gray10") +  # Add state labels
   theme_void()
)

ggsave("../Figures/_inset_basic.png", br_inset_basic,
       dpi = 300, width = 5, height = 5)

## from 2012DroughtDiffs ---------
### 1.3.1: Plot Basic Inset --------
ggplot(states_mw_diss)+
  geom_sf(fill = color_us, color = color_us)+
  geom_sf(data = states_conus_diss, fill = NA, color = "gray30", lwd = 2.0)+
  theme_void()

ggsave("../Figures/_inset_usmw_basic.png")

# USA
ggplot(states_conus_diss)+
  geom_sf(fill = color_us, color = color_us)+
  #geom_sf(data = states_conus_diss, fill = NA, color = "gray30", lwd = 2.0)+
  theme_void()
ggsave("../Figures/_inset_us_basic.png")
