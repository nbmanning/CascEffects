# Section 00: Script Details ----------------

# title: 3_FigureS2_MapB_Conversion.R
# author: Nick Manning
# purpose: Re-use code from SIMPLE-G to plot certain land transititon categories from mapbiomas
# created date:
# last edit date: 12/19/24

# REQUIRES: 
## "mapb_col8_clean_long.Rdata" from 1_Data_Import_Clean.R
## "muni_codes_cerr.Rdata" from 00_GetShapefiles.R

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# 0) Set Paths & Constants & Libraries -------
rm(list = ls())

## Libraries
library(ggplot2)
library(dplyr)
library(stringi) # removing accents

## Paths
folder_source <- "../Data_Source/"
folder_derived <- "../Data_Derived/"

## Constants
yr_range <- 2008:2018

## Load Clean & Long Data - can skip to here once you've run code 1_Data_Import_Clean at least once ------
load(file = paste0(folder_derived, "mapb_col8_clean_long.Rdata"))

# load municipality codes for Cerrado 
load(file = paste0(folder_derived, "muni_codes_cerr.Rdata"))


# 2: Filter & Calculate Stats ---------

## 2.1: Filter ------

df_g <- df

# filter to Level 4 data in Cerrado
# matches script 1 for redundancy and a double-check
df_g <- df_g %>% 
  
  # filter to municipalities from spatial intersection instead of biome == Cerrado to be consistent with previous maps
  filter(geocode %in% muni_codes_cerr) %>%
  
  # important as this makes sure we only have things that changed 
  filter(to_level_4 != from_level_4) %>%
  
  # change "soy beans" to "soybeans"
  mutate(from_level_4 = str_replace_all(from_level_4, "Soy Beans", "Soybeans")) %>% 
  mutate(to_level_4 = str_replace_all(to_level_4, "Soy Beans", "Soybeans")) %>% 
  
  # Create from-to column
  mutate(fromto = paste0(from_level_4, " to ", to_level_4)) 

# Save
# save(df_g, file = paste0(folder_derived, "mapb_col8_clean_long_cerr_nosamefromto.Rdata"))

## 2.2: Calc. Stats ------

### 2.2.1: Name Classes #####
classes_lvl_4 <- c("Pasture", "Soybeans", "Other Temporary Crops",
  "Forest Plantation", "Mosaic of Agriculture and Pasture",
  "Sugar Cane", "Rice", "Cotton","Other Perennial Crops",
  "Coffe", "Citrus")


names_fromto <-c(
  "Forest Formation to Pasture",
  "Forest Formation to Soybeans",
  "Pasture to Soybeans",
  "Savanna Formation to Pasture",
  "Savanna Formation to Soybeans"
)

### 2.2.2: Calculate ------

# get only the specifically mentioned "from-to" categories of interest
df_g_specific_fromto <- df_g %>% 
  filter(fromto %in% names_fromto) %>% 
  group_by(year, fromto) %>% 
  na.omit() %>% 
  summarise(total_trans = sum(ha)) %>% 
  filter(year %in% yr_range) 

# create intervals
df_g_specific_fromto <- df_g_specific_fromto %>% 
  mutate(years = paste0(
    as.numeric(year)-1, "-", as.numeric(year)
  ))

# plot 
ggplot(df_g_specific_fromto, aes(x = years, y = total_trans / 1000000, group = fromto, color = fromto, shape = fromto)) +
  geom_line() +
  geom_point(fill = "white", size = 2) +
  scale_shape_manual(values = c(16, 17, 15, 18, 8)) +  # 5 distinct shapes
  xlab("") +
  labs(
    title = "Cerrado Annual Land Conversion",
    #subtitle = "Data Source: MapBiomas",
    #caption = "Dotted line shows the transitions from 2012 to 2013",
    y = "Land Change from Previous Year (Mha)",
    color = "From-To Transitions",
    shape = "From-To Transitions"
  ) +
  geom_vline(xintercept = "2012-2013", color = "red", linetype = "dotted", linewidth = 0.5) +
  theme_bw() +
  theme(
    legend.title = element_blank(),
    legend.text = element_text(size = 12, hjust = 0.5),
    legend.position = "top",
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 11, angle = 20, vjust = 0.5),
    plot.title = element_text(size = 16, hjust = 0.5),
    #plot.caption = element_text(hjust=0),
    plot.subtitle = element_text(size = 14, hjust = 0.5)
  ) +
  guides(
    color = guide_legend(nrow = 2), # Arrange legend items into 2 rows
    shape = guide_legend(nrow = 2)
  )

# save
ggsave(
  filename = "../Figures/mapb_LandTrans_20072018.png",
  width = 10,
  height = 4
)