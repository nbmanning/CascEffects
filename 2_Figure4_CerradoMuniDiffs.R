# Section 00: Script Details ----------------

# title: 2_Figure4_CerradoMuniDiffs.R
# author: Nick Manning
# purpose: Get Change in Production and Value at the muncipality level 

# Notes:
## Use of USDA QuickStats requires a Key

# Created: 3/11/24
# Last Edited: Dec 2025

# REQUIRES:
## ../Data_Source/soymaize_2010_2022_andres/soy_Brazil_2010_2022t 
### Soybean Spatial Data from SIDRA-PAM: https://sidra.ibge.gov.br/pesquisa/pam/tabelas

## ../Data_Source/soymaize_2010_2022_andres/muni_codes_cerr.Rdata 
### codes for municipalities in the Cerrado from 00_GetShapefiles.R.R

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# 0: Load Libraries & Set Constants and Paths --------------
rm(list = ls())

## Load Libraries --------
library(ggplot2) # plotting
library(dplyr) # data cleaning
library(tidyr) #pivot_longer
library(sidrar) # download BR data
library(stringr) #str_to_title() function
library(geobr) # get BR shapefiles
library(classInt) # plotting in intervals
library(RColorBrewer)
library(sf)
library(scico) # used for getting the midpoint at 0
library(patchwork) # arranging plots


## Set Constants -------- 
getwd()

folder_source <- "../Data_Source/soymaize_2010_2022_SIDRA_PAM/"
folder_figures <- "../Figures/"

###  Set Years ----------
yr1 <- 2012
yr2 <- 2017

# Set "crop"; stays "soy" for this analysis
crop <- "soy" 


# 1: Import & prep SIDRA PAM data -----------
source_soy <- read_sf(dsn = folder_source, layer = "soy_Brazil_2010_2022t")

## 1.1: Clean soy data (spatial) --------
# get codes with geometry for later join
muni <- source_soy %>% select(CODE, geometry)

# make non-spatial for melting 
soy_df <-  st_drop_geometry(source_soy)
names(soy_df)

# make long
soy_df <- soy_df %>%
  
  # gather data to become long 
  pivot_longer(
    cols = !c("NM_MUN", "SIGLA", "AREA_KM2", "CODE"),
    names_to = "base",
    values_to = "value"
  ) %>% 
  
  # split year - e.g. base == 2010y becomes year == 2010 and stat == "y" 
  mutate(
    year = as.double(substr(base, start = 1, stop = 4)), # needed to make this a double to join
    stat = substr(base, start = 5, stop = 7)) %>% 
  
  # change text to stat it represents 
  mutate(
    stat = case_when(
      stat == "y" ~ "soy_yield",
      stat == "ha" ~ "soy_area",
      stat == "t" ~ "soy_prod"
    ))

# make wide again to be the same format as maize 
soy_df <- soy_df %>%
  # remove the base column with, e.g., "2014ha", to keep "2014" and "ha" instead
  select(!base) %>% 
  # make wide again
  pivot_wider(names_from = stat, values_from = value)

# rejoin to make spatial again
# NOTE: muni is from the source data 
soy_sf <- soy_df %>% left_join(muni)


# 2: Plot Real Data --------

# get one year of real soy/maize data per municip
# sf <- df 
sf <- soy_sf
sf <- st_as_sf(sf)

# filter to just municipalities in the Cerrado by bringing in Cerrado muni codes
load(paste0(folder_source, "muni_codes_cerr.Rdata"))

sf <- sf %>% filter(CODE %in% muni_codes_cerr)

# 3: Calc. Differences with Real Data ------------

## 3.1: Test with Soybeans -----------------
sf_crop <-sf %>% 
  select(NM_MUN, SIGLA, CODE, year, soy_yield, soy_prod, soy_area)

# add function, comment out 
F_calc_diff <- function(data, year1, year2){
  lagtime <- year2 - year1
  newdf <- data %>%
    group_by(CODE) %>%
    mutate(
      # Difference = 2012 - 2011 per state; 
      production_diff = soy_prod - lag(soy_prod, n = lagtime),
      yield_diff = soy_yield - lag(soy_yield, n = lagtime),
      area_diff = soy_area - lag(soy_area, n = lagtime),

      # Change "years" column from "2013 - 2012" to "2011/2012 - 2012/2013"
      years = as.character(paste0(
        year1-1, "/", year1, " - ", year2-1, "/", year2
      ))
      ) %>% 
    # keep only the two years we are interested in 
    filter(year == year1 | year == year2)
  
  # get stats as well
  # print the total change in crop production
  cat("\n\n PROD: % Change \n")
  sum_diff <- sum(newdf$production_diff, na.rm = T)
  sum_old <- sum(newdf[year = year1]$soy_prod)
  print(sum_diff / sum_old *100)

  cat("\n\n AREA: % Change \n")
  sum_diff <- sum(newdf$area_diff, na.rm = T)
  sum_old <- sum(newdf[year = year1]$soy_area)
  print(sum_diff / sum_old *100)  
  
  cat("\n\n YIELD: % Change \n")
  sum_diff <- sum(newdf$yield_diff, na.rm = T)
  sum_old <- sum(newdf[year = year1]$soy_yield)
  print(sum_diff / sum_old *100)
  
  # get only the later year to remove all change NA's since we're only focused on change
  newdf <- newdf %>% filter(year == year2)

}

# Get crop differences
sf_crop_diff <- F_calc_diff(sf_crop, 2012, 2017)

# 4: Get Shapefile & Data ----

## 4.1: Load Spatial Data ------
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

shp_cerr_states <- read_state(
  year = 2019, 
  simplified = T,
  showProgress = T 
) %>% 
  dplyr::filter(abbrev_state %in% c(
    "BA", "DF", "GO", "MA", "MT", "MS", "MG", "PR", "PI", "SP", "TO"
  ))



## 4.2: Run F_calc_diff then rbind then plot in a grid -----

# create three test datasets by running through soy with different years 
t_1213 <- F_calc_diff(sf, 2012, 2013)
t_1215 <- F_calc_diff(sf, 2012, 2015)
t_1217 <- F_calc_diff(sf, 2012, 2017)
t_1222 <- F_calc_diff(sf, 2012, 2022)

# rbind to get one long df 
t <- rbind(
  t_1213, 
  t_1215, 
  t_1217, 
  t_1222
)
names(t_1217)
names(t_1222)
names(t)

F_facet <- function(data, var, units){
  
  ### NOTE: MIGHT NEED TO USE THE GEOBR 'SF' VARIABLE INSTEAD ###
  data <- data %>% rename("geom" = "geometry")

    
  # get titles
  y_var <- as.character(var)
  y_var_title <- str_to_title(sub("*_diff","", y_var))
  
  p <- ggplot(data)+
    geom_sf(aes(fill = !!sym(var), geometry = geom), col = "darkgray", linewidth = 0.02)+
    theme_bw()+
    facet_wrap(~years, nrow = 1)+
    
    # use the custom theme 
    scale_fill_scico(palette = "bam", direction = 1, na.value = "white",
                     midpoint = 0)+
    
    labs(
      fill = paste0(str_to_title(paste(y_var_title, "Change")),"\n",
                    "(", units, ")"),
      x = "",
      y = ""
    )+
    theme(
      legend.text = element_text(size = 12),
      legend.title = element_text(size = 14),
      #theme(strip.clip = "off")+
      strip.text = element_text(size = 15))
  
  # add State & Cerrado outlines  
  p <- p +
    geom_sf(data = shp_cerr_states, color = "gray40", fill = "transparent", lwd = 0.1)#+
    #geom_sf(data = shp_br_cerr, color = "gray10", fill = "transparent", lwd = 0.2)

    

  # save figure
  ggsave(paste0("../Figures/",
                "_gg_", crop, "_facet_", y_var,
                ".png"),
         plot = p,
         width = 12,
         height = 6)
  
  return(p)
  
}

# adjust units; originals are production_diff in tons, yield_diff in kg/ha, and area_diff in ha
t <- t %>% 
  mutate(
    production_diff = production_diff/1000,
    area_diff = area_diff/1000
  )

# 5: Plot & Save -------

# NOTE: Try not to plot p1 in the R window - it is a huge element and will likely freeze R

# plot each then arrange on top of each other 
pf_prod <- F_facet(t, "production_diff", units = "1000 tons")
pf_yield <- F_facet(t, "yield_diff", units = "kg/ha")
pf_area <- F_facet(t, "area_diff", units = "kha")

# Plot on a grid - add legend after as three have NA's
p1 <- pf_prod + pf_yield + pf_area 
p1 <- p1 + plot_layout(nrow = 3) + plot_annotation(tag_levels = 'a')

ggsave(paste0(folder_figures, "_gg_facet_soy_yap_2013_2015_2017_2022.png"),
       plot = p1, width = 14, height = 12)