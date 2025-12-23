# Section 00: Script Details ----------------

# title: 2_1_changedates_lineplots.R
# author: Nick Manning
# purpose: Import the data from 1_data_import_clean.R and change the years to
# represent the similar harvests temporally (e.g. US harvests before Brazil;
# 2012 US Harvest + 2012-2013 US Market Year --> 2012-2013 US Harvest+MY; 2012-2013 Brazil
# Harvest + MY --> 2012-2013 Harvest+MY) so we can compare the two 

# Description: Added a “Harvest + Market Year” variable then moved the Brazil year 
# to be the ending year (e.g. Brazil Year of Soybean Production 
# Stat = 2012, Brazil HMY = 2011-2012, USA HMY = 2012-2013) 

# Created: Feb 2024
# Last Updated: Dec 2025

# REQUIRES:
## "../Data_Derived/prod_price_area_yield_exports.RData" from 1_data_import_clean.R
## "../Data_Source/FAOSTAT_BrUS_2000_2020_ExportQuantity.csv" from Data_Source folder
## "../Data_Derived/land_trans_tosoy_df.RData" from 0_GetMircoMeso_in_Cerrado.R
## "../Data_Derived/land_trans_toclasses_df.RData" from 0_GetMircoMeso_in_Cerrado.R
## "../Data_Source/Terrabrasilis_CerradoDeforestation.csv" from Data_Source folder - TerraBrasilis

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

rm(list = ls())
getwd()

# 0: Load Libraries & Set Constants and Paths ------

### Libraries ###
library(tidyverse)
library(stringr)
library(terra)
library(patchwork)

### Plotting Constants ###
breaks <- c(2007, 2012, 2017)

col_US = "darkorange"
col_BR = "darkblue"
col_pas = "purple"
col_soy = "green"
pt_us = 17 # triangle
pt_br = 16 # circle
pt_size = 4

### Paths ###
# NOTE: Users should change these to their local file structure
path_data_source <- "../Data_Source/"
path_data_derived <- "../Data_Derived/"
path_figures <- "../Figures/"

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# create function that adds a market year column 
## NOTE: the {{}} is used to dynamically refer to the column specified by user rather than having it as a character
F_add_marketyear <- function(df, year_col, area_col){
  df <- df %>% 
    # create new column
    mutate(harvest_marketyr = paste0(as.character({{year_col}}), "-", as.character({{year_col}}+1))) %>% 
    # if country == BR, then harvest_yr equals the previous year
    mutate(harvest_marketyr = case_when(
      {{area_col}} == "Brazil" ~  paste0(as.character({{year_col}}-1), "-", as.character({{year_col}})),
      TRUE ~ harvest_marketyr
    )) 
  
  return(df)
}


# 1: Plot Line Plots -----

# load cleaned stats from previous script - see Section 00 for details
load(paste0(path_data_derived, "prod_price_area_yield_exports.RData"))

## 1.0 : Create Plotting Function --------
F_plot_harvestMY <- function(df, x_var, y_var, group_var, title, subtitle, y_axis_title) {
  ggplot(df, aes(x = {{x_var}}, y = {{y_var}}, group = {{group_var}})) +
    
    # set line and color shape based on fxn input (probably "country")
    geom_line(aes(color = {{group_var}})) + 
    geom_point(aes(color = {{group_var}}, shape = {{group_var}}), size = pt_size) +
    
    # create a vertical dashed line on the 2012-2013 harvest season
    geom_vline(aes(xintercept = "2012-2013"), color = "red", linetype = "dashed", linewidth = 0.5) +
    theme_bw() +
    
    # set up legend manually 
    scale_color_manual(
      name = "Country",
      values = c(US = col_US, Brazil = col_BR),
      breaks = c("US", "Brazil")
    ) +
    scale_shape_manual(
      name = "Country",
      values = c(US = pt_us, Brazil = pt_br),
      breaks = c("US", "Brazil")
    ) +
    
    # add labels
    labs(
      title = title,
      subtitle = subtitle,
      x = "",
      y = y_axis_title
    ) +
    
    # adjust theme to turn labels vertical and remove legend for plotting fxn
    theme(
      axis.text.x = element_text(angle = 90, vjust = 0.8),
      legend.position = "none"
    )
}

## Plot Example
(ggplot(df2_prod_USMW_BRCerr, aes(x = harvest_marketyr, y = prod/1000000, group = country)) +
    geom_point(aes(color = country, shape = country), size = pt_size) +
    geom_line(aes(color = country)) + 
    ylim(y_limits) + # use NA for lower limits
    geom_vline(aes(xintercept = "2012-2013"), color = "red", linetype = "dashed", linewidth = 0.5) +
    theme_bw() +
    
    scale_color_manual(
      name = "Country",
      values = c(US = col_US, Brazil = col_BR),
      breaks = c("US", "Brazil")
    ) +
    scale_shape_manual(
      name = "Country",
      values = c(US = pt_us, Brazil = pt_br), # 16 = filled circle, 17 = filled triangle
      breaks = c("US", "Brazil")
    ) +
    scale_x_discrete() +
    
    labs(
      title = "Annual Regional Soybean Production",
      subtitle = "Data Sources: USDA-NASS & SIDRA",
      x = "",
      y = "Reg. Production (Mmt)"
    ) +
    
    theme(axis.text.x = element_text(angle = 90, vjust = 0.8))  # keep legend on one plot for the final graph
  
)

## 1.1: Production --------

# add Harvest + Market Year Variable
df2_prod_USMW_BRCerr <- F_add_marketyear(df_prod_USMW_BRCerr, yr, country)
df2_prod_USBR <- F_add_marketyear(df_prod_USBR, yr, country)


y_upper <- max(df2_prod_USBR$prod/1000000)
y_lower <- min(df2_prod_USMW_BRCerr$prod/1000000)
y_limits <- range(y_lower, y_upper)

### 1.1.1: Regional (US-MW & Cerrado) -----

# plot 
(p_prod_regional <-
    ggplot(df2_prod_USMW_BRCerr, aes(x = harvest_marketyr, y = prod/1000000, group = country)) +
    geom_point(aes(color = country, shape = country), size = pt_size) +
    geom_line(aes(color = country)) + 
    ylim(y_limits) + # use NA for lower limits
    geom_vline(aes(xintercept = "2012-2013"), color = "red", linetype = "dashed", linewidth = 0.5) +
    theme_bw() +
    
    scale_color_manual(
      name = "Country",
      values = c(US = col_US, Brazil = col_BR),
      breaks = c("US", "Brazil")
    ) +
    scale_shape_manual(
      name = "Country",
      values = c(US = pt_us, Brazil = pt_br), # 16 = filled circle, 17 = filled triangle
      breaks = c("US", "Brazil")
    ) +
    scale_x_discrete() +
    
    labs(
      title = "Annual Regional Soybean Production",
      subtitle = "Data Sources: USDA-NASS & SIDRA",
      x = "",
      y = "Reg. Production (Mmt)"
    ) +
    
    theme(axis.text.x = element_text(angle = 90, vjust = 0.8))  # keep legend on one plot for the final graph

)



### 1.1.2: National (US & BR) ----------

# Plotted manually so we can compare axes more easily
(p_prod_national <- 
    ggplot(df2_prod_USBR, aes(x=harvest_marketyr, y=prod/1000000, group = country)) +
   geom_point(aes(color = country, shape = country), size = pt_size) +
   geom_line(aes(color = country)) + 
    ylim(y_limits) +  # Use NA for the lower limit
    geom_vline(aes(xintercept = "2012-2013"), color = "red",
               linetype="dashed", linewidth=0.5)+
    theme_bw()+
    scale_color_manual(
      name = "Country",
      values = c(
        US = col_US,
        Brazil = col_BR),
      breaks = c("US", "Brazil"),
    )+
   scale_shape_manual(
     name = "Country",
     values = c(US = pt_us, Brazil = pt_br),
     breaks = c("US", "Brazil")
   ) +
    scale_x_discrete()+
    labs(
      title = "Annual National Soybean Production",
      subtitle = "Data Sources: USDA-NASS & SIDRA",
      x = "",
      y = "Natl. Production (Mmt)"
    )+
   
    theme(axis.text.x = element_text(angle = 90, vjust = 0.8)
    )
)


## 1.2: Exports -------

### 1.2.1: Export Quantity (World) -------
# import csv of export quantities from FAOSTAT
exports_usbr <-read.csv(paste0(path_data_source, "FAOSTAT_BrUS_2000_2020_ExportQuantity.csv"))
exports_usbr<- exports_usbr %>% 
  select(Area, Year, Element, Value) %>% 
  filter(Year >= 2007 & Year <= 2017)

# change to USA for consistency
exports_usbr$Area <- str_replace(exports_usbr$Area, "United States of America", "US")

# add MY
exports2_usbr <- F_add_marketyear(exports_usbr, Year, Area)

# plot
(p_exportqty_usbr_toworld <- F_plot_harvestMY(
  df = exports2_usbr, 
  x_var = harvest_marketyr, y_var = Value/1000000, group_var = Area,
  title = "Annual Soybean Export Quantity to World",
  subtitle = "Data Source: FAOSTAT",
  y_axis_title = "Natl. Export Quantity (Mmt)"
))


## 1.3: Yield -------

### 1.3.1 Regional Yield ------
df2_yield_USMW_BRCerr <- F_add_marketyear(df_yield_USMW_BRCerr, yr, country)

# plot
(p_yield_usmwbrcerr <- F_plot_harvestMY(
  df = df2_yield_USMW_BRCerr, 
  x_var = harvest_marketyr, y_var = yield/1000, group_var = country,
  title = "Annual Regional Soybean Yield",
  subtitle = "Data Sources: USDA-NASS & SIDRA",
  y_axis_title = "Reg. Soybean Yield (1000 kg/ha)"
))


## 1.4: Area (Regional) ------------

### 1.4.1 Area Harvested ---------

# add Market Year
df2_area_h_USMW_BRCerr <- F_add_marketyear(df_area_h_USMW_BRCerr, yr, country)

# plot
(p_area_h_regional <- 
    ggplot(df2_area_h_USMW_BRCerr, aes(x=harvest_marketyr, y=area_harvested/1000000, group = country)) +
    geom_point(aes(color = country, shape = country), size = pt_size) +
    geom_line(aes(color = country)) + 
    geom_vline(aes(xintercept = "2012-2013"), color = "red",
               linetype="dashed", linewidth=0.5)+
    theme_bw()+
    scale_color_manual(
      name = "Country",
      values = c(
        US = col_US,
        Brazil = col_BR),
      breaks = c("US", "Brazil")
    )+
    scale_shape_manual(
      name = "Country",
      values = c(US = pt_us, Brazil = pt_br),
      breaks = c("US", "Brazil")
    ) +
    scale_x_discrete()+
    labs(
      title = "Annual Regional Soybean Area Harvested",
      subtitle = "Data Sources: USDA-NASS & SIDRA",
      x = "",
      y = "Reg. Area Harvested (Mha)"
    )+
    
    theme(legend.position="bottom",
          legend.text = element_text(size = 20),
          legend.title = element_text(size = 20))+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.8)
    )
)


# 2: DATA NOT RELIANT ON AGRICULTURAL YEAR --------------------------------

## 2.1: Market Price -----

# plot 
(p_price_usmwbrcerr<-
   ggplot(df_price_USMW_BRCerr, aes(x=date, y=price)) +
   geom_point(aes(color = country, shape = country), size = pt_size) +
   geom_line(aes(color = country)) +  
   theme_bw()+
   scale_x_date(date_breaks = "1 year", date_labels = "%Y")+
   theme(axis.text.x = element_text(size = 6))+
   scale_color_manual(
     name = "Country",
     values = c(
       US = col_US,
       Brazil = col_BR),
     breaks = c("US", "Brazil"))+
  
    scale_shape_manual(
     name = "Country",
     values = c(US = pt_us, Brazil = pt_br),
     breaks = c("US", "Brazil")
   ) +
   geom_vline(aes(xintercept = as.Date("2012-07-01")), color = "red",linetype="dashed", linewidth=0.5, alpha = 0.5)+
   labs(
     title = "Monthly Price of Soybean",
     subtitle = "Data Sources: USDA-NASS & CEPEA",
     x = "",
     y = "Soybean Price (USD/bu)"
   )+
   
   theme(legend.position="none", legend.text = element_text(size = 12))+
   theme(axis.text.x = element_text(angle = 90, vjust = 0.8))
 
)

## 2.2: Land Conversion -------

br_int_yr <- 2012

labels2 <- paste(seq(2006, 2017, 1), seq(2007, 2018, 1), sep = "-")
breaks2 <- seq(2007, 2018, 1)

### 2.2.1: Land Conversion in the Cerrado --------

load(paste0(path_data_derived, "land_trans_tosoy_df.RData"))

# filter to timae frame
df_trans_to_soy_BRCerr_muni <- df_trans_to_soy_BRCerr_muni %>% 
  filter(yr >= 2007 & yr <= 2017) %>% 
  mutate(to_level_4 = str_replace(to_level_4, "Soy Beans", "Soy"))

# plot
(p_trans_tosoy <-
    ggplot(df_trans_to_soy_BRCerr_muni, aes(x=yr, y=trans/1000000, color = to_level_4)) +
    geom_point(size = pt_size, aes(shape = to_level_4)) +
    geom_line() + 
    geom_vline(aes(xintercept = br_int_yr), color = "red",
               linetype="dashed", linewidth=0.5)+
    
    scale_color_manual(
      name = "",
      values = c(
        Pasture = "darkgreen",
        Soy = "purple"),
      breaks = c("Pasture", "Soy"))+
    
    scale_shape_manual(
      name = "",
      values = c(Pasture = 15, Soy = 23),
      breaks = c("Pasture", "Soy")
    ) +

    scale_x_continuous(breaks = breaks2, labels = labels2)+
    
    theme_bw()+  
    labs(
      title = "Annual Cerrado Land Conversion to Pasture or Soy",
      subtitle = "Data Source: Aggregated from municipality-level MapBiomas",
      x = "",
      y = "Cerrado Land Conversion (Mha)"
    )+
    theme(legend.position= c(0.9, 0.4), legend.title = element_blank())+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.8))

)

### 2.2.2: Land Conversion of Other Classes in the Cerrado --------
load(paste0(path_data_derived, "land_trans_toclasses_df.RData"))

# NOTE: Other Classes includes "Soy Beans", "Pasture",
# "Other Temporary Crops", "Mosaic of Agriculture and Pasture",
# "Sugar Cane", "Other Non Vegetated Area", "Coffee" ("Coffe"),
# "Other Non Forest Natural Formation", "Citrus", and "Rice"

# filter to time frame
df_trans_to_classes_BRCerr_muni <- df_trans_to_classes_BRCerr_muni %>% filter(yr >= 2007 & yr <= 2017)

# plot
(p_trans_toclasses <-
    ggplot(df_trans_to_classes_BRCerr_muni, 
           aes(x=yr, y=trans/1000000)) +
    geom_line(color = col_BR) + 
    geom_point(color = col_BR, size = pt_size) +
    geom_vline(aes(xintercept = br_int_yr), color = "red",
               linetype="dashed", linewidth=0.5)+
    theme_bw()+  
    scale_x_continuous(breaks = seq(2000, 2017, 1), labels = seq(2000, 2017, 1))+
    
    labs(
      title = "Annual Cerrado Land Conversion to Soy & Other Non-Native Cover",
      subtitle = "Aggregated from municipality-level MapBiomas",
      x = "",
      y = "Cerrado Land Conversion (Mha)"
    )+
    
    theme(legend.position="none")+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.8))
)

## 2.3 TerraBrasilis Deforestation --------
# import TerraBrasilis - PRODES CSV: https://terrabrasilis.dpi.inpe.br/app/dashboard/deforestation/biomes/cerrado/increments
df_trans_deforest <- read.csv(paste0(path_data_source, "Terrabrasilis_CerradoDeforestation.csv"))

# filter to only 2007-2017
df_trans_deforest <- df_trans_deforest %>% filter(year >= 2007 & year <= 2017)

(p_trans_deforest <-
    ggplot(df_trans_deforest, aes(x=year, y=area_km2)) +
    geom_line(color = col_BR) + 
    geom_point(color = col_BR, size = pt_size) +
    geom_vline(aes(xintercept = br_int_yr), color = "red",
               linetype="dashed", linewidth=0.5)+
    #scale_x_continuous(breaks = breaks, labels = breaks)+
    scale_x_continuous(breaks = seq(2000, 2017, 1), labels = seq(2000, 2017, 1))+
    #scale_x_continuous(breaks = breaks2, labels = labels2)+
    
    theme_bw()+  
    labs(
      title = "Annual Cerrado Deforestation",
      subtitle = "Data Source: TerraBrasilis",
      x = "",
      y = "Cerrado Deforestation (km^2)"
    )+
    
    theme(legend.position="none")+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.8))
)



# 3: Arrange Plots ------------

## 3.1: Arrange Final Figure ----
# Set themes
theme_text_sizes <- theme(
  # Set size and style for the title
  plot.title = element_blank(),
  plot.subtitle = element_blank(),
  
  # Set size for x-axis text
  axis.title.x = element_text(size = 12),
  axis.text.x = element_text(size = 15),  
  
  # Set size for y-axis text
  axis.title.y = element_text(size = 18),
  axis.text.y = element_text(size = 15)  
) 

# get and arrange plots with spacers (ABC where B is spacer)
p1 <- 
  (p_yield_usmwbrcerr + theme(legend.position = "none")+ theme_text_sizes) + plot_spacer() +
  (p_prod_regional + theme(legend.position = "none") + theme_text_sizes) +
  (p_prod_national + theme(legend.position = "none")+ theme_text_sizes) + plot_spacer() +
  (p_price_usmwbrcerr + theme(legend.position = "none")+  theme_text_sizes) + 
  (p_area_h_regional+ theme(legend.position = "none")+theme_text_sizes) + plot_spacer() +
  (p_exportqty_usbr_toworld + theme(legend.position = "none")+ theme_text_sizes) + 
  (p_trans_deforest + theme(legend.position = "none")+ theme_text_sizes) + plot_spacer() +
  (p_trans_tosoy + theme(legend.title = element_blank()) + theme_text_sizes + 
     theme(legend.text = element_text(size = 12), legend.title = element_blank()))

# re-arrange plots to fit correctly with margins
p2 <- p1 +
  plot_layout(nrow = 4, widths = c(1, 0.2, 1))

# add tags
p2 <- p2 + 
  plot_annotation(tag_levels = 'a') & 
  theme(plot.tag = element_text(size = 36))

## 3.2: Save Final Figure -----
ggsave(filename = paste0(path_figures, "soybeanstats_harvestmarketyear.png"),
       p2, height = 22, width = 19, 
       dpi = 300)  
