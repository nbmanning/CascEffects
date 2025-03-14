# (For Chapter 2 of Thesis & Casc Effects Paper)

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
# Last Updated: Feb 2025

# Working Notes:
## 3/4/24: Currently working to remove the duplicate legends and clean the 
## script by creating a function to plot the harvest + MY data

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

rm(list = ls())
getwd()

# 0: Load Libraries & Set Constants ------
library(tidyverse)
library(stringr)
library(terra)


breaks <- c(2007, 2012, 2017)
col_US = "salmon"
col_BR = "cornflowerblue"

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

load("../Data_Derived/prod_price_area_yield_exports.RData")

theme_text_sizes <- theme(
  # Set size and style for the title
  plot.title = element_text(size = 16),  
  
  # Set size for x-axis text
  axis.title.x = element_text(size = 12),
  axis.text.x = element_text(size = 12),  
  
  # Set size for y-axis text
  axis.title.y = element_text(size = 12),
  axis.text.y = element_text(size = 12)  
  #plot.title = element_text(size = 16, face = "bold", hjust = 0.5)  # Set size and style for the title
  ) 

## 1.0 : Create Plotting Function --------
F_plot_harvestMY <- function(df, x_var, y_var, group_var, title, subtitle, y_axis_title){
  # set plot parameters as inputs from the fxn
  ggplot(df, aes(x={{x_var}}, y={{y_var}}, color = {{group_var}}, group = {{group_var}})) +
    geom_line() + 
    geom_point() +
    # create a vertical dashed line on the 2012-2013 harvest season
    geom_vline(aes(xintercept = "2012-2013"), color = "red",
               linetype="dashed", linewidth=0.5)+
    theme_bw()+
    # set up legend manually
    scale_color_manual(
      name = "Country",
      values = c(
        US = col_US,
        Brazil = col_BR),
      breaks = c("US", "Brazil"))+
    # set labels
    labs(
      title = title,
      subtitle = subtitle,
      x = "",
      y = y_axis_title)+   
    # adjust theme to turn labels vertical and remove legend for plotting fxn
    
    theme(axis.text.x = element_text(angle = 90, vjust = 0.8),
          legend.position = "none")
}

# (p2_prod_national <- F_plot_harvestMY(
#   df = df2_prod_USBR, x_var = harvest_marketyr, y_var = prod, group_var = country,
#   title = "Annual National Soybean Production",
#   subtitle = "Data Sources: USDA-NASS & SIDRA",
#   y_axis_title = "Production (mt)"
# ))


## 1.1: Production --------

### 1.1.1: Regional (US-MW & Cerrado) -----
# add Harvest + Market Year Variable
df2_prod_USMW_BRCerr <- F_add_marketyear(df_prod_USMW_BRCerr, yr, country)
df2_prod_USBR <- F_add_marketyear(df_prod_USBR, yr, country)


y_upper <- max(df2_prod_USBR$prod)
y_lower <- min(df2_prod_USMW_BRCerr$prod)
y_limits <- range(y_lower, y_upper)

# plot 
(p_prod_regional <- 
    ggplot(df2_prod_USMW_BRCerr, aes(x=harvest_marketyr, y=prod, color = country, group = country)) +
    geom_point() +
    geom_line() + 
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
    #labels = c("US-MW", "Cerrado")
  )+
  scale_x_discrete()+
  labs(
    title = "Annual Regional Soybean Production",
    subtitle = "Data Sources: USDA-NASS & SIDRA",
    x = "",
    y = "Production (mt)"
  )+
  
    theme(axis.text.x = element_text(angle = 90, vjust = 0.8)#,
        # keep legend on one plot for the final graph
        #legend.position = "none"
  )
)




### 1.1.2: National (US & BR) ----------

# Plotted manually so we can compare axes more easily
(p_prod_national <- 
    ggplot(df2_prod_USBR, aes(x=harvest_marketyr, y=prod, color = country, group = country)) +
    geom_point() +
    geom_line() + 
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
      #labels = c("US-MW", "Cerrado")
    )+
    scale_x_discrete()+
    labs(
      title = "Annual National Soybean Production",
      subtitle = "Data Sources: USDA-NASS & SIDRA",
      x = "",
      y = "Production (mt)"
    )+
   
    theme(axis.text.x = element_text(angle = 90, vjust = 0.8)#,
          # keep legend on one plot for the final graph
          #legend.position = "none"
    )
)


## 1.2: Exports -------

### 1.2.1: Export Value (US/BR --> China) --------

# add Harvest + Market Year Variable
df2_exports_USBR_china <- F_add_marketyear(df_exports_USBR_china, Period, ReporterDesc)

# plot
(p_exportvalue_usbr_tochina <- F_plot_harvestMY(
  df = df2_exports_USBR_china, 
  x_var = harvest_marketyr, y_var = PrimaryValue, group_var = ReporterDesc,
  title = "Annual Soybean Export Value to China",
  subtitle = "Data Sources: UN Comtrade",
  y_axis_title = "Export Value (USD)"
))


### 1.2.1: Export Value (US/BR --> World) --------
# add Harvest + Market Year Variable
df2_exports_USBR_world <- F_add_marketyear(df_exports_USBR_world, Period, ReporterDesc)

# plot
(p_exportvalue_usbr_toworld <- F_plot_harvestMY(
  df = df2_exports_USBR_world, 
  x_var = harvest_marketyr, y_var = PrimaryValue, group_var = ReporterDesc,
  title = "Annual Soybean Export Value to World",
  subtitle = "Data Sources: UN Comtrade",
  y_axis_title = "Export Value (USD)"
))


### 1.2.2: Export Quantity (World) -------
# import
exports_usbr <-read.csv("../Data_Source/FAOSTAT_BrUS_2000_2020_ExportQuantity.csv")
exports_usbr<- exports_usbr %>% 
  select(Area, Year, Element, Value) %>% 
  filter(Year >= 2007 & Year <= 2017)

exports_usbr$Area <- str_replace(exports_usbr$Area, "United States of America", "US")

# add MY
exports2_usbr <- F_add_marketyear(exports_usbr, Year, Area)

# plot
(p_exportqty_usbr_toworld <- F_plot_harvestMY(
  df = exports2_usbr, 
  x_var = harvest_marketyr, y_var = Value, group_var = Area,
  title = "Annual Soybean Export Quantity to World",
  subtitle = "Data Source: UN Comtrade",
  y_axis_title = "Quantity (kg)"
))


## 1.3: Yield -------

### 1.3.1: National Yield ------
df2_yield_USBR <- F_add_marketyear(df_yield_USBR, yr, country)

# plot
(p_yield_usbr <- F_plot_harvestMY(
  df = df2_yield_USBR, 
  x_var = harvest_marketyr, y_var = yield, group_var = country,
  title = "Annual National Soybean Yield",
  subtitle = "Data Sources: USDA-NASS & SIDRA",
  y_axis_title = "Soybean Yield (kg/ha)"
))


### 1.3.2 Regional Yield ------
df2_yield_USMW_BRCerr <- F_add_marketyear(df_yield_USMW_BRCerr, yr, country)

# plot
(p_yield_usmwbrcerr <- F_plot_harvestMY(
  df = df2_yield_USMW_BRCerr, 
  x_var = harvest_marketyr, y_var = yield, group_var = country,
  title = "Annual Regional Soybean Yield",
  subtitle = "Data Sources: USDA-NASS & SIDRA",
  y_axis_title = "Soybean Yield (kg/ha)"
))


## 1.4: Area (Regional) ------------

### 1.4.1 Area Harvested ---------

# add Market Year
df2_area_h_USMW_BRCerr <- F_add_marketyear(df_area_h_USMW_BRCerr, yr, country)

# plot
(p_area_h_regional <- 
    ggplot(df2_area_h_USMW_BRCerr, aes(x=harvest_marketyr, y=area_harvested, color = country, group = country)) +
    geom_point() +
    geom_line() + 
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
    scale_x_discrete()+
    labs(
      title = "Annual Regional Soybean Area Harvested",
      subtitle = "Data Sources: USDA-NASS & SIDRA",
      x = "",
      y = "Area (ha)"
    )+
    
    theme(legend.position="none")+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.8)
    )
)

### 1.4.2 Area Planted -----------
df2_area_p_USMW_BRCerr <- F_add_marketyear(df_area_p_USMW_BRCerr, yr, country)

# plot
(p_area_p_regional <- 
    ggplot(df2_area_p_USMW_BRCerr, aes(x=harvest_marketyr, y=area_planted, color = country, group = country)) +
    geom_point() +
    geom_line() + 
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
    scale_x_discrete()+
    labs(
      title = "Annual Regional Soybean Area Planted",
      subtitle = "Data Sources: USDA-NASS & SIDRA",
      x = "",
      y = "Area (ha)"
    )+
    
    theme(legend.position="none")+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.8)
    )
)

# 2: UNCHANGED DATA --------------------------------

## 2.1: Market Price -----

# plot 
# (p_yield_usmwbrcerr <- F_plot_harvestMY(
#   df = df2_yield_USMW_BRCerr, 
#   x_var = harvest_marketyr, y_var = yield, group_var = country,
#   title = "Annual Regional Soybean Yield",
#   subtitle = "Data Sources: USDA-NASS & SIDRA",
#   y_axis_title = "Soybean Yield (kg/ha)"
# ))


(p_price_usmwbrcerr<-
   ggplot(df_price_USMW_BRCerr, aes(x=date, y=price, color = country)) +
   geom_line() + 
   geom_point() +  
   theme_bw()+
   scale_x_date(date_breaks = "1 year", date_labels = "%Y")+
   theme(axis.text.x = element_text(size = 6))+
   scale_color_manual(
     name = "Country",
     values = c(
       US = col_US,
       Brazil = col_BR),
     breaks = c("US", "Brazil"))+
   geom_vline(aes(xintercept = as.Date("2012-07-01")), color = "red",linetype="dashed", linewidth=0.5, alpha = 0.5)+
   labs(
     title = "Monthly Price of Soybean",
     subtitle = "Data Sources: USDA-NASS & CEPEA",
     x = "",
     y = "Soybean Price (USD)"
   )+
   
   theme(legend.position="none")+
   theme(axis.text.x = element_text(angle = 90, vjust = 0.8))
 
)

## 2.2: Land Conversion -------

br_int_yr <- 2012

labels2 <- paste(seq(2006, 2017, 1), seq(2007, 2018, 1), sep = "-")
breaks2 <- seq(2007, 2018, 1)

### 2.2.1: Land Conversion in the Cerrado --------

load("../Data_Derived/land_trans_tosoy_df.RData")

df_trans_to_soy_BRCerr_muni <- df_trans_to_soy_BRCerr_muni %>% 
  filter(yr >= 2007 & yr <= 2017) %>% 
  mutate(to_level_4 = str_replace(to_level_4, "Soy Beans", "Soy"))

(p_trans_tosoy <-
    ggplot(df_trans_to_soy_BRCerr_muni, aes(x=yr, y=trans/1000000, color = to_level_4)) +
    # geom_line(color = col_BR) + 
    # geom_point(color = col_BR) +
    geom_line() +
    geom_point() +
    geom_vline(aes(xintercept = br_int_yr), color = "red",
               linetype="dashed", linewidth=0.5)+
    #scale_x_continuous(breaks = breaks, labels = breaks)+
    #scale_x_continuous(breaks = seq(2000, 2017, 1), labels = seq(2000, 2017, 1))+
    scale_x_continuous(breaks = breaks2, labels = labels2)+
    
    theme_bw()+  
    labs(
      title = "Annual Cerrado Land Conversion to Soy",
      subtitle = "Data Source: Aggregated from municipality-level MapBiomas",
      x = "",
      y = "Land Conversion (Mha)"
    )+
    
    # theme(legend.position="none")+
    theme(legend.position= c(0.9, 0.4), legend.title = element_blank())+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.8))

)

### 2.2.2: Land Conversion of Other Classes in the Cerrado --------
load("../Data_Derived/land_trans_toclasses_df.RData")

# NOTE: Other Classes includes "Soy Beans", "Pasture",
# "Other Temporary Crops", "Mosaic of Agriculture and Pasture",
# "Sugar Cane", "Other Non Vegetated Area", "Coffee" ("Coffe"),
# "Other Non Forest Natural Formation", "Citrus", and "Rice"


df_trans_to_classes_BRCerr_muni <- df_trans_to_classes_BRCerr_muni %>% filter(yr >= 2007 & yr <= 2017)

(p_trans_toclasses <-
    ggplot(df_trans_to_classes_BRCerr_muni, 
           aes(x=yr, y=trans/1000000)) +
    geom_line(color = col_BR) + 
    geom_point(color = col_BR) +
    geom_vline(aes(xintercept = br_int_yr), color = "red",
               linetype="dashed", linewidth=0.5)+
    theme_bw()+  
    #scale_x_continuous(breaks = breaks, labels = breaks)+
    scale_x_continuous(breaks = seq(2000, 2017, 1), labels = seq(2000, 2017, 1))+
    
    labs(
      title = "Annual Cerrado Land Conversion to Soy & Other Non-Native Cover",
      subtitle = "Aggregated from municipality-level MapBiomas",
      x = "",
      y = "Land Conversion (Mha)"
    )+
    
    theme(legend.position="none")+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.8))
)

## 2.3 TerraBrasilis Deforestation --------

df_trans_deforest <- read.csv("../Data_Source/Terrabrasilis_CerradoDeforestation.csv")
(p_trans_deforest <-
    ggplot(df_trans_deforest, aes(x=year, y=area_km2)) +
    geom_line(color = col_BR) + 
    geom_point(color = col_BR) +
    geom_vline(aes(xintercept = br_int_yr), color = "red",
               linetype="dashed", linewidth=0.5)+
    #scale_x_continuous(breaks = seq(2000, 2022, 2), labels = seq(2000, 2022, 2))+
    scale_x_continuous(breaks = seq(2000, 2017, 1), labels = seq(2000, 2017, 1))+
    
    theme_bw()+  
    labs(
      title = "Annual Cerrado Deforestation",
      subtitle = "Data Source: TerraBrasilis",
      x = "",
      y = "Deforestation (km^2)"
    )+
    
    theme(legend.position="none")+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.8))
)

# filter to only 2007-2017
df_trans_deforest <- df_trans_deforest %>% filter(year >= 2007 & year <= 2017)

(p_trans_deforest2 <-
    ggplot(df_trans_deforest, aes(x=year, y=area_km2)) +
    geom_line(color = col_BR) + 
    geom_point(color = col_BR) +
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
      y = "Deforestation (km^2)"
    )+
    
    theme(legend.position="none")+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.8))
)




# 3: Arrange Plots ------------
library(patchwork)

# option to add other stats here
p1 <- 
  p_prod_regional + theme(legend.position = "none") +
  p_prod_national + theme(legend.position = "none")+
  
  p_yield_usmwbrcerr + theme(legend.position = "none")+ 
  #p_yield_usbr +
  
  p_price_usmwbrcerr + theme(legend.position = "none")+  
 
  p_exportqty_usbr_toworld + theme(legend.position = "none")+ 
  
  #p_area_p_regional+
  p_area_h_regional+ theme(legend.position = "none")+
  
  #p_exportvalue_usbr_toworld + 
  #p_exportvalue_usbr_tochina + 
  
  p_trans_deforest2 + theme(legend.position = "none")+ 
  p_trans_tosoy + theme(legend.title = element_blank())

p1

# manually extract one legend to add to plot since it keeps duplicating
# library(cowplot)
# legend <- cowplot::get_legend(p_prod_national + theme(legend.position = "bottom"))
# combined_plot <- cowplot::plot_grid(
#   p_prod_regional + theme(legend.position = "none"),
#   p_prod_national + theme(legend.position = "none"),
#   legend,
#   ncol = 1,
#   rel_heights = c(1, 1, 0.1)
# )

theme_text_sizes <- theme(
  # Set size and style for the title
  plot.title = element_text(size = 18),
  plot.subtitle = element_text(size = 15),
  
  # Set size for x-axis text
  axis.text.x = element_text(size = 13),  
  
  # Set size for y-axis text
  axis.title.y = element_text(size = 14),
  axis.text.y = element_text(size = 14),  

  # Set legend size
  legend.text = element_text(size = 15),
  legend.title = element_text(size = 15)
) 

p2 <- p1 +
  #plot_layout(nrow = 4, guides = "collect") & 
  plot_layout(nrow = 4) #& 
  #theme(legend.position = 'bottom') &
  #theme(legend.position = 'none') &
  #theme_text_sizes 

p2 <- p2 + 
  plot_annotation(tag_levels = 'A') +     
  theme(plot.tag = element_text(size = 18))

p2

ggsave(filename = "../Figures/soybeanstats_harvestmarketyear_v8.png",
       p2, height = 16, width = 20, 
       dpi = 300)  
