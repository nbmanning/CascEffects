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

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

rm(list = ls())
getwd()

# 0: Load Libraries & Set Constants ------
library(tidyverse)
library(stringr)
library(terra)


breaks <- c(2007, 2012, 2017)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# theme_set(theme_bw())

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
}

# 1: Plot Line Plots -----

load("../Data_Derived/prod_price_yield_exports.RData")

## 1.1: Production --------

### 1.1.1: Regional (US-MW & Cerrado) -----
# add Harvest + Market Year Variable
df2_prod_USMW_BRCerr <- df_prod_USMW_BRCerr %>% 
  # create new column
  mutate(harvest_marketyr = paste0(as.character(yr), "-", as.character(yr+1))) %>% 
  # if country == BR, then harvest_yr equals the previous year
  mutate(harvest_marketyr = case_when(
    country == "Brazil" ~  paste0(as.character(yr-1), "-", as.character(yr)),
    TRUE ~ harvest_marketyr
  ))

# plot - DONE
(p_prod_regional <- 
   ggplot(df2_prod_USMW_BRCerr, aes(x=harvest_marketyr, y=prod, color = country, group = country)) +
   geom_point() +
   geom_line() + 
   geom_vline(aes(xintercept = "2012-2013"), color = "red",
              linetype="dashed", linewidth=0.5)+
   theme_bw()+
   scale_color_manual(
     name = "Country",
     values = c(
       US = "salmon",
       Brazil = "cornflowerblue"),
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
   theme(axis.text.x = element_text(angle = 90, vjust = 0.8))
)



### 1.1.2: National (US & BR) ----------
# add Harvest + Market Year Variable
df2_prod_USBR <- df_prod_USBR %>% 
  # create new column
  mutate(harvest_marketyr = paste0(as.character(yr), "-", as.character(yr+1))) %>% 
  # if country == BR, then harvest_yr equals the previous year
  mutate(harvest_marketyr = case_when(
    country == "Brazil" ~  paste0(as.character(yr-1), "-", as.character(yr)),
    TRUE ~ harvest_marketyr
  ))

df3_prod_USBR <- F_add_marketyear(df_prod_USBR, yr, country)


# plot - DONE
(p_prod_national <- ggplot(df2_prod_USBR, aes(x=harvest_marketyr, y=prod, color = country, group = country)) +
   geom_line() + 
   geom_point() +
   geom_vline(aes(xintercept = "2012-2013"), color = "red",
              linetype="dashed", linewidth=0.5)+
   theme_bw()+
   #scale_x_continuous(breaks = breaks, labels = breaks)+
   scale_color_manual(
     name = "Country",
     values = c(
       US = "salmon",
       Brazil = "cornflowerblue"),
     breaks = c("US", "Brazil"))+
   labs(
     title = "Annual National Soybean Production",
     subtitle = "Data Sources: USDA-NASS & SIDRA",
     x = "",
     y = "Production (mt)"
   )+   
    theme(axis.text.x = element_text(angle = 90, vjust = 0.8))
)


## 1.2: Exports -------

### 1.2.1: Export Value (US/BR --> China) --------
# add Harvest + Market Year Variable
df2_exports_USBR_china <- df_exports_USBR_china %>% 
  # create new column
  mutate(harvest_marketyr = paste0(as.character(Period), "-", as.character(Period+1))) %>% 
  # if country == BR, then harvest_yr equals the previous year
  mutate(harvest_marketyr = case_when(
    ReporterDesc == "Brazil" ~  paste0(as.character(Period-1), "-", as.character(Period)),
    TRUE ~ harvest_marketyr
  ))

# plot
(p_exportvalue_usbr_tochina <- 
   ggplot(df2_exports_USBR_china, aes(x=harvest_marketyr, y=PrimaryValue, color = ReporterDesc, group = ReporterDesc)) +
   geom_line() + 
   geom_point() +
   geom_vline(aes(xintercept = "2012-2013"), color = "red", linetype="dashed", linewidth=0.5)+
   theme_bw()+
   scale_color_manual(
     name = "Country",
     values = c(
       US = "salmon",
       Brazil = "cornflowerblue"),
     breaks = c("US", "Brazil"))+
   labs(
     title = "Annual Soybean Export Value to China",
     subtitle = "Data Source: UN Comtrade",
     x = "",
     y = "Export Value (USD)"
   )+theme(axis.text.x = element_text(angle = 90, vjust = 0.8))
)

### 1.2.1: Export Value (US/BR --> World) --------
# add Harvest + Market Year Variable
df2_exports_USBR_world <- df_exports_USBR_world %>% 
  # create new column
  mutate(harvest_marketyr = paste0(as.character(Period), "-", as.character(Period+1))) %>% 
  # if country == BR, then harvest_yr equals the previous year
  mutate(harvest_marketyr = case_when(
    ReporterDesc == "Brazil" ~  paste0(as.character(Period-1), "-", as.character(Period)),
    TRUE ~ harvest_marketyr
  ))



# plot - DONE
(p_exportvalue_usbr_toworld <- 
    ggplot(df2_exports_USBR_world, aes(x=harvest_marketyr, y=PrimaryValue, color = ReporterDesc, group = ReporterDesc)) +
    geom_line() + 
    geom_point() +
    geom_vline(aes(xintercept = "2012-2013"), color = "red", linetype="dashed", linewidth=0.5)+
    theme_bw()+
    scale_color_manual(
      name = "Country",
      values = c(
        US = "salmon",
        Brazil = "cornflowerblue"),
      breaks = c("US", "Brazil"))+
    labs(
      title = "Annual Soybean Export Value to World",
      subtitle = "Data Source: UN Comtrade",
      x = "",
      y = "Export Value (USD)"
    )+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.8))
)

### 1.2.2: Export Quantity (World) -------
exports_usbr <-read.csv("../Data_Source/FAOSTAT_BrUS_2000_2020_ExportQuantity.csv")
exports_usbr<- exports_usbr %>% 
  select(Area, Year, Element, Value) %>% 
  filter(Year >= 2007 & Year <= 2017)

exports_usbr$Area <- str_replace(exports_usbr$Area, "United States of America", "US")


(p_exportqty_usbr_toworld <- 
    ggplot(exports_usbr, aes(x=Year, y=Value, color = Area)) +
    geom_line() + 
    geom_point() +
    geom_vline(aes(xintercept = 2012), color = "red",
               linetype="dashed", linewidth=0.5)+
    theme_bw()+
    scale_x_continuous(breaks = breaks, labels = breaks)+
    
    scale_color_manual(
      name = "Country",
      values = c(
        US = "salmon",
        Brazil = "cornflowerblue"),
      breaks = c("US", "Brazil"))+
    labs(
      title = "Annual National Export Quantity of Soybeans",
      subtitle = "Data Source: FAOSTAT",
      x = "",
      y = "Exports (tonnes)"
    ))


