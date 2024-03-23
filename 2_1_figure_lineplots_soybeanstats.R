# (For Chapter 2 of Thesis & Casc Effects Paper)

# title: 2_plots.R
# author: Nick Manning
# purpose: Import and clean only the data that we want to make line plots with

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

# 1: Plot Line Plots -----

load("../Data_Derived/prod_price_yield_exports.RData")

## 1.1: Production --------

### 1.1.1: Regional (US-MW & Cerrado) -----
(p_prod_regional <- 
  ggplot(df_prod_USMW_BRCerr, aes(x=yr, y=prod, color = country)) +
  geom_line() + 
  geom_point() +
  geom_vline(aes(xintercept = 2012), color = "red",
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
   scale_x_continuous(breaks = breaks, labels = breaks)+
   labs(
    title = "Annual Regional Soybean Production",
    subtitle = "Data Sources: USDA-NASS & SIDRA",
    x = "",
    y = "Production (mt)"
    )
)
  
### 1.1.2: National (US & BR) ----------
(p_prod_national <- ggplot(df_prod_USBR, aes(x=yr, y=prod, color = country)) +
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
    title = "Annual National Soybean Production",
    subtitle = "Data Sources: USDA-NASS & SIDRA",
    x = "",
    y = "Production (mt)"
  )
)


## 1.2: Exports -------

### 1.2.1: Export Value (US/BR --> China) --------
(p_exportvalue_usbr_tochina <- 
  ggplot(df_exports_USBR_china, aes(x=Period, y=PrimaryValue, color = ReporterDesc)) +
  geom_line() + 
  geom_point() +
  geom_vline(aes(xintercept = 2012), color = "red", linetype="dashed", linewidth=0.5)+
  theme_bw()+
   scale_x_continuous(breaks = breaks, labels = breaks)+
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
  )
)

### 1.2.1: Export Value (US/BR --> World) --------
(p_exportvalue_usbr_toworld <- 
  ggplot(df_exports_USBR_world, aes(x=Period, y=PrimaryValue, color = ReporterDesc)) +
  geom_line() + 
  geom_point() +
  geom_vline(aes(xintercept = 2012), color = "red", linetype="dashed", linewidth= 0.5)+
  theme_bw()+
   scale_x_continuous(breaks = breaks, labels = breaks)+
   scale_color_manual(
    name = "Country",
    values = c(
      US = "salmon",
      Brazil = "cornflowerblue"),
    breaks = c("US", "Brazil"))+
  labs(
    title = "Annual Soybean Export Value",
    subtitle = "Data Source: UN Comtrade",
    x = "",
    y = "Export Value (USD)"
  )
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


## 1.3: Market Price -----
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
      US = "salmon",
      Brazil = "cornflowerblue"),
    breaks = c("US", "Brazil"))+
   geom_vline(aes(xintercept = as.Date("2012-07-01")), color = "red",linetype="dashed", linewidth=0.5, alpha = 0.5)+
   labs(
    title = "Monthly Price of Soybean",
    subtitle = "Data Sources: USDA-NASS & CEPEA",
    x = "",
    y = "Soybean Price (USD)"
  )
)

## 1.4: Land Transition -------

### 1.4.1: Land Transition in the Cerrado --------

load("../Data_Derived/land_trans_tosoy_df.RData")

df_trans_to_soy_BRCerr_muni <- df_trans_to_soy_BRCerr_muni %>% 
  filter(yr >= 2007 & yr <= 2017)

(p_trans_tosoy <-
  ggplot(df_trans_to_soy_BRCerr_muni, aes(x=yr, y=trans/1000000)) +
  geom_line(color = "cornflowerblue") + 
  geom_point(color = "cornflowerblue") +
  geom_vline(aes(xintercept = 2012), color = "red",
             linetype="dashed", linewidth=0.5)+
   scale_x_continuous(breaks = breaks, labels = breaks)+
   
  theme_bw()+  
  labs(
    title = "Annual Cerrado Land Transition to Soy",
    subtitle = "Data Source: Aggregated from municipality-level MapBiomas",
    x = "",
    y = "Land Transition (Mha)"
  )
)

### 1.4.2: Land Transition of Other Classes in the Cerrado --------
load("../Data_Derived/land_trans_toclasses_df.RData")

# NOTE: Other Classes includes "Soy Beans", "Pasture",
# "Other Temporary Crops", "Mosaic of Agriculture and Pasture",
# "Sugar Cane", "Other Non Vegetated Area", "Coffee" ("Coffe"),
# "Other Non Forest Natural Formation", "Citrus", and "Rice"


df_trans_to_classes_BRCerr_muni <- df_trans_to_classes_BRCerr_muni %>% filter(yr >= 2007 & yr <= 2017)

(p_trans_toclasses <-
  ggplot(df_trans_to_classes_BRCerr_muni, 
         aes(x=yr, y=trans/1000000)) +
   geom_line(color = "cornflowerblue") + 
   geom_point(color = "cornflowerblue") +
  geom_vline(aes(xintercept = 2012), color = "red",
             linetype="dashed", linewidth=0.5)+
  theme_bw()+  
   scale_x_continuous(breaks = breaks, labels = breaks)+
   
  labs(
    title = "Annual Cerrado Land Transition to Soy & Other Non-Native Cover",
    subtitle = "Aggregated from municipality-level MapBiomas",
    x = "",
    y = "Land Transition (Mha)"
  )
)

### 1.4.3 TerraBrasilis Deforestation --------

df_trans_deforest <- read.csv("../Data_Source/Terrabrasilis_CerradoDeforestation.csv")
(p_trans_deforest <-
    ggplot(df_trans_deforest, aes(x=year, y=area_km2)) +
    geom_line(color = "cornflowerblue") + 
    geom_point(color = "cornflowerblue") +
    geom_vline(aes(xintercept = 2012), color = "red",
               linetype="dashed", linewidth=0.5)+
    scale_x_continuous(breaks = seq(2000, 2022, 2), labels = seq(2000, 2022, 2))+
    theme_bw()+  
    labs(
      title = "Annual Cerrado Deforestation",
      subtitle = "Data Source: TerraBrasilis",
      x = "",
      y = "Deforestation (km^2)"
    )
)

# filter to only 2007-2017
df_trans_deforest <- df_trans_deforest %>% filter(year >= 2007 & year <= 2017)
(p_trans_deforest2 <-
    ggplot(df_trans_deforest, aes(x=year, y=area_km2)) +
    geom_line(color = "cornflowerblue") + 
    geom_point(color = "cornflowerblue") +
    geom_vline(aes(xintercept = 2012), color = "red",
               linetype="dashed", linewidth=0.5)+
    scale_x_continuous(breaks = breaks, labels = breaks)+
    theme_bw()+  
    labs(
      title = "Annual Cerrado Deforestation",
      subtitle = "Data Source: TerraBrasilis",
      x = "",
      y = "Deforestation (km^2)"
    )
)



## 1.5: Yield -------

### 1.5.1: National Yield ------

(p_yield_usbr<-
   ggplot(df_yield_USBR, aes(x=yr, y=yield, color = country)) +
   geom_line() + 
   geom_point() +  
   theme_bw()+
   scale_x_continuous(breaks = breaks, labels = breaks)+
   
   scale_color_manual(
     name = "Country",
     values = c(
       US = "salmon",
       Brazil = "cornflowerblue"),
     breaks = c("US", "Brazil"))+
   geom_vline(aes(xintercept = 2012), color = "red",linetype="dashed", linewidth=0.5, alpha = 0.5)+
   labs(
     title = "Annual National Soybean Yield",
     subtitle = "Data Sources: USDA-NASS & SIDRA",
     x = "",
     y = "Soybean Yield (kg/ha)"
   )
)

### 1.5.2 Regional Yield ------

(p_yield_usmwbrcerr<-
   ggplot(df_yield_USMW_BRCerr, aes(x=yr, y=yield, color = country)) +
   geom_line() + 
   geom_point() +  
   theme_bw()+
   scale_color_manual(
     name = "Country",
     values = c(
       US = "salmon",
       Brazil = "cornflowerblue"),
     breaks = c("US", "Brazil"))+
   geom_vline(aes(xintercept = 2012), color = "red",linetype="dashed", linewidth=0.5, alpha = 0.5)+
   scale_x_continuous(breaks = breaks, labels = breaks)+
   labs(
     title = "Annual Regional Soybean Yield",
     subtitle = "Data Sources: USDA-NASS & SIDRA",
     x = "",
     y = "Soybean Yield (kg/ha)"
   )
)

# 2: Arrange Plots ------------
library(patchwork)
p1 <- p_yield_usmwbrcerr +  p_yield_usbr + 
  p_prod_regional + p_prod_national +
  p_price_usmwbrcerr + p_exportqty_usbr_toworld + 
  p_exportvalue_usbr_toworld + p_exportvalue_usbr_tochina + 
  p_trans_deforest2 + p_trans_tosoy

p2 <- p1 +
  plot_layout(nrow = 5, guides = "collect") & 
  theme(legend.position = 'bottom') & 
  theme(legend.text = element_text(size = 15)) & 
  theme(legend.title = element_text(size = 15))

p2

ggsave(filename = "../Figures/soybeanstats.png",
       p2, height = 12, width = 15, 
       dpi = 300)  
