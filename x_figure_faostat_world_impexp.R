# title: 2_5_plot_faostat_world_impexp.R
# author: Nick Manning
# purpose: quick script to plot faostat world imports and exports to emphasize that
# we are living in an increasingly interconnected world

# creation date: 3/23/24
# last updated: March 2024

# links:
# FAOSTAT: https://www.fao.org/faostat/en/#data/TCL
## Regions: World + (Total)
## Items Aggreagted:Crops and livestock products > (List) --> Cereals + (Total)
## Years: Select All
## Elements: Import Quantity & Export Quantity

# Load Libraries ---------

library(ggplot2)
library(dplyr)
library(stringr)
library(ggnewscale)
library(lubridate)

# 1: Read & Clean CSV -----
raw <- read.csv("../Data_Source/FAOSTAT_world_cereals_20240323.csv")

# clean data 
df <- raw %>% 
  select(c("Area", "Year", "Element", "Value")) %>% 
  rename(
    "country" = "Area",
    "yr" = "Year",
    "value" = "Value",
    "var" = "Element"
  ) %>% 
  # Shorten, e.g. change "Import Quantity" to "Import"
  mutate(
    #yr = as.Date(paste(yr, 1, 1, sep = "-")),
    var = gsub(" Quantity", "", var)
  )

# 2: plot time series with intervention ----------
ggplot(df, aes(x=yr, y = value, color = var)) +
  geom_line(lwd = 1.5) + 
  geom_point(size = 2) +
  # exports first, then imports 
  scale_color_manual(values=c("#CC6666", "#9999CC"))+
  theme_minimal()+
  labs(
    title = "Global Aggregated Trade (Cereals)",
    subtitle = "Source: FAOSTAT",
    color = "",
    x = "",
    y = "Quantity (tons)")+
  theme(
    #legend.position = "bottom",
    legend.position = c(0.8, 0.3),
    plot.title = element_text(size = 20, hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    legend.text = element_text(size = 20),
    axis.text = element_text(size = 14),
    axis.title.y = element_text(size = 14)
        )

# 3: Save --------------
ggsave(
  filename = "../Figures/globaltrade_faostat.png",
  width = 12,
  height = 4
)
