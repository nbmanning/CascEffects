# title: 2_figure_change_export_value.R
# Purpose: Plot change in exports to top trade partners of US and Brazil. 
# This will be Figure 4
# Date: 7/10/24
# Last Updated: December 2024
# Author: Nick Manning

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
rm(list = ls())
getwd()

library(readxl)
library(dplyr)
library(ggplot2)

# 0 Set Constants  ------------------------
# list all EU member states
list_eu <- c("Austria", "Belgium", "Bulgaria", "Croatia", "Cyprus", "Czech Republic", "Denmark", "Estonia", 
             "Finland", "France", "Germany", "Greece", "Hungary", "Ireland", "Italy", "Latvia", "Lithuania", 
             "Luxembourg", "Malta", "Netherlands", "Poland", "Portugal", "Romania", "Slovakia", "Slovenia", 
             "Spain", "Sweden")

# also list single countries of interest
list_top_partners <- c("China", "United Kingdom", "Sweden") #CHN, GBR, SWE

# years of interest for filtering 
years_of_interest <- c(2010, 2011, 2012, 2013, 2014, 2015)

# 1 Load & Prep Data ------------------------

raw <- read_excel("../Data_Source/UNComtrade_USBR_Exports_20072019_sheet.xlsx", sheet = "RelevantExportData")

df <- raw %>% 
  select(ReporterDesc, PartnerDesc, Period, Fobvalue) %>% 
  arrange(PartnerDesc, ReporterDesc, Period)

## 1.2 EDA  ------------------------
### 1.2.1 viewing top partners  ------------------------

### USA ###
df_us <- df %>% 
  filter(ReporterDesc == "USA" & PartnerDesc != "World") %>% 
  filter(Period %in% years_of_interest) %>% 
  # add annual percentage for each country
  group_by(Period) %>%
  mutate(annual_sum = sum(Fobvalue),
         AnnualPercent = (Fobvalue / annual_sum) * 100) %>%
  ungroup() %>%
  #select(-annual_sum) %>%   # Remove annual_sum column if not needed
  mutate(AnnualPercent = round(AnnualPercent, 2))

# get top 6 trade partners 
df_us_top <- df_us %>% 
  group_by(Period) %>% 
  arrange(desc(AnnualPercent)) %>%
  slice_head(n = 5) %>% 
  ungroup()

# get totals for each year 
df_us_totals <- df_us %>% 
  group_by(Period)


### Brazil ### 
df_br <- df %>% 
  filter(ReporterDesc == "Brazil" & PartnerDesc != "World") %>% 
  filter(Period %in% years_of_interest) %>% 
  # add annual percentage for each country
  group_by(Period) %>%
  mutate(annual_sum = sum(Fobvalue),
         AnnualPercent = (Fobvalue / annual_sum) * 100) %>%
  ungroup() %>%
  #select(-annual_sum) %>%   # Remove annual_sum column if not needed
  mutate(AnnualPercent = round(AnnualPercent, 2))

df_br_top <- df_br %>% 
  group_by(Period) %>% 
  arrange(desc(AnnualPercent)) %>%
  slice_head(n = 5) %>% 
  ungroup()

## 3.1 Line Plots ------------------------

df_top <- rbind(df_us_top, df_br_top)

### FACET ###
ggplot(df_top, aes(x=Period, y=Fobvalue, color = PartnerDesc)) +
  geom_line() + 
  geom_point() +
  geom_vline(aes(xintercept = 2012), color = "red", linetype="dashed", linewidth=0.5)+
  theme_bw()+
  facet_wrap(vars(ReporterDesc))+
  labs(
    title = "Annual Soybean Export Value to Top Trade Partners",
    subtitle = "Data Source: UN Comtrade",
    x = "",
    y = "Export Value, FOB (USD)",
  )+
  theme(
    legend.title = element_blank(),
    legend.text = element_text(size = 10),
    axis.text = element_text(size = 9),
    axis.title = element_text(size = 10),
    strip.text.x = element_text(size = 12, colour = "black"),
    plot.title = element_text(size = 14),
    plot.subtitle = element_text(size = 11)
  )

# save 
ggsave(
  filename = "../Figures/exports_usbr.png",
  width = 10,
  height = 3.5
)

# GRAVEYARD -------------------

# G1: Get Difference Stats -----------------
# USA
df_diff_us <- df_us %>% 
  group_by(PartnerDesc) %>% 
  mutate(
    # Difference = 2022 - 2021 per country
    ValueDiff = Fobvalue - lag(Fobvalue),
    # Pct Difference = ((2022-2021)/2021)*100 per country
    ValueDiffPct = ((Fobvalue - lag(Fobvalue))/lag(Fobvalue))*100)

# Brazil
df_diff_br <- df_br %>% 
  group_by(PartnerDesc) %>% 
  mutate(
    # Difference = 2022 - 2021 per country
    ValueDiff = Fobvalue - lag(Fobvalue),
    # Pct Difference = ((2022-2021)/2021)*100 per country
    ValueDiffPct = ((Fobvalue - lag(Fobvalue))/lag(Fobvalue))*100)

# G2: Find Individual Country Changes (EU, UK, China, Sweden) ------------------------

# create function to filter to one country pair and calculate agg stats 
F_CalcDiff <- function(country_of_interest){
  df_country <- df %>% 
    # filter to country of interest 
    filter(PartnerDesc == country_of_interest) %>%
    filter(Period %in% years_of_interest) %>% 
    
    # calculate differences 
    group_by(ReporterDesc, PartnerDesc) %>% 
    mutate(
      # Difference = 2022 - 2021 per country
      ValueDiff = Fobvalue - lag(Fobvalue),
      # Pct Difference = ((2022-2021)/2021)*100 per country
      ValueDiffPct = ((Fobvalue - lag(Fobvalue))/lag(Fobvalue))*100)
  return(df_country)
}

## China -------
df_china <- F_CalcDiff("China")

# NOTE: I planned to make plots for UK and Sweden based on Eliasson et al., 
# 2023 (https://www.sciencedirect.com/science/article/pii/S0959652622047692), but decided
# against it because these are very small % of total trade vlaue for US and Brazil
# ## UK ---
# df_uk <- F_CalcDiff("United Kingdom")
# 
# ## Sweden --
# df_sweden <- F_CalcDiff("Sweden")

## EU --------

# NOTE:
## EU comprised 10.9% of Brazil's 2013 total value and declined 4.4% from 2012-2013
## EU comprised 6.7% of US total 2013 value and increased 4.6% from 2012-2013

# pre-processing

# find all EU states that have traded with BR and US 
list_eu_partners <- intersect(list_eu, df$PartnerDesc)

# get the sum of all EU countries by trading partner by year (e.g. different sum for all BR2012 and US2012)
df_eu <- df %>% 
  filter(PartnerDesc %in% list_eu_partners) %>% 
  group_by(ReporterDesc, Period) %>% 
  summarise(Fobvalue = sum(Fobvalue)) %>% 
  mutate(PartnerDesc = "EU")

# use previous function to get trade differences
df_eu <- df_eu %>% 
  
  filter(Period %in% years_of_interest) %>% 
  
  # calculate differences 
  group_by(ReporterDesc, PartnerDesc) %>% 
  mutate(
    # Difference = 2022 - 2021 per country
    ValueDiff = Fobvalue - lag(Fobvalue),
    # Pct Difference = ((2022-2021)/2021)*100 per country
    ValueDiffPct = ((Fobvalue - lag(Fobvalue))/lag(Fobvalue))*100)

## Other Asian Countries -----


# G3 Make Plots ------------------------

# function from 2_1_figure_lineplots

breaks <- c(2007, 2012, 2017)

F_plot_comp <- function(df_c){
  p <- ggplot(df_c, aes(x=Period, y=Fobvalue, color = ReporterDesc)) +
    geom_line() + 
    geom_point() +
    geom_vline(aes(xintercept = 2012), color = "red", linetype="dashed", linewidth=0.5)+
    theme_bw()+
    #scale_x_continuous(breaks = breaks, labels = breaks)+
    scale_color_manual(
      name = "Country",
      values = c(
        USA = "salmon",
        Brazil = "cornflowerblue"),
      breaks = c("USA", "Brazil"))+
    labs(
      title = "Annual Soybean Export Value to China",
      subtitle = "Data Source: UN Comtrade",
      x = "",
      y = "Export Value (USD)"
    )
  
  return(p)
  
}

# China
F_plot_comp(df_china)
