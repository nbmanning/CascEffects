# Section 00: Script Details ----------------

# title: 2_figure_change_export_value.R
# Purpose: Plot change in exports to top trade partners of US and Brazil. 
# This will be Figure 4
# Date: 7/10/24
# Last Updated: December 2024
# Author: Nick Manning

# REQUIRES:
## "../Data_Source/UNComtrade_USBR_Exports_20072019_sheet.xlsx", sheet = "RelevantExportData"
## "../Data_Source/FAOSTAT_BrUS_2000_2020_ExportQuantity.csv")
## ("../Data_Source/FAOSTAT_BRtoChina_ExportQuantity.csv")
## ("../Data_Source/FAOSTAT_BrUS_2000_2020_ExportQuantity.csv")
## ("../Data_Source/UNComtrade_USBR_HS1201_20072018.csv")

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
rm(list = ls())
getwd()

library(readxl)
library(dplyr)
library(ggplot2)
library(stringr)

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
  arrange(PartnerDesc, ReporterDesc, Period) %>% 
  mutate(ReporterDesc = str_replace_all(ReporterDesc, "USA", "US")) %>% 
  mutate(PartnerDesc = str_replace_all(PartnerDesc, "USA", "US"))
  

## 1.2 EDA  ------------------------
### 1.2.1 viewing top partners  ------------------------

### US ###
df_us <- df %>% 
  filter(ReporterDesc == "US" & PartnerDesc != "World") %>% 
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

# 2 Line Plots ------------------------

df_top <- rbind(df_us_top, df_br_top)

### FACET ###
(gg_export_line <-  ggplot(df_top, aes(x=Period, y=Fobvalue, color = PartnerDesc)) +
  geom_line() + 
  geom_point() +
  geom_vline(xintercept = 2012, color = "red", linetype="dashed", linewidth=0.5)+
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
    plot.title = element_blank(),
    plot.subtitle = element_blank()
  )
)

# save 
ggsave(
  gg_export_line,
  filename = "../Figures/exports_usbr.png",
  width = 10,
  height = 3.5
)

# 3: Difference Stats -----------

## 3.1: Difference in Export Value --------

### 3.1.1 BR --> World ---------

# e_v_brw means export_value_BrazilToWorld
e_v_brw <- df %>%
  filter(ReporterDesc == "Brazil" & PartnerDesc == "World") 


# Calc Differences from 2013 to 5-year average

# get 2013
e_v_brw_2013 <- e_v_brw %>% 
  filter(Period == 2013) %>% 
  select(Fobvalue) %>% 
  as.numeric()

# get 2012 
e_v_brw_2012 <- e_v_brw %>% 
  filter(Period == 2012) %>% 
  select(Fobvalue) %>% 
  as.numeric()

# 5-Year Average
e_v_brw_5yravg_20032012 <- e_v_brw %>% 
  filter(Period < 2013 & Period >= 2008) %>% 
  summarise(avg_prev5yr = mean(Fobvalue)) %>%
  as.numeric()

# n = value, p = %
diff5_n_v_brw <- e_v_brw_2013 - e_v_brw_5yravg_20032012
diff5_p_v_brw <- (diff5_n_v_brw/e_v_brw_5yravg_20032012) *100

# previous year average
diff1_p_v_brw <-  (e_v_brw_2013 - e_v_brw_2012) / e_v_brw_2012

### 3.1.2 BR --> China ---------
e_v_brc <- df_br %>%
  filter(ReporterDesc == "Brazil" & PartnerDesc == "China") #%>% 
#select(ReporterDesc, Period, annual_sum) 

# Calc Differences from 2013 to 5-year average

# get 2013
e_v_brc_2013 <- e_v_brc %>% 
  filter(Period == 2013) %>% 
  select(Fobvalue) %>% 
  as.numeric()

e_v_brc_2012 <- e_v_brc %>% 
  filter(Period == 2012) %>% 
  select(Fobvalue) %>% 
  as.numeric()

# 5-Year Average
e_v_brc_5yravg_20032012 <- e_v_brc %>% 
  filter(Period < 2013 & Period >= 2008)  %>% 
  summarise(avg_prev5yr = mean(Fobvalue)) %>%
  as.numeric()

diff5_n_v_brc <- e_v_brc_2013 - e_v_brc_5yravg_20032012
diff5_p_v_brc <- (diff5_n_v_brc/e_v_brc_5yravg_20032012) *100

diff1_p_v_brc <- (e_v_brc_2013 - e_v_brc_2012) / e_v_brc_2012
### 3.1.3 US --> World ---------

# e_v_usw means export_value_USToWorld
e_v_usw <- df %>%
  filter(ReporterDesc == "US" & PartnerDesc == "World") #%>% 
#select(ReporterDesc, Period, annual_sum) 

# Calc Differences from 2013 to 5-year average

# get 2013
e_v_usw_2013 <- e_v_usw %>% 
  filter(Period == 2013) %>% 
  select(Fobvalue) %>% 
  as.numeric()

# 5-Year Average
e_v_usw_5yravg_20032012 <- e_v_usw %>% 
  filter(Period < 2013 & Period >= 2008) %>% 
  summarise(avg_prev5yr = mean(Fobvalue)) %>%
  as.numeric()

# n = value, p = %
diff5_n_v_usw <- e_v_usw_2013 - e_v_usw_5yravg_20032012
diff5_p_v_usw <- (diff5_n_v_usw/e_v_usw_5yravg_20032012) *100

### 3.1.2 US --> China ---------
e_v_usc <- df_us %>%
  filter(ReporterDesc == "US" & PartnerDesc == "China") #%>% 
#select(ReporterDesc, Period, annual_sum) 

# Calc Differences from 2013 to 5-year average

# get 2013
e_v_usc_2013 <- e_v_usc %>% 
  filter(Period == 2013) %>% 
  select(Fobvalue) %>% 
  as.numeric()

e_v_usc_2012 <- e_v_usc %>% 
  filter(Period == 2012) %>% 
  select(Fobvalue) %>% 
  as.numeric()

# 5-Year Average
e_v_usc_5yravg_20032012 <- e_v_usc %>% 
  filter(Period < 2013 & Period >= 2008)  %>% 
  summarise(avg_prev5yr = mean(Fobvalue)) %>%
  as.numeric()

diff5_n_v_usc <- e_v_usc_2013 - e_v_usc_5yravg_20032012
diff5_p_v_usc <- (diff5_n_v_usc/e_v_usc_5yravg_20032012) *100

diff1_p_v_usc <- (e_v_usc_2013 - e_v_usc_2012) / e_v_usc_2012

## 3.2 Difference in Export Quantity ----------

### 3.2.1 BR --> World ---------
# Import CSV
exports_usbr <-read.csv("../Data_Source/FAOSTAT_BrUS_2000_2020_ExportQuantity.csv")
exports_usbr<- exports_usbr %>% 
  select(Area, Year, Element, Value) %>% 
  filter(Year >= 2003 & Year <= 2017)

# Calc Differences from 2013 to 5-year average
e_q_brw_2013 <- exports_usbr %>% 
  filter(Year == 2013 & Area == "Brazil") %>% 
  select(Value) %>% 
  as.numeric()

e_q_brw_2012 <- exports_usbr %>% 
  filter(Year == 2012 & Area == "Brazil") %>% 
  select(Value) %>% 
  as.numeric()

# 5-Year Average
e_q_brw_5yravg_20032012 <- exports_usbr %>% 
  filter(Year < 2013 & Year >= 2008 & Area == "Brazil") %>% 
  summarise(avg_prev5yr = mean(Value)) %>%
  as.numeric()

diff5_n_q_brw <- e_q_brw_2013 - e_q_brw_5yravg_20032012
diff5_p_q_brw <- (diff5_n_q_brw/e_q_brw_5yravg_20032012) *100

# vs. Previous Year
diff1_p_q_brw <- (e_q_brw_2013 - e_q_brw_2012) / e_q_brw_2012
diff5_p_q_brw <- (diff5_n_q_brw/e_q_brw_5yravg_20032012) *100


# find change from 2012-2013 in percent then do the same for others 

### 3.2.2 BR --> China ---------
# Import CSV
exports_br_to_china <-read.csv("../Data_Source/FAOSTAT_BRtoChina_ExportQuantity.csv")
exports_br_to_china<- exports_br_to_china %>% 
  select(Reporter.Countries, Partner.Countries, Year, Element, Value) %>% 
  filter(Year >= 2003 & Year <= 2017) %>% 
  filter(Partner.Countries== "China, mainland")

# Calc Differences from 2013 to 5-year average

e_q_brc_2013 <- exports_br_to_china %>% 
  filter(Year == 2013) %>% 
  select(Value) %>% 
  as.numeric()

e_q_brc_2012 <- exports_br_to_china %>% 
  filter(Year == 2012) %>% 
  select(Value) %>% 
  as.numeric()

# 5-Year Average
e_q_brc_5yravg_20032012 <- exports_usbr %>% 
  filter(Year < 2013 & Year >= 2008 & Area == "Brazil") %>% 
  summarise(avg_prev5yr = mean(Value)) %>%
  as.numeric()

diff5_n_q_brc <- e_q_brc_2013 - e_q_brc_5yravg_20032012
diff5_p_q_brc <- (diff5_n_q_brc/e_q_brc_5yravg_20032012) *100

diff1_p_q_brc <- (e_q_brc_2013 - e_q_brc_2012) / e_q_brc_2012

### 3.2.3 US --> World ---------
# Import CSV
#exports_usbr <-read.csv("../Data_Source/FAOSTAT_BrUS_2000_2020_ExportQuantity.csv")
exports_usbr <-read.csv("../Data_Source/FAOSTAT_BrUS_2000_2020_ExportQuantity.csv")
exports_usbr<- exports_usbr %>% 
  select(Area, Year, Element, Value) %>% 
  filter(Year >= 2003 & Year <= 2017)

e_q_usw_2013 <- exports_usbr %>% 
  filter(Year == 2013 & Area == "United States of America") %>% 
  select(Value) %>% 
  as.numeric()

# 5-Year Average
e_q_usw_5yravg_20032012 <- exports_usbr %>% 
  filter(Year < 2013 & Year >= 2008 & Area == "United States of America") %>% 
  summarise(avg_prev5yr = mean(Value)) %>%
  as.numeric()

diff5_n_q_usw <- e_q_usw_2013 - e_q_usw_5yravg_20032012
diff5_p_q_usw <- (diff5_n_q_usw/e_q_usw_5yravg_20032012) *100


### 3.2.2 US --> China ---------
# Import CSV
exports_us_to_china <-read.csv("../Data_Source/UNComtrade_USBR_HS1201_20072018.csv")
exports_us_to_china<- exports_us_to_china %>% 
  select(ReporterDesc, PartnerDesc, Period, CmdDesc, Qty) %>% 
  filter(Period >= 2003 & Period <= 2017) %>% 
  filter(PartnerDesc == "China") %>% 
  filter(ReporterDesc == "US")

e_q_usc_2013 <- exports_us_to_china %>% 
  filter(Period == 2013) %>% 
  select(Qty) %>% 
  as.numeric()

# 5-Year Average
e_q_usc_5yravg_20082012 <- exports_us_to_china %>% 
  filter(Period < 2013 & Period >= 2008 & ReporterDesc == "US") %>% 
  summarise(avg_prev5yr = mean(Qty)) %>%
  as.numeric()

diff5_n_q_usc <- e_q_usc_2013 - e_q_usc_5yravg_20082012
diff5_p_q_usc <- (diff5_n_q_usc/e_q_usc_5yravg_20082012) *100

# 4: Final Line Plot ---- 

## 4.1: Get BR/US Trade with RoW ------

### 4.1.1: Calc. Trade -----
## A) BR / US Country changes and B) Br / US World Changes

### WORLD ###
df_usbr_world <- df %>% 
  filter(PartnerDesc == "World") %>% 
  filter(Period %in% years_of_interest) %>% 
  # add annual percentage for each country
  group_by(Period) %>%
  mutate(annual_sum = sum(Fobvalue),
         AnnualPercent = (Fobvalue / annual_sum) * 100) %>%
  ungroup() %>%
  #select(-annual_sum) %>%   # Remove annual_sum column if not needed
  mutate(AnnualPercent = round(AnnualPercent, 2))

# calculate differences
df_usbr_world <- df_usbr_world %>% 
  group_by(ReporterDesc, PartnerDesc) %>% 
  mutate(
    # Difference = 2022 - 2021 per country
    ValueDiff = Fobvalue - lag(Fobvalue),
    # Pct Difference = ((2022-2021)/2021)*100 per country
    ValueDiffPct = ((Fobvalue - lag(Fobvalue))/lag(Fobvalue))*100)


### 4.1.2: Plot -----
# set up legend manually
col_US = "darkgoldenrod"
col_BR = "darkslateblue"

# Plot
gg_export_line_usbrworld <-  ggplot(df_usbr_world, aes(x=Period, y=Fobvalue, color = ReporterDesc)) +
  geom_line() + 
  geom_point() +
  geom_vline(xintercept = 2012, color = "red", linetype="dashed", linewidth=0.5)+
  theme_bw()+
  scale_color_manual(
    name = "Country",
    values = c(
      US = col_US,
      Brazil = col_BR),
    breaks = c("US", "Brazil"))+
  #facet_wrap(vars(ReporterDesc))+
  labs(
    title = "Annual Total Soybean Export Value to All Trading Partners",
    subtitle = "Data Source: UN Comtrade",
    x = "",
    y = "Total Export Value, FOB (USD)",
  )+
  theme(
    legend.title = element_blank(),
    legend.text = element_text(size = 14),
    legend.position = c(.90, .20),
    axis.text = element_text(size = 9),
    axis.title = element_text(size = 10),
    strip.text.x = element_text(size = 12, colour = "black"),
    #plot.title = element_text(size = 14),
    #plot.subtitle = element_text(size = 11)
    plot.title = element_blank(),
    plot.subtitle = element_blank()
  )

gg_export_line_usbrworld


## 4.2: Combine Plots -----

# Combine them vertically using plot_grid
combined_plot <- plot_grid(
  gg_export_line,
  gg_export_line_usbrworld,
  ncol = 1,
  labels = c("a", "b") # Optional: adds subplot labels
)

# Display the combined plot
print(combined_plot)

## 4.3: Save Final Combined Plot ------
# save 
ggsave(
  combined_plot,
  filename = "../Figures/exports_usbr_patch2.png",
  width = 14,
  height = 6
)



############################
# 5: In-Text ------
# When compared to the previous 5-year averages, in the year following the 
# US drought there was an increase in the quantity and value of soybean exports from 
# Brazil to the rest of the world of 58% and 70% (respectively) and export quantity and value 
# to China increased 66% and 71%.

# Delete?? -------
# 6: Global Export Leaders -- are they Brazil and the US? -------
## 5.0 - get world imp and exp numbers  
raw <- read_excel("../Data_Source/UNComtrade_ImpExp_20232024.xlsx", sheet = "Sheet1")

# get the summed total imports
world_imports <- raw %>% 
  select(reporterDesc, partnerDesc, period, fobvalue, qty, flowDesc) %>%
  filter(flowDesc == "Import") %>% 
  filter(partnerDesc == "World") %>% 
  filter(period == 2023) %>%
  arrange(partnerDesc, reporterDesc, period) %>% 
  group_by(partnerDesc, period) %>% 
  summarise(qty = sum(qty, na.rm = T), fobvalue = sum(fobvalue, na.rm = T))

### PICK UP HERE #############
# You were trying to set up world maxes first to divide into percentages
num_imp_world_2023_qty <- world_imports$qty
num_imp_world_2023_value <- world_imports$fobvalue


# get the summed total world exports
world_exports <- raw %>% 
  select(reporterDesc, partnerDesc, period, fobvalue, qty, flowDesc) %>%
  filter(flowDesc == "Export") %>% 
  filter(partnerDesc == "World") %>% 
  filter(period == 2023) %>%
  arrange(partnerDesc, reporterDesc, period) %>% 
  group_by(partnerDesc, period) %>% 
  summarise(qty = sum(qty, na.rm = T), fobvalue = sum(fobvalue, na.rm = T))

num_exp_world_2023_qty <- world_exports$qty
num_exp_world_2023_value <- world_exports$fobvalue



## 6.1: Global Import Leaders -- are they China and the EU? -------
raw <- read_excel("../Data_Source/UNComtrade_ImpExp_20232024.xlsx", sheet = "Sheet1")

df <- raw %>% 
  select(reporterDesc, partnerDesc, period, fobvalue, qty, flowDesc) %>%
  filter(flowDesc == "Import") %>% 
  #filter(partnerDesc == "World") %>% 
  filter(period == 2023) %>%
  arrange(partnerDesc, reporterDesc, period)


###
# find all EU states that have traded with BR and US 
list_eu_partners <- intersect(list_eu, df$reporterDesc)

# get the sum of all EU countries by trading partner by year (e.g. different sum for all BR2012 and US2012)
df_eu <- df %>% 
  filter(reporterDesc %in% list_eu_partners) %>% 
  group_by(partnerDesc, period) %>% 
  summarise(qty = sum(qty, na.rm = T), fobvalue = sum(fobvalue, na.rm = T)) %>% 
  #summarise(fobvalue = sum(fobvalue, na.rm = T)) %>% 
  mutate(reporterDesc = "EU", flowDesc = "Import")

df2_imp <- rbind(df, df_eu)



df2_imp <- df2_imp %>% mutate(
  pct_qty = (qty/num_imp_world_2023_qty)*100,
  pct_fobvalue = (fobvalue/num_imp_world_2023_value)*100
  )
  

## 6.1: Global Export Leaders -- are they Brazil and the US? (TO DO: CHANGE IMP TO EXP xx) -------
raw <- read_excel("../Data_Source/UNComtrade_ImpExp_20232024.xlsx", sheet = "Sheet1")

df <- raw %>% 
  select(reporterDesc, partnerDesc, period, fobvalue, qty, flowDesc) %>%
  filter(flowDesc == "Export") %>% 
  #filter(partnerDesc == "World") %>% 
  filter(period == 2023) %>%
  arrange(partnerDesc, reporterDesc, period)


###
# find all EU states that have traded with BR and US 
list_eu_partners <- intersect(list_eu, df$partnerDesc)

# get the sum of all EU countries by trading partner by year (e.g. different sum for all BR2012 and US2012)
df_eu <- df %>% 
  filter(partnerDesc %in% list_eu_partners) %>% 
  group_by(reporterDesc, period) %>% 
  summarise(qty = sum(qty, na.rm = T), fobvalue = sum(fobvalue, na.rm = T)) %>% 
  #summarise(fobvalue = sum(fobvalue, na.rm = T)) %>% 
  mutate(partnerDesc = "EU", flowDesc = "Export")

df2_exp <- rbind(df, df_eu)



df2_exp <- df2_exp %>% mutate(
  pct_qty = (qty/num_exp_world_2023_qty)*100,
  pct_fobvalue = (fobvalue/num_exp_world_2023_value)*100
)

df2_eu_usbr <- df2_exp %>% 
  filter()

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

df_eu_all <- df %>% 
  filter(PartnerDesc %in% list_eu_partners)

df_eu_all2 <- df_eu_all %>% 
  
  #filter(Period %in% years_of_interest) %>% 
  filter(Period == 2012 | Period == 2013) %>% 
  
  # calculate differences 
  group_by(ReporterDesc, PartnerDesc) %>% 
  mutate(
    # Difference = 2022 - 2021 per country
    ValueDiff = Fobvalue - lag(Fobvalue),
    # Pct Difference = ((2022-2021)/2021)*100 per country
    ValueDiffPct = ((Fobvalue - lag(Fobvalue))/lag(Fobvalue))*100)

df_eu_all2_2013 <- df_eu_all2 %>% 
  filter(Period == 2013) %>%
  na.omit() %>% 
  filter(abs(ValueDiff) > 100000)
  

gg_export_eu_2013 <- ggplot(df_eu_all2_2013, aes(x = PartnerDesc, y = ValueDiff, fill = ReporterDesc))+
  geom_bar(stat = "identity", position = position_dodge())+
  scale_fill_manual(values = c("Brazil" = "cornflowerblue", "USA" = "salmon"))+
  theme_minimal()+
  labs(x = "", y = "Export Value, FOB (USD)", fill = "",
       title = "Difference Between 2013 and 2012 Brazil / USA Soybean Export Values to EU Countries",
       subtitle = "Data Source: UN Comtrade; Values <$100,000 removed")+
  #theme(legend.position = "top")
  theme(
      legend.title = element_blank(),
      legend.text = element_text(size = 10),
      axis.text = element_text(size = 9),
      axis.title = element_text(size = 10),
      strip.text.x = element_text(size = 12, colour = "black"),
      plot.title = element_text(size = 14),
      plot.subtitle = element_text(size = 11)
    #legend.position = c(0.05, 0.95)
        )

library(patchwork)

p1 <- gg_export_line + gg_export_eu_2013
p1 <- p1 + 
  plot_layout(ncol = 1) +  
  plot_annotation(tag_levels = 'A') 

p1

# save 
ggsave(
  p1,
  filename = "../Figures/exports_usbr_patch.png",
  width = 11,
  height = 6
)

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


# G4: Other text -----
# from "UNComtrade_USBR_Exports_20072019_sheet.xlsx"
24553595386 # US 2013 Export Qty --> China
39401117964 # US 2013 Export Qty --> world
24553595386/39401117964

32468028 # BR 2012 Export Qty --> World (from FAOSTAT, missing from UN Comtrade)
42796106 # BR 2013 Export Qty --> World (from FAOSTAT, missing from UN Comtrade)
17449785616 # BR 2012 Export Value --> World
22812299141 # BR 2013 Export Value --> World