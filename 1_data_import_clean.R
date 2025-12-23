# Section 00: Script Details ----------------

# title: 1_data_import_clean.R
# author: Nick Manning
# purpose: Import and clean only the data that we want to make line plots with

# Notes:
## Use of USDA QuickStats requires a Key (free API token) to use 'tidyusda' package from USDA-NASS QUickStats API: https://quickstats.nass.usda.gov/api
## Use of datazoom.amazonia requires an OAuth Token to link with Google Drive

# Last Updated: Dec 2025

# XX REQUIRES:
## "../Data_Source/br_st_abbv.csv" -- A CSV of the abbreviations for the different states

## Run 0_xx first to get CSVs of geocodes from different spatial intersections, will be used to filter here

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

rm(list = ls())
getwd()

# 0: Load Libraries & Set Constants and Paths --------------

## Libraries ----
library(ggplot2) # plotting
library(dplyr) # data cleaning
library(tidyUSDA) # prod & price data
library(sidrar) # production (acres planted & production)
library(datazoom.amazonia) # mapbiomas data

library(geobr) # load BR shapefiles 
library(sf) # intersect

## Constants -----
year_range <- 2000:2024
BR_abbvs <- read.csv("../Data_Source/br_st_abbv.csv", header = T)

micro_codes_names_cerr <- read.csv(file = "../Data_Source/microregion_codes_names_cerrado.csv")
micro_codes_cerr <- read.csv("../Data_Source/microregion_codesonly_cerrado.csv")
micro_codes_cerr <- micro_codes_cerr$x



BRCerr_abbvs <- filter(BR_abbvs, biome == "Cerrado")
BRCerr_state_abbvs <- BRCerr_abbvs$state

# get key (free API token) to use 'tidyusda' package from USDA-NASS QUickStats API: https://quickstats.nass.usda.gov/api
usda_key <- "34BD2DD3-9049-37A1-BC2C-D8A967E25E42"

## Paths -----
# NOTE: Users should change these to their local file structure
folder_source <- "../Data_Source/"
folder_derived <- "../Data_Derived/"
folder_figures <- "../Figures/"

# 1: Production (mt) ---------------

## 1.1: US Prod (US-MW States) -----

## DATA SOURCE: USDA QuickStats, accessed through tidyUSDA R Package (Lindblad, 2023): https://bradlindblad.github.io/tidyUSDA/

### NOTE: prod comes in bushels but we convert to metric tons

# get raw Production data
raw_prod_USMW <- getQuickstat(
  key = usda_key,
  program = "SURVEY",
  data_item = "SOYBEANS - PRODUCTION, MEASURED IN BU",
  commodity = "SOYBEANS",
  geographic_level = "STATE",
  year = paste(year_range),
  geometry = F)  

# Clean Data
prod_USMW_states <- raw_prod_USMW %>% 
  
  # filter to states within US-MW
  filter(state_name %in% c("NORTH DAKOTA", "SOUTH DAKOTA", "NEBRASKA", "KANSAS", "MISSOURI",
                                   "IOWA", "MINNESOTA", "WISCONSIN", "ILLINOIS", "INDIANA", "OHIO", "MICHIGAN")) %>% 
  # clean to remove Prospects (Sept/Oct/Nov Crop Outlooks)
  filter(reference_period_desc == "YEAR") %>% 
  
  # select certain columns and rename for clarity
  select(c("year", "state_alpha", "Value")) %>% 
  rename("yr" = "year",
         "state" = "state_alpha",
         "prod" = "Value") %>% 
  
  # convert from bushels to metric tons source: https://grains.org/markets-tools-data/tools/converting-grain-units/
  mutate(prod = prod*0.0272155,
         country = "US") 

# summarize to regional level
prod_USMW <- prod_USMW_states %>% 
  group_by(yr, country) %>%
  dplyr::summarize(prod = round(sum(prod), digits = 2)) %>% 
  mutate(description = "US-MW States")


## 1.2: BR Prod (Cerrado Microregions) ------------------
raw_sidra_micro <- get_sidra(x = 1612,
                       variable =  c(214, 109), # production and yield # or for first six (excluding value of production) c(109, 1000109, 216, 1000216,214, 112)
                       period = as.character(year_range), #2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021
                       geo = "MicroRegion", # Also either Brazil, State, or Município
                       geo.filter = NULL,
                       classific = "c81",
                       category = list(2713), # Soja (em grão)
                       header = T,
                       format = 3)

# clean production columns (colnames(raw_sidra))
prod_BRCerr <- raw_sidra_micro %>% 
  # select MicroReg Code, MicroReg Name, Year, Variable, Value
  select("Microrregião Geográfica (Código)", "Microrregião Geográfica", "Ano", "Variável", "Valor") %>% 
  
  # translate by renaming
  rename(
    "code_micro" = "Microrregião Geográfica (Código)",
    "name_micro_long" = "Microrregião Geográfica",
    "yr" = "Ano",
    "variable" = "Variável",
    "value" = "Valor") %>%  
  # translate by renaming 
  mutate(variable = str_replace(variable, "Quantidade produzida", "prod"),
         variable = str_replace(variable, "Área plantada", "ha_planted"),
         yr = as.double(yr)) %>% 
  select(.,c("yr", "variable", "value", "name_micro_long", "code_micro"))

prod_BRCerr$code_micro <- as.double(prod_BRCerr$code_micro)

# add names of microregions only in the Cerrado to filter
prod_BRCerr <- left_join(prod_BRCerr, micro_codes_names_cerr)

prod_BRCerr_micro <- filter(prod_BRCerr, code_micro %in% micro_codes_cerr)

# make data wide to match US data and make it easier to merge
prod_BRCerr_micro <- pivot_wider(prod_BRCerr_micro, names_from = "variable")

# add country and filter to the same variables as US
prod_BRCerr_micro <- prod_BRCerr_micro %>% 
  mutate(country = "Brazil") %>% 
  select(yr, name_micro, prod, country)

# summarize to regional level
prod_BRCerr_micro <- prod_BRCerr_micro %>% 
  na.omit() %>% 
  group_by(yr) %>%
  dplyr::summarize(prod = round(sum(prod), digits = 2)) %>% 
  mutate(description = "MicroRegions with Cerrrado",
         country = "Brazil")

## 1.3: Get Prod (USMW & BRCerr) ----
df_prod_USMW_BRCerr <- rbind(prod_BRCerr_micro, prod_USMW)

# filter to time frame
df_prod_USMW_BRCerr <- df_prod_USMW_BRCerr  %>% 
  filter(yr >= 2007 & yr <= 2017)

## 1.4: Nationwide Production -----
### 1.4.1: BR Prod ------
raw_sidra_BR <- get_sidra(x = 1612, 
                       variable = 214, #109 is yield # production and yield # or for first six (excluding value of production) c(109, 1000109, 216, 1000216,214, 112) 
                       period = as.character(year_range), #2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021
                       geo = "Brazil", # Brazil, State, or Município
                       geo.filter = NULL,
                       classific = "c81",
                       category = list(2713), # Soja (em grão)
                       header = T,
                       format = 3)

# clean SIDRA prod. stats
prod_BR <- raw_sidra_BR %>% 
  select("Nível Territorial", "Ano", "Valor") %>% 
  rename(
    "country" = "Nível Territorial",
    "yr" = "Ano",    
    "prod" = "Valor") %>% 
  mutate(yr = as.double(yr))

### 1.4.2: US Prod ------
# download
raw_prod_US <- getQuickstat(
  key = usda_key,
  program = "SURVEY",
  data_item = "SOYBEANS - PRODUCTION, MEASURED IN BU",
  commodity = "SOYBEANS",
  geographic_level = "NATIONAL",
  year = paste(year_range),
    geometry = F)  

# clean
prod_US <- raw_prod_US %>% 

  # clean to remove Prospects (Sept/Oct/Nov Crop Outlooks)
  filter(reference_period_desc == "YEAR") %>% 
  
  # select certain columns and rename for clarity
  select(c("year", "state_alpha", "Value")) %>% 
  rename("yr" = "year",
         "country" = "state_alpha",
         "prod" = "Value") %>% 
  
  # convert from bushels to metric tons source: https://grains.org/markets-tools-data/tools/converting-grain-units/
  mutate(prod = prod*0.0272155)

### 1.4.3: Get US & BR National Production ---------
df_prod_USBR <- rbind(prod_BR, prod_US)
df_prod_USBR <- df_prod_USBR %>% 
  filter(yr >= 2007 & yr <= 2017) %>%  
  mutate(country = str_replace_all(country, "Brasil", "Brazil"))



# 2: Exports (Value; USD) ------------

# NOTE: Manually replaced World Exports for Brazil with the sum of all countries because it was missing from Comtrade

# Data Source: Export Data from UN Comtrade 
raw_exports <- read.csv("../Data_Source/UNComtrade_USBR_HS1201_20072018.csv")

## 2.1: Exports to World --------
# filter raw data  
exports_usbr <- raw_exports %>% 
  select(c("Period", "ReporterDesc", "PartnerISO", "PrimaryValue")) %>% 
  filter(ReporterDesc %in% c("USA", "Brazil")) %>%
  filter(Period >= 2007 & Period <= 2017) %>% 
  mutate(ReporterDesc = str_replace_all(ReporterDesc, "USA", "US"))

# get the world Value of exports since UN Comtrade was missing Qty
exports_world <- exports_usbr %>% 
  filter(PartnerISO == "W00")

## 2.2: Export (Value) to China ----
# filter to just USA / BR exports to China
exports_usbr_china <- exports_usbr %>%
  filter(PartnerISO == "CHN")

## 2.3: Get Exports ---------
df_exports_USBR_world <- exports_world
df_exports_USBR_world <- df_exports_USBR_world %>% 
  filter(Period >= 2007 & Period <= 2017) %>%  
  mutate(ReporterDesc = str_replace_all(ReporterDesc, "USA", "US"),
         description = "Export Value (USD) from US and Brazil to the World")

df_exports_USBR_china <- exports_usbr_china
df_exports_USBR_china <- df_exports_USBR_china %>% 
  filter(Period >= 2007 & Period <= 2017) %>%  
  mutate(ReporterDesc = str_replace_all(ReporterDesc, "USA", "US"),
         description = "Export Value (USD) from US and Brazil to China")

# 3: Price (USD / bu) ---------------------

## 3.1: US Price -------

### 3.1.1: US-MW Regional Price (USD/bu) ---------

# note: daily is available from macrotrends (see thesis data), but USDA is more reliable

### Source: USDA QuickStat
### Description: Average of state-wide prices at the annual level

# get raw data from QuickStats
raw_price_USMWst <- getQuickstat(
  key = usda_key,
  program = "SURVEY",
  data_item = "SOYBEANS - PRICE RECEIVED, MEASURED IN $ / BU",
  commodity = "SOYBEANS",
  geographic_level = "STATE",
  year = paste(year_range),
  geometry = F) %>%
  # keep only some of the variables
  dplyr::select(
    year, county_name, county_code, state_name, state_alpha,
    state_fips_code, short_desc, freq_desc, reference_period_desc, Value) %>%
  # rename the Value column
  dplyr::rename(price = Value)

# clean 
price_USMW_states <- raw_price_USMWst %>% 
  filter(freq_desc == "MONTHLY") %>% 
  filter(state_name %in% c("NORTH DAKOTA", "SOUTH DAKOTA", "NEBRASKA", "KANSAS", "MISSOURI",
                           "IOWA", "MINNESOTA", "WISCONSIN", "ILLINOIS", "INDIANA", "OHIO", "MICHIGAN")) %>% 
  # select relevant variables
  select(c("year", "reference_period_desc", "state_alpha", "price")) %>% 
  rename("yr" = "year",
         "state" = "state_alpha",
         "mo" = "reference_period_desc") %>% 
  # Change from MONTH YEAR to actual date format (e.g. NOV 2016 --> 01/01/2016)
  mutate(country = "US",
         year_month_abv = paste(mo, yr),
         date = dmy(paste(1, year_month_abv)))


# filter to just date, price, country for plotting 
price_USMW <- price_USMW_states %>% 
  group_by(date, country) %>% 
  summarise(price = round(mean(price), digits = 2))

### 3.1.2: US (National) Price -------

raw_price_US <- getQuickstat(
  key = usda_key,
  program = "SURVEY",
  data_item = "SOYBEANS - PRICE RECEIVED, MEASURED IN $ / BU",
  commodity = "SOYBEANS",
  geographic_level = "NATIONAL",
  # state = c("NORTH DAKOTA", "SOUTH DAKOTA", "NEBRASKA", "KANSAS", "MISSOURI",
  #           "IOWA", "MINNESOTA", "WISCONSIN", "ILLINOIS", "INDIANA", "OHIO", "MICHIGAN"),
  year = paste(year_range),
  geometry = F) %>%
  dplyr::rename(price = Value)

# clean
price_US <- raw_price_US %>% 
  
  # clean to remove Prospects (Sept/Oct/Nov Crop Outlooks)
  filter(freq_desc == "MONTHLY") %>% 
  
  # select relevant variables
  select(c("year", "reference_period_desc", "state_alpha", "price")) %>% 
  rename("yr" = "year",
         "state" = "state_alpha",
         "mo" = "reference_period_desc") %>% 
  
  # Change from MONTH YEAR to actual date format (e.g. NOV 2016 --> 01/01/2016)
  mutate(country = "US",
         year_month_abv = paste(mo, yr),
         date = dmy(paste(1, year_month_abv))) %>% 
  select(date, country, price)

## 3.2 BR Price (USD --> USD/bu) -------

# SOURCE: https://www.cepea.esalq.usp.br/br/indicador/soja.aspx

# NOTE (from CEPEA):
## The price considered in the daily sampling corresponds to the amount converted into US dollars
## and into the present value paid in soybean deals or sales or purchase offers reported by Cepea 
## collaborating agents, per 60 kg bag, for soybeans under the conditions DAP in the yard or FAS in 
## the warehouse/port silo and deposited in a unit that loads ships via the export corridor in the 
## Port of Paranaguá, state of Paraná, free of any charges, tax or non-tax. Trades are reported in 
## Reais per 60 kg bag and converted at the 4:30 pm Commercial Selling Dollar 
## Rate in R$/US$. Forward prices are converted to spot value considering the period in calendar 
## days between the negotiation and the effective payment by the buyer,


# bring in soybean price data from CEPEA
# data are downloaded at daily temporal resolution
# data come in 60 kg / USD; take value*(100/6)*0.0272155 to get to USD/bu; *(100/6) gets to USD/1000kg

getwd()
raw_daily_price_BR <- read.csv("../Data_Source/CEPEA_Parana_BR_60kg_1997_2021_daily_raw.csv",
                               skip = 3)

# Clean daily price data and summarize to monthly to match US stats
price_BR_daily <- raw_daily_price_BR %>% 
  select(c("Date", "Price_US.")) %>% 
  rename("date" = "Date",
         'price' = 'Price_US.') %>% 
  mutate(date = as.Date(date, format = "%m/%d/%Y"),
         yr = as.numeric(year(date)),
         mo = floor_date(date, "month"),
         price = price*(100/6)*0.0272155) %>%  # convert to USD/bu 
  filter(yr >= 2000 & yr <= 2020)

# aggregate to monthly
price_BR_monthly <- aggregate(price ~ mo, price_BR_daily, mean)
price_BR_monthly <- price_BR_monthly %>% 
  mutate(country = "Brazil",
         price = round(price, digits = 2)) %>% 
  rename(date = mo)

## 3.3: Get Price (US&BR) -----
df_price_USMW_BRCerr <- rbind(price_BR_monthly, price_USMW)
df_price_USMW_BRCerr <- df_price_USMW_BRCerr %>% 
  filter(year(date) >= 2007 & year(date) <= 2017)

# 4: Yield: -------

## 4.1 US Yield --------
raw_yield_US <- getQuickstat(
  key = usda_key,
  program = "SURVEY",
  data_item = "SOYBEANS - YIELD, MEASURED IN BU / ACRE",
  commodity = "SOYBEANS",
  geographic_level = "NATIONAL",
  year = paste(year_range),
  geometry = F)  

# clean
yield_US <- raw_yield_US %>% 
  
  # clean to remove Prospects (Sept/Oct/Nov Crop Outlooks)
  filter(reference_period_desc == "YEAR") %>% 
  
  # select certain columns and rename for clarity
  select(c("year", "state_alpha", "Value")) %>% 
  rename("yr" = "year",
         "country" = "state_alpha",
         "yield" = "Value") %>%   
  # convert bu/acre to kg/ha
  # source: US Grain Council
  # 1 bu = .0272155 metric ton
  # 1 mt = 1000 kg
  # 1 acre = 0.40468564 ha
  # bu/acre * mt/bu * kg/mt * acre/ha = kg/ha
  mutate(yield = yield * ((0.0272155 * 1000) / 0.40468564) )

## 4.2: BR Yield ------
raw_yield_BR <- get_sidra(x = 1612, 
                          variable = 112, #109 is area planted # production and yield # or for first six (excluding value of production) c(109, 1000109, 216, 1000216,214, 112) 
                          period = as.character(year_range), #2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021
                          geo = "Brazil", # Brazil, State, or Município
                          geo.filter = NULL,
                          classific = "c81",
                          category = list(2713), # Soja (em grão)
                          header = T,
                          format = 3)

yield_BR <- raw_yield_BR %>% 
  select("Nível Territorial", "Ano", "Valor") %>% 
  rename(
    "country" = "Nível Territorial",
    "yr" = "Ano",    
    "yield" = "Valor") %>% 
  mutate(yr = as.double(yr))


## 4.3 Get National Yield (US & BR) -----
df_yield_USBR <- rbind(yield_BR, yield_US)
df_yield_USBR <- df_yield_USBR %>% 
  filter(yr >= 2007 & yr <= 2017) %>%  
  mutate(country = str_replace_all(country, "Brasil", "Brazil"))

## 4.4: US Yield (US-MW States) -----

## DATA SOURCE: USDA QuickStats, accessed through tidyUSDA R Package

### NOTE: yield comes in bushels but we convert to metric tons

# get raw yielduction data
raw_yield_USMW <- getQuickstat(
  key = usda_key,
  program = "SURVEY",
  data_item = "SOYBEANS - YIELD, MEASURED IN BU / ACRE",
  commodity = "SOYBEANS",
  geographic_level = "STATE",
  state = c("NORTH DAKOTA", "SOUTH DAKOTA", "NEBRASKA", "KANSAS", "MISSOURI",
            "IOWA", "MINNESOTA", "WISCONSIN", "ILLINOIS", "INDIANA", "OHIO", "MICHIGAN"),
  year = paste(year_range),
  geometry = F)  

# Clean Data
yield_USMW_states <- raw_yield_USMW %>% 
  
  # clean to remove Prospects (Sept/Oct/Nov Crop Outlooks)
  filter(reference_period_desc == "YEAR") %>% 
  
  # select certain columns and rename for clarity
  select(c("year", "state_alpha", "Value")) %>% 
  rename("yr" = "year",
         "state" = "state_alpha",
         "yield" = "Value") %>% 
  
  # convert from bu/acre to kg/ha 
  mutate(yield = yield * ((0.0272155 * 1000) / 0.40468564) ,
         country = "US") 

# summarize to regional level
yield_USMW <- yield_USMW_states %>% 
  group_by(yr, country) %>%
  dplyr::summarize(yield = round(mean(yield), digits = 2)) %>% 
  mutate(description = "US-MW States")

## 4.5: BR Yield (Cerrado MicroRegions) -----------
# get state-level data so we can merge to only those states within the extent of the Cerrado
raw_yield_BR_micro <- get_sidra(x = 1612, 
                                 variable =  c(112), # yielduction and yield # or for first six (excluding value of yielduction) c(109, 1000109, 216, 1000216,214, 112) 
                                 period = as.character(year_range), #2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021
                                 geo = "MicroRegion", # Brazil, State, or Município
                                 geo.filter = NULL,
                                 classific = "c81",
                                 category = list(2713), # Soja (em grão)
                                 header = T,
                                 format = 3)


# filter and translate
yield_BRCerr_micro <- raw_yield_BR_micro %>% 
  select("Microrregião Geográfica (Código)", "Microrregião Geográfica", "Ano", "Variável", "Valor") %>% 
  rename(
    "code_micro" = "Microrregião Geográfica (Código)",
    "name_micro_long" = "Microrregião Geográfica",
    "yr" = "Ano",
    "variable" = "Variável",
    "value" = "Valor") %>%  
  mutate(
    #variable = str_replace(variable, "Quantidade produzida", "prod"),
    variable = str_replace(variable, "Rendimento médio da produção", "yield"),
    #variable = str_replace(variable, "Área plantada", "ha_planted"),
    yr = as.double(yr)) %>% 
  select(.,c("yr", "variable", "value", "name_micro_long", "code_micro"))

# Add Cerr MicroRegions to df for aggregating later
yield_BRCerr_micro$code_micro <- as.double(yield_BRCerr_micro$code_micro)

yield_BRCerr_micro <- left_join(yield_BRCerr_micro, micro_codes_names_cerr)

yield_BRCerr_micro <- filter(yield_BRCerr_micro, code_micro %in% micro_codes_cerr)

# make data wide to match US data and make it easier to merge
yield_BRCerr_micro <- pivot_wider(yield_BRCerr_micro, names_from = "variable")


# add country and filter to the same variables as US
yield_BRCerr_micro <- yield_BRCerr_micro %>% 
  mutate(country = "Brazil") %>% 
  select(yr, name_micro, yield, country)

# summarize to regional level
yield_BRCerr_micro <- yield_BRCerr_micro %>% 
  na.omit() %>% 
  group_by(yr) %>%
  dplyr::summarize(yield = round(mean(yield), digits = 2)) %>% 
  mutate(description = "MicroRegions with Cerrrado Mean",
         country = "Brazil")

## 4.6: Get Yield (USMW & BRCerr) ----
df_yield_USMW_BRCerr <- rbind(yield_BRCerr_micro, yield_USMW)
df_yield_USMW_BRCerr <- df_yield_USMW_BRCerr %>% 
  filter(yr >= 2007 & yr <= 2017)


# 5: Area: ----------

## 5.1: US-MW Area Planted ----------
# get raw data from QuickStats
area_p_USMWst <- getQuickstat(
  key = usda_key,
  program = "SURVEY",
  data_item = "SOYBEANS - ACRES PLANTED",
  commodity = "SOYBEANS",
  geographic_level = "STATE",
  # state = c("NORTH DAKOTA", "SOUTH DAKOTA", "NEBRASKA", "KANSAS", "MISSOURI",
  #           "IOWA", "MINNESOTA", "WISCONSIN", "ILLINOIS", "INDIANA", "OHIO", "MICHIGAN"),
  year = paste(year_range),
  geometry = F) %>%
  # keep only some of the variables
  dplyr::select(
    year, county_name, county_code, state_name, state_alpha,
    state_fips_code, short_desc, freq_desc, reference_period_desc, Value) %>%
  # rename the Value column
  dplyr::rename(area_planted = Value)

# clean 
area_p_USMWst <- area_p_USMWst %>% 
  filter(reference_period_desc == "YEAR") %>% 
  filter(state_name %in% c("NORTH DAKOTA", "SOUTH DAKOTA", "NEBRASKA", "KANSAS", "MISSOURI",
                           "IOWA", "MINNESOTA", "WISCONSIN", "ILLINOIS", "INDIANA", "OHIO", "MICHIGAN")) %>% 
  # select relevant variables
  select(c("year", "reference_period_desc", "state_alpha", "area_planted")) %>% 
  rename("yr" = "year",
         "state" = "state_alpha") %>% 
  # Change from MONTH YEAR to actual date format (e.g. NOV 2016 --> 01/01/2016)
  mutate(country = "US",
         #year_month_abv = paste(mo, yr),
         #date = lubridate::ymd(yr, truncated = 2L)
         )


# filter to just date, price, country for plotting 
area_p_USMW <- area_p_USMWst %>% 
  group_by(yr, country) %>% 
  #summarise(area_planted = sum(area_planted))
  dplyr::summarize(area_planted = round(sum(area_planted), digits = 2)) %>% 
  mutate(description = "US-MW States",
         country = "US",
         # convert to from acres to ha
         # 1 Acre = 0.40468564 Hectare
         area_planted = area_planted*0.40468564
  )



## 5.2: Cerrado Area Planted & Harvested (States) --------

# 1   Área plantada (Hectares) [ a ]
# 2   Área plantada - percentual do total geral (%) [ a ]
# 3   Área colhida (Hectares)
# 4   Área colhida - percentual do total geral (%)

## 5.2: Cerrado Area Planted  & Harvested (MicroRegions) --------
raw_sidra_area_micro <- get_sidra(x = 1612, 
                            variable =  c(216, 109), # production and yield # or for first six (excluding value of production) c(109, 1000109, 216, 1000216,214, 112) 
                            period = as.character(year_range), #2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021
                            geo = "MicroRegion", # Brazil, State, or Município
                            geo.filter = NULL,
                            classific = "c81",
                            category = list(2713), # Soja (em grão)
                            header = T,
                            format = 3)

# clean and translate columns
area_BRCerr_micro <- raw_sidra_area_micro %>% 
  select("Microrregião Geográfica (Código)", "Microrregião Geográfica", "Ano", "Variável", "Valor") %>% 
  rename(
    "code_micro" = "Microrregião Geográfica (Código)",
    "name_micro_long" = "Microrregião Geográfica",
    "yr" = "Ano",
    "variable" = "Variável",
    "value" = "Valor") %>%   
  mutate(variable = str_replace(variable, "Área colhida", "area_harvested"),
         #variable = str_replace(variable, "Rendimento médio da produção", "yield"
         variable = str_replace(variable, "Área plantada", "area_planted"),
         yr = as.double(yr)) %>% 
  select(.,c("yr", "variable", "value", "name_micro_long", "code_micro"))

# Add MicroRegions for filtering and later aggregating
area_BRCerr_micro$code_micro <- as.double(area_BRCerr_micro$code_micro)

area_BRCerr_micro <- left_join(area_BRCerr_micro, micro_codes_names_cerr)

area_BRCerr_micro <- filter(area_BRCerr_micro, code_micro %in% micro_codes_cerr)

# make data wide to match US data and make it easier to merge
area_BRCerr_micro <- pivot_wider(area_BRCerr_micro, names_from = "variable")


# add country and filter to the same variables as US
area_BRCerr_micro <- area_BRCerr_micro %>% 
  mutate(country = "Brazil") %>% 
  select(yr, name_micro, area_planted, area_harvested, country)

# summarize to regional level
area_p_BRCerr_micro <- area_BRCerr_micro %>% 
  na.omit() %>% 
  group_by(yr) %>%
  dplyr::summarize(area_planted = round(sum(area_planted), digits = 2)) %>% 
  mutate(description = "MicroRegions with Cerrrado Mean",
         country = "Brazil")

# get area harvested 
area_h_BRCerr_micro <- area_BRCerr_micro %>% 
  na.omit() %>% 
  group_by(yr) %>%
  dplyr::summarize(area_harvested = round(sum(area_harvested), digits = 2)) %>% 
  mutate(description = "MicroRegions with Cerrrado Mean",
         country = "Brazil")

## 5.3: US-MW Area Harvested ----------
# get raw data from QuickStats
area_h_USMWst <- getQuickstat(
  key = usda_key,
  program = "SURVEY",
  data_item = "SOYBEANS - ACRES HARVESTED",
  commodity = "SOYBEANS",
  geographic_level = "STATE",
  # state = c("NORTH DAKOTA", "SOUTH DAKOTA", "NEBRASKA", "KANSAS", "MISSOURI",
  #           "IOWA", "MINNESOTA", "WISCONSIN", "ILLINOIS", "INDIANA", "OHIO", "MICHIGAN"),
  year = paste(year_range),
  geometry = F) %>%
  # keep only some of the variables
  dplyr::select(
    year, county_name, county_code, state_name, state_alpha,
    state_fips_code, short_desc, freq_desc, reference_period_desc, Value) %>%
  # rename the Value column
  dplyr::rename(area_harvested = Value)

# clean 
area_h_USMWst <- area_h_USMWst %>% 
  filter(reference_period_desc == "YEAR") %>% 
  filter(state_name %in% c("NORTH DAKOTA", "SOUTH DAKOTA", "NEBRASKA", "KANSAS", "MISSOURI",
                           "IOWA", "MINNESOTA", "WISCONSIN", "ILLINOIS", "INDIANA", "OHIO", "MICHIGAN")) %>% 
  # select relevant variables
  select(c("year", "reference_period_desc", "state_alpha", "area_harvested")) %>% 
  rename("yr" = "year",
         "state" = "state_alpha") %>% 
  # Change from MONTH YEAR to actual date format (e.g. NOV 2016 --> 01/01/2016)
  mutate(country = "US",
         #year_month_abv = paste(mo, yr),
         #date = lubridate::ymd(yr, truncated = 2L)
  )


# filter to just date, price, country for plotting 
area_h_USMW <- area_h_USMWst %>% 
  group_by(yr, country) %>% 
  #summarise(area_planted = sum(area_planted))
  dplyr::summarize(area_harvested = round(sum(area_harvested), digits = 2)) %>% 
  mutate(description = "US-MW States",
         country = "US",
         # convert to from acres to ha
         # 1 Acre = 0.40468564 Hectare
         area_harvested = area_harvested*0.40468564)

## 5.4: Join Regional Areas (P & H)---------
df_area_h_USMW_BRCerr <- rbind(area_h_BRCerr_micro, area_h_USMW)
df_area_h_USMW_BRCerr <- df_area_h_USMW_BRCerr  %>% 
  filter(yr >= 2007 & yr <= 2017)

df_area_p_USMW_BRCerr <- rbind(area_p_BRCerr_micro, area_p_USMW)
df_area_p_USMW_BRCerr <- df_area_p_USMW_BRCerr  %>% 
  filter(yr >= 2007 & yr <= 2017)


# 6: Land Transition to Soybean ---------

##### Potentially Delete here until **########
## 6.1: Load & Clean Land Transition Data from MapBiomas Collection 6 --------

### NOTE: Collection 7 is out, but Collection 6 is included in R package datazoom.amazonia

# source_mapb_trans_municip <- load_mapbiomas( # takes a long time
#   dataset = "mapbiomas_transition",
#   raw_data = F,
#   geo_level = "municipality",
#   #time_period = "all",
#   language = "eng",
#   #time_id = "year",
#   cover_level = 4
# )

#save(source_mapb_trans_municip, file = "../Data_Source/source_mapb_trans_municip.Rdata")
#load(file = "../Data_Source/source_mapb_trans_municip.Rdata")
##### **########


## 6.1) Import MapB Col. 8 & Clean -----------

# To get to MapBiomas Col. 8 Trans Only...
## 1) Download 'COBERTURA E TRANSIÇÕES MUNICÍPIOS & BIOMAS (COLEÇÃO 8)' from https://brasil.mapbiomas.org/estatisticas/ . Note that we saved this as 'SOURCE_trans_col8_mapbiomas_municip.xlsx' and is downloaded as 'tabela_geral_mapbiomas_col8_biomas_municipios.xlsx'. We downloaded this on Dec, 6, 2023
## 2) Save 'TRANSICOES_COL8.0' tab as it's own sheet because the file is so large. Saved this as 'SOURCE_transonly_col8_mapbiomas_municip.csv' 

# Load MapBiomas Source Data
csv_br_trans_m <- read.csv(paste0(folder_source, "SOURCE_transonly_col8_mapbiomas_municip.csv"), encoding = "UTF-8")
df <- csv_br_trans_m


# get rid of all accents
df$state <- stri_trans_general(str = df$state,  id = "Latin-ASCII")
df$biome <- stri_trans_general(str = df$biome,  id = "Latin-ASCII")
names(df)

# select levels and years to reduce df size 
df <- dplyr::select(df, c("state","municipality", "geocode", "biome",
                          # levels - we keep level 3 and 4 
                          "from_level_3", "to_level_3",
                          "from_level_4", "to_level_4",
                          
                          # years - we keep 2000 - 2021
                          #"X1985.1986", "X1986.1987", "X1987.1988", "X1988.1989", "X1989.1990", 
                          #"X1990.1991", "X1991.1992", "X1992.1993", "X1993.1994", "X1994.1995", "X1995.1996", "X1996.1997", "X1997.1998", "X1998.1999",    
                          "X1999.2000", "X2000.2001", "X2001.2002", "X2002.2003", "X2003.2004",    "X2004.2005",    "X2005.2006",   
                          "X2006.2007",    "X2007.2008",    "X2008.2009",    "X2009.2010",   "X2010.2011", "X2011.2012",    "X2012.2013",   
                          "X2013.2014",    "X2014.2015",    "X2015.2016",  "X2016.2017",    "X2017.2018",   
                          "X2018.2019",    "X2019.2020",    "X2020.2021"))

# remove all but the last four digits of all the columns 
# NOTE: this means that the year data is the transitions FROM the previous year TO the listed year
names(df) <- str_sub(names(df), - 4, - 1)
names(df)

# rename columns - BEWARE HERE, this is manual for now, if you change the 'select' above then you need to change this as well 
colnames(df)[colnames(df) %in% c("tate", "lity", "code", "iome", "el_3", "el_3",  "el_4", "el_4")] <- c("state", "municipality", "geocode", "biome", 
                                                                                                        "from_level_3", "to_level_3",                                                                                                     "from_level_4", "to_level_4")
names(df)

# Make 'long'
# gather to make into a long dataset; change the number if you changed 'select' above
ncol(df)
df <- gather(df,"year","ha",9:ncol(df))     
#df2 <- df2 %>% filter(municipality == "Alta Floresta D'Oeste") # test one municipality

# Save 
save(df, file = paste0(folder_derived, "mapb_col8_clean_long.Rdata"))

## Load Clean & Long Data - can skip to here once you've run the above code ------
load(file = paste0(folder_derived, "mapb_col8_clean_long.Rdata"))

# load municipality codes for Cerrado from script 0
load(file = paste0("../Data_Derived/muni_codes_cerr.Rdata"))



## 6.2) Filter & Calculate Stats ---------

### 6.2.1) Filter ------

df_g <- df

# filter to Level 4 data in Cerrado
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

# Save - GOAL IS TO LOAD IN 1_data; may need xx to move this all^ to a new script 0
save(df_g, file = paste0(folder_derived, "mapb_col8_clean_long_cerr_nosamefromto.Rdata"))
load(file = paste0(folder_derived, "mapb_col8_clean_long_cerr_nosamefromto.Rdata"))


# filter to just transitioning to soybeans 
trans_tosoy_BR <- df_g %>%
  #filter(to_level_4 == "Soy Beans" | to_level_4 == "Pasture")
  filter(to_level_4 == "Soybeans" | to_level_4 == "Pasture")


# break up intervals into start and end year (going from 1-year-prior to the year of transition - matches with above step in 6.1)
trans_tosoy_BR$start_year <- as.numeric(str_sub(trans_tosoy_BR$year, 1, 4))-1 
trans_tosoy_BR$end_year <- as.numeric(str_sub(trans_tosoy_BR$year, -4, -1))

str(trans_tosoy_BR)

# keep only consecutive start/end years
# this means that 2013 captures the 2012-13 harvest year in BR 
trans_tosoy_BR <- trans_tosoy_BR %>% 
  #filter(end_year == start_year+1) %>% 
  filter(to_level_4 != from_level_4) %>% 
  select("year", "end_year","start_year","state","municipality",
         #"municipality_code",
         "geocode",
         #"from_level_0", "from_level_1", "from_level_2", "from_level_3", "from_level_4",     
         #"to_level_0", "to_level_1", "to_level_2", "to_level_3", 
         "to_level_4",      
         #"value"
         "ha")

## 6.2: Aggregate to_level_4 Values -------

# aggregate to yearly transition values by combining all FROM classes per municip per year
trans_tosoy_BRmunicip_agg <- aggregate(ha ~ geocode + municipality + state + end_year + to_level_4, trans_tosoy_BR, sum)

trans_tosoy_BRmunicip_agg <- trans_tosoy_BRmunicip_agg %>% 
  filter(end_year >= 2000 & end_year <= 2020) %>% 
  rename(
    "yr" = "end_year",
    "trans" = "ha") %>%
  select(., c("yr","state", "municipality", "geocode", "trans", "to_level_4")) %>% 
  mutate(country = "Brazil")

# keep municip data
trans_tosoy <- trans_tosoy_BRmunicip_agg

## 6.3: Filter to Just the Territories (municipalities) Within the Cerrado -------

# filter
trans_tosoy_cerrmuni <- trans_tosoy %>% 
  filter(geocode %in% muni_codes_cerr) # needs shapefile information from code 0, loaded above at the end of step 6.1

## 6.4: Get Land Transition to Soy  -----

# Aggregate to one value per year
# agg to one value per entire region per year
df_trans_to_soy_BRCerr_muni <- trans_tosoy_cerrmuni %>% 
  aggregate(trans ~ yr + to_level_4, ., sum) %>%
  mutate(country = "Brazil")

df_trans_to_soy_BRCerr_muni <- df_trans_to_soy_BRCerr_muni %>% filter(yr >= 2007 & yr <= 2017)

# Maybe remove? # 7: Land Transition with Certain Classes of Interest ------
# 
# ## 7.0: Set Constants -------
# # Set the transition variables to keep 
# list_lvl4_classes <- c("Soy Beans", "Pasture",
#                        "Other Temporary Crops", "Mosaic of Agriculture and Pasture",
#                        "Sugar Cane", "Other Non Vegetated Area", "Coffe",
#                        "Other Non Forest Natural Formation", "Citrus", "Rice")
# 
# ## 7.1: Filter & Clean --------
# # set to other variable name & get others -- not just soybeans 
# trans_toclasses_BR <- 
#   #source_mapb_trans_municip %>%
#   df_g %>% 
#   filter(to_level_4 %in% list_lvl4_classes)
# 
# # break up intervals into start and end year (going from )
# trans_toclasses_BR$start_year <- as.numeric(str_sub(trans_toclasses_BR$year, 1, 4)) 
# trans_toclasses_BR$end_year <- as.numeric(str_sub(trans_toclasses_BR$year, -4, -1))
# 
# str(trans_toclasses_BR)
# 
# # keep only consecutive start/end years
# # this means that 2013 captures the 2012-13 harvest year in BR 
# trans_toclasses_BR <- trans_toclasses_BR %>% 
#   filter(end_year == start_year+1) %>% 
#   select("year", "end_year","start_year","state","municipality",
#          "municipality_code",
#          "from_level_0", "from_level_1", "from_level_2", "from_level_3", "from_level_4",     
#          "to_level_0", "to_level_1", "to_level_2", "to_level_3", "to_level_4",      
#          "value"
#          )
# 
# ## 7.2: Aggregate to_level_4 Values ----
# 
# # aggregate to yearly transition values by combining all FROM classes per municip per year
# trans_toclasses_BRmunicip_agg <- aggregate(value ~ municipality_code + municipality + state + end_year + to_level_4, trans_toclasses_BR, sum)
# 
# trans_toclasses_BRmunicip_agg <- trans_toclasses_BRmunicip_agg %>% 
#   filter(end_year >= 2000 & end_year <= 2020) %>%  
#   rename(
#     "yr" = "end_year",
#     "trans" = "value") %>%
#   select(., c("yr","state", "municipality", "municipality_code", "trans")) %>% 
#   mutate(country = "Brazil")
# 
# # rename 
# trans_toclasses <- trans_toclasses_BRmunicip_agg
# 
# ## 7.3: filter to just the territories (municipalities) within the Cerrado -----
# trans_toclasses_cerrmuni <- trans_toclasses %>% 
#   filter(municipality_code %in% muni_codes_cerr)
# 
# 
# ## 7.4: Get Land Transition to Other Classes of Interest -----
# 
# # Aggregate to one value per year
# # agg to one value per entire region per year
# df_trans_to_classes_BRCerr_muni <- trans_toclasses_cerrmuni %>% 
#   aggregate(trans ~ yr, ., sum) %>%
#   mutate(country = "Brazil")
# 
# df_trans_to_classes_BRCerr_muni <- df_trans_to_classes_BRCerr_muni %>% filter(yr >= 2007 & yr <= 2017)

# 8: Export All df's to Use in Next Script -----

# Export Production, Price, Area, and Exports
save(df_prod_USMW_BRCerr, df_prod_USBR,
     df_price_USMW_BRCerr,
     df_area_h_USMW_BRCerr, df_area_p_USMW_BRCerr,
     df_exports_USBR_china, df_exports_USBR_world, 
     df_yield_USBR, df_yield_USMW_BRCerr,
     file = "../Data_Derived/prod_price_area_yield_exports.RData")

# Export Land Change
save(df_trans_to_soy_BRCerr_muni,
     file = "../Data_Derived/land_trans_tosoy_df.RData") # used in 2.2.1

# save(df_trans_to_classes_BRCerr_muni,
#      file = "../Data_Derived/land_trans_toclasses_df.RData")
