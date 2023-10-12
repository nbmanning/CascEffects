# (For Chapter 2 of Thesis & Casc Effects Paper)

# title: 1_data_import_clean.R
# author: Nick Manning
# purpose: Import and clean only the data that we want to make line plots with

# Notes:
## Use of USDA QuickStats requires a Key
## Use of datazoom.amazonia requires an OAuth Token to link with Google Drive

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

rm(list = ls())
getwd()

# 0: Load Libraries & Set Constants --------------

## Libraries ----
library(tidyverse)
library(tidyUSDA) # prod & price data
library(sidrar) # production (acres planted & production)
library(datazoom.amazonia) # mapbiomas data

library(geobr) # load BR shapefiles 
library(sf) # intersect

## Constants -----
year_range <- 2000:2020
BR_abbvs <- read.csv("../Data_Source/br_st_abbv.csv", header = T)
BRCerr_abbvs <- filter(BR_abbvs, biome == "Cerrado")
BRCerr_state_abbvs <- BRCerr_abbvs$state

# 1: Production (mt) ---------------

## 1.0: (Maybe replace production with yield for the future?? Maybe do corn as well?) ----

## 1.1: US Prod (US-MW States) -----

## DATA SOURCE: USDA QuickStats, accessed through tidyUSDA R Package ()

### NOTE: prod comes in bushels but we convert to metric tons

# get raw Production data
raw_prod_USMW <- getQuickstat(
  key = "34BD2DD3-9049-37A1-BC2C-D8A967E25E42",
  program = "SURVEY",
  data_item = "SOYBEANS - PRODUCTION, MEASURED IN BU",
  commodity = "SOYBEANS",
  geographic_level = "STATE",
  # state = c("NORTH DAKOTA", "SOUTH DAKOTA", "NEBRASKA", "KANSAS", "MISSOURI",
  #           "IOWA", "MINNESOTA", "WISCONSIN", "ILLINOIS", "INDIANA", "OHIO", "MICHIGAN"),
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
  dplyr::summarize(prod = round(mean(prod), digits = 2)) %>% 
  mutate(description = "US-MW States")


## 1.2 BR Prod (Cerrado States) --------

# get state-level data so we can merge to only those states within the extent of the Cerrado
raw_sidra <- get_sidra(x = 1612, 
                       variable =  c(214, 109), # production and yield # or for first six (excluding value of production) c(109, 1000109, 216, 1000216,214, 112) 
                       period = as.character(year_range), #2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021
                       geo = "State", # Brazil, State, or Município
                       geo.filter = NULL,
                       classific = "c81",
                       category = list(2713), # Soja (em grão)
                       header = T,
                       format = 3)

# colnames(raw_sidra)

# clean and translate columns
prod_BRCerr_states <- raw_sidra %>% 
  select("Unidade da Federação (Código)", "Unidade da Federação", "Ano", "Variável", "Valor") %>% 
  rename(
    "state_code" = "Unidade da Federação (Código)",
    "state_name" = "Unidade da Federação",
    "yr" = "Ano",
    "variable" = "Variável",
    "value" = "Valor") %>%  
  mutate(variable = str_replace(variable, "Quantidade produzida", "prod"),
         #variable = str_replace(variable, "Rendimento médio da produção", "yield"
         variable = str_replace(variable, "Área plantada", "ha_planted"),
         yr = as.double(yr)) %>% 
  select(.,c("yr", "variable", "value", "state_name"))

prod_BRCerr_states <- left_join(prod_BRCerr_states, BR_abbvs)

# make data wide to match US data and make it easier to merge
prod_BRCerr_states <- pivot_wider(prod_BRCerr_states, names_from = "variable")

# add country and filter to the same variables as US
prod_BRCerr_states <- prod_BRCerr_states %>% 
  mutate(country = "Brazil") %>% 
  select(yr, state, prod, country)

# summarize to regional level
prod_BRCerr <- prod_BRCerr_states %>% 
  na.omit() %>% 
  group_by(yr) %>%
  dplyr::summarize(prod = round(mean(prod), digits = 2)) %>% 
  mutate(description = "States with Cerrrado",
         country = "Brazil")

## 1.3: Get Prod (US&BR) ----
prod_USMW_BRCerr <- rbind(prod_BRCerr, prod_USMW)

# 2: Exports (Value; USD) ------------

# NOTE: Manually replaced World Exports for Brazil with the sum of all countries because it was missing for some reason from Comtrade

# Data Source: Export Data from UN Comtrade (?)
raw_exports <- read.csv("../Data_Source/UNComtrade_USBR_HS1201_20072018.csv")

## 2.1: Exports to World --------
# filter raw data  
exports_usbr <- raw_exports %>% 
  select(c("Period", "ReporterDesc", "PartnerISO", "PrimaryValue")) %>% 
  filter(ReporterDesc %in% c("USA", "Brazil")) %>%
  filter(Period >= 2007 & Period <= 2017)

# get the world quantity of exports since UN Comtrade was missing them
# nope, missing them for a reason. 2012 and 2013 BR --> China QTY is missing so can't sum
# exports_world_qty <- exports_usbr %>% 
#   group_by(Period, ReporterDesc) %>% 
#   summarise(WorldQty = sum(Qty))

# get the world Value of exports since UN Comtrade was missing Qty
exports_world <- exports_usbr %>% 
  filter(PartnerISO == "W00")

## 2.2: Export (Value) to China ----
# filter to just USA / BR exports to China
exports_usbr_china <- exports_usbr %>%
  filter(PartnerISO == "CHN")

## 3.3: Get Exports ---------
exports_USBR_world <- exports_world
exports_USBR_china <- exports_usbr_china

# 3: Price (USD / bu) ---------------------

## 3.1: US-MW Price (USD/bu) ---------

# note: daily is available from macrotrends (see thesis data), but USDA is more reliable

### Source: USDA QuickStat
### Description: Average of state-wide prices at the annual level

# get raw data from QuickStats
raw_price_USMWst <- getQuickstat(
  key = "34BD2DD3-9049-37A1-BC2C-D8A967E25E42",
  program = "SURVEY",
  data_item = "SOYBEANS - PRICE RECEIVED, MEASURED IN $ / BU",
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
  mutate(country = "US",
         year_month_abv = paste(mo, yr),
         date = dmy(paste(1, year_month_abv)))


# filter to just date, price, country for plotting 
price_USMW <- price_USMW_states %>% 
  group_by(date, country) %>% 
  summarise(price = round(mean(price), digits = 2))

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
# data is downloaded at daily... I have it as annual? 
# data comes in 60 kg / USD; take value*(100/6)*0.0272155 to get to USD/bu; *(100/6) gets to USD/1000kg

getwd()
#raw_price_BR <- read.csv("current/CEPEA_soybean_BR_1997_2021_annual_main.csv", skip = 3, dec = ",")
raw_daily_price_BR <- read.csv("../Data_Source/CEPEA_Parana_BR_60kg_1997_2021_daily_raw.csv",
                               skip = 3)
str(raw_daily_price_BR)

price_BR_daily <- raw_daily_price_BR %>% 
  select(c("Date", "Price_US.")) %>% 
  rename("date" = "Date",
         'price' = 'Price_US.') %>% 
  mutate(date = as.Date(date, format = "%m/%d/%Y"),
         yr = as.numeric(year(date)),
         mo = floor_date(date, "month"),
         price = price*(100/6)*0.0272155) %>%  # convert to USD/bu 
  filter(yr >= 2000 & yr <= 2020)

price_BR_monthly <- aggregate(price ~ mo, price_BR_daily, mean)
price_BR_monthly <- price_BR_monthly %>% 
  mutate(country = "Brazil",
         price = round(price, digits = 2)) %>% 
  rename(date = mo)

## 3.3: Get Price (US&BR) -----
price_USMW_BRCerr <- rbind(price_BR_monthly, price_USMW)


# 4: Land Transition to Soybean ---------

### STEPS to get to trans_BRmunicip_year ###

## 4.0: Set Constants ------
# Set the transition variables to keep 
list_lvl4_interest <- c("Savanna Formation", "Grassland", "Pasture", "Soy Beans", 
                        "Other Temporary Crops", "Mosaic of Agriculture and Pasture",
                        "Sugar Cane", "Other Non Vegetated Area", "Coffe",
                        "Other Non Forest Natural Formation", "Citrus", "Rice")

## 4.1: Load land transition data from MapBiomas Collection 6 

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
load(file = "../Data_Source/source_mapb_trans_municip.Rdata")

# set to other variable name & get others -- not just soybeans 
trans_BR <- source_mapb_trans_municip %>% 
  filter(to_level_4 %in% list_lvl4_interest) %>% 
  filter(state %in% BRCerr_state_abbvs)

# break up intervals into start and end year (going from )
trans_BR$start_year <- as.numeric(str_sub(trans_BR$year, 1, 4)) 
trans_BR$end_year <- as.numeric(str_sub(trans_BR$year, -4, -1))

str(trans_BR)

# get only the non-subsequent intervals
# trans_BR_intervals <- trans_BR %>%
#   filter(end_year != start_year+1)

# keep only consecutive start/end years
# this means that 2013 captures the 2012-13 harvest year in BR 
trans_BR <- trans_BR %>% 
  filter(end_year == start_year+1) %>% 
  select("year", "end_year","start_year","state","municipality","municipality_code",
         "from_level_0", "from_level_1", "from_level_2", "from_level_3", "from_level_4",     
         "to_level_0", "to_level_1", "to_level_2", "to_level_3", "to_level_4",      
         "value")

# aggregate to yearly transition values by combining all FROM classes per municip per year

#trans_BRmunicip_agg <- aggregate(value ~ territory_id + municipality + state + end_year + to_level_4, trans_BR, sum)

# note to self: missing territory_id, probably called something else. let's try municipality_code first then feature_id

trans_BRmunicip_agg <- aggregate(value ~ municipality_code + municipality + state + end_year + to_level_4, trans_BR, sum)

df_trans_BR <- trans_BRmunicip_agg %>% 
  filter(end_year >= 2000 & end_year <= 2020) %>% # UGHHHHH ONLY up to 2019!!!!!!!! 
  rename(
    "yr" = "end_year",
    "trans" = "value") %>%
  select(., c("yr","state", "municipality", "municipality_code", "trans")) %>% 
  mutate(country = "Brazil")

# note to self: this is different than the other BR Cerr data, this has more. Thinkging it's because we didn't filter yet.
R_trans_BRmunicip2 <- df_trans_BR
# note to self: 425,829 obs after filtering for Cerrado State Names

unique(R_trans_BRmunicip$municipality)
unique(R_trans_BRmunicip2$municipality)

## 4.X: set cleaned trans_tosoy data ---------
load(file = "../Data_Source/trans_BRmunicip_year.R")
trans_tosoy <- R_trans_BRmunicip


## 4.3: load municipality shapefile and Cerrado shapefile and intersect -----

### 4.3.1: Load municipality shapefile

# Read all municipalities in the country at a given year
# to-do: change to shp_br_muni
shp_muni <- read_municipality(code_muni="all", year=2018)
# plot(shp_muni)

### 4.3.2: Load Cerrado shapefile ----
shp_br_cerr <- read_biomes(
  year = 2019,
  simplified = T,
  showProgress = T
) %>% dplyr::filter(name_biome == "Cerrado")

### 4.3.3: Intersect Cerrado & Muni ----
str(shp_br_cerr)
str(shp_muni)

# get municipalities that are at all within the Cerrado
shp_muni_in_cerr <- st_intersection(shp_muni, shp_br_cerr)
plot(shp_muni_in_cerr)

shp_code_muni <- shp_muni_in_cerr %>% select(code_muni, geom)

### 4.3.4: get territory codes for municipalities in intersection -----
muni_codes_cerr <- shp_muni_in_cerr$code_muni

### 4.3.5: filter to just the territories (municipalities) within the Cerrado 
trans_tosoy_cerrmuni <- trans_tosoy %>% 
  filter(municipality_code %in% muni_codes_cerr)

## 4.4: Aggregate to one value per year  -----

# agg to one value per entire region per year
df_trans_to_soy_BRCerr_muni <- trans_tosoy_cerrmuni %>% 
  aggregate(trans ~ yr, ., sum) %>%
  mutate(country = "Brazil")
