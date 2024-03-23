# title: 3_stats.R
# purpose: Get the stat changes for chapter 1, cascading effects
# date created: 7/5/2023
# last updated: July 2023
# author: Nick Manning

# # # # # # # # # # # #

# 0: Load Packages -----------
library(sidrar)
library(dplyr)
library(stringr)
library(tidyr)


# 1: National Production, Area Planted, Production Value ---------

## BR ##
raw_sidra_br <- get_sidra(x = 1612, 
                          variable =  c(214, 109, 215), # production and yield # or for first six (excluding value of production) c(109, 1000109, 216, 1000216,214, 112) 
                          period = as.character(2000:2017),# list_year_range, #2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021
                          geo = "Brazil", # Brazil, State, or Município
                          geo.filter = NULL,
                          classific = "c81",
                          category = list(2713), # Soja (em grão)
                          header = T,
                          format = 3)
colnames(raw_sidra_br)

# clean and translate columns
prod_BR <- raw_sidra_br %>% 
  select("Nível Territorial", "Ano", "Variável", "Valor") %>% 
  rename(
    "geo_level" = "Nível Territorial",
    "yr" = "Ano",
    "variable" = "Variável",
    "value" = "Valor") %>%  
  mutate(variable = str_replace(variable, "Quantidade produzida", "prod"),
         #variable = str_replace(variable, "Rendimento médio da produção", "yield"
         variable = str_replace(variable, "Área plantada", "ha_planted"),
         variable = str_replace(variable, "Valor da produção", "prod_value"),
         
         yr = as.double(yr)) %>% 
  select(.,c("yr", "variable", "value", "geo_level"))

# get differences 
df <- prod_BR

df <- pivot_wider(df, names_from = "variable")

df_diff <- df %>% 
  #filter(Year >= 2000 & Year <=2017) %>% 
  #group_by(variable) %>% 
  mutate(
    # Difference = 2022 - 2021 per country
    ProdDiff = prod - lag(prod),
    # Pct Difference = ((2022-2021)/2021)*100 per country
    ProdDiffPct = ((prod - lag(prod))/lag(prod))*100,
    
    # Difference = 2022 - 2021 per country
    PlantedAreaDiff = ha_planted - lag(ha_planted),
    # Pct Difference = ((2022-2021)/2021)*100 per country
    PlantedAreaDiffPct = ((ha_planted - lag(ha_planted))/lag(ha_planted))*100,
    
    # Difference = 2022 - 2021 per country
    ProdValueDiff = prod_value - lag(prod_value),
    # Pct Difference = ((2022-2021)/2021)*100 per country
    ProdValueDiffPct = ((prod_value - lag(prod_value))/lag(prod_value))*100
  )



## BR STATES ##
raw_sidra_st <- get_sidra(x = 1612, 
                          variable =  c(214, 109), # production and yield # or for first six (excluding value of production) c(109, 1000109, 216, 1000216,214, 112) 
                          period = as.character(2000:2017),# list_year_range, #2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021
                          geo = "State", # Brazil, State, or Município
                          geo.filter = NULL,
                          classific = "c81",
                          category = list(2713), # Soja (em grão)
                          header = T,
                          format = 3)
colnames(raw_sidra_st)

# clean and translate columns
prod_BRst <- raw_sidra_st %>% 
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


# load data from other script
load("../Data_Derived/prod_price_yield_exports.RData")
