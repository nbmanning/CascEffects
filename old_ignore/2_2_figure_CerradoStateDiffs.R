# title: 2_2_figure_CerradoStateDiffs.R
# author: Nick Manning
# purpose: Get Change in Production and Value at the state level 
# Notes:
## Use of USDA QuickStats requires a Key

# # # # # # # # 

# 0: Load Libraries & Set Constants -----

# libraries
library(tidyverse) 
library(sidrar) # download BR data
library(geobr) # get BR shapefiles
library(classInt) # plotting in intervals
library(patchwork) # getting plots together in one figure

# constants
year_range <- 2000:2020
BR_abbvs <- read.csv("../Data_Source/br_st_abbv.csv", header = T)
BRCerr_abbvs <- filter(BR_abbvs, biome == "Cerrado")
BRCerr_state_abbvs <- BRCerr_abbvs$state


# 1: Production --------
## 1.1: get state-level production data ----
# get state-level data so we can merge to only those states within the extent of the Cerrado
raw_sidra <- get_sidra(x = 1612, 
                       variable =  c(214, 112, 215), # production and yield # or for first six (excluding value of production) c(109, 1000109, 216, 1000216,214, 112) 
                       period = as.character(year_range), #2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021
                       geo = "State", # Brazil, State, or Município
                       geo.filter = NULL,
                       classific = "c81",
                       category = list(2713), # Soja (em grão)
                       header = T,
                       format = 3)

# colnames(raw_sidra)

# clean and translate columns
prod_BR <- raw_sidra %>% 
  select("Unidade da Federação (Código)", "Unidade da Federação", "Ano", "Variável", "Valor") %>% 
  rename(
    "state_code" = "Unidade da Federação (Código)",
    "state_name" = "Unidade da Federação",
    "yr" = "Ano",
    "variable" = "Variável",
    "value" = "Valor") %>%  
  mutate(variable = str_replace(variable, "Quantidade produzida", "production"),
         variable = str_replace(variable, "Rendimento médio da produção", "yield"),
         #variable = str_replace(variable, "Área plantada", "ha_planted"),
         variable = str_replace(variable, "Valor da produção", "prod_value"),
         yr = as.double(yr)) %>% 
  select(.,c("yr", "variable", "value", "state_name"))

prod_BR <- left_join(prod_BR, BR_abbvs)
prod_BR <- filter(prod_BR, state %in% BRCerr_state_abbvs)

# make data wide to match US data and make it easier to merge
prod_BR <- pivot_wider(prod_BR, names_from = "variable")

# add country and filter to the same variables as US
prod_BR <- prod_BR %>% 
  mutate(country = "Brazil")# %>% 
  #select(yr, state, prod, country)

# calculate differences
# do this the manual way to compare with function - it works! now OMIT.  
test_prod_BR_diff <- prod_BR %>% 
  group_by(state) %>% 
  mutate(
    # Difference = 2012 - 2011 per state
    production_diff = production - lag(production),
    yield_diff = yield - lag(yield),
    value_diff = prod_value - lag(prod_value),
    
    # Percent Change = ( (2012value - 2011value) / 2011value ) *100
    production_diff_pct = ((production - lag(production))/lag(production))*100,
    yield_diff_pct = ((yield - lag(yield))/lag(yield))*100,
    value_diff_pct = ((prod_value - lag(prod_value))/lag(prod_value))*100,
  )

# function for defining years and calculating differences 
# e.g. year1 = 2012, year2 = 2014, then diff = 2014value - 2012value

F_calc_diff <- function(data, year1, year2){
  lagtime <- year2 - year1
  newdf <- data %>% 
    group_by(state) %>% 
    mutate(
      # Difference = 2012 - 2011 per state
      production_diff = production - lag(production, n = lagtime),
      yield_diff = yield - lag(yield, n = lagtime),
      value_diff = prod_value - lag(prod_value, n = lagtime),
      
      # Percent Change = ( (2012value - 2011value) / 2011value ) *100
      production_diff_pct = ((production - lag(production, n = lagtime))/lag(production, n = lagtime))*100,
      yield_diff_pct = ((yield - lag(yield, n = lagtime))/lag(yield, n = lagtime))*100,
      value_diff_pct = ((prod_value - lag(prod_value, n = lagtime))/lag(prod_value, n = lagtime))*100,
      
      # add labels to track
      years = as.character(paste(year2, "-", year1))
    )
}

# test; OMIT, incorporated into function
# prod_BR_diff <- F_calc_diff(prod_BR, 2012, 2013)

# 4: Get Shapefile & Data ----
# download state shapefile
shp_muni <- read_state(year=2010)
shp_muni <- shp_muni %>% 
  select(abbrev_state, geom)

## 4.1 Join -----
# join df's to add the difference data
# OMIT: incorporated into function
# prod_BR_diff <- prod_BR_diff %>% 
#   select(yr, state_name, state, production, yield, prod_value, production_diff, yield_diff, value_diff,production_diff_pct, yield_diff_pct, value_diff_pct) %>%
#   rename(abbrev_state = state)
# 
# df_state <- left_join(prod_BR_diff, shp_muni, by = "abbrev_state")


# 5: Plot --------

yr1 <- 2012
yr2 <- 2020

# 5.1: Establish plotting the difference between two years 
# this new fxn is flexible to calculating the difference between years, prod_BR_diff only used 2012 and 2013
F_plot_gg_diffpct <- function(data, var, year1, year2){

  # calculate the difference between the two years 
  data <- F_calc_diff(data, year1, year2)
  
  # rename for joining
  data <- data %>% rename(abbrev_state = state)
  
  
  # join to shp_muni
  data <- left_join(data, shp_muni, by = "abbrev_state")

  # filter and set variables
  # data <- data %>% filter(yr == yr_choice)
  y_var <- as.character(var)
  y_var_title <- str_to_title(sub("*_diff_pct","",y_var))

  # set classes
  #class <- classIntervals(round(data[[y_var]]), n = 11, style = "fisher")

  class <- classIntervals(
    data[[y_var]],
    fixedBreaks =
      #c(-100, -80, -60, -40, -20, -1,1, 20, 40, 60, 80, 100),
      #c(-50, -40, -30, -20, -10, -0.1,0.1, 10, 20, 30, 40, 50),
      c(-200, -150, -100, -50, -25, -1,1, 25, 50, 100, 150, 200),
    
    style = "fixed")

  # set new column with the breaks for mapping
  # data <- data %>%
  #   mutate(DiffCut = cut(y_var, class$brks, include.lowest = T))
  data$DiffCut <- cut(data[[y_var]], class$brks, include.lowest = T)

  # plot changes
  p <- ggplot(data)+
    geom_sf(aes(fill = DiffCut, geometry = geom), col = "darkgray", linewidth = 0.02)+
    geom_sf_text(aes(label = abbrev_state, geometry = geom), colour = "darkgray")+
    theme_bw()+
    scale_fill_brewer(palette = "PiYG", direction = 1, drop = F)+
    labs(
      title = paste("% Change in",
                    str_to_title(paste(
                      #y_var_crop, y_var_metric,
                      y_var_title,
                      "from", year1, "to", year2))),
      fill = "% Change",
      # fill = str_to_title(paste(
      #   y_var_title,
      #   "% Change")),
      x = "",
      y = ""
    )

  # save figure
  ggsave(paste0("../Figures/CerradoStates/",
                "gg_", y_var, "_", year1, year2,
                ".png"),
         plot = p)

  return(p)

}

# get plots 
(p_value_diff_pct <- F_plot_gg_diffpct(prod_BR, "value_diff_pct", yr1, yr2))
(p_prod_diff_pct <- F_plot_gg_diffpct(prod_BR, "production_diff_pct", yr1, yr2))
(p_yield_diff_pct <- F_plot_gg_diffpct(prod_BR, "yield_diff_pct", yr1, yr2))

# plot together 
p1 <- p_value_diff_pct +  p_prod_diff_pct + p_yield_diff_pct

p2 <- p1 +
  plot_layout(nrow = 1, guides = "collect") & 
  theme(legend.position = "right") & 
  theme(legend.text = element_text(size = 15)) & 
  theme(legend.title = element_text(size = 15))

#p2

ggsave(filename = paste0("../Figures/CerradoStates/Cerrado_ValueProdYield",
                         yr1, "_", yr2, ".png"),
       p2, height = 12, width = 18, 
       dpi = 300) 





# GRAVEYARD -----------------
# # # # # # # # # # # # # # #


# F_plot_gg_diffpct <- function(data, var, yr){
#   
#   # filter and set variables
#   data <- data %>% filter(year == yr)
#   y_var <- as.character(var)
#   
#   # get "soy" from "soyDiffPctYield
#   y_var_crop <- as.character(sub("Diff.*","",y_var))
#   
#   # get "Yield" from soyDiffPctYield
#   y_var_metric <- as.character(sub(".*Pct", "", y_var))
#   
#   # set classes
#   #class <- classIntervals(round(data[[y_var]]), n = 11, style = "fisher")
#   
#   class <- classIntervals(
#     data[[y_var]], 
#     fixedBreaks = 
#       c(-100, -80, -60, -40, -20, -1,1, 20, 40, 60, 80, 100),  
#     style = "fixed")
#   
#   # set new column with the breaks for mapping
#   # data <- data %>%
#   #   mutate(DiffCut = cut(y_var, class$brks, include.lowest = T))
#   data$DiffCut <- cut(data[[y_var]], class$brks, include.lowest = T)
#   
#   # plot changes
#   p <- ggplot(data)+
#     geom_sf(aes(fill = DiffCut), col = "darkgray", linewidth = 0.02)+
#     theme_bw()+
#     scale_fill_brewer(palette = "PiYG", direction = 1, drop = F)+
#     labs(
#       title = paste("US-MW % Change in",
#                     str_to_title(paste(
#                       y_var_crop, y_var_metric,
#                       "from", yr-1, "to", yr))),
#       fill = str_to_title(paste(
#         y_var_metric,
#         "% Change"))
#     )
#   #lines(shp_br_cerr_states, lwd = 0.8, lty = 3, col = "darkgray")
#   
#   # save figure
#   ggsave(paste0("../Figures/shock_eda/",
#                 "gg_", y_var, "_", yr-1, yr,
#                 ".png"), 
#          plot = p)
#   
#   return(p)
#   
# }


# # # # # # # # # # # # # # # 

## 5.1: plot one to test fxn ------------
df_state1 <- df_state %>% 
  filter(yr == 2014)

ggplot(df_state1)+
  geom_sf(aes(fill = yield_diff_pct, geometry = geom), col = "darkgray", linewidth = 0.1)+
  theme_bw()+
  #scale_fill_brewer(palette = "PiYG", direction = 1, drop = F)
  scale_fill_distiller(palette = "PiYG", direction = 1)+
  labs(
    title = paste("Cerrado % Change in",
                  str_to_title(paste(
                    y_var, y_var_metric,
                    "from", yr-1, "to", yr))),
    fill = str_to_title(paste(
      y_var_metric,
      "% Change"))
  )

