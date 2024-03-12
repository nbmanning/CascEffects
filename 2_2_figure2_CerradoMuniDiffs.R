# title: 2_2_figure2_CerradoMuniDiffs.R
# author: Nick Manning
# purpose: Get Change in Production and Value at the muncipality level 

# Notes:
## Use of USDA QuickStats requires a Key

# Created: 3/11/24
# Last Edited: March 2024

# # # # # # # # 

# libraries
library(tidyverse) 
library(sidrar) # download BR data
library(geobr) # get BR shapefiles
library(classInt) # plotting in intervals
library(patchwork) # getting plots together in one figure
library(RColorBrewer)
# 0: Load Libraries --------
rm(list = ls())
library(tidyverse)
library(sf)
#library(terra) # for loading SIMPLE-G results
getwd()

#folder <- "../Data_Source/Commodities/soymaize_2010_2022_andres/"
#folder_simpleg <- "../Data_Derived/SIMPLEG_20240112/"
folder <- "../Data_Source/soymaize_2010_2022_andres/"


# 1: Import & prep SIDRA PAM data -----------
source_soy <- read_sf(dsn = folder, layer = "soy_Brazil_2010_2022t")

source_maize_yield <- read.csv(paste0(folder, "maize_data_yield.csv"), skip = 3)
source_maize_prod <- read.csv(paste0(folder, "maize_data_prod_tons.csv"), skip = 3)
source_maize_parea <- read.csv(paste0(folder, "maize_data_plantarea_ha.csv"), skip = 3)

## 1.1: Clean maize data (tabular)---------
colnames <- c("code", "nm_mun", 
              "2010", "2010_2", "2011", "2011_2", "2012", "2012_2",
              "2013", "2013_2", "2014", "2014_2", "2015", "2015_2", 
              "2016", "2016_2", "2017", "2017_2", "2018",  "2018_2", 
              "2019", "2019_2", "2020", "2020_2", "2021", "2021_2", 
              "2022", "2022_2")


F_clean_ag <- function(df, stat){
  # get rid of first row (the one with units)
  df <- df[-1,]
  
  # manually rename columns
  names(df) <- colnames
  
  # change to long - works now!!! 
  df <- df %>% 
    pivot_longer(
      cols = !c(code, nm_mun),
      names_to = "year",
      values_to = stat
    ) %>% 
    filter(code != "Fonte: IBGE - Produção Agrícola Municipal") %>% 
    # set "-" to NA
    mutate_all(na_if, "-") %>% 
    # set the stat value to numeric - MUTATE_AT is key for providing column name as input!!!!
    mutate_at(stat, as.numeric) %>% 
    replace(is.na(.), 0)
  
  # # change to long  
  # df <- df %>% 
  #   pivot_longer(
  #     cols = !c(code, nm_mun),
  #     names_to = "year",
  #     values_to = stat
  #   ) %>% 
  #   filter(code != "Fonte: IBGE - Produção Agrícola Municipal") %>% 
  #   # if yield == "-", replace with blank (""). Else, keep yield value
  #   mutate(yield = as.numeric(gsub("-$|\\,", "", stat))) %>% 
  #   # if yield == "" (or NA), replace with 0.
  #   mutate(yield = replace_na(stat, 0))
  
  # add harvest period column then remove _2
  df <- df %>% 
    mutate(harvest = case_when(
      endsWith(year, "_2") ~ "second",
      .default = "first"
    ))
  
  # remove _2 from year
  df <- df %>% 
    mutate(year = as.numeric(str_replace(year, "_2", "")))
}

## IN PROG: ----------
# Run this function to clean. DONE
# Then, join maize. DONE
# Then, clean soy. DONE
# Then, join maize and soy. DONE 
# Then, plot. DONE


# yield
m_yield <- F_clean_ag(source_maize_yield, "yield")

# get average yield - remove rows with 0 second harvest as this would impact average yield
m_yield_calc <- m_yield %>% filter(yield != 0)
m_yield_calc <- aggregate(m_yield_calc, yield ~ code + nm_mun + year, FUN = "mean")

# prod 
m_prod <- F_clean_ag(source_maize_prod, "prod")
m_prod_calc <- aggregate(m_prod, prod ~ code + nm_mun + year, FUN = "sum")

# planted area 
m_parea <- F_clean_ag(source_maize_parea, "area")
m_parea_calc <- aggregate(m_parea, area ~ code + nm_mun + year, FUN = "sum")


# Join maize df's and make long
## NOTE: yap = yield, area, prod
maize_yap <- m_yield %>% 
  left_join(m_prod) %>% 
  left_join(m_parea) %>% 
  rename(
    "maize_yield" = "yield",
    "maize_area" = "area",
    "maize_prod" = "prod"
  )

maize_yap_calc  <- m_yield_calc %>% 
  left_join(m_prod_calc) %>% 
  left_join(m_parea_calc) %>% 
  rename(
    "maize_yield" = "yield",
    "maize_area" = "area",
    "maize_prod" = "prod"
  ) %>% 
  # drop name for joining
  select(!nm_mun) %>% 
  rename("CODE" = "code") %>% 
  mutate(CODE = as.double(CODE))


## 1.2: Clean soy data (spatial) --------
# get codes with geometry for later join
muni <- source_soy %>% select(CODE, geometry)

# make non-spatial for melting 
soy_df <-  st_drop_geometry(source_soy)
names(soy_df)

# make long
soy_df <- soy_df %>%
  # gather data to become long 
  pivot_longer(
    cols = !c("NM_MUN", "SIGLA", "AREA_KM2", "CODE"),
    names_to = "base",
    values_to = "value"
  ) %>% 
  # split year - e.g. base == 2010y becomes year == 2010 and stat == "y" 
  mutate(
    year = as.double(substr(base, start = 1, stop = 4)), # needed to make this a double to join
    stat = substr(base, start = 5, stop = 7)) %>% 
  # change text to stat it represents 
  mutate(
    stat = case_when(
      stat == "y" ~ "soy_yield",
      stat == "ha" ~ "soy_area",
      stat == "t" ~ "soy_prod",
      #.default = "other"
    ))

# make wide again to be the same format as maize 
soy_df <- soy_df %>%
  select(!base) %>% 
  pivot_wider(names_from = stat, values_from = value)

# rejoin to make spatial again
soy_sf <- soy_df %>% left_join(muni)

## 1.3: Join maize & soydata by code -------------
# join
df <- soy_sf %>% 
  left_join(maize_yap_calc) 

# create soy+maize columns
df <- df %>% 
  mutate(sm_prod = soy_prod + maize_prod,
         sm_area = soy_area + maize_area,
         # note: we have planted area so this isn't perfect
         sm_yield = (sm_prod / sm_area)*1000,
         #sm_yield = (soy_yield + maize_yield)/2,
  ) %>%
  relocate(geometry, .after = last_col()) 

# 2: Plot Real Data --------

# get one year of real soy/maize data per municip
test_df <- df %>% 
  filter(year == 2013)
  #filter(year == 2012 | year == 2013) # used for test_df_soy

test_df <- st_as_sf(test_df)

# filter to just municipalities in the Cerrado by bringing in Cerrado muni codes from SIMPLE-G script ("aggStats_MapBiomas.R")
#load(paste0(folder_simpleg, "muni_codes_cerr.Rdata"))
load(paste0(folder, "muni_codes_cerr.Rdata"))

test_df <- test_df %>% filter(CODE %in% muni_codes_cerr)

# get just the lowest and highest to fix map
test_df2 <- test_df %>% filter(CODE == 3109600 | CODE == 5107925)
str(test_df2)

# plot soy and maize area from real data
test_df %>%
  #filter(valid_start == min_date) %>%
  ggplot(aes(fill = sm_area)) +
  geom_sf() +
  scale_fill_distiller()+
  #scale_fill_viridis_d(option = 'rocket', begin = 0, end = 1) +
  labs(
    title = 'Soy+Maize Area per County',
    fill = 'Area Planted (ha)'
  )

# 3: Calc. Differences with Real Data ------------
# LOOK TO 2_2_figure_CerradoStateDiffs.R FOR DIFF CALC
## 3.1: Test with Soybeans -----------------

#PICK UP HERE: START WITH SOYBEANS; goal is to get “test_df” variable to look like prod_BR from 2_2 line 66 
test_df_soy <-test_df %>% 
  select(NM_MUN, SIGLA, CODE, year, soy_yield, soy_prod, soy_area)

# add function, comment out 
F_calc_diff <- function(data, year1, year2){
  lagtime <- year2 - year1
  newdf <- data %>%
    group_by(CODE) %>%
    mutate(
      # Difference = 2012 - 2011 per state
      production_diff = soy_prod - lag(soy_prod, n = lagtime),
      yield_diff = soy_yield - lag(soy_yield, n = lagtime),
      area_diff = soy_area - lag(soy_area, n = lagtime),

      # # Percent Change = ( (2012value - 2011value) / 2011value ) *100
      # production_diff_pct = ((soy_prod - lag(soy_prod, n = lagtime))/lag(soy_prod, n = lagtime))*100,
      # yield_diff_pct = ((soy_yield - lag(soy_yield, n = lagtime))/lag(soy_yield, n = lagtime))*100,
      # area_diff_pct = ((soy_area - lag(soy_area, n = lagtime))/lag(soy_area, n = lagtime))*100,

      # add labels to track
      years = as.character(paste(year2, "-", year1))
    )
}

# Works!!
test_df_soy_diff <- F_calc_diff(test_df_soy, 2012, 2013)

# 4: Get Shapefile & Data ----
# download state shapefile
#shp_st <- read_state(year = 2010)
shp_muni <- read_municipality(year=2010)

shp_muni <- shp_muni %>% 
  select(code_muni, geom)

## 4.1 Join -----
# drop old geometry to merge with the shp_muni geometry
test_df_soy_diff2 <- test_df_soy_diff %>%
  st_drop_geometry() %>%
  rename(code_muni = CODE) %>%
  drop_na()


# works up to here #

df_muni <- left_join(test_df_soy_diff2, shp_muni, by = "code_muni")

#df_muni <- st_join(test_df_soy_diff, shp_muni, by = "code_muni") # Error in wk_handle.wk_wkb(wkb, s2_geography_writer(oriented = oriented,  : Loop 0 is not valid: Edge 253 crosses edge 255


# 4: Plot Differences (Real Data) -------------

## 4.1: Plot Manually --------

# filter and set variables
test_y_var <- as.character("yield_diff")
test_y_var_title <- str_to_title(sub("*_diff","",test_y_var))

# set classes

# TEST 1: Try a different break style that can be automatically adjusted
# See Here: https://handsondataviz.org/design-choropleth.html
map_sty <- "pretty"
class <- classIntervals(
  round(
    df_muni2[[test_y_var]], 
    digits = 2),
  digits = 2,
  n = 11,
  
  style = map_sty
  #style = "jenks", # don't like because we need to reduce the number of classes and it doesn't center 
  #style = "pretty", # this is fine as it center at 0 but doesn't get the range (e.g. first box is 0-50,000)
  #style = "equal" # i like equal! This doesn't look too bad
)

# STOP USING MANUAL BREAKS - this increases bias in data and decreases reproducibility

# TEST 2: Manually set breaks - works but will need to set three distinct types - one for yield, one for prod, one for area
# class <- classIntervals( # doesn't work - getting NA values for class where there shouldn't be 
#   df_muni[[test_y_var]],
#   fixedBreaks =
#     #c(-100, -80, -60, -40, -20, -1,1, 20, 40, 60, 80, 100)
#     #c(-50, -40, -30, -20, -10, -0.1,0.1, 10, 20, 30, 40, 50),
#     c(-100000, -50000, -25000, -10000, -1000, -0.1, 0.1, 1000, 10000, 25000, 50000, 100000),
#   style = "fixed")

# set new column with the breaks for mapping
df_muni2$DiffCut <- cut(df_muni2[[test_y_var]], class$brks, include.lowest = T)

num_cuts <- as.numeric((length(levels(df_muni2$DiffCut))))
test_pal <- colorRampPalette(brewer.pal(11,"PiYG"))

t <- as.numeric(length(levels(df_muni2$DiffCut)))
str(t)

# plot changes - with breaks 
p <- ggplot(df_muni2)+
  geom_sf(aes(fill = DiffCut, geometry = geom), col = "darkgray", linewidth = 0.02)+
  #geom_sf_text(aes(label = code_muni, geometry = geom), colour = "darkgray")+
  theme_bw()+
  #scale_fill_brewer(palette = "PiYG", direction = 1, drop = F, na.value = "black")+
  scale_fill_manual(values = test_pal(num_cuts), drop = F, na.value = "blue")+
  labs(
    title = paste("Change in",
                  str_to_title(paste(
                    #y_var_crop, y_var_metric,
                    test_y_var_title,
                    "from", yr1, "to", yr2))),
    fill = "Change (1000)",
    # fill = str_to_title(paste(
    #   y_var_title,
    #   "% Change")),
    x = "",
    y = ""
  )

p

# plot changes - continuous
limit <- max(abs(df_muni[[test_y_var]])) * c(-1, 1)/4

p <- ggplot(df_muni)+
  geom_sf(aes(fill = !! sym(test_y_var), geometry = geom), col = "darkgray", linewidth = 0.02)+
  #geom_sf_text(aes(label = code_muni, geometry = geom), colour = "darkgray")+
  theme_bw()+
  #scale_fill_brewer(palette = "PiYG", direction = 1, drop = F, na.value = "black")+
  scale_fill_distiller(palette = "PiYG", direction = 1, na.value = "black", limit = limit)+
  labs(
    title = paste("Raw Change in",
                  str_to_title(paste(
                    #y_var_crop, y_var_metric,
                    test_y_var_title,
                    "from", yr1, "to", yr2))),
    fill = "Raw Change",
    # fill = str_to_title(paste(
    #   y_var_title,
    #   "% Change")),
    x = "",
    y = ""
  )

p

########
yr1 <- 2012
yr2 <- 2013

# this new fxn is flexible to calculating the difference between years, prod_BR_diff only used 2012 and 2013
F_plot_gg_diffpct <- function(data, var, year1, year2){
  
  # calculate the difference between the two years 
  data <- F_calc_diff(data, year1, year2)
  
  # divide by 1000 for easier legends
  #data <- data %>% mutate_at(vars(soy_yield:area_diff), list(~./1000))
  
  # rename for joining
  data <- data %>% 
    st_drop_geometry() %>%
    rename(code_muni = CODE) %>%
    drop_na()
  
  
  # join to shp_muni
  data <- left_join(data, shp_muni, by = "code_muni")
  
  # filter and set variables
  # data <- data %>% filter(yr == yr_choice)
  y_var <- as.character(var)
  y_var_title <- str_to_title(sub("*_diff","",y_var))
  
  # set classes
  class <- classIntervals(
    round(
      data[[y_var]], 
      digits = 2),
    digits = 2,
    n = 11,
    
    style = map_sty)
  # class <- classIntervals(
  #   data[[y_var]],
  #   fixedBreaks =
  #     #c(-100, -80, -60, -40, -20, -1,1, 20, 40, 60, 80, 100)
  #     #c(-50, -40, -30, -20, -10, -0.1,0.1, 10, 20, 30, 40, 50),
  #     c(-100000, -50000, -25000, -10000, -1000, -0.1, 0.1, 1000, 10000, 25000, 50000, 100000),
  #     style = "fixed")
  
  # set new column with the breaks for mapping
  # data <- data %>%
  #   mutate(DiffCut = cut(y_var, class$brks, include.lowest = T))
  data$DiffCut <- cut(data[[y_var]], class$brks, include.lowest = T)
  
  # get the number of factors used 
  num_cut <- as.numeric((length(levels(data$DiffCut))))
  
  # get a palette based on the number of levels in our DiffCut map
  pal <- colorRampPalette(brewer.pal(11,"PiYG"))
  
  # plot changes
  p <- ggplot(data)+
    geom_sf(aes(fill = DiffCut, geometry = geom), col = "darkgray", linewidth = 0.02)+
    #geom_sf_text(aes(label = code_muni, geometry = geom), colour = "darkgray")+
    theme_bw()+
    #scale_fill_brewer(palette = pal, direction = 1, drop = F)+
    scale_fill_manual(values = pal(num_cut), drop = F, na.value = "blue")
    labs(
      title = paste("Raw Change in",
                    str_to_title(paste(
                      #y_var_crop, y_var_metric,
                      y_var_title,
                      "from", year1, "to", year2))),
      fill = "Raw Change",
      # fill = str_to_title(paste(
      #   y_var_title,
      #   "% Change")),
      x = "",
      y = ""
    )
  
  # save figure
  ggsave(paste0("../Figures/CerradoMuni/",
                "gg_", y_var, "_", year1, year2,
                ".png"),
         plot = p)
  
  return(p)
  
}

# get plots 
map_sty <- "pretty"

#style = "jenks", # don't like because we need to reduce the number of classes and it doesn't center 
#style = "pretty", # this is fine as it center at 0 but doesn't get the range (e.g. first box is 0-50,000)
#style = "equal" # i like equal! This doesn't look too bad


(p_prod_diff_pct <- F_plot_gg_diffpct(test_df_soy, "production_diff", yr1, yr2))
(p_area_diff_pct <- F_plot_gg_diffpct(test_df_soy, "area_diff", yr1, yr2))
(p_yield_diff_pct <- F_plot_gg_diffpct(test_df_soy, "yield_diff", yr1, yr2))
