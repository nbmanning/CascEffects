# title: 2_2_figure2_CerradoMuniDiffs.R
# author: Nick Manning
# purpose: Get Change in Production and Value at the muncipality level 

# Notes:
## Use of USDA QuickStats requires a Key

# Created: 3/11/24
# Last Edited: March 2024

# To-Do:
## Add Production Value per county (can we even do this?)
## Create IALE plots of a 1:1 plot between SG and different years

# # # # # # # # 

# 0: Set up Env --------
rm(list = ls())

## 0.1: Load Libraries --------
library(tidyverse) 
library(sidrar) # download BR data
library(geobr) # get BR shapefiles
library(classInt) # plotting in intervals
#library(patchwork) # getting plots together in one figure
library(RColorBrewer)
library(sf)
library(scico) # used for getting the midpoint at 0
library(cowplot) # used for arranging the facet_wrap into a grid
#library(terra) # for loading SIMPLE-G results

# 0.2: Constants -------- 
getwd()

#folder <- "../Data_Source/Commodities/soymaize_2010_2022_andres/"
#folder_simpleg <- "../Data_Derived/SIMPLEG_20240112/"
folder <- "../Data_Source/soymaize_2010_2022_andres/"

###  SET YEARS ############# 
yr1 <- 2012
yr2 <- 2017
# MANUALLY REPLACE "soy" with "maize" or "sm"
crop <- "soy" # either "soy", "maize", or "sm"


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
  # remove the base column with, e.g., "2014ha", to keep "2014" and "ha" instead
  select(!base) %>% 
  # make wide again
  pivot_wider(names_from = stat, values_from = value)

# rejoin to make spatial again
# muni is FROM THE SOURCE DATA FROM EARLIER
soy_sf <- soy_df %>% left_join(muni)

## 1.3: Join maize & soydata by code -------------
# join
df <- soy_sf %>% 
  left_join(maize_yap_calc) 

# create soy+maize columns
df <- df %>% 
  mutate(sm_prod = soy_prod + maize_prod,
         sm_area = soy_area + maize_area,
         # NOTE: we have planted area so this isn't perfect
         # NOTE: Why divide by 1000? I think based on maize column...
          ## e.g. maize 2010 for code 1100015 is 19246 prod and 8019 area so with yield of 2400. If we *1000, then we also get 2400
         sm_yield = (sm_prod / sm_area)*1000,
         #sm_yield = (soy_yield + maize_yield)/2,
  ) %>%
  relocate(geometry, .after = last_col()) 

# 2: Plot Real Data --------

# get one year of real soy/maize data per municip
sf <- df #%>% # used to be 'test_df_soy'
  #filter(year == 2013)
  #filter(year == 2012 | year == 2013) # used for test_df_soy

sf <- st_as_sf(sf)

# filter to just municipalities in the Cerrado by bringing in Cerrado muni codes from SIMPLE-G script ("aggStats_MapBiomas.R")
#load(paste0(folder_simpleg, "muni_codes_cerr.Rdata"))
load(paste0(folder, "muni_codes_cerr.Rdata"))

sf <- sf %>% filter(CODE %in% muni_codes_cerr)

# get just the lowest and highest to fix map
# sf2 <- sf %>% filter(CODE == 3109600 | CODE == 5107925)
# str(sf2)


# plot soy and maize area from real data
# uncomment to run - takes a long time but it works!!! 

# sf %>%
#   #filter(year == yr2) %>%
#   filter(year >= yr1 & year <= yr2) %>% 
#   ggplot(aes(fill = sm_area)) +
#   geom_sf() +
#   facet_wrap(~year)+
#   scale_fill_distiller()+
#   #scale_fill_viridis_d(option = 'rocket', begin = 0, end = 1) +
#   labs(
#     title = 'Soy+Maize Area per Muni',
#     fill = 'Area Planted (ha)'
#   )



# 3: Calc. Differences with Real Data ------------
# LOOK TO 2_2_figure_CerradoStateDiffs.R FOR DIFF CALC
## 3.1: Test with Soybeans -----------------

# MANUALLY CHANGE (CTRL+F)
# MANUALLY REPLACE "soy" with "maize" or "sm"
crop <- crop # either "soy", "maize", or "sm"

sf_crop <-sf %>% 
  select(NM_MUN, SIGLA, CODE, year, soy_yield, soy_prod, soy_area)

# add function, comment out 
F_calc_diff <- function(data, year1, year2){
  lagtime <- year2 - year1
  newdf <- data %>%
    group_by(CODE) %>%
    mutate(
      # Difference = 2012 - 2011 per state; 
      # MANUALLY CHANGE
      production_diff = soy_prod - lag(soy_prod, n = lagtime),
      yield_diff = soy_yield - lag(soy_yield, n = lagtime),
      area_diff = soy_area - lag(soy_area, n = lagtime),

      # # Percent Change = ( (2012value - 2011value) / 2011value ) *100
      # production_diff_pct = ((maize_prod - lag(maize_prod, n = lagtime))/lag(maize_prod, n = lagtime))*100,
      # yield_diff_pct = ((maize_yield - lag(maize_yield, n = lagtime))/lag(maize_yield, n = lagtime))*100,
      # area_diff_pct = ((maize_area - lag(maize_area, n = lagtime))/lag(maize_area, n = lagtime))*100,

      # add labels to track
      # years = as.character(paste(year2, "-", year1))
      # 3/22/24: change "years" column from "2013 - 2012" to "2011/2012 - 2012/2013"
      years = as.character(paste0(
        year1-1, "/", year1, " - ", year2-1, "/", year2
      ))
    )
  
  # PICK UP HERE: TEST IF THIS CODE GETS NON-SEQUENTIAL YEARS W/O MESSING EVERYTHING ELSE UP
  # TEST:
  newdf <- newdf %>% filter(year == year2)

  }

# Works!!
sf_crop_diff <- F_calc_diff(sf_crop, 2012, 2017)

# 4: Get Shapefile & Data ----

# ### WHY??? ###
# # download state shapefile
# #shp_st <- read_state(year = 2010)
# shp_muni <- read_municipality(year=2010)
# 
# shp_muni <- shp_muni %>% 
#   select(code_muni, geom)
# 
# 
# ## 4.1 Join -----
# # drop old geometry to merge with the shp_muni geometry
# sf_crop_diff_nogeom <- sf_crop_diff %>%
#   st_drop_geometry() %>%
#   rename(code_muni = CODE) %>%
#   drop_na()
# 
# 
# # get a different soy 'df' using geobr package - not necessary
# # df_muni_geobr <- left_join(sf_soy_diff_nogeom, shp_muni, by = "code_muni")
# 
# #df_muni <- st_join(test_df_soy_diff, shp_muni, by = "code_muni") # Error in wk_handle.wk_wkb(wkb, s2_geography_writer(oriented = oriented,  : Loop 0 is not valid: Edge 253 crosses edge 255
# 
# df_muni <- sf_crop_diff %>% 
#   rename("geom" = "geometry")
# 
# # 4: Plot Differences (Real Data) -------------
# 
# ## 4.1: Plot Manually --------
# 
# # filter and set variables
# test_y_var <- as.character("yield_diff")
# test_y_var_title <- str_to_title(sub("*_diff","",test_y_var))
# 
# 
# # plot changes - with scico()
# p3 <- ggplot(df_muni)+
#   geom_sf(aes(fill = !!sym(test_y_var), geometry = geom), col = "darkgray", linewidth = 0.02)+
#   #geom_sf_text(aes(label = code_muni, geometry = geom), colour = "darkgray")+
#   theme_bw()+
#   #scale_fill_brewer(palette = "PiYG", direction = 1, drop = F, na.value = "black")+
#   scale_fill_scico(palette = "bam", direction = 1, na.value = "blue",
#                        midpoint = 0)+
#   labs(
#     title = paste("Change in",
#                   str_to_title(paste(
#                     #y_var_crop, y_var_metric,
#                     test_y_var_title,
#                     "from", yr1, "to", yr2))),
#     fill = "Change (1000)",
#     # fill = str_to_title(paste(
#     #   y_var_title,
#     #   "% Change")),
#     x = "",
#     y = ""
#   )
# 
# p3


## 4.2 With Function ----------



## 4.X: TEST running F_calc_diff then rbind then plot in a grid -----


# create three test datasets by running through soy with different years 
#crop <- "soy"

t_1213 <- F_calc_diff(sf, 2012, 2013)
t_1215 <- F_calc_diff(sf, 2012, 2015)
t_1217 <- F_calc_diff(sf, 2012, 2017)
t_1222 <- F_calc_diff(sf, 2012, 2022)


# rbind to get one long df 
t <- rbind(
  t_1213, 
  t_1215, 
  t_1217, 
  t_1222
)
names(t_1217)
names(t_1222)
names(t)

#st_write(t, "../Data_Derived/soy_diff_yap_")

F_facet <- function(data, var){
  
  # TEST: rename for joining -- don't need to do if we keep the previous geometry 
  ### NOTE: MIGHT NEED TO USE THE GEOBR 'SF' VARIABLE INSTEAD ###
  data <- data %>% rename("geom" = "geometry")

    
  # get titles
  y_var <- as.character(var)
  y_var_title <- str_to_title(sub("*_diff","", y_var))
  
  p <- ggplot(data)+
    geom_sf(aes(fill = !!sym(var), geometry = geom), col = "darkgray", linewidth = 0.02)+
    theme_bw()+
    facet_wrap(~years, nrow = 1)+
    #theme(strip.clip = "off")+
    #theme(strip.text = element_text(size = 18))+
    #scale_fill_brewer(palette = pal, direction = 1, drop = F)+
    # use the custom theme 
    scale_fill_scico(palette = "bam", direction = 1, na.value = "white",
                     midpoint = 0)+
    labs(
      # title = paste("Raw Change in",
      #               str_to_title(paste(
      #                 "production",
      #                 "from", year1, "to", year2))),
      # fill = "Raw Change in \n Production",
      fill = str_to_title(paste(y_var_title, "Change")),
      x = "",
      y = ""
    )

  # save figure
  ggsave(paste0("../Figures/CerradoMuni/",
                "gg_", crop, "_facet_", y_var,
                ".png"),
         plot = p,
         width = 12,
         height = 6)
  
  return(p)
  
}

# plot each then arrange on top of each other 
pf_prod <- F_facet(t, "production_diff")
pf_yield <- F_facet(t, "yield_diff")
pf_area <- F_facet(t, "area_diff")

# arrange
pf_yap <- plot_grid(pf_yield, pf_area, pf_prod,
                     nrow = 3,
                     #labels = c("A", "B", "C"),
                     align = "hv")

# MANUALLY CHANGE
# save
ggsave(paste0("../Figures/CerradoMuni/gg_facet_", crop, "_yap_2013_2015_2017_2022.png"),
       plot = pf_yap)

## export new sf ##
# first, change names so they aren't auto-shortened
# CHANGE between crops 
t <- t %>% 
  rename(
    s_y = soy_yield,
    s_p = soy_prod,
    s_a = soy_area,
    
    # m_y = maize_yield,
    # m_p = maize_prod,
    # m_a = maize_area,
    # 
    # sm_y = sm_yield,
    # sm_p = sm_prod,
    # sm_a = sm_area,
    
    # MANUALLY CHANGE
    sp_dif = production_diff,
    sa_dif = area_diff,
    sy_dif = yield_diff
    # mp_dif = production_diff,
    # ma_dif = area_diff,
    # my_dif = yield_diff
    # smp_dif = production_diff,
    # sma_dif = area_diff,
    # smy_dif = yield_diff
  )

names(t)

#### SAVE t ---------------
# MANUALLY CHANGE
st_write(t, paste0("../Data_Derived/", crop, "_diff_yap_", "y12012_2013201520172022.shp"),
         write_dsn_opts = list(quiet = FALSE), delete_layer = T)



# PICK UP HERE ---------
# first, go back into data function and change "years" column from "2013 - 2012" to "2011/2012 - 2012/2013"
## DONE 
# then, arrange by adding these three plots together
## DONE
# then, save and you're done for casc effects! 
## DONE 

# 
# ## 4.3: Get Plots from Fxn -------------
# 
# ## RESET YEARS ##
# yr1 <- 2012
# yr2 <- 2013
# 
# # chosen style: one of "fixed", "sd", "equal", "pretty", "quantile", "kmeans", "hclust", "bclust", "fisher", "jenks", "dpih", "headtails", "maximum", or "box"
# map_sty <- "pretty"
# 
# #style = "jenks", # don't like because we need to reduce the number of classes and it doesn't center 
# #style = "pretty", # this is fine as it center at 0 but doesn't get the range (e.g. first box is 0-50,000)
# #style = "equal" # i like equal! This doesn't look too bad
# 
# 
# ### 4.3.1: Production ----------------------------
# (p_prod_diffcut <- F_plot_gg_diffcut(test_df_soy, "production_diff", yr1, yr2))
# (p_prod_diff_cont <- F_plot_gg_diffcont(test_df_soy, "production_diff", yr1, yr2))
# 
# # "pretty" works really well with production!
# 
# 
# 
# ### 4.3.2: Area ----------------------------
# map_sty <- "jenks"
# (p_area_diffcut <- F_plot_gg_diffcut(test_df_soy, "area_diff", yr1, yr2))
# (p_area_diff_cont <- F_plot_gg_diffcont(test_df_soy, "area_diff", yr1, yr2))
# 
# # AREA IS SUCH A PROBLEM UGHHHHHHH
# 
# 
# 
# ### 4.3.3: Yield ----------------------------
# map_sty <- "jenks"
# (p_yield_diffcut <- F_plot_gg_diffcut(test_df_soy, "yield_diff", yr1, yr2))
# (p_yield_diff_cont <- F_plot_gg_diffcont(test_df_soy, "yield_diff", yr1, yr2))
# 
# # jenks works here! 
# 
# 
# 
# 






# END --------------------









# GRAVEYARD -------------

# INCORPORATED INTO FACET #
## Still useful for single plot?? ##
F_plot_gg_diffcont <- function(data, var, year1, year2){
  
  # calculate the difference between the two years 
  data <- F_calc_diff(data, year1, year2)
  
  
  # rename for joining -- don't need to do if we keep the previous geometry 
  data <- data %>% rename("geom" = "geometry")
  
  # filter and set variables
  y_var <- as.character(var)
  y_var_title <- str_to_title(sub("*_diff","",y_var))
  
  # plot changes
  p <- ggplot(data)+
    geom_sf(aes(fill = !!sym(var), geometry = geom), col = "darkgray", linewidth = 0.02)+
    theme_bw()+
    #scale_fill_brewer(palette = pal, direction = 1, drop = F)+
    # use the custom theme 
    scale_fill_scico(palette = "bam", direction = 1, na.value = "blue",
                     midpoint = 0)+
    labs(
      title = paste("Raw Change in",
                    str_to_title(paste(
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
                "gg_cont_", y_var, "_", year1, year2,
                ".png"),
         plot = p)
  
  return(p)
  
}



# this new fxn is flexible to calculating the difference between years, prod_BR_diff only used 2012 and 2013
F_plot_gg_diffcut <- function(data, var, year1, year2){
  
  # calculate the difference between the two years 
  data <- F_calc_diff(data, year1, year2)
  
  # divide by 1000 for easier legends
  #data <- data %>% mutate_at(vars(soy_yield:area_diff), list(~./1000))
  
  # rename for joining -- don't need to do if we keep the previous geometry 
  data <- data %>% rename("geom" = "geometry")
  # data <- data %>% 
  #   st_drop_geometry() %>%
  #   rename(code_muni = CODE) %>%
  #   drop_na()
  # 
  # 
  # # join to shp_muni
  # data <- left_join(data, shp_muni, by = "code_muni")
  
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
    #n = 11,
    
    style = map_sty)
  
  # set new column with the breaks for mapping
  data$DiffCut <- cut(data[[y_var]], class$brks, include.lowest = T)
  
  # get the number of factors used 
  num_cut <- as.numeric((length(levels(data$DiffCut))))
  
  # get a palette based on the number of levels in our DiffCut map
  pal <- colorRampPalette(brewer.pal(11,"PiYG"))
  
  # plot changes
  p <- ggplot(data)+
    geom_sf(aes(fill = DiffCut, geometry = geom), col = "darkgray", linewidth = 0.02)+
    theme_bw()+
    #scale_fill_brewer(palette = pal, direction = 1, drop = F)+
    # use the custom theme 
    scale_fill_manual(values = pal(num_cut), drop = F, na.value = "blue")+
    labs(
      title = paste("Raw Change in",
                    str_to_title(paste(
                      #y_var_crop, y_var_metric,
                      y_var_title,
                      "from", year1, "to", year2))),
      subtitle = paste(breaks = map_sty),
      fill = "Raw Change",
      # fill = str_to_title(paste(
      #   y_var_title,
      #   "% Change")),
      x = "",
      y = ""
    )
  
  # save figure
  ggsave(paste0("../Figures/CerradoMuni/",
                "gg_diffcut_", map_sty, "_", y_var, "_", year1, year2,
                ".png"),
         plot = p)
  
  return(p)
  
}


## Continuous ##
F_plot_gg_cont <- function(data, var, y_var_title, year1, year2){
  # plot changes
  p <- ggplot(data)+
    geom_sf(aes(fill = !!sym(var), geometry = geom), col = "darkgray", linewidth = 0.02)+
    theme_bw()+
    #scale_fill_brewer(palette = pal, direction = 1, drop = F)+
    # use the custom theme 
    scale_fill_scico(palette = "bam", direction = 1, na.value = "blue",
                     midpoint = 0)+
    labs(
      title = paste("Raw Change in",
                    str_to_title(paste(
                      y_var_title,
                      "from", year1, "to", year2))),
      fill = "Raw Change",
      # fill = str_to_title(paste(
      #   y_var_title,
      #   "% Change")),
      x = "",
      y = ""
    )
  
  return(p)
  
}

