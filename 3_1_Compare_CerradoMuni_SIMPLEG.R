# title: 3_Compare_CerradoMuni_SIMPLEG.R
# author: Nick Manning
# purpose: Aggregate SIMPLEG results to county level and compare to real data (SIDRA) over time 

# Notes:

# Created: 3/23/24
# Last Edited: March 2024

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# 0: Set up Env --------
rm(list = ls())

## 0.1: Load Libraries --------
library(tidyverse) 
library(sidrar) # download BR data
library(geobr) # get BR shapefiles
#library(classInt) # plotting in intervals
library(patchwork) # getting plots together in one figure
library(RColorBrewer)
library(sf)
library(terra) # for loading SIMPLE-G results

## 0.2: Constants -------- 
getwd()

#folder <- "../Data_Source/Commodities/soymaize_2010_2022_andres/"
folder_simpleg <- "../Data_Derived/SIMPLEG-2024-03-03/"
folder_data_der <- "../Data_Derived/"
folder <- "../Data_Source/soymaize_2010_2022_andres/"
folder_fig_stat <- "../Figures/CerradoMuni/stats/"

# set pct
pct <- "_m"
pct_title <- " - Med"
crop <- "soy"
layer_choice <- "rawch_SOY"

# 1: Import clean real muni data ---------
# from 2_2_figure2_CerradoMuniDiffs.R

# MANUALLY CHANGE EACH TIME #
#muni_diff <- st_read("../Data_Derived/soy_diff_yap_y12012_201320152017.shp")
#muni_diff <- st_read("../Data_Derived/maize_diff_yap_y12012_2013201520172022.shp")
muni_diff <- st_read(paste0("../Data_Derived/", crop, "_diff_yap_y12012_2013201520172022.shp"))

muni_diff <- muni_diff %>% 
  rename(
    soy_yield = s_y,
    soy_prod = s_p,
    soy_area = s_a,
    # maize_yield = m_y,
    # maize_prod = m_p,
    # maize_area = m_a,
    # sm_yield = sm_y,
    # sm_prod = sm_p,
    # sm_area = sm_a,
    
    # MANUALLY CHANGE - change to elif()
    production_diff = sp_dif,
    area_diff = sa_dif,
    yield_diff = sy_dif
    
    # production_diff = mp_dif,
    # area_diff = ma_dif,
    # yield_diff = my_dif
    
    # production_diff = smp_dif,
    # area_diff = sma_dif,
    # yield_diff = smy_dif
  )


# download municipality shapefile to help with aggregating 
shp_muni <- read_municipality(year=2010)
shp_muni <- shp_muni %>% 
  dplyr::select(code_muni, geom)

# get codes of municpalities in the Cerrado
load(paste0(folder, "muni_codes_cerr.Rdata"))
shp_muni <- shp_muni %>% filter(code_muni %in% muni_codes_cerr)

# 2: Import SIMPLE-G results ---------

# load shapefiles from geobr
load(paste0(folder_data_der, "shp_usbr.RData"))

# load result rasters from SIMPLE-G Process Results Script
r_cerr <- readRDS(file = paste0(folder_simpleg, "/rds/r", pct, "_Cerrado.rds"))

# NOTE: this comes in as a PackedSpatRaster as 'terra' doesn't like saving SpatRaster
# to .Rds files. Because of this, we need to re-rasterize
# r_cerr <- rast(r_cerr)

# FUTURE To-DO: they say to use terra::unwrap() to deal with PackedSpatRaster types, so see if this is any different


## 2.1: Plot SIMPLE-G Results ---------
# plot SIMPLE-G result
terra::plot(r_cerr$new_QLAND, main = "Cerrado Post-Sim Cropland Area Per Grid Cell")
terra::plot(r_cerr$rawch_SOY, main = "Cerrado Soy Change in Area (1000 ha) Per Grid Cell")

# 3: SIMPLE-G Grid Cell Raster to County Scale ----------
library(terra)
library(sf)

## 3.1: Calc Zonal Statistics of SIMPLE-G Raster ---------

# convert shp_muni to SpatVector
sv_muni <- terra::vect(shp_muni)

## get zonal statistics for municipalities ##

# set CRS of Cerrado to the same as the Source SpatVector of municipalities 
crs(r_cerr) <- crs(sv_muni)

# set up fxn to calc zonal stats for a given layer based on sv_muni
# NOTE: since we are doing raw change per grid cell, we want the sum per county
F_zonal <- function(layer){
  layer <- zonal(layer, sv_muni, fun = sum, as.raster = T)
  layer <- layer * 1000 # get from kha to ha to compare with raw data
}

# get list of only layer names that start with rawch to aply our fxn to
r_cerr_rawch <- r_cerr %>% 
  subset(grepl( "rawch" , names(.)))

rawch_county <- sapp(r_cerr_rawch, fun = F_zonal)  

# plot some zonal stats with county outline to test  
plot(rawch_county$rawch_QLAND , main = "Sum of Raw Change in Area per County")
plot(sv_muni, add = T)


# THIS function...
## Inputs a layer from the zonal stats
## extracts the value for each county
## filters to counties within the Cerrado
## gets a clean 'df' with muncipality code and SIMPLE-G aggregated result 



F_zonal_to_OneToOne <- function(layer, yr, var_compare){
  ### PART 1: Extract Values from each municipality ### 
  
  # use 'extract()' to match the overlapping raster and vector
  # NOTE: 'ID' = T so that we have a column to match with later
  rast_ext <- extract(rawch_county[[layer]], sv_muni, ID = T)
  str(rast_ext)
  
  ### PART 2: Clean Extracted DF ### 
  
  # get the codes from the SpatVector of municipalities
  muni_codes <- sv_muni$code_muni[rast_ext$ID]
  rast_ext$code_muni <- muni_codes
  
  # get only not missing muni codes
  rast_ext_unique <- rast_ext %>% 
    filter(code_muni %in% muni_codes_cerr) %>% 
    unique() %>% 
    dplyr::select(-ID)
  
  # left_join with the county shapefile from geobr and save as spatial sf
  rast_ext_unique <- rast_ext_unique %>%
    left_join(shp_muni) %>% st_as_sf()
  
  ### PART 3: Clean and Merge Original Difference Data ###
  # get to muni_diff 'df' to one year to compare
  muni_diff_df <- muni_diff %>% 
    filter(year == yr) %>%
    rename(code_muni = CODE) %>%
    st_drop_geometry()
  
  var_name <- names(rast_ext_unique)[[1]]
  
  # merge raster with  
  rast_merged <-rast_ext_unique %>% left_join(muni_diff_df, by = "code_muni")
  
  
  
  ### PART 4: Create 1:1 Plot! ###
  
  # set the str_title to anything after the first underscore 
  str_title <- str_to_title(str_extract(layer, "(?<=_).*"))
  
  # re-assign "Maz" to Maize
  str_title <- ifelse(str_title == "Maz", "Maize", str_title)
  
  # to-do: set this to be based on "crop" and is either Maize, Soy, or Soy+Maize 
  #crop_title <- 
  
  # create a 1:1 plot with the SIMPLE-G Raw Change and Difference calculated from script 2_2
  ggplot(rast_merged, aes(x = !!sym(layer), y = !!sym(var_compare))) +
    geom_point() +
    geom_abline(intercept = 0, slope = 1, color = "red") +
    labs(
      x = paste("SIMPLE-G Change in", str_title), 
      y = paste(yr, "Change in", str_to_title(crop), "Area"), 
      title = paste("1:1 plot of", str_title, "Area Change from SIMPLE-G", pct_title, "and", yr, "Change")) +
    theme_minimal()+
    theme(
      plot.title = element_text(hjust = 0.5)
    )
  
  ggsave(filename = paste0(folder_fig_stat, "OneToOne_",crop,"_", yr, pct, ".png"),
         #file = p,
         #width = 12, height = 6
         )
}
  


# F_zonal_to_OneToOne(
#   layer = "rawch_MAZ",
#   yr = 2022,
#   var_compare = "area_diff"
# )

# run over list of years 

yr_list <- c(2013, 2015, 2017, 2022)

# MANUALLY CHANGE - for now it will always be area_diff
lapply(yr_list, function(x) F_zonal_to_OneToOne(yr = x, layer = layer_choice, var_compare = "area_diff"))


# PICK UP HERE ##############
# NEXT: Boxplot per crop with different year comparisons

# function to get ONE YEAR comparison 
# Next :) fill out this function (F_compare_SG_year) from above then run with 2013, 2015, 2017, and 2022 to:
### 1) get boxplot



### 2) calculate RMSE: https://gis.stackexchange.com/questions/265717/statistical-comparison-between-different-rasters-using-r
### 3) t-test? ANOVA? 
### check out these links:
  #### MADscatterplot function: https://cran.r-project.org/web/packages/diffeR/diffeR.pdf
  #### Bland-altman: https://cran.r-project.org/web/packages/BlandAltmanLeh/vignettes/Intro.html

#F_compare_SG_year <- function(){
### PART 1: Extract Values from each municipality ### 

# TEST WITH ONE LAYER #
# use 'extract()' to match the overlapping raster and vector
# NOTE: 'ID' = T so that we have a column to match with later

rast_ext <- extract(rawch_county[[layer_choice]], sv_muni, ID = T)
str(rast_ext)

### PART 2: Clean Extracted DF ### 

# get the codes from the SpatVector of municipalities
muni_codes <- sv_muni$code_muni[rast_ext$ID]
rast_ext$code_muni <- muni_codes

# get only not missing muni codes
rast_ext_unique <- rast_ext %>% 
  filter(code_muni %in% muni_codes_cerr) %>% 
  unique() %>% 
  dplyr::select(-ID)

# left_join with the county shapefile from geobr and save as spatial sf
rast_ext_unique <- rast_ext_unique %>%
  left_join(shp_muni) %>% st_as_sf() 

### PART 3: Clean and Merge Original Difference Data ###
# get to muni_diff 'df' to one year to compare
muni_diff_df <- muni_diff %>% 
  #filter(year == yr) %>%
  rename(code_muni = CODE) %>%
  st_drop_geometry()

var_name <- names(rast_ext_unique)[[1]]
  
# merge raster with  
rast_merged <-rast_ext_unique %>% left_join(muni_diff_df, by = "code_muni", relationship = "many-to-many")
names(rast_merged)

rast_merged2 <- rast_merged %>% 
  select(code_muni, NM_MUN, year, years, !!layer_choice, area_diff, geom)

# FORMAT FOR BOXPLOT # 
# rename our layer from SIMPLE-G to the same thing we want to compare it to
box_rast_ext_unique <- rast_ext_unique %>% 
  rename(area_diff := !!layer_choice) %>% 
  mutate(years = "SIMPLE-G", year = "SIMPLE-G") %>% 
  st_drop_geometry()

# select raw data results to compare with raw_ch
box_muni_diff_df <- muni_diff_df %>% 
  select("code_muni", "year", "years", "area_diff") %>% 
  mutate(year = as.character(year)) # so we can compare with SIMPLE-G, which doesn't have a year

#merge the extracted values with the raw data from different years (long)
box_rast_merged <- box_rast_ext_unique %>% rbind(box_muni_diff_df) 

# get rid of any municipalities that don't have a SIMPLE-G result
box_rast_merged <- box_rast_merged %>% 
  group_by(code_muni) %>%
  filter(!any(is.na(area_diff)))

# PLOT boxplot 
ggplot(box_rast_merged_box, aes(x=year, y=area_diff)) + 
  geom_boxplot()+
  labs(
    title = paste0("SIMPLE-G ", str_to_title(crop), pct_title, " Compared to Area Change"),
    subtitle = paste("n =", length(unique(box_rast_merged$code_muni)), "Municipalities"),
    x = "",
    y = ""
  )+
  theme_bw()+
  theme(
    axis.text = element_text(size = 14),
    plot.title = element_text(size = 18),
    plot.subtitle = element_text(size = 14)
  )

ggsave(filename = paste0(folder_fig_stat, "box_", crop, pct, ".png"),
       width = 20, height = 10)

# aggregate results to show change 
agg_results <- box_rast_merged %>% aggregate(area_diff ~ year + years, FUN = sum)

# PLOT barplot
ggplot(agg_results, aes(x = year, y = area_diff)) + 
  geom_bar(stat = "identity")+
  labs(
    title = paste0("SIMPLE-G ", str_to_title(crop), pct_title, " Compared to Area Change"),
    x = "",
    y = ""
  )+
  theme_bw()+
  theme(
    axis.text = element_text(size = 14),
    plot.title = element_text(size = 18),
  )

ggsave(filename = paste0(folder_fig_stat, "bar_", crop, pct, ".png"),
       width = 20, height = 10)

# CALCULATE RMSE #

# # note: all compared to 2011/2012 (year1 = 2012)
# years = as.character(paste0(
#   year1-1, "/", year1, " - ", year2-1, "/", year2))

# running into errors when pivoting, so I have to do this first
omit_muni <- box_rast_merged %>%
  dplyr::group_by(code_muni, year) %>%
  dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
  dplyr::filter(n > 1L) %>% 
  select(code_muni) %>% 
  pull() %>% 
  as.character()

# create function 
F_calc_rmse <- function(pred, actual, yr){
  rmse <- sqrt(mean((actual-pred)^2))
  df <- data.frame(
    year = as.character(yr),
    RMSE = rmse
  )
  return(df)
}

rmse_df <- box_rast_merged %>% 
  select(-years)%>% 
  # get ride of duplicate columns -- not sure why there are duplicates?? This is a problem
  filter(!code_muni %in% omit_muni) %>%  
  pivot_wider(
    names_from = year,
    names_prefix = "y",
    values_from = area_diff) %>% 
  na.omit()

# Calculate RMSE for each y column
rmse1 <- F_calc_rmse(rmse_df$`ySIMPLE-G`, rmse_df$y2013, yr = 2013)
rmse2 <- F_calc_rmse(rmse_df$`ySIMPLE-G`, rmse_df$y2015, yr = 2015)
rmse3 <- F_calc_rmse(rmse_df$`ySIMPLE-G`, rmse_df$y2017, yr = 2017)
rmse4 <- F_calc_rmse(rmse_df$`ySIMPLE-G`, rmse_df$y2022, yr = 2022)

rmse_calc <- rbind(rmse1, rmse2, rmse3, rmse4)

# PLOT lineplot to show changes in RMSE 
ggplot(rmse_calc, aes(x = year, y = RMSE, group = 1))+
  geom_line()+
  geom_point()+
  labs(
    title = paste0("RMSE of SIMPLE-G ", str_to_title(crop), pct_title, " Compared to Area Change"),
    x = ""
  )+
  theme_bw()+
  theme(
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 14),
    plot.title = element_text(size = 16)
  )

ggsave(filename = paste0(folder_fig_stat, "RMSE_", crop, pct, ".png"),
       width = 20, height = 10)



# NOTE:
# this code finds out that muni_code 5006275 is in 1434 but not 1433
# test_results2 <- unique(rast_ext)
# tr2 <- unique(test_results2$muni_code)
# diff_codes <- setdiff(muni_codes_cerr, tr2)
# test_codes <- unique(muni_codes_cerr)

#########
## FUTURE (3/24/24) ###################
## Get facet from 2_2 with 2022 as well - should be straightforward - DONE
## Change into function and run over 2013, 2015, 2017, 2022 - DONE
## Repeat this code but with Maize this time - DONE
## Repeat this code but with Both Combined - DONE 
## See if we can get Value at the county level and area harvested as well - FUTURE


#########
## FUTURE - DONE :) ##
## compare using df's -- might not work##
# try get SIMPLE-G Cerr into df 


# 3: Aggregate SIMPLE-G data to municipality level -

# Maybe try using 'st_interpolate_aw()' from 'sf' package
## link: https://r-spatial.github.io/sf/reference/interpolate_aw.html

# Other links
## help with using 'areal' package: https://cran.r-project.org/web/packages/areal/vignettes/areal-weighted-interpolation.html
## ESRI article on mapping, good example of intensive vs extensive data: https://www.esri.com/about/newsroom/arcuser/understanding-statistical-data-for-mapping-purposes/

# *best* OR we could try first bringing in Brazil County Vector, then combining it 
# with the SIMPLE-G Results using the 'zonal()' function
# link: https://bookdown.org/mcwimberly/gdswr-book/combining-vector-data-with-continuous-raster-data.html
