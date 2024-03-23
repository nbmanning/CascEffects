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

# set pct
pct <- "_m"
pct_title <- " - Med"

# 1: Import clean real muni data ---------
# from 2_2_figure2_CerradoMuniDiffs.R

muni_diff <- st_read("../Data_Derived/soy_diff_yap_y12012_201320152017.shp")
muni_diff <- muni_diff %>% 
  rename(
    soy_yield = s_y,
    soy_prod = s_p,
    soy_area = s_a,
    production_diff = sp_dif,
    area_diff = sa_dif,
    yield_diff = sy_dif
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

# select one band
# NOTE: since we are doing raw change per grid cell, we want the sum per county
rast <- r_cerr$rawch_SOY

# plot grid-cell and municpalities
terra::plot(rast, main = "Grid-Cell Soy Change in Area (1000 ha) Per Grid Cell")
terra::plot(sv_muni, add = T)

# get results as raster
rast_sum_muni <- zonal(rast, sv_muni, fun = sum, as.raster = T)

# plot zonal stats with county outline  
plot(rast_sum_muni, main = "Sum of Raw Change in Area per County")
plot(sv_muni, add = T)

# use 'extract()' to match the overlapping raster and vector
# NOTE: 'ID' = T so that we have a column to match with later
rast_ext <- extract(rast_sum_muni, sv_muni, ID = T)
str(rast_ext)

# get the codes from the SpatVector of municipalities
muni_codes <- sv_muni$code_muni[rast_ext$ID]
rast_ext$code_muni <- muni_codes

# get only not missing muni codes
#test_results2 <- unique(rast_ext)
rast_ext_unique <- rast_ext %>% 
  filter(code_muni %in% muni_codes_cerr) %>% 
  unique() %>% 
  dplyr::select(-ID) %>% 
  mutate(
    rawch_SOY = rawch_SOY*1000
  )
  
rast_ext_unique <- rast_ext_unique %>% 
  left_join(shp_muni) %>% st_as_sf() 

#terra::plot(rast_ext_unique)

# Plot SIMPLE-G at County Level
ggplot(rast_ext_unique)+
  geom_sf(aes(fill = rawch_SOY))+
  theme_minimal()+
  scale_fill_gradientn(colors = brewer.pal(8, "OrRd"))+
  labs(
    title = "SIMPLE-G Raw Change Summed to County",
    fill = "Area Change (ha)"
  )+
  theme(
    plot.title = element_text(hjust = 0.5)
    )

# 4: Join and Plot Merged Data -----------

## 4.1 clean and filter test from 2_2 -------

# TO-DO: Make into function

# set yr
yr <- 2013 


# change to df (so spatial shapes come from the other df) and choose one year
muni_diff_df <- muni_diff %>% 
  filter(year == yr) %>%
  rename(code_muni = CODE) %>%
  st_drop_geometry()

# merge the two 
rast_merged <-rast_ext_unique %>% left_join(muni_diff_df, by = "code_muni")

# create a 1:1 plot with the SIMPLE-G Raw Change and Difference calcualted from script 2_2
ggplot(rast_merged, aes(x = rawch_SOY, y = area_diff)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red") +
  labs(
    x = "Raw Change in Soy", 
    y = paste(yr, "Change in Soy Area"),
    title = paste("1:1 plot of Soy Area Change from SIMPLE-G", pct_title, "and", yr, "Change")) +
  theme_minimal()+
  theme(
    plot.title = element_text(hjust = 0.5)
  )

ggsave(filename = "../Figures/")
# NOTE:
# this code finds out that muni_code 5006275 is in 1434 but not 1433
# test_results2 <- unique(rast_ext)
# tr2 <- unique(test_results2$muni_code)
# diff_codes <- setdiff(muni_codes_cerr, tr2)
# test_codes <- unique(muni_codes_cerr)

#########
# PICK UP HERE ##############
## FUTURE (3/24/24) ###################
## Get facet from 2_2 with 2022 as well - should be straightforward
## Change into function and run over 2013, 2015, 2017, 2022
## Repeat this code but with Maize this time 
## Repeat this code but with Both Combined
## See if we can get Value at the county level and area harvested as well 


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
