# Section 00: Script Details ----------------

# title: 0_GetMicroMeso_in_Cerrado
# author: Nick Manning
# purpose: Intersect Cerrado shapefile and MicroRegions Shapefile to 
# end up with a list of MicroRegions in Cerrado. This will be used in 1_data_import_clean.R to 
# obtain SIDRA stats at an ideal resolution  

# Last Updated: Dec 2025

library(geobr)
library(sf)
library(dplyr)

#########################################################################

# 0: Load Cerrado Shapefiles ------- 

# Read micro-regions
micro <- read_micro_region(
  code_micro = "all",
  year = 2010,
  simplified = F,
  showProgress = TRUE
)

# Read Cerrado
shp_br_cerr <- read_biomes(
  year = 2019,
  simplified = T, # simple is okay here bc we're just intersecting regions not doing stats
  showProgress = T
) %>% dplyr::filter(name_biome == "Cerrado")

# Read all municipalities in the country at a given year
shp_muni <- read_municipality(
  code_muni="all", 
  year=2018)

# 1: Intersect Cerrado & Micro ----

## 1.1: Filter shapefiles to Micro in Cerrado -----
str(shp_br_cerr)
str(micro)

# get municipalities that are at all within the Cerrado
shp_micro_in_cerr <- st_intersection(micro, shp_br_cerr)
# terra::plot(shp_micro_in_cerr$geom)

shp_code_micro <- shp_micro_in_cerr %>% select(code_micro, geom)

## 1.2: get territory codes for municipalities in intersection -----
micro_codes_names_cerr <- shp_micro_in_cerr %>% select(code_state, abbrev_state, name_state, code_micro, name_micro) %>% st_drop_geometry()
micro_codes_cerr <- shp_micro_in_cerr$code_micro

## 1.3: Write CSV --------
write.csv(micro_codes_names_cerr, "../Data_Source/microregion_codes_names_cerrado.csv", row.names = F)
write.csv(micro_codes_cerr, "../Data_Source/microregion_codesonly_cerrado.csv", row.names = F)

# 2: Intersect Cerrado & Muni ----

## 2.1: Intersect to get Muni in Cerrado -----
# check Cerrado and Muncipality shapefiles
str(shp_br_cerr)
str(shp_muni)

# get municipalities that are at all within the Cerrado
shp_muni_in_cerr <- st_intersection(shp_muni, shp_br_cerr)

## 2.2: Get Territory Codes for Municipalities in Intersection -----
# get shp of intersecting municipalities
shp_code_muni <- shp_muni_in_cerr %>% select(code_muni, geom)

# get only codes of intersecting
muni_codes_cerr <- shp_muni_in_cerr$code_muni

## 2.3: Save Muni in Cerrado -----
# save 
save(muni_codes_cerr, file = "../Data_Derived/muni_codes_cerr.Rdata")

# 3: Save General Rdata ------
# save general shapefiles
save(shp_br_cerr, shp_muni, shp_code_muni, shp_muni_in_cerr,
     file = "../Data_Derived/land_trans_shp.RData")