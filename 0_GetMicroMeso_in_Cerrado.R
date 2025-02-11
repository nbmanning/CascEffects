# title: 0_GetMicroMeso_in_Cerrado
# author: Nick Manning
# purpose: Use a similar workflow to other script (section 6.3 in Code 1) but intersect Cerrado shapefile and MicroRegions Shapefile to 
# end up with a list of MicroRegions in Cerrado. This will be used in 1_data_import_clean.R to SIDRA stats at finest resolution  

# Last Updated: Feb 2025

library(geobr)
library(sf)
library(dplyr)

micro <- read_micro_region(
  code_micro = "all",
  year = 2010,
  simplified = F,
  showProgress = TRUE
)


#########################################################################

## 1: Load Cerrado Shapefile 

# Read all municipalities in the country at a given year
# to-do: change to shp_br_muni
# shp_muni <- read_municipality(code_muni="all", year=2018)
# plot(shp_muni)

### 6.3.2: Load Cerrado shapefile ----
shp_br_cerr <- read_biomes(
  year = 2019,
  simplified = T, # simple is okay here bc we're just intersecting regions not doing stats
  showProgress = T
) %>% dplyr::filter(name_biome == "Cerrado")

### 6.3.3: Intersect Cerrado & Muni ----
str(shp_br_cerr)
str(micro)

# get municipalities that are at all within the Cerrado
shp_micro_in_cerr <- st_intersection(micro, shp_br_cerr)
# terra::plot(shp_micro_in_cerr$geom)

shp_code_micro <- shp_micro_in_cerr %>% select(code_micro, geom)

### 6.3.4: get territory codes for municipalities in intersection -----
micro_codes_names_cerr <- shp_micro_in_cerr %>% select(code_state, abbrev_state, name_state, code_micro, name_micro) %>% st_drop_geometry()
micro_codes_cerr <- shp_micro_in_cerr$code_micro

write.csv(micro_codes_names_cerr, "../Data_Source/microregion_codes_names_cerrado.csv", row.names = F)
write.csv(micro_codes_cerr, "../Data_Source/microregion_codesonly_cerrado.csv", row.names = F)

# ### 6.3.5: filter to just the territories (municipalities) within the Cerrado 
# trans_tosoy_cerrmicro <- trans_tosoy %>% 
#   filter(municipality_code %in% muni_codes_cerr)
# 
# 
# ## 6.4: Get Land Transition to Soy  -----
# 
# # Aggregate to one value per year
# # agg to one value per entire region per year
# df_trans_to_soy_BRCerr_muni <- trans_tosoy_cerrmuni %>% 
#   aggregate(trans ~ yr, ., sum) %>%
#   mutate(country = "Brazil")
# 
# df_trans_to_soy_BRCerr_muni <- df_trans_to_soy_BRCerr_muni %>% filter(yr >= 2007 & yr <= 2017)


