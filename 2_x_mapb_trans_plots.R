# (For Casc Effects Paper)

# title: 2_mapb_trans_plots.R
# author: Nick Manning
# purpose: Re-use code from SIMPLE-G to plot certain land transititon categories from mapbiomas
# NOTE: Cleaning code is from "aggStats_MapBiomas.R" and filter/plot code from "lineplot_trans_MapB_Cerr_lvl4.R"

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# 0) Set Paths & Constants & Libraries -------
rm(list = ls())

## Libraries
library(tidyverse)
library(stringi) # removing accents

## Paths
folder_source <- "../Data_Source/"
folder_derived <- "../Data_Derived/"

## Constants
yr_range <- 2008:2018


# 1) Import MapB Col. 8 & Clean -----------

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


# Save 
save(df, file = paste0(folder_derived, "mapb_col8_clean_long.Rdata"))

## Load Clean & Long Data - can skip to here once you've run the above code ------
load(file = paste0(folder_derived, "mapb_col8_clean_long.Rdata"))

# load municipality codes for Cerrado 
load(file = paste0(folder_derived, "muni_codes_cerr.Rdata"))

# 2) Filter & Calculate Stats ---------

# SUMMARY: 

## 2.1) Filter ------
# filter to only "Temporary Crops" & From-To's that do not stay the same
df <- df #%>% 
 # filter(to_level_3 == "Temporary Crops") %>%
 # filter(to_level_3 != from_level_3)

# get 
df_g <- df

# maybe: plot "from" specific classes

# Broader 
# names_from <- c("Forest Formation", "Grassland", "Mosaic of Agriculture and Pasture",
#                 "Pasture", "Other Temporary Crops", "Other Perennial Crops",
#                 "Savanna Formation", "Soy Beans", "Sugar Cane", "Wetland")

# Fewer, but more specific, classes
# names_from <- c("Forest Formation", "Grassland", "Pasture", "Savanna", "Wetland")

# filter to Level 4 data in Cerrado
df_g <- df_g %>% 
  filter(geocode %in% muni_codes_cerr) %>%
  # filter(from_level_4 %in% names_from) %>%
  
  # important as this makes sure we only have things that changed 
  filter(to_level_4 != from_level_4) %>%
  
  # Create from-to column
  mutate(fromto = paste0(from_level_4, " to ", to_level_4)) #%>% 
  #filter(biome == "Cerrado")

# get csv of all the unique columns - useful for choosing filter levels
# names_df_g <- df_g %>% dplyr::select(from_level_4, to_level_4, fromto) %>% distinct()
# write.csv(names_df_g, "../Data_Source/MapBiomas/names_fromto.csv", row.names = F)


## 2.2) Calc. & Print Stats ------

### CLASSES #####
classes_lvl_4 <- c("Pasture", "Soy Beans", "Other Temporary Crops",
  "Forest Plantation", "Mosaic of Agriculture and Pasture",
  "Sugar Cane", "Rice", "Cotton","Other Perennial Crops",
  "Coffe", "Citrus")

classes_fewer <- c("Other Temporary Crops", "Mosaic of Agriculture and Pasture", "Cotton",
                   "Pasture", "Savanna Formation", "Grassland")

names_fromto <-c(
  #"Forest Formation to Mosaic of Agriculture and Pasture",
  "Forest Formation to Pasture",
  "Forest Formation to Soy Beans",
  #"Forest Formation to Other Temporary Crops",
  #"Pasture to Mosaic of Agriculture and Pasture",
  #"Pasture to Other Temporary Crops",
  "Pasture to Soy Beans",
  #"Pasture to Sugar Cane",
  #"Savanna Formation to Mosaic of Agriculture and Pasture",
  #"Savanna Formation to Other Temporary Crops",
  "Savanna Formation to Pasture",
  "Savanna Formation to Soy Beans"
  
  # "Grassland to Soy Beans", # tested grassland -  very minimal total ha
  # "Grassland to Pasture",
  # "Grassland to Other Temporary Crops"
)


### Calculate ------



# calculate each of the 11 main Lvl 4 "TO" classes per year 
# Remember that each year is the year that came before it to that year, e.g. 2011 is "from 2010, to 2011"
df_g_to_all <- df_g %>% 
  group_by(year, to_level_4) %>%
  na.omit() %>% 
  summarize(total_trans = sum(ha)) %>% 
  filter(to_level_4 %in% classes_lvl_4) %>% 
  filter(year %in% yr_range) #%>% 
  #mutate(year = as.Date(paste(year, 1, 1), '%Y %m %d'))


# get only the specifically mentioned "from-to" categories of interest
df_g_specific_fromto <- df_g %>% 
  filter(fromto %in% names_fromto) %>% 
  group_by(year, fromto) %>% 
  na.omit() %>% 
  summarise(total_trans = sum(ha)) %>% 
  filter(year %in% yr_range) #%>% 
  #mutate(year = as.Date(paste(year, 1, 1), '%Y %m %d'))

# create intervals
df_g_specific_fromto <- df_g_specific_fromto %>% 
  mutate(years = paste0(
    as.numeric(year)-1, "-", as.numeric(year)
  ))

# plot 
ggplot(df_g_specific_fromto, aes(x=years, y=total_trans/1000000, group = fromto, color = fromto)) +
  geom_line() +
  geom_point(fill = "white", size = 0.8) +
  xlab("")+
  #scale_x_date(date_labels = "%Y")+
  labs(title = paste("Cerrado Annual Land Transition"),
       #subtitle = "Data Source: MapBiomas",
       #caption = "Dotted line shows the transitions from 2012 to 2013",
       y = "Land Change from Previous Year (Mha)",
       color = "From-To Transitions")+
  geom_vline(xintercept = "2012-2013", color = "red",
             linetype="dotted", linewidth=0.5)+
  theme_bw()+
  theme(
    legend.title = element_blank(),
    legend.text = element_text(size = 12, hjust = 0.5),
    legend.position = "top",
    
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 11, angle = 20, vjust = 0.5),
    
    
    plot.title = element_text(size = 16, hjust = 0.5),
    #plot.caption = element_text(hjust=0)
    plot.subtitle = element_text(size = 14, hjust = 0.5)
  )+  
  guides(
    color = guide_legend(nrow = 2)  # Arrange legend items into 2 rows
  )

# save
ggsave(
  filename = "../Figures/mapb_LandTrans_20072018.png",
  width = 10,
  height = 4
)

### Stats ###### 
# Broader 
names_from <- c("Forest Formation", "Grassland", "Mosaic of Agriculture and Pasture",
                "Pasture", "Other Temporary Crops", "Other Perennial Crops",
                "Savanna Formation", "Soy Beans", "Sugar Cane", "Wetland")

# Fewer, but more specific, classes
names_from <- c("Forest Formation", "Grassland", "Pasture", "Savanna", "Wetland")

# all classes to soybean for Cerrado
df_g_all_to_soy <- df_g %>%
  # optional filter
  # filter(from_level_4 %in% names_from) %>% 
  
  group_by(year, to_level_4) %>%
  na.omit() %>% 
  summarize(total_trans = sum(ha)) %>% 
  filter(to_level_4 == "Soy Beans") %>% 
  filter(year %in% yr_range) %>%  
  as.data.frame() #%>% 

df_g_all_to_soy <- df_g_all_to_soy %>% 
  arrange(year) %>% 
  mutate(
    # Difference = 2013 - 2012 per category
    diff =  round(total_trans - lag(total_trans), 1),
    # Pct Difference = ((2013-2012)/2012)*100 per category
    diffpct = round(((total_trans - lag(total_trans))/lag(total_trans))*100, 1)
  )

# all classes to soybean for Brazil 
df_g_all_to_soy <- df_g %>%
  # optional filter
  # filter(from_level_4 %in% names_from) %>% 
  
  group_by(year, to_level_4) %>%
  na.omit() %>% 
  summarize(total_trans = sum(ha)) %>% 
  filter(to_level_4 == "Soy Beans") %>% 
  filter(year %in% yr_range) %>%  
  as.data.frame() #%>% 

df_g_all_to_soy <- df_g_all_to_soy %>% 
  arrange(year) %>% 
  mutate(
    # Difference = 2013 - 2012 per category
    diff =  round(total_trans - lag(total_trans), 1),
    # Pct Difference = ((2013-2012)/2012)*100 per category
    diffpct = round(((total_trans - lag(total_trans))/lag(total_trans))*100, 1)
  )


# Add columnd that shows the difference from the previous interval
df_g_specific_fromto <- df_g_specific_fromto %>% 
  group_by(fromto) %>% 
  mutate(
    # Difference = 2013 - 2012 per category
    diff =  round(total_trans - lag(total_trans), 1),
    # Pct Difference = ((2013-2012)/2012)*100 per category
    diffpct = round(((total_trans - lag(total_trans))/lag(total_trans))*100, 1)
    )


# Calc Differences from 2013 to 5-year average
df_avg_pre2013 <- df_g_specific_fromto %>% 
  filter(year < 2013) %>% 
  group_by(fromto) %>% 
  na.omit() %>% 
  summarise(avg_5yr_trans = mean(total_trans))

# bring these together into one variable for easy export
df_diff <- df_g_specific_fromto %>% 
  # first, filter our full from-to variable to just 2013
  filter(year == 2013) %>% 
  
  # then, rename all of the relevant columns
  # note that 20122013 means the difference between the transition totals between these years
  # i.e. 2012 is the transitions between 2011-2012 and 2013 is the transitions between 2012-2013 and the diff is the difference between these two 
  rename(total_trans_2013 = total_trans,
         diff_20122013 = diff,
         diff_20122013_pct = diffpct) %>% 
  
  # then, add in our 5 year prior averages
  # this is the avg. transition from 2008 (i.e. 2007-2008) to 2012 (i.e. 2011-2012)
  left_join(df_avg_pre2013) %>%
  mutate(
    diff_5yr = total_trans_2013 - avg_5yr_trans,
    dff_5yr_pct = ((total_trans_2013 - avg_5yr_trans) / avg_5yr_trans) * 100
    ) %>% 
  ungroup() %>% 
  select(-year) %>% 
  mutate_if(is.numeric, round)


# Write to CSV for easy copy-pasting into manuscript
write.csv(df_diff, "../Figures/transition_table_mapb_2013stats.csv", row.names = F)

### BR & Cerrado Soybean Stats ###
df_br_s <- df %>% 
  filter(to_level_4 != from_level_4) %>%
  filter(to_level_4 == "Soy Beans")

df_br_s_year <- df_br_s %>% 
  group_by(year, to_level_4) %>% 
  na.omit() %>% 
  summarise(total_trans = sum(ha)) %>% 
  filter(year %in% yr_range)

unique(df$biome)
