# name: x_figures_CerradoDrought_20112012.R
# purpose: plot the Brazilian Integrated Drought Index during 2011-2012 to see regional impacts to Cerrado

# Data Source: https://data.mendeley.com/datasets/dd95bhn7mh/1

# Created: March 2025
# Last Edited: March 2025

library(terra)
library(geobr)
library(dplyr)
library(ggplot2)
library(tidyterra)
library(sf)

# Define the file path
nc_file <- "../Data_Source/Zeri2024/IDI3.nc"

# Open the NetCDF file as a SpatRaster
r <- rast(nc_file)

# Print summary information
print(r)

# Plot the first layer (if applicable)
plot(r[[1]])

# load in Cerrado shapefile using 'geobr'
cerr_shp <- read_biomes(
  year = 2019,
  simplified = T,
  showProgress = T
) %>% dplyr::filter(name_biome == "Cerrado")

cerr_shp2 <- as_spatvector(cerr_shp)

# filter to 2011; January 2011 = 94 since April 2003 = 1. Apr2003=1, Jan2004=10, Jan2011 = 10+(12*7)
r_2011 <- r[[94:105]]
r_2012 <- r[[106:117]]

#test plot
terra::plot(r_2011[[1]],col =  map.pal("viridis",100))

# set output folder
output_folder <- "../Figures/CerradoDrought20112012"

# Define fixed breaks and color scale
breaks <- 1:7  # Intervals from 1 to 6 (7 is the upper limit)
colors <- terrain.colors(length(breaks) - 1)  # Create color palette

# facet-wrap

# Combine all layers into a single data frame
r_2011_df <- bind_rows(lapply(1:nlyr(r_2011), function(i) {
  r_2011[[i]] %>%
    as.data.frame() %>%
    mutate(Month = i)
}))

# Create facet wrap plot
ggplot() +
  geom_spatraster(data = r_2011_df, aes(fill = value)) +
  scale_fill_gradientn(
    colors = rev(terrain.colors(6)),
    limits = c(1, 6),
    breaks = 1:6
  ) +  # Adjust color scale if needed
  geom_sf(data = cerr_shp2, fill = NA, color = "black", lwd = 1) +
  labs(title = "Y: 2011") +
  theme_minimal() +
  facet_wrap(~ Month)
  
# Loop through each layer and save to file
for (i in 1:nlyr(r_2011)) {
  # Create plot with tidyterra + ggplot2
  p <- ggplot() +
    geom_spatraster(data = r_2011[[i]]) +
    scale_fill_gradientn(
      colors = rev(terrain.colors(6)),
      limits = c(1, 6),
      breaks = 1:6
    ) +  # Adjust color scale if needed
    geom_sf(data = cerr_shp2, fill = NA, color = "black", lwd = 1) +
    labs(title = paste("Y: 2011, M:", i)) +
    theme_minimal()
  
  # Save to file
  file_name <- file.path(output_folder, paste0("LayerMonth_2011_", i, ".png"))
  ggsave(file_name, plot = p, width = 8, height = 6)
}


# one example 
ggplot() +
  geom_spatraster(data = r_2011[[1]]) +
  scale_fill_gradientn(
    colors = rev(terrain.colors(6)),
    limits = c(1, 6),
    breaks = 1:6,
    name = "Drought Index",
    na.value = "white"
  ) +  # Adjust color scale if needed
  geom_sf(data = cerr_shp2, fill = NA, color = "black", lwd = 1) +
  labs(title = paste("Y: 2011, M:", "1")) +
  theme_minimal()+
  theme(
    axis.text = element_blank(),
    plot.title = element_text(h = 0.5)
  )

# Create a list of plots for each layer
plots_2011 <- lapply(1:nlyr(r_2011), function(i) {
  ggplot() +
    geom_spatraster(data = r_2011[[i]]) +
    scale_fill_gradientn(
      colors = rev(terrain.colors(6)),
      limits = c(1, 6),
      breaks = 1:6,
      name = "Drought Index",
      na.value = "white"
    ) +  # Adjust color scale if needed
    geom_sf(data = cerr_shp2, fill = NA, color = "black", lwd = 0.3) +
    labs(title = paste("Y: 2011, M:", i)) +
    theme_minimal()+
    theme(
      axis.text = element_blank(),
      plot.title = element_text(h = 0.5)
    )
})

plots_2012 <- lapply(1:nlyr(r_2012), function(i) {
  ggplot() +
    geom_spatraster(data = r_2012[[i]]) +
    scale_fill_gradientn(
      colors = rev(terrain.colors(6)),
      limits = c(1, 6),
      breaks = 1:6,
      name = "Drought Index",
      na.value = "white"
    ) +  # Adjust color scale if needed
    geom_sf(data = cerr_shp2, fill = NA, color = "black", lwd = 0.3) +
    labs(title = paste("Y: 2012, M:", i)) +
    theme_minimal()+
    theme(
      axis.text = element_blank(),
      plot.title = element_text(h = 0.5)#,
      #legend.position = "bottom"
    )
})

# Combine plots into a single plot using patchwork
library(patchwork)
combined_plot11 <- wrap_plots(plots_2011, ncol = 3, guides = "collect")
combined_plot12 <- wrap_plots(plots_2012, ncol = 3, guides = "collect")

#combined_plot

file_name <- file.path(output_folder, paste0("Wrap_2011.png"))
ggsave(file_name, plot = combined_plot11, width = 9, height = 8)

file_name <- file.path(output_folder, paste0("Wrap_2012.png"))
ggsave(file_name, plot = combined_plot12, width = 9, height = 8)





# ### MAKING A GIF ###
# # Future work; needs to change the file labels to have 01, 02, instead of 1, 2, or else it will go 1, 10, 11, 12, 2, 3
# library(magick)
# 
# # Define the folder containing the PNG files
# image_folder <- "path/to/your/image/folder"
# 
# # List PNG files in the folder (sorted by name)
# png_files <- list.files(image_folder, pattern = "\\.png$", full.names = TRUE)
# png_files <- sort(png_files)  # Ensure the order is correct
# 
# # Read images and combine them into a GIF
# gif <- image_read(png_files) %>%
#   image_animate(fps = 5)  # Adjust 'fps' for speed (frames per second)
# 
# # Save the GIF
# output_file <- file.path(image_folder, "animation.gif")
# image_write(gif, output_file)
# 
# cat("GIF saved to:", output_file, "\n")