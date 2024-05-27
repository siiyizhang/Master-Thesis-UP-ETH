# compute the ratio of zoo and phyto species number

library(reshape2)
library(ggplot2)
library(scales)
library(sf)
library(rnaturalearth)
library(R.matlab)
library(dplyr)
library(raster)
library(rgdal)

plankton_pixel = list()
for (m in 1:12){
  plankton_pixel[[m]] = plankton_simple_biome[plankton_simple_biome$month==m,c(1,2,3,5:(ncol(plankton_simple_biome)-2))] # try January
  lonlat=as.data.frame(readMat("/net/meso/work/siyzhang/urs/Data/HelpVariables.mat")$LatLon)
  colnames(lonlat)[2:5]=c("lon","lat","y_lon","x_lat")
  for (i in 1:nrow(plankton_pixel[[m]])){
    current_pixel = plankton_pixel[[m]][i,]
    # Vector of species names
    species_vector <- t(current_pixel[,4:ncol(current_pixel)]);
    species_df = data.frame(species=rownames(species_vector),record=species_vector[,1])
    merged_df = merge(species_df, att_new,by="species")
    # Count the number of phytoplankton and zooplankton
    zoo_count <- sum(merged_df$record[merged_df$type == "zoo"])
    phyto_count <- sum(merged_df$record[merged_df$type == "phyto"])
    ratio=zoo_count/phyto_count
    plankton_pixel[[m]][i,"ratio"]=ratio
  }
  plankton_pixel[[m]] = left_join(lonlat, plankton_pixel[[m]], by=c("y_lon","x_lat"))
}
ratio_pixel = plankton_pixel[[1]][,c("lon","lat")]
for (m in 1:12){
  ratio_pixel=cbind(ratio_pixel,plankton_pixel[[m]]$ratio)
}
colnames(ratio_pixel)=c("lon","lat",month.name)
replace_inf_with_max <- function(x) {
  # Find the maximum value in the row, excluding Inf
  max_val <- max(x[x != Inf], na.rm = TRUE)
  
  # Replace Inf values with this maximum value
  x[x == Inf] <- max_val
  
  # Return the modified row
  return(x)
} # a function to replace Inf with the maximum value
ratio_pixel <- apply(ratio_pixel, 1, replace_inf_with_max)
ratio_pixel <- as.data.frame(t(ratio_pixel))

ratio_mean_pixel=ratio_pixel[,1:2]
ratio_mean_pixel$ratio=rowMeans(ratio_pixel[,3:14],na.rm = T)
ratio_mean_pixel <- ratio_mean_pixel %>%
  arrange(desc(lat), lon)
# Create a raster with 180 rows and 360 columns
raster_ratio <- raster(nrows=180, ncols=360)

# Set the extent to cover the global scale: longitude from -180 to 180, latitude from -90 to 90
extent(raster_ratio) <- c(-180, 180, -90, 90)

# Set the projection to WGS84 (this is a common choice for global data)
crs(raster_ratio) <- CRS("+proj=longlat +datum=WGS84")
values(raster_ratio) <- ratio_mean_pixel$ratio
plot(raster_ratio, main="Global Distribution of Ratio")

DOC_pixel=X41586_2023_6772_MOESM3_ESM
colnames(DOC_pixel)[3]="flux"
DOC_pixel$lon <- DOC_pixel$lon - 1
# Convert Dataset2 points to a spatial format
coordinates(DOC_pixel) <- ~lon+lat  # Define spatial properties
proj4string(DOC_pixel) <- CRS("+proj=longlat +datum=WGS84")  # Define projection
# Create a raster template based on the extent and resolution of DOC_ratio
raster_DOC <- raster(nrows=90, ncols=180, xmn=0, xmx=360, ymn=-90, ymx=90)  # Change xmn to -180 and xmx to 180 if needed
crs(raster_DOC) <- CRS("+proj=longlat +datum=WGS84")  # Ensure proper CRS

# Rasterize
DOC_raster <- rasterize(DOC_pixel, raster_DOC, field="flux", fun=mean)
# Rotate the raster to shift longitudes from 0-360 to -180 to 180
DOC_raster_rotated <- rotate(DOC_raster)
# Resample raster_ratio to match DOC_raster
raster_DOC_resample <- resample(DOC_raster_rotated, raster_ratio,method="bilinear")
masked_raster_DOC_resample <- mask(raster_DOC_resample, raster_ratio)
plot(masked_raster_DOC_resample, main="Global Distribution of Ratio")
####################correlation analysis
# Extract raster values
DOC_values <- values(raster_DOC_resample)
ratio_values <- values(raster_ratio)

# Remove NA values for correlation
valid_indices <- !is.na(DOC_values) & !is.na(ratio_values)
DOC_values <- DOC_values[valid_indices]
ratio_values <- ratio_values[valid_indices]

plot(ratio_values, DOC_values, xlab="Ratio Values", ylab="DOC Values", 
     main="Scatterplot with Regression Line",
     pch=20, cex=0.5,col=rgb(0, 0, 1, 0.07))  # Blue with 50% transparency
# Fit a linear model
model <- lm(DOC_values ~ ratio_values)

# Add a regression line
abline(model, col="red")  # Red color for the regression line
# Calculate correlation
correlation_result <- cor(DOC_values, ratio_values, method = "spearman")  # choose "spearman" if non-parametric test is needed

test_result <- cor.test(DOC_values, ratio_values, method = "spearman")  # Change method as needed

# Print results
print(test_result)
# S = 3.1954e+12, p-value < 2.2e-16
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
#   rho 
# 0.4479981 

########### plot latitudinal pattern
ratio_values <- getValues(raster_ratio);DOC_values=getValues(masked_raster_DOC_resample)
coords <- xyFromCell(raster_ratio, seq_len(ncell(raster_ratio)))
# Create a data frame
ratio_df <- data.frame(coords, ratio_values);doc_df=data.frame(coords, DOC_values)
colnames(ratio_df) <- c("Longitude", "Latitude", "Value")
colnames(doc_df) <- c("Longitude", "Latitude", "Value")
# Calculate mean value per latitude
mean_values_per_latitude <- aggregate(Value ~ Latitude, ratio_df, mean)
mean_doc_per_latitude <- aggregate(Value ~ Latitude, doc_df, mean)

# Plotting
ggplot(ratio_df, aes(x = Latitude, y = Value)) +
  geom_point(alpha = 0.05) +  # Show individual data points
  geom_line(data = mean_values_per_latitude, aes(y = Value), color = "red") +  # Mean values line
  labs(x = "Latitude", y = "Mean Value", title = "Latitudinal Pattern of Z:P Diversity Ratio") +
  scale_x_continuous(
    breaks = seq(from = floor(min(ratio_df$Latitude)), 
                 to = ceiling(max(ratio_df$Latitude)), 
                 by = 15)
  ) +
  theme_minimal()



#######################
# Get world map
world <- ne_countries(scale = "medium", returnclass = "sf")
world_mollweide <- st_transform(world, crs = "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs")
# Assuming your data is in a data.frame, convert it to an sf object first
plankton_sf <- st_as_sf(plankton_pixel[[m]]_lonlat, coords = c("lon", "lat"), crs = 4326)  # WGS84

# Transform coordinates to match the Mollweide projection
plankton_sf <- st_transform(plankton_sf, st_crs(world_mollweide))
plankton_sf$ratio[is.infinite(plankton_sf$ratio)] <- max(plankton_sf$ratio[plankton_sf$ratio != Inf])


ggplot() +
  geom_sf(data = world_mollweide) +
  geom_sf(data = plankton_sf, aes(color = ratio), size = 1) +
  scale_color_gradient(
    low = "white",  # Light green for low values
    high = "forestgreen",  # Dark green for high values
    limits = c(NA, 10),  # Set the upper limit of the color scale to 10
    oob = scales::squish,  # Use squish to cap values out of bounds to the limits
    breaks = c(0, 5, 10),  # Define where the ticks should be
    labels = c("0", "5", ">10")  # Custom labels for each break
  ) +
  coord_sf(crs = st_crs(world_mollweide), datum = NA) +  # Ensuring use of the same CRS
  theme_minimal() +
  labs(title = "Z:P ratio of species number, January",
       color = "Ratio")

ggplot() +
  geom_sf(data = world_mollweide) +
  geom_sf(data = plankton_sf, aes(color = logratio), size = 1) +
  scale_color_gradient(
    low = "forestgreen",  # Light green for low values
    high = "white"  # Dark green for high values
  ) +
  coord_sf(crs = st_crs(world_mollweide), datum = NA) +
  theme_minimal() +
  labs(
    title = "log (Z:P) ratio of species number",
    color = "Log Ratio"
  )
#############
############# link biome to ecosystem function (carbon pump)

