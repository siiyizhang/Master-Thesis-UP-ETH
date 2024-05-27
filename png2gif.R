# Install the magick package if you haven't already
install.packages("magick")

# Load the magick package
library(magick)

# Create a vector with the paths to your PNG files
png_files <- list.files(path = "/net/meso/work/siyzhang/thesis/urs/Data/05Biomes/Plots/monthlyBiome", 
                        pattern = "\\.png$", full.names = TRUE)
tmp=png_files[2:4]; png_files[2:9]=png_files[5:12]
png_files[10:12]=tmp
titled_images_dir <-"/net/meso/work/siyzhang/thesis/urs/Data/05Biomes/Plots/titledMonthlyBiome"

month=c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
# Loop over PNG files to read, annotate with title, and add to the image list
for (i in seq_along(png_files)) {
  # Read the PNG file
  img <- image_read(png_files[i])
  
  # Define a title for the image (customize as needed)
  title <- month[i]
  
  # Annotate the image with the title
  img_annotated <- image_annotate(img, title, gravity = 'north', location = '+0+200', size = 80, color = "black")
  image_write(img_annotated, file.path(titled_images_dir, paste0("titled_frame_", i, ".png")))
  
}
annotated_images <- image_read(list.files(path = titled_images_dir, pattern = "\\.png$", full.names = TRUE))
order=c(1,5:12,2:4)
reordered_images <- annotated_images[order]
# Animate the images
animated_gif <- image_animate(reordered_images, fps = 2)  # Adjust FPS for desired speed

# Save the animated GIF
image_write(animated_gif, "/net/meso/work/siyzhang/thesis/urs/Data/05Biomes/animatedMonthly.gif")


####for seasonal biomes
png_files_season <- list.files(path = "/net/meso/work/siyzhang/urs/Data/05Biomes/Plots/seasonalBiome", 
                        pattern = "\\.png$", full.names = TRUE)
titled_images_season_dir <-"/net/meso/work/siyzhang/urs/Data/05Biomes/Plots/titledSeasonalBiome"
season=c("Spring","Summer","Autumn","Winter")
for (i in seq_along(png_files_season)) {
  img <- image_read(png_files_season[i])
  title <- season[i]
  img_annotated <- image_annotate(img, title, gravity = 'north', location = '+0+200', size = 80, color = "black")
  image_write(img_annotated, file.path(titled_images_season_dir, paste0("titled_frame_", i, ".png")))
  
}
annotated_images_season <- image_read(list.files(path = titled_images_season_dir, pattern = "\\.png$", full.names = TRUE))
animated_gif_season <- image_animate(annotated_images_season, fps = 2)  # Adjust FPS for desired speed
image_write(animated_gif_season, "/net/meso/work/siyzhang/urs/Data/05Biomes/animatedSeason.gif")
