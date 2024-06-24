library(raster)
library(sp)
library(scales)

# Set working directory
setwd("R:/Natural_regneration_BF_datasets/Chapter_2_analysis")

##### First, convert potential for natural regeneration raster into points with attribute table #########

# Load the Potential for natural regeneration file
pnr <- "Data/PNR_1KM.tif"
raster_pnr <- raster(pnr)

# Convert pnr raster to points
points_pnr <- rasterToPoints(raster_pnr, spatial = TRUE)

# Check the structure of the points data
str(points_pnr)

# Print the first few rows of the data part, this should be the potential score (0-1)
head(points_pnr@data)

# Print the first few coordinates, with x,y represents longitude and latitude respectively
head(points_pnr@coords)

# Extract coordinates, to make the attribute table more informed
coords <- coordinates(points_pnr)

# Extract the raster values
pnrvalues <- as.data.frame(points_pnr)[, 1]

# Combine coordinates and values into a data frame
points_pnr_df <- data.frame(Longitude = coords[, 1], Latitude = coords[, 2], Potential_score = pnrvalues)

# Check the structure of the corrected pnr data frame
str(points_pnr_df)

# Print the first few rows of the pnr data frame to verify
print(head(points_pnr_df))

# Check the minimum and maximum values of the PNR raster (if it's 0-1 ranged)
pnr_min <- minValue(raster_pnr)
pnr_max <- maxValue(raster_pnr)

# Print the minimum and maximum values
print(paste("Minimum value of PNR raster:", pnr_min))
print(paste("Maximum value of PNR raster:", pnr_max))

##### Second, pre-process benefits variables, mask into same extent/projection/resolution ######

#carbon layer

# Load the carbon sequestration raster
carbon_raster <- raster("Data/Benefits/Cook_Patton_Carbon/young_forest_sequestration_rate_Griscom_extent.tif")

# Ensure the carbon layer has the same projection, extent, resolution with the pnr layer
carbon_raster <- projectRaster(carbon_raster, raster_pnr)

# Custom rescale function that handles NA values
rescale_with_na <- function(x) {
  if (all(is.na(x))) {
    return(x)
  }
  valid_values <- !is.na(x)
  x[valid_values] <- scales::rescale(x[valid_values], to = c(0, 1))
  return(x)
}

# Rescale the carbon layer to 0-1 range while handling NA values
carbon_raster <- calc(carbon_raster, rescale_with_na)

# Verify the rescaled values
carbon_min_rescaled <- minValue(carbon_raster)
carbon_max_rescaled <- maxValue(carbon_raster)

# Print the rescaled minimum and maximum values
print(paste("Minimum value of rescaled Carbon raster:", carbon_min_rescaled))
print(paste("Maximum value of rescaled Carbon raster:", carbon_max_rescaled))

# Convert the rescaled carbon raster to points and extract coordinates and values
carbon_points <- rasterToPoints(carbon_raster, spatial = TRUE)
carbon_coords <- coordinates(carbon_points)
carbon_values <- as.data.frame(carbon_points)[, 1]

# Combine coordinates and values into a data frame for Carbon raster
carbon_df <- data.frame(Longitude = carbon_coords[, 1], Latitude = carbon_coords[, 2], Carbon_score = carbon_values)

# Check the structure of the carbon points dataframe
print(head(carbon_df))

# IUCN threatened biodiversity layer#



