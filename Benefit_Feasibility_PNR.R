library(terra)
library(scales)
library(raster)
library(sp)
library(dplyr)

# Set working directory
setwd("R:/Natural_regneration_BF_datasets/Chapter_2_analysis")

##### First, convert potential for natural regeneration raster into points with attribute table #########

# Load the Potential for natural regeneration file
pnr_raster <- raster("Data/PNR_1KM.tif")

# Convert pnr raster to points
pnr_points <- rasterToPoints(pnr_raster, spatial = TRUE)

# Check the structure of the points data
str(pnr_points)

# Print the first few rows of the data part, this should be the potential score (0-1)
head(pnr_points@data)

# Print the first few coordinates, with x,y represents longitude and latitude respectively
head(pnr_points@coords)

# Extract coordinates, to make the attribute table more informed
pnr_coords <- coordinates(pnr_points)

# Extract the raster values
pnr_values <- as.data.frame(pnr_points)[, 1]

# Combine coordinates and values into a data frame
pnr_df <- data.frame(Longitude = pnr_coords[, 1], Latitude = pnr_coords[, 2], PNR_score = pnr_values)

# Print the first few rows of the pnr data frame to verify
print(head(pnr_df))


##### Second, pre-process benefits variables, mask into same extent/projection/resolution with the pnr layer ######

######## carbon layer ####### trial#########

# Load the carbon sequestration raster
carbon_raster <- rast("Data/Benefits/Cook_Patton_Carbon/young_forest_sequestration_rate_Griscom_extent.tif")

# Load pnr raster again easier in using terra for reprojection
project_pnr <- rast("Data/PNR_1KM.tif")

# Reproject the carbon raster to match the CRS of the PNR raster
carbon_raster <- terra::project(carbon_raster, project_pnr)

# Resample the carbon raster to match the extent and resolution of the PNR raster
carbon_raster <- terra::resample(carbon_raster, project_pnr, method='bilinear')

# Convert terra raster to raster object for extract coordination/carbon value/create dataframe
carbon_raster <- raster(carbon_raster)

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
carbon_rescale_raster <- calc(carbon_raster, rescale_with_na)

# Verify the rescaled values
carbon_min_rescaled <- minValue(carbon_rescale_raster)
carbon_max_rescaled <- maxValue(carbon_rescale_raster)

# Print the rescaled minimum and maximum values
print(paste("Minimum value of rescaled Carbon raster:", carbon_min_rescaled))
print(paste("Maximum value of rescaled Carbon raster:", carbon_max_rescaled))

# Extract coordinates and values using raster functions
carbon_values <- extract(carbon_rescale_raster, pnr_coords)
carbon_df <- data.frame(Longitude = pnr_coords[, 1], Latitude = pnr_coords[, 2], Carbon_score = carbon_values)
print(head(carbon_df))

### the above trial match with pnr points, but there are some NA values ######

######## all these following are trials, using raster function, but the points did not match with the pnr layer ######




carbon_points <- rasterToPoints(carbon_rescale_raster, spatial = FALSE)
carbon_coords <- carbon_points[, 1:2]
carbon_values <- carbon_points[, 3]
colnames(carbon_points) <- c("Longitude", "Latitude", "Carbon_score")
carbon_df <- data.frame(Longitude = carbon_coords[, 1], Latitude = carbon_coords[, 2], Carbon_score = carbon_values)

# Check the structure and head of the data frame to ensure correctness
print(head(carbon_df))

merged_df <- merge(pnr_df, carbon_df, by = c("Longitude", "Latitude"))
print(head(merged_df))









# Ensure the carbon layer has the same projection, extent, resolution with the pnr layer
carbon_raster <- terra::project(carbon_raster, pnr_raster)
carbon_raster <- terra::resample(carbon_raster, pnr_raster, method='bilinear')

# Custom rescale function that handles NA values
rescale_with_na <- function(x) {
  if (all(is.na(x))) {
    return(x)
  }
  valid_values <- !is.na(x)
  x[valid_values] <- scales::rescale(x[valid_values], to = c(0, 1))
  return(x)
}

#terra
rescale_with_na <- function(x) {
  valid_values <- !is.na(values(x))
  x[valid_values] <- scales::rescale(values(x)[valid_values], to = c(0, 1))
  return(x)
}

# Rescale the carbon layer to 0-1 range while handling NA values
carbon_min <- minmax(carbon_raster)[1]
carbon_max <- minmax(carbon_raster)[2]
carbon_raster <- (carbon_raster - carbon_min) / (carbon_max - carbon_min)
carbon_points <- as.data.frame(terra::xyFromCell(carbon_raster, 1:ncell(carbon_raster)))
carbon_values <- terra::values(carbon_raster)
carbon_df <- data.frame(carbon_points, Carbon_score = carbon_values)

colnames(carbon_df) <- c("Longitude", "Latitude", "Carbon_score")

# Check the structure and head of the data frame to ensure correctness
print(head(carbon_df))




print(minmax(carbon_raster))

carbon_raster <- app(carbon_raster, fun = rescale_with_na)

# Verify the rescaled values
carbon_min_rescaled <- minValue(carbon_raster)
carbon_max_rescaled <- maxValue(carbon_raster)

# Print the rescaled minimum and maximum values
print(paste("Minimum value of rescaled Carbon raster:", carbon_min_rescaled))
print(paste("Maximum value of rescaled Carbon raster:", carbon_max_rescaled))

# Convert the rescaled carbon raster to points and extract coordinates and values
carbon_points <- as.data.frame(terra::xyFromCell(carbon_raster, 1:ncell(carbon_raster)))
carbon_values <- as.data.frame(values(carbon_raster))
carbon_df <- data.frame(carbon_points, Carbon_score = carbon_values[,1])
colnames(carbon_df) <- c("Longitude", "Latitude", "Carbon_score")
print(head(carbon_df))



carbon_points <- rasterToPoints(carbon_raster, spatial = TRUE)
carbon_coords <- coordinates(carbon_points)
carbon_values <- as.data.frame(carbon_points)[, 1]

# Combine coordinates and values into a data frame for Carbon raster
colnames(carbon_points) <- c("Longitude", "Latitude", "Carbon_score")
carbon_df <- data.frame(Longitude = carbon_coords[, 1], Latitude = carbon_coords[, 2], Carbon_score = carbon_values)

# Check the structure of the carbon points dataframe
print(head(carbon_df))

# IUCN threatened biodiversity layer#

# Load the biodiversity raster
bio_raster <- raster("Data/Benefits/Combined_THR_SR_2023/Combined_THR_SR_2023.tif")

# Ensure the bio layer has the same projection, extent, resolution with the pnr layer
bio_raster <- projectRaster(bio_raster, raster_pnr)

# Rescale the bio layer to 0-1 range while handling NA values
bio_reraster <- calc(bio_raster, rescale_with_na)

# Verify the rescaled values
bio_min_rescaled <- minValue(bio_reraster)
bio_max_rescaled <- maxValue(bio_reraster)

# Print the rescaled minimum and maximum values
print(paste("Minimum value of rescaled Biodiversity raster:", bio_min_rescaled))
print(paste("Maximum value of rescaled Biodiversity raster:", bio_max_rescaled))

# Convert the rescaled bio raster to points and extract coordinates and values
bio_points <- rasterToPoints(bio_reraster, spatial = TRUE)
bio_coords <- coordinates(bio_points)
bio_values <- as.data.frame(bio_points)[, 1]

# Combine coordinates and values into a data frame for bio raster
bio_df <- data.frame(Longitude = bio_coords[, 1], Latitude = bio_coords[, 2], bio_score = bio_values)

# Check the structure of the bio points dataframe
print(head(bio_df))

######################## Feasibility layer ###############################
#Cropland opportunity cost layer

# Load the crop land opportunity cost raster
cropland_raster <- rast("Data/Feasibility/Vincent_opportunity_cost/Cropland_opp_cost_rast.tif")

# Ensure the cropland layer has the same projection, extent, resolution with the pnr layer
cropland_raster <- projectRaster(cropland_raster, raster_pnr)

# Rescale the cropland layer to 0-1 range while handling NA values
cropland_rescale_raster <- calc(cropland_raster, rescale_with_na)

# Verify the rescaled values
cropland_min_rescaled <- minValue(cropland_rescale_raster)
cropland_max_rescaled <- maxValue(cropland_rescale_raster)

# Print the rescaled minimum and maximum values
print(paste("Minimum value of rescaled Cropland opportunity raster:", cropland_min_rescaled))
print(paste("Maximum value of rescaled Cropland opportunity raster:", cropland_max_rescaled))

# Convert the rescaled bio raster to points and extract coordinates and values
cropland_points <- rasterToPoints(cropland_rescale_raster, spatial = TRUE)
cropland_coords <- coordinates(cropland_points)
cropland_values <- as.data.frame(cropland_points)[, 1]

# Combine coordinates and values into a data frame for bio raster
cropland_df <- data.frame(Longitude = cropland_coords[, 1], Latitude = cropland_coords[, 2], cropland_score = cropland_values)

# Check the structure of the bio points dataframe
print(head(cropland_df))





















