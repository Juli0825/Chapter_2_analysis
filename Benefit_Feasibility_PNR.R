library(raster)
library(sp)

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

##### Second, pre-process benefits variables, mask into same extent/projection/resolution ######

#Biodiversity layer


















