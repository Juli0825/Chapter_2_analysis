library(terra)
library(scales)
library(raster)
library(sp)
library(dplyr)
library(sf)
library(exactextractr)

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

# Subset to remove rows where PNR_score is 0
pnr_df <- pnr_df[pnr_df$PNR_score != 0, ]

# Save the dataframe as an RData file
save(pnr_df, file = "./preprocessing/pnr_data.RData")

# Load the saved RData file
load("preprocessing/pnr_data.RData")

# Print the first few rows of the pnr data frame to verify
print(head(pnr_df))


##### Second, pre-process benefits variables, mask into same extent/projection/resolution with the pnr layer ######

######## carbon layer#########

# Load the carbon sequestration raster
carbon_raster <- rast("Data/Benefits/Cook_Patton_Carbon/sequestration_rate__mean__aboveground__full_extent__Mg_C_ha_yr.tif")

# Load pnr raster again easier in using terra for reprojection
project_pnr <- rast("Data/PNR_1KM.tif")

# Ensure the CRS of the PNR raster and carbon raster are the same
crs(carbon_raster) <- crs(project_pnr)

# Reproject the carbon raster to match the CRS of the PNR raster
carbon_raster_proj <- terra::project(carbon_raster, project_pnr)

# Convert terra raster to raster object for extract coordination/carbon value/create dataframe
carbon_raster <- raster(carbon_raster_proj)

# Extract coordinates from the small pnr dataframe
pnr_df_coords <- pnr_df[,1:2]

# Extract carbon values according to small PNR points
carbon_values <- extract(carbon_raster, pnr_df_coords)

#plot(points(pnr_df_coords, col='red', pch=19))

# Count the number of NA values
na_count <- sum(is.na(carbon_values))
na_count

# Directly adding carbon value into the existed pnr dataframe
pnr_df$carbon <- carbon_values

# Save the with carbon value pnr dataframe as an RData file
save(pnr_df, file = "./preprocessing/pnr_data.RData")

# Load the saved RData file
load("preprocessing/pnr_data.RData")

# Check carbon dataframe
print(head(pnr_df))

################## IUCN threatened biodiversity layer ################################################

# Load the biodiversity raster using terra
bio_rast <- raster("Data/Benefits/Combined_THR_SR_2023/Combined_THR_SR_2023.tif")

# Ensure the CRS of the PNR raster and bio rast are the same
crs(bio_rast) <- crs(project_pnr)

# Resample the reprojected raster (this doesn't work, resulted in all NaN values)
#bio_rast <- resample(bio_rast, project_pnr, method = 'bilinear')

# try another method (this disaggregate needs to be run under raster raster, doesn't wrok as well)

# use the disaggregate function, split each large cell into multiple smaller cells, maintaining the original values
# Calculate the disaggregation factor
# factor_x <- round(res(bio_rast)[1] / res(project_pnr)[1])
# factor_y <- round(res(bio_rast)[2] / res(project_pnr)[2])
# 
# # Define a function to disaggregate in steps
# disaggregate_in_steps <- function(raster, factor_x, factor_y, step = 10) {
#   current_raster <- raster
#   remaining_factor_x <- factor_x
#   remaining_factor_y <- factor_y
#   
#   while (remaining_factor_x > step || remaining_factor_y > step) {
#     step_x <- min(step, remaining_factor_x)
#     step_y <- min(step, remaining_factor_y)
#     
#     current_raster <- disaggregate(current_raster, fact = c(step_x, step_y))
#     
#     remaining_factor_x <- remaining_factor_x / step_x
#     remaining_factor_y <- remaining_factor_y / step_y
#   }
#   
#   disaggregate(current_raster, fact = c(remaining_factor_x, remaining_factor_y))
# }
# 
# # Disaggregate the bio_rast in steps to match the resolution of project_pnr
# bio_rast_disaggregated <- disaggregate_in_steps(bio_rast, factor_x, factor_y)
# 
# # Verify the resolution of the disaggregated raster
# print(res(bio_rast_disaggregated))
# 



# Reproject the bio rast to match the CRS of the PNR raster
#bio_rast_proj <- terra::project(bio_rast, project_pnr)

# bio_rast_resampled <- terra::resample(bio_rast, project_pnr, method='bilinear')
# 
# # Convert bio terra raster to raster object for extract coordination/carbon value/create dataframe
# bio_raster <- raster(bio_rast_proj)
# 
# # Extract bio values according to small PNR points
# bio_values <- extract(bio_raster, pnr_df_coords)
# 
# # Count the number of NA values
# na_count <- sum(is.na(bio_values))
# na_count

######## Try raster funtion projectRaster ## Warning messages: Point outside of projection domain (GDAL error 1), but values seemed to be kept alright)
raster_pnr <- raster("Data/PNR_1KM.tif")
# Ensure the bio layer has the same projection, extent, resolution with the pnr layer
bio_raster <- projectRaster(bio_rast, raster_pnr)


######################## Feasibility layer ###############################
# opportunity cost layer

# Load the land opportunity cost raster, All_cost.tif raster is a combination of both cropland and pastureland opportunity cost layers, 
#where the highest value is used (ie. if the opportunity cost value of cropland is higher than pastureland where they overlap, cropland is used)

landcost_rast <- rast("Data/Feasibility/Vincent_opportunity_cost/All_cost.tif")

# Ensure the cropland layer has the same projection, extent, resolution with the pnr layer
landcost_rast_proj <- terra::project(landcost_rast, project_pnr)

# Convert terra raster to raster object for extract coordination/carbon value/create dataframe
landcost_raster <- raster(landcost_rast_proj)

# Extract carbon values according to small PNR points (0.2748491 of Na VALUES)
landcost_values <- extract(landcost_raster, pnr_df_coords)

# Directly adding landcost value into the existed pnr dataframe
pnr_df$landcost <- landcost_values

# Save the new pnr dataframe (with carbon/landcost) as an RData file
save(pnr_df, file = "./preprocessing/pnr_data.RData")

# Load the saved RData file
load("preprocessing/pnr_data.RData")

# Check carbon dataframe
print(head(pnr_df))





















