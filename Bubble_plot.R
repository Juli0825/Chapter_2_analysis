library(writexl)
library(readxl)
library(ggplot2) 
library(tidyverse)
library(viridis)


# Set working directory
setwd("R:/Natural_regneration_BF_datasets/Chapter_2_analysis")
#setwd("/Users/uqmluski/Dropbox/UQ HDR student projects (PhD, masters, honours)/Juli PhD UQ projects/Juli Chapter 2 benefits and feasibility")

################### Stats bubble plot/ circle plot #################
# Load the cleaned Excel file
cleaned_lmics <- read_excel("R:/NRP_2024-A13363/Natural_regneration_BF_datasets/Chapter_2_analysis/stat_results/cleaned_lmics.xlsx")
cleaned_lmics=read.csv('cleaned_lmics.csv')
# Calculate additional columns
head(cleaned_lmics)
cleaned_lmics <- cleaned_lmics %>%
  mutate(
    proportion_category_4 = Category_4_km2 / total_area * 100,   # Proportion of Category 4
    total_km2 = total_area  # Use total_area for plotting
  )
head(cleaned_lmics)
cleaned_lmics$total_km2x1000=cleaned_lmics$total_km2/1000
cleaned_lmics$prop_cat4x100=cleaned_lmics$proportion_category_4/100
cleaned_lmics$cat4x1000=cleaned_lmics$Category_4_km2/1000

head(cleaned_lmics)

############## repeat again on log scale 

bubble <- ggplot(cleaned_lmics, aes(x = total_km2x1000, y = cat4x1000, size = proportion_category_4, label = NAME_0)) +
  coord_cartesian(xlim = c(.5, 2500), ylim = c(.1, 400), clip = 'on') +
  geom_point(shape = 21, colour = "darkgreen", fill='#008000', stroke = 1.2, alpha = 0.7) +  # Bubble with borders
  scale_size_area(max_size = 14, guide = "none") +  # Bubble size is proportional to category 4 proportion
  #scale_fill_manual(values = c("Asia" = "lightblue", "Africa" = "orange", "South America" = "green", "North America" = "yellow")) +
  geom_text(size = 3, vjust = 1.2, hjust = 1) +  # Add country labels
  scale_x_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
                labels = scales::label_number()) +  # Real number labels
  scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
                labels = scales::label_number()) +  # Real number labels
  annotation_logticks() +
  theme_bw() +
  labs(
    title = "Total natural reforestation potential versus best price and co-benefits (countries having >0.1 km C4 land)",
    x = " Natural regeneration potential (km²)",
    y = "Reforestable area with max co-benefits (km²)",
  ) +
  theme(
    plot.title = element_text(size = 8, face = "bold"),
    axis.title = element_text(size = 12),
    legend.title = element_text(size = 12),
    legend.position = "right"
  )


# Save as a PDF to process in illustrator
ggsave("bubble_plot.pdf", plot = bubble, width = 8, height = 6, units = "in", dpi = 300)

# Create the bubble plot
# ggplot(cleaned_lmics, aes(x = total_km2x1000, y = cat4x1000, size = proportion_category_4, label = NAME_0, fill = 'CONTINENT')) +
#   coord_cartesian(xlim = c(-100, 2000), ylim = c(-25, 200), clip = 'off') +
#   geom_point(shape = 21, colour = "black", stroke = 0.5) +  # Bubble with borders
#   scale_size_area(max_size = 10, guide = "none") +  # Bubble size is proportional to category 4 proportion
#   #scale_fill_manual(values = c("Asia" = "lightblue", "Africa" = "orange", "South America" = "green", "North America" = "yellow")) +
#   geom_text(size = 1.5, vjust = 1.5, hjust = 1.5) +  # Add country labels
#   theme_bw() +
#   labs(
#     title = "Cheapest with highest co-benefits x1000 (km²)",
#     x = "Natural regeneration (km²)",
#     y = "8.0% of total reforestable area with max co-benefits (km²)",
#     fill = "Continent"
#   ) +
#   theme(
#     plot.title = element_text(size = 16, face = "bold"),
#     axis.title = element_text(size = 12),
#     legend.title = element_text(size = 12),
#     legend.position = "right"
#   )

##### line ###

# Create a new cost category based on the sum of the low, medium, and high-cost categories

cleaned_lmics <- cleaned_lmics %>%
  mutate(
    low_cost = rowSums(select(., Category_1_km2:Category_4_km2), na.rm = TRUE),
    medium_cost = rowSums(select(., Category_5_km2:Category_8_km2), na.rm = TRUE),
    high_cost = rowSums(select(., Category_9_km2:Category_12_km2), na.rm = TRUE),
    cost_category = case_when(
      low_cost > 0 ~ 1,       # Low cost
      medium_cost > 0 ~ 2,    # Medium cost
      high_cost > 0 ~ 3       # High cost
    )
  )

# Create the plot
ggplot(cleaned_lmics, aes(x = total_area, y = factor(cost_category), label = GID_0)) +
  geom_point(aes(size = total_area), alpha = 0.7) +
  geom_text(vjust = -0.5, hjust = 0.5, size = 3) +  # Add country codes
  scale_x_log10() +  # Log scale for better visualization
  scale_y_discrete(labels = c("Low cost", "Medium cost", "High cost")) +  # Adjust y-axis order
  labs(title = "Reforestable Land vs. Costs",
       x = "Reforestable Land (km², log scale)",
       y = "Cost Category",
       size = "Reforestable Land (km²)") +
  theme_minimal() +
  theme(legend.position = "none")


############################# circular barplot ###############################################
# Load the cleaned Excel file
cleaned_lmics <- read_excel("R:/NRP_2024-A13363/Natural_regneration_BF_datasets/Chapter_2_analysis/stat_results/cleaned_lmics.xlsx")

# Step 1: Transform the data into long format (Category_4_km2 and total_area), ensuring Total Area comes first
barplot <- cleaned_lmics %>%
  select(NAME_0, Biomes, total_area, Category_4_km2) %>%
  rename(individual = NAME_0, group = Biomes, value1 = total_area, value2 = Category_4_km2)

# Transform data into long format
barplot_long <- barplot %>%
  gather(key = "observation", value = "value", -c(individual, group))

# Add 'empty bars' to separate groups visually
empty_bar <- 2
nObsType <- nlevels(as.factor(barplot_long$observation))

# Create the to_add data frame with the correct number of columns
to_add <- data.frame(matrix(NA, empty_bar * nlevels(as.factor(barplot_long$group)) * nObsType, ncol(barplot_long)))

# Make sure the column names match
colnames(to_add) <- colnames(barplot_long)

# Replicate the group names in the new rows to add the empty bars between groups
to_add$group <- rep(levels(as.factor(barplot_long$group)), each = empty_bar * nObsType)

# Bind the rows together
barplot_long <- rbind(barplot_long, to_add)

# Create a separate column for total_area to use for sorting
barplot_long <- barplot_long %>%
  group_by(group, individual) %>%
  mutate(total_area = ifelse(observation == "value1", value, NA)) %>%
  fill(total_area, .direction = "downup")  # Fills the total_area column for sorting

# Arrange by biome groups, maintaining order of bars within each group based on total_area
barplot_long <- barplot_long %>%
  arrange(group, desc(total_area))  # Sort by total_area

# Create the 'id' column for positioning the bars and labels
barplot_long$id <- rep(seq(1, nrow(barplot_long) / nObsType), each = nObsType)

# Prepare data for label positions (use only total_area for labeling)
label_barplot <- barplot_long %>%
  filter(observation == "value1") %>%  # Only use total_area for label positions
  group_by(id, individual) %>%
  summarize(tot = sum(value, na.rm = TRUE))  # Summarize the total_area only

# Calculate the angle and hjust for labels
number_of_bar <- nrow(label_barplot)
angle <- 90 - 360 * (label_barplot$id - 0.5) / number_of_bar
label_barplot$hjust <- ifelse(angle < -90, 1, 0)
label_barplot$angle <- ifelse(angle < -90, angle + 180, angle)

# Step 2: Make the plot, stacking Total Area first and customizing colors
p <- ggplot(barplot_long) +      
  
  # Add the stacked bar (Category 4 first, Total Area second)
  geom_bar(aes(x = as.factor(id), y = value, fill = observation), stat = "identity", alpha = 0.7) +
  
  # Customize the colors manually (yellow for Total Area, neon purple for Category 4)
  scale_fill_manual(values = c("value2" = "#9400D3", "value1" = "#DFFF00"),  # Neon purple for Category 4, yellow for total area
                    labels = c("Category 4 Area", "Total Area")) +  
  
  # Remove background, axis text, axis titles, and panel grid
  theme_minimal() +
  theme(
    legend.position = "right",
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-1, 4), "cm")
  ) +
  
  coord_polar() +
  
  # Add country name labels (from NAME_0) without bold and with a smaller size
  geom_text(data = label_barplot, aes(x = id, y = tot + 10, label = individual, hjust = hjust), 
            color = "black", alpha = 0.6, size = 2.5, angle = label_barplot$angle, inherit.aes = FALSE) +
  
  # Set the y-axis limits to ensure all countries are visible
  ylim(-100, 10000)  # Dynamically adjust y limits

# Display the plot
p

# Save at png
ggsave(p, file="output.png", width=10, height=10)
              
              
              
              
              
              
              
              
              
# Create a dataset with total_area (yellow) and Category_4_km2 (green) for each country (NAME_0)
barplot <- cleaned_lmics %>%
  select(NAME_0, total_area, Category_4_km2) %>%
  rename(individual = NAME_0, value1 = total_area, value2 = Category_4_km2) %>%
  mutate(group = "Restoration Potential")

# Transform data into long format
barplot <- barplot %>%
  gather(key = "observation", value = "value", -c(individual, group))

# Set a number of 'empty bars' to add at the end of each group (to space things out)
empty_bar <- 20  # Increased the number of empty bars for greater spacing
nObsType <- nlevels(as.factor(barplot$observation))
to_add <- data.frame(matrix(NA, empty_bar*nlevels(barplot$group)*nObsType, ncol(barplot)))
colnames(to_add) <- colnames(barplot)
to_add$group <- rep(levels(barplot$group), each = empty_bar*nObsType)
barplot <- rbind(barplot, to_add)
barplot <- barplot %>% arrange(group, individual)
barplot$id <- rep(seq(1, nrow(barplot)/nObsType), each = nObsType)

# Get the name and the y position of each label
label_barplot <- barplot %>% group_by(id, individual) %>% summarize(tot = sum(value, na.rm = TRUE))
number_of_bar <- nrow(label_barplot)
angle <- 90 - 360 * (label_barplot$id - 0.5) / number_of_bar  # Adjust angles for labels

# Adjust hjust and angle for text labels
label_barplot$hjust <- ifelse(angle < -90, 1, 0)
label_barplot$angle <- ifelse(angle < -90, angle + 180, angle)

# Prepare a data frame for base lines
base_data <- barplot %>%
  group_by(group) %>%
  summarize(start = min(id), end = max(id) - empty_bar) %>%
  rowwise() %>%
  mutate(title = mean(c(start, end)))

# Prepare a data frame for grid (scales)
grid_data <- base_data
grid_data$end <- grid_data$end[c(nrow(grid_data), 1:nrow(grid_data)-1)] + 1
grid_data$start <- grid_data$start - 1
grid_data <- grid_data[-1,]

# Adjust the bar size, label font size, and overall image size
p <- ggplot(barplot) +      
  
  # Add the stacked bars (yellow: total_area, green: Category_4_km2)
  geom_bar(aes(x = as.factor(id), y = value, fill = observation), stat = "identity", alpha = 0.8, width = 1.5) +  # Increased bar width
  scale_fill_manual(values = c("#FDE725FF", "#2D708EFF"), labels = c("Total Reforestable Area", "Low-Cost High-Benefit Area")) +  # Updated legend labels
  
  # Add grid lines (optional)
  geom_segment(data = grid_data, aes(x = end, y = 0, xend = start, yend = 0), colour = "grey", alpha = 1, linewidth = 0.5, inherit.aes = FALSE) +
  geom_segment(data = grid_data, aes(x = end, y = 50, xend = start, yend = 50), colour = "grey", alpha = 1, linewidth = 0.5, inherit.aes = FALSE) +
  geom_segment(data = grid_data, aes(x = end, y = 100, xend = start, yend = 100), colour = "grey", alpha = 1, linewidth = 0.5, inherit.aes = FALSE) +
  geom_segment(data = grid_data, aes(x = end, y = 150, xend = start, yend = 150), colour = "grey", alpha = 1, linewidth = 0.5, inherit.aes = FALSE) +
  
  # Add text showing the value of each 50/100/150 lines
  ggplot2::annotate("text", x = rep(max(barplot$id), 4), y = c(0, 50, 100, 150), label = c("0", "50", "100", "150"), color = "grey", size = 6, angle = 0, fontface = "bold", hjust = 1) +
  
  ylim(-100, max(label_barplot$tot, na.rm = TRUE) + 50) +
  theme_minimal() +
  theme(
    legend.position = "right",
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-1, 4), "cm")
  ) +
  coord_polar() +
  
  # Add labels on top of each bar (Smaller font size for country labels)
  geom_text(data = label_barplot, aes(x = id, y = tot + 10, label = individual), color = "black", fontface = "bold", alpha = 0.8, size = 2, angle = label_barplot$angle, inherit.aes = FALSE) +  # Smaller font size for country labels
  
  # Add base line information (optional)
  geom_segment(data = base_data, aes(x = start, y = -10, xend = end, yend = -10), colour = "black", alpha = 0.8, linewidth = 0.6, inherit.aes = FALSE) +
  geom_text(data = base_data, aes(x = title, y = -30, label = group), hjust = 0.5, colour = "black", alpha = 0.8, size = 4, fontface = "bold", inherit.aes = FALSE)

# Print the plot
print(p)

























# Define the mapping for category names (numeric -> descriptive)
# # category_name <- c(
#   "1" = "Low_Both Low",
#   "2" = "Low_Biodiversity Low, Carbon High",
#   "3" = "Low_Biodiversity High, Carbon Low",
#   "4" = "Low_Both High",
#   "5" = "Medium_Both Low",
#   "6" = "Medium_Biodiversity Low, Carbon High",
#   "7" = "Medium_Biodiversity High, Carbon Low",
#   "8" = "Medium_Both High",
#   "9" = "High_Both Low",
#   "10" = "High_Biodiversity Low, Carbon High",
#   "11" = "High_Biodiversity High, Carbon Low",
#   "12" = "High_Both High"
# )











####################### The following would be in the supplementary ###############################################
######################## quantile #################################

# create 3 bins using quantile
esta_pnr_shp$quartile <- ntile(esta_pnr_shp$estb_pn, 3)

# Add quantile bins for estb_pn
esta_pnr_shp <- esta_pnr_shp %>%
  mutate(
    estb_pn_category = ntile(estb_pn, 3),
    estb_pn_category = recode(estb_pn_category, `1` = "Low", `2` = "Medium", `3` = "High")
  )

# Group by estb_pn_category and calculate medians
medians <- esta_pnr_shp %>%
  group_by(estb_pn_category) %>%
  summarise(
    median_biodiversity = median(bdvrsty, na.rm = TRUE),
    median_carbon = median(carbon, na.rm = TRUE)
  )

# Join the medians back to the original dataframe
esta_pnr_shp <- esta_pnr_shp %>%
  left_join(medians, by = "estb_pn_category")

# Categorize biodiversity and carbon based on medians
esta_pnr_shp <- esta_pnr_shp %>%
  mutate(
    biodiversity_category = if_else(bdvrsty <= median_biodiversity, "Low", "High"),
    carbon_category = if_else(carbon <= median_carbon, "Low", "High")
  )

# Create or adjust combined_category based on new sub-categories
esta_pnr_shp <- esta_pnr_shp %>%
  mutate(
    combined_category = case_when(
      biodiversity_category == "Low" & carbon_category == "Low" ~ "Both Low",
      biodiversity_category == "High" & carbon_category == "High" ~ "Both High",
      biodiversity_category == "Low" & carbon_category == "High" ~ "Biodiversity Low, Carbon High",
      biodiversity_category == "High" & carbon_category == "Low" ~ "Biodiversity High, Carbon Low",
      TRUE ~ "Mixed Categories"  # Covers remaining combinations
    )
  )

# Drop intermediate columns if not needed
esta_pnr_shp <- select(esta_pnr_shp, -median_biodiversity, -median_carbon)

# Print the updated data to verify
print(head(esta_pnr_shp))

# Calculate the counts of each category and subcategory
category_counts <- esta_pnr_shp %>%
  group_by(estb_pn_category, biodiversity_category, carbon_category, combined_category) %>%
  summarise(count = n(), .groups = 'drop')  # Drop the automatic grouping

# Calculate total counts for each estb_pn_category
total_counts <- category_counts %>%
  group_by(estb_pn_category) %>%
  summarise(total = sum(count))

# Join the total back to the original counts dataframe
category_counts <- category_counts %>%
  left_join(total_counts, by = "estb_pn_category")

# Calculate percentages
category_counts <- category_counts %>%
  mutate(percentage = (count / total) * 100)

# Print the results to check
print(category_counts)

############################################### establishment cost map ############################
# Select only the relevant columns to save
estb_pnr_qsubset <- esta_pnr_shp %>%
select(estb_pn_category, combined_category, geometry)

# Update the dataframe with a new numeric column for category combinations
estb_pnr_qsubset <- estb_pnr_qsubset %>%
  mutate(
    category_numeric = case_when(
      estb_pn_category == "Low" & combined_category == "Both Low" ~ 1,
      estb_pn_category == "Low" & combined_category == "Carbon Low, Biodiversity High" ~ 2,
      estb_pn_category == "Low" & combined_category == "Carbon High, Biodiversity Low" ~ 3,
      estb_pn_category == "Low" & combined_category == "Both High" ~ 4,
      estb_pn_category == "Medium" & combined_category == "Both Low" ~ 5,
      estb_pn_category == "Medium" & combined_category == "Carbon Low, Biodiversity High" ~ 6,
      estb_pn_category == "Medium" & combined_category == "Carbon High, Biodiversity Low" ~ 7,
      estb_pn_category == "Medium" & combined_category == "Both High" ~ 8,
      estb_pn_category == "High" & combined_category == "Both Low" ~ 9,
      estb_pn_category == "High" & combined_category == "Carbon Low, Biodiversity High" ~ 10,
      estb_pn_category == "High" & combined_category == "Carbon High, Biodiversity Low" ~ 11,
      estb_pn_category == "High" & combined_category == "Both High" ~ 12,
      TRUE ~ NA_real_  # Handle any cases that do not match the conditions above
    )
  )

# Check the changes
print(head(estb_pnr_qsubset))

# Convert your sf object to a Spatial object if it's not already
if (!inherits(estb_pnr_qsubset, "sf") && "geometry" %in% names(estb_pnr_qsubset)) {
  estb_pnr_qsubset <- st_as_sf(estb_pnr_qsubset, sf_column_name = "geometry")
}
points_sp <- as(estb_pnr_qsubset, "Spatial")

# Create a raster template
# Define the extent and resolution according to your data's need
raster_template <- raster(extent(points_sp), res = 0.008)  
crs(raster_template) <- crs(points_sp)

# Assuming the 'category_numeric' field holds your category values from 1-12
# Rasterize the points
# Using the 'category_numeric' to fill the raster
rasterized_data <- rasterize(points_sp, raster_template, field = points_sp$category_numeric, fun = mean)

# View the raster
plot(rasterized_data)

# Save the raster to a TIFF file
writeRaster(rasterized_data, filename = "R:/Natural_regneration_BF_datasets/Chapter_2_analysis/Shapefile/emap.tif", format = "GTiff", overwrite = TRUE)
