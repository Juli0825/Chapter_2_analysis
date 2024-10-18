library(tidyverse)
library(viridis)

setwd("R:/Natural_regneration_BF_datasets/Chapter_2_analysis")

data <- read.csv("logx_1000_1.csv", header = TRUE)

# Transform data in a tidy format (long format)
#Can also change the order that stacked bars appear here
data = data %>% gather(key = "observation", value="value", c("value1", "value2"), factor_key = TRUE)

# Ensure 'group' is a factor
data$group <- factor(data$group)

# Ensure the 'individual' column is treated as a factor and keeps the order from Excel (to make the bar shows in height order)
data$individual <- factor(data$individual, levels = unique(data$individual))

# Set a number of 'empty bar' to add at the end of each group
empty_bar=2
nObsType=nlevels(as.factor(data$observation))
to_add = data.frame(matrix(NA, empty_bar*nlevels(data$group)*nObsType, ncol(data)) )
colnames(to_add) = colnames(data)
to_add$group=rep(levels(data$group), each=empty_bar*nObsType )
data=rbind(data, to_add)
data=data %>% arrange(group, individual)
# data <- data %>%
#   group_by(group) %>%
#   arrange(group, desc(value))
data$id=rep( seq(1, nrow(data)/nObsType) , each=nObsType)

# Get the name and the y position of each label
label_data= data %>% group_by(id, individual) %>% summarize(tot=sum(value))

number_of_bar=nrow(label_data)
angle= 90 - 360 * (label_data$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
label_data$hjust<-ifelse( angle < -90, 1, 0)
label_data$angle<-ifelse(angle < -90, angle+180, angle)

# ####
# # Get the name and the y position of each label after sorting
# label_data = data %>%
#   group_by(id, individual) %>%
#   summarize(tot = sum(value, na.rm = TRUE))
# 
# # Calculate the number of bars (including empty bars for space between groups)
# number_of_bar = nrow(label_data)
# 
# # Recalculate angles for the labels after reordering
# angle = 90 - 360 * (label_data$id - 0.5) / number_of_bar  # Same calculation, but after reordering
# 
# # Adjust label horizontal justification (hjust) based on the recalculated angles
# label_data$hjust <- ifelse(angle < -90, 1, 0)  # Flip hjust for angles < -90 (left side of the plot)
# 
# # Flip angles for readability on the left side of the plot
# label_data$angle <- ifelse(angle < -90, angle + 180, angle)
#####

# Assign colour to bars
total_aera <- "#008000"
C4 <- "#a14bef"

# Make the plot
p = ggplot(data) +      
  
  # Add the stacked bar
  geom_bar(aes(x=as.factor(id), y=value, fill=observation), stat="identity") +
  
  
  scale_fill_manual(values=c(total_aera, C4)) +
  
  ylim(-15, 50) + #ylim(, 178) is the limit that no row being removed)
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-1,4), "cm") 
  ) +
  coord_polar() +
  
  # Add labels on top of each bar
  geom_text(data=label_data, aes(x=id, y=tot+2, label=individual, hjust=hjust), color="black", fontface="bold",
            alpha=0.7, size=3, angle= label_data$angle, inherit.aes = FALSE ) 

# SEE THE PLOT
p

jpeg(filename = 'test_logx_1000_1.jpg', height=18, width=18, units=c("in"), res = 1000)
p
dev.off()

# if there's Warning messages:
#   1: Removed 24 rows containing missing values or values outside the scale range (`geom_bar()`). 
# 2: Removed 12 rows containing missing values or values outside the scale range (`geom_text()`). 
# those are the empty bars



##### for single value in barplot #####
data <- read.csv("more_than_1000_log.csv", header = TRUE)

# Transform data in a tidy format (long format)
#Can also change the order that stacked bars appear here
data = data %>% gather(key = "observation", value="value", c("value1", "value2"), factor_key = TRUE)

# Ensure 'group' is a factor
data$group <- factor(data$group)

# Ensure the 'individual' column is treated as a factor and keeps the order from Excel (to make the bar shows in height order)
data$individual <- factor(data$individual, levels = unique(data$individual))

# Set a number of 'empty bar' to add at the end of each group
empty_bar=2
nObsType=nlevels(as.factor(data$observation))
to_add = data.frame(matrix(NA, empty_bar*nlevels(data$group)*nObsType, ncol(data)) )
colnames(to_add) = colnames(data)
to_add$group=rep(levels(data$group), each=empty_bar*nObsType )
data=rbind(data, to_add)
data=data %>% arrange(group, individual)
# data <- data %>%
#   group_by(group) %>%
#   arrange(group, desc(value))
data$id=rep( seq(1, nrow(data)/nObsType) , each=nObsType)

# Get the name and the y position of each label
label_data= data %>% group_by(id, individual) %>% summarize(tot=sum(value))

number_of_bar=nrow(label_data)
angle= 90 - 360 * (label_data$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
label_data$hjust<-ifelse( angle < -90, 1, 0)
label_data$angle<-ifelse(angle < -90, angle+180, angle)

# ####
# # Get the name and the y position of each label after sorting
# label_data = data %>%
#   group_by(id, individual) %>%
#   summarize(tot = sum(value, na.rm = TRUE))
# 
# # Calculate the number of bars (including empty bars for space between groups)
# number_of_bar = nrow(label_data)
# 
# # Recalculate angles for the labels after reordering
# angle = 90 - 360 * (label_data$id - 0.5) / number_of_bar  # Same calculation, but after reordering
# 
# # Adjust label horizontal justification (hjust) based on the recalculated angles
# label_data$hjust <- ifelse(angle < -90, 1, 0)  # Flip hjust for angles < -90 (left side of the plot)
# 
# # Flip angles for readability on the left side of the plot
# label_data$angle <- ifelse(angle < -90, angle + 180, angle)
#####

# Assign colour to bars
total_aera <- "#008000"
C4 <- "#a14bef"

# Make the plot
p = ggplot(data) +      
  
  # Add the stacked bar
  geom_bar(aes(x=as.factor(id), y=value, fill=observation), stat="identity") +
  
  
  scale_fill_manual(values=c(total_aera, C4)) +
  
  ylim(-15, 50) + #ylim(, 178) is the limit that no row being removed)
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-1,4), "cm") 
  ) +
  coord_polar() +
  
  # Add labels on top of each bar
  geom_text(data=label_data, aes(x=id, y=tot+2, label=individual, hjust=hjust), color="black", fontface="bold",
            alpha=0.7, size=4, angle= label_data$angle, inherit.aes = FALSE ) 

# SEE THE PLOT
p

jpeg(filename = 'test_more_than_1000_log.jpg', height=18, width=18, units=c("in"), res = 1000)
p
dev.off()



##### adding coordinates into excel ####
install.packages("tidygeocoder")

library(tidygeocoder)
library(readr)
library(dplyr)

df <- read_csv("circular_total_C4.csv")

# Geocoding using OpenStreetMap (OSM)
df_with_coords <- df %>%
  geocode(country = individual, method = 'osm', lat = latitude, long = longitude)

head(df_with_coords)

# Writing back to CSV
write_csv(df_with_coords, "geocoded_countries.csv")
