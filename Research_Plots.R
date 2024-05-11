library(tidyverse)
library(dplyr)
library(ggplot2)
# install.packages("GGally")
# library(GGally)
# install.packages("TSstudio")
library(TSstudio)

#install.packages("ggmap")
#library(ggmap)
#install.packages("ggpubr")
library(ggpubr)

#install.packages('leaflet')
library(leaflet)

#install.packages('astsa')
library(astsa)

#install.packages("maps")

library(maps)

setwd("C:/Users/wccha/Documents/Rutgers/Research/Plots")

# Set the directory where your CSV files are located
folder_path <- "C:/Users/wccha/Documents/Rutgers/Research/Data/"

# Get a list of CSV files in the folder
csv_files <- list.files(folder_path, pattern = ".csv")

# Initialize an empty list to store dataframes
df_list <- list()


# Subset Data
for (file in csv_files) {
  file_path <- paste(folder_path, file, sep = "")
  check <- strtoi(strsplit(file, split = "[.]")[[1]][3])
  if (check > 20000701 && check < 20000703){
    #print(check)
    df <- read_csv(file_path) # Use read.csv() if you prefer
    df_list[[file]] <- df
  }
}

# Combine all dataframes into one dataframe
data <- do.call(rbind, df_list)

#View(data)
#---------------------------------------------
# Function to calculate Ugeo and Vgeo components from wind direction and speed
calculate_UV_components <- function(Dirgeo, Spd) {
  RperD <- pi / 180  # Conversion factor from degrees to radians
  
  # Calculate Ugeo and Vgeo components
  Ugeo <- -Spd * sin(Dirgeo * RperD)
  Vgeo <- -Spd * cos(Dirgeo * RperD)
  
  return(list(Ugeo = -Ugeo, Vgeo = -Vgeo))
}

components <- calculate_UV_components(data$wdir_vec_mean, data$wspd_arith_mean)
data$wdir_easterly <- components$Ugeo
data$wdir_northerly <- components$Vgeo

# Group rows by Hour
# data$hour <- as.numeric(format(data$time, format = "%m%d%Y%H"))

# data_hour <- data %>% group_by(hour, lat, lon) %>% summarise(wdir_northerly_avg = mean(wdir_northerly))


classify_region <- function(lat, lon) {
  
  if (lat == 38.202 & lon == -99.316) {
    return("Larned")
  } else if (lat == 38.201 & lon == -95.597) {
    return("LeRoy")
  } else if (lat == 37.953 & lon == -98.329) {
    return("Plevna")
  } else if (lat == 38.114 & lon == -97.513) {
    return("Halstead")
  } else if (lat == 37.842 & lon == -97.02) {
    return("Towanda")
  } else {
    return("Other")
  }
}

data[, 'location'] <- "NA"

for (i in 1:length(data$lat)){
  print(i)
  data[i,]$location <- classify_region(data[i,]$lat, data[i,]$lon)
}

# Creating First Differences Columns
data$first_differences <- c(0, diff(data$wdir_vec_mean))

data$first_differences_easterly <- c(0, diff(data$wdir_easterly))
data$first_differences_northerly <- c(0, diff(data$wdir_northerly))
################3###############################################################
# PLOTS
#-------------------------------------------------------------------------------
# Northerly First Differences Grouped By Hour

# Select Columns
wind.data <- select(data, c("time", "wdir_easterly", "wdir_northerly", "first_differences_easterly", "first_differences_northerly", "location"))

# Add column for Hour
wind.data$hour <- as.numeric(format(wind.data$time, format = "%H"))

# Create location based dfs
wind.data.Larned <- wind.data %>% filter(location == "Larned")
wind.data.LeRoy <- wind.data %>% filter(location == "LeRoy")
wind.data.Plevna <- wind.data %>% filter(location == "Plevna")
wind.data.Halstead <- wind.data %>% filter(location == "Halstead")
wind.data.Towanda <- wind.data %>% filter(location == "Towanda")

# Group By Hour
Larned <- ggplot(wind.data.Larned, aes(group = hour, x = time, y = first_differences_northerly)) +
  geom_boxplot() +
  labs(x = "Time", y = "Northerly (m/s)") + 
  ggtitle("Northerly First Differences Grouped By Hour") +
  theme(plot.title = element_text(hjust = 0.5))
LeRoy <- ggplot(wind.data.LeRoy, aes(group = hour, x = time, y = first_differences_northerly)) +
  geom_boxplot() +
  labs(x = "Time", y = "Northerly (m/s)")
Plevna <- ggplot(wind.data.Plevna, aes(group = hour, x = time, y = first_differences_northerly)) +
  geom_boxplot() +
  labs(x = "Time", y = "Northerly (m/s)")
Halstead <- ggplot(wind.data.Halstead, aes(group = hour, x = time, y = first_differences_northerly)) +
  geom_boxplot() +
  labs(x = "Time", y = "Northerly (m/s)")
Towanda <- ggplot(wind.data.Towanda, aes(group = hour, x = time, y = first_differences_northerly)) +
  geom_boxplot() +
  labs(x = "Time", y = "Northerly (m/s)")

ggarrange(Larned, Plevna, Halstead, Towanda, LeRoy,
          labels = c("Larned", "Plevna", "Halstead", "Towanda", "LeRoy"),
          ncol = 1, nrow = 5)
#-------------------------------------------------------------------------------
# Northerly First Differences

# Select Columns
wind.data <- select(data, c("time", "wdir_easterly", "wdir_northerly", "first_differences_easterly", "first_differences_northerly", "location"))

# Create location based dfs
wind.data.Larned <- wind.data %>% filter(location == "Larned")
wind.data.LeRoy <- wind.data %>% filter(location == "LeRoy")
wind.data.Plevna <- wind.data %>% filter(location == "Plevna")
wind.data.Halstead <- wind.data %>% filter(location == "Halstead")
wind.data.Towanda <- wind.data %>% filter(location == "Towanda")

Larned <- ggplot(wind.data.Larned, aes(time, first_differences_northerly)) +
  geom_line() +
  labs(x = "Time", y = "Northerly (m/s)") + 
  ggtitle("Northerly First Differences") +
  theme(plot.title = element_text(hjust = 0.5))
LeRoy <- ggplot(wind.data.LeRoy, aes(time, first_differences_northerly)) +
  geom_line() +
  labs(x = "Time", y = "Northerly (m/s)")
Plevna <- ggplot(wind.data.Plevna, aes(time, first_differences_northerly)) +
  geom_line() +
  labs(x = "Time", y = "Northerly (m/s)")
Halstead <- ggplot(wind.data.Halstead, aes(time, first_differences_northerly)) +
  geom_line() +
  labs(x = "Time", y = "Northerly (m/s)")
Towanda <- ggplot(wind.data.Towanda, aes(time, first_differences_northerly)) +
  geom_line() +
  labs(x = "Time", y = "Northerly (m/s)")

ggarrange(Larned, Plevna, Halstead, Towanda, LeRoy,
          labels = c("Larned", "Plevna", "Halstead", "Towanda", "LeRoy"),
          ncol = 1, nrow = 5)
#-------------------------------------------------------------------------------
# Northerly Regular Grouped By Hour

# Select Columns
wind.data <- select(data, c("time", "wdir_easterly", "wdir_northerly", "first_differences_easterly", "first_differences_northerly", "location"))

# Add column for Hour
wind.data$hour <- as.numeric(format(wind.data$time, format = "%H"))

# Create location based dfs
wind.data.Larned <- wind.data %>% filter(location == "Larned")
wind.data.LeRoy <- wind.data %>% filter(location == "LeRoy")
wind.data.Plevna <- wind.data %>% filter(location == "Plevna")
wind.data.Halstead <- wind.data %>% filter(location == "Halstead")
wind.data.Towanda <- wind.data %>% filter(location == "Towanda")

# Grouped by hour
Larned <- ggplot(wind.data.Larned, aes(group = hour, y = wdir_northerly)) +
  geom_boxplot()+
  labs(x = "Time", y = "Northerly (m/s)") + 
  ggtitle("Northerly Grouped By Hour") +
  theme(plot.title = element_text(hjust = 0.5))
LeRoy <- ggplot(wind.data.LeRoy, aes(group = hour, y =  wdir_northerly)) +
  geom_boxplot()+
  labs(x = "Time", y = "Northerly (m/s)")
Plevna <- ggplot(wind.data.Plevna, aes(group = hour, y =  wdir_northerly)) +
  geom_boxplot()+
  labs(x = "Time", y = "Northerly (m/s)")
Halstead <- ggplot(wind.data.Halstead, aes(group = hour, y = wdir_northerly)) +
  geom_boxplot()+
  labs(x = "Time", y = "Northerly (m/s)")
Towanda <- ggplot(wind.data.Towanda, aes(group = hour, y = wdir_northerly)) +
  geom_boxplot()+
  labs(x = "Time", y = "Northerly (m/s)")

ggarrange(Larned, Plevna, Halstead, Towanda, LeRoy,
          labels = c("Larned", "Plevna", "Halstead", "Towanda", "LeRoy"),
          ncol = 1, nrow = 5)
#-------------------------------------------------------------------------------
# Northerly Regular

# Select Columns
wind.data <- select(data, c("time", "wdir_easterly", "wdir_northerly", "first_differences_easterly", "first_differences_northerly", "location"))

# Create location based dfs
wind.data.Larned <- wind.data %>% filter(location == "Larned")
wind.data.LeRoy <- wind.data %>% filter(location == "LeRoy")
wind.data.Plevna <- wind.data %>% filter(location == "Plevna")
wind.data.Halstead <- wind.data %>% filter(location == "Halstead")
wind.data.Towanda <- wind.data %>% filter(location == "Towanda")

Larned <- ggplot(wind.data.Larned, aes(time, wdir_northerly)) +
  geom_line() +
  labs(x = "Time", y = "Northerly (m/s)") + 
  ggtitle("Northerly") +
  theme(plot.title = element_text(hjust = 0.5))
LeRoy <- ggplot(wind.data.LeRoy, aes(time, wdir_northerly)) +
  geom_line()+
  labs(x = "Time", y = "Northerly (m/s)")
Plevna <- ggplot(wind.data.Plevna, aes(time, wdir_northerly)) +
  geom_line()+
  labs(x = "Time", y = "Northerly (m/s)")
Halstead <- ggplot(wind.data.Halstead, aes(time, wdir_northerly)) +
  geom_line()+
  labs(x = "Time", y = "Northerly (m/s)")
Towanda <- ggplot(wind.data.Towanda, aes(time, wdir_northerly)) +
  geom_line()+
  labs(x = "Time", y = "Northerly (m/s)")

ggarrange(Larned, Plevna, Halstead, Towanda, LeRoy,
          labels = c("Larned", "Plevna", "Halstead", "Towanda", "LeRoy"),
          ncol = 1, nrow = 5)
#-------------------------------------------------------------------------------
# Northerly Zoomed
# Select Columns
wind.data <- select(data, c("time", "wdir_easterly", "wdir_northerly", "first_differences_easterly", "first_differences_northerly", "location"))

wind.data <- wind.data %>% filter(time)
# Create location based dfs
wind.data.Larned <- wind.data %>% filter(location == "Larned")
wind.data.LeRoy <- wind.data %>% filter(location == "LeRoy")
wind.data.Plevna <- wind.data %>% filter(location == "Plevna")
wind.data.Halstead <- wind.data %>% filter(location == "Halstead")
wind.data.Towanda <- wind.data %>% filter(location == "Towanda")

Larned <- ggplot(wind.data.Larned, aes(time, wdir_northerly)) +
  geom_line() +
  labs(x = "Time", y = "Northerly (m/s)") + 
  ggtitle("Northerly") +
  theme(plot.title = element_text(hjust = 0.5))
LeRoy <- ggplot(wind.data.LeRoy, aes(time, wdir_northerly)) +
  geom_line()+
  labs(x = "Time", y = "Northerly (m/s)")
Plevna <- ggplot(wind.data.Plevna, aes(time, wdir_northerly)) +
  geom_line()+
  labs(x = "Time", y = "Northerly (m/s)")
Halstead <- ggplot(wind.data.Halstead, aes(time, wdir_northerly)) +
  geom_line()+
  labs(x = "Time", y = "Northerly (m/s)")
Towanda <- ggplot(wind.data.Towanda, aes(time, wdir_northerly)) +
  geom_line()+
  labs(x = "Time", y = "Northerly (m/s)")

ggarrange(Larned, Plevna, Halstead, Towanda, LeRoy,
          labels = c("Larned", "Plevna", "Halstead", "Towanda", "LeRoy"),
          ncol = 1, nrow = 5)
#-------------------------------------------------------------------------------
# Easterly First Differences Grouped By Hour

# Select Columns
wind.data <- select(data, c("time", "wdir_easterly", "wdir_northerly", "first_differences_easterly", "first_differences_northerly", "location"))

# Add column for Hour
wind.data$hour <- as.numeric(format(wind.data$time, format = "%H"))

# Create location based dfs
wind.data.Larned <- wind.data %>% filter(location == "Larned")
wind.data.LeRoy <- wind.data %>% filter(location == "LeRoy")
wind.data.Plevna <- wind.data %>% filter(location == "Plevna")
wind.data.Halstead <- wind.data %>% filter(location == "Halstead")
wind.data.Towanda <- wind.data %>% filter(location == "Towanda")

# Group By Hour
Larned <- ggplot(wind.data.Larned, aes(group = hour, x = time, y = first_differences_easterly)) +
  geom_boxplot() +
  labs(x = "Time", y = "easterly (m/s)")  + 
  ggtitle("Easterly First Differences Grouped By Hour") +
  theme(plot.title = element_text(hjust = 0.5))
LeRoy <- ggplot(wind.data.LeRoy, aes(group = hour, x = time, y = first_differences_easterly)) +
  geom_boxplot() +
  labs(x = "Time", y = "easterly (m/s)")
Plevna <- ggplot(wind.data.Plevna, aes(group = hour, x = time, y = first_differences_easterly)) +
  geom_boxplot() +
  labs(x = "Time", y = "easterly (m/s)")
Halstead <- ggplot(wind.data.Halstead, aes(group = hour, x = time, y = first_differences_easterly)) +
  geom_boxplot() +
  labs(x = "Time", y = "easterly (m/s)")
Towanda <- ggplot(wind.data.Towanda, aes(group = hour, x = time, y = first_differences_easterly)) +
  geom_boxplot() +
  labs(x = "Time", y = "easterly (m/s)")

ggarrange(Larned, Plevna, Halstead, Towanda, LeRoy,
          labels = c("Larned", "Plevna", "Halstead", "Towanda", "LeRoy"),
          ncol = 1, nrow = 5)
#-------------------------------------------------------------------------------
# Easterly First Differences 

# Select Columns
wind.data <- select(data, c("time", "wdir_easterly", "wdir_northerly", "first_differences_easterly", "first_differences_northerly", "location"))

# Create location based dfs
wind.data.Larned <- wind.data %>% filter(location == "Larned")
wind.data.LeRoy <- wind.data %>% filter(location == "LeRoy")
wind.data.Plevna <- wind.data %>% filter(location == "Plevna")
wind.data.Halstead <- wind.data %>% filter(location == "Halstead")
wind.data.Towanda <- wind.data %>% filter(location == "Towanda")

Larned <- ggplot(wind.data.Larned, aes(time, first_differences_easterly)) +
  geom_line() +
  labs(x = "Time", y = "easterly (m/s)")  + 
  ggtitle("Easterly First Differences") +
  theme(plot.title = element_text(hjust = 0.5))
LeRoy <- ggplot(wind.data.LeRoy, aes(time, first_differences_easterly)) +
  geom_line() +
  labs(x = "Time", y = "easterly (m/s)")
Plevna <- ggplot(wind.data.Plevna, aes(time, first_differences_easterly)) +
  geom_line() +
  labs(x = "Time", y = "easterly (m/s)")
Halstead <- ggplot(wind.data.Halstead, aes(time, first_differences_easterly)) +
  geom_line() +
  labs(x = "Time", y = "easterly (m/s)")
Towanda <- ggplot(wind.data.Towanda, aes(time, first_differences_easterly)) +
  geom_line() +
  labs(x = "Time", y = "easterly (m/s)")

ggarrange(Larned, Plevna, Halstead, Towanda, LeRoy,
          labels = c("Larned", "Plevna", "Halstead", "Towanda", "LeRoy"),
          ncol = 1, nrow = 5)
#-------------------------------------------------------------------------------
# Easterly Regular Grouped By Hour

# Select Columns
wind.data <- select(data, c("time", "wdir_easterly", "wdir_northerly", "first_differences_easterly", "first_differences_northerly", "location"))

# Add column for Hour
wind.data$hour <- as.numeric(format(wind.data$time, format = "%H"))

# Create location based dfs
wind.data.Larned <- wind.data %>% filter(location == "Larned")
wind.data.LeRoy <- wind.data %>% filter(location == "LeRoy")
wind.data.Plevna <- wind.data %>% filter(location == "Plevna")
wind.data.Halstead <- wind.data %>% filter(location == "Halstead")
wind.data.Towanda <- wind.data %>% filter(location == "Towanda")

# Grouped by hour
Larned <- ggplot(wind.data.Larned, aes(group = hour, y = wdir_easterly)) +
  geom_boxplot()+
  labs(x = "Time", y = "easterly (m/s)")  + 
  ggtitle("Easterly Grouped By Hour") +
  theme(plot.title = element_text(hjust = 0.5))
LeRoy <- ggplot(wind.data.LeRoy, aes(group = hour, y =  wdir_easterly)) +
  geom_boxplot()+
  labs(x = "Time", y = "easterly (m/s)")
Plevna <- ggplot(wind.data.Plevna, aes(group = hour, y =  wdir_easterly)) +
  geom_boxplot()+
  labs(x = "Time", y = "easterly (m/s)")
Halstead <- ggplot(wind.data.Halstead, aes(group = hour, y = wdir_easterly)) +
  geom_boxplot()+
  labs(x = "Time", y = "easterly (m/s)")
Towanda <- ggplot(wind.data.Towanda, aes(group = hour, y = wdir_easterly)) +
  geom_boxplot()+
  labs(x = "Time", y = "easterly (m/s)")

ggarrange(Larned, Plevna, Halstead, Towanda, LeRoy,
          labels = c("Larned", "Plevna", "Halstead", "Towanda", "LeRoy"),
          ncol = 1, nrow = 5)
#-------------------------------------------------------------------------------
# Easterly Regular

# Select Columns
wind.data <- select(data, c("time", "wdir_easterly", "wdir_northerly", "first_differences_easterly", "first_differences_northerly", "location"))

# Create location based dfs
wind.data.Larned <- wind.data %>% filter(location == "Larned")
wind.data.LeRoy <- wind.data %>% filter(location == "LeRoy")
wind.data.Plevna <- wind.data %>% filter(location == "Plevna")
wind.data.Halstead <- wind.data %>% filter(location == "Halstead")
wind.data.Towanda <- wind.data %>% filter(location == "Towanda")

Larned <- ggplot(wind.data.Larned, aes(time, wdir_easterly)) +
  geom_line() +
  labs(x = "Time", y = "easterly (m/s)")  + 
  ggtitle("Easterly") +
  theme(plot.title = element_text(hjust = 0.5))
LeRoy <- ggplot(wind.data.LeRoy, aes(time, wdir_easterly)) +
  geom_line()+
  labs(x = "Time", y = "easterly (m/s)")
Plevna <- ggplot(wind.data.Plevna, aes(time, wdir_easterly)) +
  geom_line()+
  labs(x = "Time", y = "easterly (m/s)")
Halstead <- ggplot(wind.data.Halstead, aes(time, wdir_easterly)) +
  geom_line()+
  labs(x = "Time", y = "easterly (m/s)")
Towanda <- ggplot(wind.data.Towanda, aes(time, wdir_easterly)) +
  geom_line()+
  labs(x = "Time", y = "easterly (m/s)")

ggarrange(Larned, Plevna, Halstead, Towanda, LeRoy,
          labels = c("Larned", "Plevna", "Halstead", "Towanda", "LeRoy"),
          ncol = 1, nrow = 5)
#-------------------------------------------------------------------------------
# Scatter Plot

# Select Columns
wind.data <- select(data, c("time", "wdir_easterly", "wdir_northerly", "first_differences_easterly", "first_differences_northerly", "location"))

# Add column for Hour
wind.data$hour <- as.numeric(format(wind.data$time, format = "%H"))

# Create hour based dfs
#wind.data.0 <- wind.data %>% filter(hour == 0)
#wind.data.1 <- wind.data %>% filter(hour == 1)
#wind.data.2 <- wind.data %>% filter(hour == 2)
#wind.data.3 <- wind.data %>% filter(hour == 3)
#wind.data.4 <- wind.data %>% filter(hour == 4)
#wind.data.5 <- wind.data %>% filter(hour == 5)
wind.data.6 <- wind.data %>% filter(hour == 6)
wind.data.7 <- wind.data %>% filter(hour == 7)
wind.data.8 <- wind.data %>% filter(hour == 8)
wind.data.9 <- wind.data %>% filter(hour == 9)
wind.data.10 <- wind.data %>% filter(hour == 10)
wind.data.11 <- wind.data %>% filter(hour == 11)
wind.data.12 <- wind.data %>% filter(hour == 12)
wind.data.13 <- wind.data %>% filter(hour == 13)
wind.data.14 <- wind.data %>% filter(hour == 14)
wind.data.15 <- wind.data %>% filter(hour == 15)
wind.data.16 <- wind.data %>% filter(hour == 16)
wind.data.17 <- wind.data %>% filter(hour == 17)
wind.data.18 <- wind.data %>% filter(hour == 18)

hour6 <- ggplot(wind.data.6, aes(x = wdir_easterly, y = wdir_northerly, color = location)) + 
  geom_point(show.legend = FALSE) + 
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())
hour7 <- ggplot(wind.data.7, aes(x = wdir_easterly, y = wdir_northerly, color = location)) + 
  geom_point(show.legend = FALSE) + 
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())
hour8 <- ggplot(wind.data.8, aes(x = wdir_easterly, y = wdir_northerly, color = location)) + 
  geom_point(show.legend = FALSE) + 
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())
hour9 <- ggplot(wind.data.9, aes(x = wdir_easterly, y = wdir_northerly, color = location)) + 
  geom_point(show.legend = FALSE) + 
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())
hour10 <- ggplot(wind.data.10, aes(x = wdir_easterly, y = wdir_northerly, color = location)) + 
  geom_point(show.legend = FALSE) + 
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())
hour11 <- ggplot(wind.data.11, aes(x = wdir_easterly, y = wdir_northerly, color = location)) + 
  geom_point(show.legend = FALSE) + 
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())
hour12 <- ggplot(wind.data.12, aes(x = wdir_easterly, y = wdir_northerly, color = location)) + 
  geom_point(show.legend = FALSE) + 
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())
hour13 <- ggplot(wind.data.13, aes(x = wdir_easterly, y = wdir_northerly, color = location)) + 
  geom_point(show.legend = FALSE) + 
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())
hour14 <- ggplot(wind.data.14, aes(x = wdir_easterly, y = wdir_northerly, color = location)) + 
  geom_point(show.legend = FALSE) + 
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())
hour15 <- ggplot(wind.data.15, aes(x = wdir_easterly, y = wdir_northerly, color = location)) + 
  geom_point(show.legend = FALSE) + 
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())
hour16 <- ggplot(wind.data.16, aes(x = wdir_easterly, y = wdir_northerly, color = location)) + 
  geom_point(show.legend = FALSE) + 
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())
hour17 <- ggplot(wind.data.17, aes(x = wdir_easterly, y = wdir_northerly, color = location)) + 
  geom_point()


plot <- ggarrange(hour6, hour7, hour8, hour9, hour10, hour11, hour12, hour13, hour14, hour15, hour16, hour17, #hour12,
          labels = c("hour6", "hour7", "hour8", "hour9", "hour10", "hour11", "hour12", "hour13", "hour14", "hour15", "hour16", "hour17"),
          ncol = 4, nrow = 3)
annotate_figure(plot, top = text_grob("Northerly vs. Easterly (m/s)", 
                                      face = "bold", size = 14))
#-------------------------------------------------------------------------------
# Scatter Plot First Differences

# Select Columns
wind.data <- select(data, c("time", "wdir_easterly", "wdir_northerly", "first_differences_easterly", "first_differences_northerly", "location"))

# Add column for Hour
wind.data$hour <- as.numeric(format(wind.data$time, format = "%H"))

# Create hour based dfs
#wind.data.0 <- wind.data %>% filter(hour == 0)
#wind.data.1 <- wind.data %>% filter(hour == 1)
#wind.data.2 <- wind.data %>% filter(hour == 2)
#wind.data.3 <- wind.data %>% filter(hour == 3)
#wind.data.4 <- wind.data %>% filter(hour == 4)
#wind.data.5 <- wind.data %>% filter(hour == 5)
wind.data.6 <- wind.data %>% filter(hour == 6)
wind.data.7 <- wind.data %>% filter(hour == 7)
wind.data.8 <- wind.data %>% filter(hour == 8)
wind.data.9 <- wind.data %>% filter(hour == 9)
wind.data.10 <- wind.data %>% filter(hour == 10)
wind.data.11 <- wind.data %>% filter(hour == 11)
wind.data.12 <- wind.data %>% filter(hour == 12)
wind.data.13 <- wind.data %>% filter(hour == 13)
wind.data.14 <- wind.data %>% filter(hour == 14)
wind.data.15 <- wind.data %>% filter(hour == 15)
wind.data.16 <- wind.data %>% filter(hour == 16)
wind.data.17 <- wind.data %>% filter(hour == 17)
wind.data.18 <- wind.data %>% filter(hour == 18)

hour6 <- ggplot(wind.data.6, aes(x = first_differences_easterly, y = first_differences_northerly, color = location)) + 
  geom_point(show.legend = FALSE) + 
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())
hour7 <- ggplot(wind.data.7, aes(x = first_differences_easterly, y = first_differences_northerly, color = location)) + 
  geom_point(show.legend = FALSE) + 
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())
hour8 <- ggplot(wind.data.8, aes(x = first_differences_easterly, y = first_differences_northerly, color = location)) + 
  geom_point(show.legend = FALSE) + 
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())
hour9 <- ggplot(wind.data.9, aes(x = first_differences_easterly, y = first_differences_northerly, color = location)) + 
  geom_point(show.legend = FALSE) + 
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())
hour10 <- ggplot(wind.data.10, aes(x = first_differences_easterly, y = first_differences_northerly, color = location)) + 
  geom_point(show.legend = FALSE) + 
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())
hour11 <- ggplot(wind.data.11, aes(x = first_differences_easterly, y = first_differences_northerly, color = location)) + 
  geom_point(show.legend = FALSE) + 
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())
hour12 <- ggplot(wind.data.12, aes(x = first_differences_easterly, y = first_differences_northerly, color = location)) + 
  geom_point(show.legend = FALSE) + 
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())
hour13 <- ggplot(wind.data.13, aes(x = first_differences_easterly, y = first_differences_northerly, color = location)) + 
  geom_point(show.legend = FALSE) + 
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())
hour14 <- ggplot(wind.data.14, aes(x = first_differences_easterly, y = first_differences_northerly, color = location)) + 
  geom_point(show.legend = FALSE) + 
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())
hour15 <- ggplot(wind.data.15, aes(x = first_differences_easterly, y = first_differences_northerly, color = location)) + 
  geom_point(show.legend = FALSE) + 
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())
hour16 <- ggplot(wind.data.16, aes(x = first_differences_easterly, y = first_differences_northerly, color = location)) + 
  geom_point(show.legend = FALSE) + 
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())
hour17 <- ggplot(wind.data.17, aes(x = first_differences_easterly, y = first_differences_northerly, color = location)) + 
  geom_point()


plot <- ggarrange(hour6, hour7, hour8, hour9, hour10, hour11, hour12, hour13, hour14, hour15, hour16, hour17, #hour12,
                  labels = c("hour6", "hour7", "hour8", "hour9", "hour10", "hour11", "hour12", "hour13", "hour14", "hour15", "hour16", "hour17"),
                  ncol = 4, nrow = 3)
annotate_figure(plot, top = text_grob("Northerly vs. Easterly First Differences (m/s)", 
                                      face = "bold", size = 14))
#-------------------------------------------------------------------------------
# Scatterplot Grouped by Hour
#-------------------------------------------------------------------------------
#K Means Clustering
#-------------------------------------------------------------------------------
# Median Wind vec for 1 min every 10 min northerly HAVE A GRAPH THAT AGGREGATES WIND SPEED OVER 10 MIN???

# Select relevant columns
wind.data <- select(data, c("time", "wspd_vec_mean", "wdir_vec_mean", "wdir_northerly", "location"))

# Group rows by 10 mins
wind.data$interval <- cut(wind.data$time, breaks = "10 min") #THIS IS CAUSING THE ISSUE OF REMOVING THE TIME FOR THE FIRST HOUR

# Convert the interval column to POSIXct
wind.data$interval <- as.POSIXct(wind.data$interval, format = "%Y-%m-%d %H:%M:%S")

# Find indices where the time is NA
na_indices <- is.na(wind.data$interval)

# Replace NA values with "2020-06-01 00:00:00"
wind.data$interval[na_indices] <- as.POSIXct("2020-06-01 00:00:00")

# Convert the interval column to POSIXct
wind.data$interval <- as.POSIXct(wind.data$interval, format = "%Y-%m-%d %H:%M:%S")

wind.data$hour <- format(wind.data$interval, format = "%H:%M")

wind.data$hour <- as.POSIXct(wind.data$hour, format = "%H:%M")

# Create location based dfs
wind.data.Larned <- wind.data %>% filter(location == "Larned")
wind.data.LeRoy <- wind.data %>% filter(location == "LeRoy")
wind.data.Plevna <- wind.data %>% filter(location == "Plevna")
wind.data.Halstead <- wind.data %>% filter(location == "Halstead")
wind.data.Towanda <- wind.data %>% filter(location == "Towanda")

# Get averages over 10 min groups
wind.data.Larned.avg.wspd.10min <- wind.data.Larned %>% group_by(hour) %>% summarise(avg.wspd = median(wdir_northerly))
wind.data.LeRoy.avg.wspd.10min <- wind.data.LeRoy %>% group_by(hour) %>% summarise(avg.wspd = median(wdir_northerly))
wind.data.Plevna.avg.wspd.10min <- wind.data.Plevna %>% group_by(hour) %>% summarise(avg.wspd = median(wdir_northerly))
wind.data.Halstead.avg.wspd.10min <- wind.data.Halstead %>% group_by(hour) %>% summarise(avg.wspd = median(wdir_northerly))
wind.data.Towanda.avg.wspd.10min <- wind.data.Towanda %>% group_by(hour) %>% summarise(avg.wspd = median(wdir_northerly))


Larned <- ggplot(wind.data.Larned.avg.wspd.10min, aes(hour, avg.wspd)) +
  geom_line() +
  labs(x = "Time", y = "Northerly (m/s)")  + 
  ggtitle("Northerly 10min Median Speeds") +
  theme(plot.title = element_text(hjust = 0.5))
LeRoy <- ggplot(wind.data.LeRoy.avg.wspd.10min, aes(hour, avg.wspd)) +
  geom_line()+
  labs(x = "Time", y = "Northerly (m/s)")
Plevna <- ggplot(wind.data.Plevna.avg.wspd.10min, aes(hour, avg.wspd)) +
  geom_line()+
  labs(x = "Time", y = "Northerly (m/s)")
Halstead <- ggplot(wind.data.Halstead.avg.wspd.10min, aes(hour, avg.wspd)) +
  geom_line()+
  labs(x = "Time", y = "Northerly (m/s)")
Towanda <- ggplot(wind.data.Towanda.avg.wspd.10min, aes(hour, avg.wspd)) +
  geom_line()+
  labs(x = "Time", y = "Northerly (m/s)")

ggarrange(Larned, Plevna, Halstead, Towanda, LeRoy,
          labels = c("Larned", "Plevna", "Halstead", "Towanda", "LeRoy"),
          ncol = 1, nrow = 5)

#plot(avg.wspd.10min)
#ts_plot(avg.wspd.10min)
#-------------------------------------------------------------------------------
# Average Wind Speed Interquartile Range by 10min for northerly

# Select relevant columns
wind.data <- select(data, c("time", "wspd_vec_mean", "wdir_vec_mean", "wdir_northerly", "location"))

# Group rows by 10 mins
wind.data$interval <- cut(wind.data$time, breaks = "10 min") #THIS IS CAUSING THE ISSUE OF REMOVING THE TIME FOR THE FIRST HOUR

# Convert the interval column to POSIXct
wind.data$interval <- as.POSIXct(wind.data$interval, format = "%Y-%m-%d %H:%M:%S")

# Find indices where the time is NA
na_indices <- is.na(wind.data$interval)

# Replace NA values with "2020-06-01 00:00:00"
wind.data$interval[na_indices] <- as.POSIXct("2020-06-01 00:00:00")

# Convert the interval column to POSIXct
wind.data$interval <- as.POSIXct(wind.data$interval, format = "%Y-%m-%d %H:%M:%S")

wind.data$hour <- format(wind.data$interval, format = "%H:%M")

wind.data$hour <- as.POSIXct(wind.data$hour, format = "%H:%M")

# Create location based dfs
wind.data.Larned <- wind.data %>% filter(location == "Larned")
wind.data.LeRoy <- wind.data %>% filter(location == "LeRoy")
wind.data.Plevna <- wind.data %>% filter(location == "Plevna")
wind.data.Halstead <- wind.data %>% filter(location == "Halstead")
wind.data.Towanda <- wind.data %>% filter(location == "Towanda")

# Get average interquartile range
wind.data.Larned.IQR.wspd <- wind.data.Larned %>% group_by(hour) %>% summarise(IQR.wspd = IQR(wdir_northerly))
wind.data.LeRoy.IQR.wspd <- wind.data.LeRoy %>% group_by(hour) %>% summarise(IQR.wspd = IQR(wdir_northerly))
wind.data.Plevna.IQR.wspd <- wind.data.Plevna %>% group_by(hour) %>% summarise(IQR.wspd = IQR(wdir_northerly))
wind.data.Halstead.IQR.wspd <- wind.data.Halstead %>% group_by(hour) %>% summarise(IQR.wspd = IQR(wdir_northerly))
wind.data.Towanda.IQR.wspd <- wind.data.Towanda %>% group_by(hour) %>% summarise(IQR.wspd = IQR(wdir_northerly))

Larned <- ggplot(wind.data.Larned.IQR.wspd, aes(hour, IQR.wspd)) +
  geom_line() +
  labs(x = "Time", y = "Northerly (m/s)")  + 
  ggtitle("Northerly Interquartile Wind Speeds") +
  theme(plot.title = element_text(hjust = 0.5))
LeRoy <- ggplot(wind.data.LeRoy.IQR.wspd, aes(hour, IQR.wspd)) +
  geom_line()+
  labs(x = "Time", y = "Northerly (m/s)")
Plevna <- ggplot(wind.data.Plevna.IQR.wspd, aes(hour, IQR.wspd)) +
  geom_line()+
  labs(x = "Time", y = "Northerly (m/s)")
Halstead <- ggplot(wind.data.Halstead.IQR.wspd, aes(hour, IQR.wspd)) +
  geom_line()+
  labs(x = "Time", y = "Northerly (m/s)")
Towanda <- ggplot(wind.data.Towanda.IQR.wspd, aes(hour, IQR.wspd)) +
  geom_line()+
  labs(x = "Time", y = "Northerly (m/s)")

ggarrange(Larned, Plevna, Halstead, Towanda, LeRoy,
          labels = c("Larned", "Plevna", "Halstead", "Towanda", "LeRoy"),
          ncol = 1, nrow = 5)

#ts_plot(IQR.wspd)
#plot(IQR.wspd)
#-------------------------------------------------------------------------------
# Average Wind Speed Range by 10 min for northerly

# Select relevant columns
wind.data <- select(data, c("time", "wspd_vec_mean", "wdir_vec_mean", "wdir_northerly", "location"))

# Group rows by 10 mins
wind.data$interval <- cut(wind.data$time, breaks = "10 min") #THIS IS CAUSING THE ISSUE OF REMOVING THE TIME FOR THE FIRST HOUR

# Convert the interval column to POSIXct
wind.data$interval <- as.POSIXct(wind.data$interval, format = "%Y-%m-%d %H:%M:%S")

# Find indices where the time is NA
na_indices <- is.na(wind.data$interval)

# Replace NA values with "2020-06-01 00:00:00"
wind.data$interval[na_indices] <- as.POSIXct("2020-06-01 00:00:00")

# Convert the interval column to POSIXct
wind.data$interval <- as.POSIXct(wind.data$interval, format = "%Y-%m-%d %H:%M:%S")

wind.data$hour <- format(wind.data$interval, format = "%H:%M")

wind.data$hour <- as.POSIXct(wind.data$hour, format = "%H:%M")

# Create location based dfs
wind.data.Larned <- wind.data %>% filter(location == "Larned")
wind.data.LeRoy <- wind.data %>% filter(location == "LeRoy")
wind.data.Plevna <- wind.data %>% filter(location == "Plevna")
wind.data.Halstead <- wind.data %>% filter(location == "Halstead")
wind.data.Towanda <- wind.data %>% filter(location == "Towanda")

# Get average range
wind.data.Larned.range.wspd <- wind.data.Larned %>% group_by(hour) %>% summarise(range.wspd = range(wdir_northerly)[2] - range(wdir_northerly)[1])
wind.data.LeRoy.range.wspd <- wind.data.LeRoy %>% group_by(hour) %>% summarise(range.wspd = range(wdir_northerly)[2] - range(wdir_northerly)[1])
wind.data.Plevna.range.wspd <- wind.data.Plevna %>% group_by(hour) %>% summarise(range.wspd = range(wdir_northerly)[2] - range(wdir_northerly)[1])
wind.data.Halstead.range.wspd <- wind.data.Halstead %>% group_by(hour) %>% summarise(range.wspd = range(wdir_northerly)[2] - range(wdir_northerly)[1])
wind.data.Towanda.range.wspd <- wind.data.Towanda %>% group_by(hour) %>% summarise(range.wspd = range(wdir_northerly)[2] - range(wdir_northerly)[1])

Larned <- ggplot(wind.data.Larned.range.wspd, aes(hour, range.wspd)) +
  geom_line() +
  labs(x = "Time", y = "Northerly (m/s)")  + 
  ggtitle("Northerly Range of Wind Speeds") +
  theme(plot.title = element_text(hjust = 0.5))
LeRoy <- ggplot(wind.data.LeRoy.range.wspd, aes(hour, range.wspd)) +
  geom_line()+
  labs(x = "Time", y = "Northerly (m/s)")
Plevna <- ggplot(wind.data.Plevna.range.wspd, aes(hour, range.wspd)) +
  geom_line()+
  labs(x = "Time", y = "Northerly (m/s)")
Halstead <- ggplot(wind.data.Halstead.range.wspd, aes(hour, range.wspd)) +
  geom_line()+
  labs(x = "Time", y = "Northerly (m/s)")
Towanda <- ggplot(wind.data.Towanda.range.wspd, aes(hour, range.wspd)) +
  geom_line()+
  labs(x = "Time", y = "Northerly (m/s)")

ggarrange(Larned, Plevna, Halstead, Towanda, LeRoy,
          labels = c("Larned", "Plevna", "Halstead", "Towanda", "LeRoy"),
          ncol = 1, nrow = 5)
#ts_plot(range.wspd)
#plot(range.wspd)
#-------------------------------------------------------------------------------
# Median Wind vec for 1 min every 10 min easterly HAVE A GRAPH THAT AGGREGATES WIND SPEED OVER 10 MIN???

# Select relevant columns
wind.data <- select(data, c("time", "wspd_vec_mean", "wdir_vec_mean", "wdir_easterly", "location"))

# Group rows by 10 mins
wind.data$interval <- cut(wind.data$time, breaks = "10 min") #THIS IS CAUSING THE ISSUE OF REMOVING THE TIME FOR THE FIRST HOUR

# Convert the interval column to POSIXct
wind.data$interval <- as.POSIXct(wind.data$interval, format = "%Y-%m-%d %H:%M:%S")

# Find indices where the time is NA
na_indices <- is.na(wind.data$interval)

# Replace NA values with "2020-06-01 00:00:00"
wind.data$interval[na_indices] <- as.POSIXct("2020-06-01 00:00:00")

# Convert the interval column to POSIXct
wind.data$interval <- as.POSIXct(wind.data$interval, format = "%Y-%m-%d %H:%M:%S")

wind.data$hour <- format(wind.data$interval, format = "%H:%M")

wind.data$hour <- as.POSIXct(wind.data$hour, format = "%H:%M")

# Create location based dfs
wind.data.Larned <- wind.data %>% filter(location == "Larned")
wind.data.LeRoy <- wind.data %>% filter(location == "LeRoy")
wind.data.Plevna <- wind.data %>% filter(location == "Plevna")
wind.data.Halstead <- wind.data %>% filter(location == "Halstead")
wind.data.Towanda <- wind.data %>% filter(location == "Towanda")

# Get averages over 10 min groups
wind.data.Larned.avg.wspd.10min <- wind.data.Larned %>% group_by(hour) %>% summarise(avg.wspd = median(wdir_easterly))
wind.data.LeRoy.avg.wspd.10min <- wind.data.LeRoy %>% group_by(hour) %>% summarise(avg.wspd = median(wdir_easterly))
wind.data.Plevna.avg.wspd.10min <- wind.data.Plevna %>% group_by(hour) %>% summarise(avg.wspd = median(wdir_easterly))
wind.data.Halstead.avg.wspd.10min <- wind.data.Halstead %>% group_by(hour) %>% summarise(avg.wspd = median(wdir_easterly))
wind.data.Towanda.avg.wspd.10min <- wind.data.Towanda %>% group_by(hour) %>% summarise(avg.wspd = median(wdir_easterly))


Larned <- ggplot(wind.data.Larned.avg.wspd.10min, aes(hour, avg.wspd)) +
  geom_line() +
  labs(x = "Time", y = "Easterly (m/s)")  + 
  ggtitle("Easterly 10min Median Speeds") +
  theme(plot.title = element_text(hjust = 0.5))
LeRoy <- ggplot(wind.data.LeRoy.avg.wspd.10min, aes(hour, avg.wspd)) +
  geom_line()+
  labs(x = "Time", y = "Easterly (m/s)")
Plevna <- ggplot(wind.data.Plevna.avg.wspd.10min, aes(hour, avg.wspd)) +
  geom_line()+
  labs(x = "Time", y = "Easterly (m/s)")
Halstead <- ggplot(wind.data.Halstead.avg.wspd.10min, aes(hour, avg.wspd)) +
  geom_line()+
  labs(x = "Time", y = "Easterly (m/s)")
Towanda <- ggplot(wind.data.Towanda.avg.wspd.10min, aes(hour, avg.wspd)) +
  geom_line()+
  labs(x = "Time", y = "Easterly (m/s)")

ggarrange(Larned, Plevna, Halstead, Towanda, LeRoy,
          labels = c("Larned", "Plevna", "Halstead", "Towanda", "LeRoy"),
          ncol = 1, nrow = 5)

#plot(avg.wspd.10min)
#ts_plot(avg.wspd.10min)
#-------------------------------------------------------------------------------
# Average Wind Speed Interquartile Range by hour for easterly

# Select relevant columns
wind.data <- select(data, c("time", "wspd_vec_mean", "wdir_vec_mean", "wdir_easterly", "location"))

# Group rows by 10 mins
wind.data$interval <- cut(wind.data$time, breaks = "10 min") #THIS IS CAUSING THE ISSUE OF REMOVING THE TIME FOR THE FIRST HOUR

# Convert the interval column to POSIXct
wind.data$interval <- as.POSIXct(wind.data$interval, format = "%Y-%m-%d %H:%M:%S")

# Find indices where the time is NA
na_indices <- is.na(wind.data$interval)

# Replace NA values with "2020-06-01 00:00:00"
wind.data$interval[na_indices] <- as.POSIXct("2020-06-01 00:00:00")

# Convert the interval column to POSIXct
wind.data$interval <- as.POSIXct(wind.data$interval, format = "%Y-%m-%d %H:%M:%S")

wind.data$hour <- format(wind.data$interval, format = "%H:%M")

wind.data$hour <- as.POSIXct(wind.data$hour, format = "%H:%M")

# Create location based dfs
wind.data.Larned <- wind.data %>% filter(location == "Larned")
wind.data.LeRoy <- wind.data %>% filter(location == "LeRoy")
wind.data.Plevna <- wind.data %>% filter(location == "Plevna")
wind.data.Halstead <- wind.data %>% filter(location == "Halstead")
wind.data.Towanda <- wind.data %>% filter(location == "Towanda")

# Get average interquartile range
wind.data.Larned.IQR.wspd <- wind.data.Larned %>% group_by(hour) %>% summarise(IQR.wspd = IQR(wdir_easterly))
wind.data.LeRoy.IQR.wspd <- wind.data.LeRoy %>% group_by(hour) %>% summarise(IQR.wspd = IQR(wdir_easterly))
wind.data.Plevna.IQR.wspd <- wind.data.Plevna %>% group_by(hour) %>% summarise(IQR.wspd = IQR(wdir_easterly))
wind.data.Halstead.IQR.wspd <- wind.data.Halstead %>% group_by(hour) %>% summarise(IQR.wspd = IQR(wdir_easterly))
wind.data.Towanda.IQR.wspd <- wind.data.Towanda %>% group_by(hour) %>% summarise(IQR.wspd = IQR(wdir_easterly))

Larned <- ggplot(wind.data.Larned.IQR.wspd, aes(hour, IQR.wspd)) +
  geom_line() +
  labs(x = "Time", y = "Easterly (m/s)")  + 
  ggtitle("Easterly Interquartile Wind Speeds") +
  theme(plot.title = element_text(hjust = 0.5))
LeRoy <- ggplot(wind.data.LeRoy.IQR.wspd, aes(hour, IQR.wspd)) +
  geom_line()+
  labs(x = "Time", y = "Easterly (m/s)")
Plevna <- ggplot(wind.data.Plevna.IQR.wspd, aes(hour, IQR.wspd)) +
  geom_line()+
  labs(x = "Time", y = "Easterly (m/s)")
Halstead <- ggplot(wind.data.Halstead.IQR.wspd, aes(hour, IQR.wspd)) +
  geom_line()+
  labs(x = "Time", y = "Easterly (m/s)")
Towanda <- ggplot(wind.data.Towanda.IQR.wspd, aes(hour, IQR.wspd)) +
  geom_line()+
  labs(x = "Time", y = "Easterly (m/s)")

ggarrange(Larned, Plevna, Halstead, Towanda, LeRoy,
          labels = c("Larned", "Plevna", "Halstead", "Towanda", "LeRoy"),
          ncol = 1, nrow = 5)

#ts_plot(IQR.wspd)
#plot(IQR.wspd)
#-------------------------------------------------------------------------------
# Average Wind Speed Range by hour for easterly

# Select relevant columns
wind.data <- select(data, c("time", "wspd_vec_mean", "wdir_vec_mean", "wdir_easterly", "location"))

# Group rows by 10 mins
wind.data$interval <- cut(wind.data$time, breaks = "10 min") #THIS IS CAUSING THE ISSUE OF REMOVING THE TIME FOR THE FIRST HOUR

# Convert the interval column to POSIXct
wind.data$interval <- as.POSIXct(wind.data$interval, format = "%Y-%m-%d %H:%M:%S")

# Find indices where the time is NA
na_indices <- is.na(wind.data$interval)

# Replace NA values with "2020-06-01 00:00:00"
wind.data$interval[na_indices] <- as.POSIXct("2020-06-01 00:00:00")

# Convert the interval column to POSIXct
wind.data$interval <- as.POSIXct(wind.data$interval, format = "%Y-%m-%d %H:%M:%S")

wind.data$hour <- format(wind.data$interval, format = "%H:%M")

wind.data$hour <- as.POSIXct(wind.data$hour, format = "%H:%M")

# Create location based dfs
wind.data.Larned <- wind.data %>% filter(location == "Larned")
wind.data.LeRoy <- wind.data %>% filter(location == "LeRoy")
wind.data.Plevna <- wind.data %>% filter(location == "Plevna")
wind.data.Halstead <- wind.data %>% filter(location == "Halstead")
wind.data.Towanda <- wind.data %>% filter(location == "Towanda")

# Get average range
wind.data.Larned.range.wspd <- wind.data.Larned %>% group_by(hour) %>% summarise(range.wspd = range(wdir_easterly)[2] - range(wdir_easterly)[1])
wind.data.LeRoy.range.wspd <- wind.data.LeRoy %>% group_by(hour) %>% summarise(range.wspd = range(wdir_easterly)[2] - range(wdir_easterly)[1])
wind.data.Plevna.range.wspd <- wind.data.Plevna %>% group_by(hour) %>% summarise(range.wspd = range(wdir_easterly)[2] - range(wdir_easterly)[1])
wind.data.Halstead.range.wspd <- wind.data.Halstead %>% group_by(hour) %>% summarise(range.wspd = range(wdir_easterly)[2] - range(wdir_easterly)[1])
wind.data.Towanda.range.wspd <- wind.data.Towanda %>% group_by(hour) %>% summarise(range.wspd = range(wdir_easterly)[2] - range(wdir_easterly)[1])

Larned <- ggplot(wind.data.Larned.range.wspd, aes(hour, range.wspd)) +
  geom_line() +
  labs(x = "Time", y = "Easterly (m/s)")  + 
  ggtitle("Easterly Range of Wind Speeds") +
  theme(plot.title = element_text(hjust = 0.5))
LeRoy <- ggplot(wind.data.LeRoy.range.wspd, aes(hour, range.wspd)) +
  geom_line()+
  labs(x = "Time", y = "Easterly (m/s)")
Plevna <- ggplot(wind.data.Plevna.range.wspd, aes(hour, range.wspd)) +
  geom_line()+
  labs(x = "Time", y = "Easterly (m/s)")
Halstead <- ggplot(wind.data.Halstead.range.wspd, aes(hour, range.wspd)) +
  geom_line()+
  labs(x = "Time", y = "Easterly (m/s)")
Towanda <- ggplot(wind.data.Towanda.range.wspd, aes(hour, range.wspd)) +
  geom_line()+
  labs(x = "Time", y = "Easterly (m/s)")

ggarrange(Larned, Plevna, Halstead, Towanda, LeRoy,
          labels = c("Larned", "Plevna", "Halstead", "Towanda", "LeRoy"),
          ncol = 1, nrow = 5)
#ts_plot(range.wspd)
#plot(range.wspd)
#-------------------------------------------------------------------------------
# Median Wind vec for 1 min every 10 min northerly first differences

# Select relevant columns
wind.data <- select(data, c("time", "wspd_vec_mean", "wdir_vec_mean", "wdir_northerly", "first_differences_northerly", "first_differences_easterly", "location"))

# Group rows by 10 mins
wind.data$interval <- cut(wind.data$time, breaks = "10 min") #THIS IS CAUSING THE ISSUE OF REMOVING THE TIME FOR THE FIRST HOUR

# Convert the interval column to POSIXct
wind.data$interval <- as.POSIXct(wind.data$interval, format = "%Y-%m-%d %H:%M:%S")

# Find indices where the time is NA
na_indices <- is.na(wind.data$interval)

# Replace NA values with "2020-06-01 00:00:00"
wind.data$interval[na_indices] <- as.POSIXct("2020-06-01 00:00:00")

# Convert the interval column to POSIXct
wind.data$interval <- as.POSIXct(wind.data$interval, format = "%Y-%m-%d %H:%M:%S")

wind.data$hour <- format(wind.data$interval, format = "%H:%M")

wind.data$hour <- as.POSIXct(wind.data$hour, format = "%H:%M")

# Create location based dfs
wind.data.Larned <- wind.data %>% filter(location == "Larned")
wind.data.LeRoy <- wind.data %>% filter(location == "LeRoy")
wind.data.Plevna <- wind.data %>% filter(location == "Plevna")
wind.data.Halstead <- wind.data %>% filter(location == "Halstead")
wind.data.Towanda <- wind.data %>% filter(location == "Towanda")

# Get averages over 10 min groups
wind.data.Larned.avg.wspd.10min <- wind.data.Larned %>% group_by(hour) %>% summarise(first_diff_avg.wspd = median(first_differences_northerly))
wind.data.LeRoy.avg.wspd.10min <- wind.data.LeRoy %>% group_by(hour) %>% summarise(first_diff_avg.wspd = median(first_differences_northerly))
wind.data.Plevna.avg.wspd.10min <- wind.data.Plevna %>% group_by(hour) %>% summarise(first_diff_avg.wspd = median(first_differences_northerly))
wind.data.Halstead.avg.wspd.10min <- wind.data.Halstead %>% group_by(hour) %>% summarise(first_diff_avg.wspd = median(first_differences_northerly))
wind.data.Towanda.avg.wspd.10min <- wind.data.Towanda %>% group_by(hour) %>% summarise(first_diff_avg.wspd = median(first_differences_northerly))


Larned <- ggplot(wind.data.Larned.avg.wspd.10min, aes(hour, first_diff_avg.wspd)) +
  geom_line() +
  labs(x = "Time", y = "Northerly (m/s)")  + 
  ggtitle("Northerly First Differences 10min Median") +
  theme(plot.title = element_text(hjust = 0.5))
LeRoy <- ggplot(wind.data.LeRoy.avg.wspd.10min, aes(hour, first_diff_avg.wspd)) +
  geom_line()+
  labs(x = "Time", y = "Northerly (m/s)")
Plevna <- ggplot(wind.data.Plevna.avg.wspd.10min, aes(hour, first_diff_avg.wspd)) +
  geom_line()+
  labs(x = "Time", y = "Northerly (m/s)")
Halstead <- ggplot(wind.data.Halstead.avg.wspd.10min, aes(hour, first_diff_avg.wspd)) +
  geom_line()+
  labs(x = "Time", y = "Northerly (m/s)")
Towanda <- ggplot(wind.data.Towanda.avg.wspd.10min, aes(hour, first_diff_avg.wspd)) +
  geom_line()+
  labs(x = "Time", y = "Northerly (m/s)")

ggarrange(Larned, Plevna, Halstead, Towanda, LeRoy,
          labels = c("Larned", "Plevna", "Halstead", "Towanda", "LeRoy"),
          ncol = 1, nrow = 5)

#plot(avg.wspd.10min)
#ts_plot(avg.wspd.10min)
#-------------------------------------------------------------------------------
# Wind Speed Interquartile Range by 10min for northerly first differences

# Select relevant columns
wind.data <- select(data, c("time", "wspd_vec_mean", "wdir_vec_mean", "wdir_northerly", "first_differences_northerly", "first_differences_easterly", "location"))

# Group rows by 10 mins
wind.data$interval <- cut(wind.data$time, breaks = "10 min") #THIS IS CAUSING THE ISSUE OF REMOVING THE TIME FOR THE FIRST HOUR

# Convert the interval column to POSIXct
wind.data$interval <- as.POSIXct(wind.data$interval, format = "%Y-%m-%d %H:%M:%S")

# Find indices where the time is NA
na_indices <- is.na(wind.data$interval)

# Replace NA values with "2020-06-01 00:00:00"
wind.data$interval[na_indices] <- as.POSIXct("2020-06-01 00:00:00")

# Convert the interval column to POSIXct
wind.data$interval <- as.POSIXct(wind.data$interval, format = "%Y-%m-%d %H:%M:%S")

wind.data$hour <- format(wind.data$interval, format = "%H:%M")

wind.data$hour <- as.POSIXct(wind.data$hour, format = "%H:%M")

# Create location based dfs
wind.data.Larned <- wind.data %>% filter(location == "Larned")
wind.data.LeRoy <- wind.data %>% filter(location == "LeRoy")
wind.data.Plevna <- wind.data %>% filter(location == "Plevna")
wind.data.Halstead <- wind.data %>% filter(location == "Halstead")
wind.data.Towanda <- wind.data %>% filter(location == "Towanda")

# Get average interquartile range
wind.data.Larned.IQR.wspd <- wind.data.Larned %>% group_by(hour) %>% summarise(first_diff_IQR.wspd = IQR(first_differences_northerly))
wind.data.LeRoy.IQR.wspd <- wind.data.LeRoy %>% group_by(hour) %>% summarise(first_diff_IQR.wspd = IQR(first_differences_northerly))
wind.data.Plevna.IQR.wspd <- wind.data.Plevna %>% group_by(hour) %>% summarise(first_diff_IQR.wspd = IQR(first_differences_northerly))
wind.data.Halstead.IQR.wspd <- wind.data.Halstead %>% group_by(hour) %>% summarise(first_diff_IQR.wspd = IQR(first_differences_northerly))
wind.data.Towanda.IQR.wspd <- wind.data.Towanda %>% group_by(hour) %>% summarise(first_diff_IQR.wspd = IQR(first_differences_northerly))

Larned <- ggplot(wind.data.Larned.IQR.wspd, aes(hour, first_diff_IQR.wspd)) +
  geom_line() +
  labs(x = "Time", y = "Northerly (m/s)")  + 
  ggtitle("Northerly Interquartile First Differences") +
  theme(plot.title = element_text(hjust = 0.5))
LeRoy <- ggplot(wind.data.LeRoy.IQR.wspd, aes(hour, first_diff_IQR.wspd)) +
  geom_line()+
  labs(x = "Time", y = "Northerly (m/s)")
Plevna <- ggplot(wind.data.Plevna.IQR.wspd, aes(hour, first_diff_IQR.wspd)) +
  geom_line()+
  labs(x = "Time", y = "Northerly (m/s)")
Halstead <- ggplot(wind.data.Halstead.IQR.wspd, aes(hour, first_diff_IQR.wspd)) +
  geom_line()+
  labs(x = "Time", y = "Northerly (m/s)")
Towanda <- ggplot(wind.data.Towanda.IQR.wspd, aes(hour, first_diff_IQR.wspd)) +
  geom_line()+
  labs(x = "Time", y = "Northerly (m/s)")

ggarrange(Larned, Plevna, Halstead, Towanda, LeRoy,
          labels = c("Larned", "Plevna", "Halstead", "Towanda", "LeRoy"),
          ncol = 1, nrow = 5)
#-------------------------------------------------------------------------------
# Average Wind Speed Range by 10 min for northerly First Differences

# Select relevant columns
wind.data <- select(data, c("time", "wspd_vec_mean", "wdir_vec_mean", "wdir_northerly", "first_differences_northerly", "first_differences_easterly", "location"))

# Group rows by 10 mins
wind.data$interval <- cut(wind.data$time, breaks = "10 min") #THIS IS CAUSING THE ISSUE OF REMOVING THE TIME FOR THE FIRST HOUR

# Convert the interval column to POSIXct
wind.data$interval <- as.POSIXct(wind.data$interval, format = "%Y-%m-%d %H:%M:%S")

# Find indices where the time is NA
na_indices <- is.na(wind.data$interval)

# Replace NA values with "2020-06-01 00:00:00"
wind.data$interval[na_indices] <- as.POSIXct("2020-06-01 00:00:00")

# Convert the interval column to POSIXct
wind.data$interval <- as.POSIXct(wind.data$interval, format = "%Y-%m-%d %H:%M:%S")

wind.data$hour <- format(wind.data$interval, format = "%H:%M")

wind.data$hour <- as.POSIXct(wind.data$hour, format = "%H:%M")

# Create location based dfs
wind.data.Larned <- wind.data %>% filter(location == "Larned")
wind.data.LeRoy <- wind.data %>% filter(location == "LeRoy")
wind.data.Plevna <- wind.data %>% filter(location == "Plevna")
wind.data.Halstead <- wind.data %>% filter(location == "Halstead")
wind.data.Towanda <- wind.data %>% filter(location == "Towanda")

# Get average range
wind.data.Larned.range.wspd <- wind.data.Larned %>% group_by(hour) %>% summarise(first_diff_range.wspd = range(first_differences_northerly)[2] - range(first_differences_northerly)[1])
wind.data.LeRoy.range.wspd <- wind.data.LeRoy %>% group_by(hour) %>% summarise(first_diff_range.wspd = range(first_differences_northerly)[2] - range(first_differences_northerly)[1])
wind.data.Plevna.range.wspd <- wind.data.Plevna %>% group_by(hour) %>% summarise(first_diff_range.wspd = range(first_differences_northerly)[2] - range(first_differences_northerly)[1])
wind.data.Halstead.range.wspd <- wind.data.Halstead %>% group_by(hour) %>% summarise(first_diff_range.wspd = range(first_differences_northerly)[2] - range(first_differences_northerly)[1])
wind.data.Towanda.range.wspd <- wind.data.Towanda %>% group_by(hour) %>% summarise(first_diff_range.wspd = range(first_differences_northerly)[2] - range(first_differences_northerly)[1])

Larned <- ggplot(wind.data.Larned.range.wspd, aes(hour, first_diff_range.wspd)) +
  geom_line() +
  labs(x = "Time", y = "Northerly (m/s)")  + 
  ggtitle("Northerly Range of First Differences") +
  theme(plot.title = element_text(hjust = 0.5))
LeRoy <- ggplot(wind.data.LeRoy.range.wspd, aes(hour, first_diff_range.wspd)) +
  geom_line()+
  labs(x = "Time", y = "Northerly (m/s)")
Plevna <- ggplot(wind.data.Plevna.range.wspd, aes(hour, first_diff_range.wspd)) +
  geom_line()+
  labs(x = "Time", y = "Northerly (m/s)")
Halstead <- ggplot(wind.data.Halstead.range.wspd, aes(hour, first_diff_range.wspd)) +
  geom_line()+
  labs(x = "Time", y = "Northerly (m/s)")
Towanda <- ggplot(wind.data.Towanda.range.wspd, aes(hour, first_diff_range.wspd)) +
  geom_line()+
  labs(x = "Time", y = "Northerly (m/s)")

ggarrange(Larned, Plevna, Halstead, Towanda, LeRoy,
          labels = c("Larned", "Plevna", "Halstead", "Towanda", "LeRoy"),
          ncol = 1, nrow = 5)
#ts_plot(range.wspd)
#plot(range.wspd)
#-------------------------------------------------------------------------------
# Median Wind vec for 1 min every 10 min easterly first differences

# Select relevant columns
wind.data <- select(data, c("time", "wspd_vec_mean", "wdir_vec_mean", "wdir_easterly", "first_differences_easterly", "first_differences_easterly", "location"))

# Group rows by 10 mins
wind.data$interval <- cut(wind.data$time, breaks = "10 min") #THIS IS CAUSING THE ISSUE OF REMOVING THE TIME FOR THE FIRST HOUR

# Convert the interval column to POSIXct
wind.data$interval <- as.POSIXct(wind.data$interval, format = "%Y-%m-%d %H:%M:%S")

# Find indices where the time is NA
na_indices <- is.na(wind.data$interval)

# Replace NA values with "2020-06-01 00:00:00"
wind.data$interval[na_indices] <- as.POSIXct("2020-06-01 00:00:00")

# Convert the interval column to POSIXct
wind.data$interval <- as.POSIXct(wind.data$interval, format = "%Y-%m-%d %H:%M:%S")

wind.data$hour <- format(wind.data$interval, format = "%H:%M")

wind.data$hour <- as.POSIXct(wind.data$hour, format = "%H:%M")

# Create location based dfs
wind.data.Larned <- wind.data %>% filter(location == "Larned")
wind.data.LeRoy <- wind.data %>% filter(location == "LeRoy")
wind.data.Plevna <- wind.data %>% filter(location == "Plevna")
wind.data.Halstead <- wind.data %>% filter(location == "Halstead")
wind.data.Towanda <- wind.data %>% filter(location == "Towanda")

# Get averages over 10 min groups
wind.data.Larned.avg.wspd.10min <- wind.data.Larned %>% group_by(hour) %>% summarise(first_diff_avg.wspd = median(first_differences_easterly))
wind.data.LeRoy.avg.wspd.10min <- wind.data.LeRoy %>% group_by(hour) %>% summarise(first_diff_avg.wspd = median(first_differences_easterly))
wind.data.Plevna.avg.wspd.10min <- wind.data.Plevna %>% group_by(hour) %>% summarise(first_diff_avg.wspd = median(first_differences_easterly))
wind.data.Halstead.avg.wspd.10min <- wind.data.Halstead %>% group_by(hour) %>% summarise(first_diff_avg.wspd = median(first_differences_easterly))
wind.data.Towanda.avg.wspd.10min <- wind.data.Towanda %>% group_by(hour) %>% summarise(first_diff_avg.wspd = median(first_differences_easterly))


Larned <- ggplot(wind.data.Larned.avg.wspd.10min, aes(hour, first_diff_avg.wspd)) +
  geom_line() +
  labs(x = "Time", y = "Easterly (m/s)")  + 
  ggtitle("Easterly First Differences 10min Median") +
  theme(plot.title = element_text(hjust = 0.5))
LeRoy <- ggplot(wind.data.LeRoy.avg.wspd.10min, aes(hour, first_diff_avg.wspd)) +
  geom_line()+
  labs(x = "Time", y = "Easterly (m/s)")
Plevna <- ggplot(wind.data.Plevna.avg.wspd.10min, aes(hour, first_diff_avg.wspd)) +
  geom_line()+
  labs(x = "Time", y = "Easterly (m/s)")
Halstead <- ggplot(wind.data.Halstead.avg.wspd.10min, aes(hour, first_diff_avg.wspd)) +
  geom_line()+
  labs(x = "Time", y = "Easterly (m/s)")
Towanda <- ggplot(wind.data.Towanda.avg.wspd.10min, aes(hour, first_diff_avg.wspd)) +
  geom_line()+
  labs(x = "Time", y = "Easterly (m/s)")

ggarrange(Larned, Plevna, Halstead, Towanda, LeRoy,
          labels = c("Larned", "Plevna", "Halstead", "Towanda", "LeRoy"),
          ncol = 1, nrow = 5)

#plot(avg.wspd.10min)
#ts_plot(avg.wspd.10min)
#-------------------------------------------------------------------------------
# Wind Speed Interquartile Range by 10min for easterly first differences

# Select relevant columns
wind.data <- select(data, c("time", "wspd_vec_mean", "wdir_vec_mean", "wdir_easterly", "first_differences_easterly", "first_differences_easterly", "location"))

# Group rows by 10 mins
wind.data$interval <- cut(wind.data$time, breaks = "10 min") #THIS IS CAUSING THE ISSUE OF REMOVING THE TIME FOR THE FIRST HOUR

# Convert the interval column to POSIXct
wind.data$interval <- as.POSIXct(wind.data$interval, format = "%Y-%m-%d %H:%M:%S")

# Find indices where the time is NA
na_indices <- is.na(wind.data$interval)

# Replace NA values with "2020-06-01 00:00:00"
wind.data$interval[na_indices] <- as.POSIXct("2020-06-01 00:00:00")

# Convert the interval column to POSIXct
wind.data$interval <- as.POSIXct(wind.data$interval, format = "%Y-%m-%d %H:%M:%S")

wind.data$hour <- format(wind.data$interval, format = "%H:%M")

wind.data$hour <- as.POSIXct(wind.data$hour, format = "%H:%M")

# Create location based dfs
wind.data.Larned <- wind.data %>% filter(location == "Larned")
wind.data.LeRoy <- wind.data %>% filter(location == "LeRoy")
wind.data.Plevna <- wind.data %>% filter(location == "Plevna")
wind.data.Halstead <- wind.data %>% filter(location == "Halstead")
wind.data.Towanda <- wind.data %>% filter(location == "Towanda")

# Get average interquartile range
wind.data.Larned.IQR.wspd <- wind.data.Larned %>% group_by(hour) %>% summarise(first_diff_IQR.wspd = IQR(first_differences_easterly))
wind.data.LeRoy.IQR.wspd <- wind.data.LeRoy %>% group_by(hour) %>% summarise(first_diff_IQR.wspd = IQR(first_differences_easterly))
wind.data.Plevna.IQR.wspd <- wind.data.Plevna %>% group_by(hour) %>% summarise(first_diff_IQR.wspd = IQR(first_differences_easterly))
wind.data.Halstead.IQR.wspd <- wind.data.Halstead %>% group_by(hour) %>% summarise(first_diff_IQR.wspd = IQR(first_differences_easterly))
wind.data.Towanda.IQR.wspd <- wind.data.Towanda %>% group_by(hour) %>% summarise(first_diff_IQR.wspd = IQR(first_differences_easterly))

Larned <- ggplot(wind.data.Larned.IQR.wspd, aes(hour, first_diff_IQR.wspd)) +
  geom_line() +
  labs(x = "Time", y = "Easterly (m/s)")  + 
  ggtitle("Easterly Interquartile First Differences") +
  theme(plot.title = element_text(hjust = 0.5))
LeRoy <- ggplot(wind.data.LeRoy.IQR.wspd, aes(hour, first_diff_IQR.wspd)) +
  geom_line()+
  labs(x = "Time", y = "Easterly (m/s)")
Plevna <- ggplot(wind.data.Plevna.IQR.wspd, aes(hour, first_diff_IQR.wspd)) +
  geom_line()+
  labs(x = "Time", y = "Easterly (m/s)")
Halstead <- ggplot(wind.data.Halstead.IQR.wspd, aes(hour, first_diff_IQR.wspd)) +
  geom_line()+
  labs(x = "Time", y = "Easterly (m/s)")
Towanda <- ggplot(wind.data.Towanda.IQR.wspd, aes(hour, first_diff_IQR.wspd)) +
  geom_line()+
  labs(x = "Time", y = "Easterly (m/s)")

ggarrange(Larned, Plevna, Halstead, Towanda, LeRoy,
          labels = c("Larned", "Plevna", "Halstead", "Towanda", "LeRoy"),
          ncol = 1, nrow = 5)
#-------------------------------------------------------------------------------
# Average Wind Speed Range by 10 min for easterly First Differences

# Select relevant columns
wind.data <- select(data, c("time", "wspd_vec_mean", "wdir_vec_mean", "wdir_easterly", "first_differences_easterly", "first_differences_easterly", "location"))

# Group rows by 10 mins
wind.data$interval <- cut(wind.data$time, breaks = "10 min") #THIS IS CAUSING THE ISSUE OF REMOVING THE TIME FOR THE FIRST HOUR

# Convert the interval column to POSIXct
wind.data$interval <- as.POSIXct(wind.data$interval, format = "%Y-%m-%d %H:%M:%S")

# Find indices where the time is NA
na_indices <- is.na(wind.data$interval)

# Replace NA values with "2020-06-01 00:00:00"
wind.data$interval[na_indices] <- as.POSIXct("2020-06-01 00:00:00")

# Convert the interval column to POSIXct
wind.data$interval <- as.POSIXct(wind.data$interval, format = "%Y-%m-%d %H:%M:%S")

wind.data$hour <- format(wind.data$interval, format = "%H:%M")

wind.data$hour <- as.POSIXct(wind.data$hour, format = "%H:%M")

# Create location based dfs
wind.data.Larned <- wind.data %>% filter(location == "Larned")
wind.data.LeRoy <- wind.data %>% filter(location == "LeRoy")
wind.data.Plevna <- wind.data %>% filter(location == "Plevna")
wind.data.Halstead <- wind.data %>% filter(location == "Halstead")
wind.data.Towanda <- wind.data %>% filter(location == "Towanda")

# Get average range
wind.data.Larned.range.wspd <- wind.data.Larned %>% group_by(hour) %>% summarise(first_diff_range.wspd = range(first_differences_easterly)[2] - range(first_differences_easterly)[1])
wind.data.LeRoy.range.wspd <- wind.data.LeRoy %>% group_by(hour) %>% summarise(first_diff_range.wspd = range(first_differences_easterly)[2] - range(first_differences_easterly)[1])
wind.data.Plevna.range.wspd <- wind.data.Plevna %>% group_by(hour) %>% summarise(first_diff_range.wspd = range(first_differences_easterly)[2] - range(first_differences_easterly)[1])
wind.data.Halstead.range.wspd <- wind.data.Halstead %>% group_by(hour) %>% summarise(first_diff_range.wspd = range(first_differences_easterly)[2] - range(first_differences_easterly)[1])
wind.data.Towanda.range.wspd <- wind.data.Towanda %>% group_by(hour) %>% summarise(first_diff_range.wspd = range(first_differences_easterly)[2] - range(first_differences_easterly)[1])

Larned <- ggplot(wind.data.Larned.range.wspd, aes(hour, first_diff_range.wspd)) +
  geom_line() +
  labs(x = "Time", y = "Easterly (m/s)")  + 
  ggtitle("Easterly Range of First Differences") +
  theme(plot.title = element_text(hjust = 0.5))
LeRoy <- ggplot(wind.data.LeRoy.range.wspd, aes(hour, first_diff_range.wspd)) +
  geom_line()+
  labs(x = "Time", y = "Easterly (m/s)")
Plevna <- ggplot(wind.data.Plevna.range.wspd, aes(hour, first_diff_range.wspd)) +
  geom_line()+
  labs(x = "Time", y = "Easterly (m/s)")
Halstead <- ggplot(wind.data.Halstead.range.wspd, aes(hour, first_diff_range.wspd)) +
  geom_line()+
  labs(x = "Time", y = "Easterly (m/s)")
Towanda <- ggplot(wind.data.Towanda.range.wspd, aes(hour, first_diff_range.wspd)) +
  geom_line()+
  labs(x = "Time", y = "Easterly (m/s)")

ggarrange(Larned, Plevna, Halstead, Towanda, LeRoy,
          labels = c("Larned", "Plevna", "Halstead", "Towanda", "LeRoy"),
          ncol = 1, nrow = 5)
#ts_plot(range.wspd)
#plot(range.wspd)
#-------------------------------------------------------------------------------
# Northerly Lagged Correlation
# Northerly First Differences

# Select Columns
wind.data <- select(data, c("time", "wdir_easterly", "wdir_northerly", "first_differences_easterly", "first_differences_northerly", "location"))

# Create location based dfs
wind.data.Larned <- wind.data %>% filter(location == "Larned")
wind.data.LeRoy <- wind.data %>% filter(location == "LeRoy")
wind.data.Plevna <- wind.data %>% filter(location == "Plevna")
wind.data.Halstead <- wind.data %>% filter(location == "Halstead")
wind.data.Towanda <- wind.data %>% filter(location == "Towanda")

lag2.plot(wind.data$first_differences_northerly, wind.data$first_differences_easterly, 10)

Larned <- ggplot(wind.data.Larned, aes(time, first_differences_northerly)) +
  geom_line() +
  labs(x = "Time", y = "Northerly (m/s)") + 
  ggtitle("Northerly First Differences") +
  theme(plot.title = element_text(hjust = 0.5))
LeRoy <- ggplot(wind.data.LeRoy, aes(time, first_differences_northerly)) +
  geom_line() +
  labs(x = "Time", y = "Northerly (m/s)")
Plevna <- ggplot(wind.data.Plevna, aes(time, first_differences_northerly)) +
  geom_line() +
  labs(x = "Time", y = "Northerly (m/s)")
Halstead <- ggplot(wind.data.Halstead, aes(time, first_differences_northerly)) +
  geom_line() +
  labs(x = "Time", y = "Northerly (m/s)")
Towanda <- ggplot(wind.data.Towanda, aes(time, first_differences_northerly)) +
  geom_line() +
  labs(x = "Time", y = "Northerly (m/s)")

ggarrange(Larned, Plevna, Halstead, Towanda, LeRoy,
          labels = c("Larned", "Plevna", "Halstead", "Towanda", "LeRoy"),
          ncol = 1, nrow = 5)
#-------------------------------------------------------------------------------
# MAP

# Get map data for Kansas
kansas_map <- map_data("state", region = "kansas")

points_data <- data.frame(
  lat = c(38.202, 37.953, 38.114, 37.842, 38.201),  # Example latitudes of points
  long = c(-99.316, -98.329, -97.513, -97.02, -95.597),  # Example longitudes of points
  label = c("Larned", "Plevna", "Halstead", "Towanda", "LeRoy")
)

# Plotting the map of Kansas with points, labels, and a horizontal line at latitude 39
ggplot() +
  geom_polygon(data = kansas_map, aes(x = long, y = lat, group = group), 
               fill = "lightblue", color = "black") +
  geom_point(data = points_data, aes(x = long, y = lat), color = "red", size = 3) +
  geom_text(data = points_data, aes(x = long, y = lat, label = label), 
            color = "black", size = 3, nudge_y = 0.1) +  # Add text labels
  geom_hline(yintercept = 38, linetype = "dashed", color = "blue", size = 1) +  # Add horizontal line
  geom_text(aes(x = -101, y = 38, label = "Latitude 38"), 
            color = "blue", size = 3, hjust = 0, vjust = -0.5) +  # Label for the line
  coord_fixed(1.3) +  # Adjust the aspect ratio
  ggtitle("Locations of Measurements") +
  theme_void()  # Remove axes and gridlines
#-------------------------------------------------------------------------------
# Lagged Correlation

# Select Columns
wind.data <- select(data, c("time", "wdir_easterly", "wdir_northerly", "first_differences_easterly", "first_differences_northerly", "location"))

# Create location based dfs
wind.data.Larned <- wind.data %>% filter(location == "Larned")
wind.data.LeRoy <- wind.data %>% filter(location == "LeRoy")
wind.data.Plevna <- wind.data %>% filter(location == "Plevna")
wind.data.Halstead <- wind.data %>% filter(location == "Halstead")
wind.data.Towanda <- wind.data %>% filter(location == "Towanda")

acf_points <- acf(wind.data.Larned$wdir_easterly, lag.max = 60)
acf_points
# Plot ACF values as a scatterplot
plot(acf_points$lag, acf_points$acf, lwd = 2, col = "blue",
     main = "Autocorrelation Function (ACF)",
     xlab = "Lag", ylab = "ACF Value")


ccf_points_Larned_Plevna <- ccf(wind.data.Larned$wdir_easterly, wind.data.Plevna$wdir_easterly, 60)
ccf_points_Larned_Halstead <- ccf(wind.data.Larned$wdir_easterly, wind.data.Halstead$wdir_easterly, 60)
ccf_points_Larned_Towanda <- ccf(wind.data.Larned$wdir_easterly, wind.data.Towanda$wdir_easterly, 60)
ccf_points_Larned_LeRoy <- ccf(wind.data.Larned$wdir_easterly, wind.data.LeRoy$wdir_easterly, 60)

Plevna <- plot(ccf_points_Larned_Plevna$lag, ccf_points_Larned_Plevna$acf, lwd = 2, col = "blue",
                   main = "Autocorrelation Function (ACF)",
                   xlab = "Lag", ylab = "ACF Value")
Halstead <- plot(ccf_points_Larned_Halstead$lag, ccf_points_Larned_Halstead$acf, lwd = 2, col = "blue",
                      main = "Autocorrelation Function (ACF)",
                      xlab = "Lag", ylab = "ACF Value")
Towanda <- plot(ccf_points_Larned_Towanda$lag, ccf_points_Larned_Towanda$acf, lwd = 2, col = "blue",
                      main = "Autocorrelation Function (ACF)",
                      xlab = "Lag", ylab = "ACF Value")
LeRoy <- plot(ccf_points_Larned_LeRoy$lag, ccf_points_Larned_LeRoy$acf, lwd = 2, col = "blue",
              main = "Autocorrelation Function (ACF)",
              xlab = "Lag", ylab = "ACF Value")

palette()
# Plot ACF for multiple locations on the same plot
plot(ccf_points_Larned_Plevna$lag, ccf_points_Larned_Plevna$acf, 
     lwd = 2, col = "dodgerblue3", 
     main = "Lagged Correlation between Larned and others",
     xlab = "Lag", ylab = "ACF Value", 
     ylim = c(-.35, .2))#,  # Set y-axis limits to -1 to 1 for comparison
     #type = "h")       # Plot as a high-density plot (vertical bars)

points(ccf_points_Larned_Halstead$lag, ccf_points_Larned_Halstead$acf, 
      lwd = 2, col = "#F6483C")

points(ccf_points_Larned_Towanda$lag, ccf_points_Larned_Towanda$acf, 
      lwd = 2, col = "#CD1874")

points(ccf_points_Larned_LeRoy$lag, ccf_points_Larned_LeRoy$acf, 
       lwd = 2, col = "#00BA38")

# Add horizontal reference lines at significance levels
abline(h = c(0, -1.96/sqrt(length(wind.data.Larned$wdir_easterly))), 
       col = "gray", lty = 2)
abline(h = c(0, 1.96/sqrt(length(wind.data.Larned$wdir_easterly))), 
       col = "gray", lty = 2)

# Add legend outside the plot area
legend("topright", 
       legend = c("Plevna", "Halstead", "Towanda", "LeRoy"), 
       col = c("dodgerblue3", "#F6483C", "#CD1874", "#00BA38"), 
       lty = 1, lwd = 2,
       xjust = 10, yjust = 10,  # Adjust justification for legend position
       bg = "white",          # Set legend background color
       box.lty = 0,           # Remove legend border
       xpd = TRUE)            # Allow legend outside plot area




