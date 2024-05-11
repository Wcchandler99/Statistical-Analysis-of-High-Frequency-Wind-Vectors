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


# View the updated data frame
print(df)
#--------------------------------------------
############################################
# Basic Data Munging
wind.data <- select(data, c("time", "wdir_easterly", "wdir_northerly", "first_differences_easterly", "first_differences_northerly", "location"))

# Add column for Hour
wind.data$hour <- as.numeric(format(wind.data$time, format = "%H"))

# Add column for Minute
wind.data$minute <- as.numeric(format(wind.data$time, format = "%M"))

# Filter to just hours 11, 12, 13
wind.data.midday <- wind.data %>% filter(hour > 10 & hour < 14)

# Aggregate wdir by hour Northerly
wind.data <- wind.data %>% group_by(hour) %>% summarise(wdir_northerly_avg = mean(wdir_northerly))

# Aggregate wdir by hour Northerly
wind.data <- wind.data %>% group_by(hour) %>% summarise(wdir_easterly_avg = mean(wdir_easterly))

# Create location based dfs
wind.data.Larned <- wind.data %>% filter(location == "Larned")
wind.data.LeRoy <- wind.data %>% filter(location == "LeRoy")
wind.data.Plevna <- wind.data %>% filter(location == "Plevna")
wind.data.Halstead <- wind.data %>% filter(location == "Halstead")
wind.data.Towanda <- wind.data %>% filter(location == "Towanda")

# Create hour based dfs
wind.data.0 <- wind.data %>% filter(hour == 0)
wind.data.1 <- wind.data %>% filter(hour == 1)
wind.data.2 <- wind.data %>% filter(hour == 2)
wind.data.3 <- wind.data %>% filter(hour == 3)
wind.data.4 <- wind.data %>% filter(hour == 4)
wind.data.5 <- wind.data %>% filter(hour == 5)
wind.data.6 <- wind.data %>% filter(hour == 6)
wind.data.7 <- wind.data %>% filter(hour == 7)
wind.data.8 <- wind.data %>% filter(hour == 8)
wind.data.9 <- wind.data %>% filter(hour == 9)
wind.data.10 <- wind.data %>% filter(hour == 10)
wind.data.11 <- wind.data %>% filter(hour == 11)
wind.data.12 <- wind.data %>% filter(hour == 12)

################################################3
# Advanced Data Munging
#------------------
#Kmeans clustering (group by hour first)
wind.data <- select(wind.data, c("wdir_northerly", "wdir_easterly"))

kmeans_result <- kmeans(wind.data, centers = 2)
#------------------
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

# Median Wind vec for 1 min every 10 min northerly HAVE A GRAPH THAT AGGREGATES WIND SPEED OVER 10 MIN???
# Get averages over 10 min groups
avg.wspd.10min <- wind.data %>% group_by(hour) %>% summarise(avg.wspd = mean(wdir_northerly))

# Average Wind Speed Interquartile Range by hour for northerly
# Get average interquartile range
IQR.wspd <- wind.data %>% group_by(hour) %>% summarise(IQR.wspd = IQR(wdir_northerly))

# Average Wind Speed Range by hour for northerly
# Get average range
range.wspd <- wind.data %>% group_by(hour) %>% summarise(range.wspd = range(wdir_northerly)[2] - range(wdir_northerly)[1])

############################################
# PLOTS
#------------------------------
# northerly first differences

# Group By Hour
Larned <- ggplot(wind.data.Larned, aes(group = hour, x = time, y = first_differences_northerly)) +
  geom_boxplot() +
  labs(x = "Time", y = "Northerly (m/s)") 
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

# Not Grouped by Hour
Larned <- ggplot(wind.data.Larned, aes(time, first_differences_northerly)) +
  geom_line() +
  labs(x = "Time", y = "Northerly (m/s)")
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
#-----------------------------------
# northerly Regular
# Grouped by hour
Larned <- ggplot(wind.data.Larned, aes(group = hour, y = wdir_northerly)) +
  geom_boxplot()+
  labs(x = "Time", y = "Northerly (m/s)")
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

# Not Grouped by Hour
Larned <- ggplot(wind.data.Larned, aes(time, wdir_northerly)) +
  geom_line() +
  labs(x = "Time", y = "Northerly (m/s)")
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
#--------------------------------
# easterly
Larned <- ggplot(wind.data.Larned, aes(time, wdir_easterly)) +
  geom_line()
LeRoy <- ggplot(wind.data.LeRoy, aes(time, wdir_easterly)) +
  geom_line()
Plevna <- ggplot(wind.data.Plevna, aes(time, wdir_easterly)) +
  geom_line()
Halstead <- ggplot(wind.data.Halstead, aes(time, wdir_easterly)) +
  geom_line()
Towanda <- ggplot(wind.data.Towanda, aes(time, wdir_easterly)) +
  geom_line()

ggarrange(Larned, Plevna, Halstead, Towanda, LeRoy,
          labels = c("Larned", "Plevna", "Halstead", "Towanda", "LeRoy"),
          ncol = 1, nrow = 5)
#----------------------------------
# Scatter Plot
hour0 <- ggplot(wind.data.0, aes(x = wdir_easterly, y = wdir_northerly, color = location)) + 
  geom_point()
hour1 <- ggplot(wind.data.1, aes(x = wdir_easterly, y = wdir_northerly, color = location)) + 
  geom_point()
hour2 <- ggplot(wind.data.2, aes(x = wdir_easterly, y = wdir_northerly, color = location)) + 
  geom_point()
hour3 <- ggplot(wind.data.3, aes(x = wdir_easterly, y = wdir_northerly, color = location)) + 
  geom_point()
hour4 <- ggplot(wind.data.4, aes(x = wdir_easterly, y = wdir_northerly, color = location)) + 
  geom_point()
hour5 <- ggplot(wind.data.5, aes(x = wdir_easterly, y = wdir_northerly, color = location)) + 
  geom_point()
hour6 <- ggplot(wind.data.6, aes(x = wdir_easterly, y = wdir_northerly, color = location)) + 
  geom_point()
hour7 <- ggplot(wind.data.7, aes(x = wdir_easterly, y = wdir_northerly, color = location)) + 
  geom_point()
hour8 <- ggplot(wind.data.8, aes(x = wdir_easterly, y = wdir_northerly, color = location)) + 
  geom_point()
hour9 <- ggplot(wind.data.9, aes(x = wdir_easterly, y = wdir_northerly, color = location)) + 
  geom_point()
hour10 <- ggplot(wind.data.10, aes(x = wdir_easterly, y = wdir_northerly, color = location)) + 
  geom_point()
hour11 <- ggplot(wind.data.11, aes(x = wdir_easterly, y = wdir_northerly, color = location)) + 
  geom_point()
hour12 <- ggplot(wind.data.12, aes(x = wdir_easterly, y = wdir_northerly, color = location)) + 
  geom_point()


ggarrange(hour0, hour1, hour2, hour3, hour4, hour5, hour6, hour7, hour8, hour9, hour10, hour11, #hour12,
          labels = c("hour0", "hour1", "hour2", "hour3", "hour4", "hour5", "hour6", "hour7", "hour8", "hour9", "hour10", "hour11"), #"hour12"),
          ncol = 4, nrow = 3)
#------------------------------------
# Scatter plot by hour
hour0 <- ggplot(wind.data.0, aes(x = wdir_easterly_avg, y = wdir_northerly_avg, color = location)) + 
  geom_point()
hour1 <- ggplot(wind.data.1, aes(x = wdir_easterly_avg, y = wdir_northerly_avg, color = location)) + 
  geom_point()
hour2 <- ggplot(wind.data.2, aes(x = wdir_easterly_avg, y = wdir_northerly_avg, color = location)) + 
  geom_point()
hour3 <- ggplot(wind.data.3, aes(x = wdir_easterly_avg, y = wdir_northerly_avg, color = location)) + 
  geom_point()
hour4 <- ggplot(wind.data.4, aes(x = wdir_easterly_avg, y = wdir_northerly_avg, color = location)) + 
  geom_point()
hour5 <- ggplot(wind.data.5, aes(x = wdir_easterly_avg, y = wdir_northerly_avg, color = location)) + 
  geom_point()
hour6 <- ggplot(wind.data.6, aes(x = wdir_easterly_avg, y = wdir_northerly_avg, color = location)) + 
  geom_point()
hour7 <- ggplot(wind.data.7, aes(x = wdir_easterly_avg, y = wdir_northerly_avg, color = location)) + 
  geom_point()
hour8 <- ggplot(wind.data.8, aes(x = wdir_easterly_avg, y = wdir_northerly_avg, color = location)) + 
  geom_point()
hour9 <- ggplot(wind.data.9, aes(x = wdir_easterly_avg, y = wdir_northerly_avg, color = location)) + 
  geom_point()
hour10 <- ggplot(wind.data.10, aes(x = wdir_easterly_avg, y = wdir_northerly_avg, color = location)) + 
  geom_point()
hour11 <- ggplot(wind.data.11, aes(x = wdir_easterly_avg, y = wdir_northerly_avg, color = location)) + 
  geom_point()
hour12 <- ggplot(wind.data.12, aes(x = wdir_easterly_avg, y = wdir_northerly_avg, color = location)) + 
  geom_point()


ggarrange(hour0, hour1, hour2, hour3, hour4, hour5, hour6, hour7, hour8, hour9, hour10, hour11, #hour12,
          labels = c("hour0", "hour1", "hour2", "hour3", "hour4", "hour5", "hour6", "hour7", "hour8", "hour9", "hour10", "hour11"), #"hour12"),
          ncol = 4, nrow = 3)
#------------------------------------------
# KMeans clustering
ggplot(wind.data, aes(x = wdir_easterly, y = wdir_northerly)) +
  geom_point(aes(color = factor(kmeans_result$cluster))) +
  geom_point(data = as.data.frame(kmeans_result$centers), aes(x = wdir_easterly, y = wdir_northerly), color = "black", size = 3, shape = 4) +
  labs(title = "K-means Clustering") +
  theme_minimal()
#------------------------------------------
# Median Wind vec for 1 min every 10 min northerly HAVE A GRAPH THAT AGGREGATES WIND SPEED OVER 10 MIN???
plot(avg.wspd.10min)
ts_plot(avg.wspd.10min)
#------------------------------------------
# Average Wind Speed Interquartile Range by hour for northerly
ts_plot(IQR.wspd)
plot(IQR.wspd)
#------------------------------------------
# Average Wind Speed Range by hour for northerly
ts_plot(range.wspd)
plot(range.wspd)
#------------------------------------------
################################################################################
#------------------------------SCRATCH WORK-------------------------------------
################################################################################

# northerly first differences

# Group rows by Hour
#wind.data$hour <- as.numeric(format(wind.data$time, format = "%H"))

#wind.data <- wind.data %>% filter(hour > 10 & hour < 14)

#wind.data.Larned <- wind.data %>% filter(location == "Larned")
#wind.data.LeRoy <- wind.data %>% filter(location == "LeRoy")
#wind.data.Plevna <- wind.data %>% filter(location == "Plevna")
#wind.data.Halstead <- wind.data %>% filter(location == "Halstead")
#wind.data.Towanda <- wind.data %>% filter(location == "Towanda")

Larned <- ggplot(wind.data.Larned, aes(group = hour, y = first_differences_northerly)) +
  geom_boxplot() 
LeRoy <- ggplot(wind.data.LeRoy, aes(group = hour, y = first_differences_northerly)) +
  geom_boxplot()
Plevna <- ggplot(wind.data.Plevna, aes(group = hour, y = first_differences_northerly)) +
  geom_boxplot()
Halstead <- ggplot(wind.data.Halstead, aes(group = hour, y = first_differences_northerly)) +
  geom_boxplot()
Towanda <- ggplot(wind.data.Towanda, aes(group = hour, y = first_differences_northerly)) +
  geom_boxplot()

Larned <- ggplot(wind.data.Larned, aes(time, first_differences_northerly)) +
  geom_line() 
LeRoy <- ggplot(wind.data.LeRoy, aes(time, first_differences_northerly)) +
  geom_line()
Plevna <- ggplot(wind.data.Plevna, aes(time, first_differences_northerly)) +
  geom_line()
Halstead <- ggplot(wind.data.Halstead, aes(time, first_differences_northerly)) +
  geom_line()
Towanda <- ggplot(wind.data.Towanda, aes(time, first_differences_northerly)) +
  geom_line()

ggarrange(Larned, Plevna, Halstead, Towanda, LeRoy,
          labels = c("Larned", "Plevna", "Halstead", "Towanda", "LeRoy"),
          ncol = 1, nrow = 5)

############################################
# Using all wind directions
#wind.data <- select(data, c("time", "wspd_arith_mean", "wspd_vec_mean", "wdir_vec_mean", "wdir_vec_std", "lat", "lon", "alt", "location"))
#View(wind.data)

#wind.data <- wind.data %>% mutate(location = if_else(.$lat = 38.202))

ggplot(wind.data, aes(time, wspd_arith_mean)) +
  geom_line()

ggplot(wind.data, aes(time, wspd_vec_mean)) +
  geom_line()
############################################3
# Northerly Wind Vectors
#wind.data <- select(data, c("time", "wspd_arith_mean", "wspd_vec_mean", "wdir_vec_mean", "wdir_vec_std", "wdir_northerly", "lat", "lon", "alt", "location"))
#View(wind.data)
#----------------------------------
# Group rows by Hour
#wind.data$hour <- as.numeric(format(wind.data$time, format = "%H"))

#wind.data <- wind.data %>% group_by(hour) %>% summarise(wdir_northerly_avg = mean(wdir_northerly))

wind.data.Larned <- wind.data %>% filter(location == "Larned")
wind.data.LeRoy <- wind.data %>% filter(location == "LeRoy")
wind.data.Plevna <- wind.data %>% filter(location == "Plevna")
wind.data.Halstead <- wind.data %>% filter(location == "Halstead")
wind.data.Towanda <- wind.data %>% filter(location == "Towanda")
#----------------------------------
# Group rows by Hour and Minute
#wind.data$hour <- as.numeric(format(wind.data$time, format = "%H"))
#wind.data$minute <- as.numeric(format(wind.data$time, format = "%M"))

#wind.data <- wind.data %>% filter(hour > 11 & hour < 14)

#wind.data <- wind.data %>% group_by(hour) %>% summarise(wdir_northerly_avg = mean(wdir_northerly))

wind.data.Larned <- wind.data %>% filter(location == "Larned")
wind.data.LeRoy <- wind.data %>% filter(location == "LeRoy")
wind.data.Plevna <- wind.data %>% filter(location == "Plevna")
wind.data.Halstead <- wind.data %>% filter(location == "Halstead")
wind.data.Towanda <- wind.data %>% filter(location == "Towanda")
#---------------------------------
Larned <- ggplot(wind.data.Larned, aes(time, wdir_northerly)) +
  geom_line() 
LeRoy <- ggplot(wind.data.LeRoy, aes(time, wdir_northerly)) +
  geom_line()
Plevna <- ggplot(wind.data.Plevna, aes(time, wdir_northerly)) +
  geom_line()
Halstead <- ggplot(wind.data.Halstead, aes(time, wdir_northerly)) +
  geom_line()
Towanda <- ggplot(wind.data.Towanda, aes(time, wdir_northerly)) +
  geom_line()

ggarrange(Larned, Plevna, Halstead, Towanda, LeRoy,
                    labels = c("Larned", "Plevna", "Halstead", "Towanda", "LeRoy"),
                    ncol = 1, nrow = 5)
#---------------------------------
Larned <- ggplot(wind.data.Larned, aes(group = hour, y = wdir_northerly)) +
  geom_boxplot()
LeRoy <- ggplot(wind.data.LeRoy, aes(group = hour, y =  wdir_northerly)) +
  geom_boxplot()
Plevna <- ggplot(wind.data.Plevna, aes(group = hour, y =  wdir_northerly)) +
  geom_boxplot()
Halstead <- ggplot(wind.data.Halstead, aes(group = hour, y = wdir_northerly)) +
  geom_boxplot()
Towanda <- ggplot(wind.data.Towanda, aes(group = hour, y = wdir_northerly)) +
  geom_boxplot()

ggarrange(Larned, Plevna, Halstead, Towanda, LeRoy,
          labels = c("Larned", "Plevna", "Halstead", "Towanda", "LeRoy"),
          ncol = 1, nrow = 5)
#---------------------------------
# Easterly Wind Vectors

#wind.data <- select(data, c("time", "wspd_arith_mean", "wspd_vec_mean", "wdir_vec_mean", "wdir_vec_std", "wdir_easterly", "lat", "lon", "alt", "location"))
#View(wind.data)

#wind.data.Larned <- wind.data %>% filter(time < "2020-06-21 04:00:00") %>% filter(location == "Larned")
#wind.data.LeRoy <- wind.data %>% filter(time < "2020-06-21 04:00:00") %>% filter(location == "LeRoy")
#wind.data.Plevna <- wind.data %>% filter(time < "2020-06-21 04:00:00") %>% filter(location == "Plevna")
#wind.data.Halstead <- wind.data %>% filter(time < "2020-06-21 04:00:00") %>% filter(location == "Halstead")
#wind.data.Towanda <- wind.data %>% filter(time < "2020-06-21 04:00:00") %>% filter(location == "Towanda")

#wind.data <- wind.data %>% group_by(hour) %>% summarise(wdir_easterly_avg = mean(wdir_easterly))

Larned <- ggplot(wind.data.Larned, aes(time, wdir_easterly)) +
  geom_line()
LeRoy <- ggplot(wind.data.LeRoy, aes(time, wdir_easterly)) +
  geom_line()
Plevna <- ggplot(wind.data.Plevna, aes(time, wdir_easterly)) +
  geom_line()
Halstead <- ggplot(wind.data.Halstead, aes(time, wdir_easterly)) +
  geom_line()
Towanda <- ggplot(wind.data.Towanda, aes(time, wdir_easterly)) +
  geom_line()

ggarrange(Larned, Plevna, Halstead, Towanda, LeRoy,
          labels = c("Larned", "Plevna", "Halstead", "Towanda", "LeRoy"),
          ncol = 1, nrow = 5)
#---------------------------------
# Wind Vectors by hours scatterplots
wind.data <- select(data, c("time", "wspd_vec_mean", "wdir_vec_mean", "wdir_northerly", "wdir_easterly", "location"))
#View(wind.data)

# Group rows by Hour
wind.data$hour <- as.numeric(format(wind.data$time, format = "%H"))

wind.data.0 <- wind.data %>% filter(hour == 0)
wind.data.1 <- wind.data %>% filter(hour == 1)
wind.data.2 <- wind.data %>% filter(hour == 2)
wind.data.3 <- wind.data %>% filter(hour == 3)
wind.data.4 <- wind.data %>% filter(hour == 4)
wind.data.5 <- wind.data %>% filter(hour == 5)
wind.data.6 <- wind.data %>% filter(hour == 6)
wind.data.7 <- wind.data %>% filter(hour == 7)
wind.data.8 <- wind.data %>% filter(hour == 8)
wind.data.9 <- wind.data %>% filter(hour == 9)
wind.data.10 <- wind.data %>% filter(hour == 10)
wind.data.11 <- wind.data %>% filter(hour == 11)
wind.data.12 <- wind.data %>% filter(hour == 12)


hour0 <- ggplot(wind.data.0, aes(x = wdir_easterly, y = wdir_northerly, color = location)) + 
  geom_point()
hour1 <- ggplot(wind.data.1, aes(x = wdir_easterly, y = wdir_northerly, color = location)) + 
  geom_point()
hour2 <- ggplot(wind.data.2, aes(x = wdir_easterly, y = wdir_northerly, color = location)) + 
  geom_point()
hour3 <- ggplot(wind.data.3, aes(x = wdir_easterly, y = wdir_northerly, color = location)) + 
  geom_point()
hour4 <- ggplot(wind.data.4, aes(x = wdir_easterly, y = wdir_northerly, color = location)) + 
  geom_point()
hour5 <- ggplot(wind.data.5, aes(x = wdir_easterly, y = wdir_northerly, color = location)) + 
  geom_point()
hour6 <- ggplot(wind.data.6, aes(x = wdir_easterly, y = wdir_northerly, color = location)) + 
  geom_point()
hour7 <- ggplot(wind.data.7, aes(x = wdir_easterly, y = wdir_northerly, color = location)) + 
  geom_point()
hour8 <- ggplot(wind.data.8, aes(x = wdir_easterly, y = wdir_northerly, color = location)) + 
  geom_point()
hour9 <- ggplot(wind.data.9, aes(x = wdir_easterly, y = wdir_northerly, color = location)) + 
  geom_point()
hour10 <- ggplot(wind.data.10, aes(x = wdir_easterly, y = wdir_northerly, color = location)) + 
  geom_point()
hour11 <- ggplot(wind.data.11, aes(x = wdir_easterly, y = wdir_northerly, color = location)) + 
  geom_point()
hour12 <- ggplot(wind.data.12, aes(x = wdir_easterly, y = wdir_northerly, color = location)) + 
  geom_point()


ggarrange(hour0, hour1, hour2, hour3, hour4, hour5, hour6, hour7, hour8, hour9, hour10, hour11, #hour12,
          labels = c("hour0", "hour1", "hour2", "hour3", "hour4", "hour5", "hour6", "hour7", "hour8", "hour9", "hour10", "hour11"), #"hour12"),
          ncol = 4, nrow = 3)
#---------------------------------
# Wind Vectors by hours scatterplots
wind.data <- select(data, c("time", "wspd_vec_mean", "wdir_vec_mean", "wdir_northerly", "wdir_easterly", "location"))
#View(wind.data)

# Group rows by Hour
wind.data$hour <- as.numeric(format(wind.data$time, format = "%H"))

wind.data <- wind.data %>% group_by(location, hour)  %>% summarise(wdir_easterly_avg = mean(wdir_easterly), wdir_northerly_avg = mean(wdir_northerly))

wind.data.0 <- wind.data %>% filter(hour == 0)
wind.data.1 <- wind.data %>% filter(hour == 1)
wind.data.2 <- wind.data %>% filter(hour == 2)
wind.data.3 <- wind.data %>% filter(hour == 3)
wind.data.4 <- wind.data %>% filter(hour == 4)
wind.data.5 <- wind.data %>% filter(hour == 5)
wind.data.6 <- wind.data %>% filter(hour == 6)
wind.data.7 <- wind.data %>% filter(hour == 7)
wind.data.8 <- wind.data %>% filter(hour == 8)
wind.data.9 <- wind.data %>% filter(hour == 9)
wind.data.10 <- wind.data %>% filter(hour == 10)
wind.data.11 <- wind.data %>% filter(hour == 11)
wind.data.12 <- wind.data %>% filter(hour == 12)


hour0 <- ggplot(wind.data.0, aes(x = wdir_easterly_avg, y = wdir_northerly_avg, color = location)) + 
  geom_point()
hour1 <- ggplot(wind.data.1, aes(x = wdir_easterly_avg, y = wdir_northerly_avg, color = location)) + 
  geom_point()
hour2 <- ggplot(wind.data.2, aes(x = wdir_easterly_avg, y = wdir_northerly_avg, color = location)) + 
  geom_point()
hour3 <- ggplot(wind.data.3, aes(x = wdir_easterly_avg, y = wdir_northerly_avg, color = location)) + 
  geom_point()
hour4 <- ggplot(wind.data.4, aes(x = wdir_easterly_avg, y = wdir_northerly_avg, color = location)) + 
  geom_point()
hour5 <- ggplot(wind.data.5, aes(x = wdir_easterly_avg, y = wdir_northerly_avg, color = location)) + 
  geom_point()
hour6 <- ggplot(wind.data.6, aes(x = wdir_easterly_avg, y = wdir_northerly_avg, color = location)) + 
  geom_point()
hour7 <- ggplot(wind.data.7, aes(x = wdir_easterly_avg, y = wdir_northerly_avg, color = location)) + 
  geom_point()
hour8 <- ggplot(wind.data.8, aes(x = wdir_easterly_avg, y = wdir_northerly_avg, color = location)) + 
  geom_point()
hour9 <- ggplot(wind.data.9, aes(x = wdir_easterly_avg, y = wdir_northerly_avg, color = location)) + 
  geom_point()
hour10 <- ggplot(wind.data.10, aes(x = wdir_easterly_avg, y = wdir_northerly_avg, color = location)) + 
  geom_point()
hour11 <- ggplot(wind.data.11, aes(x = wdir_easterly_avg, y = wdir_northerly_avg, color = location)) + 
  geom_point()
hour12 <- ggplot(wind.data.12, aes(x = wdir_easterly_avg, y = wdir_northerly_avg, color = location)) + 
  geom_point()


ggarrange(hour0, hour1, hour2, hour3, hour4, hour5, hour6, hour7, hour8, hour9, hour10, hour11, #hour12,
          labels = c("hour0", "hour1", "hour2", "hour3", "hour4", "hour5", "hour6", "hour7", "hour8", "hour9", "hour10", "hour11"), #"hour12"),
          ncol = 4, nrow = 3)

#ggplot(wind.data, aes(x = wdir_easterly_avg, y = wdir_northerly_avg, color = location, label = hour)) + 
#  geom_point() +
#  geom_text(aes(label = hour), color = "black", vjust = 1, hjust = -0.5) +
#  geom_line() +
  # Group the data by location to ensure that lines are drawn separately for each location
#  geom_line(aes(group = location))

#-------------------------------------------------
#Kmeans clustering northerly
wind.data <- select(data, c("time", "wdir_northerly", "wdir_easterly"))
#View(wind.data)

# Group rows by Hour
wind.data$hour <- as.numeric(format(wind.data$time, format = "%H"))

wind.data <- wind.data %>% filter(hour == 0)

#Kmeans
kmeans_result <- kmeans(wind.data, centers = 2)


# Visualize the clustering result
ggplot(wind.data, aes(x = wdir_easterly, y = wdir_northerly)) +
  geom_point(aes(color = factor(kmeans_result$cluster))) +
  geom_point(data = as.data.frame(kmeans_result$centers), aes(x = wdir_easterly, y = wdir_northerly), color = "black", size = 3, shape = 4) +
  labs(title = "K-means Clustering") +
  theme_minimal()
#---------------------------------
# Median Wind vec for 1 min every 10 min northerly HAVE A GRAPH THAT AGGREGATES WIND SPEED OVER 10 MIN???

# Select relevant columns
wind.data <- select(data, c("time", "wspd_vec_mean", "wdir_vec_mean", "wdir_northerly"))

# Northerly = 0 - 90 & 270 - 360 
# wind.data <- wind.data %>% mutate(northerly = if_else(.$wdir_vec_mean < 90 | .$wdir_vec_mean > 270, TRUE, FALSE))

# wind.data <- wind.data %>% filter(northerly == TRUE)

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

# Get averages over 10 min groups
avg.wspd.10min <- wind.data %>% group_by(hour) %>% summarise(avg.wspd = mean(wdir_northerly))

# Convert the interval column to POSIXct
#avg.wspd.10min$interval <- as.POSIXct(avg.wspd.10min$interval, format = "%Y-%m-%d %H:%M:%S")

# Find indices where the time is NA
#na_indices <- is.na(avg.wspd.10min$interval)

# Replace NA values with "2020-06-01 00:00:00"
#avg.wspd.10min$interval[na_indices] <- as.POSIXct("2020-06-01 00:00:00")

# Group rows by Hour
#avg.wspd.10min$hour <- format(avg.wspd.10min$interval, format = "%H")

# Get averages over hour groups
#avg.wspd.hour <- avg.wspd.10min %>% group_by(hour) %>% summarise(avg.wspd.10min = mean(avg.wspd))

plot(avg.wspd.10min)
ts_plot(avg.wspd.10min)
#-----------------------------------------
# Average Wind Speed Interquartile Range by hour for northerly

# Select relevant columns
wind.data <- select(data, c("time", "wspd_vec_mean", "wdir_vec_mean", "wdir_northerly"))

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

# Get average interquartile range
IQR.wspd <- wind.data %>% group_by(hour) %>% summarise(IQR.wspd = IQR(wdir_northerly))

ts_plot(IQR.wspd)
plot(IQR.wspd)
#------------------------------------------
# Average Wind Speed Range by hour for northerly

# Select relevant columns
wind.data <- select(data, c("time", "wspd_vec_mean", "wdir_vec_mean", "wdir_northerly"))

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

# Get average range
range.wspd <- wind.data %>% group_by(hour) %>% summarise(range.wspd = range(wdir_northerly)[2] - range(wdir_northerly)[1])

ts_plot(range.wspd)
plot(range.wspd)
 

