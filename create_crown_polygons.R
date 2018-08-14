# create tree polygons based on the North-South and East-West radius and 
# diameter measurements, along with the azimuth angle of the N-S axis. 

library(dplyr)
library(NISTunits)


# set working directory
setwd("~/github/earthlab-gra/")

# read the crown measurement data 
crown_data_orig <- read.csv('data/MRS-04_crown-diameter-and-tree-height_2018-07-26.csv',
                       stringsAsFactors = FALSE)

# remove the "Notes" colum and keep only rows with complete data
crown_data <- crown_data_orig %>%
  select(-Notes) %>% 
  na.omit()

# read the stem location data for absolute tree coordinates (center points)
# (currently we do not have this for Tom's plots, so I will 
# create some simulated tree locations for testing) 







### SINGLE TREE POLYGON

# index or row in crown data
i = 2

# center point 
x <- 5 
y <- 10 

# N-S, E-W measurements
ns_rad <- as.numeric(crown_data$N_radius[i])
ns_diam <- as.numeric(crown_data$NS_diameter[i])
ew_rad <- as.numeric(crown_data$E_radius[i])
ew_diam <- as.numeric(crown_data$EW_diameter[i])

print(ns_rad)
print(ns_diam)
print(ew_rad)
print(ew_diam)

# Azimuth angle of N-S radius relative to North 
az <- 80


# Calculate the angle (in radians) of the N-S radius based on azimuth

if(az >= 0 & az <= 90){
  print('Azimuth angle is greater than 0 and less than 90 degrees')
  theta <- NISTunits::NISTdegTOradian(az)
  
  # upper right
  x_UR <- x + (ns_rad * sin(theta))
  y_UR <- y + (ns_rad * cos(theta))
  
  # lower left 
  x_LL <- x - ((ns_diam - ns_rad) * sin(theta))
  y_LL <- y - ((ns_diam - ns_rad) * cos(theta))
  
  # lower right 
  x_LR <- x + (ew_rad * cos(theta))
  y_LR <- y - (ew_rad * sin(theta))
  
  # upper left 
  x_UL <- x - ((ew_diam - ew_rad) * cos(theta))
  y_UL <- y + ((ew_diam - ew_rad) * sin(theta))
  
} else if(az >= 270 & az < 360){
  print('Azimuth angle is greater than 270 and less than 360 degrees')
  theta <- NISTunits::NISTdegTOradian(360 - az)
  
  # upper left 
  
  # lower right 
  
  # upper right 
  
  # lower left 
  
} else{ 
  print('Invalid azimuth angle:')
  print('Must be greater than 0 and less than 90 degrees') 
  print('OR or greater than 270 and less than 360 degrees.')
  
  next 
  }


  
    
# plot points
plot(x,y, xlim = c(-5,15), ylim = c(0, 20))
points(x_UR, y_UR, col = "red")
points(x_LL, y_LL, col = "orange")
points(x_LR, y_LR, col = "green")
points(x_UL, y_UL, col = "blue")

# create polygon 


