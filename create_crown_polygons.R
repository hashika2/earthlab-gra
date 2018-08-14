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
# create some simulated tree location coordinats for testing) 







### SINGLE TREE POLYGON

# center point 
x <- 5 
y <- 10 
center <- c(x, y)
plot(x,y)

# NS measurements
ns_rad <- 1.1
ns_diam <- 2.9

# Azimuth angle of N-S radius relative to North 
az <- 30


# Calculate the angle (in radians) of the N-S radius based on azimuth

if(az >= 0 & az <= 90){
  print('Azimuth angle is greater than 0 and less than 90 degrees')
  theta <- NISTunits::NISTdegTOradian(az)
  
} else if(az >= 270 & az < 360){
  print('Azimuth angle is greater than 270 and less than 360 degrees')
  theta <- NISTunits::NISTdegTOradian(360 - az)
  
} else{ 
  print('Invalid azimuth angle:')
  print('Must be greater than 0 and less than 90 degrees') 
  print('OR or greater than 270 and less than 360 degrees.')
  }

# calculate the x and y coordinates of the upper right polygon corner
x_UR <- x + ns_rad * sin(theta)
y_UR <- y + ns_rad * cos(theta)
x_LL <- x - ((ns_diam - ns_rad) * sin(theta))
y_LL <- y - ((ns_diam - ns_rad) * cos(theta))

plot(x,y)
points(x_UR, y_UR)
points(x_LL, y_LL)


