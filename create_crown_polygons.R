# create tree polygons based on the North-South and East-West radius and 
# diameter measurements, along with the azimuth angle of the N-S axis. 

library(dplyr)
library(NISTunits)
library(sp)


# set working directory
setwd("~/github/earthlab-gra/")

# read the crown measurement data 
crown_data_orig <- read.csv('data/MRS-04_crown-diameter-and-tree-height_2018-07-26.csv',
                       stringsAsFactors = FALSE)

# remove the "Notes" colum, keep only rows with complete data,
# and rename the "Tag_number" column to "id" 
crown_data <- crown_data_orig %>%
  select(-Notes) %>% 
  na.omit() %>% 
  rename(id = Tag_number)

# read the stem location data for absolute tree coordinates (center points)
# (currently we do not have this for Tom's plots, so I will 
# create some simulated tree locations for testing).
# This generates randomly selected integers for x and y coordinates
# ranging between 0 and the number of rows in crown_data plus 20,
# so there is some space between trees. Randomly assign ID numbers
# from the crown data to pair the stem and crown data. 
set.seed(42)
stems_x <- sample(x =nrow(crown_data) + 20,
                  size = nrow(crown_data))
stems_y <- sample(x =nrow(crown_data) + 20,
                  size = nrow(crown_data))
stems_id <- sample(crown_data$id,
                   size = nrow(crown_data),
                   replace = FALSE)

stem_coords <- data.frame(id = stems_id, 
                          x = stems_x,
                          y = stems_y)

plot(stem_coords$x, stem_coords$y,
     xlab = 'x coordinate',
     ylab = 'y coordinate')

# create a data frame to store crown points
crown_points_x <- numeric()
crown_points_y <- numeric()

# loop through tree entries in crown_data
for(i in 1:nrow(crown_data)){
  print(i)
  
  # use ID (tag number) to match stem + crown data istead of index 
  id <- crown_data$id[i]
  
  # center point 
  x <- stem_coords$x[stem_coords$id == id]
  y <- stem_coords$y[stem_coords$id == id]
  
  # N-S, E-W measurements
  ns_rad <- as.numeric(crown_data$N_radius[stem_coords$id == id])
  ns_diam <- as.numeric(crown_data$NS_diameter[stem_coords$id == id])
  ew_rad <- as.numeric(crown_data$E_radius[stem_coords$id == id])
  ew_diam <- as.numeric(crown_data$EW_diameter[stem_coords$id == id])
  
  # Azimuth angle of N-S radius relative to North 
  az <- crown_data$Azimuth_angle[stem_coords$id == id]
  print('azimuth angle: ')
  print(az)
  
  if(az >= 0 & az <= 90){
    print('Azimuth angle is greater than 0 and less than 90 degrees')
    
    # Calculate the angle (in radians) of the N-S radius relative to 
    # north (0 degrees) based on azimuth angle (degrees)
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
    
    # Calculate the angle (in radians) of the N-S radius relative to 
    # north (0 degrees) based on azimuth angle (degrees).
    # Subtract 270 to obtain the angle between the NS-axis and due West. 
    theta <- NISTunits::NISTdegTOradian(az - 270)
    
    # upper left. use NSradius
    x_UL <- x - (ns_rad * cos(theta))
    y_UL <- y + (ns_rad * sin(theta))
    
    # lower right. use NSdiam - NSradius. 
    x_LR <- x + ((ns_diam - ns_rad) * cos(theta))
    y_LR <- y - ((ns_diam - ns_rad) * sin(theta))
    
    # upper right 
    x_UR <- x + (ew_rad * sin(theta))
    y_UR <- y + (ew_rad * cos(theta))
    
    # lower left 
    x_LL <- x - ((ew_diam - ew_rad) * sin(theta))
    y_LL <- y - ((ew_diam - ew_rad) * cos(theta))
    
  } else{ 
    print('Invalid azimuth angle:')
    print('Must be greater than 0 and less than 90 degrees') 
    print('OR or greater than 270 and less than 360 degrees.')
    
    next 
  }
  
  # plot points
  if(i==1){
    plot(x,y, pch = 20, 
         xlim = c(0,max(stem_coords$x) + 5), 
         ylim = c(0, max(stem_coords$y) + 5),
         xlab = "x (m)",
         ylab = "y (m)")
  } else{
    points(x,y, col = "black", pch = 20)
  }
  points(x_UR, y_UR, col = "red", pch = 20, cex = 0.75)
  points(x_LL, y_LL, col = "orange", pch = 20, cex = 0.75)
  points(x_LR, y_LR, col = "green", pch = 20, cex = 0.75)
  points(x_UL, y_UL, col = "blue", pch = 20, cex = 0.75)
  
  # store points in data frame to experiment with polygons 
  crown_points_x <- rbind(crown_points_x, 
                          c(x_UR, x_LR, x_LL, x_UL))
  crown_points_y <- rbind(crown_points_y,
                          c(y_UR, y_LR, y_LL, y_UL))
  
  # create polygon 
  coords <- matrix(c(x_UR, y_UR,
                     x_LR, y_LR,
                     x_LL, y_LL,
                     x_UL, y_UL,
                     x_UR, y_UR),
                     ncol = 2, 
                     byrow = TRUE)
  p <- sp::Polygon(coords)
  p_sp <- sp::SpatialPolygons(list(sp::Polygons(list(p), ID = id)))
  
  plot(p_sp, add=TRUE)


}

# rename the columns of the x,y coordinate data frames 
colnames(crown_points_x) <- c("x_UR", "x_LR", "x_LL", "x_UL")
colnames(crown_points_y) <- c("y_UR", "y_LR", "y_LL", "y_UL")

# combine crown measurement coordinates into a single data frame
# to experiment with different types of polygon creation
crown_points <- cbind(id = crown_data$id, 
                      crown_points_x, 
                      crown_points_y)


