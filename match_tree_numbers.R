# match the tree ID number from our 4-point crown measurements with 
# permanent plot data up at Niwot Ridge to determine how many species are 
# represented with these crown measurements. 

library(dplyr)
library(knitr)


# set working directory
setwd("~/github/earthlab-gra/")


# read the crown measurement data 
crown_data_orig <- read.csv('data/MRS-04_crown-diameter-and-tree-height_2018-10-05.csv',
                            stringsAsFactors = FALSE)

# remove the "Notes" colum, keep only rows with complete data,
# and rename the "Tag_number" column to "id" 
crown_data <- crown_data_orig %>%
  select(-Notes) %>% 
  na.omit() %>% 
  rename(tree_number = Tag_number)

# read the permanent plot tree data, which contains ID number in a column
# called "tree_number#" and species codes in the "species" column 
pp_data <- read.csv('data/Veblen_LTER_PP_Data_03262018_MRS04.csv',
                    stringsAsFactors = FALSE)

# subset the permanent plot data, keep only rows with tree_number values that
# appear in the crown measurement data set
pp_subset <- pp_data %>% subset(tree_number %in% crown_data$tree_number)

# summarise the number of species present in the crown data subset
pp_subset_species_count <- pp_subset %>% count(species)

# add on a column with the scientific and common names of each species
# for easier interpretation
species_names <- data.frame(codes = c('ABLA','PIEN',
                                      'PICO', 'PIFL'),
                               common_names = c('Subalpine fir', 
                                                 'Engelmann spruce', 
                                                 'Lodgepole pine', 
                                                 'Limber pine'),
                               sci_names = c('Abies lasiocarpa',
                                             'Picea engelmannii',
                                             'Pinus contorta',
                                             'Pinus flexilis'))

# display a table with the count of crowns measured per species
pp_subset_summary <- merge(pp_subset_species_count, species_names, 
                         by.x = "species", by.y = "codes") 

knitr::kable(pp_subset_summary)
head(pp_subset_summary)



