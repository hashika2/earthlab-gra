# load required libraries
library(openxlsx)
library(dplyr)

# set working directory 
setwd("~/CU-Boulder/earthlab/")

# list files in directory Tom's field data
input_dir <- "Tom_Field_inventory/"
list_files <- list.files(path = input_dir,
                        pattern = "*.xlsx",
                        full.names = TRUE)

for (i in 1:length(list_files)){
  print(i)
  # current .xlsx filename
  current_file <- list_files[i]
  # get plot ID from filename 
  plt <- file_path_sans_ext(strsplit(current_file, "//")[[1]][2])
  # read current file
  tree_data <- read.xlsx(current_file,
                         na.strings = NA)
  # make column names lowercase for consistency 
  colnames(tree_data) <- tolower(colnames(tree_data))
  # count species, add column with plot ID, calculate % per species
  species <- as.data.frame(tree_data) %>% 
    select(sp.) %>% 
    group_by(sp.) %>% 
    summarise(count = n()) %>% 
    mutate(plot = plt, 
           percent = prop.table(count)*100) %>% 
    select(plot, sp., count, percent) # reorder columns 
  
  # merge into a single data frame
  if(i==1){
    species_all = species
  }
  else{
    species_all = rbind(species_all, species)
  }
}

# write to CSV 
write.csv(species_all, paste(input_dir,
                             "species_per_plot.csv",
                             sep=""))
