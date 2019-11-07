#### Read data and combine the two file types ####
#Load Packages
library(dplyr)
library(tidyverse)
library(readxl)
library(openxlsx)
#List all of the folder in the directory
folders <- list.files()
files <- setNames(data.frame(matrix(ncol = 3, nrow = 0)), c("Folder", "Plant Info", "Compound Info"))
#loop through each folder storing the file names for each
for (i in 0:length(folders)) {
  files[i,1] <- folders[i] 
  files[i,2] <- list.files(path = folders[i], pattern = "plant", full.names = TRUE)
  files[i,3] <- list.files(path = folders[i], pattern = "compound", full.names = TRUE)
   }
# define function to read in the data and join as we want
merge_compound_plant <- function(compound_info, plant_info)
 return(left_join(x = compound_info, y = plant_info, by = c("Plant Name","Location","Plant part")))

#loop function over the files data frame and write the results to a new xlsx
for (i in 1:nrow(files)) {
  x <- read_xlsx(paste(files[i,3]))
  y <- read_xlsx(paste(files[i,2]))
  join <- merge_compound_plant(x,y) #Should I add here that we will only keep columns that we want
  new_folder <- paste("C:/Users/Ewan1/Desktop/Project_combined_data/")
  write.xlsx(join,path = new_folder, file = paste("/Users/Ewan1/Desktop/Project_combined_data/",files[i,1],"_combined.xlsx", sep =""))
}
#### Check that the new combined files are what we expect ####
#Loop through the combined files checking that their dimensions are what we expect and are formatted in the correct way
#Store those who's dimensions aren't what we expect in a new XLSX called aberrant_files for manual inspection and restoration to preferred format

allowed_names <- c( "Plant.Name", "Location", "Compound","Percentage", "Identification/.Techniques", "Plant.part",
                    "Experimental.Condition","Group.x", "Date.of.Collection","Title","Author", "Journal.Name",              
                    "Family",  "Group.y" )
aberrant_files <- setNames(data.frame(matrix(ncol = 7, nrow = 0)), c("File", "ncol", "name1", "name2", "name3","name4","name5"))
combined_files <- list.files("/Users/Ewan1/Desktop/Project_combined_data/", pattern = "combined")

for (i in 1:length(combined_files)) {
  file_read <- read.xlsx(paste("/Users/Ewan1/Desktop/Project_combined_data/",combined_files[i],sep=""))
  if (ncol(file_read) != 14) {
  aberrant_files[i,1] <- combined_files[i]
  aberrant_files[i,2] <- ncol(file_read)
  n_aberrant_names <- length(setdiff(colnames(file_read), allowed_names))
  aberrant_files[i,3:(2+n_aberrant_names)] <- setdiff(colnames(file_read), allowed_names)}
  else if ((length(setdiff(colnames(file_read), allowed_names))) > 0) {       #This should be changed to show that the order also matches
  aberrant_files[i,1] <- combined_files[i]
  aberrant_files[i,2] <- ncol(file_read)
  n_aberrant_names <- length(setdiff(colnames(file_read), allowed_names))
  aberrant_files[i,3:(2+n_aberrant_names)] <- setdiff(colnames(file_read), allowed_names)}
  else {
    if (identical(colnames(file_read), allowed_names) == FALSE) {
    aberrant_files[i,1] <- combined_files[i]
    aberrant_files[i,2] <- ncol(file_read)
    aberrant_files[i,3] <- "WRONG ORDER"}}
}
#Filter out the rows that only contain NA values as these represent non aberrant files
#Write the aberrant files to a new xlsx
aberrant_files <- aberrant_files %>%  filter(!is.na(aberrant_files$File))
write.xlsx(aberrant_files, file = "/Users/Ewan1/Desktop/Project_combined_data/aberrant_files.xlsx")

#This code can then be looped through after manual restoration of the combined files until the aberrant_files file is empty

