#### Name Normalisation ####
#This code can deal with synonyms, orthographic errors and standardisation however it will class a name as aberrant if it 
# a) cannot find a match in the plant list database b) the error is both a synonym and an orthographic one
#Load Necessary Packages
library(dplyr)
library(tidyverse)
library(Taxonstand)
library(taxize)
library(openxlsx)

#Read in desired data and extract names

data <- read.xlsx("v30_combined.xlsx")
names <- data %>% pull(1) %>% unique()  #Must use unique values otherwise Taxonstand takes too long

#Apply the taxonstand function TPL to every name in this list and store the results as a data frame names_results

names_results <- sapply(names, TPL) %>%  t() %>% data.frame()

#From names_results pull out aberrant names that Taxonstand could not resolve and write them to an XLSX file for manual inspection
aberrant_names <- names_results %>% filter(trimws(ID) == "" & Plant.Name.Index == FALSE)
write.xlsx() #Add line here to write the aberrant names to an xlsx


#Select the names_results where the Taxonstand suggested name != The original name
#This should pull out all syonyms orthographic errors and general corrections 
names_change <- names_results %>% filter(Plant.Name.Index == TRUE & trimws(ID) != "") %>% 
                                  mutate(New.Taxon = paste(New.Genus,New.Species,New.Infraspecific.rank,New.Infraspecific)) %>% 
                                  filter(New.Taxon != Taxon)

#Script to then use names_change to convert the names in data automatically
names_to_change <- unlist(names_change$Taxon)
for (i in names_to_change) {
  data$Plant.Name[data$Plant.Name == i] <- names_change[(which(names_change == i)),27]}
  
#Script to ask to manually inspect each change and decide if it should go ahead
names_to_change <- unlist(names_change$Taxon)
for (i in names_to_change) {
  old_name <- i
  new_name <- names_change[(which(names_change == i)),27]
  if (names_change[(which(names_change == i)),22] == TRUE) {
    reason <- "Typo"} 
  else if (names_change[(which(names_change == i)),12] == "Synonym") {
    reason <- "Synonym"}
  else
    reason <- "unknown"
  m <- askYesNo(msg = paste("Do you want to change", old_name, "for", new_name,"?","Reason =", reason), prompts = c("y", "n", "c")) 
  if (m == TRUE) {
  data$Plant.Name[data$Plant.Name == i] <- new_name}
  }


#Need to then add a line which overwrites the initial data with the new data
