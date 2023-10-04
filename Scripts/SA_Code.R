# Stutzman
# Pitch Pine Regeneration Project : Sapling Code
#  2023-10-04

# libraries -------------------
library(dplyr)
library(tidyverse)
library(ggplot2)
library(writexl)
library(openxlsx)

# global variables -------------------

file_folder_SA <- "CleanData/sapling_CSV/"

# Body -------------------

file_names_SA <- list.files(path=file_folder_SA)

#for loop to read in the files, copied from Bio381 class notes/homework 12

for (i in seq_along(file_names_SA)) {
  assign(paste0(file_names_SA[i], i),
         read.csv(paste0(file_folder_SA, file_names_SA[i])))
}

# Adding treatment types -------------------

SA_Control_Data.csv1$Treat_Type <- "Control"
SA_Fall_Burn_Data.csv2$Treat_Type <- "FallRx"
SA_Mow_Burn_Data.csv3$Treat_Type <- "MowRx"
SA_No_Treatment_SPB.csv4$Treat_Type <- "SPB"
SA_Spring_Burn_Data.csv5$Treat_Type <- "SpringRx"
SA_Thinned_Data.csv6$Treat_Type <- "Harvest"

# merge into one data set -------------------

sapling <- rbind(SA_Control_Data.csv1, SA_Fall_Burn_Data.csv2, SA_Mow_Burn_Data.csv3, SA_No_Treatment_SPB.csv4, SA_Spring_Burn_Data.csv5, SA_Thinned_Data.csv6) 

# get mean abundance and frequency to look at species -------------------

x3 <- sapling %>% 
  group_by(Latin_Name) %>% 
  summarize(meanDBH = mean(DBH.cm.), number_of_occurrences=n())

# add new worksheet to existing file and save -------------------

wb1 <- loadWorkbook("/Users/user/Desktop/Data/Regen_RProj/DataObjects/regen_specieslist.xlsx")
addWorksheet(wb=wb1, sheetName = "Sapling")
writeData(wb=wb1, sheet="Sapling", x=x3, startCol = 1, startRow = 1)

saveWorkbook(wb=wb1, file="/Users/user/Desktop/Data/Regen_RProj/DataObjects/regen_specieslist.xlsx", overwrite = TRUE)


# thinking about the sapling data and I think i'll just go for occurence because the dbh sizing is confusing -------------------

sa3 <- sapling %>% 
  group_by(Latin_Name) %>% 
  summarize(SUM = sum(DBH.cm.), number_of_occurrences=n()) %>% 
  dplyr::mutate(T_Mean = number_of_occurrences/1065) %>%
  dplyr::mutate(Per_HA = T_Mean*400)


# now to make this an excel -------------------

wb2 <- loadWorkbook("/Users/user/Desktop/Data/Regen_RProj/DataObjects/specieslist.xlsx")
addWorksheet(wb=wb2, sheetName = "Sapling")
writeData(wb=wb2, sheet="Sapling", x=sa3, startCol = 1, startRow = 1)

saveWorkbook(wb=wb2, file="/Users/user/Desktop/Data/Regen_RProj/DataObjects/specieslist.xlsx", overwrite = TRUE)
















