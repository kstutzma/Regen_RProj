# Stutzman
# Pitch Pine Regeneration Project : Large Seedling code
#  2023-10-04

# libraries -------------------
library(dplyr)
library(tidyverse)
library(ggplot2)
library(writexl)
library(openxlsx)

# global variables -------------------

file_folder_LS <- "CleanData/largeseedling_CSV/"

# Body -------------------

file_names_LS <- list.files(path=file_folder_LS)

#for loop to read in the files, copied from Bio381 class notes/homework 12

for (i in seq_along(file_names_LS)) {
  assign(paste0(file_names_LS[i], i),
         read.csv(paste0(file_folder_LS, file_names_LS[i])))
}

# Adding treatment types -------------------

LS_Control_Data.csv1$Treat_Type <- "Control"
LS_Fall_Burn_Data.csv2$Treat_Type <- "FallRx"
LS_Mow_Burn_Data.csv3$Treat_Type <- "MowRx"
LS_No_Treatment_SPB.csv4$Treat_Type <- "SPB"
LS_Spring_Burn_Data.csv5$Treat_Type <- "SpringRx"
LS_Thinned_Data.csv6$Treat_Type <- "Harvest"

# merge into one data set -------------------

large_seedling <- rbind(LS_Control_Data.csv1, LS_Fall_Burn_Data.csv2, LS_Mow_Burn_Data.csv3, LS_No_Treatment_SPB.csv4, LS_Spring_Burn_Data.csv5, LS_Thinned_Data.csv6) 

# get mean abundance and frequency to look at species -------------------

x2 <- large_seedling %>% 
  group_by(Latin_Name) %>% 
  summarize(meanAbundance = mean(Total), number_of_occurrences=n())

# add new worksheet to existing file and save -------------------

wb1 <- loadWorkbook("/Users/user/Desktop/Data/Regen_RProj/DataObjects/regen_specieslist.xlsx")
addWorksheet(wb=wb1, sheetName = "LargeSeedling")
writeData(wb=wb1, sheet="LargeSeedling", x=x2, startCol = 1, startRow = 1)

saveWorkbook(wb=wb1, file="/Users/user/Desktop/Data/Regen_RProj/DataObjects/regen_specieslist.xlsx", overwrite = TRUE)


# now trying the gross totals for this -------------------

ls3 <- large_seedling %>% 
  group_by(Latin_Name) %>% 
  summarize(SUM = sum(Total), number_of_occurrences=n()) %>% 
  dplyr::mutate(T_Mean = SUM/1065) %>% 
  dplyr::mutate(Per_HA = T_Mean*1000)


# now to make this an excel -------------------

wb2 <- loadWorkbook("/Users/user/Desktop/Data/Regen_RProj/DataObjects/specieslist.xlsx")
addWorksheet(wb=wb2, sheetName = "LargeSeedling")
writeData(wb=wb2, sheet="LargeSeedling", x=ls3, startCol = 1, startRow = 1)

saveWorkbook(wb=wb2, file="/Users/user/Desktop/Data/Regen_RProj/DataObjects/specieslist.xlsx", overwrite = TRUE)



