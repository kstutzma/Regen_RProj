# Stutzman
# Pitch Pine Regen Project : Small seedling code
#  2023-10-04

# libraries -------------------
library(dplyr)
library(tidyverse)
library(ggplot2)
library(writexl)

# global variables -------------------

file_folder <- "CleanData/smallseedling_CSV/"

# Body -------------------

file_names <- list.files(path=file_folder)

#for loop to read in the files, copied from Bio381 class notes/homework 12

for (i in seq_along(file_names)) {
  assign(paste0(file_names[i], i),
         read.csv(paste0(file_folder, file_names[i])))
  }

# Adding treatment types -------------------

SS_Control_Data.csv1$Treat_Type <- "Control"
SS_Fall_Burn_Data.csv2$Treat_Type <- "FallRx"
SS_Mow_Burn_Data.csv3$Treat_Type <- "MowRx"
SS_No_Treatment_SPB.csv4$Treat_Type <- "SPB"
SS_Spring_Burn_Data.csv5$Treat_Type <- "SpringRx"
SS_Thinned_Data.csv6$Treat_Type <- "Harvest"

# merge into one data set -------------------

small_seedling <- rbind(SS_Control_Data.csv1, SS_Fall_Burn_Data.csv2, SS_Mow_Burn_Data.csv3, SS_No_Treatment_SPB.csv4, SS_Spring_Burn_Data.csv5, SS_Thinned_Data.csv6) 

# get mean abundance and frequency to look at species -------------------

x1 <- small_seedling %>% 
  group_by(Latin_Name) %>% 
  summarize(meanAbundance = mean(Total), number_of_occurrences=n())

# print to excel file -------------------

write_xlsx(x1, "/Users/user/Desktop/Data/Regen_RProj/DataObjects/regen_specieslist.xlsx")


# now thinking about what is the true mean -------------------

small_seedling2 <- small_seedling %>% 
  group_by(Treat_Type) %>% 
  group_by(Site) %>% 
  dplyr::mutate(No_of_Plots = n_distinct(Plot_No)) %>% 
  dplyr::mutate(T_Mean = Total/No_of_Plots)

# how to get a true mean (aka total / no of plots) for each site? 

z1 <- small_seedling2 %>% 
  group_by(Latin_Name) %>% 
  summarise(meanAbun = mean(T_Mean), no_occur = n())

z2 <- small_seedling2 %>% 
  group_by(Latin_Name) %>% 
  group_by(Treat_Type) %>% 
  summarise(meanAbun = mean(T_Mean), no_occur = n())


# adding this data to the workbook -------------------

wb1 <- loadWorkbook("/Users/user/Desktop/Data/Regen_RProj/DataObjects/regen_specieslist.xlsx")
addWorksheet(wb=wb1, sheetName = "SS_total/no_plots")
writeData(wb=wb1, sheet="SS_total/no_plots", x=z1, startCol = 1, startRow = 1)

addWorksheet(wb=wb1, sheetName = "SS_total/treatment")
writeData(wb=wb1, sheet="SS_total/treatment", x=z2, startCol = 1, startRow = 1)


saveWorkbook(wb=wb1, file="/Users/user/Desktop/Data/Regen_RProj/DataObjects/regen_specieslist.xlsx", overwrite = TRUE)


# just want to see if I can get a total for the number of plots overall taken -------------------



nototalplots <- small_seedling %>% 
  dplyr::mutate(pl = n_distinct(Plot_No)) 

#I took 1,065 plots


# thinking what if i get a total and divide by overall number of plots? something about weighting is missing but I'm not quite sure what. it's a gross estimate but might be useful???





ss3 <- small_seedling %>% 
  group_by(Latin_Name) %>% 
  summarize(SUM = sum(Total), number_of_occurrences=n()) %>% 
  dplyr::mutate(T_Mean = SUM/1065) %>% 
  dplyr::mutate(Per_HA = T_Mean*10000)


# now to make this an excel -------------------

write_xlsx(ss3, "/Users/user/Desktop/Data/Regen_RProj/DataObjects/specieslist.xlsx")



  
  
  