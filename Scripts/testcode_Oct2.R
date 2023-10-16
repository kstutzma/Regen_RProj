# Stutman
# PP Regen Proj : Import test
#  2023-10-02

#Going to try to write code to import all the CSVs and maybe combine them? Should this be a function?

ss_data <- read.table(file="smallseedling_CSV/Control_Data.csv",
                      header = T,
                      sep = ";")

#ok, issues were that excel uses a ; instead of a ,

# now to write a function to upload all contents of this folder. I think I should then add their names so that I know the treatment type and maybe also state/location so I'll be able to sort/group them


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

#ok, this reads in all the files, now to work on adding details to them

# Adding treatment types -------------------

Control_Data.csv1$Treat_Type <- "Control"
Fall_Burn_Data.csv2$Treat_Type <- "FallRx"
Fall_Mow_Burn_Data.csv3$Treat_Type <- "MowRx_F"
No_Treatment_SPB.csv4$Treat_Type <- "SPB"
Spring_Burn_Data.csv5$Treat_Type <- "SpringRx"
Spring_Mow_Burn_Data.csv6$Treat_Type <- "MowRx_Sp"
Summer_Mow_Burn_Data.csv7$Treat_Type <- "MowRx_Su"
Thinned_Data.csv8$Treat_Type <- "Harvest"

# lets start by exploring just one data set ----

Summary_SS_Control <- Control_Data.csv1 %>% 
  group_by(Region) %>% 
  group_by(Latin_Name) %>% 
  summarize(meanAbundance = mean(Total.), number=n())

ggplot(Control_Data.csv1, aes(x=Latin_Name, y=Total.)) +
  geom_boxplot()+
  guides(x=guide_axis(angle = 75))


# 5 number summary just of total

SS_Total <- select(Control_Data.csv1, Site:Total.)

bp1 <- SS_Total %>% 
  group_by(Region) %>% 
  group_by(Latin_Name)
  #%>% 
  # summarize(meanAbundance = mean(Total.), number=n())

by(bp1, bp1$Latin_Name, summary)

ggplot(bp1, aes(x=Latin_Name, y=Total.)) +
  geom_boxplot()+
  guides(x=guide_axis(angle = 75))

# ok, above is some rough totals, I think I want to look at this by region



# merge into one data set -------------------

small_seedling <- rbind(Control_Data.csv1, Fall_Burn_Data.csv2, Fall_Mow_Burn_Data.csv3, No_Treatment_SPB.csv4, Spring_Burn_Data.csv5, Spring_Mow_Burn_Data.csv6, Summer_Mow_Burn_Data.csv7, Thinned_Data.csv8) 

x1 <- small_seedling %>% 
  group_by(Latin_Name) %>% 
  summarize(meanAbundance = mean(Total.), number_of_occurrences=n())

write_xlsx(x1, "/Users/user/Desktop/Data/Regen_RProj/DataObjects/smallseedling_specieslist.xlsx")

# now subset a region -------------------

MA_SS <- filter(small_seedling, Region == "MA")

# find # of unique plots for each site -------------------

MA_SS2 <- MA_SS %>% 
  group_by(Treat_Type) %>% 
  group_by(Site) %>% 
  dplyr::mutate(No_of_Plots = n_distinct(Plot.))

# now pulling out just one species -------------------

PIRI_MA <- MA_SS2 %>% 
  filter(Species_Code == "PIRI")

ggplot(PIRI_MA) +
  aes(x=Treat_Type, y=Total., fill=Site) +
  geom_boxplot()+
  guides(x=guide_axis(angle = 45))+
  theme(legend.position = "bottom")

# now look at PIRI across all regions -------------------

small_seedling2 <-  small_seedling %>% 
  group_by(Treat_Type) %>% 
  group_by(Site) %>% 
  dplyr::mutate(No_of_Plots = n_distinct(Plot.))

SS_PIRI <- small_seedling2 %>% 
  filter(Species_Code=="PIRI")

ggplot(SS_PIRI) +
  aes(x=Treat_Type, y=Total., fill=Site) +
  geom_boxplot()+
  facet_grid(Region~.)+
  guides(x=guide_axis(angle = 45))+
  theme(legend.position = "none")

by(SS_PIRI, SS_PIRI$Treat_Type, summary)

SS_PIRI2 <- SS_PIRI %>% 
  select(Site, Region, Species_Code, Latin_Name, Total., Treat_Type, No_of_Plots)

by(SS_PIRI2, SS_PIRI$Treat_Type, summary)

# summary stats -------------------

str(small_seedling)

by(small_seedling, small_seedling$Latin.Name, summary)

small_seedling %>%
  group_by(Treat_Type) %>% 
  group_by(Region) %>% 
  group_by(Latin.Name) %>% 
  summarize(meanAbundance = mean(Total..), number=n())


# moving code here that was used for early totals/proportions to condense species categories -------------------

# get mean abundance and frequency to look at species -------------------

x1 <- small_seedling %>% 
  group_by(Latin_Name) %>% 
  summarize(meanAbundance = mean(Total), number_of_occurrences=n())


# print to excel file -------------------

write_xlsx(x1, "/Users/user/Desktop/Data/Regen_RProj/DataObjects/regen_specieslist.xlsx")


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





# Adding locations for later ease of grouping NONE OF THESE WORKED -------------------
# 
# Control_Data.csv1$Region <- Control_Data.csv1$Plot..
# 
# Control_Data.csv1 %>% 
#   mutate(Region = case_when(Plot.. >=88 & Plot..<=159 <-  "LI",
#                             Plot..>=391 & Plot..<=641 <-  "MA",
#                             Plot..>=642 & Plot..<=681 <-  "ALB",
#                             Plot..>=912 & Plot..<=1064 <-  "NH",
#                             Plot..>=1074 & Plot..>=1119 <-  "ME"))
# 
# 
# Control_Data.csv1$Region[[Control_Data.csv1$Plot..>=88]] <- "LI"
# Control_Data.csv1$Region[Control_Data.csv1$Plot.. >=88 & <=159] <- "LI"
# Control_Data.csv1$Region[Control_Data.csv1$Site=="RPD2_B7" ] <- "LI"
# Control_Data.csv1$Region[Control_Data.csv1$Site=="APB_Hippo"] <- "ALB"
# Control_Data.csv1$Region[Control_Data.csv1$Sit=="APB_Hydro"  ] <- "ALB"
# 
# #RPD2_7 | RPD2_B3
# 
# 
# Control_Data.csv1$Region <- ifelse(Control_Data.csv1$Plot..>=88 &  Control_Data.csv1$Plot..<=159, "LI", 
#                                    Control_Data.csv1$Plot..>=391 &  Control_Data.csv1$Plot..<=641, "MA")
#                                    # Control_Data.csv1$Plot..>=642 &  Control_Data.csv1$Plot..<=681, "ALB",
#                                    # Control_Data.csv1$Plot..>=912 &  Control_Data.csv1$Plot..<=1064, "NH",
#                                    # Control_Data.csv1$Plot..>=1074 &  Control_Data.csv1$Plot..<=1119, "LI")
# 
# 
# Control_Data.csv1$Region %>% mutate(
#   Control_Data.csv1$Region2=case_when(
#   Control_Data.csv1$Region <= 1119 ~ "ME",
#   Control_Data.csv1$Region <= 1064 ~ "NH",
#   Control_Data.csv1$Region <= 681 ~ "ALB",
#   Control_Data.csv1$Region <= 641 ~ "MA",
#   Control_Data.csv1$Region <= 159 ~ "LI",
#   TRUE~NA_real_
#   )
# )
# 
# # if(Control_Data.csv1$Region <= 1119) {
# #   "ME"
# # } else if (Control_Data.csv1$Region <= 1064) {
# #     "NH"
# # } else {Control_Data.csv1$Region <= 681} {
# #     "ALB"
# # }
#   
# # } else if (Control_Data.csv1$Region <= 642) {
# #     "MA"
# # } else (Control_Data.csv1$Region <= 159) {
# #     "LI"
# #   }
# 
# 
# 
# regiontest <- function(p=Control_Data.csv1$Region) {
#   if (p>=88|p<=159)
#     regiontest2 <- "LI"
#   return(regiontest2)
# }







