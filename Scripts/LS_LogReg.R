# Stutzman
# Pitch pine regen project : large seedling code for logistic regression
#  2023-11-01

# libraries -------------------
library(dplyr)
library(tidyverse)
library(ggplot2)
library(moments)
# library(writexl)
# library(openxlsx)
# library(doBy)

# source functions -------------------

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
LS_Spring_Burn_Data.csv4$Treat_Type <- "SpringRx"
LS_Thinned_Data.csv5$Treat_Type <- "Harvest"

# merge into one data set -------------------

large_seedling <- rbind(LS_Control_Data.csv1, 
                        LS_Fall_Burn_Data.csv2, 
                        LS_Mow_Burn_Data.csv3,
                        LS_Spring_Burn_Data.csv4, 
                        LS_Thinned_Data.csv5)

# remove merged datasets to keep the envr clean -------------------

rm(LS_Control_Data.csv1, 
   LS_Fall_Burn_Data.csv2, 
   LS_Mow_Burn_Data.csv3, 
   LS_Spring_Burn_Data.csv4, 
   LS_Thinned_Data.csv5)



# change species groupings -------------------
# PIRI
# Other

large_seedling$Species_Groups <- NA

# PIRI
large_seedling$Species_Groups <- ifelse(large_seedling$Species_Code == 'PIRI', 'PIRI', large_seedling$Species_Groups)

# Other
large_seedling$Species_Groups <- ifelse(large_seedling$Species_Code %in% c('ACRU', 'QUIL', 'QUPR', 'QUCO', 'QUAL', 'QUVE', 'QURU', 'QUPA', 'SAAL', 'AMSP', 'BELE', 'BEPO', 'BENI', 'PODE', 'POGR', 'POTR', 'ACPE', 'FRAM', 'SAHU', 'FAGR', 'MASP', 'ACNE', 'CRSP', 'FRAL', 'ROPS', 'RHCA', 'ELUM', 'FRPE', 'PRSE', 'PRVI', 'PRPE', 'PIST', 'ABBA'), 'Other', large_seedling$Species_Groups)

# now to check that all species have been assigned and only NAs are NAs in new column -------------------

check_ls <- large_seedling %>% 
  filter(if_any(Species_Groups, is.na))


# select for just the information I want, can lose species code and latin name -------------------

large_seedling <- large_seedling %>% 
  select(Region,
         Treat_Type,
         Site,
         Plot_No,
         Species_Groups,
         Total,
         Browsed,
         StumpSprout)

#split treat type, site, region data off -------------------
large_seedling_meta <- large_seedling %>% 
  select(Region, Treat_Type, Site, Plot_No) %>% 
  distinct(Plot_No, .keep_all = TRUE)

large_seedling_plot <- large_seedling %>% 
  select(Plot_No, Species_Groups, Total, Browsed, StumpSprout)

# duplicates of "other" need to be combined into one row before using complete -------------------
large_seedling_plot1 <- large_seedling_plot %>% 
  group_by(Plot_No, Species_Groups) %>% 
  summarise(Total = sum(Total),
            Browsed = sum(Browsed),
            StumpSprout = sum(StumpSprout)) %>% 
  ungroup()

large_seedling_plot2 <- large_seedling_plot1 %>% 
  complete(Plot_No, Species_Groups, fill = list(Total = 0, Browsed = 0, StumpSprout = 0))

# get rid of NAs -------------------
large_seedlingNA <- large_seedling_plot2[complete.cases(large_seedling_plot2),]

# now to join it back with the meta data -------------------
ls_merge <- merge(large_seedling_meta, large_seedlingNA, by = 'Plot_No')


n_distinct(ls_merge$Plot_No)
# this yields 1001  plots

# remove dataframes no longer needed -------------------
rm(check_ls,
   large_seedling,
   large_seedling_meta,
   large_seedling_plot,
   large_seedling_plot1,
   large_seedling_plot2,
   large_seedlingNA)







# exploring total data using histogram -------------------
ggplot(ls_merge, aes(x=Total))+
  geom_histogram(binwidth = 2)+
  facet_grid(cols = vars(Species_Groups), rows = vars(Treat_Type))


# boxplots? -------------------

ggplot(ls_merge, aes(x = Species_Groups, y=Total))+
  geom_boxplot()+
  facet_wrap(vars(Treat_Type))

# lets look at some summary statistics -------------------

ls_sumstats2 <- ls_merge %>% 
  pivot_longer(c(Total, Browsed, StumpSprout), names_to = "Type") %>% 
  group_by(Treat_Type, Species_Groups, Type) %>% 
  summarize(average = round(mean(value), digits=1),
            med = median(value),
            sd = round(sd(value), digits = 1),
            var = round(var(value), digits = 1),
            min = min(value),
            max = max(value),
            .groups = "drop")


by(ls_merge, ls_merge$Treat_Type, summary)




# looking at harvest in MA -------------------

ls_maharvest <- ls_merge %>% 
  filter(Region ==  "MA",
         Treat_Type == "Harvest",
         Species_Groups == "PIRI",
         Site != "Mashpee_G3")

wilcox.test(Total~Site, data = ls_maharvest, paired = FALSE, conf.int = TRUE)


rm(ls_maharvest,
   ls_sumstats2)

# trying to transform the data adding a constant -------------------

ls_merge2 <- ls_merge

ls_merge2$Constant_Total <- (ls_merge2$Total + 1)

ls_merge2$sqrt_Total <- (sqrt(ls_merge2$Constant_Total))

ls_merge2$log_Total <- (log(ls_merge2$Constant_Total))

# log transforming appeared to work the best when analyzing the skew and kurtosis pre-test -------------------













