# Fooling around with analysis
# Pitch pine regen project
#  2023-11-28


# ******** let's take a look at the harvest category in MA, to see if any of the plots are significantly different. Only Wheelock & BP20 have piri present in SS -------------------
wheelock <- ss_merge %>% 
  filter(Site == "CE_Wheelock",
         Species_Groups == "PIRI")

bp20  <- ss_merge %>% 
  filter(Site == "CE_BP20",
         Species_Groups == "PIRI")
shapiro.test(wheelock$Total) #p = 0.000002
shapiro.test(bp20$Total) #p = 0.0000004
wilcox.test(wheelock$Total, bp20$Total, paired = FALSE) 
#p-value of 0.1234, so non-significant, v = 201.5
wilcox.test(bp20$Total, wheelock$Total, paired = FALSE) #same p, v= 327.5
wilcox.test(wheelock$Total, bp20$Total, paired = FALSE, conf.int = T)

rm(wheelock,
   bp20)


# trying to transform the data adding a constant -------------------
ss_merge2 <- ss_merge
ss_merge2$Constant_Total <- (ss_merge2$Total + 1)
ss_merge2$sqrt_Total <- (sqrt(ss_merge2$Constant_Total))
ss_merge2$log_Total <- (log(ss_merge2$Constant_Total))

# just run a straight up anova ------------------- maybe there is a way to have fixed and random effects within in ANOVA ....
 z <- aov(log_Total ~ Treat_Type + Site, data = ss_merge3)
 summary(z) # treat type and site both very significant
 
 par(mfrow=c(2,2))
plot(z)
                        
 shapiro.test(resid(z))
                        
z2 <- aov(Total ~ Treat_Type + Site, data = ss_merge3)
summary(z2) # treat type and site both very significant
par(mfrow=c(2,2))
plot(z2)
                        
z1 <- aov(log_Total ~ Treat_Type + Region + Site, data = ss_merge3)
summary(z1) # all significant
                        
rm(z,
z2,
z1) # residuals are not normally distributed, so can't use an ANOVA
                        
                        
                        
# keeping language to make plots -------------------
qqPlot(resid(mmodel1), id=F,
       main = 'Normal Q-Q Plot of Residuals')
                        
par(mfrow = c(1,2))
qqPlot(resid(mmodel1), id=F,
       main = 'Normal Q-Q Plot of Residuals')
plot(fitted(mmodel1), resid(mmodel1),
     ylim = c(-5,5),
     main = 'Residuals vs. Fitted')
abline(h=0, col = 'red')
                        
hist(resid(mmodel1))
                        
par(mfrow = c(1,1))
plot(ss_merge3$Treat_Type, resid(mmodel1))
plot(ss_merge3$Site, resid(mmodel1))
                        


 # looking at if treatment types small seedling totals fit a negative binomial distribution
library(fitdistrplus) #this masks select from dplyr
                        
fit <- fitdist(ss_merge3[ss_merge3$Treat_Type == "Control",]$Total, distr = "nbinom")
gofstat(fit) # not enough cells for p value
fit <- fitdist(ss_merge3[ss_merge3$Treat_Type == "Harvest",]$Total, distr = "nbinom")
gofstat(fit) # p = .5
 fit <- fitdist(ss_merge3[ss_merge3$Treat_Type == "SpringRx",]$Total, distr = "nbinom")
gofstat(fit) # p = .6
fit <- fitdist(ss_merge3[ss_merge3$Treat_Type == "FallRx",]$Total, distr = "nbinom")
gofstat(fit) # not enough cells for p value
fit <- fitdist(ss_merge3[ss_merge3$Treat_Type == "MowRx",]$Total, distr = "nbinom")
gofstat(fit) # not enough cells for p value



 #plotting residuals of model to validate
par(mfrow = c(1, 2))
plot(fitted(ss.nb_null), resid(ss.nb_null),
     ylim = c(-5,5),
     main = 'Residuals vs. Fitted')
abline(h=0, col = 'red')
                        
random_intercept <-  ranef(ss.nb_null)$Site[[1]]

hist(x=random_intercept,
     main = "Residuals of GLMM")
                        
                        
tibble(random_intercept = ranef(ss.nb_null)$Site[[1]]) %>% 
ggplot(aes(random_intercept))+
  geom_histogram(bins = 8)+
  labs(title = "Residuals of GLMM")


# post hoc comparison
summary(glht(ss.nb_null, lsm(pairwise ~ Treat_Type)), adjusted(type = "holm"))

summary(glht(ss.nb_plot, lsm(pairwise ~ Treat_Type)), adjusted(type = "holm"))
# this is also including Plot as a random effect - it increases the p-value here quite significantly, though there is still no significant difference between treatment types


summary(glht(ss.nb_plot2, lsm(pairwise ~ Treat_Type)), adjusted(type = "holm"))
# this is using Plot & region as a random effect - it increases the p-value here quite significantly, though there is still no significant difference between treatment types



# cleveland plots -------------------

dotchart(ss_PIRI$log_Total, 
         groups = factor(ss_PIRI$Treat_Type),
         pch=ss_PIRI$Treat_Type)


ss_pair <- ss_PIRI %>% 
  select(Region, Treat_Type, log_Total)

ggpairs(ss_pair)
#does not do what I want it to do














# SAPLING DATA ********* -------------------

# make the two species groups two columns -------------------
sapling1 <- sapling %>%
  group_by(Region,
           Treat_Type,
           Site,
           Plot_No,
           Species_Groups) %>%
  summarise(Spec_Total = sum(Tally)) %>%
  pivot_wider(names_from = Species_Groups, values_from = Spec_Total, values_fill = NA)

# re-order data
sapling1 <- sapling1 %>%
  select(Region,
         Treat_Type,
         Site,
         Plot_No,
         PIRI,
         Other)

# get plot total and expand to per HA basis (plots are 1/400 HA)
sapling3 <- sapling %>%
  group_by(Plot_No) %>%
  summarise(Plot_Total = sum(Tally),
            Total_Avg_HA = Plot_Total/0.0025)


# merge two sapling dataframes together
sa_merge1 <- merge(sapling1, sapling3, by = "Plot_No")


#find the proportion of PIRI for saplings
sa_merge2 <- sa_merge1 %>%
  group_by(Plot_No) %>%
  summarise(Prop_PIRI = round((PIRI/Plot_Total), digits = 2))

# merge this into sapling data
sa_merge3 <- merge(sa_merge1, sa_merge2, by = 'Plot_No')

# re-order and keep columns wanted
sa_merge3 <- sa_merge3 %>%
  select(Region,
         Treat_Type,
         Site,
         Plot_No,
         Plot_Total,
         Total_Avg_HA,
         Prop_PIRI)

# re-assign NAs as 0s
sa_merge3$Prop_PIRI <- replace(sa_merge3$Prop_PIRI, is.na(sa_merge3$Prop_PIRI),0)


# remove dataframes to keep envr neat -------------------
rm(check_ss,
   sa_merge1,
   sa_merge2,
   sapling,
   sapling1,
   sapling2)


# lets look at this in a histogram and boxplot -------------------

ggplot(sa_merge3, aes(x=Total_Avg_HA))+
  geom_histogram()+
  facet_wrap(~Treat_Type)


ggplot(sa_merge3, aes(y=Total_Avg_HA, x=Treat_Type))+
  geom_boxplot()







# LARGE SEEDLING DATA ************* -------------------

# ********  looking at harvest in MA -------------------

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



#plotting residuals of model to validate
par(mfrow = c(1, 2))
plot(fitted(ls.nb_null), resid(ls.nb_null),
     ylim = c(-5,5),
     main = 'Residuals vs. Fitted')
abline(h=0, col = 'red')

random_intercept <-  ranef(ls.nb_null)$Site[[1]]
hist(x=random_intercept,
     main = "Residuals of GLMM")
#residuals are not normally distributed

tibble(random_intercept = ranef(ls.nb_null)$Site[[1]]) %>% 
  ggplot(aes(random_intercept))+
  geom_histogram(bins = 8)+
  labs(title = "Residuals of GLMM")







