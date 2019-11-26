# Sets working Directory
setwd("H:\\WMRS\\! CENTRAL PROJ. FILES\\! Aquatics Research\\Federal Aid Fisheries Projects\\New Zealand Mud Snail\\Data")
options(scipen=999)
# Import required packages
library(readxl)
library(plyr)
library(ggplot2)
library(lme4)
library(glmmTMB)
library(DHARMa)
library(MuMIn)
library(emmeans)
library(tidyverse)
# Read and Manipulate
Data = data.frame(read_xlsx(path = "New_Zealand_Mud_Snail_DATA.xlsx"))
Data = subset(Data, select = -c(Raceway, date, Personnel, Comments))
colnames(Data)[9] = "Freshwater_Time"
Data$Sub_Bag = as.factor(Data$Sub_Bag) 
Data$Sub_Bag = revalue(Data$Sub_Bag, c("TAB"="1", "NO TAB"="2"))
Data$Species = revalue(Data$Species, c("PYRGS"="Spring", "PHYSA"="Pond", "NZMS"="Mud"))
Data$Bag = as.factor(Data$Bag)
Data$Bank = as.factor(Data$Bank)
Data$Species = as.factor(Data$Species)
Data = droplevels(Data[-which(Data$Bank %in% c('Control')),])
Data$Day_Treatment = as.factor(as.character(Data$Day_Treatment))
Data = droplevels(Data[-which(Data$Day_Treatment %in% c('1')),])
Data$Day_Treatment = factor(Data$Day_Treatment, levels = c("3", "6", "9", "12"))
Data$offsets = Data$Total_Counts -10
Data$unique_rep = interaction(Data$Bag,Data$Species, Data$Sub_Bag)

Plot_data = Data %>% group_by(Species,Day_Treatment) %>%summarise(Mean = mean(Active_count),
                                                                  se = se(Active_count))
  
  

#Visualize
png("weight_Partial_plot.png",width = 6.95, height = 4.89,units = 'in', res = 1080,bg = "white")
ggplot(data=Plot_data, aes(x=Day_Treatment, y=Mean, group=Species, color =Species))+
  geom_line()+
  geom_point()+
  geom_errorbar(aes(ymin=Mean-se, ymax=Mean+se), width=.1) +
  scale_color_manual(name="Species (Tukey Grouping)",
                           breaks=c("Mud", "Spring", "Pond"),
                    labels=c("Mud (A)", "Spring (B)", "Pond (C)"),
                    values=c("#C8C8C8", "#686868", "#000000") )+
  scale_x_discrete(name ="Days of Treatment",
                     labels=c("3" = "3 (A)", "6" = "6 (A)",
                              "9" = "9 (A)", "12" = "12 (B)"))+
  scale_y_continuous(name = "Mean Number of Live Individuals")+
  theme_classic() + ggtitle("Survivorship of Three Snail Species Exposed to EarthTec QZ")
dev.off()
#summary of missing
percentile = c(0.05,0.95)# Percentiles of interest 
se =  function(x) sd(x)/(sqrt(length(x)))# calculates standard error 
Descriptive_stats = summarise(group_by(Data,Species),# applys following statistics by group 
                              Mean = mean(Total_Counts), 
                              N = length(Total_Counts),# number of observations
                              SD = sd(Total_Counts),# standard deviation 
                              SE = se(Total_Counts),# standard error 
                              Median = median(Total_Counts),
                              Fifth_percentile= quantile(Total_Counts, probs = percentile[1], type = 2),# type 2 corrisponds to the same estimation method used in sas
                              Ninety_Fifth_percentile= quantile(Total_Counts, probs = percentile[2], type = 2))# type 2 corrisponds to the same estimation method used in sas 
#model
model = glmmTMB(Active_count ~ Day_Treatment + Species +(1|Bag/unique_rep),
                data = Data, 
                family = nbinom2(link = "log"),
                na.action = na.omit, verbose = T,
                control = glmmTMBControl(optCtrl = list(iter.max = 9000,
                                                        eval.max = 9000)))    
summary(model)
sim_res = simulateResiduals(model)
plot(sim_res , rank=T)

emmeans_day = emmeans(model,~Day_Treatment)
contrast(emmeans_day, method = "pairwise", adjust="tukey")
CLD(emmeans_day)
plot(emmeans_day, comparisons = T)

emmeans_species = emmeans(model,~Species)
contrast(emmeans_species, method = "pairwise", adjust="tukey")
CLD(emmeans_species)
plot(emmeans_species, comparisons = T)