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


#Visualize
plots = ggplot(data=Data, aes(x=Day_Treatment, y=Active_count, col=Bag ,group=Species)) 
plots + stat_summary(fun.data = "mean_se", aes(color=paste("mean", Species))) + theme_classic()

#model
model = glmmTMB(Active_count ~ Day_Treatment + Species + offset(Total_Counts)+(1|Bag/Sub_Bag),
                data = Data, 
                family = nbinom2(link = "log"),
                na.action = na.omit, verbose = T,
                control = glmmTMBControl(optCtrl = list(iter.max = 4000,
                                                        eval.max = 4000)))    
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
