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
library(AER)
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
Data$Day_Treatment = factor(Data$Day_Treatment, levels = c("3", "6", "9",
                                                           "12", "15", "18",
                                                           "21","24", "27",
                                                           "30", "33", "36",
                                                           "39"))
Data$offsets = Data$Total_Counts -10
Data$unique_rep = interaction(Data$Bag,Data$Species, Data$Sub_Bag)
write.csv(Data,"H:\\WMRS\\! CENTRAL PROJ. FILES\\! Aquatics Research\\Federal Aid Fisheries Projects\\New Zealand Mud Snail\\Manuscript\\Mud_Snail_Survival_Count.csv", row.names = F)

# user functios and specs
percentile = c(0.05,0.95)# Percentiles of interest 
se =  function(x) sd(x)/(sqrt(length(x)))# calculates standard error 
overdisp_fun <- function(model) {
  rdf <- df.residual(model)
  rp <- residuals(model,type="pearson")
  Pearson.chisq <- sum(rp^2)
  prat <- Pearson.chisq/rdf
  pval <- pchisq(Pearson.chisq, df=rdf, lower.tail=FALSE)
  c(chisq=Pearson.chisq,ratio=prat,rdf=rdf,p=pval)
}  

#Visualize
Plot_data = Data %>% group_by(Species,Day_Treatment) %>%summarise(Mean = mean(Active_count),
                                                         se = se(Active_count))
Plot_data$percent = (Plot_data$Mean/10)*100 
Plot_data$percent_se = (Plot_data$se/10)*100     
jpeg("NZMS_EarthtecQZ_Survivorship_Fig.jpeg",width = 6.95, height = 4.89,units = 'in', res = 1080,bg = "white")
ggplot(data=Plot_data, aes(x=Day_Treatment, y=percent, group=Species, color =Species))+
  geom_line()+
  geom_point()+
  geom_errorbar(aes(ymin=percent-percent_se, ymax=percent+percent_se), width=.1) +
  scale_color_manual(name="Species",
                           breaks=c("Mud", "Spring", "Pond"),
                    labels=c("New Zealand Mud Snail", "Page Springsnail", "Pond Snail"),
                    values=c("#C8C8C8", "#686868", "#000000") )+
  scale_x_discrete(name ="Days of Treatment",
                     labels=c("3" = "3", "6" = "6",
                              "9" = "9", "12" = "12",
                              "15" = "15", "18" = "18",
                              "21" = "21", "24" = "24",
                              "27" = "27", "30" = "30",
                              "33" = "33","36" = "36",
                              "39" = "39"))+
  scale_y_continuous(name = "Mean Percent of Active Individuals with SE", limits=c(0,102), breaks = c(0,10,20,
                                                                                                               30,40,50,
                                                                                                               60,70,80,
                                                                                                               90,100))+
  theme_classic()+theme(legend.position = c(0.8, 0.8))
  
dev.off()
#summary of missing
Descriptive_stats = summarise(group_by(Data,Species,Day_Treatment),# applys following statistics by group 
                              Mean = mean(Active_count), 
                              N = length(Active_count),# number of observations
                              SD = sd(Active_count),# standard deviation 
                              SE = se(Active_count),# standard error 
                              Median = median(Active_count),
                              Fifth_percentile= quantile(Active_count, probs = percentile[1], type = 2),# type 2 corrisponds to the same estimation method used in sas
                              Ninety_Fifth_percentile= quantile(Active_count, probs = percentile[2], type = 2))# type 2 corrisponds to the same estimation method used in sas 
#model


model = glmmTMB(Active_count ~ Day_Treatment*Species + (1|Bag/unique_rep),
                data = Data,
                ziformula = ~ Species,
                family = genpois(link = "log"),
                na.action = na.omit, verbose = T,
                control = glmmTMBControl(optCtrl = list(iter.max = 15000,
                                                        eval.max = 15000)))

summary(model)
sim_res = simulateResiduals(model)
plot(sim_res , rank=T)
overdisp_fun(model) 

temp_emmeans = emmeans(model, ~Day_Treatment*Species)
Contrasts_out = data.frame(contrast(temp_emmeans, "pairwise", simple = "each", combine = T, adjust = "tukey"))

write.csv(Contrasts_out,"H:\\WMRS\\! CENTRAL PROJ. FILES\\! Aquatics Research\\Federal Aid Fisheries Projects\\New Zealand Mud Snail\\Data\\Pairwise_Comp_Output..txt", row.names = FALSE)
