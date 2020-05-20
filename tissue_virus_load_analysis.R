#Script for analysis for Figure 5C
# Virus load analysis of testes and ovaries compared to bodies

#Aims of this script:

# 1) Plot the virus loads (as determined by virus RNA relative to RpL-32)
# 2) perform a statistical analysis to see if sex tissues have higher virus load
# as compared to the whole body

library(tidyverse)
library(readxl)
library(ggpubr)

#set wd to where the file is saved
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#import the data
df_tissues <- read_excel("tissue_virus_load_table.xlsx")

#statistical analysis

#does the data follow normality assumptions
#do testing to know wilcoxon vs t test

#doesn't appear to be normalized, do wilcoxon
ggplot(df_tissues, aes(x = normalized)) + 
  geom_histogram() +
  facet_wrap(~tissue)
library(car)
qqp(df_tissues$normalized)
ggdensity(df_tissues$normalized)


#Plot each type by the respective sex
#do wilcoxon test
#showing it on a plot to be able to use tidyverse (ggplot2 with ggpubr)

#separate each plot by sex
#not comparing across sexes, only within sexes

#pull out male tissue data
df_tissues_m <- df_tissues %>% 
                filter(sex == 'male')

theme_set(theme_classic())
p_tissue_m <- ggplot(df_tissues_m, aes(tissue, normalized)) +
    geom_boxplot(aes(fill = factor(tissue))) +
    labs(title = "Galbut Virus RNA Levels in Male Tissues",
         y = "Virus RNA levels relative to RpL-32 (2^-deltaCt)",
         x = "Tissue Type") +
    scale_fill_manual(values=c("#56B4E9", "#D55E00")) +
    theme(legend.position = "none",
          plot.title = element_text(hjust = 0.5)) +
    scale_y_log10() +
    stat_compare_means() #this naturally chose to do Wilcoxon
p_tissue_m #if you want to view the plot

#separate out the female tissue data
df_tissues_f <- df_tissues %>% 
    filter(sex == 'female')  

p_tissue_f <- ggplot(df_tissues_f, aes(tissue, normalized)) +
  geom_boxplot(aes(fill = factor(tissue))) +
  labs(title = "Galbut Virus RNA Levels in Female Tissues",
       y = "Virus RNA levels relative to RpL-32 (2^-deltaCt)",
       x = "Tissue Type") +
  scale_fill_manual(values=c("#56B4E9", "#D55E00")) +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5)) +
  scale_y_log10() +
  stat_compare_means()

p_tissue_f #if you want to visualize the plot here    

#combine the plots
library(patchwork)
p_tissue_f | p_tissue_m

#save it as a PDF
ggsave("tissue_virus_load.pdf")

#This PDF was then opened with Affinity designer for aesthetics and sizing
