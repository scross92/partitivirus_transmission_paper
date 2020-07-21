#Script for analysis for Figure 5C and D
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
#calculate mean and median of tissues for finding fold change difference
df_tissues_m %>% group_by(tissue) %>% summarise(mean = mean(normalized), median = median(normalized))

#add in the comparisons you want to make for stats
my_comparisons_m <- list( c("body", "midgut"), c("body", "testes"), c("midgut", "testes") )

theme_set(theme_classic())
p_tissue_m <- ggplot(df_tissues_m, aes(tissue, normalized)) +
    geom_boxplot(aes(fill = factor(tissue))) +
    labs(title = "Galbut Virus RNA Levels in Male Tissues",
         y = "Galbut virus RNA levels relative to RpL-32 (2^-deltaCt)",
         x = "Tissue Type") +
    scale_fill_manual(values=c("grey", "#009E73", "#D55E00")) +
    theme(legend.position = "none",
          plot.title = element_text(hjust = 0.5)) +
    scale_y_continuous() +
    stat_compare_means(comparisons = my_comparisons_m)  #this naturally chose to do Wilcoxon
    #stat_compare_means(label.y = 50)
p_tissue_m #if you want to view the plot

#separate out the female tissue data
df_tissues_f <- df_tissues %>% 
    filter(sex == 'female')  

#calculate mean and median of tissues for finding fold change difference
df_tissues_f %>% group_by(tissue) %>% summarise(mean = mean(normalized), median = median(normalized))

#create comparisons list for plotting
my_comparisons_f <- list( c("body", "midgut"), c("body", "ovaries"), c("midgut", "ovaries") )

p_tissue_f <- ggplot(df_tissues_f, aes(tissue, normalized)) +
  geom_boxplot(aes(fill = factor(tissue))) +
  labs(title = "Galbut Virus RNA Levels in Female Tissues",
       y = "Galbut virus RNA levels relative to RpL-32 (2^-deltaCt)",
       x = "Tissue Type") +
  scale_fill_manual(values=c("grey", "#009E73", "#56B4E9")) +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous() +
  stat_compare_means(comparisons = my_comparisons_f)

p_tissue_f #if you want to visualize the plot here    

#combine the plots
library(patchwork)
p_tissue_f | p_tissue_m

#save it as a PDF
ggsave("sex_tissue_virus_load_v2.pdf")

#This PDF was then opened with Affinity designer for aesthetics and sizing

#do comparison of body tissues by sex
df_tissues_body <- df_tissues %>%
  filter(tissue == "body")

#is it significant? yes
df_tissues_body %>% wilcox_test(normalized~sex, 
            paired = FALSE, 
            p.adjust.method = "holm")

#plot it
p_tissue_body <- ggplot(df_tissues_body, aes(sex, normalized)) +
  geom_boxplot(aes(fill = factor(sex))) +
  labs(title = "Galbut Virus RNA Levels in Female Tissues",
       y = "Galbut virus RNA levels relative to RpL-32 (2^-deltaCt)",
       x = "Sex") +
  scale_fill_manual(values=c("#56B4E9", "#D55E00")) +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous() +
  stat_compare_means()

p_tissue_body

ggsave("body_virus_load_v2.pdf")


#calculate mean and median of tissues for finding fold change difference
df_tissues_body %>% group_by(sex) %>% summarise(mean = mean(normalized), median = median(normalized))

