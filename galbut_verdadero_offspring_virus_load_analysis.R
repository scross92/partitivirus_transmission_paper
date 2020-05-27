#This script creates Figure 6

#Galbut virus and verdadero virus vertical transmission analysis
#I am comparing virus RNA levels of offspring from maternal and paternal transmission
#Using VT data (individual bleached eggs) for galbut virus
#Using VT data from Aedes aegypti for verdadero virus

#goals of this script:

# 1) plot galbut virus RNA levels in all offspring by transmission mode
# 2) run wilcoxon test to see if they are significantly different from each other

#Created by: Shaun Cross
#Date: 2020-05-13

library(tidyverse)
library(readxl)
library(ggthemes)
library(ggpubr)

#set wd to where this file is saved
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#load the data
df_fly_offspring <- read_excel("offspring_virus_load_by_transmission.xlsx", sheet = "galbut")

#normalized aboundance (2^-deltaCt) to RpL-32

######Galbut virus analysis: ########

#do normality testing to know wilcoxon vs t test
#doesn't appear to be normalized, do wilcoxon
ggplot(df_fly_offspring, aes(x = normalized)) + 
  geom_histogram(bins = 1000) +
  facet_wrap(~transmission)
library(car)
qqp(df_fly_offspring$normalized)
ggdensity(df_fly_offspring$normalized)

library(rstatix)
df_fly_offspring %>% wilcox_test(normalized~transmission, 
                             paired = FALSE, 
                             p.adjust.method = "holm")

#make the plot
#colorblind friendly colors found here: https://personal.sron.nl/~pault/#sec:qualitative
theme_set(theme_classic())

g_fly_offspring <- ggplot(df_fly_offspring, aes(transmission, normalized)) +
  geom_boxplot(aes(fill=factor(transmission)), outlier.shape = NA) + 
  scale_fill_manual(values=c("#56B4E9", "#D55E00")) +
  labs(title="Galbut virus RNA Levels in Offspring",
       x="Mode of Vertical Transmission") +
  geom_dotplot(
    aes(fill = transmission), trim = FALSE,
    binaxis='y', stackdir='center', dotsize = 0.5,
    position = position_jitter(0.1), alpha = 1
  ) +
  scale_y_log10(name="Virus RNA Levels Relative to RpL-32") +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5, size = 11),
        axis.text.x = element_text(size = 9),
        axis.title.x = element_text(size = 10),
        axis.text.y = element_text(size = 9),
        axis.title.y = element_text(size = 10)
  ) +
  stat_compare_means()


g_fly_offspring

#calculate mean and median of offspring for finding fold change difference
df_fly_offspring %>% group_by(transmission) %>% summarise(mean = mean(normalized), median = median(normalized))


#######Verdadero virus analysis###########

df_mos_offspring <- read_excel("offspring_virus_load_by_transmission.xlsx", sheet = "verdadero")

#normalized aboundance (2^-deltaCt) to Actin-1

#do normality testing to know wilcoxon vs t test
#doesn't appear to be normalized, do wilcoxon
ggplot(df_mos_offspring, aes(x = normalized)) + 
  geom_histogram(bins = 1000) +
  facet_wrap(~transmission)
library(car)
qqp(df_mos_offspring$normalized)
ggdensity(df_mos_offspring$normalized)

library(rstatix)
df_mos_offspring %>% wilcox_test(normalized~transmission, 
                             paired = FALSE, 
                             p.adjust.method = "holm")

#plot it
g_mosquito_offspring <- ggplot(df_mos_offspring, aes(transmission, normalized)) +
  geom_boxplot(aes(fill=factor(transmission))) + 
  #theme(axis.text.x = element_text(angle=65, vjust=0.6)) + 
  scale_fill_manual(values=c("#56B4E9", "#D55E00")) +
  #guides(fill=guide_legend(title="Offspring Groups")) +
  labs(title="Verdadero virus RNA Levels in Offspring",
       x="Mode of Vertical Transmission") +
  geom_dotplot(
    aes(fill = transmission), trim = FALSE,
    binaxis='y', stackdir='center', dotsize = 0.5,
    position = position_jitter(0.1), alpha = 1
  ) +
  scale_y_log10(name="Virus RNA Levels Relative to Actin-1") +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5, size = 11),
        axis.text.x = element_text(size = 9),
        axis.title.x = element_text(size = 10),
        axis.text.y = element_text(size = 9),
        axis.title.y = element_text(size = 10)
  ) +
  stat_compare_means()

g_mosquito_offspring

#calculate mean and median of offspring for finding fold change difference
df_mos_offspring %>% group_by(transmission) %>% summarise(mean = mean(normalized), median = median(normalized))


#now can combine those two plots together for general vorus load
library(patchwork)
g_fly_offspring | g_mosquito_offspring

#save the plot as a PDF
ggsave("RNA_virus_loads_in_offspring.pdf", units = "cm", width = 8.7, height = 11.5)

#then imported into Affinity designer for aethetics and sizing
