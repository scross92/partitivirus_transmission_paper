#Script for creating Figure 1C

#The goal of this script is:

#This script makes heatmaps of individual flies from 2017 and 2018 colonies (FoCo-17 and FoCo-18)
#this script focuses on galbut virus (all segs separated), chaq virus, vera virus (all segs separated) and chaq-like (vera) virus

#created by: Shaun Cross
#initial creation date: 05-18-2020


library(tidyverse)
library(readxl)
library(gridExtra)
library(RColorBrewer)
#set wd to file saved location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#import the data

df <- read_excel("individual_foco_flies_galbut_vera.xlsx", sheet = "tidy_separate")
#This file has the sample names, corresponding year, the normalized reads (mapping reads per million unique reads), and the virus
#Sample name: [Sex][sample#]-[colony_year] e.g. F10-18 is female fly 10 from the FoCo-18 colony
#Sample name becomes somewhat irrelevant because these sample names are removed from y axis during plot
#Sheet "tidy_separate" has each RNA separated
#Sheet "tidy_condensed" has RNAs mapping read count (besides chaq/chaq-like) combined
#for Figure 2, RNAs were separated -> sheet "tidy_separate"


#add a 1 to any reads that are zero. Need it for log transformation in heatmap
df$normalized_reads <- df$normalized_reads+1

#make a variable for color breaks
my_breaks = c(0, 10, 100, 1000, 10000, 100000, 1000000)

#Turn the 'sample' column into a character vector
df$sample <- as.character(df$sample)
#Then turn it back into a factor with the levels in the correct order
df$sample <- factor(df$sample, levels=unique(df$sample))

#Use the following if using sheet tidy_condensed
# #order the virus seqs instead of alphabetical
# df <- df %>% mutate(virus = fct_relevel(virus, 
#                                         "galbut", "chaq", "vera", 
#                                         "chaq_vera"))

#use the following if using sheet tidy_separate
#order the virus seqs instead of alphabetical
#this keeps RNAs with their respective virus, and keeps chaq RNAs at the end rather than beginning
df <- df %>% mutate(virus = fct_relevel(virus, 
                                        "galbut_RNA1", "galbut_RNA2", "galbut_RNA3",
                                        "galbut_RNA_Chaq",
                                        "vera_RNA1", "vera_RNA2",
                                        "vera_RNA_Chaq",
                                        "RpL32", "pepper_cryptic_virus1"))

#plotting together gets messy because of different sample names (i.e. F-10-18 and F-10-17)
#filter by year, plot separately then recombine with patchwork
df_foco17 <- filter(df, df$year=="2017")
df_foco18 <- filter(df, df$year=="2018")

#plot FoCo-17 heatmap
#note that the legend is removed in this plot for combining both together
foco17.hm <- ggplot(data = df_foco17, mapping = aes(x = virus,
                                                  y = sample,
                                                  fill = normalized_reads)) +
  geom_tile(color = "gray22") +
  xlab(label = "Virus segment") +
  #facet_grid(~ location) +
  scale_fill_gradient(name = "Reads",
                      trans = "log",
                      breaks= my_breaks,
                      labels = my_breaks,
                      low = "#FFFFFF",
                      high = "#034e7b") +
  ggtitle(label = "Partitivirus reads in Drosophila samples") +
  theme_classic() + # Use the classic theme
  theme(strip.placement = "outside",
        plot.title = element_text(hjust = 0.5),
        axis.title.y = element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        strip.background = element_rect(fill = "#EEEEEE", color = "#FFFFFF"),
        legend.position = "none")
  #facet_grid(sample_type ~ RNA_type) #used to break apart by groups...but this didnt work well, so will split after

foco17.hm

#plot FoCo-18 heatmap
foco18.hm <- ggplot(data = df_foco18, mapping = aes(x = virus,
                                                    y = sample,
                                                    fill = normalized_reads)) +
  geom_tile(color = "gray22") +
  xlab(label = "Virus segment") +
  #facet_grid(~ location) +
  scale_fill_gradient(name = "Reads",
                      trans = "log",
                      breaks= my_breaks,
                      labels = my_breaks,
                      low = "#FFFFFF",
                      high = "#034e7b") +
  ggtitle(label = "Partitivirus reads in Drosophila samples") +
  theme_classic() + # Use the classic theme
  theme(strip.placement = "outside",
        plot.title = element_text(hjust = 0.5),
        axis.title.y = element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        strip.background = element_rect(fill = "#EEEEEE", color = "#FFFFFF"))

foco18.hm

#patch together
library(patchwork)

foco17.hm | foco18.hm

#save the file
ggsave("combined_2017_2018_invidual_flies_separate.pdf", width = 6, height = 8, units = "in")

#This PDF was then imported into affinity designer to edit aesthetics for final fig


#do males and females differ in RNA levels as determined by mapping reads?
#using combined data for this portion
df_combined <- read_excel("individual_foco_flies_galbut_vera.xlsx", sheet = "tidy_condensed")

#remove chaq and chaq-like RNAs since they dont always co-occur
df_combined_filter <- filter(df_combined, virus != "chaq", virus != "chaq_vera")

#does it appear to be normalized? no, do wilcoxon
ggplot(df_combined_filter, aes(x = normalized_reads)) + 
  geom_histogram() +
  facet_wrap(~sex)
library(car)
qqp(df_combined_filter$normalized_reads) # this is the major red flag
ggdensity(df_combined_filter$normalized_reads)



filter(df_combined_filter, year == "2018") %>%
  wilcox_test(normalized_reads~virus, 
                                paired = FALSE, 
                                p.adjust.method = "holm")


