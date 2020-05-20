#Script for making Figure 1

#Created by: Shaun Cross

#Goals of the script:

#This script makes a basic bar chart of prevalence data across 3 years in FoCo flies
#Includes Confidence intervals

#Also added to include chart of prevalence across generations 
#Includes confidence intervals

library(ggplot2)
library(readxl)

#Set wd to where this file is saved
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

######Fig 1A: Prevalence across 3 years (2017-2019)########
#read in df
df_prev <- read_excel("FoCo_prevalence_across_years.xlsx", sheet = "wild")
#This file contains the year, prevalence (x/y flies), and upper and lower confidence intervals
#confidence intervals were created using the binom package
#See below for example of the use of this package:
# library(binom)
# observations <- 44
# trials <- 48
# binom.confint(observations, trials, methods = "exact")

#This next part is needed to keep it ordered as listed and not alphabetical
#plot it
ggplot(df_prev, aes(x = year, y = prevalence)) +
  geom_bar(stat = "identity", color = "black",
           position = position_dodge()) +
  geom_errorbar(aes(ymin = cil, ymax = ciu), width = 0.2,
                position = position_dodge()) +
  labs( x = "Year", y = "Galbut virus Prevalence (%)", 
        title = "Galbut virus Prevalence in Wild Flies") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))

#save it as a pdf
ggsave("Galbut_virus_prevalence_over_years.pdf")


#####Fig 1B: Prevalence across generations in colony FoCo-17########

#read in df
df_colony <- read_excel("FoCo_prevalence_across_years.xlsx", sheet = "colony")
#This file contains the year, prevalence (x/y flies), and upper and lower confidence intervals
#confidence intervals were created using the binom package
#See above for example of the use of this package.


#This next part is needed to keep it ordered as listed and not alphabetical
#Turn the 'sample' column into a character vector
df_colony$generation <- as.character(df_colony$generation)
#Then turn it back into a factor with the levels in the correct order
df_colony$generation <- factor(df_colony$generation, levels=unique(df_colony$generation))

#plot it
ggplot(df_colony, aes(x=generation, y=prevalence, group = 1)) + 
  geom_errorbar(aes(ymin = cil, ymax = ciu), width = 0.2,
                position = position_dodge()) +
  geom_line() + 
  geom_point()+
  scale_y_continuous(limits = c(0, 100)) +
  labs( x = "Generation", y = "Percent infected flies", 
        title = "Galbut virus Prevalence in Colonized Flies") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))

#save it as a pdf
ggsave("Galbut_virus_prevalence_colony.pdf")

####I then imported these files into affinity designer to edit for aesthetics and size####
####Final figure is post editing in affinity designer.
  