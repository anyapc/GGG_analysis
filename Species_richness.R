### Global Grassy Group Protocol Analysis
### Species Richness
### Written in June 2023 by A.P. Courtenay with J. Wieczorkowski
### Contact: globalgrassygroup@gmail.com


### 1. Set up ----

# Run the R script 'Formatting_GGG_data_for_analysis.R' first

# Install then load the required R packages

install.packages('tidyverse')
install.packages('readxl')
install.packages('openxlsx')
#install.packages('lme4')
#install.packages('ggeffects')
library(tidyverse)  # to manipulate data
library(readxl)     # to use excel
library(openxlsx)   # to use excel
#library(lme4)       # for models
#library(ggeffects)  # for model predictions - ggpredict

# Set working directory to the folder where your formatted GGG data csv file is saved
setwd("C:/Users/My Drive/datafolder") # replace the pathway in (" ") with your folder

# Loading data
table3 <- read.xlsx("GGG_data_SITENAME.xlsx", sheet = "Table3") # load the dataset

### 2. Calculate species richness per plot ----
# The number of species recorded in each plot (i.e. in each 1 out of 21 plots at a site)

plot_richness <- table3 %>%  # take table 3 with a list of species IDs per plot
  select(SiteNo, PlotNo, SpID) %>% # select only the needed variables
  group_by(SiteNo, PlotNo) %>%  # create a grouping by site and by plot
  tally() %>%  # calculate the number of species per plot
  rename(richness_per_plot = n) %>% # change the name of variable 'n' to 'richness'
  ungroup()  # removes grouping

# Save
write.xlsx(file = "plot_richness.xlsx", plot_richness) 

### 3. Calculate species richness per site ----

# Total number of unique species per site
site_total_richness <- table3 %>%  # take table 3 with a list of species IDs per site
  select(SiteNo, SpID) %>%  # select Site No and species ID
  distinct() %>%  # keep unique rows only (i.e. unique species per site)
  group_by(SiteNo) %>%  # create a grouping by site
  tally() %>%  # calculate the number of species per site
  rename(site_total_richness = n)  %>%  # change the name of variable 'n' to 'richness'
  ungroup() # remove the grouping

# Save
write.xlsx(file = "site_total_richness.xlsx", site_total_richness)

# Average plot richness per site
site_average_richness <- plot_richness %>%  # take plot richness
  select(SiteNo, richness_per_plot) %>%  # select only site number and plot richness values
  group_by(SiteNo) %>%  # group by the site
  mutate(average = mean(richness_per_plot)) %>% # create a new variable called 'average' which should be equal to a mean of all plot richness values
  select(SiteNo, average) %>%  # select only site number and average plot richness
  rename(average_richness = average) %>%  # change the 'average' variable to 'average_richness'
  distinct() %>% # keep unique rows only
  ungroup()  # remove the grouping

# Save
write.xlsx(file = "site_average_richness.xlsx", site_average_richness)

