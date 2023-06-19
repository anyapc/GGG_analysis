### Global Grassy Group Protocol Analysis
### Formatting GGGsubmit spreadsheets for analysis
### Written in June 2023 by A.P. Courtenay with J. Wieczorkowski
### Contact: globalgrassygroup@gmail.com

### 1. Set up ----

# Install then load the required R packages
install.packages('tidyverse') # add a hash (#) before if you have already installed these packages 
install.packages('readxl')
install.packages('openxlsx')
install.packages('tibble')
install.packages('janitor')
library(tidyverse)
library(readxl)
library(openxlsx)
library(tibble)
library(janitor)

# Set the working directory to the folder where your GGGsubmit.xlsx files are saved
setwd("C:/Users/My Drive/datafolder") # replace the pathway in (" ") with your folder

# Copy the link into your internet browser to download 'Template_Data_Formatting.xlsx' (press the '...' button on the right) then open the file with excel - it has the required column headings and 5 sheets
# https://github.com/anyapc/GGG_analysis/blob/main/Template_Data_Formatting.xlsx 


### 2. Formatting Datasheets 1 & 4 ----

# For Table 1
d1 <- read_excel("GGGsubmit.xlsx", sheet = "Datasheet1") %>% rename(var = 1) # replace 'GGGsubmit' with the name of your file
d4 <- read_excel("GGGsubmit.xlsx", sheet = "Datasheet4") %>% rename(var = 1) # replace 'GGGsubmit' with the name of your file
d14 <- bind_rows(d1, d4) %>%
  select(-Notes) %>%
  add_row(var = "SiteNo", Input = "1", .before = 1) %>% # replace the number 1 in Input = "1" with your site number
  column_to_rownames(var = "var") %>%
  t() %>%
  data.frame() %>%
  # mutate(Date = excel_numeric_to_date(as.numeric(as.character(Date)), date_system = "modern")) %>%
  select(SiteNo, Country, Repeat, Location:OtherNotes, Funding:VegType)

# Copy the table
write.table(d14, "clipboard", sep="\t", col.names = FALSE, row.names=FALSE)

# Remember to paste into excel sheet 1


### 3. Formatting Datasheet 2 ----

# For Table 2
d2 <- read_excel("GGGsubmit.xlsx", sheet = "Datasheet2") %>% # replace 'GGGsubmit' with the name of your file
  rename(var = 1) %>%
  select(-Notes) %>%
  select(-var) %>%
  t() %>%
  data.frame() %>%
  rownames_to_column() %>%
  mutate(SiteNo = 1, PlotNo = paste(SiteNo, rowname, sep = "")) %>% # replace the number 1 in 'SiteNo = 1' with your site number
  select(SiteNo, PlotNo, X1:X23)

# Copy the table
write.table(d2, "clipboard", sep="\t", col.names = FALSE, row.names=FALSE)

# Remember to paste into excel sheet 2

### 4. Formatting Datasheet 4 and 3 ----

# For Table 4
d3 <- read_excel("GGGsubmit.xlsx", sheet = "Datasheet3") %>% # replace 'GGGsubmit' with the name of your file
  mutate(SiteNo = 1, # replace the number 1 in 'SiteNo = 1' with your site number
         SiteSpNo = row_number(),
         SpID = paste(SiteNo, SiteSpNo, sep = "_"),
         VoConID = "ABC") # replace 'ABC' with the initials used on your voucher collections

d3a <- d3 %>%
  select(SiteNo:VoConID, FieldID, Voucher)

# Copy the table
write.table(d3a, "clipboard", sep="\t", col.names = FALSE, row.names=FALSE)

# Remember to paste into excel sheet 4


# For Table 3
d3b <- d3 %>%
  select(-FieldID, -Voucher, -VoConID, -SiteSpNo) %>%
  gather(plot, p, c(Z0:D25)) %>%
  filter(!is.na(p)) %>%
  mutate(PlotNo = paste(SiteNo, plot, sep = "")) %>%
  group_by(PlotNo) %>%
  mutate(OccurID = paste(PlotNo, row_number(), sep = "_")) %>%
  ungroup() %>%
  select(OccurID, SiteNo, PlotNo, SpID)

# Copy the table
write.table(d3b, "clipboard", sep="\t", col.names = FALSE, row.names=FALSE)

# Remember to paste into excel sheet 3


### 5. Formatting Datasheet 5 ----

# For Table Contributions
d5 <- read_excel("GGGsubmit.xlsx", sheet = "Datasheet5") %>% # replace GGGsubmit with the name of your file
  filter(!row_number() == 1) %>%
  select(name = 1, role = 2) %>%
  mutate(name = str_to_title(name),
         SiteNo = 1, # replace the number 1 in 'SiteNo = 1' with your site number
         ContributionID = paste(SiteNo, row_number(), sep = "_")) %>%
  select(SiteNo, name, role, ContributionID)

#Copy into excel
write.table(d5, "clipboard", sep="\t", col.names = FALSE, row.names=FALSE)

# Remember to paste into excel sheet 5

# Save your data as GGG_data_SITENAME.xlsx file and email it to: globalgrassygroup@gmail.com


### End 