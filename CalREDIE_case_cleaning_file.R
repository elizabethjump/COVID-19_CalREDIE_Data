#######################################################################################
################################ Lab Data for Dashboard ###############################
#######################################################################################

# check your library. Make sure the library is in a 'write-able' place so that you can install packages. If it's on a drive that you can't edit (ex: the C: drive, you won't be able to install any packages)
.libPaths()

# if needed, change your library. Put the path here in quotes. Ex: "H:/R-4.0.0/library"
.libPaths("path here")

# Install any packages that you need. Put the package name in quotes when you install it. This only needs to be done once
## Ex:
install.packages("package name here")

# library (enable) these packages. You only need to install the packages once, but you need to library them each time you open an R session
library(dplyr)
library(tidyr)
library(readxl)
library(fuzzyjoin)
library(writexl)
library(lubridate)
library(stringr)
library(readr)

# set your working directory. This should be the place where your raw data outputs are stored. You can easily get this file path by going to the menu bar and going to Session > Set Working Directory > Choose Directory. The file path will appear in the console and then you can copy that path here: 
setwd("working directory here")

# set the file paths here
disgrp_file <- "DiseaseCountyDisGrpData.tsv" 
elr_file <- "SystemLabData.tsv"
verily_file <- "verily_20200402_cleaned.Rda"

# read in files
## tsv files
disgrp <- read_tsv(disgrp_file, guess_max = 10000) # guess max is for determining column types
elr <- read_tsv(elr_file, guess_max = 10000)

## Rda file - you can just load these since these file types are native to R
verily <- load(verily_file)

# load functions to clean the calredie data
source("calredie_elr_cleaning_function.R")
source("calredie_disease_group_cleaning_function.R")

#######################################################################################
###################################### clean data #####################################
#######################################################################################

#################################### patient file #####################################
# the disgrp function filters and cleans the disease file
disgrp1 <- disgrp_fun(disgrp)

# check on which diseases are included in the file - it should just be Novel Coronavirus & Coronavirus - Non-positive ELR
disgrp1 %>% 
  distinct(Disease)

# check on LHJ in the disgrp file, this should only contain your LHJ. If it doesn't you'll need to add a filter to only keep your LHJ
disgrp1 %>%
  distinct(LHJ)

###################################### lab files ######################################
# the elr function give all labs a status based on the resulted organism and Results column. 
elr1 <- elr_fun(elr) 

# look at potential labs by result values to see how to recode
## recoding is done in the function using keywords from the Results and Resulted Organism fields to create the lab status field
elr_stat <- elr1 %>%
  group_by(lab_status, resulted_org, results) %>%
  count()

#################################### verily file ######################################
# use an anti-join to compare the verily file to the disgrp1 file so you only keep people whose names are in verily but not in CalREDIE
verily_only <- verily1 %>%
  anti_join(disgrp1, by = c("FirstName", "LastName", "DOB"))

# these are the case statuses (Positive, Negative, Invalid) for the people only in Verily
verily_only %>%
  distinct(case_status)

#######################################################################################
###################################### merge data #####################################
#######################################################################################
# now you need to combine the datasets you've created from above

#################################### disgrp1 + elr ####################################
combined <- disgrp1 %>%
  left_join(elr1, "IncidentID") %>% # we're keeping everyone in the disgrp1 file and pulling in elrs for those incidents
  filter(RStatus != "Out of State") %>% # filtering out anyone that has an RStatus of Out of State (might be no one)
  mutate(lab_collection_date = case_when(is.na(lab_collection_date) ~ elr_lab_collect,
                                         TRUE ~ lab_collection_date), # if lab collect is missing in the disgrp file, replace it with the lab received date from the elr
         lab_result_date = case_when(is.na(lab_result_date) ~ elr_result_date,
                                     TRUE ~ lab_result_date), # if the result date is missing in the disgrp file, replace it with the lab resulted date from the elr 
         case_status = case_when(RStatus == "Confirmed" ~ "Positive", 
                                 RStatus == "Not A Case" ~ "Negative",
                                 (is.na(lab_status) | lab_status %in% c("Invalid or Pending", "Unknown")) &
                                   !RStatus %in% c("Confirmed", "Not a Case") &
                                   Disease == "Coronavirus Disease 2019 - Non-positive ELR" ~
                                   "Negative",
                                 (is.na(lab_status) | lab_status %in% c("Invalid or Pending", "Unknown")) &
                                   !RStatus %in% c("Confirmed", "Not a Case") &
                                   Disease != "Coronavirus Disease 2019 - Non-positive ELR" ~
                                   "Unknown",
                                 TRUE ~ lab_status)) # make a 'case status' per person using RStatus, Lab & Disease

# check out the breakdown of how RStatus + lab_status are used to determine case_status
## keep in mind that you have multiple labs per incident right now
case_stat <- combined %>%
  group_by(RStatus, lab_status, case_status) %>%
  count()

# get counts of statuses by distinct people (use name + dob since verily folks have different identifiers)
## positive shouldn't change much, but Negative and Unknown numbers will likely get smaller
combined %>%
  distinct(FirstName, LastName, DOB, case_status) %>%
  group_by(case_status) %>%
  count()

################################## calredie + verily #################################
all <- combined %>%
  bind_rows(verily_only) %>% # append the verily data if you have it, otherwise, comment out this line
  filter(!is.na(FirstName)) %>% # get rid of any rows without needed info
  mutate(DOB = case_when(is.na(DOB) ~ as.Date("1900-01-01"), # make blank birthdays 1/1/1900
                         TRUE ~ DOB))

# get counts of statuses by distinct people (use name + dob since verily folks have different identifiers)
all %>%
  distinct(FirstName, LastName, DOB, case_status) %>%
  group_by(case_status) %>%
  count()

#######################################################################################
####################################### final data ####################################
#######################################################################################

############################## determine status per case ##############################

# get one "result" per person: if the person has one positive, they're positive. no positives and a negative, they're negative. otherwise, unknown
status <- all %>%
  select(FirstName, LastName, DOB, case_status) %>% # can't use CR ID anymore because verily was appended
  group_by(FirstName, LastName, DOB, case_status) %>% # group according to these criteria
  count() %>% # count the number of rows per group 
  ungroup() %>% # remove groups
  spread(case_status, n) %>% # this makes a column for Positive, Negative and Unknown with the number of records each person has for each category
  replace_na(list(Negative = 0, Positive = 0, Unknown = 0)) %>% # replace missings with 0's
  mutate(final_stat = case_when(Positive >= 1 ~ "Positive",
                                Positive == 0 & Negative >= 1 ~ "Negative", 
                                TRUE ~ "Unknown")) # make a final status variable based on which statuses each person has

# check the final counts
status %>%
  group_by(final_stat) %>%
  count()

# now join the statuses back to the all (CalREDIE + Verily dataset)
all1 <- all %>% 
  left_join(status, by = c("FirstName", "LastName", "DOB")) %>% # this keeps everyone in the original dataset
  filter(case_status == final_stat) %>% # only keep rows with the 'correct' status 
  group_by(FirstName, LastName, DOB) %>% 
  arrange(lab_collection_date) %>%
  slice(1) %>% # only keep earliest lab collect for each person (group)
  ungroup() 

# get case counts
all1 %>%
  group_by(final_stat) %>%
  count()

# since the visualizations go off of lab collect date, we want to filter out anyone missing a lab collect date
## this is the number of people missing a lab collect date, by case status
all1 %>%
  filter(is.na(lab_collection_date)) %>%
  group_by(final_stat) %>%
  count()

# make a data frame where we only keep people with a lab collect date
all2 <- all1 %>%
  filter(!is.na(lab_collection_date))

# get final (actually final) counts to report
all2 %>% 
  group_by(final_stat) %>%
  count()

################################## duplicate checks #################################
# make sure each person is only in there once (this code should output a tibble wiht a value of 0)
all2 %>%
  group_by(FirstName, LastName, DOB, case_status) %>%
  filter(n() > 1) %>%
  ungroup() %>% 
  count() 

# make sure each person is only in there once (this code should output a tibble wiht a value of 0)
all2 %>%
  group_by(FirstName, LastName, DOB) %>%
  filter(n() > 1) %>% 
  ungroup() %>% 
  count()

################################################################################################
############################ generate & write tables for reports ###############################
################################################################################################
## this creates a table with sums per day, per status and cumulative counts per day, per status
cases_by_date <- all2 %>%
  group_by(lab_collection_date, case_status) %>%
  count() %>%
  spread(case_status, n) %>%
  replace_na(list(Negative = 0, Positive = 0, Unknown = 0)) %>%
  # get counts per day
  mutate(all_test_per_day = Negative + Positive + Unknown,
         category = "all",
         pct_pos_per_day = Positive / all_test_per_day,
         pct_pos_per_day = round(pct_pos_per_day, 2)) %>%
  # get running totals for all variables
  group_by(category) %>%
  arrange(lab_collection_date) %>%
  mutate(negative_cum_sum = cumsum(Negative),
         positive_cum_sum = cumsum(Positive),
         unknown_cum_sum = cumsum(Unknown),
         all_test_cum_sum = cumsum(all_test_per_day),
         pct_pos_cum_sum = positive_cum_sum/all_test_cum_sum,
         pct_pos_cum_sum = round(pct_pos_cum_sum, 2)) %>%
  ungroup() %>%
  select(-category) %>% # remove category variable
  rename(positive_per_day = Positive,
         negative_per_day = Negative,
         unknown_per_day = Unknown) 

# if you run reports in the evening, you might want to think about using this to display yesterday's data (since today's DDP isn't complete)
cases_by_date1 <- cases_by_date %>%
  arrange(lab_collection_date) %>%
  slice(1:nrow(cases_by_date)-1)

# get the summary stats for time to result
tt_result_stats <- all2 %>%
  filter(!is.na(lab_result_date) & !is.na(lab_collection_date)) %>%
  mutate(tt_result = difftime(lab_result_date, lab_collection_date, units = "days")) %>%
  filter(tt_result >= 0) %>%
  summarize(min_tt_result = min(tt_result),
            mean_tt_result = mean(tt_result),
            median_tt_result = median(tt_result),
            max_tt_result = max(tt_result),
            count = n())

###################################### write files ############################################
# this line of code makes an object called 'date' with the current date + time
date <- paste0(substr(Sys.Date(), 1, 4), substr(Sys.Date(), 6, 7), substr(Sys.Date(), 9, 10), "_", substr(Sys.time(), 12,13), substr(Sys.time(), 15,16))

# this makes a file path where you can write the 'current' file; overwrite this each day; replace `<path>` with your file path
path_cases_by_date <- "<path>/cases_by_date.csv"
# this makes a file path where you can write an archived file for each day and uses the date variable to timestamp that file; replace `<path>` with your file path
path_cases_by_date_archive <- paste0("<path>/archive/cases_by_date", date, ".csv")

# this makes a file path where you can write the 'current' file; overwrite this each day; replace `<path>` with your file path
path_tt_result_stats <- "<path>/lab_tt_result_stats.csv"
# this makes a file path where you can write an archived file for each day and uses the date variable to timestamp that file; replace `<path>` with your file path
path_tt_result_stats_archive <- paste0("<path>/archive/lab_tt_result_stats_", date, ".csv")

# write files
## write_csv takes three arguments: write_csv(data, path, na = ""); na = "" makes NA's print as blanks

## write the big date table to the 'current' file
write_csv(path_cases_by_date, 
          path_date,
          na = "")
## write the big date table to the 'archive' file
write_csv(cases_by_date, 
          path_cases_by_date_archive,
          na = "")

## write the summary table to the 'current' file
write_csv(tt_result_stats, 
          path_tt_result_stats,
          na = "")
## write the summary table to the 'archive' file
write_csv(tt_result_stats, 
          path_tt_result_stats_archive,
          na = "")