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
library(tidyverse)
library(readr)
library(readxl)
library(fuzzyjoin)
library(writexl)
library(lubridate)

# set your working directory. This should be the place where your raw data outputs are stored. You can easily get this file path by going to the menu bar and going to Session > Set Working Directory > Choose Directory. The file path will appear in the console and then you can copy that path here: 
setwd("working directory here")

# files
verily_file <- "verily file path here"

# read in files
## you only need to use ONE of these, use the one that matches your file extension
verily <- read_xls(verily_file) # for .xls files
verily <- read_xlsx(verily_file) # for .xlsx files
verily <- read_csv(verily_file) # for .csv files
verily <- read_tsv(verily_file) # for .tsv files
verily <- read_delim(verily_file, delim = "|") # for | delimited files

#######################################################################################
###################################### clean data #####################################
#######################################################################################

# GOAL 1: clean the verily data (get addresses, etc)

## Make a list of cities that exist in your county. Just keep adding them to the county_cities list. Make sure you include all spellings and any variations in spelling 
county_cities <- c("CITY ONE", "CITY TWO", "CITY THREE")

## make a list of all zip codes in your county. 
county_zips <- c(00001, 00002, 00003, 00004, 00005)

# clean up addresses and filter to only keep addresses that are actually in your county
## if someone has a city and a zip in the lists above, then we'll assume their address is correct. Anyone without a city on that list AND/OR without a zip on that list will fall onto the 'check' list for manual review of the address

verily1 <- verily %>%
  mutate(city = str_trim(toupper(customer_address_city), "both"), # get rid of spaces and make the city all uppercase
         county_city = case_when(city %in% county_cities ~ 1, 
                                 TRUE ~ 0), # matches the city to the list of cities above, 1 means it matches and 0 means it doesn't
         county_zip = case_when(zipcode %in% county_zips ~ 1, 
                                TRUE ~ 0), # matches the zip to the list of zips above, 1 means it matches and 0 means it doesn't
         check_me = case_when(county_city == 1 & county_zip == 0 ~ 1,
                              county_city == 0 & county_zip == 1 ~ 1,
                              county_city == 0 & county_zip == 0 ~ 1,
                              TRUE ~ 0), # anyone with a 1 will be reviewed below
         FirstName = toupper(customer_first_name), # make name uppercase
         LastName = toupper(customer_last_name), # make name uppercase
         DOB = as.Date(customer_birth_date), # make DOB a date
         case_status = case_when(cov2_result == "NEGATIVE" ~ "Negative", # recode that statuses
                                 cov2_result == "POSITIVE" ~ "Positive", # recode that statuses
                                 TRUE ~ "Invalid"), # recode that statuses
         lab_result_date = as.Date(str_sub(result_updated_time, 1, 10), format = "%Y-%m-%d"), # format date
         lab_collection_date = as.Date(str_sub(sample_collection_time, 1, 10), format = "%Y-%m-%d")) %>%
  filter(county_cities == 1 & county_zips == 1) %>% # only keep people who are definitely county residents
  rename(verily_id = participant_id, # rename certain variables
         Age = age,
         State = customer_address_state) %>%
  # only keep these variables
  select(verily_id, LastName, FirstName, DOB, Age, customer_address_line, customer_address_line2, city, State, zipcode, lab_result_date, lab_collection_date, case_status)

## review addresses that need to be "checked". Note: you can clean these up using R OR you can just update the source file
review_addresses <- verily1 %>%
  filter(check_me == 1) %>%
  select(verily_id, LastName, FirstName, DOB, Age, customer_address_line, customer_address_line2, city, State, zipcode, lab_result_date, lab_collection_date, case_status)

#######################################################################################
####################################### save data #####################################
#######################################################################################

# if you plan to use this in other R scripts, you can save it as an R file and easily load it into another script. Replace 'file_path' with the file path you want to use
save(verily1, file = "file_path.Rda")

# you can also just save this as an .xlsx file. Replace the `file_path` with the file path you want to use
write_xlsx(verily,
           "file_path.xlsx")
