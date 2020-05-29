#############################################################################################
######################### function for disease grouping file data ###########################
#############################################################################################

# function for cleaning  the  Disease Group file

disgrp_fun <- function(x){
  x1 <- x %>%
    filter(DisShort %in% c("E-NCOV2019NP", "NCOV2019")) %>%  # only keep people with a disease of NCOV or Non-positive ELR
    mutate(FirstName = toupper(FirstName), # make first name all uppercase
           LastName = toupper(LastName), # make last name all uppercase
           DOB = as.Date(DOB, format = "%m/%d/%Y"), # convert to a date
           lab_collection_date = as.Date(DtLabCollect, format = "%m/%d/%Y"), # convert to a date
           lab_result_date = as.Date(DtLabResult, format = "%m/%d/%Y")) %>% # convert to a date
    # keep select columns
    select(IncidentID, PersonId, Disease, DtEpisode, DtCreate, DtReceived, LastName, FirstName, DOB, Age, Sex, Ethnicity, Race, Address, AptNo, City, State, Zip, CntyOfResid, LHJ, Investigator, RStatus, DtOnset, lab_collection_date, DtLabResult, lab_result_date, Laboratory, ) 
  x1
}
