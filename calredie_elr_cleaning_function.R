#############################################################################################
########################## function for cleaning the elr data ###############################
#############################################################################################

# function for cleaning elrs
elr_fun <- function(x) {
  x1 <- x %>%
    # remove the Serology results from the final dataset
    filter(!Disease %in% "Coronavirus Disease 2019 - Serology Result") %>% 
    mutate(results = str_trim(toupper(Results), "both"), # get rid of spaces and convert to uppercase
           resulted_org = str_trim(toupper(`Resulted Organism`), "both"), # get rid of spaces and convert to uppercase
           elr_lab_collect = as.Date(`Specimen Received Date`, format = "%m-%d-%Y"), # convert to a date
           elr_result_date = as.Date(`Result Date`, format = "%m-%d-%Y"), # convert to a date
           lab_status = case_when(grepl("POSITIVE|POS", resulted_org) ~ "Positive",  
                                  resulted_org %in% c("DETECTED") ~ "Positive", 
                                  grepl("NOT DETECTED|NEGATIVE|NEG|NOT DET|UNDETECTED|NONE DET", resulted_org) ~ "Negative", 
                                  is.na(resulted_org) & results %in% c("POS-SEE REPORT", "POSITIVE", "DETECTED") ~ "Positive",
                                  is.na(resulted_org) & results %in% c("NOT DETECTED", "NEG-SEE REPORT", "NEGATIVE") ~ "Negative",
                                  is.na(resulted_org) & grepl("NOT DETECTED", results) ~ "Negative",
                                  grepl("INCONCLUSIVE|INVALID|INDETERMINATE|PENDING", resulted_org) ~ "Invalid or Pending",
                                  TRUE ~ "Unknown"),
           serology = case_when(grepl("IGG|IgG|IgM",  `Resulted Test`) ~ "serology",
                                TRUE ~ "PCR")) %>%
    # double check to get rid of serology tests: 
    filter(serology == "PCR") %>%
    # rename the multi-word columns 
    rename(local_org_descr = `Local Organism Description`,
           abnormal_flag = `Abnormal Flag`,
           performing_lab = `Performing Facility ID`,
           referring_physician = `Provider Name`,
           ordering_facility = `Facility Name`)  %>% 
    # only keep the columns we need
    select(IncidentID, DtCreate, elr_lab_collect, elr_result_date, results, resulted_org, lab_status, performing_lab, serology, referring_physician, ordering_facility)
  x1
}
