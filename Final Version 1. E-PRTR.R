rm(list = ls())
cat("\014")  # Shortcut to clear the console

library(readxl)
library(dplyr)   # For data manipulation
library(tidyr)   # For reshaping data
library(stringr)

setwd("C:/Users/Tobyd/OneDrive - Aarhus universitet/Uni-livet/Kandidatgrad/10. Semester/Specialet/Data/Final data/")

EPRTR_Data <- read_excel(
  "Final_Data_For_R.xlsx",
  guess_max = 200000  # or a number ≥ the # of rows in your sheet
)

EPRTR_Data_city_postal_code <- read_excel("2_ProductionFacility.xlsx")

EPRTR_Data <- EPRTR_Data %>%
  left_join(EPRTR_Data_city_postal_code, by = c("Facility_ID" = "Facility_INSPIRE_ID"))

#Rename "city" "postalCode" to "City" "Postal_Code"
colnames(EPRTR_Data)[colnames(EPRTR_Data) == "city"] <- "City"
colnames(EPRTR_Data)[colnames(EPRTR_Data) == "postalCode"] <- "Postal_Code"

EPRTR_Data$Postal_Code <- as.character(EPRTR_Data$Postal_Code)

#Divide columns with 1000 to get it in tonne
EPRTR_Data <- EPRTR_Data %>%
  mutate(across(ends_with("_Emission"), ~ . / 1000))

################################### Deleting Data ####################################################################
# Count confidential facilities using nrow()
confidential_facilities <- EPRTR_Data %>%
  filter(Facility_Name == "CONFIDENTIAL")

#Delete data with confidential pollutant
EPRTR_Data <- EPRTR_Data %>%
  filter(Facility_Name != "CONFIDENTIAL")

#Delete data with confidential Parent_Name
EPRTR_Data <- EPRTR_Data %>%
  filter(Parent_Name != "CONFIDENTIAL")

#Delete data with confidential Street_Name
EPRTR_Data <- EPRTR_Data %>%
  filter(Street_Name != "CONFIDENTIAL")

#Delete duplicate facilities with two values for the same year, pollutant, medium and facility
EPRTR_Data <- EPRTR_Data %>%
  filter(!(Facility_ID == "RS.EEA/124101.FACILITY"))

EPRTR_Data <- EPRTR_Data %>%
  filter(!(Facility_ID == "UK.CAED/EW_EA-16632.FACILITY"))
#################################### Fix Naming System ###################################################
#Fix naming system. Many Site_Name start with Site of facility:
EPRTR_Data$Site_Name <- gsub("Site of facility: ", "", EPRTR_Data$Site_Name)

# Define a helper function that replaces several specified values with NA.
replace_na_multiple <- function(x, replacements) {
  for (r in replacements) {
    x <- na_if(x, r)
  }
  return(x)
}

# Now apply this function to the desired columns.
EPRTR_Data <- EPRTR_Data %>%
  mutate(
    across(c(Parent_Name,Building_Number, Street_Name), ~ replace_na_multiple(.x, c("keine", "-", "---","n.a.",".","N/A","--","“???? ? ????? - ????” ??"
                                                                                    ,"“????????????? ???????” ??","XXX","x")))
  )

################Observe the Data #######################################################################################
#Check the number of unique values in each column
unique_values <- sapply(EPRTR_Data, function(x) length(unique(x)))
print(unique_values)

#Summary statistics
summary(EPRTR_Data)

#Count NA
na_count <- sapply(EPRTR_Data, function(x) sum(is.na(x)))
print(na_count)

#Group data by Year,Facility_ID,Pollutant_Code, Medium
# Define all of your grouping keys *except* Total_Onsite_Emission
Checking1 <- nrow(EPRTR_Data %>%
  group_by(Year, Facility_ID, Pollutant_Code, Medium,Country,
           Site_ID,Site_Name,Parent_Name,Facility_Name,Subsector,
           Lat,Lon,Street_Name,Building_Number,ETS_ID,EPER_ID,Pollutant_Name,City,Postal_Code) %>%
  summarise(Total_Emission = sum(Total_Facility_Emission), .groups = "drop"))

Checking2 <- nrow(EPRTR_Data %>%
  group_by(Year, Facility_ID, Pollutant_Code) %>%
  summarise(Total_Emission = sum(Total_Facility_Emission,na.rm = TRUE), .groups = "drop"))

Checking3 <- nrow(EPRTR_Data %>%
  group_by(Year, Facility_ID) %>%
  summarise(Total_Emission = sum(Total_Facility_Emission,na.rm = TRUE), .groups = "drop"))

Checking4 <- nrow(EPRTR_Data %>%
  group_by(Facility_ID) %>%
  summarise(Total_Emission = sum(Total_Facility_Emission,na.rm = TRUE), .groups = "drop"))

#Filter Medium = Air and Filter Medium = Water and Filter Medium = Land
Checking5 <- nrow(EPRTR_Data %>%
  filter(Medium == "AIR"))

Checking6 <- nrow(EPRTR_Data %>%
  filter(Medium == "WATER"))

Checking7 <- nrow(EPRTR_Data %>%
  filter(Medium == "LAND"))

#Filter for having both Air and Water
Checking8 <-  EPRTR_Data %>%
  group_by(Pollutant_Code,Medium) %>%
  summarise(#Count
    count = n(),
    Total_Emission = mean(Total_Facility_Emission,na.rm = TRUE), .groups = "drop")

#Delete columns
EPRTR_Data <- EPRTR_Data %>%
  select(-c(Offsite_Emission,Total_Onsite_Emission,Pollutant_Name,Non_Routine_Emission,Routine_Emission))

#Rename columns
colnames(EPRTR_Data)[colnames(EPRTR_Data) == "Total_Facility_Emission"] <- "E"

############################################ Pivot data to wide format #################
#Pivot data to wide format
EPRTR_Data_wide <- EPRTR_Data %>%
  pivot_wider(
    # each pollutant × medium becomes a column
    names_from  = c(Pollutant_Code, Medium),
    values_from = c(E),
    names_glue  = "{Pollutant_Code}_{Medium}"
  )

#Check the number of unique values in each column
unique_values <- sapply(EPRTR_Data_wide, function(x) length(unique(x)))
print(unique_values)

#Delete columns with low amount of info
EPRTR_Data_wide <- EPRTR_Data_wide %>%
  select(-c(NH3_WATER,
            NIANDCOMPOUNDS_LAND,
            NOX_WATER,
            SOX_WATER,
            TOC_AIR, TOC_LAND,
            TOTALNITROGEN_AIR, TOTALNITROGEN_LAND,
            TOTALPHOSPHORUS_LAND,
            ZNANDCOMPOUNDS_LAND))

#Summary statistics
summary(EPRTR_Data_wide)

#Count Observations
observation_count <- sapply(EPRTR_Data_wide, function(x) sum(!is.na(x)))
print(observation_count)

#Count NA
na_count <- sapply(EPRTR_Data_wide, function(x) sum(is.na(x)))
print(na_count)

################################### Check to see if there are several facilities at the same adress #############
#Make a dataset with only the unique combinations of Country,Facility_ID,Site_ID,Site_Name,Parent_Name,Facility_Name, Subsector,Lat,Lon,Street_Name, Building_number,ETS_ID,EPER_ID
EPRTR_full <- EPRTR_Data_wide %>%
  select(Country, Site_ID, Site_Name, Facility_ID, Parent_Name, Facility_Name, Subsector, Lat, Lon, City, Postal_Code, Street_Name, Building_Number, ETS_ID, EPER_ID) %>%
  distinct()

#Remove all numbers in the adress
EPRTR_full$Street_Name2 <- gsub("[0-9]", "", EPRTR_full$Street_Name)

EPRTR_full <- EPRTR_full %>%
  mutate(
    Street_Name2 = Street_Name2 %>%
      tolower() %>% 
      str_squish() %>%                      # Collapses spaces and trims leading/trailing whitespace
      str_replace_all("[^[:alnum:] ]", "")    # Remove non-alphanumeric characters (except spaces)
  ) %>%
  mutate(
    Street_Name2 = if_else(Street_Name2 == "", NA_character_, Street_Name2)
  )

EPRTR_full <- EPRTR_full %>%
  mutate(
    Street_Name2 = Street_Name2 %>%
      str_squish()  
)

EPRTR_full <- EPRTR_full %>%
  mutate(
    across(c(Street_Name2), ~ replace_na_multiple(.x, c("","nr","straße","x")))
)

EPRTR_full <- EPRTR_full %>%
  mutate(
    City = City %>%
      tolower() %>% 
      str_squish() %>%                      
      str_replace_all("[^[:alnum:] ]", "")  
  ) %>%  mutate(
    City = if_else(City == "", NA_character_, City)
  )

EPRTR_full <- EPRTR_full %>%
  mutate(
    City = City %>%
      tolower() %>% 
      str_squish() %>%                      
      str_replace_all("[^[:alnum:] ]", "")  
  ) %>%  mutate(
    City = if_else(City == "", NA_character_, City)
  )

#Check if there are several facilities at the same adress
Street_EPRTR <- EPRTR_full %>%
  group_by(City) %>%
  summarise(count = n(),
            .groups = "drop") %>%
  filter(count > 1) %>%
  arrange(desc(count))

######################################### Check the ETS EUTL Dataset ###########################################
ETS_Data <- read_excel("ETS_ACTUAL_EUTL_data_July_2024.xlsx")

ETS_Data_full <- ETS_Data %>%
  select(COUNTRY_CODE, LAST_NAME, INSTALLATION_NAME, ADDRESS1,CITY,POSTAL_CODE) %>%
  distinct()

ETS_Data_full <- ETS_Data_full %>%
  mutate(
    across(c(COUNTRY_CODE,LAST_NAME, INSTALLATION_NAME,ADDRESS1), ~ replace_na_multiple(.x, c("keine", "-", "---","n.a.",".","N/A","--","“???? ? ????? - ????” ??"
                                                                                    ,"“????????????? ???????” ??","XXX","x")))
  )

#Remove all numbers in the address
ETS_Data_full$ADDRESS2 <- gsub("[0-9]", "", ETS_Data_full$ADDRESS1)

ETS_Data_full <- ETS_Data_full %>%
  mutate(ADDRESS2 = ADDRESS2 %>%
           tolower() %>%
           str_trim(side = "both") %>%
           str_replace_all("[^[:alnum:] ]", "")) %>%
  mutate(ADDRESS2 = na_if(ADDRESS2, ""))

ETS_Data_full <- ETS_Data_full %>%
  mutate(
    ADDRESS2 = ADDRESS2 %>%
      tolower() %>% 
      str_squish() %>%                      # Collapses spaces and trims leading/trailing whitespace
      str_replace_all("[^[:alnum:] ]", "")    # Remove non-alphanumeric characters (except spaces)
  ) %>%
  mutate(
    ADDRESS2 = if_else(ADDRESS2 == "", NA_character_, ADDRESS2)
  )

ETS_Data_full <- ETS_Data_full %>%
  mutate(
    ADDRESS2 = ADDRESS2 %>%
      str_squish()  
  )

ETS_Data_full <- ETS_Data_full %>%
  mutate(
    across(c(ADDRESS2,CITY,POSTAL_CODE), ~ replace_na_multiple(.x, c("","nr","straße","x","-")))
  )

ETS_Data_full <- ETS_Data_full %>%
  mutate(
    CITY = CITY %>%
      tolower() %>% 
      str_squish() %>%                    
      str_replace_all("[^[:alnum:] ]", "")  
  )  %>%  mutate(
    CITY = if_else(CITY == "", NA_character_, CITY)
  )

Street_ETS <- ETS_Data_full %>%
  group_by(CITY) %>%
  summarise(count = n(),
            .groups = "drop") %>%
  filter(count > 1) %>%
  arrange(desc(count))

#Find unique country codes for ETS_Data_full
unique_country_codes_ETS <- unique(ETS_Data_full$COUNTRY_CODE)
unique_country_codes_EPRTR <- unique(EPRTR_full$Country)

print(unique_country_codes_ETS)
print(unique_country_codes_EPRTR)
#Check for non-overlap
setdiff(unique_country_codes_ETS, unique_country_codes_EPRTR)
# codes in EPRTR but not in ETS
setdiff(unique_country_codes_EPRTR, unique_country_codes_ETS)

#Change country code XI into GB
ETS_Data_full$COUNTRY_CODE <- gsub("XI", "GB", ETS_Data_full$COUNTRY_CODE)

############################################### Check the address overlap in the datasets using City, street and country #######
#Extract the BUILDING_NUMBER from the ETS dataset
ETS_Data_full <- ETS_Data_full %>%
  mutate(
    BUILDING_NUMBER = str_extract(ADDRESS1, "\\d+")
  )

EPRTR_full <- EPRTR_full %>%
  mutate(
    Building_Number2 = str_extract(Street_Name, "\\d+")
  )

#If Building_Number is NA and Building_Number2 is not NA, then set Building_Number to Building_Number2
EPRTR_full <- EPRTR_full %>%
  mutate(
    Building_Number = ifelse(is.na(Building_Number) & !is.na(Building_Number2), Building_Number2, Building_Number)
  )

#Remove letters in the Building_Number
ETS_Data_full$BUILDING_NUMBER <- gsub("[A-Za-z]", "", ETS_Data_full$BUILDING_NUMBER)
EPRTR_full$Building_Number <- gsub("[A-Za-z]", "", EPRTR_full$Building_Number)

ETS_Data_full <- ETS_Data_full %>%
  mutate(
    BUILDING_NUMBER = BUILDING_NUMBER %>%
      tolower() %>% 
      str_squish() %>%                      # Collapses spaces and trims leading/trailing whitespace
      str_replace_all("[^[:alnum:] ]", "")    # Remove non-alphanumeric characters (except spaces)
  )

ETS_Data_full <- ETS_Data_full %>%
  mutate(
    BUILDING_NUMBER = BUILDING_NUMBER %>%
      str_squish()
  )

ETS_Data_full <- ETS_Data_full %>%
  mutate(
    BUILDING_NUMBER = if_else(BUILDING_NUMBER == "", NA_character_, BUILDING_NUMBER)
  )

EPRTR_full <- EPRTR_full %>%
  mutate(
    Building_Number = Building_Number %>%
      tolower() %>% 
      str_squish() %>%                      # Collapses spaces and trims leading/trailing whitespace
      str_replace_all("[^[:alnum:] ]", "")    # Remove non-alphanumeric characters (except spaces)
  )

EPRTR_full <- EPRTR_full %>%
  mutate(
    Building_Number = Building_Number %>%
      str_squish()
  )

EPRTR_full <- EPRTR_full %>%
  mutate(
    Building_Number = if_else(Building_Number == "", NA_character_, Building_Number)
  )

ETS_Data_full <- ETS_Data_full %>%
  mutate(
    ADDRESS3 = ADDRESS2
  )

EPRTR_full_city <- EPRTR_full %>%
  filter(!is.na(Street_Name2) & !is.na(City) & !is.na(Building_Number) & !is.na(Country))


ETS_Data_full_city <- ETS_Data_full %>%
  filter(!is.na(ADDRESS2)  & !is.na(CITY) & !is.na(BUILDING_NUMBER) & !is.na(COUNTRY_CODE))

#I needed to also match the building number or else i would have several wrong matches.

#Join the two datasets on the address
EPRTR_City <- EPRTR_full_city %>%
  left_join(
    ETS_Data_full_city %>% 
      select(ADDRESS3,ADDRESS2, CITY, COUNTRY_CODE, BUILDING_NUMBER) %>% 
      distinct(),                  # reduce ETS to one row per key
    by = c(
      "Street_Name2"   = "ADDRESS2",
      "City"           = "CITY",
      "Country"        = "COUNTRY_CODE",
      "Building_Number"= "BUILDING_NUMBER"
    )
  ) %>%
  mutate(
    Match_City = if_else(!is.na(ADDRESS3), 1, 0)
  )

#Join the dummy back to the EPRTR_Data dataset
EPRTR_Data_wide <- EPRTR_Data_wide %>%
  left_join(EPRTR_City %>% select(Facility_ID, Match_City), 
            by = c("Facility_ID"))

#Count the number of matches
EPRTR_Data_wide %>%
  group_by(Match_City) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

#Now do the same for matching on postal code instead of city

EPRTR_full_city <- EPRTR_full %>%
  filter(!is.na(Street_Name2) & !is.na(Postal_Code) & !is.na(Building_Number) & !is.na(Country))


ETS_Data_full_city <- ETS_Data_full %>%
  filter(!is.na(ADDRESS2)  & !is.na(POSTAL_CODE) & !is.na(BUILDING_NUMBER) & !is.na(COUNTRY_CODE))

EPRTR_Postal <- EPRTR_full_city %>%
  left_join(
    ETS_Data_full_city %>% 
      select(ADDRESS3,ADDRESS2, POSTAL_CODE, COUNTRY_CODE, BUILDING_NUMBER) %>% 
      distinct(),                  # reduce ETS to one row per key
    by = c(
      "Street_Name2"   = "ADDRESS2",
      "Postal_Code"     = "POSTAL_CODE",
      "Country"        = "COUNTRY_CODE",
      "Building_Number"= "BUILDING_NUMBER"
    )
  ) %>%
  mutate(
    Match_Postal = if_else(!is.na(ADDRESS3), 1, 0)
  )

#Join the dummy back to the EPRTR_Data_wide dataset
EPRTR_Data_wide <- EPRTR_Data_wide %>%
  left_join(EPRTR_Postal %>% select(Facility_ID, Match_Postal), 
            by = c("Facility_ID"))

#Count the number of matches
EPRTR_Data_wide %>%
  group_by(Match_Postal) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

####Do the same just on names on the two datasets
#EPRTR_full 
#ETS_Data_full

EPRTR_full_Names <- EPRTR_full %>%
  select(Country,Facility_ID, Site_Name, Parent_Name, Facility_Name,City,Postal_Code) %>% 
  distinct()                  # reduce EPRTR to one row per key

ETS_full_Names <- ETS_Data_full %>%
  select(CITY,POSTAL_CODE,COUNTRY_CODE,LAST_NAME, INSTALLATION_NAME) %>% 
  distinct()                  # reduce ETS to one row per key


preclean <- function(x) {
  x %>%
    na_if("") %>%                    # "" → NA
    tolower() %>%                    # N/A → n/a etc.
    na_if("n/a") %>%                 # catch literal n/a
    str_replace_all("[^[:alnum:] ]+", " ") %>%  # non‑alnum → space
    str_replace_all("\\b[a-z]\\b",   "") %>%    # remove any single letter
    str_squish() %>%                 # trim & collapse spaces
    na_if("")                        # if it became empty, → NA
}


# Apply to EPRTR_full
EPRTR_full_Names <- EPRTR_full_Names %>%
  mutate(across(
    c(Facility_Name, Parent_Name, Site_Name),
    preclean
  ))

ETS_full_Names <- ETS_full_Names %>%
  mutate(across(
    c(LAST_NAME, INSTALLATION_NAME),
    preclean
  ))

##Check the datasets
Site_Name_EPRTR <- EPRTR_full_Names %>%
  group_by(Site_Name) %>%
  summarise(count = n(),
            .groups = "drop") %>%
  filter(count > 1) %>%
  arrange(desc(count))

Parent_Name_EPRTR <- EPRTR_full_Names %>%
  group_by(Parent_Name) %>%
  summarise(count = n(),
            .groups = "drop") %>%
  filter(count > 1) %>%
  arrange(desc(count))

Facility_Name_EPRTR <- EPRTR_full_Names %>%
  group_by(Facility_Name) %>%
  summarise(count = n(),
            .groups = "drop") %>%
  filter(count > 1) %>%
  arrange(desc(count))

INSTALLATION_NAME_ETS <- ETS_full_Names %>%
  group_by(INSTALLATION_NAME) %>%
  summarise(count = n(),
            .groups = "drop") %>%
  filter(count > 1) %>%
  arrange(desc(count))

LAST_NAME_ETS <- ETS_full_Names %>%
  group_by(LAST_NAME) %>%
  summarise(count = n(),
            .groups = "drop") %>%
  filter(count > 1) %>%
  arrange(desc(count))

# Tokenize EPRTR names
tokens_EPRTR <- EPRTR_full_Names %>%
  select(Facility_Name, Parent_Name, Site_Name) %>%
  distinct() %>% 
  pivot_longer(
    cols      = everything(),
    names_to  = "field",     # which column it came from
    values_to = "full_name"  # the actual text
  ) %>%
  filter(!is.na(full_name)) %>%
  mutate(
    # lowercase and split on whitespace
    full_name = str_to_lower(full_name),
    word      = str_split(full_name, "\\s+")
  ) %>%
  unnest(word) %>%
  # optional: keep only letters/digits/hyphens/slashes
  # filter(str_detect(word, "^[\\p{L}\\d/\\-]+$")) %>%
  count(word, sort = TRUE)

# Tokenize ETS names
tokens_ETS <- ETS_full_Names %>%
  select(LAST_NAME, INSTALLATION_NAME) %>%
  distinct() %>%
  pivot_longer(
    cols      = everything(),
    names_to  = "field",
    values_to = "full_name"
  ) %>%
  filter(!is.na(full_name)) %>%
  mutate(
    full_name = str_to_lower(full_name),
    word      = str_split(full_name, "\\s+")
  ) %>%
  unnest(word) %>%
  count(word, sort = TRUE)

print(tokens_ETS,n=100)
print(tokens_EPRTR,n=100)

jargon_tokens <- c(
  # legal forms & suffixes
  "gmbh", "group","limited", "ltd", "ab", "sa", "s\\.l\\.", "spa", "sp", 
  "kg", "inc", "sc", "ag", "srl", "kft", "sas", "co", "bv", "oy", "ead", 
  "e\\.o\\.", "pc", "llc", "d\\.o\\.o\\.", "s\\.p\\.a\\.", "s\\.r\\.o\\.", "corporation",
  # country codes / names
  "uk", "france", "polska", "norway", "belgium", "españa", 
  # generic words
  "site", "station", "plant", "werk", "matfisk", "granja", "anlage", "company", "services",
  "shipping", "management", "aviation", "air", "water", "landfill",
  "of", "and", "und", "le", "la", "do", "del", "ex", "de", "di",
  # standalone numbers
  "\\b\\d+\\b",
  # standalone single letters
  "\\b[a-z]\\b"
)


# build a single regex matching any of the above, as whole words
jargon_pattern <- paste0("\\b(", paste(jargon_tokens, collapse="|"), ")\\b")

clean_names <- function(x) {
  x %>%
    tolower() %>%
    str_squish() %>%                       # normalize whitespace
    str_remove_all(jargon_pattern) %>%     # drop all jargon tokens
    str_squish()   %>%                        # collapse any now‑extra spaces
    na_if("")                        # if it became empty, → NA
}

EPRTR_pruned <- EPRTR_full_Names %>%            # EPRTR_clean is your precleaned names tibble
  mutate(across(
    c(Facility_Name, Parent_Name, Site_Name),
    clean_names
  ))

ETS_pruned <- ETS_full_Names %>%            # EPRTR_clean is your precleaned names tibble
  mutate(across(
    c(LAST_NAME, INSTALLATION_NAME),
    clean_names
  ))

#Drop na
EPRTR_pruned_Facility_Name <- EPRTR_pruned %>%
  filter(!is.na(Facility_Name) & !is.na(Country))

EPRTR_pruned_Parent_Name <- EPRTR_pruned %>%
  filter(!is.na(Parent_Name) & !is.na(Country))

EPRTR_pruned_Site_Name <- EPRTR_pruned %>%
  filter(!is.na(Site_Name) & !is.na(Country))

#Drop na in ETS
ETS_pruned_LAST_NAME <- ETS_pruned %>%
  filter(!is.na(LAST_NAME) & !is.na(COUNTRY_CODE)) %>%
  mutate(LAST_NAME2 = LAST_NAME)

ETS_pruned_INSTALLATION_NAME <- ETS_pruned %>%
  filter(!is.na(INSTALLATION_NAME) & !is.na(COUNTRY_CODE)) %>%
  mutate(INSTALLATION_NAME2 = INSTALLATION_NAME)

# Join the two datasets on the F_L
EPRTR_Match_F_L <- EPRTR_pruned_Facility_Name %>%
  left_join(
    ETS_pruned_LAST_NAME %>% 
      select(COUNTRY_CODE, LAST_NAME,LAST_NAME2) %>% 
      distinct(),                  # reduce ETS to one row per key
    by = c(
      "Facility_Name"   = "LAST_NAME",
      "Country"         = "COUNTRY_CODE"
    )
  ) %>%
  mutate(
    Match_F_L = if_else(!is.na(LAST_NAME2), 1, 0)
  )

#Join the dummy back to the EPRTR_Data_wide dataset
EPRTR_Data_wide <- EPRTR_Data_wide %>%
  left_join(EPRTR_Match_F_L %>% select(Facility_ID, Match_F_L), 
            by = c("Facility_ID"))

EPRTR_Data_wide %>%
  group_by(Match_F_L) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

# Join the two datasets on the F_I
EPRTR_Match_F_I <- EPRTR_pruned_Facility_Name %>%
  left_join(
    ETS_pruned_INSTALLATION_NAME %>% 
      select(COUNTRY_CODE, INSTALLATION_NAME,INSTALLATION_NAME2) %>% 
      distinct(),                  # reduce ETS to one row per key
    by = c(
      "Facility_Name"   = "INSTALLATION_NAME",
      "Country"         = "COUNTRY_CODE"
    )
  ) %>%
  mutate(
    Match_F_I = if_else(!is.na(INSTALLATION_NAME2), 1, 0)
  )

#Join the dummy back to the EPRTR_Data_wide dataset
EPRTR_Data_wide <- EPRTR_Data_wide %>%
  left_join(EPRTR_Match_F_I %>% select(Facility_ID, Match_F_I), 
            by = c("Facility_ID"))

EPRTR_Data_wide %>%
  group_by(Match_F_I) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

# Join the two datasets on the P_L
EPRTR_Match_P_L <- EPRTR_pruned_Parent_Name %>%
  inner_join(
    ETS_pruned_LAST_NAME %>% 
      select(COUNTRY_CODE, LAST_NAME,LAST_NAME2) %>% 
      distinct(),                  # reduce ETS to one row per key
    by = c(
      "Parent_Name"   = "LAST_NAME",
      "Country"         = "COUNTRY_CODE"
    )
  ) %>%
  mutate(
    Match_P_L = if_else(!is.na(LAST_NAME2), 1, 0)
  )
#Join the dummy back to the EPRTR_Data_wide dataset
EPRTR_Data_wide <- EPRTR_Data_wide %>%
  left_join(EPRTR_Match_P_L %>% select(Facility_ID, Match_P_L), 
            by = c("Facility_ID"))
EPRTR_Data_wide %>%
  group_by(Match_P_L) %>%
  summarise(count = n()) %>%
  arrange(desc(count))
# Join the two datasets on the P_I
EPRTR_Match_P_I <- EPRTR_pruned_Parent_Name %>%
  left_join(
    ETS_pruned_INSTALLATION_NAME %>% 
      select(COUNTRY_CODE, INSTALLATION_NAME,INSTALLATION_NAME2) %>% 
      distinct(),                  # reduce ETS to one row per key
    by = c(
      "Parent_Name"   = "INSTALLATION_NAME",
      "Country"         = "COUNTRY_CODE"
    )
  ) %>%
  mutate(
    Match_P_I = if_else(!is.na(INSTALLATION_NAME2), 1, 0)
  )
#Join the dummy back to the EPRTR_Data_wide dataset
EPRTR_Data_wide <- EPRTR_Data_wide %>%
  left_join(EPRTR_Match_P_I %>% select(Facility_ID, Match_P_I), 
            by = c("Facility_ID"))
EPRTR_Data_wide %>%
  group_by(Match_P_I) %>%
  summarise(count = n()) %>%
  arrange(desc(count))
# Join the two datasets on the S_L
EPRTR_Match_S_L <- EPRTR_pruned_Site_Name %>%
  left_join(
    ETS_pruned_LAST_NAME %>% 
      select(COUNTRY_CODE, LAST_NAME,LAST_NAME2) %>% 
      distinct(),                  # reduce ETS to one row per key
    by = c(
      "Site_Name"   = "LAST_NAME",
      "Country"         = "COUNTRY_CODE"
    )
  ) %>%
  mutate(
    Match_S_L = if_else(!is.na(LAST_NAME2), 1, 0)
  )
#Join the dummy back to the EPRTR_Data_wide dataset
EPRTR_Data_wide <- EPRTR_Data_wide %>%
  left_join(EPRTR_Match_S_L %>% select(Facility_ID, Match_S_L), 
            by = c("Facility_ID"))
EPRTR_Data_wide %>%
  group_by(Match_S_L) %>%
  summarise(count = n()) %>%
  arrange(desc(count))
# Join the two datasets on the S_I
EPRTR_Match_S_I <- EPRTR_pruned_Site_Name %>%
  left_join(
    ETS_pruned_INSTALLATION_NAME %>% 
      select(COUNTRY_CODE, INSTALLATION_NAME,INSTALLATION_NAME2) %>% 
      distinct(),                  # reduce ETS to one row per key
    by = c(
      "Site_Name"   = "INSTALLATION_NAME",
      "Country"         = "COUNTRY_CODE"
    )
  ) %>%
  mutate(
    Match_S_I = if_else(!is.na(INSTALLATION_NAME2), 1, 0)
  )
#Join the dummy back to the EPRTR_Data_wide dataset
EPRTR_Data_wide <- EPRTR_Data_wide %>%
  left_join(EPRTR_Match_S_I %>% select(Facility_ID, Match_S_I), 
            by = c("Facility_ID"))
EPRTR_Data_wide %>%
  group_by(Match_S_I) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

######################################### Observe the amount of dummies ########################################################
#Change NA to 0 for ETS_ID, EPER_ID, Match_City, Match_Postal, Match_F_L, Match_F_I, Match_P_L, Match_P_I, Match_S_L, Match_S_I
EPRTR_Data_wide <- EPRTR_Data_wide %>%
  mutate(
    ETS_ID = ifelse(is.na(ETS_ID), 0, ETS_ID),
    EPER_ID = ifelse(is.na(EPER_ID), 0, EPER_ID),
    Match_City = ifelse(is.na(Match_City), 0, Match_City),
    Match_Postal = ifelse(is.na(Match_Postal), 0, Match_Postal),
    Match_F_L = ifelse(is.na(Match_F_L), 0, Match_F_L),
    Match_F_I = ifelse(is.na(Match_F_I), 0, Match_F_I),
    Match_P_L = ifelse(is.na(Match_P_L), 0, Match_P_L),
    Match_P_I = ifelse(is.na(Match_P_I), 0, Match_P_I),
    Match_S_L = ifelse(is.na(Match_S_L), 0, Match_S_L),
    Match_S_I = ifelse(is.na(Match_S_I), 0, Match_S_I)
  )

Dummy_combinations <- EPRTR_Data_wide %>%
  group_by(ETS_ID, EPER_ID, Match_City, Match_Postal, Match_F_L, Match_F_I, Match_P_L, Match_P_I, Match_S_L, Match_S_I) %>%
  summarise(count = n(),
            .groups = "drop") %>%
  arrange(desc(count))

#I would like to observe the rows where the sum of the dummies is equal to 1
Dummy_combinations <- Dummy_combinations %>%
  mutate(
    Sum_Dummies = ETS_ID + EPER_ID + Match_City + Match_Postal + Match_F_L + Match_F_I + Match_P_L + Match_P_I + Match_S_L + Match_S_I
  )

#Count the number of rows where the sum of the dummies is equal to 1
Unique <- Dummy_combinations %>%
  filter(Sum_Dummies == 1)

#Create a column equal to 1 if there is a match in any of the columns and 0 if not
EPRTR_Data_wide <- EPRTR_Data_wide %>%
  mutate(
    ETS_Match = ifelse(ETS_ID == 1 | EPER_ID == 1 | Match_City == 1 | Match_Postal == 1 | Match_F_L == 1 | Match_F_I == 1 | Match_P_L == 1 | Match_P_I == 1 | Match_S_L == 1 | Match_S_I == 1, 1, 0)
  )

#Count the number of matches
EPRTR_Data_wide %>%
  group_by(ETS_Match) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

#For each dummy variable, count the number of matches using summary
dummy_cols <- c("ETS_ID", "EPER_ID",
                "Match_City", "Match_Postal",
                "Match_F_L", "Match_F_I",
                "Match_P_L", "Match_P_I",
                "Match_S_L", "Match_S_I",
                "ETS_Match")

EPRTR_Data_wide %>%
  summarise(across(all_of(dummy_cols),
                   ~ mean(.x == 1, na.rm = TRUE)
  ))

unique_counts <- Unique %>%
  summarise(across(
    c(ETS_ID, EPER_ID,
      Match_City, Match_Postal,
      Match_F_L, Match_F_I,
      Match_P_L, Match_P_I,
      Match_S_L, Match_S_I),
    ~ sum(. == 1),
    .names = "{.col}_count1"
  ))

dist_sum <- Dummy_combinations %>%
  count(Sum_Dummies) %>%
  arrange(Sum_Dummies)


EPRTR_Data_wide %>% 
  summarise(across(all_of(dummy_cols),
                   ~ sum(.x == 1, na.rm = TRUE),
                   .names = "{.col}_count"))

Observe <- EPRTR_Data_wide %>%
  summarise(across(
    all_of(dummy_cols),
    list(
      count = ~ sum(. == 1, na.rm = TRUE),
      rate  = ~ mean(. == 1, na.rm = TRUE)
    ),
    .names = "{.col}_{.fn}"
  ))

summary(EPRTR_Data_wide)

######################################### Working on the large dataset ########################################################
####Create a dummy for the parent company
EPRTR_Data_wide <- EPRTR_Data_wide %>%
  group_by(Parent_Name) %>%
  mutate(
    tmp = any(ETS_Match == 1, na.rm = TRUE),        # TRUE/FALSE per parent
    Parent_ETS_dummy = case_when(
      is.na(Parent_Name)   ~ ETS_Match,            # keep NA for missing parents
      tmp                  ~ 1L,                     # if any child matched
      TRUE                 ~ 0L                      # otherwise
    )
  ) %>%
  ungroup() %>%
  select(-tmp)

#Summarize the data
summary(EPRTR_Data_wide)
#I would like to see the rows where ETS_Match is not equel to Parent_ETS_Dummy
Observe <- EPRTR_Data_wide %>%
  filter(is.na(Parent_Name))

####Prepare data for matching####
#Create sector codes
EPRTR_Data_wide <- EPRTR_Data_wide %>%
  mutate(
    Sector = as.integer(str_extract(Subsector, "^\\d+"))
  )

#Create distinct dataset
EPRTR_Data_wide_distinct <- EPRTR_Data_wide %>%
#  filter(!is.na(CO2_AIR)) %>%
#  filter(Sector !=7) %>%
  select(Country, Facility_ID,ETS_Match) %>%
  distinct()

#Create a region map
region_map <- tribble(
  ~Country, ~Region,
  
  # Northern Europe
  "DK", "Northern Europe",  # Denmark
  "EE", "Northern Europe",  # Estonia
  "FI", "Northern Europe",  # Finland
  "GB", "Northern Europe",  # United Kingdom
  "IE", "Northern Europe",  # Ireland
  "LV", "Northern Europe",  # Latvia
  "LT", "Northern Europe",  # Lithuania
  "NO", "Northern Europe",  # Norway
  "IS", "Northern Europe",  # Iceland
  "SE", "Northern Europe",  # Sweden
  
  # Eastern Europe
  "BG", "Eastern Europe",   # Bulgaria
  "HU", "Eastern Europe",   # Hungary
  "PL", "Eastern Europe",   # Poland
  "RO", "Eastern Europe",   # Romania
  "SK", "Eastern Europe",   # Slovakia
  "CZ", "Eastern Europe",   # Czech Republic
  
  # Southern Europe
  "AL", "Southern Europe",  # Albania (if you have it)
  "AD", "Southern Europe",  # Andorra
  "BA", "Southern Europe",  # Bosnia–Herzegovina
  "GI", "Southern Europe",  # Gibraltar
  "GR", "Southern Europe",  # Greece
  "IT", "Southern Europe",  # Italy
  "HR", "Southern Europe",  # Croatia
  "MK", "Southern Europe",  # North Macedonia
  "MT", "Southern Europe",  # Malta
  "ME", "Southern Europe",  # Montenegro
  "PT", "Southern Europe",  # Portugal
  "RS", "Southern Europe",  # Serbia
  "SI", "Southern Europe",  # Slovenia
  "ES", "Southern Europe",  # Spain
  "CY", "Southern Europe",  # Cyprus
  
  # Western Europe
  "BE", "Western Europe",   # Belgium
  "DE", "Western Europe",   # Germany
  "FR", "Western Europe",   # France
  "LI", "Western Europe",   # Liechtenstein
  "LU", "Western Europe",   # Luxembourg
  "MC", "Western Europe",   # Monaco
  "NL", "Western Europe",   # Netherlands
  "AT", "Western Europe",   # Austria
  "CH", "Western Europe"    # Switzerland
)

# Join onto your data:
EPRTR_Data_wide <- EPRTR_Data_wide %>%
  left_join(region_map, by = "Country")


# And check:
EPRTR_with_regions <- EPRTR_Data_wide_distinct %>%
  left_join(region_map, by = "Country")

EPRTR_with_regions %>%
  count(Region) %>%
  arrange(desc(n))

print(EPRTR_with_regions %>%
  count(Country) %>%
  arrange(desc(n)),n=32)


Regions_balance <- EPRTR_with_regions %>%
  group_by(Region,ETS_Match) %>%
  summarise(count = n(),
            .groups = "drop") %>%
  arrange(desc(count))

#Group by subsector
EPRTR_Data_wide_Subsector <- EPRTR_Data_wide %>%
  #  filter(!is.na(CO2_AIR)) %>%
  #  filter(Sector !=7) %>%
  select(Country, Facility_ID,ETS_Match,Subsector,Sector) %>%
  distinct()


EPRTR_Data_wide_Subsector2 <- EPRTR_Data_wide_Subsector %>%
  group_by(Subsector,ETS_Match) %>%
  summarise(count = n(),
            .groups = "drop") %>%
  arrange(desc(count))

# 1) Define your mapping: old → new
Subsector_mapping <- tribble(
  ~Subsector,    ~Subsector_New,
  
  # 1 Energy Sector
  "1(a)",        "1abdf",  
  "1(b)",        "1abdf",  
  "1(d)",        "1abdf",  
  "1(e)",        "1abdf",  
  "1(f)",        "1abdf",  
  "1(c)",        "1(c)",  
  
  # 2 Production and processing of metals
  "2(a)",        "2bad",
  "2(b)",        "2bad",
  "2(c)",        "2(c)",
  "2(c)(i)",     "2(c)",
  "2(c)(ii)",    "2(c)",
  "2(c)(iii)",   "2(c)",
  "2(d)",        "2bad",
  "2(e)",        "2(e)",
  "2(e)(i)",     "2(e)",
  "2(e)(ii)",    "2(e)",
  "2(f)",        "2(f)",
  
  # 3 Minerals & Cement
  "3(a)",        "3ab",
  "3(b)",        "3ab",
  "3(c)",        "3(c)",
  "3(c)(i)",     "3(c)",
  "3(c)(ii)",    "3(c)",
  "3(c)(iii)",   "3(c)",
  "3(e)",        "3ef",
  "3(f)",        "3ef",
  "3(g)",        "3(g)",
  
  # 4 Chemicals
  "4(a)",        "4(a)",
  "4(a)(i)",     "4(a)",
  "4(a)(ii)",    "4(a)",
  "4(a)(iii)",   "4(a)",
  "4(a)(iv)",    "4(a)",
  "4(a)(v)",     "4(a)",
  "4(a)(vi)",    "4(a)",
  "4(a)(vii)",   "4(a)",
  "4(a)(viii)",  "4(a)",
  "4(a)(ix)",    "4(a)",
  "4(a)(x)",     "4(a)",
  "4(a)(xi)",    "4(a)",
  "4(b)",        "4(b)",
  "4(b)(i)",     "4(b)",
  "4(b)(ii)",    "4(b)",
  "4(b)(iii)",   "4(b)",
  "4(b)(iv)",    "4(b)",
  "4(b)(v)",     "4(b)",
  "4(c)",        "4cdef",
  "4(d)",        "4cdef",
  "4(e)",        "4cdef",
  "4(f)",        "4cdef",
  
  # 5 Waste & Water treatment
  "5(a)",        "5(a)",
  "5(b)",        "5bc",
  "5(c)",        "5bc",
  "5(d)",        "5(d)",
  "5(e)",        "5eg",
  "5(f)",        "5(f)",
  "5(g)",        "5eg",
  
  # 6 Pulp & Paper
  "6(a)",        "6bac",
  "6(b)",        "6bac",
  "6(c)",        "6bac",
  
  # 7 Agriculture & Aquaculture
  "7(a)",        "7ab",
  "7(a)(i)",     "7ab",
  "7(a)(ii)",    "7ab",
  "7(a)(iii)",   "7ab",
  "7(b)",        "7ab", #Skal lige overvejes senere om det er en god idé
  
  # 8 Food & Beverage
  "8(a)",        "8ac",
  "8(b)",        "8(b)",
  "8(b)(i)",     "8(b)",
  "8(b)(ii)",    "8(b)",
  "8(c)",        "8ac",
  
  # 9 Other Industries
  "9(a)",        "9(a)",
  "9(b)",        "9(b)",
  "9(c)",        "9(c)",
  "9(d)",        "9(d)",
  "9(e)",        "9(e)"
)

# 2) Apply it
EPRTR_balanced <- EPRTR_Data_wide_Subsector %>%
  left_join(Subsector_mapping, by = "Subsector")

Subsector_balance <- EPRTR_balanced %>%
  group_by(Subsector_New,ETS_Match) %>%
  summarise(count = n(),
            .groups = "drop") %>%
  arrange(desc(count))

#Count the number of distinct subsectors
nrow(EPRTR_Data_wide_Subsector %>%
  select(Subsector) %>%
  distinct())

# Join onto your data:
EPRTR_Data_wide <- EPRTR_Data_wide %>%
  left_join(Subsector_mapping, by = "Subsector")

EPRTR_Data_wide <- EPRTR_Data_wide %>%
  select(-c(Site_ID,Site_Name,Street_Name,Building_Number,ETS_ID,EPER_ID,Postal_Code,City,Match_City,Match_Postal,Match_F_L,Match_F_I,Match_P_L,Match_P_I,Match_S_L,Match_S_I))

merged_data <- EPRTR_Data_wide %>%
  rename(Tph_Water = TOTALPHOSPHORUS_WATER,
         Zn_Water = ZNANDCOMPOUNDS_WATER,
         Zn_Air = ZNANDCOMPOUNDS_AIR,
         Nox = NOX_AIR,
         Nickel_Water = NIANDCOMPOUNDS_WATER,
         Nickel_Air = NIANDCOMPOUNDS_AIR,
         TNitrogen_water = TOTALNITROGEN_WATER,
         Sox = SOX_AIR,
         Nh3 = NH3_AIR,
         Ch4 = CH4_AIR,
         N2o = N2O_AIR,
         Pfc = PFCS_AIR,
         Co2ExclBio = CO2EXCLBIOMASS_AIR,
         Co2 = CO2_AIR,
         Toc_Water = TOC_WATER,
         Firm = Facility_ID,
         Subsector_Original = Subsector,
         Subsector = Subsector_New,
         ETS = ETS_Match,
         year = Year)


colnames(merged_data)

library(dplyr)
library(purrr)


# 1) first, compute all your empirical thresholds (say 95th percentile folds):
pollutant_vars <- c("Co2","Toc_Water","Tph_Water","Zn_Water","Nox",
                    "Nickel_Water","TNitrogen_water","Sox","Zn_Air",
                    "Nh3","Ch4","Nickel_Air","N2o","Pfc","Co2ExclBio")

thresholds <- map_dbl(pollutant_vars, function(var) {
  merged_data %>%
    arrange(Firm, year) %>%
    group_by(Firm) %>%
    mutate(prev = lag(.data[[var]])) %>%
    filter(!is.na(prev) & prev > 0 & .data[[var]] > 0) %>%
    transmute(
      fold = if_else(
        (.data[[var]]/prev) < 1,
        prev/.data[[var]],
        .data[[var]]/prev
      )
    ) %>%
    pull(fold) %>%
    quantile(0.95, na.rm = TRUE)
})
names(thresholds) <- pollutant_vars

# 2) now loop through each pollutant and blank out only those "sandwiched spikes":
cleaned_data <- merged_data

for(var in pollutant_vars){
  thr <- thresholds[var]
  cleaned_data <- cleaned_data %>%
    arrange(Firm, year) %>%
    group_by(Firm) %>%
    mutate(
      prev = lag(.data[[var]]),
      lead = lead(.data[[var]]),
      
      # fold‐change relative to previous year
      fold_prev = if_else(
        !is.na(prev) & prev > 0 & .data[[var]] > 0,
        if_else(.data[[var]]/prev < 1, prev/.data[[var]], .data[[var]]/prev),
        NA_real_
      ),
      # fold‐change relative to lead year
      fold_lead = if_else(
        !is.na(lead) & lead > 0 & .data[[var]] > 0,
        if_else(lead/.data[[var]] < 1, .data[[var]]/lead, lead/.data[[var]]),
        NA_real_
      ),
      
      # if both jumps exceed the threshold, blank out the middle year
      !!sym(var) := if_else(
        !is.na(fold_prev) & !is.na(fold_lead) &
          fold_prev > thr & fold_lead > thr,
        NA_real_,
        .data[[var]]
      )
    ) %>%
    ungroup() %>%
    select(-prev, -lead, -fold_prev, -fold_lead)
}

thresholds

#Compare the number of new na by comparing merged_data and cleaned_data
new_na <- map_dbl(pollutant_vars, function(var) {
  sum(is.na(cleaned_data[[var]])) - sum(is.na(merged_data[[var]]))
})

new_na

merged_data <- cleaned_data

##########Further cleaning#########
emission_var <- "Co2" # Set the emission variable to Co2

############Save the data###########
save(merged_data, file = "C:/Users/Tobyd/OneDrive - Aarhus universitet/Uni-livet/Kandidatgrad/10. Semester/Specialet/Data/Final data/merged_data.RData")