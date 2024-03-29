# Read data
# school-shooting-analysis/code/
# Melissa Juarez

library(here)
library(readxl)
library(dplyr)
library(ggplot2)

######### READ DATA ###########
## data has 4 tabs: INCIDENT, SHOOTER, VICTIM, WEAPON
incident_df <- read_excel(here::here("data/SSDB_Raw_Data_2022.xlsx"), sheet = "INCIDENT") %>% janitor::clean_names()
shooter_df <- read_excel(here::here("data/SSDB_Raw_Data_2022.xlsx"), sheet = "SHOOTER") %>% janitor::clean_names()
victim_df <- read_excel(here::here("data/SSDB_Raw_Data_2022.xlsx"), sheet = "VICTIM") %>% janitor::clean_names()
weapon_df <- read_excel(here::here("data/SSDB_Raw_Data_2022.xlsx"), sheet = "WEAPON") %>% janitor::clean_names()

######### CLEAN DATA ###########

## replace 'null' string values with N/As
incident_df[incident_df == 'null'] <- NA
shooter_df[shooter_df == 'null'] <- NA
victim_df[victim_df == 'null'] <- NA
weapon_df[weapon_df == 'null'] <- NA

incident_df[incident_df == 'N/A'] <- NA
shooter_df[shooter_df == 'N/A'] <- NA
victim_df[victim_df == 'N/A'] <- NA
weapon_df[weapon_df == 'N/A'] <- NA

incident_df$domestic_violence[incident_df$domestic_violence == 'NO'] <- 'No'


## fix date formats to date
incident_df$date <- as.Date(incident_df$date)
incident_df$yr_since_1970 <- as.numeric(format(incident_df$date,'%Y')) - 1970

########################### AGGREGATION #############################
# The goal of this is to create one dataframe used for our analysis
# that contains relevant information on the victims, weapons, and shooters,
# for each incident. currently, each of the victims, weapons, and shooters
# dataframes contains information on the victim, weapon, and shooter level,
# rather than at the incident level.

######### VICTIMS AGGREGATION ###########

## For each shooting incident, how many total victims were there? How are the number of victims per incident distributed?
victims_aggregation <- victim_df %>%
  group_by(incidentid) %>% 
  mutate(victim_count = n()) %>%
  distinct(incidentid, .keep_all = TRUE) %>%
  select(incidentid, victim_count) %>%
  mutate(any_victims = case_when(
    victim_count == 0 ~ 0,
    victim_count > 0 ~ 1
  ), multi_victim = case_when(
    victim_count <= 1 ~ 0,
    victim_count > 1 ~ 1
  )
  )

## merge victim aggregation with incident_df
model_df <- incident_df %>% merge(victims_aggregation, by.x = "incident_id", by.y = "incidentid", all.x=TRUE)
model_df$victim_count[is.na(model_df$victim_count)] <- 0
model_df$any_victims[is.na(model_df$any_victims)] <- 0


table(model_df$victim_count, useNA = "ifany")

######### SHOOTERS AGGREGATION ###########
# the goal is to retain information on if there were multiple shooters at the incident, 
# and how many shooters of a certain gender were the incident

table(shooter_df$gender, useNA = "ifany")
## (1) For each shooting incident, how many total shooters were there & what were their genders?
shooters_agg <- shooter_df %>%
  group_by(incidentid) %>% 
  mutate(shooter_count = n(),
         shootersexes = paste(gender, collapse = ", "),
         shooterages = paste(age, collapse = ", ")
         ) %>%
  distinct(incidentid, .keep_all = TRUE) %>%
  select(incidentid, shooter_count, shootersexes, shooterages)

# look at distribution of number of shooters and ages per incident
table(shooters_agg$shooter_count, useNA = "ifany")

# filter to only look at instances of single-shooters
single_shooters_agg <- shooters_agg %>% 
  filter(shooter_count == 1) %>%
  mutate(sex = case_when(
    stringr::str_count(shootersexes, "Male") == TRUE ~ 0,
    stringr::str_count(shootersexes, "Female") == TRUE ~ 1
  ))
  
#shooters_agg$shootersexes <- gsub(', NA','',shooters_agg$shootersexes)
single_shooters_agg$shootersexes <- ifelse(stringr::str_detect(single_shooters_agg$shootersexes, "NA"), 
                                  NA, single_shooters_agg$shootersexes)
single_shooters_agg$shooterages <- ifelse(stringr::str_detect(single_shooters_agg$shooterages, "NA"), 
                                           NA, single_shooters_agg$shooterages)

table(single_shooters_agg$shootersexes, useNA = "ifany")
table(single_shooters_agg$shooterages, useNA = "ifany")

# For each incident, was the shooter underage and therefore in illegal possession of a gun?
# For these purposes, anyone under 18 possessing of a gun is considered unlawful.

single_shooters_agg <- single_shooters_agg %>% 
  mutate(minor = case_when(
    shooterages %in% c('5', '6', '7', '8', '9', '10','11','12','13','14','15','16','17', 
                       'Child', 'Minor','Teen') ~ 1,
    !(shooterages %in% c('5', '6', '7', '8', '9', '10','11','12','13','14','15','16','17', 
                         'Child', 'Minor','Teen')) ~ 0
  ))

## merge victim aggregation with model_df
model_df <- model_df %>% 
  merge(single_shooters_agg, by.x = "incident_id", by.y = "incidentid", all.x=TRUE) %>%
  filter(!is.na(shooter_count) & shooter_count == 1)


######### WEAPONS AGGREGATION ###########
# the goal is to retain information on if there were multiple weapons at the incident, and what category of weapons there were

table(weapon_df$weapontype, useNA = "ifany")

# weapon_df <- weapon_df %>%
#   mutate(weapontype_agg = case_when(
#     weapontype %in% c("Handgun", "Multiple Handguns") ~ "Handgun(s)",
#     weapontype %in% c("Rifle", "Multiple Rifles") ~ "Rifle(s)",
#     weapontype == "Shotgun" ~ "Shotgun(s)",
#     weapontype == "Other" ~ "Other",
#   ), multiple_weapons = case_when(
#     weapontype %in% c('Multiple Rifles', 'Multiple Unknown', 'Multiple Handguns') ~ 1,
#     weapontype %in% c('Handgun', 'Rifle', 'Shotgun') ~ 0,
#   ))

weapons_agg <- weapon_df %>% 
  group_by(incidentid) %>% 
  mutate(weapon_entries = n(),
         weapontypes = paste(weapontype, collapse = ", ")) %>%
  distinct(incidentid, .keep_all = TRUE) %>%
  select(incidentid, weapon_entries, weapontypes)

# code string NAs  and "No Data" entries as <NA>
weapons_agg$weapontypes <- ifelse(stringr::str_detect(weapons_agg$weapontypes, "NA|No Data"), 
                                  NA, weapons_agg$weapontypes)

# merge weapon aggregation to model_df
model_df <- model_df %>% merge(weapons_agg, by.x = "incident_id", by.y = "incidentid", all.x=TRUE)
table(incident_df$weapontypes, useNA = "ifany")

# create columns `handgun_used`, `rifle_used`, `shotgun_used`, `multiple_weapons`, based on weapontypes list & length of list
model_df <- model_df %>%
  mutate(handgun_used = case_when( 
    is.na(weapontypes) ~ NA_real_,
    stringr::str_detect(weapontypes, "Handgun") ~ 1,
    !(stringr::str_detect(weapontypes, "Handgun")) ~ 0,
  ), 
  rifle_used = case_when( 
    is.na(weapontypes) ~ NA_real_,
    stringr::str_detect(weapontypes, "Rifle") ~ 1,
    !(stringr::str_detect(weapontypes, "Rifle")) ~ 0,
  ), 
  shotgun_used = case_when( 
    is.na(weapontypes) ~ NA_real_,
    stringr::str_detect(weapontypes, "Shotgun") ~ 1,
    !(stringr::str_detect(weapontypes, "Shotgun")) ~ 0,
  ), 
  multiple_weapons = case_when(
    is.na(weapontypes)  ~ NA_real_,
    weapon_entries > 1 | stringr::str_detect(weapontypes, "Multiple") ~ 1,
    !(weapon_entries > 1 | stringr::str_detect(weapontypes, "Multiple")) ~ 0
  )
  )



### Recoding model_df for no = 0, yes = 1

model_df <- model_df %>% mutate(
   during_school = case_when(
     during_school == "No" ~ 0,
     during_school == "Yes" ~ 1
   ),
   bullied = case_when(
     bullied == "No" ~ 0,
     bullied == "Yes" ~ 1
   ),
   preplanned = case_when(
     preplanned == "No" ~ 0,
     preplanned == "Yes" ~ 1
   )
)


