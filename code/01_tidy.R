# Read data
# school-shooting-analysis/code/
# Melissa Juarez

## Research Question: Using a logit model, how do X, Y, and Z variables affect the 
## likelhihood that there will be a victim in a school shooting incident?

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


######### AGGREGATION ###########

## For each shooting incident, how many total victims were there? How are the number of victims per incident distributed?
victims_aggregation <- victim_df %>%
  group_by(incidentid) %>% 
  mutate(victim_count = n()) %>%
  distinct(incidentid, .keep_all = TRUE) %>%
  select(incidentid, victim_count)

## merge victim aggregation with incident_df
incident_df <- incident_df %>% merge(victims_aggregation, by.x = "incident_id", by.y = "incidentid", all.x=TRUE)
incident_df$victim_count[is.na(incident_df$victim_count)] <- 0

table(incident_df$victim_count)

## For each shooting incident, how many total shooters were there?
shooters_aggregation <- shooter_df %>%
  group_by(incidentid) %>% 
  mutate(shooter_count = n()) %>%
  distinct(incidentid, .keep_all = TRUE) %>%
  select(incidentid, shooter_count)

## merge victim aggregation with incident_df
incident_df <- incident_df %>% merge(shooters_aggregation, by.x = "incident_id", by.y = "incidentid", all.x=TRUE)
## some incidents have no specified shooter in the shooter_df; will keep them marked as NA


## aggregation of weapons used
# the goal is to retain information on if there were multiple weapons at the incident, and what category of weapons there were

table(weapon_df$weapontype)

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

# merge weapon aggregation to incident_df
incident_df <- incident_df %>% merge(weapons_agg, by.x = "incident_id", by.y = "incidentid", all.x=TRUE)

# create columns `handgun_used`, `rifle_used`, `shotgun_used`, `multiple_weapons`, based on weapontypes list
incident_df <- incident_df %>%
  mutate(handgun_used = case_when( 
    is.na(weapontypes) ~ NA_real_,
    stringr::str_detect(weapontypes, "Handgun") ~ 1,
    !(stringr::str_detect(weapontypes, "Handgun")) & !is.na(weapontypes) ~ 0,
  ), rifle_used = case_when( 
    is.na(weapontypes) ~ NA_real_,
    stringr::str_detect(weapontypes, "Rifle") ~ 1,
    !(stringr::str_detect(weapontypes, "Rifle")) & !is.na(weapontypes) ~ 0,
  ), shotgun_used = case_when( 
    is.na(weapontypes) ~ NA_real_,
    stringr::str_detect(weapontypes, "Shotgun") ~ 1,
    !(stringr::str_detect(weapontypes, "Shotgun")) & !is.na(weapontypes) ~ 0,
  ), multiple_weapons = case_when(
    is.na(weapontypes)  ~ NA_real_,
    weapon_entries > 1 | stringr::str_detect(weapontypes, "Multiple") ~ 1,
    ###### left off here: have to figure out how to include unknown/no data/other/
  )
  )





