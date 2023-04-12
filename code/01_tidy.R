# Read data
# school-shooting-analysis/R/
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

ggplot(incident_df, aes(x=as.numeric(sub("%", "",victim_count,fixed=TRUE)))) + 
  geom_histogram(aes(y=..density..), color="darkblue", fill="lightblue", binwidth = 1) + 
  geom_density(alpha=.2, fill="#FF6666") +
  geom_vline(aes(xintercept=median(as.numeric(sub("%", "",victim_count,fixed=TRUE)))),
             color="black", linetype="dashed", size=0.5) +
  labs(title="Distribution of Victim Count for School Shooting resulting in more than 1 victim",
       x ="Victim Count", y = "Density")

##
## Most school shooting between 1970-2022 between 0 and 2 victims.
##

