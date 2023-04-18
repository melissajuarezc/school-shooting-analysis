# explore data to find variables of interest
# school-shooting-analysis/code/
# Melissa Juarez

## Research Question: Using a logit model, how do X, Y, and Z variables affect the 
## likelihood that there will be a victim in a school shooting incident?

library(here)
library(readxl)
library(dplyr)
library(ggplot2)

source(here::here("code/01_tidy.R"))
source(here::here("code/functions.R"))

## Our Dependent Variable will be victim counts, so let's explore the distribution of victim counts
## for school schootings 1970-2022

table(incident_df$victim_count)
ggplot(incident_df, aes(x=as.numeric(sub("%", "",victim_count,fixed=TRUE)))) + 
  geom_histogram(aes(y=..density..), color="darkblue", fill="lightblue", binwidth = 1) + 
  geom_density(alpha=.2, fill="#FF6666") +
  geom_vline(aes(xintercept=median(as.numeric(sub("%", "",victim_count,fixed=TRUE)))),
             color="black", linetype="dashed", size=0.5) +
  labs(title="Distribution of Victim Count for School Shooting resulting in more than 1 victim",
       x ="Victim Count", y = "Density")
# Most school shooting between 1970-2022 between 0 and 2 victims.


## POSSIBLE INDEPENDENT VARIABLES

### school level
one_var_tabyl(incident_df, school_level) %>% k()

###
one_var_tabyl(incident_df, school_level) %>% k()
