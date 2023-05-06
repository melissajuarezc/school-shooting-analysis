# Analyze Data & Create Models
# school-shooting-analysis/code/
# Melissa Juarez

library(here)
library(readxl)
library(dplyr)
library(ggplot2)

## source functions and variables from prior scripts
source(here::here("code/01_tidy.R"))

##########  Visualizing Missingness ########## 
# to select variables of interest
library(naniar)
vis_miss(incident_df)
vis_miss(shooter_df)
vis_miss(victim_df)
vis_miss(weapon_df)

# visualize our target dataframe, which contains recoded and aggregated variables
# at the incident-level of analysis

model_df <- model_df %>% 
  select(yr_since_1970, bullied, minor, sex, shootersexes, multiple_weapons, handgun_used, 
         rifle_used, shotgun_used, victim_count, multi_victim)
vis_miss(model_df)

## we see that our dataframe has 12.5% missingness


########## GUN EFFECTS MODEL ########## 

# describe variables
model_df %>% 
  select(yr_since_1970, victim_count, multiple_weapons, handgun_used, 
         rifle_used, shotgun_used, minor) %>%  # keep only the columns of interest
  tbl_summary(type = list(yr_since_1970   ~ "continuous",
                          victim_count     ~ "continuous",
                          multiple_weapons      ~ "categorical",
                          handgun_used      ~ "categorical",
                          rifle_used      ~ "categorical",
                          shotgun_used      ~ "categorical",
                          minor      ~ "categorical"
  ),
  statistic = list(all_continuous() ~ "{mean} ({sd})",       
                   all_categorical() ~ "{n} / {N} ({p}%)"),
  missing_text = "N/A") %>%
  modify_header(label = "**Variable**") %>%
  modify_caption("Gun Model Variables") %>%
  bold_labels()

# create initial linear model
attach(model_df)
g1 <- lm(victim_count ~ handgun_used + rifle_used + shotgun_used)
summary(g1)

## handgun, rifle, and shotgun are expected to be more deadly compared to cases with unknown/other weapon used
## among the three, rifle has the strongest effect on victimhood, handgun & shotgun held constant

## what happens when I control for instances where the shooter has multiple weapons?
g2 <- lm(victim_count ~ handgun_used + rifle_used + shotgun_used + multiple_weapons)
summary(g2)

## minors & year
g3 <- lm(victim_count ~ handgun_used + rifle_used + shotgun_used + multiple_weapons + minor + yr_since_1970)
summary(g3)

## interaction between quantity and type of gun?
g4 <- lm(victim_count ~ handgun_used*multiple_weapons + rifle_used*multiple_weapons + shotgun_used*multiple_weapons)
summary(g4)


## which model reduces RSE and fits our data better?
## the fact that g4 reduces to g2 means that we can test a hypothesis where the null is that the model 
## g2 is an adequate fit for the data and the alternative is the full model g4, as we'll test below.
anova(g2, g4)

## Based on the lack of fit test of the two models, we get an F-statistic of 70.258 and a p-value of less 
## than 0.05. Thus, there is compelling evidence for us to reject the hypothesis that the reduced g2 model
## fits our data well.


## model with only significant terms
guns_df <- model_df %>%
  select(victim_count, multiple_weapons, rifle_used, handgun_used, shotgun_used) %>%
  na.omit()

rownames(guns_df) <- 1:nrow(guns_df)

g5 <- lm(victim_count ~ multiple_weapons + rifle_used*multiple_weapons + 
           handgun_used:multiple_weapons + shotgun_used:multiple_weapons, data = guns_df)
summary(g5)


## Diagnostics
cd <- cooks.distance(g5); cd
plot(cd)
ot <- outlierTest(g5) 

ot_df <- as.data.frame(do.call(cbind, ot))
outliers <- merge(guns_df, ot_df, by = 0)
outliers$Row.names <- as.numeric(outliers$Row.names)

par(mfrow=c(2,2))
plot(g5)

# VIF
car::vif(g5, type = 'predictor')

## considering quasipoisson?
# visualize the outcome
ggplot(model_df, aes(x=victim_count)) + 
  geom_histogram(aes(y=..density..), color="darkblue", fill="lightblue", binwidth = 1) + 
  scale_y_continuous(
    
    # Features of the first axis
    name = "Density",
    
    # Add a second axis and specify its features
    sec.axis = sec_axis( trans=~.*2069, name="Frequency")
  ) + 
  geom_density(alpha=.2, fill="#FF6666") +
  geom_vline(aes(xintercept=mean(victim_count)),
             color="black", linetype="dashed", size=0.5) +
  xlim(-1,20) + 
  labs(title="Distribution of Victim Count for School Shooting",
       x ="Victim Count", y = "Density")

g6.poi <- glm(victim_count ~ rifle_used*multiple_weapons + shotgun_used*multiple_weapons, data = model_df, family = quasipoisson())
summary(g6.poi)

########## MENTAL HEALTH EFFECTS MODEL ########## 

# describe variables
library(gtsummary)
# describe variables
model_df %>% 
  select(yr_since_1970, victim_count, bullied, shootersexes) %>%  # keep only the columns of interest
  tbl_summary(type = list(yr_since_1970   ~ "continuous",
                          victim_count     ~ "continuous",
                          bullied      ~ "categorical",
                          shootersexes      ~ "categorical"),
              statistic = list(all_continuous() ~ "{mean} ({sd})",        # stats and format for continuous columns
                               all_categorical() ~ "{n} / {N} ({p}%)"),
              missing_text = "N/A",
              label = list(shootersexes  ~ "sex"))  %>%
  modify_header(label = "**Variable**") %>%
  modify_caption("Mental Health Model Variables") %>%
  bold_labels()


# create initial linear model
attach(model_df)
m1 <- lm(victim_count ~ bullied)
summary(m1)

## what happens when I control for gender and year?
m2 <- lm(victim_count ~ bullied + yr_since_1970 + sex)
summary(m2)

## take away sex bc not significant
m3 <- lm(victim_count ~ bullied + yr_since_1970)
summary(m3)

## interaction term?
m4 <- lm(victim_count ~ bullied *yr_since_1970 )
summary(m4)

## which fit is better?
anova(m3,m4)
## not enough evidence to say that the interaction effects model is a better fit for the data.

mentalhealth_df <- model_df %>%
  select(victim_count, bullied, yr_since_1970) %>%
  na.omit()

rownames(mentalhealth_df) <- 1:nrow(mentalhealth_df)


## Diagnostics
cd2 <- cooks.distance(m3); cd
plot(cd2)
ot2 <- outlierTest(m3) 

ot2_df <- as.data.frame(do.call(cbind, ot2))
outliers2 <- merge(mentalhealth_df, ot2_df, by = 0)
outliers2$Row.names <- as.numeric(outliers2$Row.names)

par(mfrow=c(2,2))
plot(m3)

# VIF
car::vif(m3)












