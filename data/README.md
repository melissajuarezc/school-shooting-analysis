## Data
This directory contains the raw data file used in the School Shooting Analysis project.

### Source
The primary dataset used in this project is the Shooting Incidents at K-12 Schools dataset, which is publicly available from the Center for Homeland Defense and Security.

Center for Homeland Defense and Security. (2022). Shooting Incidents at K-12 Schools (January 1970 - June 2022). Naval Postgraduate School, Center for Homeland Defense and Security.

### About the Dataset
The dataset is provided in a CSV format and is named SSDB_Raw_Data_2022.csv. The file contains information on over 1,000 shooting incidents that have occurred at K-12 schools in the United States since 1970.

The dataset is comprised of 4 sub-datasets:
`incident`: incident-specific information such as date and location of the incident
`shooter`: demographic information and charges files for the shooter(s)
`victom`: information about each victim, their injuries, and their demographics
`weapon`: weapon type and caliber

Each dataset is related to eachother through `incident_id`, a unique ID specific to each event of a school shooting.

### Contact
If you have any questions or concerns about the data, please contact the Center for Homeland Defense and Security.