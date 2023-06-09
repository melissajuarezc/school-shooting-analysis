## Code
This directory contains the R code used in the School Shooting Analysis project. The code is divided into three files:

`01_tidy.R`: This script reads in the raw data and cleans and transforms the data into a tidy format. 
`02_analysis.R`: This file reads in the tidy data and creates linear models to better understand the variables and their relationships. 

Each script is annotated with comments explaining the code. To run the analysis, you should run the scripts in the order of the naming conventions.

### Requirements
The code was written and tested using R version 4.1.0. The following packages are required to run the code:

- `tidyverse`: for data cleaning and transformation
- `ggplot2`: for data visualization
- `knitr`: for generating reports
- `kableExtra`: for formatting tables
- `readxl`: for reading Excel files
- `dplyr`: for data manipulation


You can install these packages using the following code:

`install.packages(c("tidyverse", "ggplot2", "knitr", "kableExtra", "readxl", "dplyr"))`
