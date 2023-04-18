# Function definitions
# school-shooting-analysis/R/
# Melissa Juarez

# Use this file for storing functions to be used in the script files

one_var_tabyl <- function(df, var) {
  var = enquo(var)
  
  table <- df %>% janitor::tabyl(!!var) %>%
    arrange(desc(percent)) %>% 
    mutate(count = scales::comma(n, accuracy = 1),
           percent = scales::percent(percent, accuracy = 0.01)) %>%
    select(!!var, count, percent)
  
  return(table)
}

k <- function(table) {
  
  table <- table %>%
    knitr::kable() %>%
    kableExtra::kable_styling() %>%
    kableExtra::scroll_box(height = "400px")
  
  return(table)
  
}
