rm(list = ls())
cat("\014") 
graphics.off()
options(scipen = -1)

setwd("C:/Users/Tobyd/OneDrive - Aarhus universitet/Uni-livet/Kandidatgrad/10. Semester/Specialet/Data/Final data/")

#Install all packages
library(dplyr)
library(ggplot2)
library(broom)
library(estimatr)
library(fixest)
library(stringr)
library(readr)   # for parse_number()

# ----------------------------------------
# 1) THE ANALYSIS FUNCTION
# ----------------------------------------
analyze_full <- function(df, emission_var) {
  df <- df %>% 
    filter(year >= 2008 & year <= 2023) %>%
    mutate(phase3_post = ifelse(year >= 2013 & year <= 2020, 1, 0),
           phase4_post = ifelse(year >= 2021, 1, 0))
  
  # 2. Remove rows with NA or 0 in the chosen emission variable
  df <- df %>%
    filter(!is.na(.data[[emission_var]]) & .data[[emission_var]] != 0) %>%
    mutate(emission_log = log(.data[[emission_var]]))
  
  #two‐period DiD 3 | 4
  did_plain <- feols(
    emission_log ~ ETS + phase3_post + ETS:phase3_post + ETS:phase4_post + phase4_post,
    data     = df
    ) %>% tidy()
  
  did_fe_sector <- feols(
    emission_log ~ ETS:phase3_post + ETS:phase4_post | Firm + year + Sector:year,
    data     = df
  ) %>% tidy()
  
  sum_df1  = nrow(df)
  n_firmdf1 = n_distinct(df$Firm)
  
  list(
    did_plain                      = did_plain,
    did_fe_sector                  = did_fe_sector,
    n_obs  = sum_df1,
    n_firms = n_firmdf1
  )
}

# ----------------------------------------
# 2) Load the data
# ----------------------------------------
load("merged_data.RData")

#Loop over merged_data
# ----------------------------------------
# 0) Define list of emission variables
# ----------------------------------------
polluters <- c(
  "Ch4", "Co2", "Co2ExclBio", "N2o", 
  "Nh3", "Nox", "Sox", 
  "TNitrogen_water", "Tph_Water", "Toc_Water", 
  "Nickel_Air", "Nickel_Water", "Zn_Air", "Zn_Water"
)

# ----------------------------------------
# 1) Run analyze_full on each pollutant
# ----------------------------------------
results_full <- lapply(setNames(polluters, polluters), function(var) {
  res <- analyze_full(merged_data, var)
  
  plain <- res$did_plain %>% filter(term %in% c("ETS:phase3_post", "ETS:phase4_post"))
  fe  <- res$did_fe_sector %>% filter(term %in% c("ETS:phase3_post", "ETS:phase4_post"))
  
  
  list(
    plain     = plain,
    fe        = fe,
    n_obs     = res$n_obs,
    n_firms   = res$n_firms
  )
})

# 6) Print in desired format
for (poll in names(results_full)) {
  cat("\n----", poll, "----\n")
  cat("\n Unmatched \n")
  print(results_full[[poll]]$plain)

  cat("\n Unmatched FE Sector \n")
  print(results_full[[poll]]$fe)

  cat(
    "\nN obs (2008–2023):", results_full[[poll]]$n_obs,
    "; Unique firms:", results_full[[poll]]$n_firms, "\n"
  )
}