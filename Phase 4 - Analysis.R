rm(list = ls())
cat("\014") 
graphics.off()
options(scipen = -1)

setwd("C:/Users/Tobyd/OneDrive - Aarhus universitet/Uni-livet/Kandidatgrad/10. Semester/Specialet/Data/Final data/")

library(dplyr)
library(ggplot2)
library(broom)
library(estimatr)
library(fixest)
library(stringr)
library(readr)  

# ----------------------------------------
# 1) THE ANALYSIS FUNCTION
# ----------------------------------------
analyze_phase4 <- function(df, emission_var) {
  df <- df %>% 
    filter(year >= 2013 & year <= 2023)
  # two‐period DiD
  did_plain <- feols(
    emission_log ~ ETS * phase4_post,
    data     = df,
    weights  = ~weights,
    cluster  = ~subclass
  ) %>% tidy()
  
  did_fe <- feols(
    emission_log ~ ETS:phase4_post | Firm + year + Sector:year,
    data     = df,
    weights  = ~weights,
    cluster  = ~subclass
  ) %>% tidy()
  
  # two‐period DiD parent 4
  did_plain_parent <- feols(
    emission_log ~ Parent_ETS_dummy  + Parent_ETS_dummy:phase4_post + phase4_post,
    data     = df,
    weights  = ~weights,
    cluster  = ~subclass
  ) %>% tidy()
  
  did_plain_parent_fe <- feols(
    emission_log ~ Parent_ETS_dummy:phase4_post | Firm + year + Sector:year,
    data     = df,
    weights  = ~weights,
    cluster  = ~subclass
  ) %>% tidy()
  
  
  df3 <- df %>%
    filter(year >= 2017, year <= 2022)
  
  df4 <- df3 %>%
    group_by(Firm) %>%
    filter( sum(year >= 2017 & year <= 2022, na.rm = TRUE) == 6 ) %>%
    ungroup()
  
  did_2022 <- feols(
    emission_log ~ ETS:phase4_post | Firm + year + Sector:year,
    data     = df3,
    weights  = ~weights,
    cluster  = ~subclass
  ) %>% tidy()
  
  did_balanced2022 <- feols(
    emission_log ~ ETS:phase4_post | Firm + year + Sector:year,
    data     = df4,
    weights  = ~weights,
    cluster  = ~subclass
  ) %>% tidy()

  sum_df1  = sum(df$weights, na.rm = TRUE)
  n_firmdf1 = n_distinct(df$Firm)
  
  sum_df3  = sum(df3$weights, na.rm = TRUE)
  n_firmdf3 = n_distinct(df3$Firm)
  
  sum_df4  = sum(df4$weights, na.rm = TRUE)
  n_firmdf4 = n_distinct(df4$Firm)
  
  list(
    did_plain     = did_plain,
    did_plain_parent = did_plain_parent,
    did_fe        = did_fe,
    did_2022 = did_2022,
    did_balanced2022 = did_balanced2022,
    did_fe_parent = did_plain_parent_fe,
    sum_df1  = sum_df1,
    n_firmdf1 = n_firmdf1,
    sum_df3  = sum_df3,
    n_firmdf3 = n_firmdf3,
    sum_df4  = sum_df4,
    n_firmdf4 = n_firmdf4
  )
}

# ----------------------------------------
# 2) GET ALL FILES
# ----------------------------------------
all_files <- list.files(pattern="^matched_data4\\..*\\.RData$")

# ----------------------------------------
# 3) LOOP
# ----------------------------------------
results <- lapply(all_files, function(f) {
  load(f)  
  varname <- sub("^matched_data4\\.(.*)\\.RData$", "\\1", f)
  analyze_phase4(matched_data, varname)
})
names(results) <- sub("^matched_data4\\.(.*)\\.RData$", "\\1", all_files)

# ----------------------------------------
# 4) PRINT EVERYTHING
# ----------------------------------------
for (v in names(results)) {
   cat("\n----", v, "----\n\n")
  cat("\n Facility P4 Post4:\n")
  # # pick only the ETS:phase3_post and ETS:phase4_post rows
  sel_terms <- c("ETS:phase4_post")
  print(
    results[[v]]$did_plain[
      results[[v]]$did_plain$term %in% sel_terms,
    ]
  )

  sel_terms_parent <- c("Parent_ETS_dummy:phase4_post")
  cat("\n Firm P4 Post4:\n")
  print(
    results[[v]]$did_plain_parent[
      results[[v]]$did_plain_parent$term %in% sel_terms_parent,
    ]
  )

  cat("\n Firm FE P4 Post4:\n")
  print(
    results[[v]]$did_fe_parent[
      results[[v]]$did_fe_parent$term %in% sel_terms_parent,
    ]
  )

  cat("\n Facility FE P4 Post4:\n")
  print(
    results[[v]]$did_fe[
      results[[v]]$did_fe$term %in% sel_terms,
    ]
  )

  cat("\n Subperiod FE P4 Post4 (2017-2022):\n")
  print(
    results[[v]]$did_2022[
      results[[v]]$did_2022$term %in% sel_terms,
    ]
  )
  
  cat("\n\n")
  
  cat(" Subperiod FE Sum weights P4 Post4:   ", round(results[[v]]$sum_df3), "\n")
  cat(" Subperiod FE Unique firms P4 Post4:  ", results[[v]]$n_firmdf3,   "\n")
  
  cat("\n Balanced FE P4 Post4 (balanced 2017-2022):\n")
  print(
    results[[v]]$did_balanced2022[
      results[[v]]$did_balanced2022$term %in% sel_terms,
    ]
  )
  cat("\n\n")
  cat(" P4 Sum weights:   ", round(results[[v]]$sum_df1), "\n")
  cat(" P4 Unique firms:  ", results[[v]]$n_firmdf1,   "\n")
  cat(" Balanced FE Sum weights P4 Post4:   ", round(results[[v]]$sum_df4), "\n")
  cat(" Balanced FE Unique firms P4 Post4:  ", results[[v]]$n_firmdf4,   "\n")
  
  }