rm(list = ls())
cat("\014") 
graphics.off()
options(scipen = -1)

setwd("C:/Users/Tobyd/OneDrive - Aarhus universitet/Uni-livet/Kandidatgrad/10. Semester/Specialet/Data/Final data/")

#Install all packages
# List all the packages your code uses

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
analyze_phase3 <- function(df, emission_var) {
  df <- df %>% 
    filter(year >= 2008 & year <= 2023)
  
  #two‐period DiD 3 | 4
  did_plain <- feols(
    emission_log ~ ETS + phase3_post + ETS:phase3_post + ETS:phase4_post + phase4_post,
    data     = df,
    weights  = ~weights,
    cluster  = ~subclass
  ) %>% tidy()
  
  did_fe_sector <- feols(
    emission_log ~ ETS:phase3_post + ETS:phase4_post | Firm + year + Sector:year,
    data     = df,
    weights  = ~weights,
    cluster  = ~subclass
  ) %>% tidy()
  
  # two‐period DiD parent 3 | 4
  did_plain_parent <- feols(
    emission_log ~ Parent_ETS_dummy + phase3_post + Parent_ETS_dummy:phase3_post + Parent_ETS_dummy:phase4_post + phase4_post,
    data     = df,
    weights  = ~weights,
    cluster  = ~subclass
  ) %>% tidy()
  
  did_fe_parent_sector <- feols(
    emission_log ~ Parent_ETS_dummy:phase3_post + Parent_ETS_dummy:phase4_post | Firm + year + Sector:year,
    data     = df,
    weights  = ~weights,
    cluster  = ~subclass
  ) %>% tidy()
  
  df3 <- df %>%
    filter(year >= 2008, year <= 2017)

  # two‐period DiD
  did_fe_sector2017 <- feols(
    emission_log ~ ETS:phase3_post | Firm + Sector^year,
    data     = df3,
    weights  = ~weights,
    cluster  = ~subclass
  ) %>% tidy()
  
  did_ETS_Region <- feols(
    emission_log ~ ETS:phase3_post|
      Firm + year + Sector^year + Region^year,
    data     = df3,
    weights  = ~weights,
    cluster  = ~subclass
  ) %>% tidy()
  
  df5 <- df3 %>%
    mutate(    Article10c    = ifelse(
      Country %in% c(
        "BG",  # Bulgaria
        "CZ",  # Czechia
        "HR",  # Croatia
        "EE",  # Estonia
        "LV",  # Latvia
        "LT",  # Lithuania
        "HU",  # Hungary
        "PL",  # Poland
        "RO",  # Romania
        "SK"   # Slovakia
      ),
      1,
      0
    )
    )
  
  
  did_ETS_Article10c <- feols(
    emission_log ~ ETS:phase3_post+ETS:phase3_post:Article10c|
      Firm + Sector^year,
    data     = df5,
    weights  = ~weights,
    cluster  = ~subclass
  ) %>% tidy()
  
  
  #Filter for fully balanced sample in 2008 to 2017
  df4 <- df3 %>%
    group_by(Firm) %>%
    # count how many years in [2008,2017] each firm has
    filter( sum(year >= 2008 & year <= 2017, na.rm = TRUE) == 10 ) %>%
    ungroup()
  
  did_fe_sector_balanced2017 <- feols(
    emission_log ~ ETS:phase3_post | Firm + year + Sector:year,
    data     = df4,
    weights  = ~weights,
    cluster  = ~subclass
  ) %>% tidy()
  
  ################### Look into both periods
  #two‐period DiD 3 | 4
  did_plain34 <- feols(
    emission_log ~ ETS + phase3_4_post + ETS:phase3_4_post,
    data     = df,
    weights  = ~weights,
    cluster  = ~subclass
  ) %>% tidy()
  
  did_fe_sector34 <- feols(
    emission_log ~ ETS:phase3_4_post | Firm + year + Sector:year,
    data     = df,
    weights  = ~weights,
    cluster  = ~subclass
  ) %>% tidy()
  
  # two‐period DiD parent 3 | 4
  did_parent34 <- feols(
    emission_log ~ Parent_ETS_dummy + phase3_4_post + Parent_ETS_dummy:phase3_4_post,
    data     = df,
    weights  = ~weights,
    cluster  = ~subclass
  ) %>% tidy()
  
  did_fe_sector_parent34 <- feols(
    emission_log ~ Parent_ETS_dummy:phase3_4_post | Firm + year + Sector:year,
    data     = df,
    weights  = ~weights,
    cluster  = ~subclass
  ) %>% tidy()
  
  # two‐period DiD
  did_342017 <- feols(
    emission_log ~ ETS:phase3_post | Firm + year + Sector:year,
    data     = df3,
    weights  = ~weights,
    cluster  = ~subclass
  ) %>% tidy()
  
  did_balanced342017 <- feols(
    emission_log ~ ETS:phase3_post | Firm + year + Sector:year,
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
  
  summaryArticle10.c <- df5 %>%
    group_by(Article10c,ETS) %>%
    summarise(
      n = sum(weights, na.rm = TRUE),
      n_firms = n_distinct(Firm)
    ) %>%
    ungroup()
  ################### Event study
  
  # yearly event‐study, without FE
  es_plain <- feols(
    emission_log ~ i(year,ETS,ref=2012) | Firm + year + Sector:year,
    data     = df,
    weights  = ~weights,
    clusters = ~subclass
  ) %>% tidy()
  
  # extract only the ETS × year terms
  ev_plain <- es_plain %>%
    filter(str_detect(term, "^year::\\d{4}:ETS$")) %>%
    mutate(
      year = as.integer(str_remove_all(term, "year::|:ETS")),
      low   = estimate - 1.96 * std.error,
      high  = estimate + 1.96 * std.error
    )
  
  # ** Insert your baseline check here **
  if (! any(ev_plain$year == 2012)) {
    ev_plain <- bind_rows(
      ev_plain,
      tibble(
        term      = "year::2012:ETS",
        estimate  = 0,
        std.error = 0,
        year      = 2012,
        lower     = 0,
        upper     = 0
      )
    ) %>% arrange(year)
  }

  # only build plots if there's something to plot
  plt_plain <- ggplot(ev_plain, aes(year, estimate)) +
    geom_line(color = "#00BFC4"
, linewidth = 1) +
    geom_point(color = "#00BFC4"
, size = 2) +
    geom_errorbar(aes(ymin = low, ymax = high), width = 0.2, color = "#00BFC4"
) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
    geom_vline(xintercept = 2012.5, linetype = "dashed", color = "black", size = 1) +
    geom_vline(xintercept = 2020.5, linetype = "dashed", color = "black", size = 1) +
    scale_x_continuous(breaks = seq(2008, 2023, 1)) +
    labs(
      title = paste(emission_var, "event study (FE)"),
      x     = "Year",
      y     = "Treatment effect"
    ) + theme_minimal() +  theme(legend.position = "none",
        axis.text.x      = element_text(angle = 45, hjust = 1, vjust = 1))
  
  fname <- paste0(emission_var, "_eventstudy.png")
  
  # 11. Save the pretrends plot with a white background
  ggsave(fname, plot = plt_plain, width = 5, height = 4, bg = "white")
  
  list(
    did_plain                      = did_plain,
    did_fe_sector                  = did_fe_sector,
    did_plain_parent               = did_plain_parent,
    did_fe_parent_sector           = did_fe_parent_sector,
    did_fe_sector2017              = did_fe_sector2017,
    did_fe_sector_balanced2017     = did_fe_sector_balanced2017,
    did_plain34                    = did_plain34,
    did_parent34                   = did_parent34,
    did_fe_sector34                = did_fe_sector34,
    did_fe_sector_parent34         = did_fe_sector_parent34,
    did_342017                     = did_342017,
    did_balanced342017             = did_balanced342017,
    sum_df1  = sum_df1,
    n_firmdf1 = n_firmdf1,
    sum_df3  = sum_df3,
    n_firmdf3 = n_firmdf3,
    sum_df4  = sum_df4,
    n_firmdf4 = n_firmdf4,
    plt_plain = plt_plain,
    did_ETS_Region = did_ETS_Region,
    did_ETS_Article10c = did_ETS_Article10c,
    summaryArticle10.c = summaryArticle10.c
  )
}

# ----------------------------------------
# 2) GET ALL FILES AND DROP THOSE WITH NO TREATED FACILITIES
# ----------------------------------------
all_files <- list.files(pattern="^matched_data\\..*\\.RData$")

# ----------------------------------------
# 3) LOOP OVER THE GOOD ONES
# ----------------------------------------
results <- lapply(all_files, function(f) {
  load(f)  
  varname <- sub("^matched_data\\.(.*)\\.RData$", "\\1", f)
  analyze_phase3(matched_data, varname)
})
names(results) <- sub("^matched_data\\.(.*)\\.RData$", "\\1", all_files)

for (v in names(results)) {
  cat("\n----", v, "----\n")
  # 
  # # 1) Plain DiD, phase 3|4
  # sel_terms <- c("ETS:phase3_post", "ETS:phase4_post")
  # cat("\ Facility P3 Post3 and Post4 \n")
  # print(
  #   results[[v]]$did_plain[
  #     results[[v]]$did_plain$term %in% sel_terms,
  #   ]
  # )
  # 
  # # 2) FE + sector×year 3|4
  # cat("\n Facility FE P3 Post3 and Post4 \n")
  # print(
  #   results[[v]]$did_fe_sector[
  #     results[[v]]$did_fe_sector$term %in% sel_terms,
  #   ]
  # )
  # 
  # # 3) Parent DiD 3|4
  # sel_terms_parent <- c("Parent_ETS_dummy:phase3_post", "Parent_ETS_dummy:phase4_post")
  # cat("\n Firm P3 Post3 and Post4 \n")
  # print(
  #   results[[v]]$did_plain_parent[
  #     results[[v]]$did_plain_parent$term %in% sel_terms_parent,
  #   ]
  # )
  # 
  # # 4) FE + sector×year, parent 3|4
  # cat("\n Firm FE P3 Post3 and Post4 \n")
  # print(
  #   results[[v]]$did_fe_parent_sector[
  #     results[[v]]$did_fe_parent_sector$term %in% sel_terms_parent,
  #   ]
  # )
  # 
  # 5) FE + sector×year, subperiod (2008–2017)
  cat("\nFE + Sector Trend 2008-2017\n")
  print(
    results[[v]]$did_fe_sector2017[
      results[[v]]$did_fe_sector2017$term %in% c("ETS:phase3_post"),
    ]
  )
  # cat("\n FE + Sector Trend + Region Trend 2008-2017\n")
  # print(
  #   results[[v]]$did_ETS_Region
  # )

  cat("\n FE + Article10c\n")
  print(
    results[[v]]$did_ETS_Article10c
  )  
  # #
  # # # 6) FE + sector×year, balanced subperiod (2008–2017)
  # # cat("\nFE + Sector Trend Balanced 2008-2017\n")
  # # print(
  # #   results[[v]]$did_fe_sector_balanced2017[
  # #     results[[v]]$did_fe_sector_balanced2017$term %in% c("ETS:phase3_post"),
  # #   ]
  # # )
  # 
  # # 7) DiD, phases 3&4
  # cat("\n Facility P3 Post34 \n")
  # print(
  #   results[[v]]$did_plain34[
  #     results[[v]]$did_plain34$term %in% c("ETS:phase3_4_post"),
  #   ]
  # )
  # 
  # # 8) Parent DiD, phases 3&4
  # cat("\n Firm P3 Post34 \n")
  # print(
  #   results[[v]]$did_parent34[
  #     results[[v]]$did_parent34$term %in% c("Parent_ETS_dummy:phase3_4_post"),
  #   ]
  # )
  # 
  # # 9) FE + sector×year, phases 3&4
  # cat("\n Facility FE P3 Post34 \n")
  # print(
  #   results[[v]]$did_fe_sector34[
  #     results[[v]]$did_fe_sector34$term %in% c("ETS:phase3_4_post"),
  #   ]
  # )
  # 
  # # 10) FE + sector×year, parent phases 3&4
  # cat("\n Firm FE P3 Post34 \n")
  # print(
  #   results[[v]]$did_fe_sector_parent34[
  #     results[[v]]$did_fe_sector_parent34$term %in% c("Parent_ETS_dummy:phase3_4_post"),
  #   ]
  # )
  # 
  # # 11) FE + sector×year, subperiod (phases 3&4, 2008–2017)
  # cat("\n Subperiod FE 2008-2017\n")
  # print(
  #   results[[v]]$did_342017[
  #     results[[v]]$did_342017$term %in% c("ETS:phase3_post"),
  #   ]
  # )
  # 
  cat("\n Subperiod FE P3 Post3 (2008-2017):\n",
      "  Sum weights:   ", round(results[[v]]$sum_df3), "\n",
      "  Unique facilities:  ", results[[v]]$n_firmdf3,   "\n")
  
  print(results[[v]]$summaryArticle10.c)
  # 
  # 
  # # 12) Balanced FE
  # cat("\n Balanced FE P3 Post3 \n")
  # print(
  #   results[[v]]$did_balanced342017[
  #     results[[v]]$did_balanced342017$term %in% c("ETS:phase3_post"),
  #   ]
  # )
  
  # 12) Balanced FE
  # cat("\n Balanced FE P3 Post3 \n")
  # print(results[[v]]$did_ETS_sector)
  # 
  # cat("\n\nSum weights (2008-2023):\n",
  #     "  Sum weights:   ", results[[v]]$sum_df1, "\n",
  #     "  Unique facilities:  ", results[[v]]$n_firmdf1,   "\n")
  # cat("\n Balanced FE:\n",
  #     "  Sum weights P3 Post3:   ", round(results[[v]]$sum_df4), "\n",
  #     "  Unique facilities P3 Post3:  ", results[[v]]$n_firmdf4,   "\n")
  # cat("\n Eventtime \n")
  # print(results[[v]]$plt_plain)
}

#######Update the N2o subsample table
##################N2o###############

# load("matched_data.N2o.RData")
# 
# matched_data_2016 <- matched_data %>%
#   filter(year >= 2008, year <= 2016)
# 
# did_ETS_RegionN2o_2016 <- feols(
#   emission_log ~ ETS:phase3_post|
#     Firm + year+ Sector:year,
#   data     = matched_data_2016,
#   weights  = ~weights,
#   cluster  = ~subclass
# ) %>% tidy()
# 
# did_ETS_RegionN2o_2016
# 
# sum_2016  = sum(matched_data_2016$weights, na.rm = TRUE)
# n_firmd2016 = n_distinct(matched_data_2016$Firm)
# 
# #Filter for fully balanced sample in 2008 to 2016
# matched_data_balanced <- matched_data_2016 %>%
#   group_by(Firm) %>%
#   # count how many years in [2008,2017] each firm has
#   filter( sum(year >= 2008 & year <= 2016, na.rm = TRUE) == 9 ) %>%
#   ungroup()
# 
# #Run the DiD for the balanced sample
# did_ETS_RegionN2o_balanced <- feols(
#   emission_log ~ ETS:phase3_post|
#     Firm + year + Sector:year,
#   data     = matched_data_balanced,
#   weights  = ~weights,
#   cluster  = ~subclass
# ) %>% tidy()
# 
# did_ETS_RegionN2o_balanced
# 
# sum_2016_balanced  = sum(matched_data_balanced$weights, na.rm = TRUE)
# n_firm_2016_balanced = n_distinct(matched_data_balanced$Firm)
# 
# did_ETS_RegionN2o_2016
# sum_2016
# n_firmd2016
# 
# did_ETS_RegionN2o_balanced
# sum_2016_balanced
# n_firm_2016_balanced

# did_ETS_RegionN2o_2016 <- feols(
#   emission_log ~ ETS:phase3_post|
#     Firm + year+ Sector^year + Region^year,
#   data     = matched_data_2016,
#   weights  = ~weights,
#   cluster  = ~subclass
# ) %>% tidy()
# 
# did_ETS_RegionN2o_2016
