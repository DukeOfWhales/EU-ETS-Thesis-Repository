rm(list = ls())
cat("\014") 
graphics.off()
options(scipen = 999)

setwd("C:/Users/Tobyd/OneDrive - Aarhus universitet/Uni-livet/Kandidatgrad/10. Semester/Specialet/Data/Final data/")

library(readxl)
library(dplyr)
library(tidyr)  
library(stringr)
library(stringdist) 
library(MatchIt)
library(broom)
library(ggplot2)
library(cobalt)
library(leaflet)


#################################################Data Import and cleaning############################################

load("merged_data.RData")

match_phase3 <- function(data, emission_var) {
  # 1. Filter data for Phase 3 and 4 (2008–2023) and create a phase3_post indicator
  data_phase3 <- data %>%
    filter(year >= 2007, year <= 2023) %>%
    mutate(phase3_post = ifelse(year >= 2013 & year <= 2020, 1, 0),
           phase3_4_post = ifelse(year >= 2013, 1, 0),
           phase4_post = ifelse(year >= 2021, 1, 0))
  
  # 2. Remove rows with NA or 0 in the chosen emission variable
  data_phase3 <- data_phase3 %>%
    filter(!is.na(.data[[emission_var]]) & .data[[emission_var]] != 0) %>%
    mutate(emission_log = log(.data[[emission_var]]))
  
  data_phase3 <- data_phase3 %>%
    group_by(Firm) %>%
    # count how many years in [2013,2020] each firm has
    filter( sum(year >= 2013 & year <= 2020, na.rm = TRUE) >= 3 ) %>%
    ungroup()
  
  
  # 4. Compute pre-treatment statistics (2008–2012) for each Firm
  Firm_pretrend <- data_phase3 %>%
    filter(year >= 2008, year <= 2012) %>%
    group_by(Firm) %>%
    summarize(
      first_year = min(year),
      last_year  = max(year),
      first_val = first(.data[[emission_var]], order_by = year),
      last_val  = last(.data[[emission_var]], order_by = year),
      # Compute the yearly percentage growth (in level form) as a decimal.
      Growth = ifelse(
        last_year > first_year & first_val > 0,
        ((last_val / first_val)^(1 / (last_year - first_year)) - 1),
        NA_real_
      ),
      Zn_Water = mean(emission_log, na.rm = TRUE),
      Region = first(Region),
      Sector = first(Sector),
      Subsector = first(Subsector),
      ETS = first(ETS),
      Region = first(Region),
      .groups = "drop"
    )
  
  # 6. Matching: remove rows with missing pretrend stats, then match
  Firm_pretrend <- Firm_pretrend %>%
    filter(!is.na(Zn_Water), !is.na(Growth))
  
  #Remove outliers
  bounds <- quantile(Firm_pretrend$Growth, probs = c(0.005, 0.995), na.rm = TRUE)
  lower_thr <- bounds[1]
  upper_thr <- bounds[2]
  
  
  Firm_pretrend <- Firm_pretrend %>%
    filter(
      Growth >= lower_thr,
      Growth <= upper_thr
    )
  
  match_out <- matchit(
    ETS ~ Zn_Water + Growth,
    data = Firm_pretrend,
    method = "nearest",
   # distance = "mahalanobis",
    ratio = 2,
    #    replace = TRUE,
    caliper = c(Growth=0.3,Zn_Water=0.3),
    #    min.controls = 1, max.controls = 5,
      exact = ~ Sector + Subsector + Region
  )
  
  matched_firms <- match.data(match_out)
  
  #Drop columns
  matched_firms <- matched_firms %>%
    select(-first_year, -last_year, -first_val, -last_val,-Sector,-Subsector,-ETS,-Zn_Water,-Region)
  
  #merge with dataphase3
  matched_data <- data_phase3 %>%
    inner_join(matched_firms, by = "Firm")
  
  # 12. Visualize the prior period (2008–2012)
  pretrend_plot_data <- matched_data %>%
    filter(year >= 2008, year <= 2023) %>%
    group_by(year, ETS) %>%
    summarize(mean_emission_log = weighted.mean(emission_log,weights, na.rm = TRUE),
              .groups = "drop")
  
  pretrends_plot <- ggplot(pretrend_plot_data, aes(x = year, y = mean_emission_log, color = factor(ETS))) +
    geom_line(size = 1) +
    geom_point(size = 2) +
    geom_vline(xintercept = 2012.5, linetype = "dashed", color = "black", size = 1) +
    geom_vline(xintercept = 2020.5, linetype = "dashed", color = "black", size = 1) +
    scale_x_continuous(breaks = seq(2008, 2023, 1)) +
    labs(title = paste("Time Trends -", emission_var, "- Phase 3 Match"),
         x = "Year",
         y = "Weighted Mean Log Emission",
         color = "ETS Treatment") +
    theme_minimal() +
    theme(legend.position = "none",
          axis.text.x      = element_text(angle = 45, hjust = 1, vjust = 1))
  
  # Return a list of key results and data
  list(
    Firm_pretrend = Firm_pretrend,
    matched_firms = matched_firms,
    matched_data = matched_data,
    match_out = match_out,
    pretrends_plot = pretrends_plot
  )
}

########################################### Zn_Water ####################################################
pollutant_var <- c("Zn_Water")
# 1. Create a list to store results for each variable using your pipeline
results_list <- lapply(pollutant_var, function(var) {
  cat("Analyzing", var, "\n")
  match_phase3(merged_data, emission_var = var)
})

names(results_list) <- pollutant_var

# 3. Print the pretrends plot
matched_data <- results_list[["Zn_Water"]]$matched_data
match_out <- results_list[["Zn_Water"]]$match_out
matched_firms <- results_list[["Zn_Water"]]$matched_firms

keep_vars <- !grepl("^(Region|Sector|Subsector)", colnames(match_out$X))
# Create a copy of match_out with a subset of the covariates
match_out_relevant <- match_out
match_out_relevant$X <- match_out$X[, keep_vars]
Match_summary <- summary(match_out_relevant,interactions = TRUE)
Match_summary

####################Visual Match############################################
#Scatter plot#
#plot(match_out, type = "jitter", interactive = FALSE)
#Histogram plot#
#plot(match_out, type = "hist", interactive = FALSE,
#     which.xs = ~Zn_Water + Growth)
#Density plot#
plot(match_out, type = "density", interactive = FALSE,
     which.xs = ~Zn_Water + Growth)
#QQ plot#
plot(match_out, type = "qq", interactive = FALSE,
     which.xs = ~Zn_Water + Growth)
#ECDF plot#
plot(match_out, type = "ecdf", interactive = FALSE,
     which.xs = ~Zn_Water + Growth)
#Love Plot#
plot(Match_summary,abs = FALSE,var.order = "alphabetical")

cat("\nPretrends Plot:\n")
print(results_list[[pollutant_var]]$pretrends_plot)


# 13. Save the matched data
save(matched_data, file = "matched_data.Zn_Water.RData")

setwd("C:/Users/Tobyd/OneDrive - Aarhus universitet/Uni-livet/Kandidatgrad/10. Semester/Specialet/Data/Final data/Images and figures/Matching/Local air and water polluters/")

# 11. Save the pretrends plot with a white background
ggsave("TimeTrendZn_WaterPhase3.png", plot = results_list[[pollutant_var]]$pretrends_plot, width = 5, height = 4, bg = "white")

##Apendix files## 
Geography <- leaflet(matched_data) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addCircleMarkers(
    lng        = ~Lon,
    lat        = ~Lat,
    radius     = 3,
    stroke     = FALSE,
    fillOpacity= 0.4,
    color      = ~ifelse(ETS == 1, "blue", "red"),
    popup      = ~paste0(
      "<strong>Facility:</strong> ", Firm, "<br/>",
      "<strong>ETS:</strong> ", ifelse(ETS==1, "Yes", "No")
    )
  ) %>%
  addLegend(
    position = "bottomright",
    colors   = c("blue","red"),
    labels   = c("ETS = 1","ETS = 0"),
    title    = "EU ETS Status"
  ) %>%
  setView(lng = 4, lat = 54, zoom = 4)

library(mapview)

mapshot2(
  Geography,
  file = "Zn_WaterGeoPhase3.png",
  remove_controls = c("zoomControl","layersControl"),  # optional cleanup
  vheight         = 870,       # increase viewport height (px)
  vwidth          = 750,       # increase viewport width (px)
)

# Save the covariate balance plot with a title and white background
ggsave("LovePlotZn_WaterPhase3.png", plot = love.plot(
  match_out_relevant,
  abs        = TRUE,
  poly       = 2,
  binary     = "std",
  thresholds = c(m = .1),
  var.order  = "alphabetical",
  title      = "",
  position   = "none",
  xlab       = "Absolute Standardized Mean Differences",
  ylab       = "",
  shapes = c("circle filled", "circle"),
  colors = c("black",     "black"),
  fill   = c("black",     "black"),
), width = 5, height = 4, bg = "white")

Weights <- matched_data %>%
  group_by(year,ETS) %>%
  summarize(
    total_weight = sum(weights, na.rm = TRUE),
    .groups = "drop"
  )
print(Weights,n=34)