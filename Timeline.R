library(ggplot2)
library(dplyr)
library(lubridate)
library(tibble)

# 1. Build policy table
policies <- tribble(
  ~Policy,    ~start,       ~end,         ~Category,
  "IPPC",     "1999-01-01", "2013-01-01", "Both",
  "LCPD",     "2001-01-01", "2013-01-01", "Both",
  "Phase 1",  "2005-01-01", "2007-01-01", "ETS facilities",
  "Phase 2",  "2008-01-01", "2012-01-01", "ETS facilities",
  "RED I",    "2010-01-01", "2020-01-01", "Both",
  "Phase 3",  "2013-01-01", "2020-01-01", "ETS facilities",
  "ESD",      "2013-01-01", "2020-01-01", "Non-ETS facilities",
  "IED",      "2013-01-01", "2024-01-01", "Both",
  "EED",      "2014-01-01", "2024-01-01", "Both",
  "MCPD",     "2015-01-01", "2024-01-01", "Both",
  "Phase 4",  "2021-01-01", "2024-01-01", "ETS facilities",
  "ESR",      "2021-01-01", "2024-01-01", "Non-ETS facilities",
  "RED II",   "2021-01-01", "2024-01-01", "Both"
) %>%
  mutate(
    start = ymd(start),
    # add one day so the bar fully covers the final year
    end   = ymd(end) + days(1),
    # preserve ordering:
    Policy = factor(Policy, levels=(Policy))
  )

# 2. Define color palette
my_cols <- c(
  "Both"             = "forestgreen",
  "ETS facilities"   = "steelblue",
  "Non-ETS facilities"= "firebrick"
)

# 3. Plot
ggplot(policies) +
  geom_segment(aes(x = start, xend = end, y = Policy, yend = Policy, color = Category),
               size = 6, lineend = "round") +
  scale_color_manual(values = my_cols, name = "Policy target") +
  scale_x_date(date_breaks = "5 years", date_labels = "%Y",
               expand = expansion(mult = c(0.01, 0.01))) +
  labs(x = "Year", y = NULL,
       title = "Timeline of Key EU Policies") +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.y      = element_text(size = 10),
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank(),
    axis.ticks.y     = element_blank(),
    legend.position  = "bottom"
  )

ggsave(
  filename = "EU_policy_timeline.png",
  plot     = last_plot(),
  width    = 10,    
  height   = 6,     
  dpi      = 300,   
  bg = "white"  
  )
