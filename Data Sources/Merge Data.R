setwd("C:\\Users\\thoma\\OneDrive\\Desktop\\WCU\\Classes\\MATH-479\\Data Sources")
library(tidyverse)

# ============================================================
# Presidential Election Capstone
# Merge Background + Election Data
# ============================================================

library(tidyverse)


# ------------------------------------------------------------
# 1. Import cleaned datasets
# ------------------------------------------------------------

county_background <- read_csv(
  "county_panel_background.csv",
  show_col_types = FALSE
)

state_background <- read_csv(
  "state_panel_background.csv",
  show_col_types = FALSE
)

county_elections <- read_csv(
  "pres_elec_panel_county.csv",
  show_col_types = FALSE
)

state_elections <- read_csv(
  "pres_elec_panel_state.csv",
  show_col_types = FALSE
)


# ------------------------------------------------------------
# 2. Inspect datasets
# ------------------------------------------------------------

glimpse(county_background)
glimpse(county_elections)

glimpse(state_background)
glimpse(state_elections)

# Standardize county FIPS/GEOID to 5 characters for counties and states

county_background <- county_background %>%
  mutate(
    GEOID = str_pad(
      as.character(GEOID),
      width = 5,
      side = "left",
      pad = "0"
    )
  )

county_elections <- county_elections %>%
  mutate(
    GEOID = str_pad(
      as.character(GEOID),
      width = 5,
      side = "left",
      pad = "0"
    )
  )

state_background <- state_background %>%
  mutate(
    GEOID = str_pad(
      as.character(GEOID),
      width = 5,
      side = "left",
      pad = "0"
    )
  )

state_elections <- state_elections %>%
  mutate(
    GEOID = str_pad(
      as.character(GEOID),
      width = 5,
      side = "left",
      pad = "0"
    )
  )

View(county_background)
View(county_elections)
View(state_background)
View(state_elections)
####
county_panel <- county_background %>%
  left_join(
    county_elections,
    by = c(
      "GEOID",
      "election_year"
    )
  )

state_panel <- state_background %>%
  left_join(
    state_elections,
    by = c(
      "GEOID",
      "election_year"
    )
  )

nrow(county_background)
nrow(county_panel)

nrow(state_background)
nrow(state_panel)

View(county_panel)
View(state_panel)

# Import Connecticut election results


write_csv(
  county_panel,
  "county_panel_complete.csv"
)

write_csv(
  state_panel,
  "state_panel_complete.csv"
)