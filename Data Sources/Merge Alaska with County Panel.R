setwd("C:\\Users\\thoma\\OneDrive\\Desktop\\WCU\\Classes\\MATH-479\\Data Sources")
library(tidyverse)
library(readxl)

# Import Alaska election results
alaska_elections <- read_excel(
  "alaska_borough_election_panel.xlsx",
  sheet = "Alaska Election Panel",
  col_types = "text"
)

# Make GEOID and election year compatible with county_panel
alaska_elections <- alaska_elections %>%
  mutate(
    GEOID = str_pad(
      GEOID,
      width = 5,
      side = "left",
      pad = "0"
    ),
    
    election_year = as.numeric(election_year)
  )

county_panel <- read.csv("county_panel_background.csv")

county_panel <- county_panel %>%
  mutate(
    GEOID = str_pad(
      as.character(GEOID),
      width = 5,
      side = "left",
      pad = "0"
    ),
    
    election_year = as.numeric(election_year)
  )

alaska_complete <- county_panel %>%
  filter(state_po == "AK") %>%
  left_join(
    alaska_elections %>%
      select(
        -NAME,
        -state_po.x
      ),
    by = c(
      "GEOID",
      "election_year"
    )
  )

alaska_complete <- alaska_complete %>%
  mutate(
    election_year = case_when(
      acs_year == 2011 ~ 2012,
      acs_year == 2015 ~ 2016,
      acs_year == 2020 ~ 2020,
      acs_year == 2023 ~ 2024,
      TRUE ~ NA_real_
    )
  )

View(alaska_complete)

alaska_complete <- alaska_complete %>%
  filter(
    !is.na(dem_pct)
  )

write.csv(alaska_complete, "alaska_county_panel.csv")