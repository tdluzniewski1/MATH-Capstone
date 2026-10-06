setwd("C:\\Users\\thoma\\OneDrive\\Desktop\\WCU\\Classes\\MATH-479\\Data Sources")
county_pres = read.csv("countypres_2000-2024.csv")
state_wide_pres = read.csv("1976-2024-president.csv")

library(tidyverse)

# Filtering presidential

presidential_c <- county_pres %>%
  filter(
    year %in% c(
      2008,
      2012,
      2016,
      2020,
      2024
    )
  )

presidential_s <- state_wide_pres %>%
  filter(
    year %in% c(
      2008,
      2012,
      2016,
      2020,
      2024
    )
  )

# Standardizing FIPS codes to match for merging later on
presidential_c <- presidential_c %>%
  mutate(
    county_fips = str_pad(
      as.character(county_fips),
      width = 5,
      side = "left",
      pad = "0"
    )
  )

presidential_s <- presidential_s %>%
  mutate(
    state_fips = str_pad(
      as.character(state_fips),
      width = 5,
      side = "left",
      pad = "0"
    )
  )

# 
presidential_s %>%
  count(party_simplified) %>%
  arrange(desc(n))

presidential_c %>%
  count(party) %>%
  arrange(desc(n))

# Aggregate Dem and Rep votes

presidential_two_party_c <- presidential_c %>%
  filter(
    party %in% c(
      "DEMOCRAT",
      "REPUBLICAN"
    )
  ) %>%
  group_by(
    year,
    state,
    state_po,
    county_name,
    county_fips,
    party
  ) %>%
  summarise(
    votes = sum(candidatevotes, na.rm = TRUE),
    .groups = "drop"
  )

presidential_two_party_s <- presidential_s %>%
  filter(
    party_simplified %in% c(
      "DEMOCRAT",
      "REPUBLICAN"
    )
  ) %>%
  group_by(
    year,
    state,
    state_po,
    state_fips,
    party_simplified
  ) %>%
  summarise(
    votes = sum(candidatevotes, na.rm = TRUE),
    .groups = "drop"
  )

# Turn Dem and Rep votes into columns

presidential_two_party_c <- presidential_two_party_c %>%
  pivot_wider(
    names_from = party,
    values_from = votes,
    values_fill = 0
  ) %>%
  rename(
    dem_votes = DEMOCRAT,
    rep_votes = REPUBLICAN,
    election_year = year,
    GEOID = county_fips
  )

presidential_two_party_s <- presidential_two_party_s %>%
  pivot_wider(
    names_from = party_simplified,
    values_from = votes,
    values_fill = 0
  ) %>%
  rename(
    dem_votes = DEMOCRAT,
    rep_votes = REPUBLICAN,
    election_year = year,
    GEOID = state_fips
  )

# Two party totals

presidential_two_party_c <- presidential_two_party_c %>%
  mutate(
    two_party_votes =
      dem_votes + rep_votes,
    
    dem_share =
      dem_votes / two_party_votes,
    
    rep_share =
      rep_votes / two_party_votes,
    
    dem_margin =
      dem_share - rep_share
  )

presidential_two_party_s <- presidential_two_party_s %>%
  mutate(
    two_party_votes =
      dem_votes + rep_votes,
    
    dem_share =
      dem_votes / two_party_votes,
    
    rep_share =
      rep_votes / two_party_votes,
    
    dem_margin =
      dem_share - rep_share
  )

presidential_two_party_c <- presidential_two_party_c %>%
  mutate(
    winner_party = case_when(
      dem_votes > rep_votes ~ "Democratic",
      rep_votes > dem_votes ~ "Republican",
      dem_votes == rep_votes ~ "Tie"
    )
  )

presidential_two_party_s <- presidential_two_party_s %>%
  mutate(
    winner_party = case_when(
      dem_votes > rep_votes ~ "Democratic",
      rep_votes > dem_votes ~ "Republican",
      dem_votes == rep_votes ~ "Tie"
    )
  )

View(presidential_two_party_c)
View(presidential_two_party_s)

# Calculating shifts in vote totals for both
# County level
presidential_two_party_c <- presidential_two_party_c %>%
  mutate(
    # Total two-party vote
    two_party_votes = dem_votes + rep_votes,
    
    # Vote percentages
    dem_pct = (dem_votes / two_party_votes) * 100,
    rep_pct = (rep_votes / two_party_votes) * 100
  ) %>%
  
  # Must be ordered within each county before using lag()
  arrange(GEOID, election_year) %>%
  group_by(GEOID) %>%
  
  mutate(
    # Previous election percentages
    previous_dem_pct = lag(dem_pct),
    previous_rep_pct = lag(rep_pct),
    
    # Change from previous presidential election
    dem_shift = dem_pct - previous_dem_pct,
    rep_shift = rep_pct - previous_rep_pct
  ) %>%
  
  ungroup()

# State level
presidential_two_party_s <- presidential_two_party_s %>%
  mutate(
    two_party_votes = dem_votes + rep_votes,
    
    dem_pct = (dem_votes / two_party_votes) * 100,
    rep_pct = (rep_votes / two_party_votes) * 100
  ) %>%
  
  arrange(state_po, election_year) %>%
  group_by(state_po) %>%
  
  mutate(
    previous_dem_pct = lag(dem_pct),
    previous_rep_pct = lag(rep_pct),
    
    dem_shift = dem_pct - previous_dem_pct,
    rep_shift = rep_pct - previous_rep_pct
  ) %>%
  
  ungroup()

presidential_two_party_c <- presidential_two_party_c %>%
  arrange(GEOID, election_year) %>%
  group_by(GEOID) %>%
  mutate(
    dem_margin = dem_pct - rep_pct,
    margin_shift = dem_margin - lag(dem_margin)
  ) %>%
  ungroup()

presidential_two_party_s <- presidential_two_party_s %>%
  arrange(state_po, election_year) %>%
  group_by(state_po) %>%
  mutate(
    dem_margin = dem_pct - rep_pct,
    margin_shift = dem_margin - lag(dem_margin)
  ) %>%
  ungroup()

write_csv(
  presidential_two_party_c,
  "pres_elec_panel_county.csv"
)
write_csv(
  presidential_two_party_s,
  "pres_elec_panel_state.csv"
)
