setwd("C:\\Users\\thoma\\OneDrive\\Desktop\\WCU\\Classes\\MATH-479\\Data Sources")
install.packages(c("tidycensus", "tidyverse"))

library(tidycensus)
library(tidyverse)

census_api_key(
  "548a79a8d13b3d280471b2d4ab901c3ae1d63240",
  install = TRUE,
  overwrite = TRUE
)

states <- c(
  "AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA",
  "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD",
  "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ",
  "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC",
  "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY"
)

acs_years <- c(
  2011,
  2015,
  2020,
  2023
)



acs_variables <- c(
  
  # Population / demographics
  population = "B01003_001",
  median_age = "B05004_001",
  
  white_nonhispanic = "B01001H_001",
  black_nonhispanic = "B01001B_001",
  hispanic = "B01001I_001",
  # Hispanic subgroups
  hispanic_mexican = "B03001_004",
  hispanic_puerto_rican = "B03001_005",
  hispanic_cuban = "B03001_006",
  hispanic_honduran = "B03001_011",
  hispanic_venezuelan = "B03001_025",
  hispanic_salvadoran = "B03001_014",
  hispanic_nicaraguan = "B03001_012",
  hispanic_guatemalan = "B03001_010",
  hispanic_dominican = "B03001_007",
  # others
  asian = "B01001D_001",
  native_american = "B01001C_001",
  native_hawaiian_pacific = "B01001E_001",
  
  median_household_income = "B19013_001",
  per_capita_income = "B19301_001",
  poverty_universe = "B17001_001",
  below_poverty = "B17001_002",
  
  education_25plus = "B15002_001",
  
  # Male
  male_hs = "B15002_011",
  male_some_college_less_1yr = "B15002_012",
  male_some_college_1plus = "B15002_013",
  male_associates = "B15002_014",
  male_bachelors = "B15002_015",
  male_masters = "B15002_016",
  male_professional = "B15002_017",
  male_doctorate = "B15002_018",
  
  # Female
  female_hs = "B15002_028",
  female_some_college_less_1yr = "B15002_029",
  female_some_college_1plus = "B15002_030",
  female_associates = "B15002_031",
  female_bachelors = "B15002_032",
  female_masters = "B15002_033",
  female_professional = "B15002_034",
  female_doctorate = "B15002_035"
)

get_county_acs <- function(state_abbr, acs_year) {
  
  message(
    "Downloading ",
    state_abbr,
    " - ",
    acs_year
  )
  
  get_acs(
    geography = "county",
    variables = acs_variables,
    state = state_abbr,
    year = acs_year,
    survey = "acs5",
    output = "wide",
    geometry = FALSE,
    cache_table = TRUE
  ) %>%
    transmute(
      GEOID,
      NAME,
      state_po = state_abbr,
      year = acs_year,
      
      population = populationE,
      median_age = median_ageE,
      white_nonhispanic = white_nonhispanicE,
      black_nonhispanic = black_nonhispanicE,
      hispanic = hispanicE,
      hispanic_mexican = hispanic_mexicanE,
      hispanic_puerto_rican = hispanic_puerto_ricanE,
      hispanic_cuban = hispanic_cubanE,
      hispanic_honduran = hispanic_honduranE,
      hispanic_venezuelan = hispanic_venezuelanE,
      hispanic_salvadoran = hispanic_salvadoranE,
      hispanic_nicaraguan = hispanic_nicaraguanE,
      hispanic_guatemalan = hispanic_guatemalanE,
      hispanic_dominican = hispanic_dominicanE,
      asian = asianE,
      native_american = native_americanE,
      native_hawaiian_pacific = native_hawaiian_pacificE,
      median_household_income = median_household_incomeE,
      per_capita_income = per_capita_incomeE,
      poverty_universe = poverty_universeE,
      below_poverty = below_povertyE,
      
      education_25plus = education_25plusE,
      
      male_hs = male_hsE,
      male_some_college_less_1yr = male_some_college_less_1yrE,
      male_some_college_1plus = male_some_college_1plusE,
      male_associates = male_associatesE,
      male_bachelors = male_bachelorsE,
      male_masters = male_mastersE,
      male_professional = male_professionalE,
      male_doctorate = male_doctorateE,
      
      female_hs = female_hsE,
      female_some_college_less_1yr = female_some_college_less_1yrE,
      female_some_college_1plus = female_some_college_1plusE,
      female_associates = female_associatesE,
      female_bachelors = female_bachelorsE,
      female_masters = female_mastersE,
      female_professional = female_professionalE,
      female_doctorate = female_doctorateE
    )
}

get_state_acs <- function(state_abbr, acs_year) {
  
  message(
    "Downloading ",
    state_abbr,
    " - ",
    acs_year
  )
  
  get_acs(
    geography = "state",
    variables = acs_variables,
    state = state_abbr,
    year = acs_year,
    survey = "acs5",
    output = "wide",
    geometry = FALSE,
    cache_table = TRUE
  ) %>%
    transmute(
      GEOID,
      NAME,
      state_po = state_abbr,
      year = acs_year,
      
      population = populationE,
      median_age = median_ageE,
      white_nonhispanic = white_nonhispanicE,
      black_nonhispanic = black_nonhispanicE,
      hispanic = hispanicE,
      hispanic_mexican = hispanic_mexicanE,
      hispanic_puerto_rican = hispanic_puerto_ricanE,
      hispanic_cuban = hispanic_cubanE,
      hispanic_honduran = hispanic_honduranE,
      hispanic_venezuelan = hispanic_venezuelanE,
      hispanic_salvadoran = hispanic_salvadoranE,
      hispanic_nicaraguan = hispanic_nicaraguanE,
      hispanic_guatemalan = hispanic_guatemalanE,
      hispanic_dominican = hispanic_dominicanE,
      asian = asianE,
      native_american = native_americanE,
      native_hawaiian_pacific = native_hawaiian_pacificE,
      
      median_household_income = median_household_incomeE,
      per_capita_income = per_capita_incomeE,
      poverty_universe = poverty_universeE,
      below_poverty = below_povertyE,
      
      education_25plus = education_25plusE,
      
      male_hs = male_hsE,
      male_some_college_less_1yr = male_some_college_less_1yrE,
      male_some_college_1plus = male_some_college_1plusE,
      male_associates = male_associatesE,
      male_bachelors = male_bachelorsE,
      male_masters = male_mastersE,
      male_professional = male_professionalE,
      male_doctorate = male_doctorateE,
      
      female_hs = female_hsE,
      female_some_college_less_1yr = female_some_college_less_1yrE,
      female_some_college_1plus = female_some_college_1plusE,
      female_associates = female_associatesE,
      female_bachelors = female_bachelorsE,
      female_masters = female_mastersE,
      female_professional = female_professionalE,
      female_doctorate = female_doctorateE
    )
}

######
######
######
######
######
######
# Importing Employment and Education USDA Files
unemployment = read.csv("Unemployment by County and State.csv")

unemployment <- unemployment %>%
  mutate(
    FIPS_Code = str_pad(
      as.character(FIPS_Code),
      width = 5,
      side = "left",
      pad = "0"
    )
  )
View(unemployment)

education <- read.csv(
  "Education by County and State Over Time.csv",
  fileEncoding = "latin1"
)

education <- education %>%
  mutate(
    FIPS_Code = str_pad(
      as.character(FIPS_Code),
      width = 5,
      side = "left",
      pad = "0"
    )
  )

View(education)

unemployment_clean <- unemployment %>%
  filter(
    Measure == "Unemployment Rate"
  ) %>%
  select(
    FIPS_Code,
    Year,
    Measure,
    Value
  ) %>%
  mutate(
    unemployment_year = as.numeric(Year),
    
    election_year = case_when(
      unemployment_year == 2011 ~ 2012,
      unemployment_year == 2015 ~ 2016,
      unemployment_year == 2019 ~ 2020,
      unemployment_year == 2023 ~ 2024,
      TRUE ~ NA_real_
    )
  ) %>%
  filter(
    !is.na(election_year)
  ) %>%
  rename(
    unemployment_rate = Value
  )
View(unemployment_clean)

### State unemployment

state_unemployment_clean <- unemployment %>%
  filter(
    Measure == "Unemployment Rate"
  ) %>%
  mutate(
    FIPS_Code = str_pad(
      as.character(FIPS_Code),
      width = 5,
      side = "left",
      pad = "0"
    )
  ) %>%
  
  # Keep statewide observations
  filter(
    substr(FIPS_Code, 3, 5) == "000"
  ) %>%
  
  mutate(
    state_fips = substr(FIPS_Code, 1, 2),
    unemployment_year = as.numeric(Year),
    
    election_year = case_when(
      unemployment_year == 2011 ~ 2012,
      unemployment_year == 2015 ~ 2016,
      unemployment_year == 2019 ~ 2020,
      unemployment_year == 2023 ~ 2024,
      TRUE ~ NA_real_
    )
  ) %>%
  filter(
    !is.na(election_year)
  ) %>%
  select(
    state_fips,
    election_year,
    unemployment_rate = Value
  )

#education_clean <- education %>%
#  filter(
#    Measure == c("Percent Less Than High School", "Percent High School Only", "Percent Some College or Associate Degree", "Percent Bachelor's Degree or Higher")
#  ) %>%
#  select(
#    FIPS_Code,
#    Year,
#    Measure,
#    Value
#  )
#View(education_clean)

######
######
######
######
######
######
######
######
######



#get_state_acs <- function(state_abbr, acs_year) {
#  
#  message(
#    "Downloading statewide data: ",
#    state_abbr,
#    " - ",
#    acs_year
#  )
#  
#  get_acs(
#    geography = "state",
#    variables = acs_variables,
#    state = state_abbr,
#    year = acs_year,
#    survey = "acs5",
#    output = "wide",
#    geometry = FALSE,
#    cache_table = TRUE
#  ) %>%
#    transmute(
#      GEOID,
#      NAME,
#      state_po = state_abbr,
#      year = acs_year,
#      
#      population = populationE,
#      median_age = median_ageE,
#      white_nonhispanic = white_nonhispanicE,
#      black_nonhispanic = black_nonhispanicE,
#      hispanic = hispanicE
#    )
#}
#View(test_nc_state)


######################################### Create final county version

# Create every state × ACS year combination
acs_requests <- crossing(
  state_po = states,
  acs_year = acs_years
)

# Download all county ACS data
county_acs <- acs_requests %>%
  mutate(
    data = map2(
      state_po,
      acs_year,
      get_county_acs
    )
  ) %>%
  select(data) %>%
  unnest(data)

county_acs <- county_acs %>%
  rename(
    acs_year = year
  ) %>%
  mutate(
    election_year = case_when(
      acs_year == 2011 ~ 2012,
      acs_year == 2015 ~ 2016,
      acs_year == 2020 ~ 2020,
      acs_year == 2023 ~ 2024
    )
  )

county_acs %>%
  count(state_po, acs_year, election_year) %>%
  arrange(state_po, acs_year) %>%
  print(n = Inf)

county_acs %>%
  summarise(
    rows = n(),
    unique_counties = n_distinct(GEOID),
    states = n_distinct(state_po)
  )
county_acs <- county_acs %>%
  filter(acs_year != 2010)

View(county_acs)

# Preparing to merge with acs data
unemployment_clean %>%
  count(FIPS_Code, election_year) %>%
  filter(n > 1)

county_panel <- county_acs %>%
  left_join(
    unemployment_clean,
    by = c(
      "GEOID" = "FIPS_Code",
      "election_year" = "election_year"
    )
  )

county_panel$Year = NULL
county_panel$Measure = NULL
county_panel$unemployment_year = NULL

county_panel <- county_panel %>%
  mutate(
    white_nonhispanic_pct =
      (white_nonhispanic / population) * 100,
    
    black_pct =
      (black_nonhispanic / population) * 100,
    
    hispanic_pct =
      (hispanic / population) * 100,
    
    hispanic_mexican_pct =
      (hispanic_mexican / population) * 100,
    
    hispanic_puerto_rican_pct =
      (hispanic_puerto_rican / population) * 100,
    
    hispanic_cuban_pct =
      (hispanic_cuban / population) * 100,
    
    hispanic_honduran_pct =
      (hispanic_honduran / population) * 100,
    
    hispanic_venezuelan_pct =
      (hispanic_venezuelan / population) * 100,
    
    hispanic_salvadoran_pct =
      (hispanic_salvadoran / population) * 100,
    
    hispanic_nicaraguan_pct =
      (hispanic_nicaraguan / population) * 100,
    
    hispanic_guatemalan_pct =
      (hispanic_guatemalan / population) * 100,
    
    hispanic_dominican_pct =
      (hispanic_dominican / population) * 100,
    
    asian_pct =
      (asian / population) * 100,
    
    native_american_pct =
      (native_american / population) * 100,
    
    native_hawaiian_pacific_pct =
      (native_hawaiian_pacific / population) * 100,
    
    poverty_pct =
      (below_poverty / poverty_universe) * 100
  )

county_panel <- county_panel %>%
  mutate(
    
    # High school graduate or higher
    hs_or_higher =
      male_hs +
      male_some_college_less_1yr +
      male_some_college_1plus +
      male_associates +
      male_bachelors +
      male_masters +
      male_professional +
      male_doctorate +
      female_hs +
      female_some_college_less_1yr +
      female_some_college_1plus +
      female_associates +
      female_bachelors +
      female_masters +
      female_professional +
      female_doctorate,
    
    # Bachelor's degree or higher
    bachelors_or_higher =
      male_bachelors +
      male_masters +
      male_professional +
      male_doctorate +
      female_bachelors +
      female_masters +
      female_professional +
      female_doctorate,
    
    # Percentages
    hs_or_higher_pct =
      (hs_or_higher / education_25plus) * 100,
    
    bachelors_or_higher_pct =
      (bachelors_or_higher / education_25plus) * 100
  )

View(county_panel)

write_csv(
  county_panel,
  "county_panel_background.csv"
)


####################### Create final state panel

# Create every state × ACS year combination
state_acs_requests <- crossing(
  state_po = states,
  acs_year = acs_years
)

# Download all state ACS data
state_acs <- state_acs_requests %>%
  mutate(
    data = map2(
      state_po,
      acs_year,
      get_state_acs
    )
  ) %>%
  select(data) %>%
  unnest(data)

# Add corresponding election year
state_acs <- state_acs %>%
  rename(
    acs_year = year
  ) %>%
  mutate(
    election_year = case_when(
      acs_year == 2011 ~ 2012,
      acs_year == 2015 ~ 2016,
      acs_year == 2020 ~ 2020,
      acs_year == 2023 ~ 2024,
      TRUE ~ NA_real_
    )
  )

View(state_acs)

state_panel <- state_acs %>%
  left_join(
    state_unemployment_clean,
    by = c(
      "GEOID" = "state_fips",
      "election_year" = "election_year"
    )
  )

state_panel <- state_panel %>%
  mutate(
    white_nonhispanic_pct =
      (white_nonhispanic / population) * 100,
    
    black_pct =
      (black_nonhispanic / population) * 100,
    
    hispanic_pct =
      (hispanic / population) * 100,
    
    hispanic_mexican_pct =
      (hispanic_mexican / population) * 100,
    
    hispanic_puerto_rican_pct =
      (hispanic_puerto_rican / population) * 100,
    
    hispanic_cuban_pct =
      (hispanic_cuban / population) * 100,
    
    hispanic_honduran_pct =
      (hispanic_honduran / population) * 100,
    
    hispanic_venezuelan_pct =
      (hispanic_venezuelan / population) * 100,
    
    hispanic_salvadoran_pct =
      (hispanic_salvadoran / population) * 100,
    
    hispanic_nicaraguan_pct =
      (hispanic_nicaraguan / population) * 100,
    
    hispanic_guatemalan_pct =
      (hispanic_guatemalan / population) * 100,
    
    hispanic_dominican_pct =
      (hispanic_dominican / population) * 100,
    
    asian_pct =
      (asian / population) * 100,
    
    native_american_pct =
      (native_american / population) * 100,
    
    native_hawaiian_pacific_pct =
      (native_hawaiian_pacific / population) * 100,
    
    poverty_pct =
      (below_poverty / poverty_universe) * 100
  )

state_panel <- state_panel %>%
  mutate(
    
    hs_or_higher =
      male_hs +
      male_some_college_less_1yr +
      male_some_college_1plus +
      male_associates +
      male_bachelors +
      male_masters +
      male_professional +
      male_doctorate +
      female_hs +
      female_some_college_less_1yr +
      female_some_college_1plus +
      female_associates +
      female_bachelors +
      female_masters +
      female_professional +
      female_doctorate,
    
    bachelors_or_higher =
      male_bachelors +
      male_masters +
      male_professional +
      male_doctorate +
      female_bachelors +
      female_masters +
      female_professional +
      female_doctorate,
    
    hs_or_higher_pct =
      (hs_or_higher / education_25plus) * 100,
    
    bachelors_or_higher_pct =
      (bachelors_or_higher / education_25plus) * 100
  )

View(state_panel)

write_csv(
  state_panel,
  "state_panel_background.csv"
)
