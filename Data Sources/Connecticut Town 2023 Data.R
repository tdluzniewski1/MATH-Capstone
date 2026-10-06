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
  "CT"
)

acs_years <- c(
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
get_town_acs <- function(state_abbr, acs_year) {
  
  message(
    "Downloading towns: ",
    state_abbr,
    " - ",
    acs_year
  )
  
  get_acs(
    geography = "county subdivision",
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

unemployment = read.csv("Unemployment by County and State.csv")

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

######################################### Create final county version
ct_towns <- get_town_acs("CT", 2023)

ct_towns %>%
  select(
    GEOID,
    NAME,
    population
  ) %>%
  arrange(NAME) %>%
  print(n = Inf)

ct_towns <- ct_towns %>%
  mutate(
    town = str_remove(NAME, " town,.*$")
  )

View(ct_towns)


# Combine with counties and towns
ct_county_crosswalk <- read.csv("ct-town-county-fips-list.csv")
ct_county_crosswalk <- ct_county_crosswalk %>%
  rename(
    town = Town
  )

ct_towns <- ct_towns %>%
  left_join(
    ct_county_crosswalk,
    by = "town"
  )

ct_towns <- ct_towns %>%
  filter(!is.na(County))

View(ct_towns)

ct_towns <- ct_towns %>%
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
      female_doctorate
  )

ct_historical_counties <- ct_towns %>%
group_by(County) %>%
  summarise(
    
    # Weighted variables -- calculate BEFORE summarising population
    median_age =
      weighted.mean(
        median_age,
        population,
        na.rm = TRUE
      ),
    
    median_household_income =
      weighted.mean(
        median_household_income,
        population,
        na.rm = TRUE
      ),
    
    per_capita_income =
      weighted.mean(
        per_capita_income,
        population,
        na.rm = TRUE
      ),
    
    # Population
    population =
      sum(population, na.rm = TRUE),
    
    # Race / ethnicity
    white_nonhispanic =
      sum(white_nonhispanic, na.rm = TRUE),
    
    black_nonhispanic =
      sum(black_nonhispanic, na.rm = TRUE),
    
    hispanic =
      sum(hispanic, na.rm = TRUE),
    
    hispanic_mexican =
      sum(hispanic_mexican, na.rm = TRUE),
    
    hispanic_puerto_rican =
      sum(hispanic_puerto_rican, na.rm = TRUE),
    
    hispanic_cuban =
      sum(hispanic_cuban, na.rm = TRUE),
    
    hispanic_honduran =
      sum(hispanic_honduran, na.rm = TRUE),
    
    hispanic_venezuelan =
      sum(hispanic_venezuelan, na.rm = TRUE),
    
    hispanic_salvadoran =
      sum(hispanic_salvadoran, na.rm = TRUE),
    
    hispanic_nicaraguan =
      sum(hispanic_nicaraguan, na.rm = TRUE),
    
    hispanic_guatemalan =
      sum(hispanic_guatemalan, na.rm = TRUE),
    
    hispanic_dominican =
      sum(hispanic_dominican, na.rm = TRUE),
    
    asian =
      sum(asian, na.rm = TRUE),
    
    native_american =
      sum(native_american, na.rm = TRUE),
    
    native_hawaiian_pacific =
      sum(native_hawaiian_pacific, na.rm = TRUE),
    
    # Poverty
    poverty_universe =
      sum(poverty_universe, na.rm = TRUE),
    
    below_poverty =
      sum(below_poverty, na.rm = TRUE),
    
    # Education
    education_25plus =
      sum(education_25plus, na.rm = TRUE),
    
    hs_or_higher =
      sum(hs_or_higher, na.rm = TRUE),
    
    bachelors_or_higher =
      sum(bachelors_or_higher, na.rm = TRUE),
    
    # Approximate weighted values
    median_age =
      weighted.mean(
        median_age,
        population,
        na.rm = TRUE
      ),
    
    median_household_income =
      weighted.mean(
        median_household_income,
        population,
        na.rm = TRUE
      ),
    
    per_capita_income =
      weighted.mean(
        per_capita_income,
        population,
        na.rm = TRUE
      ),
    
    .groups = "drop"
  )

ct_historical_counties <- ct_historical_counties %>%
  mutate(
    
    state_po = "CT",
    acs_year = 2023,
    election_year = 2024,
    
    white_nonhispanic_pct =
      100 * white_nonhispanic / population,
    
    black_pct =
      100 * black_nonhispanic / population,
    
    hispanic_pct =
      100 * hispanic / population,
    
    hispanic_mexican_pct =
      100 * hispanic_mexican / population,
    
    hispanic_puerto_rican_pct =
      100 * hispanic_puerto_rican / population,
    
    hispanic_cuban_pct =
      100 * hispanic_cuban / population,
    
    hispanic_honduran_pct =
      100 * hispanic_honduran / population,
    
    hispanic_venezuelan_pct =
      100 * hispanic_venezuelan / population,
    
    hispanic_salvadoran_pct =
      100 * hispanic_salvadoran / population,
    
    hispanic_nicaraguan_pct =
      100 * hispanic_nicaraguan / population,
    
    hispanic_guatemalan_pct =
      100 * hispanic_guatemalan / population,
    
    hispanic_dominican_pct =
      100 * hispanic_dominican / population,
    
    asian_pct =
      100 * asian / population,
    
    native_american_pct =
      100 * native_american / population,
    
    native_hawaiian_pacific_pct =
      100 * native_hawaiian_pacific / population,
    
    poverty_pct =
      100 * below_poverty / poverty_universe,
    
    hs_or_higher_pct =
      100 * hs_or_higher / education_25plus,
    
    bachelors_or_higher_pct =
      100 * bachelors_or_higher / education_25plus
  )

View(ct_historical_counties)

# Check if this matches the codes in election data
ct_historical_counties <- ct_historical_counties %>%
  mutate(
    GEOID = case_when(
      County == "Fairfield"  ~ "09001",
      County == "Hartford"   ~ "09003",
      County == "Litchfield" ~ "09005",
      County == "Middlesex"  ~ "09007",
      County == "New Haven"  ~ "09009",
      County == "New London" ~ "09011",
      County == "Tolland"    ~ "09013",
      County == "Windham"    ~ "09015"
    ),
    
    NAME = paste0(
      County,
      " County, Connecticut"
    )
  )

ct_historical_counties <- ct_historical_counties %>%
  select(
    GEOID,
    NAME,
    population
  ) %>%
  arrange(NAME) %>%
  print(n = Inf)

write.csv(ct_historical_counties, "ct_historical_counties.csv")