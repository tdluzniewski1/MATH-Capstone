library(tidycensus)
library(tidyverse)

vars_2024 <- load_variables(
  2024,
  "acs5",
  cache = TRUE
)


vars_2024 %>%
  filter(str_detect(
    label,
    regex("median household income", ignore_case = TRUE)
  ))

vars_2024 %>%
  filter(str_detect(
    label,
    regex("bachelor", ignore_case = TRUE)
  ))

vars_2024 %>%
  filter(str_detect(
    label,
    regex("unemployed", ignore_case = TRUE)
  ))

View(vars_2024)

search_acs <- function(data, term) {
  
  data %>%
    filter(
      str_detect(
        paste(concept, label),
        regex(term, ignore_case = TRUE)
      )
    ) %>%
    select(name, label, concept)
}

search_acs(vars_2024, "median household income")

w <- search_acs(vars_2024, "Hispanic or latino")
View(w)

search_acs(vars_2024, "educational attainment")

search_acs(vars_2024, "labor force")

w <- search_acs(vars_2024, "participation")
View(w)

r<- search_acs(vars_2024, "education")
View(r)
t <- vars_2024
t <- t %>%
  filter(name == "B15003_025")
View(t)

