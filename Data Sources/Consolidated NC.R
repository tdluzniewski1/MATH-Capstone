setwd("C:\\Users\\thoma\\OneDrive\\Desktop\\WCU\\Classes\\MATH-479\\Data Sources")

library(tidyverse)

# --------------------------------------------------
# Helper functions
# --------------------------------------------------

clean_label <- function(x) {
  x %>%
    str_replace_all("Â", "") %>%
    str_replace_all("\u00A0", " ") %>%
    str_squish()
}

clean_numeric <- function(x) {
  if (is.numeric(x)) {
    x
  } else {
    readr::parse_number(as.character(x))
  }
}

# --------------------------------------------------
# Find all NC statewide ACS files automatically
# --------------------------------------------------

acs_files <- list.files(
  path = ".",
  pattern = "^NC Statewide ACS.*\\.csv$",
  full.names = TRUE
)

acs_files

read_acs_file <- function(file) {
  
  file_name <- basename(file)
  
  # Get year automatically from filename
  file_year <- file_name %>%
    str_extract("(19|20)\\d{2}") %>%
    as.integer()
  
  # Determine category automatically
  file_category <- case_when(
    str_detect(file_name, regex("Demographics", ignore_case = TRUE)) ~
      "Demographics",
    
    str_detect(file_name, regex("Economic Characteristics", ignore_case = TRUE)) ~
      "Economic",
    
    str_detect(file_name, regex("Educational Attainment", ignore_case = TRUE)) ~
      "Education",
    
    str_detect(file_name, regex("Income", ignore_case = TRUE)) ~
      "Income",
    
    str_detect(file_name, regex("Poverty", ignore_case = TRUE)) ~
      "Poverty",
    
    TRUE ~ "Other"
  )
  
  # Read every column as character
  df <- read_csv(
    file,
    locale = locale(encoding = "latin1"),
    col_types = cols(.default = col_character()),
    show_col_types = FALSE
  )
  
  # Rename first column
  names(df)[1] <- "label"
  
  df %>%
    mutate(
      label = clean_label(label),
      state = "North Carolina",
      state_po = "NC",
      year = file_year,
      category = file_category,
      source_file = file_name,
      .before = 1
    )
}

# --------------------------------------------------
# Combine every ACS file
# --------------------------------------------------

acs_files <- list.files(
  path = ".",
  pattern = "^NC Statewide ACS.*\\.csv$",
  full.names = TRUE
)

acs_all <- map_dfr(
  acs_files,
  read_acs_file
)

acs_all_long <- acs_all %>%
  pivot_longer(
    cols = -c(
      state,
      state_po,
      year,
      category,
      source_file,
      label
    ),
    names_to = "measure",
    values_to = "value",
    values_drop_na = TRUE
  ) %>%
  arrange(
    year,
    category,
    label,
    measure
  )

acs_model_relevant <- acs_all_long %>%
  filter(
    # Remove margins of error
    !str_detect(
      measure,
      regex(
        "margin of error|moe",
        ignore_case = TRUE
      )
    )
  ) %>%
  
  filter(
    
    # ------------------------
    # Demographics
    # ------------------------
    
    str_detect(
      label,
      regex(
        paste(
          c(
            "^Total population$",
            "Median age",
            "White alone",
            "Black or African American alone",
            "Hispanic or Latino",
            "Asian alone"
          ),
          collapse = "|"
        ),
        ignore_case = TRUE
      )
    ) |
      
      # ------------------------
    # Education
    # ------------------------
    
    str_detect(
      label,
      regex(
        paste(
          c(
            "High school graduate or higher",
            "Bachelor's degree or higher",
            "Some college",
            "Associate"
          ),
          collapse = "|"
        ),
        ignore_case = TRUE
      )
    ) |
      
      # ------------------------
    # Income
    # ------------------------
    
    str_detect(
      label,
      regex(
        paste(
          c(
            "Median household income",
            "Per capita income"
          ),
          collapse = "|"
        ),
        ignore_case = TRUE
      )
    ) |
      
      # ------------------------
    # Labor market
    # ------------------------
    
    str_detect(
      label,
      regex(
        paste(
          c(
            "^In labor force$",
            "Unemployment rate"
          ),
          collapse = "|"
        ),
        ignore_case = TRUE
      )
    ) |
      
      # ------------------------
    # Poverty
    # ------------------------
    
    str_detect(
      label,
      regex(
        paste(
          c(
            "Population for whom poverty status is determined",
            "Below poverty level"
          ),
          collapse = "|"
        ),
        ignore_case = TRUE
      )
    )
  ) %>%
  
  arrange(
    year,
    category,
    label,
    measure
  )

View(acs_model_relevant)