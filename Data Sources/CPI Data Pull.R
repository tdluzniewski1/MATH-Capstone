library(httr)
library(jsonlite)
library(tidyverse)

# BLS regional CPI-U series
cpi_series <- c(
  Northeast = "CUUR0100SA0",
  Midwest   = "CUUR0200SA0",
  South     = "CUUR0300SA0",
  West      = "CUUR0400SA0"
)

# Function to retrieve BLS data
get_bls_cpi <- function(start_year, end_year) {
  
  request_body <- list(
    seriesid = unname(cpi_series),
    startyear = as.character(start_year),
    endyear = as.character(end_year)
  )
  
  response <- POST(
    "https://api.bls.gov/publicAPI/v2/timeseries/data/",
    body = request_body,
    encode = "json"
  )
  
  data <- content(response, as = "parsed")
  
  map_dfr(data$Results$series, function(series) {
    
    region <- names(cpi_series)[
      cpi_series == series$seriesID
    ]
    
    map_dfr(series$data, function(x) {
      tibble(
        region = region,
        series_id = series$seriesID,
        year = as.integer(x$year),
        period = x$period,
        month = x$periodName,
        cpi = as.numeric(x$value)
      )
    })
  })
}

cpi_2000_2009 <- get_bls_cpi(2000, 2009)

cpi_2010_2019 <- get_bls_cpi(2010, 2019)

cpi_2020_2024 <- get_bls_cpi(2020, 2024)

# Combine all three periods
regional_cpi <- bind_rows(
  cpi_2000_2009,
  cpi_2010_2019,
  cpi_2020_2024
) %>%
  arrange(region, year, period)

# Check years retrieved
regional_cpi %>%
  count(region, year) %>%
  arrange(region, year) %>%
  print(n = Inf)

sort(unique(regional_cpi$year))
View(regional_cpi)

write.csv(regional_cpi, "CPI by Region since 2000.csv", row.names=FALSE)