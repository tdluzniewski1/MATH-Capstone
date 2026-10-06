setwd("C:\\Users\\thoma\\OneDrive\\Desktop\\WCU\\Classes\\MATH-479\\Data Sources")
install.packages("corrplot")
install.packages("car")
install.packages("cli")
install.packages("tidymodels")
library(tidyverse)
library(corrplot)
library(car)

county_panel <- read_csv(
  "county_panel_complete.csv",
  show_col_types = FALSE
)

state_panel <- read_csv(
  "state_panel_complete.csv",
  show_col_types = FALSE
)

# Initial EDA

county_panel %>%
  group_by(election_year) %>%
  summarise(
    counties = n(),
    
    avg_dem_pct = mean(dem_pct, na.rm = TRUE),
    median_dem_pct = median(dem_pct, na.rm = TRUE),
    
    avg_dem_shift = mean(dem_shift, na.rm = TRUE),
    median_dem_shift = median(dem_shift, na.rm = TRUE),
    
    sd_dem_shift = sd(dem_shift, na.rm = TRUE),
    
    avg_unemployment = mean(unemployment_rate, na.rm = TRUE),
    avg_bachelors_pct = mean(bachelors_or_higher_pct, na.rm = TRUE),
    avg_poverty_pct = mean(poverty_pct, na.rm = TRUE)
  )
# Change in democratic vote share (2012-2024)
ggplot(
  county_panel,
  aes(
    x = dem_shift,
    fill = factor(election_year)
  )
) +
  geom_histogram(
    bins = 50,
    alpha = 0.7
  ) +
  facet_wrap(~ election_year) +
  geom_vline(
    xintercept = 0,
    linetype = "dashed"
  ) +
  labs(
    title = "Distribution of County Presidential Vote Shifts",
    subtitle = "Change in Democratic two-party vote share from the previous presidential election",
    x = "Democratic Vote Share Shift (Percentage Points)",
    y = "Number of Counties",
    fill = "Election Year"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none"
  )

selected_states <- c(
  "NC", "GA", "PA", "MI",
  "WI", "AZ", "TX", "FL"
)



# Education shift for each election
ggplot(
  county_panel,
  aes(
    x = bachelors_or_higher_pct,
    y = dem_shift
  )
) +
  geom_point(
    alpha = 0.15,
    size = 1
  ) +
  geom_smooth(
    method = "lm",
    se = TRUE
  ) +
  facet_wrap(~ election_year) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed"
  ) +
  labs(
    title = "Educational Attainment and County Presidential Vote Shifts",
    subtitle = "Relationship shown separately for each presidential election",
    x = "Bachelor's Degree or Higher (%)",
    y = "Democratic Vote Share Shift (Percentage Points)"
  ) +
  theme_minimal()

# Unemployment shift relative to dem_shift
ggplot(
  county_panel,
  aes(
    x = unemployment_rate,
    y = dem_shift
  )
) +
  geom_point(
    alpha = 0.15,
    size = 1
  ) +
  geom_smooth(
    method = "lm",
    se = TRUE
  ) +
  facet_wrap(~ election_year) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed"
  ) +
  labs(
    title = "Unemployment and County Presidential Vote Shifts",
    x = "Unemployment Rate (%)",
    y = "Democratic Vote Share Shift (Percentage Points)"
  ) +
  theme_minimal()
# ============================================================
# LINEAR REGRESSION MODELS
# ============================================================
#
# Primary model:
#   Economic and demographic characteristics only
#
# Political-history model:
#   Economic/demographic characteristics +
#   previous Democratic two-party vote share
#
# Training elections: 2012, 2016, 2020
# Test election:       2024
# ============================================================


# ============================================================
# 1. COUNTY-LEVEL MODELS
# ============================================================
# Create previous Democratic vote share
county_model_data <- county_panel %>%
  arrange(GEOID, election_year) %>%
  group_by(GEOID) %>%
  ungroup() %>%
  filter(
    !is.na(dem_pct),
    !is.na(previous_dem_pct),
    !is.na(median_age),
    !is.na(median_household_income),
    !is.na(unemployment_rate),
    !is.na(poverty_pct),
    !is.na(bachelors_or_higher_pct),
    !is.na(white_nonhispanic_pct),
    !is.na(black_pct),
    !is.na(hispanic_pct),
    !is.na(hispanic_mexican_pct),
    !is.na(hispanic_puerto_rican_pct),
    !is.na(hispanic_cuban_pct),
    !is.na(hispanic_honduran_pct),
    !is.na(hispanic_venezuelan_pct),
    !is.na(hispanic_salvadoran_pct),
    !is.na(hispanic_nicaraguan_pct),
    !is.na(hispanic_guatemalan_pct),
    !is.na(hispanic_dominican_pct)
    
  )


# ------------------------------------------------------------
# Training/test split
# ------------------------------------------------------------

county_training <- county_model_data %>%
  filter(election_year < 2024)


county_test <- county_model_data %>%
  filter(election_year == 2024)


# ------------------------------------------------------------
# Correlation with Variables
# ------------------------------------------------------------

correlation_data <- county_training %>%
  select(
    dem_pct,
    previous_dem_pct,
    median_age,
    median_household_income,
    unemployment_rate,
    poverty_pct,
    bachelors_or_higher_pct,
    white_nonhispanic_pct,
    black_pct,
    hispanic_pct
  )

core_cor <- cor(
  correlation_data,
  use = "pairwise.complete.obs"
)

corrplot(
  core_cor,
  method = "color",
  type = "lower",
  addCoef.col = "black",
  number.cex = 0.8,
  tl.col = "black",
  tl.srt = 45,
  diag = FALSE
)

## Correlation with Hispanic subgroups

correlation_data_hispanic <- county_training %>%
  select(
    dem_pct,
    previous_dem_pct,
    hispanic_mexican_pct,
    hispanic_puerto_rican_pct,
    hispanic_cuban_pct,
    hispanic_honduran_pct,
    hispanic_venezuelan_pct,
    hispanic_salvadoran_pct,
    hispanic_nicaraguan_pct,
    hispanic_guatemalan_pct,
    hispanic_dominican_pct
  )

core_cor_hispanic <- cor(
  correlation_data_hispanic,
  use = "pairwise.complete.obs"
)

corrplot (
  core_cor_hispanic,
  method = "color",
  type = "lower",
  addCoef.col = "black",
  number.cex = 0.8,
  tl.col = "black",
  tl.srt = 45,
  diag = FALSE
)


# ------------------------------------------------------------
# Economic/demographic model
# ------------------------------------------------------------

county_model_econ <- lm(
  dem_pct ~
    median_age +
    median_household_income +
    unemployment_rate +
    poverty_pct +
    bachelors_or_higher_pct +
    white_nonhispanic_pct +
    black_pct +
    hispanic_mexican_pct +
    hispanic_puerto_rican_pct +
    hispanic_cuban_pct +
    hispanic_honduran_pct +
    hispanic_venezuelan_pct +
    hispanic_salvadoran_pct +
    hispanic_nicaraguan_pct +
    hispanic_guatemalan_pct +
    hispanic_dominican_pct,
  data = county_training
)

summary(county_model_econ)


# ------------------------------------------------------------
# Political-history augmented model
# ------------------------------------------------------------

county_model_pol <- lm(
  dem_pct ~
    previous_dem_pct +
    median_age +
    median_household_income +
    unemployment_rate +
    poverty_pct +
    bachelors_or_higher_pct +
    white_nonhispanic_pct +
    black_pct +
    hispanic_mexican_pct +
    hispanic_puerto_rican_pct +
    hispanic_cuban_pct +
    hispanic_honduran_pct +
    hispanic_venezuelan_pct +
    hispanic_salvadoran_pct +
    hispanic_nicaraguan_pct +
    hispanic_guatemalan_pct +
    hispanic_dominican_pct,
  data = county_training
)

summary(county_model_pol)

# ------------------------------------------------------------
# Correlation with Variables (County)
# ------------------------------------------------------------

correlation_data <- county_training %>%
  select(
    dem_pct,
    median_age,
    median_household_income,
    unemployment_rate,
    poverty_pct,
    bachelors_or_higher_pct,
    white_nonhispanic_pct,
    black_pct,
    hispanic_pct
  )

core_cor <- cor(
  correlation_data,
  use = "pairwise.complete.obs"
)

corrplot(
  core_cor,
  method = "color",
  type = "lower",
  addCoef.col = "black",
  number.cex = 0.8,
  tl.col = "black",
  tl.srt = 45,
  diag = FALSE
)


# ------------------------------------------------------------
# 2024 predictions: economic/demographic model
# ------------------------------------------------------------

county_test_econ <- county_test %>%
  mutate(
    predicted_dem_pct = predict(
      county_model_econ,
      newdata = county_test
    ),
    
    prediction_error =
      dem_pct - predicted_dem_pct,
    
    absolute_error =
      abs(prediction_error),
    
    squared_error =
      prediction_error^2,
    
    actual_winner = case_when(
      dem_pct > 50 ~ "Democratic",
      dem_pct < 50 ~ "Republican",
      TRUE ~ "Tie"
    ),
    
    predicted_winner = case_when(
      predicted_dem_pct > 50 ~ "Democratic",
      predicted_dem_pct < 50 ~ "Republican",
      TRUE ~ "Tie"
    ),
    
    winner_correct =
      actual_winner == predicted_winner,
    
    winner_result = case_when(
      actual_winner == predicted_winner ~ "Correct",
      actual_winner == "Democratic" &
        predicted_winner == "Republican" ~
        "Predicted R / Actual D",
      actual_winner == "Republican" &
        predicted_winner == "Democratic" ~
        "Predicted D / Actual R",
      TRUE ~ "Other"
    )
  )

# ------------------------------------------------------------
# 2024 predictions: political-history model
# ------------------------------------------------------------

county_test_pol <- county_test %>%
  mutate(
    predicted_dem_pct = predict(
      county_model_pol,
      newdata = county_test
    ),
    
    prediction_error =
      dem_pct - predicted_dem_pct,
    
    absolute_error =
      abs(prediction_error),
    
    squared_error =
      prediction_error^2,
    
    actual_winner = case_when(
      dem_pct > 50 ~ "Democratic",
      dem_pct < 50 ~ "Republican",
      TRUE ~ "Tie"
    ),
    
    predicted_winner = case_when(
      predicted_dem_pct > 50 ~ "Democratic",
      predicted_dem_pct < 50 ~ "Republican",
      TRUE ~ "Tie"
    ),
    
    winner_correct =
      actual_winner == predicted_winner,
    
    winner_result = case_when(
      actual_winner == predicted_winner ~ "Correct",
      actual_winner == "Democratic" &
        predicted_winner == "Republican" ~
        "Predicted R / Actual D",
      actual_winner == "Republican" &
        predicted_winner == "Democratic" ~
        "Predicted D / Actual R",
      TRUE ~ "Other"
    )
  )


# ------------------------------------------------------------
# Overall county model performance
# ------------------------------------------------------------

county_performance_econ <- county_test_econ %>%
  summarise(
    observations = n(),
    
    MAE =
      mean(absolute_error, na.rm = TRUE),
    
    RMSE =
      sqrt(mean(squared_error, na.rm = TRUE)),
    
    correlation =
      cor(
        dem_pct,
        predicted_dem_pct,
        use = "complete.obs"
      ),
    
    winner_accuracy =
      mean(winner_correct, na.rm = TRUE) * 100
  )


county_performance_pol <- county_test_pol %>%
  summarise(
    observations = n(),
    
    MAE =
      mean(absolute_error, na.rm = TRUE),
    
    RMSE =
      sqrt(mean(squared_error, na.rm = TRUE)),
    
    correlation =
      cor(
        dem_pct,
        predicted_dem_pct,
        use = "complete.obs"
      ),
    
    winner_accuracy =
      mean(winner_correct, na.rm = TRUE) * 100
  )


county_performance_econ
county_performance_pol

write.csv(county_performance_econ, "county_econ_reg_performance.csv")
write.csv(county_performance_pol, "county_pol_reg_performance.csv")


# ------------------------------------------------------------
# County performance by state
# ------------------------------------------------------------

state_model_performance_econ <- county_test_econ %>%
  group_by(state_po.x) %>%
  summarise(
    counties = n(),
    
    MAE =
      mean(absolute_error, na.rm = TRUE),
    
    RMSE =
      sqrt(mean(squared_error, na.rm = TRUE)),
    
    mean_error =
      mean(prediction_error, na.rm = TRUE),
    
    winner_accuracy =
      mean(winner_correct, na.rm = TRUE) * 100,
    
    .groups = "drop"
  )


state_model_performance_pol <- county_test_pol %>%
  group_by(state_po.x) %>%
  summarise(
    counties = n(),
    
    MAE =
      mean(absolute_error, na.rm = TRUE),
    
    RMSE =
      sqrt(mean(squared_error, na.rm = TRUE)),
    
    mean_error =
      mean(prediction_error, na.rm = TRUE),
    
    winner_accuracy =
      mean(winner_correct, na.rm = TRUE) * 100,
    
    .groups = "drop"
  )


# ------------------------------------------------------------
# Export county predictions for Tableau
# ------------------------------------------------------------

write_csv(
  county_test_econ,
  "county_2024_predictions_econ.csv"
)

write_csv(
  county_test_pol,
  "county_2024_predictions_pol.csv"
)

write_csv(
  state_model_performance_econ,
  "county_2024_predictions_econ_agg_state.csv"
)

write_csv(
  state_model_performance_pol,
  "county_2024_predictions_pol_agg_state.csv"
)



# ============================================================
# 2. STATE-LEVEL MODELS
# ============================================================


# Create previous Democratic vote share
state_model_data <- state_panel %>%
  arrange(state_po.x, election_year) %>%
  group_by(state_po.x) %>%
  ungroup() %>%
  filter(
    !is.na(dem_pct),
    !is.na(previous_dem_pct),
    !is.na(median_age),
    !is.na(median_household_income),
    !is.na(unemployment_rate),
    !is.na(poverty_pct),
    !is.na(bachelors_or_higher_pct),
    !is.na(white_nonhispanic_pct),
    !is.na(black_pct),
    !is.na(hispanic_pct),
    !is.na(hispanic_mexican_pct),
    !is.na(hispanic_puerto_rican_pct),
    !is.na(hispanic_cuban_pct),
    !is.na(hispanic_honduran_pct),
    !is.na(hispanic_venezuelan_pct),
    !is.na(hispanic_salvadoran_pct),
    !is.na(hispanic_nicaraguan_pct),
    !is.na(hispanic_guatemalan_pct),
    !is.na(hispanic_dominican_pct)
  )


# ------------------------------------------------------------
# Training/test split
# ------------------------------------------------------------

state_training <- state_model_data %>%
  filter(election_year < 2024)

state_test <- state_model_data %>%
  filter(election_year == 2024)


# ------------------------------------------------------------
# Correlation with Variables (State)
# ------------------------------------------------------------

correlation_data_s <- state_training %>%
  select(
    dem_pct,
    median_age,
    median_household_income,
    unemployment_rate,
    poverty_pct,
    bachelors_or_higher_pct,
    white_nonhispanic_pct,
    black_pct,
    hispanic_pct
  )

core_cor <- cor(
  correlation_data_s,
  use = "pairwise.complete.obs"
)

corrplot(
  core_cor,
  title="State Data Correlation with Explanatory Variables",
  method = "color",
  type = "lower",
  addCoef.col = "black",
  number.cex = 0.8,
  tl.col = "black",
  tl.srt = 45,
  diag = FALSE,
)


## Correlation with Hispanic subgroups

correlation_data_hispanic <- state_training %>%
  select(
    dem_pct,
    previous_dem_pct,
    hispanic_mexican_pct,
    hispanic_puerto_rican_pct,
    hispanic_cuban_pct,
    hispanic_honduran_pct,
    hispanic_venezuelan_pct,
    hispanic_salvadoran_pct,
    hispanic_nicaraguan_pct,
    hispanic_guatemalan_pct,
    hispanic_dominican_pct
  )

core_cor_hispanic <- cor(
  correlation_data_hispanic,
  use = "pairwise.complete.obs"
)

corrplot (
  core_cor_hispanic,
  title="State Data Correlation with Hispanic Subgroups",
  method = "color",
  type = "lower",
  addCoef.col = "black",
  number.cex = 0.8,
  tl.col = "black",
  tl.srt = 45,
  diag = FALSE
)


# ------------------------------------------------------------
# Economic/demographic model
# ------------------------------------------------------------

state_model_econ <- lm(
  dem_pct ~
    median_age +
    median_household_income +
    unemployment_rate +
    poverty_pct +
    bachelors_or_higher_pct +
    white_nonhispanic_pct +
    black_pct +
    hispanic_mexican_pct +
    hispanic_puerto_rican_pct +
    hispanic_cuban_pct +
    hispanic_honduran_pct +
    hispanic_venezuelan_pct +
    hispanic_salvadoran_pct +
    hispanic_nicaraguan_pct +
    hispanic_guatemalan_pct +
    hispanic_dominican_pct,
  
  data = state_training
)

summary(state_model_econ)


# ------------------------------------------------------------
# Political-history augmented model
# ------------------------------------------------------------

state_model_pol <- lm(
  dem_pct ~
    previous_dem_pct +
    median_age +
    median_household_income +
    unemployment_rate +
    poverty_pct +
    bachelors_or_higher_pct +
    white_nonhispanic_pct +
    black_pct +hispanic_mexican_pct +
    hispanic_puerto_rican_pct +
    hispanic_cuban_pct +
    hispanic_honduran_pct +
    hispanic_venezuelan_pct +
    hispanic_salvadoran_pct +
    hispanic_nicaraguan_pct +
    hispanic_guatemalan_pct +
    hispanic_dominican_pct,
  data = state_training
)

summary(state_model_pol)


# ------------------------------------------------------------
# 2024 predictions: economic/demographic model
# ------------------------------------------------------------

state_test_econ <- state_test %>%
  mutate(
    predicted_dem_pct = predict(
      state_model_econ,
      newdata = state_test
    ),
    
    prediction_error =
      dem_pct - predicted_dem_pct,
    
    absolute_error =
      abs(prediction_error),
    
    squared_error =
      prediction_error^2,
    
    actual_winner = case_when(
      dem_pct > 50 ~ "Democratic",
      dem_pct < 50 ~ "Republican",
      TRUE ~ "Tie"
    ),
    
    predicted_winner = case_when(
      predicted_dem_pct > 50 ~ "Democratic",
      predicted_dem_pct < 50 ~ "Republican",
      TRUE ~ "Tie"
    ),
    
    winner_correct =
      actual_winner == predicted_winner,
    
    winner_result = case_when(
      actual_winner == predicted_winner ~ "Correct",
      actual_winner == "Democratic" &
        predicted_winner == "Republican" ~
        "Predicted R / Actual D",
      actual_winner == "Republican" &
        predicted_winner == "Democratic" ~
        "Predicted D / Actual R",
      TRUE ~ "Other"
    )
  )


# ------------------------------------------------------------
# 2024 predictions: political-history model
# ------------------------------------------------------------
# summary(state_model_pol)
state_test_pol <- state_test %>%
  mutate(
    predicted_dem_pct = predict(
      state_model_pol,
      newdata = state_test
    ),
    
    prediction_error =
      dem_pct - predicted_dem_pct,
    
    absolute_error =
      abs(prediction_error),
    
    squared_error =
      prediction_error^2,
    
    actual_winner = case_when(
      dem_pct > 50 ~ "Democratic",
      dem_pct < 50 ~ "Republican",
      TRUE ~ "Tie"
    ),
    
    predicted_winner = case_when(
      predicted_dem_pct > 50 ~ "Democratic",
      predicted_dem_pct < 50 ~ "Republican",
      TRUE ~ "Tie"
    ),
    
    winner_correct =
      actual_winner == predicted_winner,
    
    winner_result = case_when(
      actual_winner == predicted_winner ~ "Correct",
      actual_winner == "Democratic" &
        predicted_winner == "Republican" ~
        "Predicted R / Actual D",
      actual_winner == "Republican" &
        predicted_winner == "Democratic" ~
        "Predicted D / Actual R",
      TRUE ~ "Other"
    )
  )


# ------------------------------------------------------------
# Overall state model performance
# ------------------------------------------------------------

state_performance_econ <- state_test_econ %>%
  summarise(
    observations = n(),
    
    MAE =
      mean(absolute_error, na.rm = TRUE),
    
    RMSE =
      sqrt(mean(squared_error, na.rm = TRUE)),
    
    correlation =
      cor(
        dem_pct,
        predicted_dem_pct,
        use = "complete.obs"
      ),
    
    winner_accuracy =
      mean(winner_correct, na.rm = TRUE) * 100
  )


state_performance_pol <- state_test_pol %>%
  summarise(
    observations = n(),
    
    MAE =
      mean(absolute_error, na.rm = TRUE),
    
    RMSE =
      sqrt(mean(squared_error, na.rm = TRUE)),
    
    correlation =
      cor(
        dem_pct,
        predicted_dem_pct,
        use = "complete.obs"
      ),
    
    winner_accuracy =
      mean(winner_correct, na.rm = TRUE) * 100
  )


state_performance_econ
state_performance_pol


write.csv(state_performance_econ, "state_econ_reg_performance.csv")
write.csv(state_performance_pol, "state_pol_reg_performance.csv")


# ------------------------------------------------------------
# Export state predictions for Tableau
# ------------------------------------------------------------

write_csv(
  state_test_econ,
  "state_2024_predictions_econ.csv"
)

write_csv(
  state_test_pol,
  "state_2024_predictions_pol.csv"
)

### Compare econ and political models

model_comparison <- county_test_econ %>%
  select(
    GEOID,
    state_po.x,
    election_year,
    dem_pct,
    econ_prediction = predicted_dem_pct,
    econ_absolute_error = absolute_error
  ) %>%
  left_join(
    county_test_pol %>%
      select(
        GEOID,
        pol_prediction = predicted_dem_pct,
        pol_absolute_error = absolute_error
      ),
    by = "GEOID"
  ) %>%
  mutate(
    error_improvement =
      econ_absolute_error - pol_absolute_error
  )

model_comparison %>%
  summarise(
    mean_improvement = mean(error_improvement, na.rm = TRUE),
    
    median_improvement = median(error_improvement, na.rm = TRUE),
    
    political_better =
      mean(error_improvement > 0, na.rm = TRUE) * 100,
    
    econ_better =
      mean(error_improvement < 0, na.rm = TRUE) * 100
  )

# VIF Testing

# County economic model
vif(county_model_econ)
vif(county_model_pol)
vif(state_model_econ)
vif(state_model_pol)

####################
# K-Means Regression Prediction

