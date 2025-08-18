# ==============================================================================
# Title:    Analysis for Dissertation – "Between Demolition & Development:
#           The Impact of Slum Demolitions on Housing Prices 
#           in the Makkah Region"
# Author:   Almaha Alghamdi
# Date:     August 2025
# Purpose:  This script contains the analysis conducted for the dissertation.
#           It examines the effects of slum demolitions on housing prices in 
#           the Makkah region. 
#           This is the script for the Synthetic Difference-in-Differences (SDID).
# ==============================================================================


# ==============================================================================
# === Section 1: Preparing SDID Data and Matching Sample ===
# ==============================================================================

# --- Notes on Region Selection ---
# We used regions: Riyadh, Madinah, Eastern, Jazan (IDs: 10, 3, 6, 5)
# These were chosen for geographical, economic, and migration comparability.

# === Step 1: Prepare Data ===
sdid_data <- main_data_filtered %>%
  select(
    year, region, price_per_sqm, log_price_per_sqm, normalized_employment,
    total_employment, total_tourists, total_population, population_growth,
    tourism_religious_interaction, transaction_volume, households,
    annual_avg_salary, expenditure, affordability_ratio
  ) %>%
  mutate(Treated = ifelse(region == "Makkah", 1, 0))

# === Step 2: Summarize by Region-Year ===
sdid_data <- sdid_data %>%
  group_by(region, year) %>%
  summarise(
    avg_price_per_sqm = mean(price_per_sqm, na.rm = TRUE),
    log_price_per_sqm = mean(log_price_per_sqm, na.rm = TRUE),
    avg_total_employment = first(total_employment),
    population_growth = first(population_growth),
    normalized_employment = first(normalized_employment),
    total_tourists = first(total_tourists),
    avg_tourism_religious_interaction = first(tourism_religious_interaction),
    avg_total_population = first(total_population),
    avg_annual_salary = first(annual_avg_salary),
    avg_affordability_ratio = first(affordability_ratio),
    avg_expenditure = first(expenditure),
    avg_households = first(households),
    Treated = first(Treated),
    .groups = "drop"
  )

# === Step 3: Balance Panel Data ===
sdid_data_balanced <- sdid_data %>%
  complete(
    region,
    year = seq(min(year), max(year), by = 1),
    fill = list(
      avg_price_per_sqm = NA,
      log_price_per_sqm = NA, 
      avg_total_employment = NA,
      population_growth = NA,
      normalized_employment = NA,
      total_tourists = NA,
      avg_tourism_religious_interaction = 0,
      avg_total_population = NA,
      avg_annual_salary = NA,
      avg_affordability_ratio = NA,
      avg_expenditure = NA,
      avg_households = NA,
      Treated = 0
    )
  ) %>%
  arrange(region, year) %>%
  mutate(region_id = as.numeric(factor(region)))

# === Step 4: Residualize Outcome ===
resid_model <- lm(avg_price_per_sqm ~ total_tourists + normalized_employment +
                    avg_total_population + avg_tourism_religious_interaction +
                    population_growth + avg_expenditure + avg_affordability_ratio + 
                    avg_households,
                  data = sdid_data_balanced)

sdid_data_balanced$residualized_outcome <- residuals(resid_model)

# === Step 5: Add Treatment Timing Indicators ===
sdid_data_balanced <- sdid_data_balanced %>%
  filter(year >= 2017, year <= 2022) %>%
  mutate(
    treated_period = ifelse(year >= 2021, 1, 0),
    time_treated = ifelse(Treated == 1 & year >= 2021, 1, 0)
  )

# === Step 6: Construct Y Matrix (Residualized Outcome) ===
treated_id <- unique(sdid_data_balanced$region_id[sdid_data_balanced$Treated == 1])

Y_matrix <- sdid_data_balanced %>%
  filter(region_id %in% c(10, 3, 6, treated_id)) %>%
  select(region_id, year, residualized_outcome) %>%
  pivot_wider(names_from = year, values_from = residualized_outcome) %>%
  column_to_rownames("region_id") %>%
  as.matrix()

Y_controls <- Y_matrix[rownames(Y_matrix) != as.character(treated_id), , drop = FALSE]
Y_treated <- Y_matrix[as.character(treated_id), , drop = FALSE]
Y <- rbind(Y_controls, Y_treated)

# === Step 7: Estimate SDID ===
N0 <- nrow(Y) - 1
T0 <- sum(as.numeric(colnames(Y)) < 2021)

sdid_results <- synthdid_estimate(Y = Y, N0 = N0, T0 = T0)

# === Step 8: Visualize Results ===
plot_sdid <- synthdid_plot(sdid_results, overlay = TRUE)

years <- as.numeric(colnames(Y))

plot_sdid +
  ggtitle("Effect of Slum Demolition on Housing Prices in Makkah (SDID Estimate)") +
  xlab("Year") +
  ylab("Residualized Price per Sqm (SAR)") +
  scale_x_continuous(breaks = years) +
  scale_color_manual(values = c("treated" = "#00BFC4", "synthetic control" = "#F8766D")) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.title = element_blank(),
    legend.position = "top",
    panel.grid.major = element_line(color = "grey90"),
    panel.grid.minor = element_blank()
  )

# === Step 9: Placebo-Based Standard Error ===
sdid_se <- synthdid_se(sdid_results, method = "placebo")

# === Step 10: Build Summary Table ===
sdid_summary <- summary(sdid_results)

sdid_summary_table <- data.frame(
  Metric = c(
    "Treatment Effect Estimate",
    "Placebo Standard Error",
    "Pre-Treatment Years",
    "Post-Treatment Years",
    "Number of Control Units",
    "Effective Control Units",
    "Effective Pre-Treatment Years",
    "Top Time Weight (2019)",
    "Second Time Weight (2018)"
  ),
  Value = c(
    round(sdid_summary$estimate, 2),
    round(sdid_se[1, 1], 2),
    T0,
    ncol(Y) - T0,
    N0 + 1,
    round(sdid_summary$dimensions["N0.effective"], 2),
    round(sdid_summary$dimensions["T0.effective"], 2),
    paste0(round(sdid_summary$periods["2019", 1] * 100, 1), "%"),
    paste0(round(sdid_summary$periods["2018", 1] * 100, 1), "%")
  )
)

print(rownames(sdid_summary$periods))
print(sdid_summary$periods)

# === Step 11: Display and Export Summary Table ===
datasummary_df(
  sdid_summary_table,
  title = "SDID Summary Table – Makkah Slum Demolition",
  output = "markdown"
)

write.csv(sdid_summary_table, "sdid_summary_table_3.csv", row.names = FALSE)

# === Additional: Calculate p-value ===
t_stat <- 252.31 / 63.43
p_value <- 2 * (1 - pnorm(abs(t_stat)))
p_value


# ==============================================================================
# === Section 2: Synthetic DiD Using Logged Prices ===
# ==============================================================================

# === Step 1: Prepare Data (Log Prices) ===
sdid_data_log <- main_data_filtered %>%
  select(
    year, region, price_per_sqm, log_price_per_sqm,
    normalized_employment, total_tourists, total_population, 
    population_growth,
    tourism_religious_interaction, transaction_volume,
    annual_avg_salary, expenditure, affordability_ratio, households,
  ) %>%
  mutate(Treated = ifelse(region == "Makkah", 1, 0))

# === Step 2: Summarize by Region-Year ===
sdid_data_log <- sdid_data_log %>%
  group_by(region, year) %>%
  summarise(
    log_price_per_sqm = mean(log_price_per_sqm, na.rm = TRUE),
    normalized_employment = first(normalized_employment),
    population_growth = first(population_growth),
    total_tourists = first(total_tourists),
    avg_tourism_religious_interaction = first(tourism_religious_interaction),
    avg_total_population = first(total_population),
    avg_annual_salary = first(annual_avg_salary),
    avg_affordability_ratio = first(affordability_ratio),
    avg_expenditure = first(expenditure),
    avg_households = first(avg_households),
    Treated = first(Treated),
    .groups = "drop"
  )

# === Step 3: Balance Panel Data ===
sdid_data_log <- sdid_data_log %>%
  complete(
    region,
    year = seq(min(year), max(year), by = 1),
    fill = list(
      log_price_per_sqm = NA,
      normalized_employment = NA,
      population_growth = NA,
      total_tourists = NA,
      avg_tourism_religious_interaction = 0,
      avg_total_population = NA,
      avg_annual_salary = NA,
      avg_affordability_ratio = NA,
      avg_households = NA,
      avg_expenditure = NA,
      Treated = 0
    )
  ) %>%
  arrange(region, year) %>%
  mutate(region_id = as.numeric(factor(region)))

# === Step 4: Residualize Log Price ===
log_resid_model <- lm(log_price_per_sqm ~ normalized_employment + total_tourists +
                        population_growth + avg_households + avg_affordability_ratio +
                        avg_total_population + avg_tourism_religious_interaction + 
                        avg_annual_salary + avg_expenditure,
                      data = sdid_data_log)

sdid_data_log$residualized_log_price <- residuals(log_resid_model)

# === Step 5: Add Treatment Timing Indicators ===
sdid_data_log <- sdid_data_log %>%
  filter(year >= 2017, year <= 2022) %>%
  mutate(
    treated_period = ifelse(year >= 2021, 1, 0),
    time_treated = ifelse(Treated == 1 & year >= 2021, 1, 0)
  )

# === Step 6: Construct Y Matrix (Residualized Log Prices) ===
treated_id <- unique(sdid_data_log$region_id[sdid_data_log$Treated == 1])

Y_matrix_log <- sdid_data_log %>%
  filter(region_id %in% c(10, 3, 6, treated_id)) %>%
  select(region_id, year, residualized_log_price) %>%
  pivot_wider(names_from = year, values_from = residualized_log_price) %>%
  column_to_rownames("region_id") %>%
  as.matrix()

Y_controls_log <- Y_matrix_log[rownames(Y_matrix_log) != as.character(treated_id), , drop = FALSE]
Y_treated_log <- Y_matrix_log[as.character(treated_id), , drop = FALSE]
Y_log <- rbind(Y_controls_log, Y_treated_log)

# === Step 7: Estimate SDID (Log Prices) ===
N0_log <- nrow(Y_log) - 1
T0_log <- sum(as.numeric(colnames(Y_log)) < 2021)

sdid_results_log <- synthdid_estimate(Y = Y_log, N0 = N0_log, T0 = T0_log)

# === Step 8: Visualize (Log Prices) ===
plot_sdid_log <- synthdid_plot(sdid_results_log, overlay = TRUE)

plot_sdid_log +
  ggtitle("Effect of Slum Demolition on Housing Prices (Log Prices)") +
  xlab("Year") +
  ylab("Residualized Log Price per Sqm") +
  scale_color_manual(values = c("treated" = "#00BFC4", "synthetic control" = "#F8766D")) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.title = element_blank(),
    legend.position = "top",
    panel.grid.major = element_line(color = "grey90"),
    panel.grid.minor = element_blank()
  )

# === Step 9: Placebo-Based Standard Error (Log Prices) ===
sdid_se_log <- synthdid_se(sdid_results_log, method = "placebo")

# === Step 10: Build Summary Table (Log Prices) ===
sdid_summary_log <- summary(sdid_results_log)

sdid_summary_table_log <- data.frame(
  Metric = c(
    "Treatment Effect Estimate (Log)",
    "Placebo Standard Error (Log)",
    "Pre-Treatment Years",
    "Post-Treatment Years",
    "Number of Control Units",
    "Effective Control Units",
    "Effective Pre-Treatment Years",
    "Top Time Weight (2019)",
    "Second Time Weight (2018)"
  ),
  Value = c(
    round(sdid_summary_log$estimate, 4),
    round(sdid_se_log[1, 1], 4),
    T0_log,
    ncol(Y_log) - T0_log,
    N0_log + 1,
    round(sdid_summary_log$dimensions["N0.effective"], 2),
    round(sdid_summary_log$dimensions["T0.effective"], 2),
    paste0(round(sdid_summary_log$periods["2019", 1] * 100, 1), "%"),
    paste0(round(sdid_summary_log$periods["2018", 1] * 100, 1), "%")
  )
)

# === Step 11: Display and Export Summary (Log Prices) ===
datasummary_df(
  sdid_summary_table_log,
  title = "SDID Summary Table – Log Housing Prices (Makkah)",
  output = "markdown"
)

write.csv(sdid_summary_table_log, "sdid_summary_table_log_2.csv", row.names = FALSE)

# === Additional: Calculate p-value (Log Prices) ===
t_stat_log <- sdid_summary_log$estimate / sdid_se_log[1, 1]
p_value_log <- 2 * (1 - pnorm(abs(t_stat_log)))
p_value_log