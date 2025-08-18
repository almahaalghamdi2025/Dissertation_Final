# ==============================================================================
# Title:    Analysis for Dissertation – "Between Demolition & Development:
#           The Impact of Slum Demolitions on Housing Prices 
#           in the Makkah Region"
# Author:   Almaha Alghamdi
# Date:     August 2025
# Purpose:  This script contains the analysis conducted for the dissertation.
#           It examines the effects of slum demolitions on housing prices in 
#           the Makkah region. 
#           This is the script for the Difference-in-Differences (DID).
# ==============================================================================

# === Set Working Directory ===
setwd("C:/Users/Lenovo/Desktop/Thesis/Topic 5 - Demolitions Impact on Housing Prices")

# === Load Dataset for DiD Analysis ===
clean_data <- read.csv("Data for DiD.csv")

# === Create Log Price Variable ===
clean_data <- clean_data %>%
  mutate(
    log_price_per_sqm = log1p(price_per_sqm)
  )

# === Step 1: Add Treatment and Post Dummies ===
did_data <- clean_data %>%
  mutate(
    treated = ifelse(region == "Makkah", 1, 0),
    post = ifelse(year >= 2021, 1, 0),
    treated_post = treated * post
  )

# === Step 2: Filter for Comparable Regions and Years ===
parallel_data <- did_data %>%
  filter(region %in% c("Makkah", "Madinah", "Jazan", "Qassim", "Asir")) %>%
  filter(year >= 2015, year <= 2022)

# === Step 3: Add Annual Salary Variable ===
parallel_data$annual_salary <- parallel_data$monthly_avg_salary * 12
parallel_data$monthly_avg_salary <- NULL

# === Step 4: Add Normalized Employment ===
parallel_data <- parallel_data %>%
  mutate(
    normalized_employment = total_employment / total_population
  )

# === Step 5: Add Tourism-Religious Interaction Dummy ===
parallel_data <- parallel_data %>%
  mutate(
    tourism_religious_interaction = ifelse(region %in% c("Makkah", "Madinah"), 1, 0)
  )

# === Step 6: Matching on Pre-Treatment Predictors ===
match_data <- parallel_data %>%
  filter(year >= 2017, year < 2021)

match_model <- matchit(
  treated ~  total_population + transaction_volume + population_growth +
    normalized_employment + households + expenditure + total_tourists +
    affordability_ratio + affordable_price + domestic_tourists + num_inbound_tourists,
  data = match_data,
  method = "nearest"
)

matched_data <- match.data(match_model)
matched_regions <- unique(matched_data$region)

# === Step 7: Filter to Matched Regions Only ===
matched_full_data <- parallel_data %>%
  filter(region %in% matched_regions, year >= 2017, year <= 2022)

# === Step 8: Estimate DiD Model (Raw Prices) ===
did_model_matched <- feols(
  price_per_sqm ~ treated * post + normalized_employment + 
    expenditure + population_growth + households + 
    affordability_ratio + total_tourists + tourism_religious_interaction,
  data = matched_full_data
)

summary(did_model_matched)

# === Step 9: Visualize Price Trends (Matched Sample) ===
matched_trend <- matched_full_data %>%
  group_by(year, treated) %>%
  summarise(mean_price = mean(price_per_sqm, na.rm = TRUE), .groups = "drop")

# === Step 10: Generate Summary Table (Raw Prices) ===
sjPlot::tab_model(did_model_matched, title = "DiD Regression Results")

# === Step 11: Estimate DiD Model (Log Prices) ===
did_model_matched_log <- feols(
  log_price_per_sqm ~ treated * post + normalized_employment + 
    expenditure + population_growth + households + affordability_ratio +
    total_tourists + tourism_religious_interaction,
  data = matched_full_data
)

# === Step 12: Summary of Log-Price DiD Model ===
summary(did_model_matched_log)

# === Step 13: Export Results Side-by-Side (Raw vs Log Prices) ===
sjPlot::tab_model(
  did_model_matched, 
  did_model_matched_log,
  title = "DiD Regression Results: True Prices vs Log Prices",
  dv.labels = c("Price per Sqm", "Log Price per Sqm")
)

sjPlot::tab_model(
  did_model_matched, 
  did_model_matched_log,
  title = "DiD Regression Results: True Prices vs Log Prices",
  dv.labels = c("Price per Sqm", "Log Price per Sqm"),
  file = "did_results_table.html"
)

# === Save R Environment ===
save.image("Difference_in_Difference Model 2.RData")

# ==============================================================================
# === Visualizations: Raw Price Trends and DiD Interpretation ===
# ==============================================================================

# === Step 1: Aggregate Annual Mean Price by Group ===
matched_trend <- matched_full_data %>%
  group_by(year, treated) %>%
  summarise(mean_price = mean(price_per_sqm, na.rm = TRUE), .groups = "drop")

# === Step 2: Extract Values for DiD Estimation (δ) ===
treated_pre <- matched_trend %>% filter(year == 2020, treated == 1) %>% pull(mean_price)
treated_post <- matched_trend %>% filter(year == 2022, treated == 1) %>% pull(mean_price)
control_pre <- matched_trend %>% filter(year == 2020, treated == 0) %>% pull(mean_price)
control_post <- matched_trend %>% filter(year == 2022, treated == 0) %>% pull(mean_price)

cf_treated_post <- treated_pre + (control_post - control_pre)
delta <- treated_post - cf_treated_post

# === Step 3: Prepare Labels for Plotting ===
matched_trend$Group <- ifelse(matched_trend$treated == 1, "Treated", "Control")

# === Step 4: Plot Average Price Trends ===
ggplot(matched_trend, aes(x = year, y = mean_price, color = Group)) +
  geom_line(aes(linetype = Group), size = 1.4) +
  
  # Vertical treatment line
  geom_vline(xintercept = 2021, linetype = "dashed", color = "black") +
  
  # Dashed counterfactual trend
  geom_segment(aes(x = 2020, xend = 2022, y = treated_pre, yend = cf_treated_post),
               color = "black", linetype = "dashed", linewidth = 1.2, inherit.aes = FALSE) +
  
  # Arrow for delta
  geom_segment(aes(x = 2022.15, xend = 2022.15, y = cf_treated_post, yend = treated_post),
               arrow = arrow(length = unit(0.25, "cm")), color = "black", inherit.aes = FALSE) +
  
  # δ annotation
  annotate("text", x = 2022.2, y = (cf_treated_post + treated_post) / 2,
           label = paste0("δ = ", format(round(delta, 2), big.mark = ","), " SAR"),
           hjust = 0, size = 5) +
  
  # Color and line style
  scale_color_manual(values = c("Treated" = "darkgreen", "Control" = "grey")) +
  scale_linetype_manual(values = c("Treated" = "solid", "Control" = "dashed")) +
  
  # Axes and labels
  labs(
    title = "DiD Coefficient Estimates",
    subtitle = "Counterfactual & Estimated Treatment Effect (δ)",
    x = "Year",
    y = "Average Price per sqm (SAR/m²)"
  ) +
  scale_y_continuous(labels = function(x) paste0(comma(x), " SAR/m²")) +
  
  coord_cartesian(clip = "off") +
  
  # Theme adjustments
  theme_minimal(base_size = 15) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom",
    legend.title = element_blank(),
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 16, hjust = 0.5),
    plot.margin = margin(20, 40, 20, 20)
  )

# ==============================================================================
# === Visualization: Regional Comparison (Makkah vs Other Regions) ===
# ==============================================================================

# === Step 1: Define Region Groups ===
controls <- c("Madinah", "Jazan", "Qassim", "Asir")
treated <- "Makkah"

# === Step 2: Classify Regions into Display Groups ===
region_trend <- clean_data %>%
  filter(year >= 2017, year <= 2022) %>%
  mutate(
    LegendGroup = case_when(
      region == treated ~ "Treated Makkah",
      region %in% controls ~ "Control Regions",
      TRUE ~ "Other Regions"
    )
  ) %>%
  group_by(region, LegendGroup, year) %>%
  summarise(mean_price = mean(price_per_sqm, na.rm = TRUE), .groups = "drop")

# === Step 3: Extract Makkah's Price for Annotation Position ===
mak_price_2021 <- region_trend %>%
  filter(region == "Makkah", year == 2021) %>%
  pull(mean_price)

# === Step 4: Plot Regional Price Trends ===
ggplot(region_trend, aes(x = year, y = mean_price, group = region,
                         color = LegendGroup, linetype = LegendGroup)) +
  geom_line(size = 1) +
  
  # Red intervention line
  geom_vline(xintercept = 2021, linetype = "dashed", color = "red", size = 1) +
  annotate("text", x = 2021.1, y = mak_price_2021 + 50,
           label = "Intervention", hjust = 0, size = 3.5, color = "red") +
  
  # Axis labels and title
  labs(
    title = "Regional Housing Price Trends with Controls",
    x = "Year",
    y = "Average Price per Sqm (SAR)",
    color = "Region Type",
    linetype = "Region Type"
  ) +
  
  # Thousand separator for y-axis
  scale_y_continuous(labels = function(x) paste0(comma(x), " SAR")) +
  
  # Manual colors and linetypes
  scale_color_manual(values = c(
    "Other Regions" = "grey",
    "Control Regions" = "black",
    "Treated Makkah" = "darkgreen"
  )) +
  scale_linetype_manual(values = c(
    "Other Regions" = "dashed",
    "Control Regions" = "dashed",
    "Treated Makkah" = "solid"
  )) +
  
  theme_minimal(base_size = 11) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    legend.position = "bottom",
    legend.title = element_blank(),
    plot.margin = margin(20, 40, 20, 20)
  )

# ==============================================================================
# === Visualization: Model-Based Predicted Price Trends (DiD Estimate) ===
# ==============================================================================

# === Step 1: Build Predictions from the Fitted DiD Model ===
X  <- model.matrix(formula(did_model_matched), data = matched_full_data)
b  <- coef(did_model_matched)

# === Step 2: Compute Fitted and Counterfactual Values ===
fitted_actual <- as.numeric(X %*% b)

X_cf <- X
ix   <- with(matched_full_data, treated == 1 & post == 1)
X_cf[ix, colnames(X_cf) == "treated:post"] <- 0
fitted_cf <- as.numeric(X_cf %*% b)

# === Step 3: Add Predictions to Dataset ===
plot_data <- matched_full_data %>%
  mutate(
    y_hat       = fitted_actual,
    y_hat_cf    = fitted_cf
  )

# === Step 4: Calculate Model-Adjusted Trends ===
trend <- plot_data %>%
  group_by(year, treated) %>%
  summarise(mean_price = mean(y_hat), .groups = "drop") %>%
  mutate(Group = ifelse(treated == 1, "Treated", "Control"))

# === Step 5: Extract Values for DiD Visualization ===
treated_pre     <- trend %>% filter(year == 2020, treated == 1) %>% pull(mean_price)
treated_post    <- trend %>% filter(year == 2022, treated == 1) %>% pull(mean_price)
cf_treated_post <- plot_data %>%
  filter(year == 2022, treated == 1) %>%
  summarise(cf = mean(y_hat_cf)) %>% pull(cf)

# === Step 6: Calculate DiD Effect (δ) ===
delta <- treated_post - cf_treated_post
beta  <- coef(did_model_matched)["treated:post"]  # should match 'delta'

# === Step 7: Plot Model-Based Counterfactual and Treatment Effect ===
ggplot(trend, aes(x = year, y = mean_price, color = Group)) +
  geom_line(aes(linetype = Group), linewidth = 1.4) +
  geom_vline(xintercept = 2021, linetype = "dashed", color = "black") +
  
  # Counterfactual dashed line
  geom_segment(aes(x = 2020, xend = 2022, y = treated_pre, yend = cf_treated_post),
               color = "black", linetype = "dashed", linewidth = 1.2, inherit.aes = FALSE) +
  
  # Arrow showing δ
  geom_segment(aes(x = 2022.15, xend = 2022.15, y = cf_treated_post, yend = treated_post),
               arrow = arrow(length = unit(0.25, "cm")), color = "black", inherit.aes = FALSE) +
  
  # Annotated δ value
  annotate("text", x = 2022.2, y = (cf_treated_post + treated_post) / 2,
           label = paste0("δ = ", format(round(delta, 2), big.mark = ","), " SAR"),
           hjust = 0, size = 5) +
  
  scale_color_manual(values = c("Treated" = "darkgreen", "Control" = "grey")) +
  scale_linetype_manual(values = c("Treated" = "solid", "Control" = "dashed")) +
  
  labs(
    title = "DiD Coefficient Estimates",
    subtitle = "Counterfactual & Estimated Treatment Effect (δ)",
    x = "Year",
    y = "Average Price per sqm (SAR/m²)"
  ) +
  
  scale_y_continuous(labels = function(x) paste0(comma(x), " SAR/m²")) +
  coord_cartesian(clip = "off") +
  
  theme_minimal(base_size = 15) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom",
    legend.title = element_blank(),
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 16, hjust = 0.5),
    plot.margin = margin(20, 40, 20, 20)
  )

# === Step 8: Quick Console Check for δ ===
delta
coef(did_model_matched)["treated:post"]



