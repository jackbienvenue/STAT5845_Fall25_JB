############################################################
# Applied Spatio-Temporal Statistics
# Mauna Loa CO2 Example
#
# File: co2_daily_mlo.csv
#  - No header; data begin at line 33 (skip first 32 lines)
#  - Columns: year, month, day, decimal_date, co2 (ppm)
#
#   DETERMINISTIC (each model fit and checked ONE BY ONE)
#     A) Linear trend
#     B) Quadratic trend
#     C) Trend + seasonal harmonics
#     D) Quadratic trend + seasonal harmonics 
#   RANDOM (model residuals of the chosen deterministic model)
############################################################

# --- Setup --------------------------------------------------------------------
library(tidyverse)
library(lubridate)
library(broom)      # augment(), tidy()
library(patchwork)  # combine plots

# --- Load and tidy the daily data --------------------------------------------
co2_daily <- read.csv(
  "../data/co2_daily_mlo.csv",
  header = FALSE, skip = 32,
  col.names = c("year","month","day","decimal_date","co2")
) %>%
  # Build a proper Date from (year, month, day). Suppress warnings from odd rows.
  mutate(date = suppressWarnings(make_date(year, month, day))) %>%
  # Keep only valid dates and valid CO2 values
  filter(!is.na(date), !is.na(co2)) %>%
  arrange(date)

# ======================================================================
# DETERMINISTIC — Step 1: Visualise structure (Trend / Seasonality)
# ======================================================================

# --- Plot 1: The full daily series -------------------------------------------
# The big picture: a decades-long upward trend with clear within-year oscillations.
p_full <- ggplot(co2_daily, aes(date, co2)) +
  geom_line(linewidth = 0.3, alpha = 0.8) +
  labs(
    title = "Mauna Loa CO2 (daily): Long-term rise with strong seasonality",
    x = "Date", y = "CO2 (ppm)"
  ) +
  theme_minimal(base_size = 11)

# --- Plot 2: A close-up window ------------------------------------------------
# Zooming into one recent year exposes the sawtooth seasonal cycle.
# Choose the last complete calendar year present in the data.
last_year <- year(max(co2_daily$date, na.rm = TRUE))
has_full_year <- all(seq.Date(
  from = as.Date(paste0(last_year, "-01-01")),
  to   = as.Date(paste0(last_year, "-12-31")),
  by   = "day") %in% co2_daily$date
)

zoom_year <- if (has_full_year) last_year else (last_year - 1)

co2_zoom <- co2_daily %>%
  filter(date >= as.Date(paste0(zoom_year, "-01-01")),
         date <= as.Date(paste0(zoom_year, "-12-31")))

p_zoom <- ggplot(co2_zoom, aes(date, co2)) +
  geom_line(linewidth = 0.4) +
  labs(
    title = paste0("Zoom: the seasonal cycle in ", zoom_year),
    x = "Date", y = "CO2 (ppm)"
  ) +
  theme_minimal(base_size = 11)

# --- Display side-by-side -----------------------------------------------------
p_full / p_zoom

# What we see:
# - Top panel: Over six decades of daily CO₂ readings at Mauna Loa.
#   * A clear long-term upward trajectory, reflecting global fossil fuel emissions.
#   * On top of that rise, a sawtooth seasonal pattern is visible even at this scale.
#
# - Bottom panel: Zoom into the most recent full year (2024).
#   * The seasonal cycle comes into focus: CO₂ rises through spring, peaks around May,
#     then declines as plants in the Northern Hemisphere draw down CO₂ in summer.
#   * By fall and winter, CO₂ rebounds as vegetation decays and emissions accumulate.
#   * This “breathing of the biosphere” repeats annually, superimposed on the long-term rise.
#
# Together, the panels highlight two intertwined signals:
#   1) A strong trend driven by human activity.
#   2) A natural seasonal oscillation linked to the Earth’s carbon cycle.




# --- Next step: isolate the within-year seasonal pattern ----------------------
# So far, we saw:
#   * Long-term rise over decades.
#   * Sawtooth oscillation within each year.
# Question: What does that seasonal cycle look like if we align all years together?

# Strategy:
#   1. Collapse daily data to monthly averages (to smooth out day-to-day noise).
#   2. Compute, for each month, the average CO₂ across all years.
#   3. Plot: thin lines for each year, bold line for the multi-year monthly mean.

co2_month <- co2_daily %>%
  transmute(
    ym = floor_date(date, "month"),
    co2 = co2
  ) %>%
  group_by(ym) %>%
  summarise(co2 = mean(co2, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    year = year(ym),
    month = month(ym, label = TRUE, abbr = TRUE)
  )

# Compute monthly mean across all years
co2_month_avg <- co2_month %>%
  group_by(month) %>%
  summarise(co2 = mean(co2, na.rm = TRUE), .groups = "drop")

# --- Plot: Thin lines = each year; Thick = multi-year monthly mean -----------
p_season <- ggplot(co2_month, aes(x = month, y = co2, group = year)) +
  geom_line(alpha = 0.4, linewidth = 0.4) +      
  
  # Make the summary layers NOT inherit group=year from the main mapping
  geom_line(
    data = co2_month_avg,
    mapping = aes(x = month, y = co2, group = 1),
    inherit.aes = FALSE,
    color = "black", linewidth = 1.2
  ) +
  geom_point(
    data = co2_month_avg,
    mapping = aes(x = month, y = co2),
    inherit.aes = FALSE,
    color = "black", size = 2
  ) +
  labs(
    title = "Within-year pattern: clear SEASONALITY",
    x = "Month", y = "CO₂ (ppm)",
    caption = "Thin = each year; Thick = multi-year monthly mean"
  ) +
  theme_minimal(base_size = 11)

p_season


# What we see:
# - Each thin line traces one year of monthly CO₂ averages.
# - Despite the long-term rise, the shape of the cycle is consistent:
#     * Low values around September–October,
#     * A rise through winter and spring,
#     * A peak around May–June before declining again.
# - The bold line averages across all years: the characteristic seasonal
#   "breathing" of the Earth’s biosphere.
#
# This is the same sawtooth we noticed earlier, now isolated and made explicit.



# ======================================================================
# DETERMINISTIC — Step 2: Move from visual patterns to statistical models
# ======================================================================

# QUESTION:
# Given what we see — a long-term upward trend + strong within-year seasonality —
# how should we build a deterministic model that captures these two components?
#

# --- Prepare data for modeling ------------------------------------------------
# We’ll use the monthly series for modeling simplicity.
co2_model <- co2_month %>%
  mutate(
    # Numeric time (in years) centered at 0 for numerical stability
    year_num = year(ym) + (month(ym) - 0.5)/12,
    t        = year_num - min(year_num),
    # Seasonal harmonics (period = 1 year)
    s1 = sin(2*pi*t), 
    c1 = cos(2*pi*t)
  )


# What these columns mean 
# - year_num:
#     * Transforms each monthly timestamp into a decimal year.
#     * Example: mid-January 1980 ≈ 1980.04, June 1980 ≈ 1980.46, December 1980 ≈ 1980.96.
#     * Placing each month at its midpoint avoids December being coded as the *next* year.
#
# - t:
#     * Time in years, but shifted so the very first observation = 0.
#     * This way, the timeline runs 0, 1, 2, … up to ~65 instead of 1958, 1959, 1960, …
#     * Centering at 0 prevents numerical instability when fitting polynomials (like t²).
#
# - s1 and c1:
#     * These are sine and cosine terms with period = 1 year.
#     * They are called "harmonic regressors."
#     * Why do we need both?
#         - sin(2πt) captures a wavy pattern (up–down cycle),
#         - cos(2πt) is the same wave shifted by 3 months,
#         - together, they can reproduce any repeating annual cycle, no matter where the peak falls.
#
# In short:
#   year_num = actual calendar time,
#   t        = relative time since the start,
#   s1, c1   = seasonal building blocks (1st harmonic).

# --- Helper: overlay fit + residual plot for any lm ---------------------------
plot_fit_and_resid <- function(fit, data, label){
  aug <- broom::augment(fit, data = data)
  
  p_fit <- ggplot(aug, aes(x = ym)) +
    geom_line(aes(y = co2), linewidth = 0.5, alpha = 0.65) +
    # map the constant label as the color value
    geom_line(aes(y = .fitted, color = label), linewidth = 0.9) +
    # name the scale with the same string value used above
    scale_color_manual(values = setNames("blue", label)) +
    labs(title = paste("Observed vs Fitted —", label),
         x = "Date", y = "CO2 (ppm)", color = NULL) +
    theme_minimal(base_size = 11)
  
  p_res <- ggplot(aug, aes(x = ym, y = .resid)) +
    geom_hline(yintercept = 0, linewidth = 0.4, alpha = 0.7) +
    geom_line(linewidth = 0.4) +
    labs(title = paste("Residuals over time —", label),
         x = "Date", y = "Residual (ppm)") +
    theme_minimal(base_size = 11)
  
  print(p_fit / p_res)
  invisible(aug)
}


# --- Example deterministic models --------------------------------------------
# (1) Linear trend only
m_lin <- lm(co2 ~ t, data = co2_model)
summary(m_lin)

# What we see:
# - Fitted line tracks the long-term increase only.
# - Residuals show strong within-year oscillations → seasonality not modeled.
aug_lin <- plot_fit_and_resid(m_lin, co2_model, "A) Linear trend")

# (2) Quadratic trend
m_quad <- lm(co2 ~ t + I(t^2), data = co2_model)
summary(m_quad)

# What we see:
# - Curvature improves long-term fit.
# - Residuals still show clear seasonal wiggles → need seasonal terms.
aug_quad <- plot_fit_and_resid(m_quad, co2_model, "B) Quadratic trend")

# (3) Linear trend + seasonal harmonics
m_season <- lm(co2 ~ t + s1 + c1, data = co2_model)
summary(m_season)

# What we see:
# - Annual cycle captured; long-term curvature still off.
# - Residuals drift slowly over decades → add quadratic.
aug_season <- plot_fit_and_resid(m_season, co2_model, "C) Linear + harmonics")

# (4) Quadratic trend + seasonal harmonics
m_quad_season <- lm(co2 ~ t + I(t^2) + s1 + c1, data = co2_model)
summary(m_quad_season)

# What we see:
# - Best overlay: long-term acceleration + seasonal wiggle both tracked.
# - Residuals look smallest and closest to “white noise” visually.
aug_quad_season <- plot_fit_and_resid(m_quad_season, co2_model, "D) Quadratic + harmonics")



# --- Quick model comparison table --------------------------------------------
# Focus on: AIC (↓ better), BIC (↓), adj.R² (↑), and residual SD (sigma ↓)
comp <- bind_rows(
  glance(m_lin)         %>% mutate(model = "A) Linear trend"),
  glance(m_quad)        %>% mutate(model = "B) Quadratic trend"),
  glance(m_season)      %>% mutate(model = "C) Linear + harmonics (s1,c1)"),
  glance(m_quad_season) %>% mutate(model = "D) Quadratic + harmonics (s1,c1)")
) %>%
  select(model, nobs, AIC, BIC, adj.r.squared, sigma)

print(comp)

# What we see:
# - Model A (Linear trend):
#     * Captures the overall rise but misses curvature and seasonality.
#     * High AIC/BIC, adj.R² ≈ 0.98, residuals still noisy (σ ≈ 3.7 ppm).
#
# - Model B (Quadratic trend):
#     * Adds acceleration in the long-term rise.
#     * Dramatic improvement: AIC drops from 3346 → 2781, σ shrinks to 2.3 ppm.
#     * Still no seasonality, so residuals would show the sawtooth cycle.
#
# - Model C (Linear + harmonics):
#     * Keeps trend linear but now explains annual oscillations.
#     * Better than Model A (σ ≈ 3.0 vs 3.7), but not as good as quadratic.
#     * Long-term curvature is still unaccounted for.
#
# - Model D (Quadratic + harmonics):
#     * Combines both improvements: curvature + annual cycle.
#     * Best fit by far: lowest AIC/BIC, adj.R² ≈ 0.999, residual σ ≈ 1 ppm.
#     * This is the deterministic specification that balances trend and seasonality.
#
# Takeaway:
# - To explain CO₂, we need BOTH long-term acceleration (quadratic) AND the repeating
#   annual cycle (harmonics). Leaving out either piece inflates residuals.


# ======================================================================
# DIAGNOSTICS — What did each deterministic model miss?
# ======================================================================

res_all <- bind_rows(
  augment(m_lin, data = co2_model)   %>% mutate(model = "A) Linear trend"),
  augment(m_quad, data = co2_model)  %>% mutate(model = "B) Quadratic trend"),
  augment(m_season, data = co2_model)%>% mutate(model = "C) Linear + harmonics"),
  augment(m_quad_season, data = co2_model) %>% mutate(model = "D) Quadratic + harmonics")
) %>%
  transmute(
    ym    = ym,                                    # use ym from augment data
    month = month(ym, label = TRUE, abbr = TRUE),  # safe: ym exists here
    model,
    resid = .resid
  )

# --- Residuals by Month (is any seasonality left?) -------------------
p_box <- ggplot(res_all, aes(x = month, y = resid)) +
  geom_hline(yintercept = 0, linewidth = 0.4, alpha = 0.6) +
  geom_boxplot(outlier.alpha = 0.5, width = 0.65) +
  facet_wrap(~ model, ncol = 2) +
  labs(title = "Residuals by month",
       x = "Month", y = "Residual (ppm)") +
  theme_minimal(base_size = 11)

p_box

# What we see (residuals by month):
# - A) & B): clear monthly pattern → seasonality not captured by trend-only models.
# - C): monthly pattern largely tamed, but a mild tilt remains (trend not flexible enough).
# - D): boxes are tight and centered near zero across months → seasonality and curvature
#      are both well handled; good deterministic specification.




# ======================================================================
# BEST DETERMINISTIC FIT — Show Model D clearly
# ======================================================================

# Fitted values from Model D (Quadratic + harmonics)
co2_fitD <- co2_model %>%
  mutate(fit_quad_season = predict(m_quad_season))

p_fitD <- ggplot(co2_fitD, aes(x = ym)) +
  geom_line(aes(y = co2), linewidth = 0.5, alpha = 0.65) +
  geom_line(aes(y = fit_quad_season, color = "Model D fit"), linewidth = 0.9) +
  scale_color_manual(values = c("Model D fit" = "blue")) +
  labs(title = "Observed vs Fitted — Model D (Quadratic + Harmonics)",
       x = "Date", y = "CO₂ (ppm)", color = NULL,
       subtitle = "Deterministic part captures long-term acceleration + annual cycle") +
  theme_minimal(base_size = 11)

p_fitD



# What we see:
# - The blue curve (Model D) tracks both the overall rise and the within-year wiggle.
# - Remaining differences = residuals → that’s our RANDOM part to model next.


# ======================================================================
# RANDOM PART — Diagnostics of residuals from Model D
# ======================================================================

# Extract residuals from the best deterministic model
resid_D <- augment(m_quad_season, data = co2_model)$.resid

# --- Histogram and boxplot ----------------------------------------------------
par(mfrow = c(1,2))
hist(resid_D, breaks = 40, col = "grey", border = "white",
     main = "Residuals: Histogram", xlab = "Residual (ppm)")
boxplot(resid_D, horizontal = F, col = "lightblue",
        main = "Residuals: Boxplot", xlab = "Residual (ppm)")

# What we see:
# - Histogram: The residuals are roughly bell-shaped, centered near zero.
#   This suggests they are approximately Normally distributed.
# - Boxplot: The box is tight around zero, no strong skewness, only a few
#   mild outliers. Good sign — no big departures from symmetry.


# --- Q-Q plot -----------------------------------------------------------------
qqnorm(resid_D, main = "Q–Q plot of residuals (Model D)")
qqline(resid_D, col = "red")

# What we see:
# - Points lie close to the straight line, which means residuals
#   follow a Normal distribution fairly well.
# - Some small deviations at the tails, but nothing dramatic.


# --- ACF and PACF -------------------------------------------------------------
par(mfrow = c(1,2))
acf(resid_D, main = "ACF of residuals (Model D)")
pacf(resid_D, main = "PACF of residuals (Model D)")

# What we see:
# - ACF: Residuals are *not* completely uncorrelated — we still see
#   clear spikes at seasonal lags. This means some temporal dependence
#   is left over even after the deterministic model.
# - PACF: Confirms the same — several lags are significant.
#
# Reminder: 
# - Ideally, residuals should behave like "white noise" = random scatter,
#   with no remaining structure.
# - Here, although the residuals are approximately Normal, they still show
#   correlation over time → the deterministic model explains trend and seasonality,
#   but not all the temporal dependence.


# -----------------------------------------------
# Difference once
# -----------------------------------------------
resid_D_diff1 <- diff(resid_D)

# Plot ACF/PACF of differenced residuals
par(mfrow = c(1, 2))
acf(resid_D_diff1, main = "ACF of differenced residuals (Model D, d=1)")
pacf(resid_D_diff1, main = "PACF of differenced residuals (Model D, d=1)")

acf(resid_D_diff1, main = "ACF of differenced residuals (Model D, d=1)", lag.max = 200)
pacf(resid_D_diff1, main = "PACF of differenced residuals (Model D, d=1)", lag.max = 200)


# install.packages("tseries")   # if not already installed
library(tseries)

# run the Augmented Dickey-Fuller test
adf.test(resid_D)

adf.test(resid_D_diff1)

#Null hypothesis (H₀): The series has a unit root (non-stationary).

#Alternative (H₁): The series is stationary.

#If p-value < 0.05, → reject H₀ → series is stationary.

#If p-value ≥ 0.05, → fail to reject H₀ → series is nonstationary.


library(forecast)   # for ARIMA fitting and diagnostics

# --- Step 1: Fit a simple AR(1) model to the residuals ------------------------
# AR(1): residual_t = phi * residual_{t-1} + noise
fit_ar1 <- arima(resid_D, order = c(1,0,0))

fit_ar1

plot(fit_ar1$residuals)
acf(fit_ar1$residuals, main = "ACF: AR(1) residuals")
pacf(fit_ar1$residuals, main = "PACF: AR(1) residuals")

fit_ar <- arima(resid_D, order = c(1,1,1))

fit_ar

plot(fit_ar$residuals)
acf(fit_ar$residuals, main = "ACF: ARIMA(1,1) residuals")
pacf(fit_ar$residuals, main = "PACF: ARIMA(1,1) residuals")

# --- Step 3: Try automatic ARIMA selection -----------------------------------
fit_auto <- auto.arima(resid_D)

fit_auto

plot(fit_auto$residuals)
acf(fit_auto$residuals, main = "ACF: AUTO ARIMA residuals")
pacf(fit_auto$residuals, main = "PACF: AUTO ARIMA residuals")





############################################################
# FORECASTING — Combine deterministic + stochastic parts
############################################################

# --- Step 1: Choose how far ahead to forecast -------------------------------
h <- 12   # 12 months ahead (you can change this)

# --- Step 2: Deterministic component forecast (μ̂_t+h) -----------------------
# Build a data frame of future time points
last_t <- max(co2_model$t)
future_t <- data.frame(
  t  = seq(last_t + 1/12, last_t + h/12, by = 1/12)
) %>%
  mutate(
    s1 = sin(2*pi*t),
    c1 = cos(2*pi*t)
  )

# Predict the deterministic part using Model D (quadratic + harmonics)
mu_hat_future <- predict(m_quad_season, newdata = future_t)

# --- Step 3: Stochastic component forecast (Ŷ_t+h|t) ------------------------
# Use the ARIMA model fitted to residuals
# Extract forecasts and standard errors
arma_fc <- predict(fit_auto, n.ahead = h)
y_hat_future <- arma_fc$pred
arma_se <- arma_fc$se  # std. error of stochastic forecast

# --- Step 4: Combine both parts ---------------------------------------------
# Total forecast = deterministic + stochastic components
x_hat_future <- mu_hat_future + y_hat_future

# --- Step 5: Compute prediction intervals -----------------------------------
# Forecast error variance comes from ARIMA + residual variance
# If model assumed Gaussian innovations:
z_alpha2 <- qnorm(0.975)  # 95% interval
upper <- x_hat_future + z_alpha2 * arma_se
lower <- x_hat_future - z_alpha2 * arma_se

# --- Step 6: Visualize ------------------------------------------------------
future_dates <- seq(max(co2_month$ym) + months(1),
                    by = "month", length.out = h)

forecast_df <- data.frame(
  date = future_dates,
  forecast = x_hat_future,
  lower = lower,
  upper = upper
)

# Plot: Observed + Forecast + Interval
ggplot() +
  geom_line(data = co2_month, aes(x = ym, y = co2), color = "grey40") +
  geom_line(data = forecast_df, aes(x = date, y = forecast), color = "blue") +
  geom_ribbon(data = forecast_df,
              aes(x = date, ymin = lower, ymax = upper),
              alpha = 0.2, fill = "blue") +
  labs(
    title = "Manual CO₂ Forecast: Point and 95% Prediction Interval",
    x = "Date", y = "CO₂ (ppm)"
  ) +
  theme_minimal(base_size = 12)

############################################################
# Zoomed-in Forecast Visualization
############################################################

# --- Step 7: Zoom window -----------------------------------------------------
# Choose how many past months to show (e.g., last 3 years = 36 months)
n_past <- 36  

zoom_start <- max(co2_month$ym) - months(n_past)

# Prepare observed + forecast data for one combined plot
plot_data <- co2_month %>%
  filter(ym >= zoom_start) %>%
  rename(date = ym) %>%
  mutate(type = "Observed")

forecast_df_plot <- forecast_df %>%
  mutate(type = "Forecast")

plot_combined <- bind_rows(plot_data, forecast_df_plot)

# --- Step 8: Plot zoomed section ---------------------------------------------

ggplot() +
  # Prediction interval as shaded ribbon
  geom_ribbon(
    data = forecast_df_plot,
    aes(x = date, ymin = lower, ymax = upper),
    fill = "skyblue", alpha = 0.3
  ) +
  # Forecast line
  geom_line(
    data = forecast_df_plot,
    aes(x = date, y = forecast, color = "Forecast"),
    linewidth = 1
  ) +
  # Observed data
  geom_line(
    data = plot_data,
    aes(x = date, y = co2, color = "Observed"),
    linewidth = 0.6
  ) +
  scale_color_manual(
    values = c("Observed" = "black", "Forecast" = "blue")
  ) +
  labs(
    title = "CO2 Forecast — Zoomed View",
    subtitle = paste0("Last ", n_past, " months of data + ", h, "-month forecast"),
    x = "Date", y = "CO2 (ppm)",
    color = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "top")









############################################################
# FORECAST EVALUATION — Leave out 2024 and forecast it
############################################################

# --- Step 1: Split data ------------------------------------------------------
cutoff_date <- as.Date("2024-01-01")

train_data <- co2_month %>%
  filter(ym < cutoff_date)

test_data <- co2_month %>%
  filter(ym >= cutoff_date)

# --- Step 2: Refit deterministic model (Model D) on training data ------------
train_model <- train_data %>%
  mutate(
    year_num = year(ym) + (month(ym) - 0.5)/12,
    t        = year_num - min(year_num),
    s1       = sin(2*pi*t),
    c1       = cos(2*pi*t)
  )

m_train <- lm(co2 ~ t + I(t^2) + s1 + c1, data = train_model)

# --- Step 3: Compute residuals and fit ARIMA model ----------------------------
resid_train <- augment(m_train, data = train_model)$.resid

fit_auto_train <- auto.arima(resid_train)

# --- Step 4: Build forecast horizon into 2024 --------------------------------
h_test <- nrow(test_data)

last_t_train <- max(train_model$t)

future_t <- data.frame(
  t = seq(last_t_train + 1/12, last_t_train + h_test/12, by = 1/12)
) %>%
  mutate(
    s1 = sin(2*pi*t),
    c1 = cos(2*pi*t)
  )

# Deterministic forecast
mu_hat_future <- predict(m_train, newdata = future_t)

# Stochastic (ARIMA) forecast
arma_fc <- predict(fit_auto_train, n.ahead = h_test)
y_hat_future <- arma_fc$pred
arma_se <- arma_fc$se

# Total forecast
x_hat_future <- mu_hat_future + y_hat_future
upper <- x_hat_future + qnorm(0.975) * arma_se
lower <- x_hat_future - qnorm(0.975) * arma_se

# --- Step 5: Create forecast DataFrame ---------------------------------------
future_dates <- seq(max(train_data$ym) + months(1),
                    by = "month", length.out = h_test)

forecast_eval <- data.frame(
  date = future_dates,
  forecast = x_hat_future,
  lower = lower,
  upper = upper
)

# --- Step 6: Merge observed test data ----------------------------------------
compare_df <- test_data %>%
  rename(date = ym, actual = co2) %>%
  left_join(forecast_eval, by = "date")

# --- Step 7: Compute evaluation metrics --------------------------------------
rmse <- sqrt(mean((compare_df$actual - compare_df$forecast)^2, na.rm = TRUE))
mae  <- mean(abs(compare_df$actual - compare_df$forecast), na.rm = TRUE)

cat("Forecast evaluation for 2024:\n")
cat("RMSE =", round(rmse, 3), "ppm\n")
cat("MAE  =", round(mae, 3), "ppm\n")

############################################################
# Zoomed evaluation plot — focus on recent window
############################################################

# How many months of history before the cutoff to show?
n_past_train <- 24  # last 24 months before 2024

train_recent <- train_data %>%
  filter(ym >= cutoff_date %m-% months(n_past_train))

# Nice y-limits with small padding
ymin <- min(c(train_recent$co2, compare_df$actual, forecast_eval$lower), na.rm = TRUE)
ymax <- max(c(train_recent$co2, compare_df$actual, forecast_eval$upper), na.rm = TRUE)
ypad <- 0.02 * (ymax - ymin)

ggplot() +
  # Prediction interval ribbon (forecast period)
  geom_ribbon(
    data = forecast_eval,
    aes(x = date, ymin = lower, ymax = upper),
    fill = "skyblue", alpha = 0.3
  ) +
  # Forecast line
  geom_line(
    data = forecast_eval,
    aes(x = date, y = forecast, color = "Forecast"),
    linewidth = 1
  ) +
  # Observed in the holdout year
  geom_line(
    data = compare_df,
    aes(x = date, y = actual, color = "Observed"),
    linewidth = 0.8
  ) +
  # Recent training history
  geom_line(
    data = train_recent,
    aes(x = ym, y = co2, color = "History"),
    linewidth = 0.6
  ) +
  # Mark the forecast start. Use Date directly to avoid the warning.
  geom_vline(xintercept = cutoff_date, linetype = "dashed", color = "red") +
  annotate(
    "text",
    x = cutoff_date + 15,               # ~2 weeks into 2024
    y = ymin + ypad,
    label = "Forecast starts \u2192",
    hjust = 0, color = "red", size = 3
  ) +
  scale_color_manual(
    values = c("History" = "grey40", "Observed" = "black", "Forecast" = "blue")
  ) +
  labs(
    title = "CO₂ Forecast Evaluation — Holdout 2024 (Zoomed)",
    subtitle = paste0("RMSE = ", round(rmse, 2), " ppm   |   MAE = ", round(mae, 2), " ppm"),
    x = "Date", y = "CO₂ (ppm)", color = NULL
  ) +
  coord_cartesian(
    xlim = c(cutoff_date %m-% months(n_past_train), max(forecast_eval$date)),
    ylim = c(ymin - ypad, ymax + ypad)
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "top")







############################################################
# Scatter plot: Observed vs Predicted (Holdout 2024)
############################################################

# Create scatter data
scatter_df <- compare_df %>%
  filter(!is.na(forecast), !is.na(actual))

# 1:1 reference line range
lim_min <- min(scatter_df$actual, scatter_df$forecast, na.rm = TRUE)
lim_max <- max(scatter_df$actual, scatter_df$forecast, na.rm = TRUE)

# Compute correlation
corr_val <- cor(scatter_df$actual, scatter_df$forecast)

# Plot
ggplot(scatter_df, aes(x = actual, y = forecast)) +
  geom_point(color = "blue", alpha = 0.6, size = 2) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  coord_equal(xlim = c(lim_min, lim_max), ylim = c(lim_min, lim_max)) +
  labs(
    title = "CO₂ Forecast Evaluation — Predicted vs Observed (2024)",
    subtitle = paste0(
      "1:1 line in red   |   RMSE = ", round(rmse, 2),
      " ppm   |   MAE = ", round(mae, 2),
      " ppm   |   r = ", round(corr_val, 3)
    ),
    x = "Observed CO₂ (ppm)",
    y = "Predicted CO₂ (ppm)"
  ) +
  theme_minimal(base_size = 12)

