# FINAL RESEARCH PROJECT
# Adèle Chasle
# RQ: Does a depreciation of the Indian rupee, linked to an oil price shock, reduce GDP growth?
#
# Main sample: 1996-2019 and 2022-2023. The pandemic years 2020-2021 are excluded
# from the main estimates and used as a robustness check (section 8.d).


# 0. Packages
# We load all the packages we need for this project

library(tidyverse)
library(readxl)
library(lubridate)
library(broom)
library(infer)
library(jtools)
library(car)
library(AER)

# We fix the random seed so the bootstrap and permutation results are the same every time
set.seed(2026)


# 0.a Working directory
# Set the working directory to the folder containing this script and the data:
# Session > Set Working Directory > To Source File Location
# (or use setwd("path/to/your/folder"))


# 0.b Load all data files
# We read each file and store it as an object in R
data_gdp_quarterly <- read.csv("Data GDP quarterly.csv")                    # FRED NAEXKP01INQ652S
data_global_oil    <- read.csv("Data global oil price (brent crude).csv")   # FRED POILBREUSDQ
data_US_real_gdp   <- read_xls("US real GDP growth.xls")                    # IMF World Economic Outlook
data_india_cpi     <- read.csv("india_cpi_index_monthly.csv")               # FRED INDCPIALLMINMEI (index 2015=100)
data_usdinr        <- read.csv("India Currency conversion.csv")             # FRED CCUSMA02INM618N


# 0.c Quick look at the data before we start cleaning
# head() shows us the first few rows of each dataset
# str() shows us the variable types
head(data_gdp_quarterly)
str(data_gdp_quarterly)

head(data_global_oil)
str(data_global_oil)

head(data_US_real_gdp)
str(data_US_real_gdp)

head(data_india_cpi)
str(data_india_cpi)

head(data_usdinr)
str(data_usdinr)


#1. Data preparation
# For each dataset we follow approximately the same steps:
# 1. Rename columns to something simpler
# 2. Convert the date column so R understands it as a date
# 3. If the data is monthly, we average it to get quarterly values
# 4. We compute log differences to get growth rates
# 5. We drop any missing values


# 1.a India quarterly real GDP

gdp_clean <- data_gdp_quarterly %>%
  # Rename the original column names to something easier to type
  rename(date = observation_date,
         gdp  = NAEXKP01INQ652S) %>%
  # Keep only the two columns we need
  select(date, gdp) %>%
  # Convert the date column from text to a real date object
  # Then extract the year and quarter number from it
  mutate(date    = as.Date(date),
         year    = year(date),
         quarter = quarter(date)) %>%
  # Remove any row where GDP is missing
  filter(!is.na(gdp)) %>%
  # Sort from earliest to most recent date
  # This is important because we will compute lags (previous quarter values)
  arrange(date) %>%
  # Take the log of GDP first
  # Then compute the growth rate as the difference in logs times 100
  # This gives us the percentage change compared to the previous quarter
  mutate(log_gdp    = log(gdp),
         gdp_growth = (log_gdp - lag(log_gdp)) * 100)


# 1.b USD/INR exchange rate
# This is our main explanatory variable
# A higher value means more rupees per dollar, so the rupee has lost value (depreciated)
# A lower value means fewer rupees per dollar, so the rupee has gained value (appreciated)

usdinr_clean <- data_usdinr %>%
  rename(date   = observation_date,
         usdinr = CCUSMA02INM618N) %>%
  select(date, usdinr) %>%
  # Convert date and extract year and quarter
  # Also make sure usdinr is stored as a number (not text)
  mutate(date    = as.Date(date),
         year    = year(date),
         quarter = quarter(date),
         usdinr  = as.numeric(usdinr)) %>%
  filter(!is.na(usdinr)) %>%
  # This data is monthly, so we average the three months in each quarter
  group_by(year, quarter) %>%
  summarise(usdinr_q = mean(usdinr, na.rm = TRUE), .groups = "drop") %>%
  arrange(year, quarter) %>%
  # Compute the quarterly log change, which we interpret as the percentage change
  # If delta_usdinr is positive, the rupee depreciated that quarter
  # If delta_usdinr is negative, the rupee appreciated that quarter
  mutate(delta_usdinr = (log(usdinr_q) - log(lag(usdinr_q))) * 100)


# 1.c Global Brent crude oil price
# This is our instrument in the IV strategy
# India imports about 85% of its oil, so world oil prices matter a lot for the Indian economy

oil_clean <- data_global_oil %>%
  rename(date  = observation_date,
         brent = POILBREUSDQ) %>%
  select(date, brent) %>%
  mutate(date    = as.Date(date),
         year    = year(date),
         quarter = quarter(date)) %>%
  filter(!is.na(brent)) %>%
  arrange(date) %>%
  # Compute the quarterly percentage change in oil price
  mutate(delta_brent = (log(brent) - log(lag(brent))) * 100)


# 1.d India CPI inflation
# CPI measures the general price level for consumers in India
# We include this to control for domestic inflation that is not caused by the exchange rate
# For example, a bad harvest can push food prices up without any change in the rupee
# Without this variable, our exchange rate coefficient might pick up those effects too
# Important: we use the CPI INDEX (INDCPIALLMINMEI), not the inflation rate,
# because we compute the quarterly change ourselves

cpi_clean <- data_india_cpi %>%
  rename(date = observation_date,
         cpi  = INDCPIALLMINMEI) %>%
  select(date, cpi) %>%
  mutate(date    = as.Date(date),
         year    = year(date),
         quarter = quarter(date),
         cpi     = as.numeric(cpi)) %>%
  # We remove rows where CPI is missing or zero or negative
  # because we need to take the log and log(0) is undefined
  filter(!is.na(cpi), cpi > 0) %>%
  # This data is also monthly so we average it to quarterly
  group_by(year, quarter) %>%
  summarise(cpi_q = mean(cpi, na.rm = TRUE), .groups = "drop") %>%
  arrange(year, quarter) %>%
  # Compute the quarterly log change, which is our inflation measure
  mutate(delta_cpi = (log(cpi_q) - log(lag(cpi_q))) * 100)


# 1.e US real GDP growth (annual data from IMF)
# We use US growth as a proxy for global demand conditions
# The US is one of India's most important trading partners
# When the US economy is doing well, it buys more Indian exports and IT services
# This boosts Indian GDP regardless of what happens to the exchange rate
# One limitation: this data is annual, so we assign the same value to all 4 quarters of a given year

US_gdp_clean <- data_US_real_gdp %>%
  # The first column contains country names, we rename it to "country"
  rename(country = 1) %>%
  # Keep only the row for the United States
  filter(!is.na(country), country == "United States") %>%
  # The data is in wide format (one column per year), we convert to long format
  pivot_longer(cols = -country, names_to = "year", values_to = "us_growth") %>%
  mutate(year      = as.integer(year),
         us_growth = as.numeric(us_growth)) %>%
  filter(!is.na(us_growth)) %>%
  select(year, us_growth)

# Now we replicate each annual value across the four quarters of that year
US_gdp_quarterly <- tibble(quarter = 1:4) %>%
  cross_join(US_gdp_clean)


#2. Build the dataset
# We add some extra variables to the GDP data (dummies and a lag)
# then we merge all our cleaned datasets together into one table

df <- gdp_clean %>%
  # We create seasonal dummy variables for Q2, Q3 and Q4
  # Q1 is the reference category (it is left out on purpose)
  mutate(Q2 = ifelse(quarter == 2, 1, 0),
         Q3 = ifelse(quarter == 3, 1, 0),
         Q4 = ifelse(quarter == 4, 1, 0)) %>%
  # We also create dummies for two big crisis periods
  # The 2008-2009 global financial crisis and the 2020 COVID lockdowns
  # (the COVID dummy is only used in the full-sample robustness check, section 8.d)
  mutate(crisis_0809 = ifelse(year %in% c(2008, 2009), 1, 0),
         covid_2020  = ifelse(year == 2020,             1, 0)) %>%
  # We add a lag of GDP growth (the value from the previous quarter)
  # This is because GDP growth tends to persist from one quarter to the next
  # Including it removes that persistence and isolates the effect of the exchange rate
  arrange(date) %>%
  mutate(gdp_growth_lag1 = lag(gdp_growth))


#3. Merge all datasets into one table

df <- df %>%
  left_join(usdinr_clean %>% select(year, quarter, usdinr_q, delta_usdinr),
            by = c("year", "quarter")) %>%
  left_join(oil_clean    %>% select(year, quarter, brent, delta_brent),
            by = c("year", "quarter")) %>%
  left_join(cpi_clean    %>% select(year, quarter, delta_cpi),
            by = c("year", "quarter")) %>%
  left_join(US_gdp_quarterly,
            by = c("year", "quarter")) %>%
  # We only keep quarters where all key variables are available
  filter(!is.na(gdp_growth),
         !is.na(delta_usdinr),
         !is.na(delta_brent),
         !is.na(delta_cpi),
         !is.na(gdp_growth_lag1),
         !is.na(us_growth))

# Quick check to see how many observations we have and what the data looks like
cat("Observations after merging:", nrow(df), "\n")
cat("Years covered:", min(df$year), "to", max(df$year), "\n")
summary(df %>% select(gdp_growth, delta_usdinr, delta_brent, delta_cpi, us_growth))

# We add a label for each quarter so we can colour crisis and COVID quarters differently in plots
df <- df %>%
  mutate(period = case_when(
    year %in% c(2020, 2021)  ~ "COVID 2020-21",
    year %in% c(2008, 2009)  ~ "Crisis 2008-09",
    TRUE                     ~ "Normal"
  ))

# Print the 5 worst and 5 best quarters so we understand the extreme values
cat("\nThe 5 lowest GDP growth quarters:\n")
df %>% arrange(gdp_growth) %>%
  select(year, quarter, gdp_growth, delta_usdinr, delta_brent) %>%
  head(5) %>% print()

cat("\nThe 5 highest GDP growth quarters:\n")
df %>% arrange(desc(gdp_growth)) %>%
  select(year, quarter, gdp_growth, delta_usdinr, delta_brent) %>%
  head(5) %>% print()


#4. Visualising the main relationship before running regression
# Here we make two plots on the FULL sample, so the COVID quarters are visible

# 4.a Scatter plot: GDP growth on the y axis, rupee change on the x axis
# Each dot is one quarter, coloured by period (either normal, crisis, covid)
# We fit a regression line only on the normal quarters

ggplot(df, aes(x = delta_usdinr, y = gdp_growth, colour = period)) +
  geom_point(alpha = 0.6, size = 2) +
  geom_smooth(data   = filter(df, period == "Normal"),
              method = "lm", colour = "black", se = TRUE) +
  scale_colour_manual(values = c("Normal"         = "steelblue",
                                 "Crisis 2008-09" = "orange",
                                 "COVID 2020-21"  = "red")) +
  labs(title  = "India GDP growth vs rupee depreciation",
       x      = "Change in USD/INR in percent",
       y      = "India real GDP growth in percent",
       colour = NULL) +
  theme_minimal()

ggsave("plot_prescreening.png", width = 7, height = 5)


# 4.b Time series plot: all key variables over time
# We reshape the data to long format so ggplot can facet by variable
# This lets us see visually whether big exchange rate moves coincide with recessions

df_long <- df %>%
  select(date, gdp_growth, delta_usdinr, delta_brent, delta_cpi) %>%
  pivot_longer(-date, names_to = "variable", values_to = "value") %>%
  mutate(variable = dplyr::recode(variable,
                                  "gdp_growth"   = "India GDP growth",
                                  "delta_usdinr" = "Change in USD/INR",
                                  "delta_brent"  = "Change in Brent price",
                                  "delta_cpi"    = "Change in India CPI"))

ggplot(df_long, aes(x = date, y = value)) +
  geom_line(colour = "steelblue") +
  facet_wrap(~variable, ncol = 1, scales = "free_y") +
  labs(title = "All key variables over time",
       x     = NULL,
       y     = NULL) +
  theme_minimal()

ggsave("plot_timeseries.png", width = 9, height = 8)


# 4.c Main sample
# We first compute the leads for the placebo test (section 8.b)
# They must be computed BEFORE removing 2020-21, otherwise 2019 Q4 would be matched with 2022 Q1

df <- df %>%
  arrange(year, quarter) %>%
  mutate(delta_usdinr_lead = lead(delta_usdinr),
         delta_brent_lead  = lead(delta_brent))

# We exclude the pandemic years 2020-2021 from the main sample
# The lockdown crash and rebound (about -25% then +20% GDP growth in two quarters)
# dominate the estimation and cannot be absorbed by a simple year dummy
# We keep the full sample in df_full for the robustness check (section 8.d)

df_full <- df
df <- df %>% filter(!year %in% c(2020, 2021))

cat("Main sample:", nrow(df), "quarters | Full sample:", nrow(df_full), "quarters\n")


#5. OLS assumption checks (main sample)
# We make all the checks that we studied in class and we also add a new check (5.c) to verify that our instrument is valid

# Note: in the actual IV regression (Step 6), delta_brent will be used as an instrument
# But here we include it as a control just to check the residuals and correlations

reg_check <- lm(gdp_growth ~ delta_usdinr + delta_brent + gdp_growth_lag1 +
                  delta_cpi + Q2 + Q3 + Q4 + crisis_0809,
                data = df)


# 5.a Multicollinearity
# We check whether our explanatory variables are too correlated with each other

cat("\n--- 5.a Multicollinearity ---\n")
cor(df %>% select(delta_usdinr, delta_brent, delta_cpi, us_growth, gdp_growth_lag1),
    use = "pairwise.complete.obs")


# 5.b Residuals vs fitted values
# We plot the residuals against the fitted values to check for heteroskedasticity
# Ideally the points should be randomly scattered around zero with no clear pattern

ggplot(data.frame(fitted = fitted(reg_check),
                  resid  = resid(reg_check)),
       aes(x = fitted, y = resid)) +
  geom_point(alpha = 0.4, colour = "steelblue") +
  geom_hline(yintercept = 0, colour = "red", linetype = "dashed") +
  labs(title = "Residuals vs fitted values",
       x     = "Fitted values",
       y     = "Residuals") +
  theme_minimal()

ggsave("plot_residuals.png", width = 7, height = 5)


# 5.c First stage regression: is our instrument relevant?
# Before we can use delta_brent as an instrument we need to check that it actually predicts delta_usdinr well (this is called instrument relevance)
# We do this by regressing delta_usdinr on delta_brent and all the controls
# The rule of thumb is that the F statistic on delta_brent should be above 10
# If it is below 10 the instrument is considered weak and we cannot trust the 2SLS results
# We also look at the SIGN: our mechanism predicts a positive coefficient (oil up -> rupee depreciates)

first_stage <- lm(delta_usdinr ~ delta_brent + gdp_growth_lag1 +
                    delta_cpi + Q2 + Q3 + Q4 + crisis_0809,
                  data = df)

cat("\n--- 5.c First stage regression ---\n")
summary(first_stage)

# We isolate the F statistic for delta_brent specifically
cat("\nPartial F test for delta_brent in the first stage:\n")
linearHypothesis(first_stage, "delta_brent = 0")

cat("\nN =", nrow(df), "| Years:", min(df$year), "to", max(df$year), "\n")


#6. 2SLS AND OLS FOR COMPARISON (main sample)

# We estimate three models:
# reg_ols    : simple OLS, kept to compare with IV results
# reg_iv     : our preferred model using 2SLS with delta_brent as instrument
# reg_iv_us  : same as reg_iv but with US growth added as an extra control

# OLS baseline with no instrument, just for comparison
reg_ols <- lm(gdp_growth ~ delta_usdinr + gdp_growth_lag1 +
                delta_cpi + Q2 + Q3 + Q4 + crisis_0809,
              data = df)

# 2SLS preferred model
# Here delta_usdinr is the endogenous variable and delta_brent is its instrument

reg_iv <- ivreg(
  gdp_growth ~ delta_usdinr + gdp_growth_lag1 +
    delta_cpi + Q2 + Q3 + Q4 + crisis_0809 |
    delta_brent + gdp_growth_lag1 +
    delta_cpi + Q2 + Q3 + Q4 + crisis_0809,
  data = df
)

# 2SLS with US growth added as an extra robustness check
reg_iv_us <- ivreg(
  gdp_growth ~ delta_usdinr + gdp_growth_lag1 +
    delta_cpi + us_growth + Q2 + Q3 + Q4 + crisis_0809 |
    delta_brent + gdp_growth_lag1 +
    delta_cpi + us_growth + Q2 + Q3 + Q4 + crisis_0809,
  data = df
)


# Print the results of our preferred model
# diagnostics = TRUE also gives us the Wu-Hausman test (see below)
cat("\n--- 6. 2SLS preferred model ---\n")
summary(reg_iv, diagnostics = TRUE)
# The Wu-Hausman test checks whether OLS is actually biased
# H0: delta_usdinr is exogenous (OLS would be fine)
# p < 0.05 means we reject H0: endogeneity is confirmed and 2SLS is necessary
# p > 0.05 means OLS and 2SLS give similar answers


#7. Simulation based inference (main sample)
# We use the bootstrap and permutation tests from the infer package
# We apply the Frisch-Waugh-Lovell theorem:
# Instead of bootstrapping the raw variables, we first partial out the controls from both GDP growth and the instrumented exchange rate
# Then we bootstrap the slope on those residuals
# Therefore, we have the same coefficient as 2SLS but with simulation-based inference.
# Limitation: the first stage is estimated once and kept fixed, so its uncertainty is not included

df_iv <- df %>%
  filter(!is.na(delta_cpi), !is.na(delta_brent), !is.na(delta_usdinr))

df_iv <- df_iv %>%
  mutate(
    # We get the instrumented (predicted) values of delta_usdinr
    delta_usdinr_hat = fitted(lm(delta_usdinr ~ delta_brent + gdp_growth_lag1 +
                                   delta_cpi + Q2 + Q3 + Q4 +
                                   crisis_0809, data = df_iv)),
    # We remove the effect of all controls from GDP growth
    # The residuals capture the part of GDP growth that controls cannot explain
    gdp_growth_resid       = resid(lm(gdp_growth ~ gdp_growth_lag1 +
                                        delta_cpi + Q2 + Q3 + Q4 +
                                        crisis_0809, data = df_iv)),
    # Finally, we remove the effect of all controls from the instrumented exchange rate
    delta_usdinr_hat_resid = resid(lm(delta_usdinr_hat ~ gdp_growth_lag1 +
                                        delta_cpi + Q2 + Q3 + Q4 +
                                        crisis_0809, data = df_iv))
  )

# 7.a Bootstrap
# We resample the data 1000 times and recompute the slope each time
# This gives us a distribution of plausible slope values
# We then use that distribution to compute a standard error and confidence interval

cat("\n--- 7.a Bootstrap inference ---\n")
bootstrap_distrib <- df_iv %>%
  specify(response = gdp_growth_resid, explanatory = delta_usdinr_hat_resid) %>%
  generate(reps = 1000, type = "bootstrap") %>%
  calculate(stat = "slope")

cat("Bootstrap standard error:  ", round(sd(bootstrap_distrib$stat), 5), "\n")
cat("Conventional standard error:", round(summary(reg_iv)$coefficients["delta_usdinr", "Std. Error"], 5), "\n")

cat("Bootstrap 95% confidence interval:\n")
print(quantile(bootstrap_distrib$stat, c(0.025, 0.975)))

# Plot the bootstrap distribution
# The red line shows our 2SLS estimate

ggplot(bootstrap_distrib, aes(x = stat)) +
  geom_histogram(bins = 30, fill = "steelblue", colour = "white") +
  geom_vline(xintercept = coef(reg_iv)["delta_usdinr"],
             colour = "red", linewidth = 1) +
  labs(title = "Bootstrap distribution of the rupee depreciation coefficient",
       x     = "Bootstrap slope",
       y     = "Count") +
  theme_minimal()

ggsave("plot_bootstrap.png", width = 7, height = 5)


# 7.b Permutation test
# We test the null hypothesis that rupee depreciation has no effect on GDP growth
# Under this null, the exchange rate and GDP growth are independent
# So if we randomly shuffle the exchange rate residuals, we should get slopes near zero
# We do this 1000 times and see how often we get a slope as extreme as ours
# If that is very rare, we reject the null hypothesis

cat("\n--- 7.b Permutation test ---\n")
null_distribution <- df_iv %>%
  specify(formula = gdp_growth_resid ~ delta_usdinr_hat_resid) %>%
  hypothesize(null = "independence") %>%
  generate(reps = 1000, type = "permute") %>%
  calculate(stat = "slope")

# We standardise the observed slope to make it comparable to the null distribution
observed_stat <- coef(reg_iv)["delta_usdinr"] / sd(bootstrap_distrib$stat)
null_distribution <- null_distribution %>%
  mutate(test_stat = stat / sd(bootstrap_distrib$stat))

# We compute the simulated p-value: how often does the null produce something as extreme?
p_value_sim <- mean(abs(null_distribution$test_stat) >= abs(observed_stat))

cat("Observed test statistic:", round(observed_stat, 3), "\n")
cat("Simulated p value:      ", p_value_sim, "\n")
cat("Conventional p value:   ", round(summary(reg_iv)$coefficients["delta_usdinr", "Pr(>|t|)"], 4), "\n")

# We plot the null distribution
# The red lines show our observed statistic
# Everything outside the red lines is the p-value area

ggplot(null_distribution, aes(x = test_stat)) +
  geom_histogram(bins = 30, fill = "grey70", colour = "white") +
  geom_vline(xintercept =  abs(observed_stat), colour = "red", linewidth = 1) +
  geom_vline(xintercept = -abs(observed_stat), colour = "red", linewidth = 1) +
  labs(title = "Null distribution from permutation test",
       x     = "Test statistic under the null",
       y     = "Count") +
  theme_minimal()

ggsave("plot_null_distribution.png", width = 7, height = 5)


#8. Robustness checks
# We check whether our main result holds when we change slightly the model.

# 8.a We put all three models side by side in one table
# If the coefficient on delta_usdinr changes a lot between OLS and 2SLS, the OLS estimate was likely biased

export_summs(reg_ols, reg_iv, reg_iv_us,
             robust      = "HC1",
             model.names = c("OLS baseline",
                             "2SLS preferred",
                             "2SLS with US growth"),
             coefs       = c("Change in USDINR"    = "delta_usdinr",
                             "Change in India CPI" = "delta_cpi",
                             "US GDP growth"       = "us_growth",
                             "Lagged GDP growth"   = "gdp_growth_lag1",
                             "Q2"                  = "Q2",
                             "Q3"                  = "Q3",
                             "Q4"                  = "Q4",
                             "Crisis 2008 09"      = "crisis_0809"),
             error_format = "({std.error})",
             statistics   = c("N"       = "nobs",
                              "R2"      = "r.squared",
                              "Adj R2"  = "adj.r.squared"))


# 8.b Placebo test using the future exchange rate instead of the current one
# (the leads were computed in section 4.c)
cat("\n--- 8.b Placebo test using the lead of delta_usdinr ---\n")

reg_placebo <- ivreg(
  gdp_growth ~ delta_usdinr_lead + gdp_growth_lag1 +
    delta_cpi + Q2 + Q3 + Q4 + crisis_0809 |
    delta_brent_lead + gdp_growth_lag1 +
    delta_cpi + Q2 + Q3 + Q4 + crisis_0809,
  data = df
)

tidy(reg_placebo, conf.int = TRUE, conf.level = 0.95) %>%
  filter(term == "delta_usdinr_lead") %>%
  print()


# 8.c Reduced form: effect of oil prices directly on GDP growth
# The 2SLS coefficient equals the reduced-form coefficient divided by the first-stage coefficient
cat("\n--- 8.c Reduced form ---\n")

reduced_form <- lm(gdp_growth ~ delta_brent + gdp_growth_lag1 + delta_cpi +
                     Q2 + Q3 + Q4 + crisis_0809, data = df)
summary(reduced_form)


# 8.d Full sample including 2020-2021, with a 2020 dummy
# This shows how much the pandemic quarters drive the estimate
cat("\n--- 8.d Full sample including 2020-21 ---\n")

reg_iv_full <- ivreg(
  gdp_growth ~ delta_usdinr + gdp_growth_lag1 +
    delta_cpi + Q2 + Q3 + Q4 + crisis_0809 + covid_2020 |
    delta_brent + gdp_growth_lag1 +
    delta_cpi + Q2 + Q3 + Q4 + crisis_0809 + covid_2020,
  data = df_full
)
summary(reg_iv_full, diagnostics = TRUE)


#9. Final summary
# We print a quick summary of the key results

cat("\n--- 9.a R squared across all models ---\n")
cat("OLS baseline:         R2 =", round(summary(reg_ols)$r.squared,   3), "\n")
cat("2SLS preferred:       R2 =", round(summary(reg_iv)$r.squared,    3), "\n")
cat("2SLS with US growth:  R2 =", round(summary(reg_iv_us)$r.squared, 3), "\n")

cat("\n--- 9.b First stage ---\n")
cat("Coefficient on delta_brent:", round(coef(first_stage)["delta_brent"], 4), "\n")
cat("Partial F on delta_brent:",
    round(linearHypothesis(first_stage, "delta_brent = 0")$F[2], 2), "\n")
cat("Rule of thumb: F above 10 means the instrument is strong\n")

cat("\n--- 9.c Wu-Hausman endogeneity test ---\n")
summary(reg_iv, diagnostics = TRUE)$diagnostics

# Print the main coefficient with its standard error, p-value and confidence interval
cat("\n--- 9.d Main result (main sample, excluding 2020-21) ---\n")
ct  <- summary(reg_iv)$coefficients
ci  <- confint(reg_iv, level = 0.95)
b1  <- ct["delta_usdinr", "Estimate"]
se1 <- ct["delta_usdinr", "Std. Error"]
pv  <- ct["delta_usdinr", "Pr(>|t|)"]
cat(sprintf("Beta1 = %.4f  SE = %.4f  p = %.4f\n", b1, se1, pv))
cat(sprintf("95 percent CI: [%.4f, %.4f]\n", ci["delta_usdinr", 1], ci["delta_usdinr", 2]))

cat("\n--- 9.e Numbers for the README ---\n")
cat("OLS coefficient:          ", round(coef(reg_ols)["delta_usdinr"], 3), "\n")
cat("Reduced form (Brent):     ", round(coef(reduced_form)["delta_brent"], 4), "\n")
cat("Bootstrap 95% CI:         ", round(quantile(bootstrap_distrib$stat, c(0.025, 0.975)), 3), "\n")
cat("Permutation p-value:      ", p_value_sim, "\n")
cat("Full sample 2SLS (incl. COVID):", round(coef(reg_iv_full)["delta_usdinr"], 3), "\n")
