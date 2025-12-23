
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Compare Baseline (2007–2011) vs Summer 2012 (May–Aug) ###################
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# --- Load libraries ---
library(readr)
library(readxl)
library(dplyr)
library(ggplot2)

# --- Set paths ---
data_dir <- "../Data_Source/test_ExportsAsPriceResponse_20251215"  # folder with EXP_YYYY.csv and stocks.xlsx
stocks_path <- file.path(data_dir, "complexo-soja-stocks.xlsx")

# Months of interest
month_numbers <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
month_labels  <- c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# (1) Read all export CSV files (2007–2012)  ###################################
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
exp_files <- list.files(
  data_dir,
  pattern = "^EXP_20(07|08|09|10|11|12).*\\.csv$",
  full.names = TRUE
)

exports_all <- data.frame()

for (file in exp_files) {
  # Read file
  df <- read_delim(file, delim = ";", show_col_types = FALSE)
  
  # Standardize column names: lowercase, replace dots with underscores
  names(df) <- gsub("\\.", "_", tolower(names(df)))
  
  # Pick columns by simple patterns (first match)
  y_idx  <- grep("^co_ano$|^ano$", names(df))
  m_idx  <- grep("^co_mes$|^mes$", names(df))
  n_idx  <- grep("^co_ncm$|^ncm$", names(df))
  kg_idx <- grep("^kg_liquido$|^peso_liquido$|^kg$", names(df))
  fob_idx <- grep("^vl_fob$|^valor_fob$", names(df))
  
  # Skip if required columns missing
  if (length(y_idx) == 0 || length(m_idx) == 0 || length(n_idx) == 0 || length(kg_idx) == 0) {
    message("Skipping file (missing needed columns): ", basename(file))
    next
  }
  
  df2 <- df %>%
    transmute(
      year        = as.integer(df[[ y_idx[1] ]]),
      month       = as.integer(df[[ m_idx[1] ]]),
      ncm         = as.character(df[[ n_idx[1] ]]),
      kg_liq      = suppressWarnings(as.numeric(df[[ kg_idx[1] ]])),
      vl_fob_usd  = if (length(fob_idx) > 0) suppressWarnings(as.numeric(df[[ fob_idx[1] ]])) else NA_real_
    ) %>%
    filter(!is.na(year), !is.na(month)) %>%
    filter(grepl("^1201", ncm))  # HS 1201 only
  
  exports_all <- bind_rows(exports_all, df2)
}

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# (2) Aggregate monthly totals (kg -> metric tons)  ###################################
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
exports_monthly <- exports_all %>%
  group_by(year, month) %>%
  summarise(exports_tons = sum(kg_liq, na.rm = TRUE) / 1000, .groups = "drop")

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# (3) Baseline averages (2007–2011) for May–Aug  ###################################
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
baseline_exports <- exports_monthly %>%
  filter(year >= 2007 & year <= 2011, month %in% month_numbers) %>%
  group_by(month) %>%
  summarise(baseline_tons = mean(exports_tons, na.rm = TRUE), .groups = "drop") %>%
  mutate(month_label = factor(month_labels[match(month, month_numbers)], levels = month_labels))

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# (4) Summer 2012 exports (May–Aug)  ###################################
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
exports_2012 <- exports_monthly %>%
  filter(year == 2012, month %in% month_numbers) %>%
  transmute(
    month,
    exports_tons_2012 = exports_tons,
    month_label = factor(month_labels[match(month, month_numbers)], levels = month_labels)
  )

# Combine baseline vs 2012
exports_comp <- baseline_exports %>%
  left_join(exports_2012, by = c("month", "month_label")) %>%
  mutate(delta_tons = exports_tons_2012 - baseline_tons)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# (5) Stocks: read, tidy, baseline, 2012, and simple within-year changes   ###################################
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
stocks_raw <- read_excel(stocks_path)
names(stocks_raw) <- gsub("\\.", "_", tolower(names(stocks_raw)))

# Expect columns: "data" (date) and "soja" (inventory). Adjust if needed.
stocks_clean <- stocks_raw %>%
  mutate(
    date  = as.Date(data),
    year  = as.integer(format(date, "%Y")),
    month = as.integer(format(date, "%m")),
    soja  = suppressWarnings(as.numeric(soja))  # MAKE SURE numeric
  ) %>%
  filter(!is.na(year), !is.na(month), !is.na(soja))

baseline_stocks <- stocks_clean %>%
  filter(year >= 2007 & year <= 2011, month %in% month_numbers) %>%
  group_by(month) %>%
  summarise(baseline_stocks = mean(soja, na.rm = TRUE), .groups = "drop") %>%
  mutate(month_label = factor(month_labels[match(month, month_numbers)], levels = month_labels))

stocks_2012 <- stocks_clean %>%
  filter(year == 2012, month %in% month_numbers) %>%
  transmute(
    month,
    stocks_2012 = soja,  # already numeric from stocks_clean
    month_label = factor(month_labels[match(month, month_numbers)], levels = month_labels)
  )

stocks_comp <- baseline_stocks %>%
  left_join(stocks_2012, by = c("month", "month_label")) %>%
  mutate(delta_stocks = stocks_2012 - baseline_stocks)
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# (6) PRICES ###################################
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# create one csv to export
df_stocks_exports <- stocks_comp %>% left_join(exports_comp, by = c("month", "month_label"))

df_prices <- read.csv(paste0(data_dir, "/prices.csv"))

# Convert date to Date type and extract year/month
df_prices <- df_prices %>%
  mutate(date = as.Date(date),
         year = year(date),
         month = month(date))

# Months and years of interest
years_baseline <- 2007:2011
year_target <- 2012

# Filter for Brazil, months of interest, and years 2007–2012
df_prices_filtered <- df_prices %>%
  filter(country == "Brazil",
         month %in% month_numbers,
         year %in% c(years_baseline, year_target))

# Compute baseline average (2007–2011) and 2012 average
df_p_baseline_avg <- df_prices_filtered %>%
  filter(year %in% years_baseline) %>%
  group_by(month) %>% 
  summarise(baseline_avg_price = mean(price, na.rm = TRUE), .groups = "drop")

df_p_avg_2012 <- df_prices_filtered %>%
  filter(year == year_target) %>%
  group_by(month) %>% 
  summarise(avg_price_2012 = mean(price, na.rm = TRUE), .groups = "drop")

# Combine into one data frame
df_p_result <- left_join(df_p_baseline_avg, df_p_avg_2012)

# Print result
print(df_p_result)

df_stocks_exports_prices <- left_join(df_stocks_exports, df_p_result)
df_s_e_p <- df_stocks_exports_prices %>% 
  mutate(
    stocks_base_lag = baseline_stocks - lag(baseline_stocks),
    stocks_2012_lag = stocks_2012 - lag(stocks_2012),
    exports_base_lag = baseline_tons - lag(baseline_tons),
    exports_2012_lag = exports_tons_2012 - lag(exports_tons_2012),
    price_base_lag = baseline_avg_price - lag(baseline_avg_price),
    price_2012_lag = avg_price_2012 - lag(avg_price_2012),
    delta_price = avg_price_2012 - baseline_avg_price
  ) %>% 
  select(
    month, month_label, 
    baseline_stocks, stocks_2012, stocks_base_lag, stocks_2012_lag, delta_stocks,
    baseline_tons, exports_tons_2012, exports_base_lag, exports_2012_lag, delta_tons,
    baseline_avg_price, avg_price_2012, price_base_lag, price_2012_lag, delta_price
  ) %>% 
  rename(
    stocks_baseline = baseline_stocks,
    stocks_delta = delta_stocks,
    exports_tons_baseline = baseline_tons,
    exports_delta = delta_tons,
    price_baseline = baseline_avg_price,
    price_2012 = avg_price_2012,
    price_delta = delta_price
  )

write.csv(file = paste0(data_dir, "/df_prices_stocks_exports_lags.csv"), x = df_s_e_p, row.names = F)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# (7) Plots (kept simple)  ###################################
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

library(tidyr)

# EXPORTS #
exports_long <- exports_comp %>%
  select(month_label, baseline_tons, exports_tons_2012) %>%
  pivot_longer(cols = c(baseline_tons, exports_tons_2012),
               names_to = "series", values_to = "value")

ggplot(exports_long, aes(x = month_label, y = value, fill = series)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  scale_fill_manual(values = c("baseline_tons" = "gray70", "exports_tons_2012" = "darkgreen"),
                    labels = c("Baseline (2007–2011)", "2012"), name = NULL) +
  labs(title = "Brazil Soybean (HS 1201) Exports: 2012 vs Baseline",
       subtitle = "Monthly totals, May–August",
       x = NULL, y = "Exports (metric tons)")

# STOCKS #
stocks_long <- stocks_comp %>%
  select(month_label, baseline_stocks, stocks_2012) %>%
  pivot_longer(cols = c(baseline_stocks, stocks_2012),
               names_to = "series", values_to = "value")

ggplot(stocks_long, aes(x = month_label, y = value, fill = series)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  scale_fill_manual(values = c("baseline_stocks" = "gray70", "stocks_2012" = "darkgreen"),
                    labels = c("Baseline (2007–2011)", "2012"), name = NULL) +
  labs(title = "Brazil Soybean (HS 1201) Stocks: 2012 vs Baseline",
       subtitle = "Monthly totals, May–August",
       x = NULL, y = "Stocks (metric tons)")

# PRICES #
prices_long <- df_p_result %>%
  select(month, baseline_avg_price, avg_price_2012) %>%
  pivot_longer(cols = c(baseline_avg_price, avg_price_2012),
               names_to = "series", values_to = "value")

ggplot(prices_long, aes(x = month, y = value, fill = series)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  scale_fill_manual(values = c("baseline_avg_price" = "gray70", "avg_price_2012" = "darkgreen"),
                    labels = c("Baseline (2007–2011)", "2012"), name = NULL) +
  labs(title = "Brazil Soybean (HS 1201) Prices: 2012 vs Baseline",
       subtitle = "Monthly totals, May–August",
       x = NULL, y = "Prices (kg/USD")

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# (8) Print tables  ###################################
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

cat("\n=== Exports Comparison (2012 vs Baseline) ===\n")
print(exports_comp %>% select(month_label, baseline_tons, exports_tons_2012, delta_tons))

cat("\n=== Stocks Comparison (2012 vs Baseline) ===\n")
print(stocks_comp %>% select(month_label, baseline_stocks, stocks_2012, delta_stocks))

cat("\n=== Stocks Month-to-Month Changes (2012) ===\n")
print
