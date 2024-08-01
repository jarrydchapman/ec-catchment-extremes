## Packages

# Required packages
req_packages = c("tidyverse", "RcppRoll")

# Install any required packages
inst_packages <- req_packages %in% installed.packages()
if(length(req_packages[!inst_packages]) > 0) install.packages(req_packages[!inst_packages])

# Load packages
lapply(req_packages, require, character.only=TRUE)


# Read Data
grid_id <- readRDS("~/Documents/grid_id.rds")
river_id <- readRDS("~/Documents/river_id.rds")
river_precip <- readRDS("~/Documents/river_precip.rds")


# Summary stats for daily and 3-day catchment rainfall
river_summary_day <- river_precip %>%
  group_by(date, river) %>%
  summarise(sum = sum(mm),
            max = max(mm),
            mean = mean(mm))

river_summary_3day <- river_summary_day %>%
  group_by(river) %>%
  summarise(rollsumtotal_3day = roll_sum(sum, n = 3, fill = NA, align = "right"),
            rollmeantotal_3day = roll_sum(mean, n = 3, fill = NA, align = "right")) %>%
  mutate(date = seq.Date(ymd("1940-01-01"), ymd("2022-12-31"), 1)) %>%
  relocate(date) %>%
  arrange(date, river)


river_summary_stats <- cbind(river_summary_day, river_summary_3day) %>%
  as_tibble() %>%
  select(-c(date...6, river...7))
names(river_summary_stats)[1:2] <- c("date", "river")

saveRDS(river_summary_stats, "river_summary_stats.rds")


# Quantiles for daily and 3-day catchment rainfall
river_quantile_day <- river_summary_day %>%
  group_by(river) %>%
  summarise(sum95 = quantile(sum, 0.95),
            sum975 = quantile(sum, 0.975),
            sum99 = quantile(sum, 0.99),
            max95 = quantile(max, 0.95),
            max975 = quantile(max, 0.975),
            max99 = quantile(max, 0.99),
            mean95 = quantile(mean, 0.95),
            mean975 = quantile(mean, 0.975),
            mean99 = quantile(mean, 0.99))

river_quantile_3day <- river_summary_3day %>%
  na.omit() %>%
  group_by(river) %>%
  summarise(sum95_3day = quantile(rollsumtotal_3day, 0.95),
            sum975_3day = quantile(rollsumtotal_3day, 0.975),
            sum99_3day = quantile(rollsumtotal_3day, 0.99),
            mean95_3day = quantile(rollmeantotal_3day, 0.95),
            mean975_3day = quantile(rollmeantotal_3day, 0.975),
            mean99_3day = quantile(rollmeantotal_3day, 0.99))

river_quantiles <- river_quantile_day %>%
  select(-river) %>%
  cbind(river_quantile_3day) %>%
  relocate(river)

saveRDS(river_quantiles, "river_quantiles.rds")

            
