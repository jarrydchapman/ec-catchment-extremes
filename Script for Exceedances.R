## Packages

# Required packages
req_packages = c("tidyverse")

# Install any required packages
inst_packages <- req_packages %in% installed.packages()
if(length(req_packages[!inst_packages]) > 0) install.packages(req_packages[!inst_packages])

# Load packages
lapply(req_packages, require, character.only=TRUE)


# Read Data
river_summary_stats <- readRDS("~/Documents/river_summary_stats.rds")
river_quantiles <- readRDS("~/Documents/river_quantiles.rds")

# Exceedance Dataframe
exc_list <- list()
for(i in 1:nrow(river_quantiles)) {
thresh <- river_quantiles[i,]

sum_stats <- river_summary_stats %>%
  filter(river == thresh$river[1])

sum_stats[1:2,6:7] <- 0

exc_list[[i]] <- tibble("date" = seq.Date(ymd("1940-01-01"), ymd("2022-12-31"), 1),
            "sum95_exc" = ifelse(sum_stats$sum > thresh$sum95, 
                                 sum_stats$sum - thresh$sum95,
                                 0),
            "sum975_exc" = ifelse(sum_stats$sum > thresh$sum975, 
                                  sum_stats$sum - thresh$sum975,
                                  0),
            "sum99_exc" = ifelse(sum_stats$sum > thresh$sum99, 
                                  sum_stats$sum - thresh$sum99,
                                  0),
            "max95_exc" = ifelse(sum_stats$max > thresh$max95, 
                                  sum_stats$max - thresh$max95,
                                  0),
            "max975_exc" = ifelse(sum_stats$max > thresh$max975, 
                                 sum_stats$max - thresh$max975,
                                 0),
            "max99_exc" = ifelse(sum_stats$max > thresh$max99, 
                                 sum_stats$max - thresh$max99,
                                 0),
            "mean95_exc" = ifelse(sum_stats$mean > thresh$mean95, 
                                 sum_stats$mean - thresh$mean95,
                                 0),
            "mean975_exc" = ifelse(sum_stats$mean > thresh$mean975, 
                                  sum_stats$mean - thresh$mean975,
                                  0),
            "mean99_exc" = ifelse(sum_stats$mean > thresh$mean99, 
                                  sum_stats$mean - thresh$mean99,
                                  0),
            "sum95_3day_exc" = ifelse(sum_stats$rollsumtotal_3day > thresh$sum95_3day, 
                                 sum_stats$rollsumtotal_3day - thresh$sum95_3day,
                                 0),
            "sum975_3day_exc" = ifelse(sum_stats$rollsumtotal_3day > thresh$sum975_3day, 
                                      sum_stats$rollsumtotal_3day - thresh$sum975_3day,
                                      0),
            "sum99_3day_exc" = ifelse(sum_stats$rollsumtotal_3day > thresh$sum99_3day, 
                                      sum_stats$rollsumtotal_3day - thresh$sum99_3day,
                                      0),
            "mean95_3day_exc" = ifelse(sum_stats$rollmeantotal_3day > thresh$mean95_3day, 
                                      sum_stats$rollmeantotal_3day - thresh$mean95_3day,
                                      0),
            "mean975_3day_exc" = ifelse(sum_stats$rollmeantotal_3day > thresh$mean975_3day, 
                                       sum_stats$rollmeantotal_3day - thresh$mean975_3day,
                                       0),
            "mean99_3day_exc" = ifelse(sum_stats$rollmeantotal_3day > thresh$mean99_3day, 
                                       sum_stats$rollmeantotal_3day - thresh$mean99_3day,
                                       0))
}

names(exc_list) <- river_quantiles$river

exc <- exc_list %>%
  bind_rows() %>%
  mutate(river = rep(river_quantiles$river, each = nrow(sum_stats))) %>%
  relocate(river, .after = date)

saveRDS(exc, "exc.rds")

  


