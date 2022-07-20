# Purpose: Create Longitudinal Data from CDC Scrapes

library(stringr)
library(dplyr)
library(purrr)

h <- here::here

county_details <- fs::dir_ls(h("data"), glob = "*.json")

pull_time <- lubridate::as_datetime(str_remove(basename(county_details), "\\.json"))

pull_date <- lubridate::date(pull_time)

dat_information <- data.frame(
  county_details= unname(county_details),
  pull_time,
  pull_date
) %>%
  group_by(pull_date) %>%
  filter(pull_time==max(pull_time))

dat_raw <- map(dat_information$county_details, jsonlite::read_json)

names(dat_raw) <- dat_information[["pull_date"]]

dat_dat <- map(dat_raw, "data", idcols = "date")

my_cleaner <- function(x){
  
  keep_name <- names(x)
  
  raw <- lapply(x[[1]], bind_rows)
  
}

long <- do.call(rbind, lapply(dat_dat[[1]], bind_rows))


data.table::fwrite(long, h("output", "mpx.csv"))

