# Purpose: Create Longitudinal Data from CDC Scrapes

library(stringr)
library(dplyr)
library(purrr)
library(data.table)

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

dat_raw <- map(dat_information$county_details, jsonlite::read_json,simplifyVector = TRUE)

names(dat_raw) <- dat_information[["pull_date"]]

dat_dat <- map(dat_raw, "data", .id = "date")

dat_dat <- rbindlist(dat_dat, idcol = "date")

setDT(dat_dat)

dat_dat[,Cases := as.numeric(Cases)]

dat_dat[order(date),CasesDailyNBR := Cases - dplyr::lag(Cases, 1), by = "State"]

setnames(x = dat_dat, old = c("date","State", "Cases"), new = c("DateDT", "StateDSC", "CasesCumulativeCNT"))

dat_dat[,Range := NULL]

data.table::fwrite(dat_dat, h("output", "mpx.csv"))

