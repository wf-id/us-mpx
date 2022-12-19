# Purpose: Create Longitudinal Data from CDC Scrapes

library(stringr)
library(dplyr)
library(purrr)
library(data.table)

h <- here::here

get_data <- function(type = "json"){

county_details <- fs::dir_ls(h("data"), glob = sprintf("*.%s", type))

pull_time <- lubridate::as_datetime(tools::file_path_sans_ext(basename(county_details)))

pull_date <- lubridate::date(pull_time)

dat_information <- data.frame(
  county_details= unname(county_details),
  pull_time,
  pull_date
) %>%
  group_by(pull_date) %>%
  filter(pull_time==max(pull_time))
if(type == "json"){
  dat_raw <- map(dat_information$county_details, jsonlite::read_json,simplifyVector = TRUE)
  
  names(dat_raw) <- dat_information[["pull_date"]]
  
  dat_dat <- map(dat_raw, "data", .id = "date")
  
  dat_dat <- rbindlist(dat_dat, idcol = "date", fill = TRUE)
  
} else {
  dat_raw <-  map(dat_information$county_details, data.table::fread)
  
  names(dat_raw) <- dat_information[["pull_date"]]
  
  dat_dat <- dat_raw
  
  dat_dat <- rbindlist(dat_dat, idcol = "date", fill = TRUE)
  dat_dat$State <- dat_dat$Location
  
}



setDT(dat_dat)

dat_dat[,Cases := as.numeric(Cases)]

dat_dat[,State := ifelse(is.na(State),Location, State )]



setnames(x = dat_dat, old = c("date","State", "Cases"), 
         new = c("DateDT", "StateDSC", "CasesCumulativeCNT"))

if("Range" %in% names(dat_dat)){
  dat_dat$Range = NULL
}

dat_dat$Location = NULL

dat_dat$`Case Range` = NULL

return(dat_dat)
}

json_dat <- get_data(type = "json")
csv_dat <-  get_data(type = "csv")

dat_dat <- rbind(csv_dat, json_dat, fill = TRUE)

setorderv(dat_dat, c("DateDT","StateDSC"))

dat_dat <- dat_dat[,tail(.SD, 1), by = c("DateDT","StateDSC")]

dat_dat[order(DateDT),CasesDailyNBR := CasesCumulativeCNT - dplyr::lag(CasesCumulativeCNT, 1), by = "StateDSC"]

data.table::fwrite(dat_dat, h("output", "mpx.csv"))


# pull jynneos --------------------------------------------------------------------------------

jynneous_data <- lapply(list.files(h("data", "jynneos"), full.names = TRUE), function(x) {
  pull_date <- lubridate::as_datetime(str_remove(basename(x), "\\.csv"))
  pull_date <- lubridate::date(pull_date)

jynneous_data <- data.table::fread(x)

names(jynneous_data)[1] <- "Jurisdiction"

jynneous_data <- data.table::melt(jynneous_data, id.vars = "Jurisdiction")

jynneous_data[,value := gsub(pattern = "-", replacement = "0", value)]

jynneous_data[ ,value := as.numeric(gsub(pattern = ",", replacement = "", value))]

jyn_out <- jynneous_data[grepl("(T|t)otal",variable)][,Description := fcase(
  stringr::str_detect(string = variable, "Allocat"), "AllocatedCNT",
  stringr::str_detect(string = variable, "Shipped"), "ShippedCNT",
  stringr::str_detect(string = variable, "Requested"), "RequestedCNT"
)][]

jyn_out <- data.table::dcast(jyn_out, Jurisdiction ~ Description, value.var = "value")

jyn_out <- jyn_out[,FilledPCT := ShippedCNT/RequestedCNT]
jyn_out$DateDT <- pull_date
jyn_out
})

jynneous_data <- rbindlist(jynneous_data, fill = TRUE)

jynneous_data <- jynneous_data[,tail(.SD, 1), by = c("Jurisdiction","DateDT")]

data.table::fwrite(jynneous_data, here::here("output", "jynneos.csv"))
