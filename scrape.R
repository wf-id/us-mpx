if(!dir.exists("data")) dir.create("data")

slugify_date <- function(x){
  x <- stringi::stri_replace_all_regex(x,"[^\\P{P}-]","")
  x <- gsub(x, pattern = " ", replacement = "-")
  x
}
ping_time <- slugify_date(Sys.time())

download.file(
  #"https://www.cdc.gov/wcms/vizdata/poxvirus/monkeypox/data/USmap_counts.csv",
  "https://www.cdc.gov/wcms/vizdata/poxvirus/monkeypox/data/USmap_counts/exported_files/USmap_counts.csv",
sprintf("data/%s.csv", ping_time),
quiet = TRUE,
cacheOK = FALSE
)


# overall reporting incidence -----------------------------------------------------------------

tmp <- tempfile()

download.file(
  "https://www.cdc.gov/wcms/vizdata/poxvirus/monkeypox/data/mpx_count_by_date.csv",
  tmp,
  quiet = TRUE,
  cacheOK = FALSE
)


o <- data.table::fread(tmp)

names(o) <- c("EpidateDT", "CaseCNT", "RollingNBR","CumulativeCNT","UpdateDT")

o$EpidateDT <- as.Date(o$EpidateDT, "%m/%d/%Y")

o$CaseCNT <- as.numeric(o$CaseCNT)

o$ReportDT <- Sys.Date()

data.table::fwrite(o, here::here("data", "oa-incidence",paste0(ping_time,".csv") ))

o$CaseRollNBR <- data.table::frollmean(x = o$CaseCNT, 7)

data.table::fwrite(o, here::here("output", "us-incidence.csv"))


# jynneos-distribution ------------------------------------------------------------------------

ses <- rvest::session("https://aspr.hhs.gov/SNS/Pages/JYNNEOS-Distribution.aspx")

dat_jynneos <- rvest::html_table(ses)[[1]] 

data.table::fwrite(dat_jynneos, here::here("data", "jynneos", paste0(ping_time,".csv")))

