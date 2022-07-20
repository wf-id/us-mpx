if(!dir.exists("data")) dir.create("data")

slugify_date <- function(x){
  x <- stringi::stri_replace_all_regex(x,"[^\\P{P}-]","")
  x <- gsub(x, pattern = " ", replacement = "-")
  x
}
ping_time <- slugify_date(Sys.time())

download.file(
  "https://www.cdc.gov/poxvirus/monkeypox/response/modules/MX-response-case-count-US.json",
sprintf("data/%s.json", ping_time),
quiet = TRUE,
cacheOK = FALSE
)