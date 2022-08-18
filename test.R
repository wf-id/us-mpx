library(tidyverse)
library(data.table)

all_cases <- rbindlist(lapply(fs::dir_ls(path = here::here("data", "oa-incidence")),
                 fread))
all_cases <- all_cases[order(EpidateDT)][,cum_confirm := cumsum(CaseCNT), by = "EpidateDT"]


z<-all_cases %>% 
  rename(reference_date = EpidateDT, report_date = ReportDT, confirm = CaseCNT) %>% 
  group_by(reference_date) |>
  mutate(cum_confirm = cummax(confirm)) |>
  ungroup() |>
  mutate(confirm = ifelse(!is.na(reference_date), cum_confirm, confirm)) |>
  dplyr::select(-cum_confirm)
library(epinowcast)
max_delay <- 4
complete_df <- z |>
  enw_complete_dates(max_delay = max_delay)

enw_df <- complete_df |>
  enw_preprocess_data(max_delay = max_delay)

model <- enw_model(threads = TRUE)

fit_opts <- enw_fit_opts(
  chains = 2, parallel_chains = 2, threads_per_chain = 2,
  iter_sampling = 1000, iter_warmup = 1000, adapt_delta = 0.9,
  show_messages = TRUE, refresh = 50, pp = TRUE
)

model <- enw_model(threads = TRUE)

simple_nowcast <- epinowcast(
  obs = enw_obs(family = "poisson", data = enw_df),
  data = enw_df, model = model, fit = fit_opts,
)
simple_nowcast

simple_nowcast |>
  summary(probs = c(0.05, 0.95)) |>
  dplyr::select(
    reference_date, report_date, delay, confirm, mean, median, sd, mad
  ) |>
  tail(n = 14)

simple_nowcast |>
  plot() +
  labs(x = "Onset date", y = "Reported cases by onset date")

simple_nowcast |>
  summary(type = "fit", variables = c("refp_mean", "refp_sd")) |>
  dplyr::select(variable, mean, median, sd, mad)
