library(fpp3)
pbs_raw <- readr::read_csv(
  "https://workshop.nectric.com.au/rmedicine2024/data/pbs.csv"
)
PBS <- pbs_raw |>
  mutate(Month = yearmonth(Month)) |>
  as_tsibble(index = Month, key = c(Concession, Type, ATC1, ATC2))

PBS |>
  # as_tibble() |>
  filter(ATC2 == "A10") |>
  group_by(Concession) |>
  summarise(Scripts = sum(Scripts))

PBS |>
  # as_tibble() |>
  # index_by(Year = year(Month)) |>
  group_by(ATC1) |>
  summarise(Cost = sum(Cost)) |>
  autoplot(Cost)

as_tsibble(USAccDeaths) |>
  autoplot(value)

as_tsibble(USAccDeaths) |>
  gg_season(value)

as_tsibble(USAccDeaths) |>
  gg_subseries(value)

as_tsibble(USAccDeaths) |>
  ACF(value) |>
  autoplot()

pbs_scripts <- PBS |>
  summarise(Scripts = sum(Scripts))
pbs_scripts |>
  autoplot(Scripts)

pbs_scripts |>
  model(
    # NAIVE(Scripts),
    SNAIVE(Scripts),
    # NAIVE(Scripts ~ drift()),
    SNAIVE(Scripts ~ drift()),
    TSLM(Scripts ~ trend() + season())
  ) |>
  forecast(h = "10 years") |>
  # mutate(median(Scripts), hilo(Scripts, 95))
  autoplot(pbs_scripts, level = NULL)


library(fpp3)
pbs_a10 <- PBS |>
  filter(ATC2 == "A10") |>
  summarise(Scripts = sum(Scripts), Cost = sum(Cost))

pbs_a10 |>
  autoplot(Scripts)

# pbs_scripts |>
#   model(
#     TSLM(Scripts ~ trend() + season())
#   ) |>
#   outliers()

pbs_a10 |>
  model(
    TSLM(Scripts ~ trend() + season() + Cost),
    # SNAIVE(Scripts ~ drift())
  ) |>
  forecast(h = "10 years") |>
  autoplot(pbs_a10)

pbs_a10


pbs_a10 |>
  model(
    # TSLM(Scripts ~ trend() + season()),
    # ETS(Scripts ~ error("A") + trend("A") + season("A"))
    ETS(Scripts)
  ) |>
  forecast(h = "10 years") |>
  autoplot(pbs_a10)

pbs_a10 |>
  model(
    ETS(Scripts)
  ) |>
  components() |>
  autoplot()


pbs_a10 |>
  autoplot(Scripts)

pbs_a10 |>
  autoplot(log(Scripts))

fit <- pbs_a10 |>
  model(
    ETS(Scripts),
    ARIMA(log(Scripts))
  )

fit |>
  forecast(h = "10 years") |>
  autoplot(pbs_a10, level = 80, alpha = 0.5)

as_tsibble(USAccDeaths) |>
  autoplot(value)


library(fpp3)
pbs_scripts <- PBS |>
  summarise(Scripts = sum(Scripts))
fit <- pbs_scripts |>
  model(
    ETS(Scripts),
    ARIMA(log(Scripts))
  )
augment(fit)

accuracy(fit)

fc <- pbs_scripts |>
  # Keep some data for evaluating forecasts
  filter(Month < yearmonth("2006 Jan")) |>
  model(
    ETS(Scripts),
    ARIMA(log(Scripts))
  ) |>
  forecast(h = "2 years")

fc
accuracy(fc, pbs_scripts)

fc_cv <- pbs_scripts |>
  # Keep some data for evaluating forecasts
  filter(Month < yearmonth("2006 Jan")) |>
  # Cross-validate the remaining data
  stretch_tsibble(.step = 24, .init = 48) |>
  model(
    ETS(Scripts),
    ARIMA(log(Scripts))
  ) |>
  forecast(h = "2 years")

fc_cv

accuracy(fc_cv, pbs_scripts)


pbs_scripts |>
  autoplot(Scripts)
fit <- pbs_scripts |>
  model(TSLM(Scripts ~ trend()))
augment(fit) |>
  autoplot(.innov)

pbs_scripts |>
  gg_season(Scripts)
augment(fit) |>
  gg_season(.innov)


fit <- pbs_a10 |>
  model(
    ETS(Scripts),
    ARIMA(log(Scripts))
  )
augment(fit) |>
  autoplot(.innov)
augment(fit) |>
  gg_season(.innov)

fit |>
  select(`ETS(Scripts)`) |>
  gg_tsresiduals()
