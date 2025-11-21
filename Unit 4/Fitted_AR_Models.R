
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, 
               tsibble, fable,
               feasts, tsibbledata,
               fable.prophet,
               patchwork,
               lubridate, 
               rio,
               ggplot2,
               kableExtra
)

set.seed(123)
n_rep <- 1000
alpha1 <- 0.75
alpha0 <- 50

dat_ts <- tibble(w = rnorm(n_rep)) |>
  mutate(
    index = 1:n(),
    x = purrr::accumulate2(
      lag(w), w, 
      \(acc, nxt, w) alpha1 * acc + w,
      .init = 0)[-1]) |>
  tsibble::as_tsibble(index = index)

dat_ts |> 
  autoplot(.vars = x) +
  labs(
    x = "Time",
    y = "Simulated Time Series",
    title = "Simulated Values from an AR(1) Process"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5)
  )


# Fit the AR(1) model
fit_ar <- dat_ts |>
  model(AR(x ~ order(1)))
tidy(fit_ar)

set.seed(123)
n_rep <- 1000
alpha1 <- 0.75
sigma_sqr <- 5
alpha0 <- 50

dat_ts <- tibble(w = rnorm(n = n_rep, sd = sqrt(sigma_sqr))) |>
  mutate(
    index = 1:n(),
    x = purrr::accumulate2(
      lag(w), w, 
      \(acc, nxt, w) alpha1 * acc + w,
      .init = 0)[-1]) |>
  mutate(x = x + alpha0) |> 
  tsibble::as_tsibble(index = index)

dat_ts |> 
  autoplot(.vars = x) +
  labs(
    x = "Time",
    y = "Simulated Time Series",
    title = "Simulated Values from an AR(1) Process"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5)
  )

fit_ar1 <- dat_ts |> 
  model(AR(x ~ order(1)))
tidy(fit_ar1)


# Small Group Activity
temps_ts <- rio::import("https://byuistats.github.io/timeseries/data/global_temparature.csv") |>
  as_tsibble(index = year)

temps_ts |> autoplot(.vars = change) +
  labs(
    x = "Year",
    y = "Temperature Change (Celsius)",
    title = paste0("Change in Mean Annual Global Temperature (", min(temps_ts$year), "-", max(temps_ts$year), ")")
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5)
  )

pacf(temps_ts$change)

temp_fit <-temps_ts |> 
  model(AR(change ~ order(1:9))) # runs it for order 1 to 9
tidy(temp_fit)

temp_fit3 <-temps_ts |> 
  model(AR(change ~ order(3))) 
tidy(temp_fit3)

temp_fit6 <-temps_ts |> 
  model(AR(change ~ order(6))) # runs it for order 1 to 9
tidy(temp_fit6)

alphas <- temp_fit3 |> coefficients() |> dplyr::select(estimate) |> pull()
cat(
  "0 = 1", 
  "- (", alphas[1], ") * x",
  "- (", alphas[2], ") * x^2",
  "- (", alphas[3], ") * x^3"
)

polyroot(c(1, -alphas)) |> round(3)
polyroot(c( -alphas)) |> abs() |> round(3)

polyroot(c(1, -(temp_fit3 |> coefficients() |> dplyr::select(estimate) |> pull()))) |> abs() |> round(3)


alphas <- temp_fit6 |> coefficients() |> dplyr::select(estimate) |> pull()
cat(
  "0 = 1", 
  "- (", alphas[1], ") * x",
  "- (", alphas[2], ") * x^2",
  "- (", alphas[3], ") * x^3",
  "- (", alphas[4], ") * x^4",
  "- (", alphas[5], ") * x^5",
  "- (", alphas[6], ") * x^6"
)

polyroot(c(1, -alphas)) |> round(3)
polyroot(c(-alphas)) |> abs() |> round(3)

polyroot(c(1, -(temp_fit6 |> coefficients() |> dplyr::select(estimate) |> pull()))) |> abs() |> round(3)
