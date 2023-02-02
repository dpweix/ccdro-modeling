library("tidyverse")
theme_set(theme_bw())
theme_update(plot.title = element_text(hjust = 0.5))
theme_update(plot.subtitle = element_text(hjust = 0.5))
theme_update(legend.position = "top")
library("here")
library("lubridate")
library("forecast")
library("kableExtra")

# Load Data -------------------------------------------------------------------
dat <- readRDS(here("data", "open-loop.rds"))

tib <- 1:length(dat) |>
  map_dfr(\(x) {
    mutate(dat[[x]], index = 1:nrow(dat[[x]]), sequence = as_factor(x), .before = p_f)
  })

# Open Loop Time Series Plots ---------------------------------------------

tib |> 
  pivot_longer(c(p_f, p_r)) |> 
  ggplot(aes(index, value, color = sequence)) + 
  geom_line() +
  facet_wrap(~ name) +
  labs(x = 'Observation Index', y = '',
       color = "Sequence",
       title = "Power Consumption of Feed and Recirculation Pump",
       subtitle = "Open Loop Experiments: Sequences 1-10") +
  scale_color_viridis_d()

ggsave("~/git/reports/CCDRO-modeling/figures/open-loop-sequences.png",
       width = 25, height = 10, units = 'cm')

# Predict Next Sequence from Previous Sequence --------------------------------

start <- 300 # forecast begins at observation number 100

# ARIMAX Models - FPPC
fit_arx_pf <- 1:10 |> 
  map(\(k) {
    
    # Matrix of exogenous variables
    xreg <- dat[[k]] |> select(c("f_p", "c_r")) |> as.matrix()
    
    # Fitted model
    auto.arima(dat[[k]]$p_f, xreg = xreg)
  })

# Semi-Empirical Models - FPPC
fit_sem_pf <- 1:10 |> 
  map(\(k) {
    
    # Fit semi-empirical model
    lm(p_f ~ f_p + c_r, dat[[k]])
  })

# Predictions ARIMAX - FPPC
pred_arx_pf <- 2:10 |> 
  map(\(k) {
    xreg_start <- fit_arx_pf[[k]]$xreg[1:start, ]
    xreg_forecast <- fit_arx_pf[[k]]$xreg[-c(1:start), ]
    
    # Fit with model estimated on previous experiment.
    Arima(y = dat[[k]]$p_f[1:start], model = fit_arx_pf[[k-1]],
          xreg = xreg_start) |> 
      forecast(xreg = xreg_forecast)
  })

# Predictions Semi-Empirical - FPPC
pred_sem_pf <- 2:10 |> 
  map(\(k) {
    predict(fit_sem_pf[[k-1]], dat[[k]])
  })


# Data frame of Observed and Predicted - FPPC
pred_df_pf <- 2:10 |> 
  map_dfr(\(k) {
    bind_rows(tibble(experiment = k, type = "Observed",
                     value = dat[[k]]$p_f[-c(1:start)],
                     date_time = dat[[k]]$date_time[-c(1:start)]),
              tibble(experiment = k, type = "Arimax",
                     value = pred_arx_pf[[k-1]]$mean,
                     date_time = dat[[k]]$date_time[-c(1:start)]),
              tibble(experiment = k, type = "Semi-Empirical",
                     value = pred_sem_pf[[k-1]][-c(1:start)],
                     date_time = dat[[k]]$date_time[-c(1:start)]))
  }) |> 
  mutate(experiment = as_factor(experiment),
         type = as_factor(type))

# Plot of observed and predicted - FPPC
pred_df_pf |> 
  ggplot(aes(date_time, value, color = type)) +
  geom_line() + 
  facet_wrap(~ experiment, scales = "free_x") +
  labs(x = "", y = "", color = "",
       title = "Open Loop Experiments - Feed Pump Power Consumption")

ggsave("~/git/reports/CCDRO-modeling/figures/open-loop-fppc.png",
       width = 25, height = 15, units = 'cm')



# Predict Next Sequence from Previous Sequence --------------------------------

start <- 300 # forecast begins at observation number 100

# ARIMAX Models - RPPC
fit_arx_pr <- 1:10 |> 
  map(\(k) {
    
    # Matrix of exogenous variables
    xreg <- dat[[k]] |> select(c("f_r", "c_r")) |> as.matrix()
    
    # Fitted model
    auto.arima(dat[[k]]$p_r, xreg = xreg)
  })

# Semi-Empirical Models - RPPC
fit_sem_pr <- 1:10 |> 
  map(\(k) {
    
    # Fit semi-empirical model
    lm(p_r ~ f_r + I(f_r^2) + 0, dat[[k]])
  })

# Predictions ARIMAX - RPPC
pred_arx_pr <- 2:10 |> 
  map(\(k) {
    xreg_start <- fit_arx_pr[[k]]$xreg[1:start, ]
    xreg_forecast <- fit_arx_pr[[k]]$xreg[-c(1:start), ]
    
    # Fit with model estimated on previous experiment.
    Arima(y = dat[[k]]$p_r[1:start], model = fit_arx_pr[[k-1]],
          xreg = xreg_start) |> 
      forecast(xreg = xreg_forecast)
  })

# Predictions Semi-Empirical - RPPC
pred_sem_pr <- 2:10 |> 
  map(\(k) {
    predict(fit_sem_pr[[k-1]], dat[[k]])
  })


# Data frame of Observed and Predicted - RPPC
pred_df_pr <- 2:10 |> 
  map_dfr(\(k) {
    bind_rows(tibble(experiment = k, type = "Observed",
                     value = dat[[k]]$p_r[-c(1:start)],
                     date_time = dat[[k]]$date_time[-c(1:start)]),
              tibble(experiment = k, type = "Arimax",
                     value = pred_arx_pr[[k-1]]$mean,
                     date_time = dat[[k]]$date_time[-c(1:start)]),
              tibble(experiment = k, type = "Semi-Emperical",
                     value = pred_sem_pr[[k-1]][-c(1:start)],
                     date_time = dat[[k]]$date_time[-c(1:start)]))
  }) |> 
  mutate(experiment = as_factor(experiment),
         type = as_factor(type))

# Plot of observed and predicted - RPPC
pred_df_pr |> 
  ggplot(aes(date_time, value, color = type)) +
  geom_line() + 
  facet_wrap(~ experiment, scales = "free_x") +
  labs(x = "", y = "", color = "",
       title = "Open Loop Experiments - Recirculation Pump Power Consumption")

ggsave("~/git/reports/CCDRO-modeling/figures/open-loop-rppc.png",
       width = 25, height = 15, units = 'cm')


### MAE ---------------------------------------------------------------------
# Residuals by Model - RPPC
mae_rppc <- pred_df_pr |> 
  pivot_wider(names_from = type,
              values_from = value,
              values_fn = "list") |> 
  unnest(cols = c("Observed", "Arimax", "Semi-Emperical")) |> 
  pivot_longer(c("Arimax", "Semi-Emperical")) |> 
  mutate(name = as_factor(name)) |> 
  group_by(name, experiment) |> 
  summarise(mae = mean(abs(Observed-value))) |> 
  ungroup() |> 
  pivot_wider(names_from = name,
              names_prefix = "",
              values_from = mae)  |> 
  mutate(across(where(is.numeric), round, 3))


# Residuals by Model - FPPC
mae_fppc <- pred_df_pf |> 
  pivot_wider(names_from = type,
              values_from = value,
              values_fn = "list") |> 
  unnest(cols = c("Observed", "Arimax", "Semi-Empirical")) |> 
  pivot_longer(c("Arimax", "Semi-Empirical")) |> 
  mutate(name = as_factor(name)) |> 
  group_by(name, experiment) |> 
  summarise(mae = mean(abs(Observed-value))) |> 
  ungroup() |> 
  pivot_wider(names_from = name,
              names_prefix = "",
              values_from = mae) |> 
  mutate(across(where(is.numeric), round, 3)) #|> 
# kbl(format = "latex", booktabs = TRUE)

# LaTeX Table: Linear Data

bind_cols(mae_fppc, mae_rppc[, -1], .name_repair = "minimal") |> 
  kbl(format = "latex", booktabs = TRUE, linesep =  c('', '', '', '\\addlinespace'),
      escape = FALSE) |> 
  add_header_above(c(" " = 1,
                     "Feed Pump Power Consumption" = 2,
                     "Recirculation Pump Power Consumption" = 2))
