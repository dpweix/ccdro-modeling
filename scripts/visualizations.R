library("tidyverse")
theme_set(theme_bw())
theme_update(plot.title = element_text(hjust = 0.5, size = 15),
             plot.subtitle = element_text(hjust = 0.5, size = 10),
             strip.placement = "outside",
             strip.background = element_blank(),
             strip.text.x = element_text(size = 15))
library("ggbreak")
library("here")
library("lubridate")
library("forecast")
library("kableExtra")

# Load Data -------------------------------------------------------------------
df_open <- readRDS(here("data", "open-loop.rds"))
df_step_1 <- readRDS(here("data", "step-change-1.rds"))
df_step_2 <- readRDS(here("data", "step-change-2.rds"))

long_names <- c(sequence = "Sequence",
                date_time = "Time",
                hr = "Runtime (hr)",
                p_f = "Feed Pump Power Consumption (kW)",
                p_r = "Recirc. Pump Power Consumption (kW)",
                f_p = "Permeate Flow (L/min)",
                f_r = "Reject Flow (L/min)",
                c_p = "Permeate Conductivity (mS/cm)",
                c_r = "Reject Conductivity (mS/cm)",
                ph_f = "Feed pH",
                t_f = "Feed Temperature (C)")

# Open Loop Time Series Plots ---------------------------------------------

# Break points for open loop data
gap_1 <- c(tail(filter(df_open, sequence == 1), 1)$date_time,
           head(filter(df_open, sequence == 2), 1)$date_time)

gap_2 <- c(tail(filter(df_open, sequence == 4), 1)$date_time,
           head(filter(df_open, sequence == 5), 1)$date_time)

# Plot
df_open |> 
  pivot_longer(c(p_f, p_r, f_p, f_r, c_p, c_r, ph_f, t_f)) |> 
  filter(as.numeric(sequence) >= 5) |> 
  ggplot(aes(date_time, value, color = sequence)) + 
  geom_line(alpha = 1) +
  facet_wrap(vars(name),
             labeller = as_labeller(long_names),
             ncol = 1,
             scales = "free") +
  labs(x = "Time", y = "", color = "Sequence") +
  scale_color_viridis_d() +
  scale_x_datetime()

ggsave(here("figures", "open-loop-sequences.png"),
       width = 30, height = 30, units = 'cm')


# Hour ticks for step change
x_breaks <- list(df_step_1, df_step_2) |> 
  map(\(x) {
    seq(1, ceiling(max(x$hr)), 2)
  })

# Where data generation changes
change_points_1 <- df_step_1 |> 
  filter(sequence == 5 | sequence == 10) |> 
  group_by(sequence) |> 
  summarise(change_points = max(hr)) |> 
  pull("change_points")

change_points_2 <- df_step_2 |> 
  filter(sequence == 6) |> 
  pull("hr") |> 
  max()
  
# Plot 1
df_step_1 |> 
  pivot_longer(c(p_f, p_r, f_p, f_r, c_p, c_r, t_f)) |> 
  ggplot(aes(hr, value, color = sequence)) + 
  geom_line() +
  geom_vline(xintercept = change_points_1,
             color = "red",
             linetype = "dashed") +
  facet_wrap(vars(name),
             labeller = as_labeller(long_names),
             ncol = 1,
             scales = "free") +
  labs(x = "Time (Hours)", y = "", color = "Sequence") +
  scale_x_continuous(breaks = x_breaks[[1]]) +
  scale_color_viridis_d()

ggsave(here("figures", "step-change-sequences-1.png"),
       width = 30, height = 27, units = 'cm')

# Plot 2
df_step_2 |> 
  pivot_longer(c(p_f, p_r, f_p, f_r, c_p, c_r, t_f)) |> 
  ggplot(aes(hr, value, color = sequence)) + 
  geom_line() +
  geom_vline(xintercept = change_points_2,
             color = "red",
             linetype = "dashed") +
facet_wrap(vars(name),
           labeller = as_labeller(long_names),
           ncol = 1,
           scales = "free") +
  labs(x = "Time (Hours)", y = "", color = "Sequence") +
  scale_x_continuous(breaks = x_breaks[[2]]) +
  scale_color_viridis_d()

ggsave(here("figures", "step-change-sequences-2.png"),
       width = 30, height = 27, units = 'cm')

# Sequence lengths
df_open |> 
  group_by(sequence) |> 
  summarize(minutes = 60*(max(hr) - min(hr)))

df_step_1 |> 
  group_by(sequence) |> 
  summarize(hours = max(hr)-min(hr))

df_step_2 |> 
  group_by(sequence) |> 
  summarize(hours = max(hr)-min(hr))