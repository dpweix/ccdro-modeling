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
df_st <- readRDS(here("data", "step-change.rds"))

# Step Change Time Series Plots ---------------------------------------------

tib |> 
  pivot_longer(c(p_f, p_r)) |> 
  ggplot(aes(index, value, color = sequence)) + 
  geom_line() +
  facet_wrap(~ name) +
  labs(x = 'Observation Index', y = '',
       color = "Sequence",
       title = "Power Consumption of Feed and Recirculation Pump",
       subtitle = "Step Change Experiments: Sequences 1-16") +
  scale_color_viridis_d()

ggsave("~/git/reports/CCDRO-modeling/figures/step-change-sequences.png",
       width = 25, height = 10, units = 'cm')
